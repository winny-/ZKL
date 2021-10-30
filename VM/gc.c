/* gc.c : Garbage collection
 * 
 * Copyright (c) 2006,2007,2008-12,2013 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#define defaultTime	30000	// N : GC at N millisecond intervals

	// The next two are intended to be atomic
static unsigned long gcOnTime  = defaultTime;

#define KEEP_OBJECT_COUNTS	1	// 1 if you want [sloppy] stats
#define GC_VERBOSE		0	// print annoying stats
#define GC_DONT_FREE		0	// when all else fails ...


/* Rules:
 *   - gcType is set once.  Only instances that are not on a collectable
 *     list can have their type set.
 */

/* Assumptions:
 * - Reading/writing pointers in structs is atomic.
 * - Reading/writing different struct elements (in the same struct) by
 *   different threads, at the same time, is atomic and only effect the
 *   elements being changed.
 * - Reading/writing struct elements is atomic (like chars)
 *   It looks like (on MS VC anyway), this is true for ints but not chars
 * - If instance I isn't on a instance list, it can't be reclaimed, even if
 *   it is in a container.
 */

/* Rule: Once an instance has been added to GC (addToCollectables() has been
 * called), ONLY the GC thread can touch the Instance header (for marking).
 * With ONE exception:  Calling instanceMark() (via instanceIsOrphan()).
 * Which sucks.  Here is why I think it is safe:
 * - It is a struct that is padded to a "natural" boundry.
 * - The compiler can't read/write beyond that boundry. Because.
 * - instanceIsOrphan only calls instanceMark during the marking phase, ie
 *   setting the mark bit.
 *   - I'm guessing the compiler reads an int, sets the mark bit, writes the
 *     int (ie more of the struct than just the bit or flicking the bit
 *     directly in memory).
 *   - An atomic var is used to ensure nobody is setting marks while
 *     somebody else is clearing them.
 *   - Two threads marking the same instance at the same time: No matter
 *     what, the bit is set and nothing is garbaged.
 * - What I don't know: 
 *   - The thread caches may not be in sync (GC doesn't realize the other
 *     thread marked i). A problem in any case as GC can request a thread
 *     to mark itself.
 */

/* Rule:  A thread/VM stack is NOT marked while in a Method UNLESS that
 * thread says it is OK (ie parked in vmMightStall()).  The GC mark phase is
 * almost single threaded per VM stack (ie several threads can be marking
 * but each thread is marking only one VM stack.  But the data being marked
 * may exist in other stacks. instanceIsOrphan() being the fly in the
 * ointment).
 */

/* Read/write rules for thread safe writable containers:
 * 0) These rules only apply to threads sending or recieving instances
 *    to/from other threads.  If you are in a single thread, no worries.
 * 1) Gotta lock. Need read & write barriers.
 * 2) Reads:
 *   a) Gotta lock.
 *   b) If read is distructive and an instance is returned, it is an orphan.
 *      If >1 instances are deleted, only the returned instance(s) are
 *      orphans.
 *   c) Non distructive reads: no orphans.
 * 3) Writes:
 *   a) Gotta lock.
 *   b) Any incoming instance is an orphan.
 *   c) Distructive writes: Only the incoming is orphan.
 *   d) If an incoming overwrites many, only incoming is orphan.
 *   e) Write transforms value (ie creates new instance from value or morphs
 *      it to non instance):  no orphan.
 * 4) Single or mass deletions are OK.
 * 5) Creation + write (L(1,2,3)): Always happens in same thread so no orphans.
 *    - Create and call f to get a value to write:  OK (f doesn't do thread
 *      stuff, so created values stay in self VM).
 * Why: DIP (dangling instance pointer):
 *   T1 holds i, gives it to T2 and forgets about it.
 *   If T2 has been gc'd but T1 hasn't been, i won't be marked and is freed.
 *   Many, many variations of this exist.
 * A thread that accesses data in another thread is a closure.
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#ifdef _MSC_VER
   #include <process.h>		// _beginthread, _beginthreadex
#elif defined(__unix__)
   #ifdef __linux__
      #include <malloc.h>
   #endif
   #define PTHREADS	
//   #define __USE_GNU		// SCHED_*
   #include <pthread.h>		// pthread_*
//   #include <sched.h>		// SCHED_*
#endif
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#define __NOT_A_DLL
#define __GC_INTERNALS
#define __FCN_INTERNALS

#define __STRING_INTERNALS

#include "zklObject.h"
#include "zklAtomic.h"
#include "zklFcn.h"
#include "zklClass.h"
#include "zklList.h"
#include "zklMemory.h"
#include "zklMethod.h"
#include "zklNumber.h"
#include "zklString.h"
#include "zklUtil.h"

#define MAX_OBJ_TYPE 40	// Bigger than the max type ID, SHOULD be in vm.h.zkl

    // These lists are only touched by the GC thread
static Instance	instanceList;
static Instance mtOlympus;	// where the immortals live

static Instance  *bucketImmortals[50];	// immortal BInstances, zero'd
static CAtomicInt ibsize;		// 15 currently

static CAtomicInt markingOrphan;

CAtomicInt markingInProgress;		// 1 while GCT is marking

/* ******************************************************************** */
/* *************************** State Change *************************** */
/* ******************************************************************** */

#if KEEP_OBJECT_COUNTS
   static CAtomicInt gcTypeCounts[GC_NUM_TYPES];
   static CAtomicInt invisiCounts[MAX_OBJ_TYPE];
#endif

static char *gcTypeNames[GC_NUM_TYPES] =
{
   "","Untouchable","Immortal","Invisible",
   "Owned","Special","2Special",
};

    // 0(navel gazing), 1(GC started), 2(marking), 3(sweeping), 4(reclaiming)
    // 3(2 + running low on mem)
    // Atomic because it is used for orphans (by buckets) but is only set here.
CAtomicInt gcState;

static SpinLock sureLock;

    /* If marking, mark i. Otherwise, do nothing.
     * This will keep the orphan alive though one GC cycle
     * An orphan is an instance that is between owners, ie live but
     *   currently has nobody to mark it.  This is bad if GC is in progress.
     *   An example might be an instance that is being read from a Pipe; the
     *   writer has gone on to other things, it is no longer in the Pipe (so
     *   the Pipe can't mark it) and the reader hasn't yet got it.  Writing
     *   to a Pipe:  writer doesn't hold i, Pipe has been marked, i isn't
     *   marked.
     * I can't mark i in a truly thread safe manner, the GCT, or another
     *   owner, might also be marking it. See top of file for why I think
     *   this is OK.
     * It is important to note that markingInProgress is set in one
     *   thread/core while being read in another (the caches may not be in
     *   sync) so a write barrier is used to ensure cores are synced.
     * To make this work, BEFORE i is removed from its container (ie
     *    detached from its current owner), we have to know if marking is in
     *    progress.  If so, it has to be marked BEFORE it is popped.  Then,
     *    pop and proceed as usual.  If we wait until after pop to check,
     *    marking can finish (and sweep start) between the pop and check,
     *    with gc treating i as garbage.
not safe - test, <tick tick sweep starts> pop/mark, garbage
     * It is unknown how long marking i can take, which could be a problem
     *   if this is called right before sweep starts.
     * 
     * VM1:v1 = L("opps"), v2 = "foo";
     * Marking in progress
     * VM3 is marked
     * VM3:v3 = v1.pop();   "opps" detached from VM1, moves to VM3
     * VM3:v4 = v2; v2 = Void;    ditto "foo"
     * VM1 is marked, "oops"/"foo" is not since it moved to already marked VM2
     * Sweep: "oops" & "foo" are garbage
     */
//!!! T1: v=L(L()) T2: z=v.pop(); z.add(i); mark --> can miss i
void instanceIsOrphan(Instance *i)
{
   if (!i) return;
   	// is sweep in progress and not marked yet? Too late
//if (gcState == 3 && !GC_IS_MARKED(i))
//vmHalt("Buried alive");
//!!! only if owned?

   if (CAI_VALUE(&markingInProgress))
   {
// if I'm not marked, don't bother
      CAI_INC(&markingOrphan);	// marking is not instantaneous
         instanceMark(i);
      CAI_DEC(&markingOrphan);
   }
}

void instanceIsOrphan2(Instance *i1, Instance *i2)
{
   if (CAI_VALUE(&markingInProgress))
   {
      CAI_INC(&markingOrphan);	// marking is not instantaneous
         instanceMark(i1); instanceMark(i2);
      CAI_DEC(&markingOrphan);
   }
}

#if 0
    /* If marking is in progress, don't detach i until marking is finished.
     * Avoid problems instanceIsOrphan() has
     */
void instanceIsOrphanWait(Instance *i,pVM vm)
{
   if (CAI_VALUE(&markingInProgress))
      intWaitFor(&markingInProgress,0,1,0,vm);
}
#endif

     // i is currently invisible
     // Don't twiddle Instance after adding to gc list because marking can
     //    happening and conflict.
     // CAN TRIGGER GC but the collection doesn't start until after return.
     //   If in method, that VM won't GC until after method returns
     // Returns i
Instance *addToCollectables(Instance *i, int itype, pVM vm)
{
   #if USE_POINTER_INTS && GC_SANITY_CHECK
      if (IS_PtrInt(i)) vmHalt("addToCollectables: Argh! PtrInt");
   #endif

   if (OBJECT1(i)->isBInstance)
      vmThrow(vm,E_VM_ERROR,"addToCollectables: Please! No BInstances");

   // i's gc type is invisible or i is already in a gc list
   if (GC_TYPE(i) != I_INVISIBLE)
      vmThrow(vm,E_VM_ERROR,"addToCollectables: Please! Invisibles only");
#if 0
   #if KEEP_OBJECT_COUNTS
      CAI_DEC(&invisiCounts[TYPEO(i)]);
      CAI_DEC(&gcTypeCounts[I_INVISIBLE]);
   #endif
#endif

	// some types just don't get included
   if (itype == I_INVISIBLE || itype == I_UNTOUCHABLE || itype == I_SPECIAL ||
       itype == I_2SPECIAL)
   {
      GC_TYPE(i) = itype;
      #if KEEP_OBJECT_COUNTS
         CAI_INC(&gcTypeCounts[itype]);
	 if (itype == I_INVISIBLE) CAI_INC(&invisiCounts[TYPEO(i)]);
      #endif
      return i;
   }

//   GC_TYPE(i) = itype;
   spinLockAcquire(&sureLock);
      GC_TYPE(i) = itype;	// this needs to be atomic if BInstance
      switch(itype)
      {
	 case I_IMMORTAL: 
	    if (IS_CONTAINER(i))	// I only care if I must mark
	       { i->nexti = mtOlympus.nexti; mtOlympus.nexti = i; }
	    break;
	 case I_OWNED:		// invisible converting to visible/owned
	 {
//	    Instance *ptr = IS_CONTAINER(i) ? &containerList : &instanceList;
	    Instance *ptr = &instanceList;
	    i->nexti = ptr->nexti; ptr->nexti = i;
	    break;
	 }
	 default:
	    vmHalt("addToCollectables: Invalid type");
      }
      #if KEEP_OBJECT_COUNTS
	 CAI_INC(&gcTypeCounts[itype]);
      #endif
   SPIN_LOCK_RELEASE(&sureLock);
   return i;
}

void immortalBInstance(Instance *i)
{
   // BInstances don't have nexti so I can't link them to the Immortals
   int n = CAI_INC(&ibsize);	// bucketImmortals[ibsize] == 0
   if (n >= 50) vmHalt("immortalBInstance(): Too many on the mountain");
   CAP_SET(&bucketImmortals[n-1],i);
}

#if 0
    /* Definition: i is GC safe if:
     * 1) i is not a container
     * else
     *   2) i is confined to a single thread
     *   or
     *   3) i is in T1, can be marked by GCT while being changed by T2
     *      for any member of i
     *   
     * Thus a thread safe container might not be GC safe if it contains a
     *   non GC safe object. Which is a recursive definition.
     * A mutible container is never GC safe as it may later acquire a non
     *   GCS member (unless 2 holds).
     */
Instance *makeGCSafe(Instance *i,pVM vm)
{
   Instance *i2;
   if (!IS_CONTAINER(i)) return i;
   i2 = OBJECT1(i)->makeGCSafe(i,vm);	// which might be i
   if (!i2) notImplementedError(i,"makeGCSafe",vm);
   return i;
}
#endif

/* ******************************************************************** */
/* ************************ Garbage Collection ************************ */
/* ******************************************************************** */

/* The garbage collector is a thread. This means that, while code is
 * running, GC is taking place at the same time.  GC does not shut down the
 * system, so it is usually transparent to your code, except in the
 * following cases:
 * If your object is a container and contains objects (such as List or
 * Dictionary but not Data), you have to worry about GC happening while you
 * are changing your object. You also have to worry when creating the object
 * and initially populating it. Here is what to do:
 * - At creation, if you will creating and adding objects:
 *   Create the instance INVISIBLE and populate, then call
 *   instanceBecomesVisible(instance). This way, GC won't know about your
 *   object until after you are done with it.
 * - Use SpinLocks during object changes and your magic marker.
 */


/* Special callback routines, registered by a object.
!!! shitty name, overlaps with object.magicMarker
*   MagicMarker:  Called by the collector so that a static object
*     container can mark all its contents. This is for objects that
*     aren't known by the garbage collector.
* Object.magicMarker :  Set by Object, used by GC to mark live
*   Instances:  First, the Instance is marked live and then that
*   Instances magic marker is called.  An Instance doesn't mark itself,
*   only its contents.
*/


	// List of GC Root markers
static RootMarker *markers = 0;

/////////////////////////////////////////////////////////////////////////
////////////////////////// GC Thread ////////////////////////////////////
/////////////////////////////////////////////////////////////////////////

extern CAtomicInt _numObjects;		// object.c

    // --> 1 (i is valid), 0 (is garbage)
int instanceCheck(Instance *i,int mightBeInt)
{
   if (!i) return 1;
   #if USE_POINTER_INTS
      if (mightBeInt && IS_PtrInt(i)) return 1;
   #endif

   if (i == (Instance *)(size_t)0xfeeefeee || 	// VC freed pointer sig (Debug)
       i->objID >= (unsigned)_numObjects   ||
       ((size_t)i & 1) || 
       GC_TYPE(i) == 0 || GC_TYPE(i) > GC_NUM_TYPES)
	 return 0;
   return 1;
}

    // set $zklDumpCore=1
void instanceVerify(Instance *i,int mightBeInt,pVM vm)
{
   if (!instanceCheck(i,mightBeInt))
   {
      if (vm) stackTrace(vm,1,0);
      vmHalt("CORRUPT instance");
   }
}

    /* This gets several types of Instances:
     * 0 : Containers have pointers to null Instances
     * Invisible Instances:
     *   Only need to mark if it is a container but not worth checking (once
     *     marked, it will stay marked, so contents better not change).
     *   Examples:
     *     - Static instances, eg Void, Zero, lists that hold these instances.
     *     - Fcns and Classes that are being created.
     *     - NoArglist
     * A regular instance
     * Other cases:
     *   - The VM arglists contain zeros
     *
     * This function recurses big time. NOT EFFICIENT.
     * 
     * sureLock is NOT held, this is called from VM & GC threads.
     * ONLY CALL THIS FROM ONE THREAD!?!.
     * 
     * Marking rules: If not marked, then:
     *				 Marker	 In a GC
     * GC Type		Marked	 Called	  List	  Notes
     * -------		------	 ------	 -------  -----
     * I_UNTOUCHABLE	  N	    N	    N	  Can't change type
     * I_INVISIBLE	  N	    N	    N	  Changeable
     * I_IMMORTAL	  Y	    Y	    Y
     * I_OWNED		  Y	    Y	    Y
     * I_SPECIAL	  Y	    Y	    N	  eg VMs
     * I_2SPECIAL	  N	    Y	    N	  Recursion hell, KStrings
     *
     * Examples of specials are MLists & KStrings
     *
//!!! fuck! if marker stalls (as in locked object), gc stalls
// ie one thread can hang gc
2 threads can mark the same object at the same time
gc type can change while in this function
     */
//!!! #define GC_MARK_THINGIE(i) try to avoid fcn call, ie mark non container 
void instanceMark(Instance *i)
{
   if (!i) return;	// eg empty fcns in a class embryo

   #if USE_POINTER_INTS
      if (IS_PtrInt(i)) return;		// which could also be garbage
   #endif

   #if GC_SANITY_CHECK
      instanceVerify(i,0,NoVM);
   #endif

	// avoid interdependant recursion, NOT thread safe
   if (GC_IS_MARKED(i)) return;

   switch(GC_TYPE(i))
   {
      case I_UNTOUCHABLE: case I_INVISIBLE:
         break;
      case I_2SPECIAL:
	 if (MAGIC_MARKER(i)) MAGIC_MARKER(i)(i);
	 break;
      default:
	 GC_MARK(i);
	 if (MAGIC_MARKER(i)) MAGIC_MARKER(i)(i);
	 break;
   }
}

void instanceMarkN(int n, ...)
{
   va_list ap;

   va_start(ap,n);
      while(n--) instanceMark(va_arg(ap, Instance *));
   va_end(ap);
}

void gcRegisterMarker(RootMarker *marker)
{
   spinLockAcquire(&sureLock);
      marker->next = markers;
      markers      = marker;
   SPIN_LOCK_RELEASE(&sureLock);
}

    /* Mark all live instances on the instanceList.  When done, instances on
     *   instanceList, that are not MARKED, are garbage.
     * Container instances on other lists might be marked and those need to
     *   be unmarked so that the next time _mark() is called, containers
     *   will mark their contents.
     * Marking order: VMs, everbody else. This is so that everybody who
     *    points to stuff in a VM will be marked after the VMs are.  Then,
     *    object O can point to I in VM and, if VM dies (and isn't marked),
     *    O won't have been marked before it has a chance to point into VM
     *    and thus not mark I.
     * Marking immortal containers:
     *   - Reset marks so instanceMark() will mark.
     *   - If immortal container contains an owned container, owned is marked
     *     (round 1).  Then, owned container will have it's mark reset by
     *     _seprate() so I don't need to worry about "floating" marks.
     *   - Ditto other owned objects
     */
static void resetMarks(Instance *i)
{
   for (; i; i = i->nexti)
   {
//      if (TYPEO1(i) == ClassType) classResetMarks(i);
      GC_CLEAR_MARK(i);
   }
}

    /* Note on immortal containers (and roots):  Immortal containers don't
     * have to be on a Instance list to stay alive.  However, that means
     * that the reclaim sweep might not reset their marks (ibuckets are
     * different in that all BInstances are on a blist), or, more
     * importantly, the instances in that container.  So, for example, if an
     * immortal List (eg the one in the Vault) holds an Instance container
     * (not BInstance) (eg class), that container won't get the chance to
     * mark its contents. startup tends to hide this issue.
     * I haven't figured out how to show this problem.
     * 
     * Classes have special case code to deal with this issue.
     */
static void markRoots(void)
{
   int n;
   Instance   *i;
   RootMarker *marker;

   for (n = 0; n < ibsize; n++) instanceMark(bucketImmortals[n]);

   for (i = mtOlympus.nexti; i; i = i->nexti) 		 instanceMark(i);
   for (marker = markers; marker; marker = marker->next) marker->marker();
   markVMs();
}

    /* Once instances have been seprated from the instanceList (old), marks
     * are being reset and roots marked, something like this can happen:  A
     * new container is created (eg a Class, List, etc) and instance i is
     * transfered its old container to the new container.  If this happens
     * before the old container marks it, it looks like garbage (since it
     * didn't get marked).  Soooo, mark the new containers to cover that
     * case and do a lot of unneccesary work.  Isn't concurrent GC fun?
     * This can be a lot of containers, I see ~2,000 during tests with big
     * spikes (up around 20,000).
     * 
     * v1 = "opps"; v3 = "OK";
     * Heap partitioned, v1/"oops" on old heap
     * v2 = L(v1);  new list with old contents
     * v1 = Void;   "opps" detached from old heap, referenced by new heap
     * Old heap marked: "opps" won't get marked
     * At this point, all the old objects that are in play have been marked,
     *   except those that moved to new containers or are orphans.
     * Mark new heap, "opps" is marked.
     * v4 = L(v3); is OK as v3 has been marked
     * Sweep
     * 
     * Bad interaction with instanceIsOrphan():
     *   v = i; p = Pipe() | split heap | 
     *   p.write(L()); v2 = p.read(); v2.add(v); v = Void; | mark
     * Won't mark i as Pipe write/read marks new List
     * 
     * Compiling the parser, I'm marking < 10 containers.
     */
static void lookAgain(void)
{
#if 0
   Instance *i, *list, *last = 0;

   spinLockAcquire(&sureLock);
      list		 = instanceList.nexti;
      instanceList.nexti = 0;
   SPIN_LOCK_RELEASE(&sureLock);

   if (!list) return;

   for (i = list; i; last = i, i = i->nexti)
      if (IS_CONTAINER(i)) instanceMark(i);

   spinLockAcquire(&sureLock);
      last->nexti = instanceList.nexti; instanceList.nexti = list;
   SPIN_LOCK_RELEASE(&sureLock);
#else
   Instance *i;

   spinLockAcquire(&sureLock);
      for (i = instanceList.nexti; i; i = i->nexti)
         if (!GC_IS_MARKED(i) && IS_CONTAINER(i)) instanceMark(i);
   SPIN_LOCK_RELEASE(&sureLock);
#endif
}

void updateWeakRefs(IBucketHeader *);		// allocate.c
static IBucketHeader _wrBuckets,*wrBuckets = 0;

    // Prepend the dead to the dead lists
    // Clear marks to get ready for next mark phase
static Instance *
_seperate(Instance *listHeader, Instance **_zombies, Instance **_metalHeads)
{
   size_t    theDead = 0, theLiving = 0;
   Instance *zombies = *_zombies, *metalHeads = *_metalHeads;
   Instance *i, *next, *prev;

   updateWeakRefs(wrBuckets);

   for (prev = listHeader, i = listHeader->nexti; i; i = next)
   {
      next = i->nexti;		// i->nexti might be munged
      if (GC_IS_MARKED(i))	// if marked, leave alone
      {
//	 if (TYPEO1(i) == ClassType) classResetMarks(i);
	 GC_CLEAR_MARK(i);
	 theLiving++;
	 prev = i;
      }
      else		// dead, add to dead list, owned ONLY
      {
	 theDead++;
	 prev->nexti = next;	// unlink i
		// prepend to a dead list
#if 0
	 if (TYPEO1(i) == ClassType && classDestructor(i,0))
	    { i->nexti = metalHeads; metalHeads = i; }
	 else 
#endif
	    { i->nexti = zombies;    zombies    = i; }
      }
   } // for
   *_zombies = zombies; *_metalHeads = metalHeads;
   #if GC_VERBOSE
      printf("GC: Living: %d, dead: %d\n",theLiving,theDead);
   #endif
   return prev;		// last Instance in the list
}

    /* Split the heap (so threads can continue to run while I'm doing GC),
     * reset the marks that don't get reset during the free sweep, mark all
     * live objects, merge the heaps.
     * Note:  Marks are cleared on allocation and during the free sweep but
     * not if immortal/special/new (ie in the "new" heap but marked by an
     * "old" instance, which is a problem for contents of containers.
     * Note: compiling the parser, only reset marks on 1k - 4k instances.
     */
#if 1
static void
_seperateTheWheatFromTheChaff(Instance **zombies, Instance **metalHeads)
{
   void ibucketLookAgain(IBucketHeader *wrBuckets);	// allocate.c

   Instance *last;
   Instance  listHeader;

   // Partition the instance heap
   spinLockAcquire(&sureLock);
      listHeader.nexti   = instanceList.nexti;
      instanceList.nexti = 0;
   SPIN_LOCK_RELEASE(&sureLock);

   // Reset all marks that won't get reset during sweep
   // Immortals in Buckets reset themselves
   resetMarks(mtOlympus.nexti); vmResetMarks();

   // Mark phase
   CAI_ONE(&markingInProgress);
   CAI_SET(&gcState,2);
      markRoots();
      lookAgain();
      ibucketLookAgain(wrBuckets);
   CAI_ZERO(&markingInProgress);
   intWaitFor(&markingOrphan,0,1,0,NoVM);

   // Sweep phase
   CAI_SET(&gcState,3);
   last = _seperate(&listHeader,zombies,metalHeads);

   // Put the living back onto the live list
   spinLockAcquire(&sureLock);
      resetMarks(instanceList.nexti);  // undo lookAgain()
      last->nexti	 = instanceList.nexti;
      instanceList.nexti = listHeader.nexti;
   SPIN_LOCK_RELEASE(&sureLock);
}
#else

typedef struct 
{
   int       gcAt;	// GC this generation when tick is ==
   int       tick;	// WHEN previous generation GCs, tick++
   Instance *gi;	// Instance list for this generation
} Generation;

#define NUM_GENERATIONS 4	// Counting babies, min 2
Generation generations[NUM_GENERATIONS] =
{
   { 0, 0,0 },		// babies, allways collected
   { 30,0,0 },
   { 10,0,0 },
   {  5,0,0 },		// oldest generation never rolls over
};

static void
_seperateTheWheatFromTheChaff(Instance **zombies, Instance **metalHeads)
{
   int g,ng;

   	// always GC newest instances (the babies)
   spinLockAcquire(&sureLock);
      generations[0].gi  = instanceList.nexti;
      instanceList.nexti = 0;
   SPIN_LOCK_RELEASE(&sureLock);

   	// gather the generations that will be gc'd
   for (ng = 1; ng < NUM_GENERATIONS; ng++)
   {
      Generation *gen = &generations[ng];
      if (++gen->tick == gen->gcAt) gen->tick = 0;
      else break;
   }

   resetMarks(mtOlympus.nexti); vmResetMarks();
for (g = 1; g < NUM_GENERATIONS; g++) resetMarks(generations[g].gi);

   CAI_ONE(&markingInProgress);
      markRoots();
      lookAgain();
   CAI_ZERO(&markingInProgress);
   intWaitFor(&markingOrphan,0,1,0,NoVM);

   spinLockAcquire(&sureLock);
	// clear marks on new objects that may have been marked
      resetMarks(instanceList.nexti);
   SPIN_LOCK_RELEASE(&sureLock);

   for (g = ng; g--; )	// gotta go backwards to roll generations forwards
   {
      Instance listHeader;
      Generation *gen = &generations[g];

      listHeader.nexti = gen->gi;
      if (listHeader.nexti)		// might be empty
      {
	 Instance *liveLeaf = _seperate(&listHeader,zombies,metalHeads);

		// add this generation to next older generation
	 if (g == NUM_GENERATIONS-1)	// oldest gen don't roll
	    gen->gi = listHeader.nexti;
	 else			// add this generation to next older
	 {
	    if (listHeader.nexti)	// this gen might be completely dead
	    {
	       Generation *geezers = &generations[g+1];
	       liveLeaf->nexti     = geezers->gi;
	       geezers->gi		= listHeader.nexti;
	    }
	    gen->gi = 0;
	 }
      }
   }
}
#endif

    // !!!!remove!!!!-->metalHeads are classes with destructors
static void _reclaim(Instance *zombies, Instance *metalHeads)
{
   Instance	    *zombie, *next;
   int		     n = 0;
   unsigned long int no;

   #if GC_SANITY_CHECK && 0
      for (zombie = zombies; zombie; zombie = zombie->nexti)
	 if (TYPEO1(zombie) == ClassType) classVerify(zombie);
      for (zombie = metalHeads; zombie; zombie = zombie->nexti)
         classVerify(zombie);
   #endif

   no = 0;
   while(1)	// nuke zombies and metalHeads
   {
      for (zombie = zombies; zombie; zombie = next)
      {
	 _FreeMe freeMe = FREE_ME(zombie);
	 int	 type   = GC_TYPE(zombie);

	 #if GC_SANITY_CHECK
	    if (type != I_OWNED)
	    {
	       char buf[100];
	       sprintf(buf,
		     "BOGUS zombie! %s:%s",iname(zombie),gcTypeNames[type]);
	       vmHalt(buf);
	    }
	    if (IS_IMEM(zombie)) vmHalt("IMEM zombie!");
	 #endif

	 next = zombie->nexti;		// do this before instance is freed

	 if (freeMe)
	 {
//if ((size_t)freeMe == 5) { classFree(zombie); continue; }
	    if ((size_t)freeMe < 10) continue;	// I_IS_IN_MEM

	    #if GC_DONT_FREE
	       zombie->myID = 666;
	       continue;
	    #endif
	    if (!freeMe(zombie)) continue;  // Instance says "don't free me"
	 }
	 // Now take care of instances allocated with instanceAllocate()
	 #if !GC_DONT_FREE
	    #if __ZM_BULK_FREE
	       zibFree(zombie);
	    #else
	       ZFREE(zombie);
	    #endif
	 #endif

	 #if KEEP_OBJECT_COUNTS
	    if (type == I_OWNED) no++;
	 #endif
      } // for

      if (n++) break;
      zombies = metalHeads;
   } // while


   #if KEEP_OBJECT_COUNTS
   {
//      spinLockAcquire(&sureLock);
      CAtomicInt *n = &gcTypeCounts[I_OWNED];
      CAI_SET(n,*n - no);
//      SPIN_LOCK_RELEASE(&sureLock);
   }
   #endif
   #if GC_VERBOSE
      printf("Actually freed %d instances (somewhere around %d bytes)\n",n,n*sizeof(Instance));
   #endif
}

unsigned long int gcCount = 0;	// number of collections
static CAtomicInt startGC;	// external signal

#ifdef _MSC_VER
   static void  garbageMan(void *x)		// a thread
#elif defined(PTHREADS)
   static void *garbageMan(void *x)		// a Pthread
#endif
{
   while(1) // a garbage man's job is never done
   {
      int s;
      Instance *zombies = 0, *metalHeads = 0;

      // wait for external signal or timeout
      s = caiWait4(&startGC,1,!gcOnTime,gcOnTime);
//printf("GC STARTED %d\n",startGC);
      CAI_ONE(&startGC);	// in case of timeout; not an event

      gcCount++;

      CAI_ONE(&gcState);	// a GC cycle has started
      _seperateTheWheatFromTheChaff(&zombies,&metalHeads);

//caiSet(&startGC,0);
      CAI_SET(&gcState,4);
      _reclaim(zombies,metalHeads);
      ibucketSweep();		// good use for another thread
      #if __ZM_BULK_FREE
         ziBulkFree();
      #endif

      #ifdef __linux__
	 if (!s) malloc_trim(0);   // trim on timeout (ie not doing much)
      #endif

      CAI_ZERO(&gcState);
      caiSet(&startGC,0);  // OK, I'm ready for another round, broadcast
//!!!! deal vulture actions
//printf("---->GC has finished %ld  %d\n",gcCount,startGC);
   } // while
}

#if 0
    // Only free-able objects (no Immortals, etc)
// move to GCT
// sweep if need to
// move spareMe & marked stuff to global pool
void freeThisListOfGargage(Instance *garbage, Instance *spareMe)
{
   Instance *i, *zombies = 0, *metalHeads = 0;

   if (!garbage) return 0;

   for (i = garbage; i; i = i->nexti)
   {
      if (i == spareMe) { i = i->nexti; spareMe = 0; continue; }
      if (TYPEO1(i) == ClassType && classDestructor(i,0))
	   { i->nexti = metalHeads; metalHeads = i; }
      else { i->nexti = zombies;    zombies    = i; }
   } // for

   _reclaim(zombies,metalHeads);
   return !spareMe;
}
#endif

/* void startGarbageMan(void)
 * !!!Really need these for GC thread:
 *   master switch: kill or restart gc thread, or just stop/restart gc
 *   need a way to ping gc thread, method to check if gc is running
 *   restart gc thread if it dies
 */

#ifdef _MSC_VER
   void startGarbageMan(void)
   {
      uintptr_t	thread = _beginthread(garbageMan,0,0);
      if (thread == -1L)
	 printf("   startGarbageMan: _beginThread failed (%ld)\n", thread);
   }

#elif defined(PTHREADS)
//#include <errno.h>
//#include <time.h>
//#include <sys/resource.h>
   void startGarbageMan(void)
   {
      int	     s;
      pthread_attr_t attr;
      pthread_t	     thread;

      s = pthread_attr_init(&attr);

      // Linux default attributes: joinable, system scope, SCHED_OTHER
      s = pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_DETACHED);
      if (s) printf("pthread_attr_setdetachstate pooh: %d\n",s);

#if 0
      // SCHED_OTHER(0)
      // SCHED_FIFO(1), SCHED_RR(2) // these need a priority of 1-99 & sudo
      //   otherwise, policy is just SCHED_OTHER
      // EINVAL is 22, EPERM is 1
      // Not supported (Linux): SCHED_BATCH(3), SCHED_IDLE(5), SCHED_NORMAL(0)
      s = pthread_attr_setschedpolicy(&attr,SCHED_RR);
      if (s) printf("pthread_attr_setschedpolicy pooh: %d\n",s);
#endif

      // Linux: thread affinity is all cores

      if ((s = pthread_create(&thread,&attr,garbageMan,0)) != 0)
      if (s) printf("   startGarbageMan: pthread_create failed (%d)\n",s);

#if 0
      {  // this is rather nice
	 struct sched_param param;
	 param.sched_priority = sched_get_priority_min(SCHED_RR);
	 s = pthread_setschedparam(thread, SCHED_RR, &param);
//	 param.sched_priority = sched_get_priority_min(SCHED_OTHER)+5;
//	 s = pthread_setschedparam(thread, 3, &param);
	 if (s != 0) printf("pthread_setschedparam() didn't: %d \n",s);
      }
#endif

#if 0
      s = pthread_setschedprio(thread,5);	// only for _RR or _FIFO
      if (s != 0) printf("pthread_setschedprio() didn't: %d \n",s);
#endif

#if 0
      {
//	 pid_t tid = syscall(SYS_gettid);
	 s = setpriority(PRIO_PROCESS, 0, 10);
	 printf("-->%d   %d\n",s,getpriority(PRIO_PROCESS, 0));
      }
#endif

      //display_pthread_attr(thread,"   ");
   }
#endif

/* ******************************************************************** */
/* *************************** Garbage Man **************************** */
/* ********************* Back to VM thread land *********************** */
/* ******************************************************************** */

    // This is the ONLY entry point to start a collection cycle.
    // if waitUntilCollected, waits until the current gc cycle is done
    //   or starts another one and waits for it to finish
void collectGarbage(int waitUntilCollected,pVM vm)
{
//printf("-->%d %d\n",gcState,startGC);
//printf("              gc me  %d\n",waitUntilCollected);

int n = gcState;
again:
   caiSet(&startGC,1);		// kick the GCT (if sleeping)
   if (waitUntilCollected)
   {
      vmMightStall(vm,1);		// so calling VM can be collected
	 caiWait4(&startGC,0,1,0);	// and wait until startGC goes low
if (n > 0) { n = 0; vmMightStall(vm,0); goto again;  }
      vmMightStall(vm,0);
   }
}

    // .collect(wait=True) -->Void
static Instance *GM_collect(Instance *self,pArglist arglist,pVM vm)
{
   collectGarbage(arglistTryToGetBool(arglist,0,1,"GarbageMan.collect",vm),vm);
   return Void;
}

long ibucketCounts(char **gcTypeNames,	// allocate.c
		   long counts[][GC_NUM_TYPES+1]);

static void printGCcounts(int chop, long counts[][GC_NUM_TYPES+1])
{
   char	buf[200];
   int	i,t;
   long totals[GC_NUM_TYPES+1], num=0;

   memset(totals,0,sizeof(totals));

   *buf = '\0';
   for (t = 1; t < GC_NUM_TYPES; t++)
   {
      char tmp[20];
      if (chop && (t == I_UNTOUCHABLE || t == I_INVISIBLE)) continue;
      sprintf(tmp,"%9.8s",gcTypeNames[t]); strcat(buf,tmp);
   }

   printf("%10s%s%9s\n","Object Type",buf,"Total");
   for (i = 0; i < MAX_OBJ_TYPE; i++)
   {
      if (counts[i][GC_NUM_TYPES])
      {
	 *buf = '\0';
	 for (t = 1; t < GC_NUM_TYPES; t++)  // no zero itype
	 {
	    char tmp[20];
	    long n = counts[i][t];
	    if (chop && (t == I_UNTOUCHABLE || t == I_INVISIBLE)) continue;
	    sprintf(tmp,"%9ld",n); strcat(buf,tmp);
	    totals[t] += n; num += n;
	 }
	 printf("%10s:%s%9ld\n",typeToName(i),buf,counts[i][GC_NUM_TYPES]);
      }
   }

   printf("           ");
   for (t = chop; t < GC_NUM_TYPES; t++) printf("%9.8s","--------------");
   printf("\n");
   *buf = '\0';
   for (t = 1; t < GC_NUM_TYPES; t++)
   {
      char tmp[20];
      if (chop && (t == I_UNTOUCHABLE || t == I_INVISIBLE)) continue;
      sprintf(tmp,"%9ld",totals[t]); strcat(buf,tmp);
   }
   printf("%10s %s%9ld\n","",buf,num);
}

    // .stats([gc = False]) -->Void
static Instance *GM_stats(Instance *self, pArglist arglist, pVM vm)
{
   Instance *lists[] = { &mtOlympus, &instanceList };

   int		i,collect = arglistTryToGetBool(arglist,0,1,"",vm);
   long int	total,counts[MAX_OBJ_TYPE][GC_NUM_TYPES+1];
   Instance    *ptr;

   memset(counts,0,sizeof(counts));

   if (collect)		// collect and wait until collected
   { 
      printf("Collecting ...\n");
      collectGarbage(1,vm);
   }

   total = 0;
   spinLockAcquire(&sureLock);
      for (i = 0; i < sizeof(lists)/sizeof(Instance *); i++)
      {
	 for (ptr = lists[i]->nexti; ptr; ptr = ptr->nexti)
	 {
	    int itype = GC_TYPE(ptr);
	    int	t     = TYPEO(ptr);

	    counts[t][itype]++;
	    counts[t][GC_NUM_TYPES]++;		// total for this gcType
	    total++;
	 } // for
      } // for
   SPIN_LOCK_RELEASE(&sureLock);

   printf("GC Stats: %lu collections\n%ld malloc'd objects counted:\n",gcCount,total);
   printGCcounts(2,counts);


#if KEEP_OBJECT_COUNTS
//   spinLockAcquire(&sureLock);
      printf("\nInternal counts:\n");
      total = 0;
      for (i = 1; i < GC_NUM_TYPES; i++)
      {
	 printf("   %-11s %8d\n",gcTypeNames[i],gcTypeCounts[i]);
	 total += gcTypeCounts[i];
      }
      printf("   %11s --------\n","");
      printf("   %11s %8ld\n","",total);

      printf("\nInvisible counts:\n");
      total = 0;
      for (i = 0; i < MAX_OBJ_TYPE; i++)
      {
	 total += invisiCounts[i];
	 if (invisiCounts[i])
	    printf("   %-11s %8d\n",typeToName(i),invisiCounts[i]);
      }
      printf("   Total:      %8ld\n",total);
//   SPIN_LOCK_RELEASE(&sureLock);
#endif

   printf("\n");
   memset(counts,0,sizeof(counts));
   total = ibucketCounts(gcTypeNames,counts);
   printf("IBuckets: %ld objects counted:\n",total);
   printGCcounts(0,counts);

   return Void;
}

#if defined(__NEDMALLOC)
    // .mallocStats() -->Void
static Instance *mallocStats(Instance *self, pArglist arglist, pVM vm)
{
   #ifdef __NEDMALLOC
      #ifdef __USE_HEAPS
         nedpmalloc_stats(_zmheap);
      #else
         nedmalloc_stats();
      #endif
   #endif

   return Void;
}
#elif defined( __DLMALLOC)
	// .mallocStats()
static Instance *mallocStats(Instance *self, pArglist arglist, pVM vm)
{
   zmallocStats();
   return Void;
}
#elif defined(__linux__)
static Instance *mallocStats(Instance *self, pArglist arglist, pVM vm)
{
   malloc_stats();
   return Void;
}
#else
static Instance *mallocStats(Instance *self, pArglist arglist, pVM vm)
   { return Void; }
#endif

    // .sanityCheck([newValue]) -->Bool (new/current setting)
static Instance *GM_sanityCheck(Instance *self, pArglist arglist, pVM vm)
   { return GC_SANITY_CHECK ? BoolTrue : BoolFalse; }

    // .verify(i) -->Bool
static Instance *GM_verify(Instance *self, pArglist arglist, pVM vm)
{
   int s = instanceCheck(arglistGet(arglist,0,0,vm),1);
   return s ? BoolTrue : BoolFalse;
}

    // .gcType(i) -->T(#,text)
static Instance *GM_gcType(Instance *self, pArglist arglist, pVM vm)
{
   Instance *i     = arglistGet(arglist,0,0,vm);
   int       itype;
   #if USE_POINTER_INTS
      if (IS_PtrInt(i)) itype = 0;
      else
   #endif
   itype = i->itype;
   return tupleCreateX(vm,
       intCreate(itype,vm),kStringCreate(gcTypeNames[itype],0,I_OWNED,vm),ZNIL);
}


static const MethodTable gcMethods[] =
{
   "collect",		GM_collect,
//   "counts",		(pMethod)GM_counts,
   "stats",		(pMethod)GM_stats,
//   "schedule",		(pMethod)GM_schedule,
   "sanityCheck",	GM_sanityCheck,
   "verify",		GM_verify,
   "ibuckets",		(pMethod)ibucketStats,
   "mallocStats",	(pMethod)mallocStats,
   "gcType",		GM_gcType,
   0,			0
};


	//////////////////////// Properties /////////////////////

    // gcCount
static Instance *GM_gcCount(Instance *self,pVM vm)
   { return intCreate(gcCount,vm); }

static const PropertyTable properties[] =
{
   "gcCount",	GM_gcCount,
   0,		0
};

static ZKL_Object GarbageObject;
static Instance   GarbageMan;

static void WeakRefConstruct(void);	// below

void garbageManConstruct(void)	// do this whenever
{
   constructObject(&GarbageObject,NativeType, gcMethods,properties,0,NoVM);
   GarbageObject.name = "GarbageMan";
   instanceInit((Instance *)&GarbageMan,&GarbageObject,I_UNTOUCHABLE);
   vaultAdd("",(Instance *)&GarbageMan,NoVM);

   // zklConstruct() calls startGarbageMan().

   CAI_INIT(&gcState);

   WeakRefConstruct();
}

/* ******************************************************************** */
/* *********************************  ********************************* */
/* ******************************************************************** */

void gcConstruct(void)	// do this early, just cause
{
   mtOlympus.nexti = instanceList.nexti = 0;

   CAI_INIT(&startGC);
   CAI_INIT(&markingOrphan);
   CAI_INIT(&markingInProgress);
   CAI_INIT(&ibsize);
   spinLockInit(&sureLock);

   // zklConstruct() calls garbageManConstruct().
#if 0
{
// ulimit -Sv 209715200 --> 200 MB (virtual memory) min to make test
// depends on how gc is configured (over in allocate.c)
#include <sys/resource.h>
   struct rlimit rlim;
   getrlimit(RLIMIT_AS,&rlim);
   printf("-------------->%ul  %ul\n",rlim.rlim_cur, rlim.rlim_max);

   rlim.rlim_cur=1294967295;		// 4,294,967,295
   setrlimit(RLIMIT_AS,&rlim);

   getrlimit(RLIMIT_AS,&rlim);
   printf("-------------->%ul  %ul\n",rlim.rlim_cur, rlim.rlim_max);
}
#endif
}


/* ******************************************************************** */
/* **************************** Weak Refs ***************************** */
/* ******************************************************************** */

/* Cases:
 *   - WeakRef is in same "generation" as ref
 *   - WeakRef is newer than ref
 *     Ref pushed onto VM stack and forgotten (eg WeakRef(T(1))
 *     GC starts, WR allocated (ie goes on newbie list)
 *     return BEFORE this VM is marked
 *     --> ref never marked, reclaimed
 *     --> WR points to garbage
 *       newbies don't check refs because they don't know if they point to
 *       marked object, another newbie or dead object.
 *   X WeakRef is older than ref: Not possible
 *   - Both are "new" (created during GC)
 *   - Both are "old"
 * If both are are live, is it possible for WR to be marked but ref not?
 *   
 * The hard part, gc:  While gc is happening (and checking WR refs), new WRs
 * can be created.  Thus, when looking at a ref, and it isn't marked, how do
 * I tell if it is dead or newly created?
 */

static ZKL_Object WeakRefObject;

    // WeakRef.ref -->i
static Instance *WeakRef_ref(Instance *self, pVM vm)
   { return (Instance *)((WeakRef *)self)->ref; }

    // WeakRef.isDead -->Bool
static Instance *WeakRef_isDead(Instance *self, pVM vm)
   { return (((WeakRef *)self)->ref == Void) ? BoolTrue : BoolFalse; }

static const PropertyTable wrProperties[] =
{
   "ref",	WeakRef_ref,
   "isDead",	WeakRef_isDead,
   0,		0
};

    // GarbageMan.WeakRef.create(ref) --> Void|WR
    // ?WeakRef.create(ref,action): Do action when ref dies
/* action: set iflag, WR lives while marked || ref lives
 * if ref !marked, move action to Strand queue, mark action, kill link
 */
static Instance *WeakRef_create(Instance *self,pArglist arglist,pVM vm)
{
   Instance *ref    = arglistGet(arglist,0,".WeakRef",vm);
//   Instance *action = arglistTryToGet(arglist,1);
   WeakRef  *wr;

   	// weed out instances that don't get GC'd. That includes Void
   #if USE_POINTER_INTS
      if (IS_PtrInt(ref)) return Void;
   #endif
//!!!??? fcns are 2Special as are nested classes in copies of classes
   if (ref->itype != I_OWNED) return Void;

   if (!wrBuckets)	// WeakRef not constructed yet
   {
//!!! have to lock!!! or move to construct()
      constructObject(&WeakRefObject,NativeType, 0,wrProperties,0,vm);
      WeakRefObject.name        = "WeakRef";
      WeakRefObject.vaultPath   = "GarbageMan";
      WeakRefObject.isize       = sizeof(WeakRef);
      WeakRefObject.isBInstance = 1;

	// Have to be my own private buckets so I can gc them properly
      ibucketReserve(&WeakRefObject,50,&_wrBuckets,1,vm);
      wrBuckets = &_wrBuckets;
   }

      // WeakRefs are quasi containers
   wr = (WeakRef *)ibucketAllocate(wrBuckets,&WeakRefObject,I_INVISIBLE,1,vm);
   CAP_SET(&wr->ref,ref);	// updateWeakRefs() could be running right now
   containerIsCooked(wrBuckets,(Instance *)wr,I_OWNED);
      // in case GC is happening, make sure ref survives this cycle
   instanceIsOrphan(ref);	// don't gc ref before WR is markable
   return (Instance *)wr;
}

static void WeakRefConstruct(void)
{
   vaultAddData("GarbageMan.WeakRef",methodCreate(Void,0,WeakRef_create,NoVM),NoVM);
}


#if 0
/* ******************************************************************** */
/* ***************************** Vultures ***************************** */
/* ******************************************************************** */

// Vulture: An WeakRef that acts when the ref dies

    // WeakRef.create(ref,action) --> Void|True
static Instance *Vulture_create(Instance *self,pArglist arglist,pVM vm)
{
   Instance *ref = arglistGet(arglist,0,0,vm);
   Instance *vulture;

   vulture = WeakRef_create(self,arglist,vm);   //!!!! don't addToCollectables
   if (vulture == Void) return Void;
   vulture->iflag = 1;
   return BoolTrue;
}

mark * sweep
* == foreach vulture: 
   if ref is dead: 
      mark ref
      queue ref on "needs action" list/pipe
      kill vulture

thread/Strand/whatever: while(pop ref), ref()
   die with last ref
   adding ref kickstarts: vultureCulture.kickStart(pipe)


fcn vultureCulture(pipe)
{
   while(action := pipe.read(False)) { try{ action(); }catch {} }
}

class [static] VultureCulture
{
   fcn init
   {
if running: done
problem: BIG window between error & splashdown
BIG window between .launch() and liftoff()
      ?counts: >1 == already running
//      active pipe/list? Basically what I'm trying to do
      self.launch()
   }
   fcn liftOff
   {
      actionItems := GarbageMan.actionItems;
      while(action := actionItems.read(False)) { try{ action(); }catch {} }
   }
   fcn splashdown(h)
   {
      if (pipe) self.launch();
   }
}

Instance *actionItems = listCreate(MAX_WEAK_REFS,0x2,I_IMMORTAL,vm);

    // DO NOT run anything, just queue and signal
void dealWithThisDeadMeat(WeakRef *vulture)
{
   listAppend(actionItems,vulture->action,NoVM);	// better not GC
   instanceMark(vulture->ref);	// it rises from the grave to live again
   weakRefFree(vulture);	// but the vulture dies
   CAP_SET(&theVulturesAreCircling,1);
}

in vm main loop:
	// Try to make this cheap: Don't hit write barrier unless have to
   if (theVulturesAreCircling && 1 == CAS(&theVulturesAreCircling,1,0)) 
   {
      runInSandBox(VultureCulture,NoArglist, 0,vm);
   }

#endif
