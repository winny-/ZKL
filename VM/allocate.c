/* allocate.c : Allocate space for Instances and Objects
 * 
 * Copyright (c) 2006,2007,2008-12,2013 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

/* Note:  Minimize the use of malloc()!  On windows, with the threaded
 * library, it is EXTREMELY slow.  For example, running the test suite and
 * malloc()ing and free()ing the block stack for each VM took between 300
 * and 500 sec.  Just malloc()ing once per VM took the time down to 24sec.
 * Also, it looks like malloc effectively shut down one core of my X2.
 * Actually, it is probably the debuggable malloc.
 */

// glibc's malloc per thread arenas (limited by #cores*K) & word size (32/64)
// 32 bit, K==2, 64 bit, K==8 (hence the 63 seen on my 4/4 system).

/* OK, I seriously confused:  One threaed, allocate a bazillion of same
 * BInstances, not heinous, gc to clear them.  Repeat, NO allocates, NO gc,
 * just resue buckets, time is heinous (10x).
 * List.createLong(4000000,fcn{ 1.2 },True)  1.4 sec --> 15.5 sec
 * Only thing I can think of is memory thrash, something below me.
 */

#if 0
#elif defined(__unix__)
   #ifdef __linux__
      #include <malloc.h>
   #endif

struct mallinfo mallinfo(void);
// only gives info on main arena, tests use >60 arenas, values may be roll over

//malloc_stats();	// prints stats on all arenas
#endif


#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#include <stdio.h>
#include <string.h>

#define __NOT_A_DLL
#define __GC_INTERNALS
#define __NEED_PTHREADS

#include "zklObject.h"
#include "zklAtomic.h"
#include "zklList.h"
#include "zklMemory.h"
#include "zklUtil.h"

/* Some hints on when I should kick the garbage collector. Over in gc.c
 * there is a timer and we can tell it to gc as soon it can.
 * I'd like to use a macro but __SIZEOF_POINTER__ probably is isn't
 * everywhere (ie I don't know what the hell the equivalent is on Windows)
 * so I'll do it down in allocateConstruct().  I don't know if I care if a
 * 32 bit exe is put on a 64 bit machine.
 * And I'll count Instances allocated because sometimes they malloc also (eg
 * Data, BigInt) which means I don't know total memory allocated.
 * Yes, it would be nice to query malloc directly but eg mallinfo() (glib)
 * is too limited.
 */

//!!! probably should reset counters at gc time, not at allocate time

//#if __SIZEOF_POINTER__==8
#define TIME_TO_GC (2<<20) // gc if have allocated that much

//static unsigned TIME_TO_GC;	// since I'm not using a compiler macro

#define AIPORK 0	 // atomic int or int? seems to be a wash
#if AIPORK
   CAtomicInt pork;  
#else
   static unsigned pork=0;
#endif

    // count of Instances allocated.
    // 'cause don't know how much pork is malloc'd, eg BigInts
#define COUNT_TO_GC 5000
static unsigned icount=0;

#if 0
    /* Returns 0 if out of memory. You MUST handle this case!
     */
ZKL_Object *objectAllocate(size_t size)	// used for embryos
{
   ZKL_Object *object = (ZKL_Object *)ZCALLOC(1,size); // for ease of debugging
   return object;
}

void objectFree(Object *obj) { ZFREE(obj); }
#endif

    // This works for both Instances & BInstances
__inline Instance *instanceInit(Instance *self, ZKL_Object *object, int itype)
{
   switch(itype)
   {
      default: vmHalt("instanceInit: bad itype");   // basically assert
      case I_UNTOUCHABLE: case I_INVISIBLE: case I_IMMORTAL: case I_OWNED:
      case I_SPECIAL:     case I_2SPECIAL:  break;
   }

   self->objID  = object->id;
   self->itype  = itype;
   self->gcMark = 0;
   self->iflag  = self->iflag2 = 0;

   return self;
}

#if defined(__USE_HEAPS)	// no connection to zmalloc.c
//!!!!!!!!!!!!!! make ALL zmalloc stuff GC if run out of mem
   nedpool *_zmheap;
   static void heapConstruct(void)
   {
      _zmheap = nedcreatepool(111000,6);	// 100k
   }
   void *zmalloc(size_t size) { return nedpmalloc(_zmheap,size); }
   void *zcalloc(size_t num, size_t size) { return nedpcalloc(_zmheap,num,size);  }
   void *zrealloc(void *ptr,size_t size)  { return nedprealloc(_zmheap,ptr,size); }
   void zfree(void *ptr)      { nedpfree(_zmheap,ptr); }
#endif


    /* Throws OutOfMemory if can't find memory to allocate.  If throw==0,
     *   returns 0 and You, yes YOU, MUST handle this case!  Most likely by
     *   cleaning up and throwing OutOfMemory.  If you need to, you can also
     *   do clean up via a Fence.
     * YOU have ALREADY constructed object!
     * This is hit by a lot of threads.
     * Don't addToCollectables because the caller may need to initialize
     *   a container before a marker can look at it.
     * New instance is I_INVISIBLE.
     */
Instance *instanceAllocate(size_t size, ZKL_Object *object, int throw, pVM vm)
{
   Instance *instance;

   #ifdef __unix__   // usleep() is micro-seconds, sched_yield() doesn't work
      if (gcState == 4) usleep(10);
      else if (gcState == 3) usleep(1);
      else if (gcState > 0) SWITCH_THREADS;
   #elif _MSC_VER	// Sleep is milli-seconds
      if (gcState == 4) SWITCH_THREADS;
      // http://blogs.msdn.com/cellfish/archive/2008/09/17/sleep-less-than-one-millisecond.aspx
   #endif

   instance = (Instance *)ZCALLOC(1,size);  // because it is a good thing to do
   if (!instance)
   {
      collectGarbage(1,vm);			// wait
      instance = (Instance *)ZCALLOC(1,size);	// retry
      if (!instance)				// really out of memory
      {
	 if (throw) vmThrow(vm,E_OUT_OF_MEMORY,0);
	 return 0;
      }
   }

   instanceInit(instance,object,I_INVISIBLE);
   
   icount++;
#if AIPORK
   CAI_SET(&pork,CAI_VALUE(&pork) + size);
   if((unsigned)CAI_VALUE(&pork) > TIME_TO_GC || icount>COUNT_TO_GC)
   {
//printf("--------------------------> signal instance gc %u  %u\n",pork,icount);
      CAI_ZERO(&pork);
      icount = 0;
      collectGarbage(0,vm);	// signal for a collection
   }
#else
   pork += size;
   if(pork > TIME_TO_GC || icount>COUNT_TO_GC)
   {
//printf("--------------------------> signal instance gc %u  %u\n",pork,icount);
      pork = icount = 0;
      collectGarbage(0,vm);	// signal for a collection
   }
#endif

   return instance;
}

/* ******************************************************************** */
/* ************************ Instance Buckets ************************** */
/* ******************************************************************** */

    /* This is a cooperative allocater for fixed sized objects that have a
     * lot of churn.
     * You, the caller, knows lots about the object: frequency of use, size,
     * etc.
     * Thread safe. However, for heavily used objects, it would be better
     * for each thread to have its own buckets.
     * 
     * IBucketHeader: Some global bucket info, pointer to buckets
     * 
     * IBucket: (N = blobSize) >= sizeof(Instance *)
     *    i: [N bytes][N bytes] ... blobsPerBucket, 
     *       just clumps of bits with an address at the start of each clump
     *       [-->0][<--][<--] .... how free list is initialized
     *       The start of each free Blob slot is a pointer to the next
     *		free Blob
     *    free --> &i[blobsPerBucket-1]
     * On one hand, it seems pretty silly to have a per bucket free list (vs
     * one freelist in the header), on the other, it gives me the ability to
     * free a empty bucket or other per bucket stuff.
     * 
     * Linux profiling shows that, under some loads, ibucketFree() takes a
     * LOT of time. Burst mode is an attempt to address this. Of course,
     * that just moves ibucketFreeThemAll() to #1.
     * The test suite seems to be basically all about allocataion/free.
     */

    /* Usage cases:
     * 1) The usual case (KString):
     *    - Use BInstance instead of Instance and save some space.
     *      struct { BInstance instance; ... } MyInstance;
     *    - Set MyObject.isBInstance to 1 in your object
     *    - Don't set MyObject.freeMe
     *    - Set MyObject.isize to sizeof(MyInstance)
     *    - Call ibucketReserve() to get some buckets
     *    - Call ibucketAllocate() instead of instanceAllocate()
     * 2) You want to use buckets but your instance isn't that heavily used
     *    so you'd rather leverage somebody elses buckets (Float).
     *    - Do the above.
     *    - Create a static IBucketHeader _myBuckets;
     *    - myBuckets = ibucketHitchHike(&MyObject,slop,N,&_myBuckets)
     *      This tries to find an existing bucket that you can use (slop is
     *      the amount of wasted space you'll tolerate). If nothing is
     *      available, a new set of buckets is created.
     * 3) You need to do some clean up when your instance is freed
     *    (Dictionary).
     *    - As above but set MyObject.freeMe
     *      MyObject.freeMe = myFree; // type _FreeMe
     *      static int myFree(MyInstance *self) { // called from Bucket GC
     *         clean up
     *         return 1;
     *      }
     *      DO NOT call a *free(), GC will do that.
     * 3b) Your object is a container and needs to mark its contents during
     *     GC (Dictionary).
     *     - MyInstance.magicMarker = myMarker; // type _MagicMarker
     *       static void myMarker(Instance *self) { instanceMark(x) }
     * 4) You want use a specific set of already allocated buckets.  Just
     *    because (Int, AtomicBool)
     *    - (1) or (3).
     *    - Call ibucketPoach(theirBuckets,&MyObject) to make sure this is
     *      OK.
     * 5) For some reason, you can't use BInstance
     *    - Don't set MyObject.isBInstance.
     *    - To free:
     *      MyObject.freeMe = myFree; // type _FreeMe
     *      static int myFree(MyInstance *self) { // called from Instance GC
     *         clean up
     *         ibucketFree(myBuckets,self);
     *         return 1;
     *      }
     *    - See (3)
     *    - You can try to share buckets (by using ibucketHitchHike()).
     *    - You can share these buckets with non-Instance buckets.
     *    - One reason you might want to do this is to hitch hike or poach
     *      a non GC'd set of buckets.
     * 5b) An [not B] Instance that wants to poach/hitch hike BInstance
     *     buckets (ShortTuple, ShortSring).
     *    - No problem, Buckets don't care if you use extra space (nexti
     *      will not be used).
     *    - Instance becomes a BInstance (as far as buckets are concerned)
     *      and lives in the bucket list, not GCs instanceList.
     *    - Lie and set MyObject.isBInstance
     *    - You don't need a freeMe unless you need one.
     *    - REMEMBER that this instance really is a BInstance with some
     *      extra baggage. But it can be treated as an Instance.
     * 6) My blob isn't an Instance/BInstance (ie not an Object) (Thread)
     *    - Call bucketReserve() to get your buckets. Bucket GC is turned
     *      off for these buckets although Instance GC will work (if shared
     *      with Instances).
     *    - Use ibucketAllocate() to allocate, pass in 0 for Object.
     *    - You'll have to manually gc. Use bucketFree().
     *    - You can share these buckets with non-BInstance buckets.
     * 7) Use GarbageMan.ibuckets() to see how buckets have been laid out.
     */

#define  __IB_BULK_FREE 0  // waaaaaay out of date


    // Restrict the # of Instances per bucket to fit in i->nextb
    // so I can do per bucket GC
    // 15 bits so I can pack with bpflag if I need/want to
#define MAX_INSTANCES_PER_BUCKET	30001

typedef struct Blob	// create a linked list through the BlobSphere
{
   struct Blob *next;
   // imaginary padding to blobSize
} Blob;

typedef struct IBucket
{
   struct IBucket *nextBucket;
   Blob		  *freeList;	// free list, zero if bucket is full
   unsigned int    liveList;	// index to first live Blob, if BInstance
   unsigned int    newbies;	// BInstances created during GC
   BInstance	  *lastNewbie;	// because it's easy
   Byte		   bits[0];	// fake field for ease of coding
} IBucket;


#define IN_BUCKET(blob,bucket,bsize)	\
    ( (Byte *)bucket < (Byte *)blob && (Byte *)blob < ((Byte *)bucket) + bsize )

    // address --> index
#define BREDUCE(bh,bucket,blob)		\
	(unsigned int)(((Byte *)blob - bucket->bits) / bh->blobSize)
    // index --> address: &array.BInstanceOf<type>[n]
#define BEXPAND(bh,bucket,nextb)	\
	(BInstance *)(bucket->bits + (nextb * bh->blobSize))

    // Call with header locked, not [quite] re-entrant
    // Returns zero on malloc failure
static IBucket *balloc(IBucketHeader *header)
{
   size_t   bSize, n;
   IBucket *bucket;
   Blob	   *ptr, *next;

   	// I *ASSUME* padding is correct
   bSize  = header->blobSize;
   n	  = header->blobsPerBucket;
   bucket = ZCALLOC(1,header->bucketSize);  // so first use of bucket is known
   if (!bucket) return 0;

   	// create free list, first item is highest address
   for (ptr = (Blob *)&bucket->bits, next = 0; n--; 
        ptr = (Blob *)((Byte *)ptr + bSize))
   {
      ptr->next = next;		// point to previous Instance (lower address)
      next	= ptr;		// next Instance (higher address)
   }
   bucket->freeList = next;	// all Blobs are free
   // liveList is pointer (if gc'able) or count
   return bucket;
}

    // Bucket headers are not allocated, they live in C code
static IBucketHeader volatile *ibucketHeaderChain = 0;
static SpinLock bLock;

    // Thread safe
    // Does NOT allocate
    // Pass in a STATIC header to be filled in
IBucketHeader *bucketReserve(
int blobSize,int instancesPerBlock,IBucketHeader *header, int isPrivate, pVM vm)
{
   if (blobSize < sizeof(Blob))
      vmThrow(vm,E_ASSERTION_ERROR,"bucketReserve: size too small");

   if (instancesPerBlock >= MAX_INSTANCES_PER_BUCKET)
      instancesPerBlock = MAX_INSTANCES_PER_BUCKET - 1;

   header->buckets	  = 0;
   header->blobSize	  = blobSize;
   header->blobsPerBucket = instancesPerBlock;
   header->bucketSize	  = sizeof(IBucket) + blobSize*instancesPerBlock;
   header->numBuckets	  = 0;
   header->pleaseGCme	  = 0;	// 1 if buckets are BInstances, 0 if blob
   header->private	  = isPrivate;
   #if __IB_BULK_FREE
      header->freeList      = 0;
      header->toBeFreedList = 0;
      header->lastb	    = 0;
   #endif
   spinLockInit(&header->lock);

   	// Prepend this header to the header chain in a thread safe manner
	// 1. Serialize threads adding headers
	// 2. Atomically prepend header to chain
	// Other threads looking at chain will or won't get new header,
	//   it doesn't matter which because this header is initalized & 
	//   isn't in use yet.
	// VC7, WinBase.h has a screwy prototype that causes warnings
   SPIN_LOCK_ACQUIRE(&bLock);
      header->next = (IBucketHeader *)ibucketHeaderChain;
      #if defined(_MSC_VER)
         CAP_SET((PVOID volatile *)&ibucketHeaderChain,header);
      #else
         CAP_SET(&ibucketHeaderChain,header);
      #endif
   SPIN_LOCK_RELEASE(&bLock);

   return header;
}

IBucketHeader *ibucketReserve(ZKL_Object *obj,
int instancesPerBlock, IBucketHeader *header, int isPrivate, pVM vm)
{
   int blobSize = obj->isize;

   if (!blobSize)	// since I tend to make this error
      vmThrow(vm,E_ASSERTION_ERROR,"ibucketReserve: Set object->isize");

   bucketReserve(blobSize,instancesPerBlock,header,isPrivate,vm);
   if (obj->isBInstance) header->pleaseGCme = 1;
   return header;
}

    // Share a BucketHeader with somebody who has already reserved it.
    // Bucket size <= blobSize <= Bucket size + slop, best fit
    // Returns zero if can't find a fit, 
    //   you'd probably rather use ibucketHitchHike()
IBucketHeader *ibucketShare(ZKL_Object *obj,int slop)
{
   IBucketHeader *bh,*closest = 0;
   int		  d = 100, blobSize = obj->isize;
   int		  pleaseGCme = obj->isBInstance;

   for (bh = (IBucketHeader *)ibucketHeaderChain; bh; bh = bh->next)
   {
      int n = bh->blobSize - blobSize;

      if (bh->private) continue;
      if (bh->pleaseGCme != pleaseGCme) continue;

      if (n == 0) { closest = bh; break; }	// exact fit

      if (n >= 0 && n <= slop && n < d)
      {
	 d	 = n;
	 closest = bh;
      }
   }
   return closest;
}

    // Share if there is a good fit, otherwise reserve a new header
IBucketHeader *ibucketHitchHike(ZKL_Object *obj,
	int slop, int numBlobsPerBucket, IBucketHeader *header, pVM vm)
{
   IBucketHeader *bh;

   if ((bh = ibucketShare(obj,slop)))	// found a buddy
   {
      	// Not using the passed in header, zero it in case they try to use it
        // Can't copy bh to header, that just doesn't work
      memset(header,0,sizeof(IBucketHeader));	// for debugging
      return bh;
   }
   return ibucketReserve(obj,numBlobsPerBucket,header,0,vm);
}

    /* If you are going to share buckets without going through
     * ibucketHitchHike() or ibucketShare(), call this routine to verify
     * that the buckets and your object are compatible.
     */
void ibucketPoach(IBucketHeader *bh, ZKL_Object *obj, pVM vm)
{
   unsigned int blobSize = obj->isize;

   if (!blobSize) 
      vmThrow(vm,E_ASSERTION_ERROR,"ibucketPoach: Set object->isize");

   if (bh->blobSize < blobSize)
      vmThrow(vm,E_ASSERTION_ERROR,
	      "ibucketPoach: Instance bigger than bucket");
   if (bh->pleaseGCme != obj->isBInstance)
      vmThrow(vm,E_ASSERTION_ERROR,"ibucketPoach: isBInstance mismatch");
//      obj->isBInstance = bh->pleaseGCme;
}

    /* Bucket version of instanceAllocate(), created I_INVISIBLE
     * If not really an a bucket of instances (see Thread), object is zero
     * Instances on the free list won't be marked or swept, however, marking 
     *   and sweeping is occuring at this very moment.
     * If GC is happening, partition the binstance heap so I can deal with
     *   instances too new to get marked and new containers getting old
     *   instances.  I'll get false positives but that is OK, next cycle
     *   will get them.
     * If a bucket holds Objects, allocate will ZERO the new instance
     *   otherwise, allocated blob isn't touched.
     */
//static int porkSoda = 0;
Instance *ibucketAllocate(IBucketHeader *bh, 
			  ZKL_Object *object, int itype, int throw, pVM vm)
{
   IBucket  *bucket;
   Blob	    *b;
   Instance *i;
   int	     refried;

#if 0
   if (porkSoda)
   {
      porkSoda = 0;
      collectGarbage(0,vm);  // a request that might be, and often is, ignored
   }
#endif

   refried = 0;
again: ;
   SPIN_LOCK_ACQUIRE(&bh->lock);
      #if __IB_BULK_FREE
         if (bh->freeList)
	 {
	    b = bh->freeList;
	    i = (Instance *)b;
	    bh->freeList = b->next;
	    goto gotI;
	 }
      #endif
//!!! if allocating from last bucket & bucket is 80% full, gc. 
// how count blobs in bucket? or just assume sweep() is called often enough
// and let it count
      for (bucket = bh->buckets; bucket; bucket = bucket->nextBucket)
	 if (bucket->freeList)	// this bucket has some space
	    goto gotBucket;

	 /* Out of buckets, allocate another one
	  * It would seem like a good idea to say, hey, just had to get a
	  * new bucket, next time, see there is some garbage in younger
	  * buckets. Unless there is somebody who is allocating a ton of the
	  * same thing and thing go to hell.
	  * List.createLong(4000000,fcn{ 1.2 },True)
	  */
//      porkSoda = 1;  // GC in case buckets in case of bucket churn
//printf("porkSoda  111111111111111111111111 new bucket\n");
      if ((bucket = balloc(bh)))
      {
	 /* Put allocated bucket at front of list so bucket list acts like
	  * nursury (ie new bucket Instances die fast, first bucket will most
	  * likely have space. Probably pretty short lived goodness since I
	  * don't migrate Instances towards last bucket (generations)
	  */
	 bucket->nextBucket = bh->buckets;
	 bh->buckets	    = bucket;
	 bh->numBuckets++;
	 if (bh->pleaseGCme)
	 {
	    bucket->liveList   = MAX_INSTANCES_PER_BUCKET; // no live Blobs
	    bucket->newbies    = MAX_INSTANCES_PER_BUCKET; // no newbies
	    bucket->lastNewbie = 0;			   // no newbies
	 }
	 else bucket->liveList = 0;		// live count
      }
      else	// malloc failure
      {
	 SPIN_LOCK_RELEASE(&bh->lock);
	 if (!refried)	// one try
	 {
	    collectGarbage(1,vm);	// wait, sets stall flag
	    refried = 1;
	    goto again;
	 }
	 if (throw) vmThrow(vm,E_OUT_OF_MEMORY,0);
	 return 0;
      }

      // signal actually had to malloc
#if AIPORK
      CAI_SET(&pork,CAI_VALUE(&pork) + bh->bucketSize);
      if((unsigned)CAI_VALUE(&pork) > TIME_TO_GC)
      {
//printf("--------------------------> signal bucket gc  %u\n",pork);
	 CAI_ZERO(&pork);
	 collectGarbage(0,vm);	// signal for a collection
      }
#else
      pork += bh->bucketSize;
      if(pork > TIME_TO_GC)
      {
//printf("--------------------------> signal bucket gc  %u\n",pork);
	 pork = 0;
	 collectGarbage(0,vm);	// signal for a collection
      }
#endif

   gotBucket: ;
      b = bucket->freeList;
      i = (Instance *)b;
      bucket->freeList = b->next;

      #if GC_SANITY_CHECK
	 if (!IN_BUCKET(i,bucket,bh->bucketSize))
	    vmHalt("CORRUPT ibucket free list");
      #endif

//   gotI: ;
      if (object)	// flick bits while ibucketSweep() can't
      {
	 memset(i,0,bh->blobSize);	// calloc()
	 instanceInit(i,object,itype);
// !!!flick iflags?

	 if (bh->pleaseGCme)
	 {
	    BInstance *bi = (BInstance *)i;

	    if (IS_CONTAINER(i)) i->itype = I_INVISIBLE; // contents unmarkable
	    if (gcState == 0)	// GC is not happening
	    {
	       bi->nextb = bucket->liveList; // 1st is MAX_INSTANCES_PER_BUCKET
	       bucket->liveList = BREDUCE(bh,bucket,bi);
	    }
	    else	// put in "new" partition
	    {
	       bi->nextb       = bucket->newbies;	// "backwards" list
	       bucket->newbies = BREDUCE(bh,bucket,bi);
	       if (!bucket->lastNewbie) bucket->lastNewbie = bi;
	    }
	 }
      }
      else bucket->liveList++;	// "don't gc me" buckets
   SPIN_LOCK_RELEASE(&bh->lock);

   return i;
}

Instance *iHazBucket(ZKL_Object *obj,int sz,int slop, 
		     int itype, IBucketHeader **pbh, pVM vm)
{
   IBucketHeader *bh, *closest = 0;
   int		  minDiff = 100;

   for (bh = (IBucketHeader *)ibucketHeaderChain; bh; bh = bh->next)
   {
      int n = bh->blobSize - sz;

      if (bh->private)     continue;
      if (!bh->pleaseGCme) continue;

      if (n == 0) { closest = bh; break; }	// exact fit

      if (n > 0 && n <= slop && n < minDiff)
      {
	 minDiff = n;
	 closest = bh;
      }
   }
   if (closest)
   {
      if (pbh) *pbh = closest;
      return ibucketAllocate(closest,obj,itype,1,vm);
   }
   return 0;
}

   // IMPORTANT! call this BEFORE you exit your method!
   /* Since buckets are global, GC can mark/sweep them while you are in a
    * method.  If allocate during GC, new i is put on a newbie list.
    * Containers are allocated as INVISIBLE so marking won't poke at
    * uninitialized contents.  At sweep (after mark), newbies are moved to
    * the live list UNLESS the newbie is INVISIBLE.  Which means you need
    * to fence when creating composite objects.
    */
Instance *containerIsCooked(IBucketHeader *bh, Instance *i, int itype)
{
   if (!OBJECT1(i)->isBInstance)  return(i);
   if (GC_TYPE(i) != I_INVISIBLE) return(i);

   // We know only one thread owns i, and it is in a Method 
   // so GCT won't be marking i but can see it.
   // Sweep & lookAgain ARE looking at i
   // so, if I lock the bucket i is in, neither can see or touch it

   SPIN_LOCK_ACQUIRE2(&bh->lock);
      i->itype = itype;		// can not be done atomicly, is a bit op
   SPIN_LOCK_RELEASE(&bh->lock);   
   if (!IS_CONTAINER(i)) return(i);
   if (itype == I_IMMORTAL) immortalBInstance(i);
   return i;
}

#if __IB_BULK_FREE
#if 0
    // Only called from the GC thread, no locking
void ibucketFree(IBucketHeader *bh, Instance *i)
{
   Blob *b = (Blob *)i;

   if (bh->pleaseGCme) return;		// this bucket will free itself

   b->next = bh->toBeFreedList;
   bh->toBeFreedList = b;
   if (!bh->lastb) bh->lastb = b;
}
#endif

    /* Only called from the GC thread
     * Compiling the Parser (Windows): 24k to 68k, 27 calls, average: 33,928
     *  - The numbers on a Linux 2x faster box are pretty much the same
     *  - There looks to be a fair amount of per bucket clustering, I'm
     *    seeing a range of 1k - 6k.  Given the small number of buckets this
     *    isn't surprising but it is a [small] win to check. The clustering
     *    drops a bit with repeated compiling.
     *  - Burst free gives me a 1.5% speed up compiling the parser.
     *  - Linux: the cluster check might be a benefit compiling the parser
     *    (numbers are ambiguous) but running the test suite, it is huge:
     *    25sec vs 35.
     *  - This all suggests that the per bucket free list sucks.  And it
     *    does.  Moving to a global free list is quite the improvment.
     */
void ibucketFreeThemAll(void)
{
   IBucketHeader *bh;

   for (bh = (IBucketHeader *)ibucketHeaderChain; bh; bh = bh->next)
   {
      Blob *b = bh->toBeFreedList;	// only called from GCT, thread safe

      if (!b) continue;
      SPIN_LOCK_ACQUIRE2(&bh->lock);	// don't nobody allocate
	 ((Blob *)(bh->lastb))->next = bh->freeList;
         bh->freeList	   = b;
	 bh->toBeFreedList = 0;
	 bh->lastb	   = 0;
      SPIN_LOCK_RELEASE(&bh->lock);
   }
}
#endif	// __IB_BULK_FREE

    // Re-entrant. Manually free one blob at a time if no GC
    // No BInstances in here, pleaseGCme == 0
void bucketFree(IBucketHeader *bh, void *blob)
{
   IBucket *bucket, *next, *pb;
   size_t   bsize;
   int      once = 1;   // only one empty bucket may be freed

   if (bh->pleaseGCme) return;		// this blob will free itself

   SPIN_LOCK_ACQUIRE2(&bh->lock);
      bsize = bh->bucketSize;
      for (pb = 0, bucket = bh->buckets; bucket; pb = bucket, bucket = next)
      {     // if i is in this bucket, add it to the free list
	 next = bucket->nextBucket;
	 if (IN_BUCKET(blob,bucket,bsize))
	 {
	    ((Blob *)blob)->next = bucket->freeList;
	    bucket->freeList     = blob;
	    bucket->liveList--;
	    bsize	         = 0;	// for sanity checking
//	    break;			// all done
	 }
#if 1
	 // if the bucket is empty, I might just free it
	 if (once && pb && bucket->liveList == 0)
	 {
	    IBucket *nb = bucket->nextBucket;
	    bh->numBuckets--;
	    pb->nextBucket = nb;
	    ZFREE(bucket);
	    once = 0;
//	    if (!nb) break;	// last bucket
	    next = nb;		// pb will no longer be looked at
	 }
	 if (!bsize) break;	// blob was freed
#endif
      } // for
   SPIN_LOCK_RELEASE(&bh->lock);
   #if GC_SANITY_CHECK		// not able to free i?
      if (bsize) vmHalt("bucketFree(): blob not found");
   #endif
}

    /* Walk all buckets and mark visible containers
     * See gc.c:lookAgain() for why
     * Can't merge with sweep because I have to mark Instances in additon to 
     *   BInstances.
     * Numbers:
     *   - With newbies:
     *     - Compiling the parser, I'm marking ~30 containers,
     *       while looking at ~200 instances.
     *     - Tests: 200-1,000 / 1,500-4,000
     *   - Everybody:
     *     - Parser: 20-80 / 40,000-70,000
     *     - Tests: 100-300 / 45,000-75,000
     * The time diff between just marking newbies vs everybody is pretty
     *   small (2 core) but time is time.
     * Have to be careful:
     *    Mark phase happens
     *    Allocate container, it is un-initialized
     *    lookAgain() is called
     *    Can't mark container as it contains garbage
     *    But it won't be marked (in a mark phase) until the method returns
     */
void ibucketLookAgain(IBucketHeader *wrBuckets)
{
   IBucketHeader *bh;

   for (bh = (IBucketHeader *)ibucketHeaderChain; bh; bh = bh->next)
   {
      IBucket *bucket, *pb;
      int      isWR = (bh == wrBuckets);

      if (!bh->pleaseGCme) continue;

      SPIN_LOCK_ACQUIRE2(&bh->lock);
	 for (pb = 0, bucket = bh->buckets; bucket;
	      pb = bucket, bucket = bucket->nextBucket)
	 {
	    unsigned int  n;
	    BInstance    *i;

	    for (n = bucket->newbies;
		 n != MAX_INSTANCES_PER_BUCKET; n = i->nextb)
	    {
	       i = BEXPAND(bh,bucket,n);
	       if (isWR) instanceMark(((WeakRef *)i)->ref);
	       else if (!GC_IS_MARKED(i) && 
			IS_CONTAINER(i) && i->itype != I_INVISIBLE)
		  instanceMark((Instance *)i);
	    } // for newbies
	 } // for buckets
      SPIN_LOCK_RELEASE(&bh->lock);
   } // for bucketChain
}

    // Called during GC by the GCT
    // Note: Both the WR & ref may newbies
void updateWeakRefs(IBucketHeader *wrBuckets)
{
   if (!wrBuckets || !wrBuckets->buckets) return;
   SPIN_LOCK_ACQUIRE2(&wrBuckets->lock);
   {
      IBucket *bucket;
      for (bucket = wrBuckets->buckets; bucket; bucket = bucket->nextBucket)
      {
	 BInstance    *i;
	 Instance     *r;
	 unsigned int  n;
	 WeakRef      *wr;

	    // don't check newbies (see gc.c WeakRef for why)
	 for (n = bucket->liveList; n != MAX_INSTANCES_PER_BUCKET; n = i->nextb)
	 {
	    i = BEXPAND(wrBuckets,bucket,n);
	    if (i->itype == I_INVISIBLE) continue;  // not initialized yet
	    wr = (WeakRef *)i; r = (Instance *)wr->ref;
	    // if ref not marked, dissolve bond between WR & ref
	    // if WR not marked, it don't matter
	    // Void is I_UNTOUCHABLE and never marked
	    // Another thread might be creating WR at the moment!
	    if (r && r != Void && !GC_IS_MARKED(r))
	       CAP_SET(&wr->ref,Void); // atomic change
	 } // bucket
      } // wrBuckets
   }
   SPIN_LOCK_RELEASE(&wrBuckets->lock);
}

    // GC BucketHeaders that wants to be GC'd
    // Only called from GCT
void ibucketSweep(void)
{
   IBucketHeader *bh;

   for (bh = (IBucketHeader *)ibucketHeaderChain; bh; bh = bh->next)
   {
unsigned long space,inUse;
      IBucket *bucket, *pb;
      int      once = 1;   // only one empty bucket may be freed (per header)

      if (!bh->pleaseGCme) continue;	// blobs, not Instances

inUse = 0;
      SPIN_LOCK_ACQUIRE2(&bh->lock);
	 for (pb = 0, bucket = bh->buckets; bucket;
	      pb = bucket, bucket = bucket->nextBucket)
	 {
	    BInstance    *prev, *i;
	    unsigned int  n, next;
	    Blob         *freeList;

	    	// if the bucket is empty, I might just free it
	    if (once && pb && bucket->liveList == MAX_INSTANCES_PER_BUCKET &&
		!bucket->lastNewbie)
	    {
	       IBucket *nb = bucket->nextBucket;
	       bh->numBuckets--;
	       pb->nextBucket = nb;
	       ZFREE(bucket);
	       once = 0;
	       if (!(bucket = nb)) break;   // the last bucket has been freed
	       // now process the bucket after the empty one
	    }

	    freeList = bucket->freeList;
	    // remove dead instances from live list
	    for (prev = 0, n = bucket->liveList;
		 n != MAX_INSTANCES_PER_BUCKET; n = next)
	    {
	       i    = BEXPAND(bh,bucket,n);
	       next = i->nextb;

	       if (!GC_IS_MARKED(i) && i->itype == I_OWNED)
	       {	// garbage, convert to Blob and put on free list
		  Blob    *b = (Blob *)i;
		  _FreeMe  freeMe;

		      /* Not a noticeable diff (speed or mem) if I bypass
		       * this check (which I can do by looking to see if any
		       * obj in bucket has a freeMe)
		       */
//!!!??? memlists?
		  if ((freeMe = FREE_ME(i))) freeMe((Instance *)i);

			// remove i from liveList
		  if (prev) prev->nextb      = i->nextb;
		  else      bucket->liveList = i->nextb;	// first item
		  // don't change prev

		  	// and add it to the free list
			// changes from BInstance to Blob
		  b->next  = freeList;
		  freeList = b;
	       }
	       else	// i lives! Get ready for next mark phase
	       {   // also UNTOUCHABLE, IMMORTAL, INVISIBLE, SPECIAL, SPECIAL2
	       	  GC_CLEAR_MARK(i);
		  prev = i;
inUse++;
	       }
	    } // for liveList

	    bucket->freeList = freeList;

	    if (bucket->lastNewbie)  // move newbies to liveList
	    {
//!!!!???? do INVISIBLE newbies need to stay on the newbie list? YES!
#if 1
	       	// first, clear marks set by ibucketLookAgain() (argh!)
	       for (n = bucket->newbies;
		    n != MAX_INSTANCES_PER_BUCKET; n = i->nextb)
	       {
		  i = BEXPAND(bh,bucket,n);
		  GC_CLEAR_MARK((Instance *)i);
	       }
	       bucket->lastNewbie->nextb = bucket->liveList;
	       bucket->liveList		 = bucket->newbies;
	       bucket->newbies		 = MAX_INSTANCES_PER_BUCKET;
	       bucket->lastNewbie	 = 0;
#else
	       	// don't move INVISIBLE containers
	       for (n = bucket->newbies, prev = 0;
		    n != MAX_INSTANCES_PER_BUCKET; n = next)
	       {
		  i    = BEXPAND(bh,bucket,n);
		  next = i->nextb;
		  if (!IS_CONTAINER(i) || i->itype != I_INVISIBLE) // move to live list
		  {
			// remove i from newbies
		     if (prev) prev->nextb     = i->nextb;
		     else      bucket->newbies = i->nextb;	// first item
		     // don't change prev
		     
		     // add to livelist
		     i->nextb  = bucket->liveList;
		     bucket->liveList = BREDUCE(bh,bucket,i);
		  }
		  else prev = i;
	       }
#endif
	    } // newbies
	 } // for buckets
      SPIN_LOCK_RELEASE(&bh->lock);	// can now allocate from these buckets
space = bh->numBuckets * bh->blobsPerBucket;  // total available space
//if (inUse*2 > space) porkSoda = 1;	 // running out of space, got garbage?
//if(porkSoda) printf("------------------------->porkSoda\n");
   } // for bucketChain

#if AIPORK
   CAI_ZERO(&pork);  // we've [sorta] reclaimed garbage, reset allocate counter
#else
   pork = 0;
#endif
}

    // I think this is now thread safe
Instance *ibucketStats(Instance *self,pArglist arglist, pVM vm)
{
   IBucketHeader *bh;
   unsigned	  totalBytes = 0, totalInUse = 0, capacity = 0;
   unsigned	  numHeaders = 0, numBuckets = 0;

   printf("Bucket Stats\n");
   for (bh = (IBucketHeader *)ibucketHeaderChain; bh; bh = bh->next)
   {
      IBucket  *bucket;
      unsigned	n, size;

      numHeaders++; numBuckets += bh->numBuckets;
      size = bh->bucketSize * bh->numBuckets;
      totalBytes += size;
      capacity   += (bh->numBuckets * bh->blobsPerBucket);
      printf("Blob size: %3d<>Num buckets: %2d<>Blobs/bucket: %5d<>"
             "Bytes: %6u<>GC: %u",
	     bh->blobSize,bh->numBuckets,bh->blobsPerBucket,size,bh->pleaseGCme);

      SPIN_LOCK_ACQUIRE(&bh->lock);
      {
	 Blob     *b;
	 unsigned  inUse = 0;
	 for (bucket = bh->buckets; bucket; bucket = bucket->nextBucket)
	 {
	    int dead = 0;
	    for (b = bucket->freeList; b; b = b->next) dead++;
	    n     = bh->blobsPerBucket - dead;
	    inUse += n;
	    printf("\n   %5d blobs in use (%3d%%, free spots: %4d)",
	        n,n*100/(n+dead),dead);
	 }
	 printf(" --> %d%%\n",inUse*100/(bh->numBuckets * bh->blobsPerBucket));
	 totalInUse += inUse;
	 #if __IB_BULK_FREE
	    for (n = 0, b = bh->freeList; b; b = b->next) n++;
	    inUse -= n;
	    printf("   %5d blobs on free list. Pending free: %s.  "
	           "In use: %u of %d\n",
		   n, bh->toBeFreedList ? "yes" : "no",
		   inUse,bh->numBuckets*bh->blobsPerBucket);
	 #endif
      }
      SPIN_LOCK_RELEASE(&bh->lock);
   }
   printf("Total bytes: %u (%.2fMB), Blob capacity: %u, Live blobs: %u (%d%%)\n",
      totalBytes,((float)totalBytes)/1048576.0,capacity,totalInUse,
      totalInUse*100/capacity);
   printf("Number of bucket headers: %u, Number of buckets: %u\n",
      numHeaders,numBuckets);
   return Void;
}

long ibucketCounts(char **gcTypeNames,long counts[][GC_NUM_TYPES+1])
{
   long int	  total = 0;
   IBucketHeader *bh;

   for (bh = (IBucketHeader *)ibucketHeaderChain; bh; bh = bh->next)
   {
      IBucket  *bucket;

      if (!bh->pleaseGCme) continue;
      SPIN_LOCK_ACQUIRE(&bh->lock);
      {
	 for (bucket = bh->buckets; bucket; bucket = bucket->nextBucket)
	 {
	    unsigned int  n;
	    BInstance    *i;
	    for (n = bucket->liveList;
		 n != MAX_INSTANCES_PER_BUCKET; n = i->nextb)
	    {
	       int itype, t;
	       i = BEXPAND(bh,bucket,n);
	       itype = GC_TYPE(i);
	       t     = TYPEO(i);

	       counts[t][itype]++;
	       counts[t][GC_NUM_TYPES]++;	// total for this gcType
	       total++;
	    }
	 }
      }
      SPIN_LOCK_RELEASE(&bh->lock);
   }
   return total;
}


static void bucketConstruct(void)
{
   spinLockInit(&bLock);
}

/* ******************************************************************** */
/* *********************************  ********************************* */
/* ******************************************************************** */

void allocateConstruct(void)	// do this early, just cause
{
   bucketConstruct();
   #ifdef __USE_HEAPS
      heapConstruct();
   #endif
   #ifdef __DLMALLOC
      zmallocConstruct();
   #endif
#if AIPORK
   CAI_INIT(&pork);
#endif


#if 0
   switch(sizeof(void *)){ // gc if have allocated this much
      case 16: TIME_TO_GC = (2<<21); break;	// 128 bit machine
      default:
      case  8: TIME_TO_GC = (2<<20); break;	// 64 bit machine
      case  4: TIME_TO_GC = (2<<19); break;	// 32 bit machine
   }
#endif
}
