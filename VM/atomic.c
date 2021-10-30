/* atomic.c : Lock, WriteLock, AtomicBool and AtomicInt objects
 * Threading constructs for both zkl and C code.
 * This file contains OS specific code.
 * My unit of time is 1 milli-second
 * 
 * Copyright (c) 2006,2007,2008-12,2013 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

//_interlockedbittestandreset, _InterlockedCompareExchange16, _InterlockedOr8 

//#define _WIN32_WINNT 0x0400 	// to get SwitchToThread (winbase.h)

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#include <stdio.h>
#include <time.h>		// for time()
#include <string.h>		// for memcpy()
#if defined(__unix__)
   #include <sys/time.h>	// gettimeofday()
#endif
#include <errno.h>

#define __NOT_A_DLL
#define __PC_INTERNALS
#define __NEED_PTHREADS

#include "zklObject.h"
#include "zklAtomic.h"
#include "zklList.h"
#include "zklNumber.h"
#include "zklMethod.h"
#include "zklString.h"
#include "zklUtil.h"	// irand()

void vmProcessInterrupt(pVM);	// vm.c

// timer_settime, setitimer, ualarm, SIGVTALRM
// timer_create, sigevent

// pause, mq_notify, mq_open, mq_send, mq_recieve
// sigaction, pthread_kill, sigwait

#ifdef _MSC_VER		// Sleep is milli-seconds
   #define milliSleep Sleep
#elif defined(__unix__)	// usleep is micro-seconds, Posix: use nanosleep
   #define milliSleep(t) usleep((useconds_t)t * 1000)
#if 0
   #undef SPIN_STALL
   #define SPIN_STALL	     cpuPause()		// stinks
#endif
#endif


//#define RIP_VAN_WINKLE	2147483647		// 24 days (INT32_MAX)
#define RIP_VAN_WINKLE	( (unsigned long)~1 )	// Biggest unsigned long

#define	TICK		 1	// milliseconds
#define	MAX_TICK       100	// milliseconds

typedef struct
{
   int n, ticker;
   unsigned long int endTime;		// milliseconds, 24.7 days (32 bits)
} SnoozeAlarm;		// = { 0 }

    /* Calculate the length of nap time.  The idea is to sleep longer as
     * you wait longer, on the assumption that the longer you wait for
     * something, the less likely it is to occur. However, this is flawed
     * for contested resources:
     *   Thread 1 is grabbing/releasing a lock quickely.
     *   If another thread tries and misses the lock, it will be less and
     *   less likely to get the lock because the sleep time is increasing.
     * Ticks of zero (give up remander of time slice) seem to make quite a
     *   difference.
     * Windows: CLOCKS_PER_SEC is 1000 on my system (ie milliseconds)
     */
#define RAMPED_SAW_TOOTH		0
#define RAMPED_SAW_TOOTH_WITH_NOISE	1	//

#define SAW_TOOTH			0
#define SAW_TOOTH_WITH_NOISE		0

#define DOUBLER				0
#define DOUBLER_WITH_NOISE		0

#define STRAIGHT			0	// grim


    // Starting condition: count = 0, ticker = 1
static unsigned int nextTick(int *count, int *ticker)
{
   unsigned int n = *count, tick = *ticker;

#if STRAIGHT
   *count = 1;
   return 1;
#endif

#if RAMPED_SAW_TOOTH_WITH_NOISE
   if (tick > n)
   {
      tick = TICK;
      if (n < MAX_TICK) n++;
   }
   else tick++;
   *count = n; *ticker = tick;
   return irand() % tick + n/4;
#endif

#if RAMPED_SAW_TOOTH
   if (tick > n)
   {
      tick = TICK;
      if (n < MAX_TICK) n++;
   }
   else tick++;
   *count = n; *ticker = tick;
   return tick;
#endif

#if SAW_TOOTH_WITH_NOISE
   if (n > MAX_TICK) n = TICK; else n++;
   tick = irand() % n;
   *count = n; *ticker = tick;
   return tick;
#endif

#if SAW_TOOTH
   	// saw tooth
   if (tick > MAX_TICK) tick = TICK; else tick++;
   *count = n; *ticker = tick;
   return tick;
#endif

#if DOUBLER
   if (n == 0 && tick < MAX_TICK) { tick *= 2; n = tick; } n--;
   *count = n; *ticker = tick;
   return tick;
#endif

#if DOUBLER_WITH_NOISE
   if (n == 0 && tick < MAX_TICK) { tick *= 2; n = tick; }
   *count = --n; *ticker = tick;
   return irand() % tick;
#endif
}

#if 0	// Timeout is, uhhh, pretty flexible
    // start with count == 0, ticker == 1
static void snooze(int *n, int *ticker) { milliSleep(nextTick(n,ticker)); }

static int	// Returns: A_OK (slept), A_TIMED_OUT, A_INTERRUPTED
snooze2(int *n, int *ticker, unsigned long *timeout, int *interrupt)
{
   int tick;

   if (interrupt && *interrupt) return A_INTERRUPTED;
   tick = nextTick(n,ticker);
   if (timeout)
   {
      unsigned long t = *timeout;
      if (t <= 0) return A_TIMED_OUT;
      if (tick > t) tick = t;
      t -= tick;
      *timeout = t;
   }
//printf(">>2>>>%d %d\n",*n,tick);
   milliSleep(tick);
   return A_OK;
}

#else
    /* Timeout is a fixed time in the future
     * This is wall time, not CPU time.
     * clock() on Linux is CPU time (?), on Windows it is process wall time.
     * This probably should be per thread wall time
     *   (clock_gettime(CLOCK_THREAD_CPUTIME_ID,...) on Unix?), I don't know
     *   if Windows has such a funcion.
     * I really should just use 64 bit ints just in case zkl gets to the
     *   point where it can run forever.
     * If non blocking (eg Pipe.read(0)), time out now. Otherwise at least
     *   two passes: Pipe.read(0.00001) is two calls even if the first call
     *   times out. I *think* this is a good thing.
     * Returns: A_OK (slept), A_TIMED_OUT, A_INTERRUPTED
     */
time_t time0;		// only used on Unix/Linux and in Time.Clock.runTime

static int	// Returns: A_OK (slept), A_TIMED_OUT, A_INTERRUPTED
snooze2(SnoozeAlarm *alarm, unsigned long *timeout, int *interrupt)
{
   unsigned int tick, n = alarm->n;

   if (interrupt && *interrupt) return A_INTERRUPTED;

   if (n == 0) { alarm->ticker = 1; }	// init

// millis too damn long
      // do some rapid spinning assuming what we are waiting for has happened
   //if (n < 50) { tick = 0; alarm->n++; } else	// slow slow slow
   tick = nextTick(&alarm->n,&alarm->ticker);
   if (timeout)
   {
      unsigned long remaining, now;

      #ifdef _MSC_VER
      		// seconds to MILLIseconds
	 now = (unsigned long)(((float)clock())/CLOCKS_PER_SEC*1000);
//	 now = (unsigned long)(((uint64_t)clock())*1000/CLOCKS_PER_SEC);
      #elif defined(__unix__)
      {
	 struct timeval tv;
	 gettimeofday(&tv,0);	// MICROseconds to MILLIseconds
		// ignore the epoch, give myself some head room
	 now = (tv.tv_sec - time0) * 1000 + tv.tv_usec / 1000;
      }
      #endif
      if (n == 0)	// first call
      {
	 remaining = *timeout;
	 alarm->endTime = now + remaining;
	 if (!remaining) return A_TIMED_OUT;	// .read(0)
      }
      else
      {
	 if (now >= alarm->endTime) return A_TIMED_OUT;
	 remaining = alarm->endTime - now;
      }
      if (tick > remaining) tick = remaining;
   }
//printf(">>2>>>%d %d\n",*n,tick);
//   if (tick)	// also slow
   milliSleep(tick);
   return A_OK;
}
#endif

/* ******************************************************************** */
/* ****************************** Locks ******************************* */
/* ******************************************************************** */

    /* Windows Mutex's are a bit funny:
     * - You acquire the the lock by waiting for it but if you wait for zero
     *   milliseconds, you are just testing it, not acquiring it.
     * - Mutex ownership is thread based: if your thread has it, acquire
     *   again and you get it. This makes sense, you are not going to have
     *   resouce conflicts in the same thread. However, there a count on the
     *   mutex, so if you ask for it n times, you need to release it n times.
     * - I create the mutex unowned, so you need to acquire it if you want
     *   ownership.
     * - If a owning thread exists, it [completely] releases the mutex.  So
     *   release() is optional in some cases.
     * - REALLY SLOW. If you use them a lot, you will be hating life.
     *   Consider SpinLocks, which use
     *     InterlockedCompareExchange/InterlockedExchange
     */

/* ******************************************************************** */
/* **************************** Spin Locks **************************** */
/* ******************************************************************** */

    /* Notes:
     *   Windows locks (Mutex) are VERY slow.  I need a fast one so I use a
     *   busy loop based on atomic compare and exchange.
     * InterlockedCompareExchange(
     *	 LPLONG Destination, LONG Exchange, LONG Comperand)
     * The InterlockedCompareExchange function performs an atomic comparison
     *   of the Destination value with the Comperand value.  If the
     *   Destination value is equal to the Comperand value, the Exchange
     *   value is stored in the address specified by Destination.
     *   Otherwise, no operation is performed.
     *   The return value is the initial value of the destination.
     * Where to use:
     *   Regular (system) locks work fine for corse grained locking, use
     *   those most of the time. Fast locks are are much lighter weight (no
     *   system resources) and fast but can suck up lots of cycles if they
     *   have to do any waiting but are quite quick.  So, if your critical
     *   section is small and fast or there is very little contention, these
     *   locks work well.
     */

static int _tryToGetLock(void *spinLock);

void spinLockInit(SpinLock *self) { CAI_INIT(self); }

int spinLockAcquire(SpinLock *self)
{
//   int i = 0, tick = 1;
//   while (InterlockedCompareExchange(self,1,0)) snooze(&i,&tick);
//   while (InterlockedCompareExchange(self,1,0)) milliSleep(1);
//   while (InterlockedCompareExchange(self,1,0)) SWITCH_THREADS;
   SPIN_LOCK_ACQUIRE(self);
   return 1;
}

//////////////////////////////////////////////////////////////////////////
#if 0
  // the async versions are very slow, only use when you know you are 
  // going to have big waits

    // Blocking async spin lock aquire 
    // Not interruptable
    // Stalls
void aslAcquire(SpinLock *lock, pVM vm)
{
   if (SPIN_LOCK_TRY_TO_ACQUIRE(lock)) return;
   asyncBlock((Instance *)lock,_tryToGetLock,(void *)lock,vm);
}

    // Async spin lock aquire with timeout
    // interruptable, blocking or timeout
int aslAcquireTO(SpinLock *lock, Instance *seconds, pVM vm)
{
   if (SPIN_LOCK_TRY_TO_ACQUIRE(lock)) return A_OK;
   return asyncRead((Instance *)lock,_tryToGetLock,(void *)lock,seconds,vm);
}

    // You have to call this to release the lock you got with slAcquire*()
void aslRelease(SpinLock *lock) { caiSet(lock,0); }
#endif
//////////////////////////////////////////////////////////////////////////

    // Returns: A_OK (got the lock), A_INTERRUPTED (interrupted)
    // vm == 0 is OK, VM == 1 NOT OK
    // Might stall, blocks
int spinLockAcquire2(SpinLock *self, int canInterrupt, pVM vm)
{
   int   *interrupt = canInterrupt ? vmInterruptEvent(vm) : 0;
   int    s = A_OK;
   SnoozeAlarm alarm = { 0 };

   vmMightStall(vm,1);
      while (!SPIN_LOCK_TRY_TO_ACQUIRE(self))
	 if (A_INTERRUPTED == snooze2(&alarm,0,interrupt))
	    { s = A_INTERRUPTED; break; }
   vmMightStall(vm,0);

   return s;
}

    // Returns 1 if can get lock within n tries
int spinLockAcquire3(SpinLock *self, int n)
{
   while (!SPIN_LOCK_TRY_TO_ACQUIRE(self) && n--) SPIN_STALL;
   return SPIN_LOCK_IS_LOCKED(self);
}
 
#if 0
    // Returns: A_OK (got the lock), A_TIMED_OUT, A_INTERRUPTED
    // vm == 0 is OK, VM == 1 NOT OK
    // Might stall
static int 
spinLockAcquireWithTimeout(SpinLock *self, unsigned long *millis, pVM vm)
{
   int *interrupt = vmInterruptEvent(vm);
   int  r = A_OK, s;
   SnoozeAlarm alarm = { 0 };

   vmMightStall(vm,1);
      while (!SPIN_LOCK_TRY_TO_ACQUIRE(self))
      {
	 s = snooze2(&alarm,millis,interrupt);
	 if (s != A_OK) { r = s; break; }	// timeout or interrupt
      }
   vmMightStall(vm,0);
   return r;
}
#endif

void spinLockRelease(SpinLock *self) { CAI_ZERO(self); }

	////////////////////////////////////////// Atomic.Lock

static ZKL_Object LockObject;

typedef struct
{
   BInstance instance;		// Inherit from Instance
   SpinLock  lock;
} LockInstance;		// 8 bytes

#define LOCK(i) ( ((LockInstance *)i)->lock )

	// lbuckets is shared with AtomicInt & AtomicBool
static IBucketHeader *lbuckets, *wbuckets;

    // Lock(locked=False), Lock.create()
static Instance *Lock_create(Instance *self,pArglist arglist,pVM vm)
{
   LockInstance *lock;
   int createLocked = arglistTryToGetBool(arglist,0,0,"Atomic.Bool",vm);

   lock = (LockInstance *)ibucketAllocate(lbuckets,&LockObject,I_OWNED,1,vm);
   spinLockInit(&lock->lock);
   if (createLocked) SPIN_LOCK_ACQUIRE(&lock->lock);
//   return addToCollectables((Instance *)lock,I_OWNED,vm);
   return (Instance *)lock;
}

    // Lock.acquire(timeout/seconds=True) --> Bool
static int _tryToGetLock(void *spinLock)
{
   int n = 50;
   while (n--) if (SPIN_LOCK_TRY_TO_ACQUIRE((SpinLock *)spinLock)) return A_OK;
   return A_FAIL;
}
Instance *Lock_acquire(Instance *self,pArglist arglist,pVM vm)
{
   int s = A_OK, v = SPIN_LOCK_IS_LOCKED(&LOCK(self));
   if (SPIN_LOCK_TRY_TO_ACQUIRE(&LOCK(self))) {}
   else s = asyncRead(self,_tryToGetLock,(void *)&LOCK(self),
		      arglistTryToGet(arglist,0),vm);
   if (s == A_OK)
   {
      if (!v) asyncWriteHappened((Instance *)self);  // actual change
      return BoolTrue;
   }
   return BoolFalse;
}

    // Lock.release() --> self
static Instance *Lock_release(LockInstance *self,pArglist arglist,pVM vm)
{
   int v = SPIN_LOCK_IS_LOCKED(&self->lock);
   SPIN_LOCK_RELEASE(&self->lock);
   if (v) asyncWriteHappened((Instance *)self);  // actual change

   return (Instance *)self;
}

#if 0
    // Lock.isSet() !!!!!! add arg so can check if locked or not locked
Instance *Lock_isSet(LockInstance *self,pArglist arglist,pVM vm)
   { return boolCreate(SPIN_LOCK_IS_LOCKED(&self->lock)); }
#endif

    // Lock.toString()
static Instance *Lock_toString(Instance *self,pArglist arglist,pVM vm)
{
   char buf[50];
   sprintf(buf,"%s(%s)",ONAME(self),
           SPIN_LOCK_IS_LOCKED(&LOCK(self)) ? "locked" : "unlocked" );
   return stringCreate(buf,I_OWNED,vm);
}

static const MethodTable lockMethods[] = 
{
   "create",	(pMethod)Lock_create,
   "acquire",	(pMethod)Lock_acquire,
   "release",	(pMethod)Lock_release,
//   "isSet",	(pMethod)Lock_isSet,
   "toString",	(pMethod)Lock_toString,
   0,		0
};

	////////////////////////// Properties

    // Lock.value, Lock.isLocked --> Bool
static Instance *Lock_isLocked(LockInstance *self,pVM vm)
   { return boolCreate(SPIN_LOCK_IS_LOCKED(&self->lock)); }

static const PropertyTable lockProperties[] = 
{
   "value",	(pProperty)Lock_isLocked,
   "isLocked",	(pProperty)Lock_isLocked,
   0,		0
};

static void lockConstruct(void)
{
   static IBucketHeader _lbuckets;

   constructObject(&LockObject, LockType, lockMethods,lockProperties,0,NoVM);
   LockObject.vaultPath   = "Atomic";
   LockObject.isize	  = sizeof(LockInstance);
   LockObject.threadSafe  = 1;
   LockObject.isBInstance = 1;
   // myName set by constructObject(), this is a system type

	// Also used for AtomicInts and AtomicBools
	// The parser uses a LOT of these
	// Would be nice to share with KString but 64 bit pointers means
	//   a lot of slop
   lbuckets = ibucketReserve(&LockObject,3333,&_lbuckets,0,NoVM);
}

/* ******************************************************************** */
/* **************************** WriteLocks **************************** */
/* ******************************************************************** */

     /* WriteLocks
      * These are locks use to protect code that can be read by anybody any
      * time, but, if write, everybody out of the pool.
      * The big idea here is to be really fast if only there are only
      * readers.  Serializes if a bazillion readers acquire at the same
      * time but don't think that is going to happen.
      */

void writeLockInit(WriteLock *self)
{
   spinLockInit(&self->writing);	// the lock
   CAI_INIT(&self->readers);
}

    /* reader is 1 if just want to read, 0 if want to write
     * If reader, block if somebody is writing
     * If writer, acquire the lock if:
     *   No writers
     *   No readers
     * If you want to create a deadlock, grab this more than once in
     *   the same thread (ie be careful of recursion).
     * Be careful of stall bits.  Stalling is a real problem here - if
     *   somebody has the write lock, GCT can't mark that container or dead
     *   lock. This happens:
     *   VM1 calls list.filter, gets read lock, stalls in vmCreate()
     *   VM2 calls list.append, gets write lock here, has been told to mark
     *     is waiting VM1 to release read lock
     *   dead lock
     * 
     * vm: NoVM: Won't GC
     * 
     * Returns: A_OK (got the lock you wanted), A_TIMED_OUT, A_INTERRUPTED
     */
#if 0
void writeLockAcquire(WriteLock *self,int readerOrWriter, pVM vm)
{
   int i = 0, tick = 1;

   	// don't stall, can deadlock GC
   spinLockAcquire(&self->writing);	// block if somebody is writing

	// at this point, there are no writers
   if (readerOrWriter == WL_READER)
   {
//!!! check interrupt
      CAI_INC(&self->readers);
      SPIN_LOCK_RELEASE(&self->writing);	// I don't want this lock
      return;				// start reading!
   }

	// A writer
	// Nobody is writing and reader/writer wannabees are blocked
	// wait until all readers are done, then it is all MINE!
   while CAI_VALUE(&self->readers)	// spin until no readers
   {
//!!! check interrupt
// special marking fcn if vm needs marking
      snooze(&i,&tick);
   }
   // I now own the write lock
}

#elif 1	// this one seems "best" but not by much
	// starvable
void writeLockAcquire(WriteLock *self,int readerOrWriter, pVM vm)
{
   while(1)
   {
      	// stall bit can't be set if holding write lock
      while (!SPIN_LOCK_TRY_TO_ACQUIRE(&self->writing))
      {
	 vmMightStall(vm,666);		// if VM needs marking, mark it
      	 SPIN_STALL;
      }

	   // at this point, there are no writers
      if (readerOrWriter == WL_READER)
      {
   //!!! check interrupt
	 CAI_INC(&self->readers);
	 SPIN_LOCK_RELEASE(&self->writing);	// I don't want this lock
	 return;				// start reading!
      }

	   // A writer
	   // Nobody is writing and reader/writer wannabees are blocked
	   // wait until all readers are done, then it is all MINE!
      if (CAI_VALUE(&self->readers))	// spin until no readers
	 SPIN_LOCK_RELEASE(&self->writing);	// I don't want this lock
      else break;	// no readers

      vmMightStall(vm,666);	// in case lock is uncontested
   } // while
   // I now own the write lock
}

#elif 0
	// starvable writers
void writeLockAcquire(WriteLock *self,int readerOrWriter, pVM vm)
{
   if (readerOrWriter == WL_READER)
   {
      CAI_INC(&self->readers);
      vmMightStall(vm,1);
	SPIN_LOCK_WAIT_WHILE_LOCKED(&self->writing);
      vmMightStall(vm,0);
      return;				// start reading!
   }

   // A writer
   vmMightStall(vm,1);
      while(1)
      {
	 SPIN_LOCK_ACQUIRE(&self->writing);
	   // at this point, there are no writers
	   // reader wannabees are blocked
	   // wait until all readers are done, then it is all MINE!
	 if (0 == CAI_VALUE(&self->readers))  // any reader wannabees?
	    break;				       // nope
	 SPIN_LOCK_RELEASE(&self->writing);	// reader wannabees wins
      } // while
   vmMightStall(vm,0);
   // I now own the write lock
}
#endif

    // Returns: A_OK (got the lock), A_TIMED_OUT, A_INTERRUPTED
    // Only call this from Methods, can dead lock otherwise
#if 1
static int
_writeLockAcquireT(WriteLock *wl,int readerOrWriter,
		   Instance *timeout, pVM vm)
{
   int         s;
   CAtomicInt *readers = &wl->readers;
   SpinLock   *lock    = &wl->writing;

   // It would be nice to just inc readers && if > 1, call it good.
   // But: 1 writer, two readers == race

   // block if somebody is writing
   if (!SPIN_LOCK_TRY_TO_ACQUIRE(lock))	// oh well, gotta wait
   {
      s = asyncRead((Instance *)lock,_tryToGetLock,(void *)lock,timeout,vm);
      if (s != A_OK) return s;
   }

   // got the write lock, which means there are no writers

   if (readerOrWriter == WL_READER)
   {
      // which means I can read
      s = CAI_INC(readers);
      caiSet(lock,0);  // not writing, release lock, notify anybody stuck above
      return A_OK;			// start reading!
   }

   // else: I want to write. Anybody reading?
   // no writers and reader/writer wannabees are blocked
   // wait until all readers are done, then it is all MINE!

//!!! can deadlock if GC here because GC can't read
   s = caiWaitFor(readers,0,timeout,vm);
///!!!!shit interrupt doesn't get here
   if (s != A_OK) caiSet(lock,0);	// release lock & notify
   return s;
}
#else
static int
_writeLockAcquireT(WriteLock *self,int readerOrWriter,
		  unsigned long *millis, pVM vm)
{
   int *interrupt = vmInterruptEvent(vm);
   int  s;
   SnoozeAlarm alarm = { 0 };

	// block if somebody is writing
   s = spinLockAcquireWithTimeout(&self->writing,millis,vm);
   if (s != A_OK) return s;

	// at this point, there are no writers
   if (readerOrWriter == WL_READER)
   {
      CAI_INC(&self->readers);
      SPIN_LOCK_RELEASE(&self->writing);	// I don't want this lock
      return A_OK;				// start reading!
   }

	// A writer
	// no writers and reader/writer wannabees are blocked
	// wait until all readers are done, then it is all MINE!
   vmMightStall(vm,1);	//!!! can deadlock if GC here and somebody is reading
      while (1)		// spin until no readers
      {
	 if (!CAI_VALUE(&self->readers))   // spin until no readers
	   { s = A_OK; break; }
	 s = snooze2(&alarm,millis,interrupt);
	 if (s != A_OK) break;			// timeout or interrupt
      }
   vmMightStall(vm,0);
   if (s != A_OK) SPIN_LOCK_RELEASE(&self->writing);	// opps
   return s;
}
#endif

    // IF you are the only reader and want to be a writer also.
    // Same old GC/Stall problem as with writeLockAcquire
void writeLockElevate(WriteLock *self, pVM vm)
{
   while(1)
   {
vmMightStall(vm,666);
      SPIN_LOCK_ACQUIRE(&self->writing);   // block if somebody is writing
      if (self->readers == 1) break;
      // somebody else is reading or you lied about being the only reader
      SPIN_LOCK_RELEASE(&self->writing);
      SWITCH_THREADS;		// give up the rest of my time slice
   }
}

void writeLockRelease(WriteLock *self, int readerOrWriter)
{
   if (readerOrWriter == WL_READER) CAI_DEC(&self->readers);
   else SPIN_LOCK_RELEASE(&self->writing);
}

static void _writeLockRelease(WriteLock *wl, int readerOrWriter)
{
   CAtomicInt *readers = &wl->readers;
   SpinLock   *lock    = &wl->writing;
   if (readerOrWriter == WL_READER)
   {
      int n = CAI_DEC(readers);
      if (0 == n) asyncWriteHappened((Instance *)readers);
   }
   else		// writer
   {
      int s = SPIN_LOCK_IS_LOCKED(lock);
      SPIN_LOCK_RELEASE(lock);
      if (s) asyncWriteHappened((Instance *)lock);
   }
}

static ZKL_Object WriteLockObject;

typedef struct
{
   BInstance	instance;		// Inherit from Instance
   WriteLock	lock;
} WLockInstance;	// 12

    // WriteLock()
static Instance *WriteLock_create(Instance *self,pArglist arglist,pVM vm)
{
   WLockInstance *lock;
   lock = (WLockInstance *)ibucketAllocate(wbuckets,
			   &WriteLockObject,I_OWNED,1,vm);
   writeLockInit(&lock->lock);
//   return addToCollectables((Instance *)lock,I_OWNED,vm);
   return (Instance *)lock;
}

    // WriteLock.acquire(timeout/seconds=True) --> Bool
static Instance *WriteLock_acquire(WLockInstance *self,
		int readerOrWriter,pArglist arglist,pVM vm)
{
#if 1
   int	s;
   Instance *wait = arglistTryToGet(arglist,0);
   s = _writeLockAcquireT(&self->lock,readerOrWriter,wait,vm);
//if (s == A_OK) asyncWriteHappened((Instance *)self);	// for Atomic.wait()
   return (s == A_OK) ? BoolTrue : BoolFalse;
#else
   unsigned long millis;
   int	s, wait = getMillis(arglistTryToGet(arglist,0),&millis,vm);

   if (wait)
   {
      s = _writeLockAcquireT(&self->lock,readerOrWriter,&millis,vm);
      return boolCreate(A_OK == s);	// VM will blow if interrupted
   }

   s = _writeLockAcquireT(&self->lock,readerOrWriter,0,vm);
   return BoolTrue;	// VM will blow if interrupted, won't timeout
#endif
}

static Instance *	// writeLock.acquireForReading(timeout/seconds=True)
WriteLock_acquireForReading(WLockInstance *self,pArglist arglist,pVM vm)
   { return WriteLock_acquire(self,WL_READER,arglist,vm); }

static Instance *	// WriteLock.acquireForWriting(timeout/seconds=True)
WriteLock_acquireForWriting(WLockInstance *self,pArglist arglist,pVM vm)
   { return WriteLock_acquire(self,WL_WRITER,arglist,vm); }

static Instance *	// WriteLock.readerRelease() --> self
WriteLock_readerRelease(WLockInstance *self,pArglist arglist,pVM vm)
{
   _writeLockRelease(&self->lock,WL_READER);
//asyncWriteHappened((Instance *)self);	// for Atomic.wait()
   return (Instance *)self;
}

static Instance *	// WriteLock.writerRelease() --> self
WriteLock_writerRelease(WLockInstance *self,pArglist arglist,pVM vm)
{
   _writeLockRelease(&self->lock,WL_WRITER);
   return (Instance *)self;
}

    // WriteLock.isSet() !!!!!! add arg so can check if locked or not locked
Instance *WriteLock_isSet(WLockInstance *self,pArglist arglist,pVM vm)
   { return boolCreate(self->lock.writing); }

static const MethodTable writeLockMethods[] = 
{
   "create",		(pMethod)WriteLock_create,
   "acquireForReading",	(pMethod)WriteLock_acquireForReading,
   "acquireForWriting",	(pMethod)WriteLock_acquireForWriting,
   "readerRelease",	(pMethod)WriteLock_readerRelease,
   "writerRelease",	(pMethod)WriteLock_writerRelease,
   "isSet",		(pMethod)WriteLock_isSet,
   0,			0
};

	////////////////////////// WriteLock Properties
    // WriteLock.value, .isLocked --> Bool
static Instance *writeLock_isLocked(WLockInstance *self,pVM vm)
   { return boolCreate(CAI_VALUE(&self->lock.writing)); }

    // WriteLock.readers --> Int
static Instance *writeLock_numReaders(WLockInstance *self,pVM vm)
   { return intCreate(CAI_VALUE(&self->lock.readers),vm); }

static const PropertyTable writeLockProperties[] = 
{
   "value",	(pProperty)writeLock_isLocked,
   "isLocked",	(pProperty)writeLock_isLocked,
   "readers",	(pProperty)writeLock_numReaders,
   0,		0
};

static void writeLockConstruct(void)
{
   static IBucketHeader _wbuckets;	// might not be used

   constructObject(&WriteLockObject, WriteLockType,
			writeLockMethods,writeLockProperties,0,NoVM);
   WriteLockObject.vaultPath   = "Atomic";
   WriteLockObject.isize       = sizeof(WLockInstance);
   WriteLockObject.threadSafe  = 1;
   WriteLockObject.isBInstance = 1;
   // myName set by constructObject(), this is a system type

	// share with Method or Fcn
   wbuckets = ibucketHitchHike(&WriteLockObject,4,100,&_wbuckets,NoVM);
}

/* ******************************************************************** */
/* ********************* Atomic Bools and Ints ************************ */
/* ******************************************************************** */

    /* Windows MS Visual C++ on Interlocked Variable Access:
     * Simple reads and writes to properly-aligned 32-bit variables are
     * atomic. In other words, when one thread is updating a 32-bit
     * variable, you will not end up with only one portion of the variable
     * updated; all 32 bits are updated in an atomic fashion. However,
     * access is not guaranteed to be synchronized. If two threads are
     * reading and writing from the same variable, you cannot determine if
     * one thread will perform its read operation before the other performs
     * its write operation.
     * 
     * Simple reads and writes to properly aligned 64-bit variables are
     * atomic on 64-bit Windows. Reads and writes to 64-bit values are not
     * guaranteed to be atomic on 32-bit Windows. Reads and writes to
     * variables of other sizes are not guaranteed to be atomic on any
     * platform.
     */


void cAtomicIntSetTo(CAtomicInt *value,long int n) { CAI_SET(value,n); }

/* Atomic Ints are 32 bits because Windows XP doesn't support the 64 bit
 * versions of the Interlocked functions.
 */

typedef struct
{
   BInstance  instance;
   CAtomicInt value;
} AtomicN;	// 8 bytes, put in Lock buckets, since same as

#define ATOMIC_VALUE(ai) ( ((AtomicN *)ai)->value )

long int atomicGet(Instance *self) { return ATOMIC_VALUE(self); }
long int caiGet(CAtomicInt *cai)   { return ATOMIC_VALUE(cai);  }

    /* If value is currently the expected value, set it to the newValue.
     *   Otherwise, fail.
     * InterlockedCompareExchange returns the inital value.  Use this to
     *   determine if the set was successful:  if the expected value is
     *   returned, then the exchange actually happened and value is now
     *   newValue.
     * Of course, if expected == new, that is fine by me.
     */
static Instance *
atomicTryToSet(Instance *self,long int newValue,long int expected)
{
   CAtomicInt *value = &ATOMIC_VALUE(self);
   int r;
   #ifdef _MSC_VER
      r = (expected == InterlockedCompareExchange(value,newValue,expected));
   #elif defined(__GNUC__)
      r = __sync_bool_compare_and_swap(value,expected,newValue);
   #endif
   if (r) asyncWroteN(self,newValue);
   return r ? BoolTrue : BoolFalse;
}

	///////////////////////////////// Atomic.Bool, Atomic.Int

static ZKL_Object AtomicBoolObject, AtomicIntObject;

static Instance *atomicCreate(ZKL_Object *object, long int x, pVM vm)
{
   AtomicN *a = (AtomicN *)ibucketAllocate(lbuckets,object,I_OWNED,1,vm);
   CAI_SET(&ATOMIC_VALUE(a),x);
   return (Instance *)a;
}

#if 0
void acIntInit(ACInt *n,int value)
{
   CAI_INIT(&n->n); CAI_SET(&n->n,value);
   pthread_cond_init(&n->cond,0);
//pthread_mutex_init(&n->mutex,0);
}

//int acIntValue(ACInt *n) { return CAI_VALUE(&n->n); }

void acIntSet(ACInt *n,int value)
{
   CAI_SET(&n->n,value);
   pthread_cond_broadcast(&n->cond);	// sync caches, signal waiters
}

    // Wait for ACInt to be set to a value
    // -->  A_OK, A_TIMED_OUT
    // block: 1: if ignore timeout and wait until v is seen
    //        0: don't wait longer than millis
    // To poll: block == 0, millis == 0
int acIntWaitFor(ACInt *aci, int v, int block, unsigned long millis)
{
   static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

   int         s;
   CAtomicInt *n = &aci->n;
   struct timespec timeout;

   if (v == CAI_VALUE(n)) return A_OK;

   s = 0;
   if (block)	// wait forever
   {
//      pthread_mutex_lock(&mutex);
      pthread_mutex_trylock(&mutex);
//      pthread_mutex_trylock(&aci->mutex);
printf("Wait for %d\n",v);
	 // predicate to account for spurious wakeups
      while(v != CAI_VALUE(n))
{
	 s = pthread_cond_wait(&aci->cond,&mutex);
printf("Waiting for %d and got %d\n",v,CAI_VALUE(n));
}
   }
   else
   {
      if (millis == 0) return A_TIMED_OUT;	// just checking

      // wait for value to be set
      clock_gettime(CLOCK_REALTIME,&timeout);
      timeout.tv_sec += millis/1000; timeout.tv_nsec += (millis % 1000) * 10;
//printf("timeout sec : %ld %ld\n",timeout.tv_sec, timeout.tv_nsec);

//      pthread_mutex_lock(&mutex);
      pthread_mutex_trylock(&mutex);
//      pthread_mutex_trylock(&aci->mutex);
      while(v != CAI_VALUE(n) && s != ETIMEDOUT)
	 s = pthread_cond_timedwait(&aci->cond,&mutex,&timeout);
   }

if (s && s!=ETIMEDOUT) { printf("POOH %d %d\n",s,errno); perror("PHHO");}

   s = (s == ETIMEDOUT) ? A_TIMED_OUT : A_OK;
//   s = (v == CAI_VALUE(n)) ? A_OK : A_TIMED_OUT;

//pthread_mutex_unlock(&aci->mutex);
   pthread_mutex_unlock(&mutex);
//   pthread_mutex_destroy(&mutex);

   return s;
}

#endif


    // Returns: A_OK (bool is nonzero), A_TIMED_OUT, A_INTERRUPTED
    // vm == 0 is OK, VM == 1 NOT OK
    // Might stall
int intWaitFor(
CAtomicInt *theInt, long value, int waitForever, unsigned long millis, pVM vm)
{
   int *interrupt = vmInterruptEvent(vm);
   int  s;
   SnoozeAlarm alarm = { 0 };
   unsigned long *timeout = waitForever ? 0 : &millis;

   vmMightStall(vm,1);
      while (1)
      {
	 if (value == CAI_VALUE(theInt)) { s = A_OK; break; }
	 s = snooze2(&alarm,timeout,interrupt);
	 if (s != A_OK) break;	// timeout or interrupt
      }
   vmMightStall(vm,0);
   if (s == A_INTERRUPTED) vmProcessInterrupt(vm);
   return s;
}

/* ******************************************************************** */
/* *********************** Atomic Bool ******************************** */
/* ******************************************************************** */

Instance *atomicBoolCreate(int x, pVM vm)
   { return atomicCreate((ZKL_Object *)&AtomicBoolObject, (long)x,vm); }

    // Bool(value=False)
Instance *AB_create(Instance *self,pArglist arglist,pVM vm)
   { return atomicBoolCreate(arglistTryToGetBool(arglist,0,0,"Atomic.Bool",vm),vm); }

    // Bool.toString()
static Instance *AB_toString(Instance *self,pArglist arglist,pVM vm)
{
   char buf[50];
   sprintf(buf,"%s(%s)",ONAME(self), atomicGet(self) ? "True" : "False" );
   return stringCreate(buf,I_OWNED,vm);
}

    // Set an AtomicBool or AtomicInt to n and signal
    // Returns previous value
int abSet(Instance *self,int nv)
{
   int ov = CAI_SET(&ATOMIC_VALUE(self),nv);
   asyncWroteN(self,nv);
   return ov;
}

    // Strobe or blink a AtomicBool:
    // if 0 --> pretend to set to 1 and signal. If 1, set to 0 and strobe
    // Returns previous value
int abPulse(Instance *ab)
{
   int ov = ATOMIC_VALUE(ab);
   if (ov) CAI_ZERO(&ATOMIC_VALUE(ab));
   asyncWroteN(ab,1);
   return ov;
}

    // Bool.set(value = 1)
    // Returns previous value
static Instance *AB_set(Instance *self,pArglist arglist,pVM vm)
{
#if 0
   int nv = arglistTryToGetBool(arglist,0,1,"Atomic.Bool.set",vm);
   int ov = CAI_SET(&ATOMIC_VALUE(self),nv);
   if (nv != ov) asyncWroteN(self,nv);	// signal all waiters
   return ov ? BoolTrue : BoolFalse;
#else
   int ov = abSet(self,arglistTryToGetBool(arglist,0,1,"Atomic.Bool.set",vm));
   return ov ? BoolTrue : BoolFalse;
#endif
}

    // Bool.pulse()
    // Returns self
static Instance *AB_pulse(Instance *self,pArglist arglist,pVM vm)
   { abPulse(self); return self; }

    // Bool.clear(), returns previous value
static Instance *AB_clear(Instance *self,pArglist arglist,pVM vm)
{
   int n = CAI_ZERO(&ATOMIC_VALUE(self));
   if (n) asyncWroteN(self,0);	// signal all waiters
   return n ? BoolTrue : BoolFalse;
}

    // Bool.tryToSet() --> True if successful
    // If value is currently 0, set it to 1. If it is already 1, fail.
static Instance *AB_tryToSet(Instance *self,pArglist arglist,pVM vm)
   { return atomicTryToSet(self,1,0); }

    // Bool.setIf(newValue,expectedValue) --> True if successful
static Instance *AB_setIf(Instance *self,pArglist arglist,pVM vm)
{
   int newValue = arglistGetBool(arglist,0,"Atomic.Bool.setif",vm);
   int expected = arglistGetBool(arglist,1,"Atomic.Bool.setif",vm);
   return atomicTryToSet(self,newValue,expected);
}

    // Bool.isSet(), Bool.toBool()  !!!!!! add arg so can check if True or False
Instance *AB_isSet(Instance *self,pArglist arglist, pVM vm)
   { return boolCreate(atomicGet(self)); }

    // Bool.wait(timeout/seconds=True,throw=False)
Instance *AB_wait(AtomicN *self,pArglist arglist,pVM vm)
{
   int r;
   r = asyncWantN((Instance *)self,0,self->value,&self->value,1,arglistTryToGet(arglist,0),vm);
   if (A_TIMED_OUT == r && arglistTryToGet(arglist,1) == BoolTrue)
      vmThrow(vm,E_TIME_OUT,0);
   return (r == A_OK) ? BoolTrue : BoolFalse;
}

    // Bool.waitFor(value,timeout/seconds=True,throw=False) -->Bool
Instance *AB_waitFor(AtomicN *self,pArglist arglist,pVM vm)
{
   int r,v = arglistGetBool(arglist,0,"Atomic.Bool.waitFor",vm);
   r = asyncWantN((Instance *)self,0,self->value,&self->value,v,arglistTryToGet(arglist,1),vm);
   if (A_TIMED_OUT == r && arglistTryToGet(arglist,2) == BoolTrue)
      vmThrow(vm,E_TIME_OUT,0);
   return (r == A_OK) ? BoolTrue : BoolFalse;
}

   // Bool.setAndWaitFor(AB,v=True,timeout/seconds=True,throw=False) --> 
   //   set AB and wait for self set to b
static Instance *AB_setAndWaitFor(AtomicN *self,pArglist arglist,pVM vm)
{
   Instance *ab = arglistGetBObj(arglist,0,&AtomicBoolObject, "AtomicBool.setAndWaitFor",vm);
//   int r,v = arglistGetBool(arglist,1,"Atomic.Bool.setAndWaitFor",vm);
   int r,v = arglistTryToGetBool(arglist,1,1,"Atomic.Bool.setAndWaitFor",vm);
   if (ab == (Instance *)self)
      vmThrow(vm,E_ASSERTION_ERROR, "AtomicBool.setAndWaitFor(self): No");
   r = asyncWantN((Instance *)self,ab,self->value,&self->value,v,
		  arglistTryToGet(arglist,2),vm);
   if (A_TIMED_OUT == r && arglistTryToGet(arglist,3) == BoolTrue)
      vmThrow(vm,E_TIME_OUT,0);
   return (r == A_OK) ? BoolTrue : BoolFalse;
}

static const MethodTable abMethods[] = 
{
   "create",	    AB_create,
   "toString",	    AB_toString,
   "toBool",	    AB_isSet,	// same as .isSet
   "set",	    AB_set,
   "tryToSet",	    AB_tryToSet,
   "pulse",	    AB_pulse,
   "setIf",	    AB_setIf,
   "clear",	    AB_clear,
   "isSet",	    AB_isSet,
   "wait",	    (pMethod)AB_wait,
   "waitFor",	    (pMethod)AB_waitFor,
   "setAndWaitFor", (pMethod)AB_setAndWaitFor,
   0,		    0
};

	////////////////////// Properties

    // Bool.value, Bool.isTrue
static Instance *AB_value(Instance *self,pVM vm)
   { return boolCreate(atomicGet(self)); }

static const PropertyTable abProperties[] = 
{
   "value",	(pProperty)AB_value,
   "isTrue",	(pProperty)AB_value,
   0,		0
};

/* ******************************************************************** */
/* *************************** Atomic Ints **************************** */
/* ******************************************************************** */

Instance *atomicIntCreate(long x, pVM vm)
   { return atomicCreate((ZKL_Object *)&AtomicIntObject, x,vm); }

    // Int(value=0)
Instance *AI_create(Instance *self,pArglist arglist,pVM vm)
{
   Instance *i = arglistTryToGet(arglist,0);
   if (i) return atomicIntCreate((long)convertToInt(i,vm),vm);
   return atomicIntCreate(0,vm);
}

    // Int.toInt()
static Instance *AI_toInt(Instance *self,pArglist arglist,pVM vm)
   { return intCreate(atomicGet(self),vm); }

    // Int.toString()
static Instance *AI_toString(Instance *self,pArglist arglist,pVM vm)
{
   long x = atomicGet(self);
   char buf[50];
   sprintf(buf,"%s(%ld)",ONAME(self),x);	// long int
   return stringCreate(buf,I_OWNED,vm);
}

    // Int.set(n), returns previous value
static Instance *AI_set(Instance *self,pArglist arglist,pVM vm)
{
   long nv = (long)arglistGetInt(arglist,0,"Atomic.Int.set",vm);
   long pv = CAI_SET(&ATOMIC_VALUE(self),nv);
   if (nv != pv) asyncWroteN(self,nv);	// signal all waiters
   return intCreate(pv,vm);
}

    // Int.isSet([n])
static Instance *AI_isSet(Instance *self,pArglist arglist,pVM vm)
{
   int	   n = atomicGet(self);
   int64_t i64;

   if (arglistTryToGetInt(arglist,0,&i64,0,vm)) return boolCreate(n == i64);
   return boolCreate(n);
}

    // Int.toBool()
static Instance *AI_toBool(Instance *self,pArglist arglist,pVM vm)
   { return boolCreate(atomicGet(self)); }

   // Int.setIf(newValue,expectedValue)
static Instance *AI_setIf(Instance *self,pArglist arglist,pVM vm)
{
   long newValue = (long)arglistGetInt(arglist,0,0,vm);
   long expected = (long)arglistGetInt(arglist,1,0,vm);
   return atomicTryToSet(self,newValue,expected);
}

    // Int.inc() --> new value
static Instance *AI_inc(Instance *self,pArglist arglist,pVM vm)
{
   long n = CAI_INC(&ATOMIC_VALUE(self));
   asyncWroteN(self,n);	// signal all waiters
   return intCreate(n,vm);
}

    // Int.dec() --> new value
static Instance *AI_dec(Instance *self,pArglist arglist,pVM vm)
{
   long n = CAI_DEC(&ATOMIC_VALUE(self));
   asyncWroteN(self,n);	// signal all waiters
   return intCreate(n,vm);
}

   // Int.waitFor(n,timeout/seconds=True,throw=False) --> wait for self set to n
static Instance *AI_waitFor(Instance *self,pArglist arglist,pVM vm)
{
   int  r;
   long n = (long)arglistGetInt(arglist,0,"AtomicInt.waitFor",vm);
   r = asyncWantN(self,0,ATOMIC_VALUE(self),&ATOMIC_VALUE(self),n,arglistTryToGet(arglist,1),vm);
   if (A_TIMED_OUT == r && arglistTryToGet(arglist,2) == BoolTrue)
      vmThrow(vm,E_TIME_OUT,0);
   return (r == A_OK) ? BoolTrue : BoolFalse;
}

   // Int.setAndWaitFor(AB,n,timeout/seconds=True,throw=False) -->Bool
   //   set AB and wait for self set to n
static Instance *AI_setAndWaitFor(Instance *self,pArglist arglist,pVM vm)
{
   int  r;
   Instance *ab = arglistGetBObj(arglist,0,&AtomicBoolObject, "AtomicInt.setAndWaitFor",vm);
   long n = (long)arglistGetInt(arglist,1,"AtomicInt.setAndWaitFor",vm);
   r = asyncWantN(self,ab,ATOMIC_VALUE(self),&ATOMIC_VALUE(self),n,arglistTryToGet(arglist,2),vm);
   if (A_TIMED_OUT == r && arglistTryToGet(arglist,3) == BoolTrue)
      vmThrow(vm,E_TIME_OUT,0);
   return (r == A_OK) ? BoolTrue : BoolFalse;
}


static const MethodTable aiMethods[] = 
{
   "toInt",		AI_toInt,
   "toString",		AI_toString,
   "create",		AI_create,
   "set",		AI_set,
   "isSet",		AI_isSet,
   "toBool",		AI_toBool,
   "setIf",		AI_setIf,
   "inc",		AI_inc,
   "dec",		AI_dec,
   "waitFor",		AI_waitFor,
   "setAndWaitFor",	AI_setAndWaitFor,
   0,			0
};


	///////////////////////////////// Properties

    // Int.value --> Int
static Instance *AI_value(Instance *self,pVM vm)
   { return intCreate(atomicGet(self),vm); }

static const PropertyTable aiProperties[] = 
{
   "value",		(pProperty)AI_value,
   0,			0
};


	///////////////////////////////// Op Codes

//!!! I should support self == AI & <, etc
static Instance *AI_eq(Instance *self,Instance *X,pVM vm)
{
   int64_t x;
   if (!iWantInt(X,&x,vm)) return BoolFalse;
   return boolCreate(atomicGet(self) == x);
}

static Instance *AI_neq(Instance *self,Instance *X,pVM vm)
   { return boolNot(AI_eq(self,X,vm)); }

static Instance *AI_lt(Instance *self,Instance *X,pVM vm)
   { return (atomicGet(self) < convertToInt(X,vm)) ? BoolTrue : BoolFalse; }

static Instance *AI_lte(Instance *self,Instance *X,pVM vm)
   { return (atomicGet(self) <= convertToInt(X,vm)) ? BoolTrue : BoolFalse; }

static Instance *AI_gt(Instance *self,Instance *X,pVM vm)
   { return (atomicGet(self) > convertToInt(X,vm)) ? BoolTrue : BoolFalse; }

static Instance *AI_gte(Instance *self,Instance *X,pVM vm)
   { return (atomicGet(self) >= convertToInt(X,vm)) ? BoolTrue : BoolFalse; }


#if 0 //Can't do this atomically unless a lock is used.
	// do I need to use InterlockedExchangeAdd here?
Instance *AI_add(Instance *self,Instance *X,pVM vm)
{
   long value = atomicGet(self);
   long n     = value + (long)convertToInt(X,vm);
   atomicSet(self,n);
   return intCreate(n,vm);
}

Instance *AI_sub(Instance *self,Instance *X,pVM vm)
{
   long value = atomicGet(self);
   long n  = value - (long)convertToInt(X,vm);
   atomicSet(self,n);
   return intCreate(n,vm);
}
#endif

static const OpcodeTable ai_opcodes[] = 
{
   OP_EQ,	(pOp)AI_eq,
   OP_NEQ,	(pOp)AI_neq,

   OP_LT,	(pOp)AI_lt,
   OP_LTE,	(pOp)AI_lte,
   OP_GT,	(pOp)AI_gt,
   OP_GTE,	(pOp)AI_gte,


//   OP_ADD,	(pOp)AI_add,
//   OP_SUB,	(pOp)AI_sub,

   0,		0
};


/* ******************************************************************** */
/* ****************************** Queue ******************************* */
/* ******************************************************************** */
    /* Thread safe queues with low contention for both readers and writers
     * using two circular queues.
     */

    // thread safe as n is set atomically
#define QFULL(q) ( (q)->n == TSQMAX )
// head == tail means emtpy or full, look at n

#define QSPACE(q) ( TSQMAX - (q)->n )	// queue can hold this much more

void tsQueueInit(TSQueue *tsq)
{
   IQueue *in = &tsq->incoming, *out = &tsq->outgoing;

   spinLockInit(&in->lock); spinLockInit(&out->lock); 
   CAI_ZERO(&in->n);  CAI_ZERO(&in->head);  CAI_ZERO(&in->tail); 
   CAI_ZERO(&out->n); CAI_ZERO(&out->head); CAI_ZERO(&out->tail);
}

    // Blocks
void tsQueueClear(TSQueue *tsq)
{
   IQueue *in = &tsq->incoming, *out = &tsq->outgoing;

	// do this first so flush won't find anything
   SPIN_LOCK_ACQUIRE2(&in->lock);
      CAI_ZERO(&in->n); CAI_ZERO(&in->head); CAI_ZERO(&in->tail);
   SPIN_LOCK_RELEASE(&in->lock);

   SPIN_LOCK_ACQUIRE2(&out->lock);
      CAI_ZERO(&out->n); CAI_ZERO(&out->head); CAI_ZERO(&out->tail);
   SPIN_LOCK_RELEASE(&out->lock);
}

void tsQueueMark(TSQueue *tsq)
{
   Instance **q;
   IQueue    *in = &tsq->incoming, *out = &tsq->outgoing;
   int        i,n;

   SPIN_LOCK_ACQUIRE2(&in->lock);
      for(i = in->head, n = in->n, q = in->queue; n--; i = (i + 1) % TSQMAX)
	instanceMark(q[i]);
   SPIN_LOCK_RELEASE(&in->lock);

   SPIN_LOCK_ACQUIRE2(&out->lock);
      for(i = out->head, n = out->n, q = out->queue; n--; i = (i + 1) % TSQMAX)
	instanceMark(q[i]);
   SPIN_LOCK_RELEASE(&out->lock);
}

    // OK to lie 
    // --> n (zero means empty)
__inline unsigned tsQueueHasData(TSQueue *tsq)
   { return (tsq->incoming.n + tsq->outgoing.n); }

    // --> 1 if in & out queues are full, racey
int tsQueueFull(TSQueue *tsq)
   { return (QFULL(&tsq->incoming) && QFULL(&tsq->outgoing)); }

#if 0
   // Items between head and end of array
static unsigned qBall(IQueue *q)
{
   if (q->head < q->tail) return q->n;
   return(TSQMAX - q->head);
}

   // Available space between tail and end of array
   // You hold lock for q
static unsigned q8Ball(IQueue *q)
{
   if (QFULL(q)) return 0;
   if (q->head <= q->tail) return(TSQMAX - q->tail);
   return(q->head - q->tail);
}
#endif

    /* Optimized for a read & a writer beating on the queue.
     * Nasty cases:
     * - Incoming buffer full, outgoing buffer empty, a writer & a reader:
     *   Deadlock if they "beat", need some jitter.
     * - With lots of contention (ie lots of writers OR lots of readers), this
     *   code will flail.
     * - GC HAS to get both locks in order to mark.
     * - Writers flush in chunks (good), readers can flush on each read
     *   (bad). Both are "as needed".
     */

    // 0 on failure to get lock
static __inline int spinLockAcquireMaybe(SpinLock *lock, int n)
{
   while (n--)
      if (SPIN_LOCK_TRY_TO_ACQUIRE(lock)) return 1;
   return 0;
}

    // Move items from the incoming queue to the outgoing queue
    // One of the locks is held by caller, the other is passed in
    // Returns: # objects transferred or 0 if no can do nothing
    // Other lock is released (the one I acquire)
    // Returns: 0: data moved
    //          1: data not moved: empty, outgoing is full
    //          2: can not access (can't get lock, etc)
    // Does NOT block
static int qFlush(TSQueue *tsq, SpinLock *otherLock)
{
   static unsigned int jitter = 0;

   IQueue *in = &tsq->incoming, *out = &tsq->outgoing;
   if (in->n && !QFULL(out))	// quick check
   {
#if 0	// only do a contiguous write
      unsigned n1,n2,z;
Instance **p1, **p2;

      z = 55 + (jitter++ & 7);  // attempt to avoid lock stepping
      if (!spinLockAcquireMaybe(otherLock,z)) return 2;  // too busy to flush
	 n1 = qBall(in); n2 = q8Ball(out);	// src size, dst can hold
	 if (n2 < n1) n1 = n2;
	 if (n1 == 0)		// nothing to move or dst full
	 {
	    SPIN_LOCK_RELEASE(otherLock);
	    return 1;
	 }
      #if 0
	 memcpy(&out->queue[out->tail],
	        &in->queue[in->head],n1*sizeof(Instance *));
      #else
	    // all [interested] cores need to see the pointers change
	 p1 = &out->queue[out->tail]; p2 = &in->queue[in->head];
	 for(z = n1; z--; p1++, p2++) CAP_SET(p1,*p2);
      #endif

	 CAI_SET(&in->n,   in->n - n1);
	 CAI_SET(&in->head,(in->head  + n1) % TSQMAX);

	 CAI_SET(&out->n,   out->n + n1);
	 CAI_SET(&out->tail,(out->tail + n1) % TSQMAX);
      SPIN_LOCK_RELEASE(otherLock);
      return 0;		// objects transferred

#else
      unsigned n1,n2,z,N;

      z = 55 + (jitter++ & 7);  // attempt to avoid lock stepping
      if (!spinLockAcquireMaybe(otherLock,z)) return 2;  // too busy to flush
	 N = in->n; n2 = QSPACE(out);	// src size, dst can hold
	 if (n2 < N) N = n2;
	 if (N == 0)		// nothing to move or dst full
	 {
	    SPIN_LOCK_RELEASE(otherLock);
	    return 1;
	 }
	    // all [interested] cores need to see the pointers change
	 n1 = in->head;		// src
	 n2 = out->tail;	// dst
	 for(z = N; z--; n1 = (n1+1)%TSQMAX, n2 = (n2+1)%TSQMAX)
	    CAP_SET(&out->queue[n2], in->queue[n1]);

	 CAI_SET(&in->n,   in->n - N);
	 CAI_SET(&in->head,n1);

	 CAI_SET(&out->n,   out->n + N);
	 CAI_SET(&out->tail,n2);
      SPIN_LOCK_RELEASE(otherLock);
      return 0;		// objects transferred
#endif
   }
   return 1;	// nothing to do
}

    // Compact the queue (pack data to the reader side)
    // --> 1 if data written, 0 if nothing happened
    // Blocks
int tsQueueFlush(TSQueue *tsq)
{
   size_t    n;
   IQueue   *in = &tsq->incoming, *out = &tsq->outgoing;
   SpinLock *outLock = &out->lock;

   SPIN_LOCK_ACQUIRE2(outLock);
      n = qFlush(tsq,&in->lock);
   SPIN_LOCK_RELEASE(outLock);
   return (n == 0);
}

    // Non blocking write (other than lock contention).
    // Either writes data or doesn't
    // If incoming queue is full, try to flush it to the outgoing queue
    // Returns: 0: payload has been written
    //          1: full
    //          2: can not access (can't get lock, etc)
    // Does NOT block
    // DIP Note: Payload might immediately (from GC PoV) exit to other thread
    //   ie could be orphan upon write. ie  T1 may never see payload
    //   Or queue is marked before payload (since 2+ threads own queue)
int tsQueueWrite(TSQPayload *payload)
{
   int	     s;
   Instance *i = payload->i;
   IQueue   *in = &payload->tsq->incoming, *out = &payload->tsq->outgoing;
   SpinLock *inLock = &in->lock;

   if (!spinLockAcquireMaybe(inLock,200)) return 2;	   // too busy
      if (QFULL(in) && (s = qFlush(payload->tsq,&out->lock)))   // full up
      {
	 SPIN_LOCK_RELEASE(inLock);
	 return s;	// not zero
      }
      	// update atomically so core caches are sync'd
      CAP_SET(&in->queue[in->tail],i);
      CAI_SET(&in->tail, (in->tail + 1) % TSQMAX);
      CAI_SET(&in->n,in->n + 1);
   SPIN_LOCK_RELEASE(inLock);
   return 0;	// success!
}

    // Non blocking read (other than lock contention).
    // Either reads data or doesn't
    // Returns: 0: payload has data
    //          1: no data to read (empty).
    //          2: can not access (can't get lock, etc)
    // Does NOT block
    // DIP Note: Payload might immediately (from GC PoV) exit other thread
    //   ie could be orphan now, so mark it (I may have already been marked 
    //   and other thread hasn't been marked).
int tsQueueRead(TSQPayload *payload)
{
   int	     s;
   Instance *i;
   IQueue   *in = &payload->tsq->incoming, *out = &payload->tsq->outgoing;
   SpinLock *outLock = &out->lock;

       // have to check & attempt to flush or might never see any data
   if (!tsQueueHasData(payload->tsq))      return 1;	// empty
//race here
   if (!spinLockAcquireMaybe(outLock,200)) return 2;    // too busy
#if 0
if (!tsQueueHasData(payload->tsq))	// empty
{SPIN_LOCK_RELEASE(outLock); return 1;}
#endif
      if (!out->n && (s = qFlush(payload->tsq,&in->lock))) // nothing available
      {
	 SPIN_LOCK_RELEASE(outLock);
	 return s;
      }
      i = out->queue[out->head];
      CAI_SET(&out->head, (out->head + 1) % TSQMAX);
      CAI_SET(&out->n, out->n - 1);
   SPIN_LOCK_RELEASE(outLock);
instanceIsOrphan(i);		// DIP, crossing thread boundry
   payload->i = i;
   return 0;	// success!
}

/* ******************************************************************** */
/* ****************************** Atomic ****************************** */
/* ******************************************************************** */

    /* Input: seconds (Instance), eg Pipe.read(seconds):
     *   0	: Block (no arg)
     *   Void	: Block
     *   True	: Block
     *   False	: Don't block
     *   n	: max seconds to wait, 0 == check, no blocking
     *   vm     : not used
     * Returns (millis munged):
     *   0	: Block forever, millis = long time
     *   1	: NonBlocking,   millis = long time
     *   1	: NonBlocking,   millis = 0
     * 32 bits of milliseconds is 49.7 days
     */
int getMillis(Instance *seconds, unsigned long *millis, pVM vm)
{
   *millis = RIP_VAN_WINKLE;
   if (seconds)
   {
      if (seconds == Void)      return 0;
      if (seconds == BoolTrue)  return 0;
      if (seconds == BoolFalse)
      {
	 *millis = 0;
      	 return 1;
      }
      {
	 double m = FLOATV(convertTo(seconds,TO_FLOAT,vm)) * 1000.0;
	 if (m < 0.0) *millis = 0;
	 else
	    *millis = (m < RIP_VAN_WINKLE) ? (unsigned long)m : RIP_VAN_WINKLE;
      }
      return 1;
   }
   return 0;	// no arg
}


static ZKL_Object AtomHeartMother;
static Instance	  Atomic;

static Instance *Atomic_Lock(Instance *self,pArglist arglist,pVM vm)
   { return Lock_create(self,arglist,vm); }

static Instance *Atomic_WriteLock(Instance *self,pArglist arglist,pVM vm)
   { return WriteLock_create(self,arglist,vm); }

    // Atomic.sleep([seconds]): Interruptible sleep
    // returns: True (restful sleep), or is interrupted
//!!!BUG! overflow at 16 minutes
static Instance *Atomic_sleep(Instance *self,pArglist arglist,pVM vm)
{
   return (A_TIMED_OUT == 
	   asyncSleep(arglistTryToGet(arglist,0),vm)) ? BoolTrue : BoolFalse;
}

    /* Atomic.waitFor(fcn|method|Bool, timeout/seconds=True,throw=False)
     *   --> Bool|Void
     * Returns: True (event happened), False (timeout or interrupted)
     * If fcn is interrupted, control doesn't return here.
     * If *I'm* interrupted from another thread, I return False and the
     *   caller can continue to run until the VM notices that there is a
     *   pending interrupt. Which is not exactly the expected behavior.
     * Interrupt handling is pretty twisted here:  Since I need to check for
     *   interrupts and I want to reuse a VM (for running functions), I
     *   watch both VMs:  If the running fcn is interrupted, the exception
     *   handler will take of things. If the interrupt happens while the
     *   function isn't running (ie VM is "dead"), the interrupt is set on
     *   the parent (ie the calling VM).  So, if a interrupt happens while
     *   sleeping, or the function isn't running, look at the calling VM.
     */
static Instance *Atomic_waitFor(Instance *self,pArglist arglist, pVM vm)
{
   Instance *f = arglistGet(arglist,0,"Atomic.waitFor",vm);
   Instance *result;
   int	     block, s, ft, stop;
   int	    *interrupt = vmInterruptEvent(vm);

   SnoozeAlarm alarm = { 0 };
   unsigned long millis, *timeout;

   block = !getMillis(arglistTryToGet(arglist,1),&millis,vm);

   ft = _ftyper(f,0,vm);

   timeout = block ? 0 : &millis;
   result  = BoolTrue;
   s	   = 0;
   while (1)
   {
      Instance *r = _zrun(0,f,ft,&stop,NoArglist,vm);
      if (stop) { result = Void; break; }
      if (resultToBool(r,vm)) break;      // success!, result is True

      vmMightStall(vm,1);
	 s = snooze2(&alarm,timeout,interrupt);
      vmMightStall(vm,0);
      if (A_OK != s) { result = BoolFalse; break; }
   } // while
   if (A_INTERRUPTED == s) vmProcessInterrupt(vm);
   if (A_TIMED_OUT == s && arglistTryToGet(arglist,2) == BoolTrue)
      vmThrow(vm,E_TIME_OUT,0);
   return result;
}

    // Atomic.wait(timeout,i,...) --> Void|i
    // Returns the i that had a write happend to it or Void on timeout
static Instance *Atomic_wait(Instance *self,pArglist arglist, pVM vm)
{
   if (listLen(arglist,vm) > 10)
      vmThrow(vm,E_ASSERTION_ERROR, "Atomic.wait: Too many static parameters");
   return asyncWaitList(arglist,0,vm);
}

    // Atomic.setAndwait(ab,timeout,i,...) --> Void|i
    // Set b and wait for an event. This is an atomic action.
    // Returns the i that had a write happen or Void on timeout
static Instance *Atomic_setAndWait(Instance *self,pArglist arglist, pVM vm)
{
   Instance *b = arglistGetBObj(arglist,0,&AtomicBoolObject, "Atomic.setAndWait",vm);
   if (listLen(arglist,vm) > 11)
      vmThrow(vm,E_ASSERTION_ERROR, "Atomic.setAndWait: Too many static parameters");
   return asyncWaitList(arglist,b,vm);
}

#if 0
    /* waitFor(fcn) for C objects.
     * fcn is a C function that takes self and returns:
     *    0 : Whatever it is that being waited for hasn't happened
     *   !0 : It happened. This should be postive (1 overlaps with A_OK)
     *        Fcn stores result in payload
     * atomicWaitFor returns:
     *    A_INTERRUPTED
     *    A_TIMED_OUT
     *    n : Whatever it was that fcn returned.
     * Might stall
     */
int atomicWaitFor(int (*fcn)(void *payload), void *payload, 
   Instance *seconds, pVM vm)
{
   unsigned long millis, *timeout;
   int	       r;
   SnoozeAlarm alarm = { 0 };
   int *interrupt = vmInterruptEvent(vm);

   timeout = !getMillis(seconds,&millis,vm) ? 0 : &millis;

   while (1)
   {
	// Can't call vmMightStall() around fcn() as fcn might also
	// call it and that is bad
      if ((r = fcn(payload))) break;	// fcn calls vmMightStall() if needbe
      vmMightStall(vm,1);		// GC me if it is time
         r = snooze2(&alarm,timeout,interrupt);
      vmMightStall(vm,0);
      if (r != A_OK) break;		// timeout or interrupt?
   }
   if (r == A_INTERRUPTED) vmProcessInterrupt(vm);
   return r;
}
#endif

static const MethodTable atomicMethods[] = 
{
   "Bool",	(pMethod)AB_create,
   "Int",	(pMethod)AI_create,
   "Lock",	(pMethod)Atomic_Lock,
   "WriteLock",	(pMethod)Atomic_WriteLock,
   "sleep",	(pMethod)Atomic_sleep,
   "wait",	Atomic_wait,
   "waitFor",	(pMethod)Atomic_waitFor,
   "setAndWait",Atomic_setAndWait,
   0,		0
};

//static pMethod in_ab_methods(Instance *ignore, register char *str);

void atomicConstruct(void)
{
   time0 = time(0);		// start up time, for time outs
   isrand((unsigned int)time(0));	// random #, for waits

   lockConstruct();
   writeLockConstruct();

   	// Sleeze alert: Sharing lbuckets with Lock
   constructObject(&AtomicBoolObject,AtomicBoolType,
		   abMethods,abProperties,0,NoVM);
//   AtomicBoolObject.methodSearch = in_ab_methods;
   AtomicBoolObject.vaultPath    = "Atomic";
   AtomicBoolObject.isize        = sizeof(AtomicN);
   AtomicBoolObject.threadSafe   = 1;
   AtomicBoolObject.isBInstance  = 1;
   ibucketPoach(lbuckets,&AtomicBoolObject,NoVM);

   constructObject(&AtomicIntObject,AtomicIntType,
		   aiMethods,aiProperties,ai_opcodes,NoVM);
   AtomicIntObject.vaultPath   = "Atomic";
   AtomicIntObject.isize       = sizeof(AtomicN);
   AtomicIntObject.threadSafe  = 1;
   AtomicIntObject.isBInstance = 1;
   ibucketPoach(lbuckets,&AtomicIntObject,NoVM);

   constructObject(&AtomHeartMother,NativeType, atomicMethods,0,0,NoVM);
   AtomHeartMother.name	      = "Atomic";
   AtomHeartMother.isize      = sizeof(Atomic);
   AtomHeartMother.threadSafe = 1;

   instanceInit(&Atomic,&AtomHeartMother,I_UNTOUCHABLE);
   vaultAdd("",(Instance *)&Atomic,NoVM);
}




//////////////////////////////////////////////////////////////////
// zkl extractTable -n abMethods   < atomic.c | gperf | zkl gperf -i ab
