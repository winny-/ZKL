/* zklAtomic.h : Threading support.
 * If you are writing zkl C code, use zklObject.h instead.  Use this header
 * if you wish to use these constructs without referring to zkl.
 * 
 * Copyright (c) 2006,7,8,9,10,2011 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#ifndef __ZKL_ATOMIC_H
#define __ZKL_ATOMIC_H

//#include <intrin.h>		// VC 9

#ifdef __unix__
   #include <unistd.h>		// sleep, usleep
//   #include <sched.h>		// sched_yield()  Minix3 macro collision
   #ifdef __NEED_PTHREADS
      #include <pthread.h>	// pthread_*, PITA for Minix3 DLL Makefiles
   #endif
#endif

#ifndef __ZKL_OBJECT_H	  // zklObject.h wasn't included, copy some stuff
   #ifdef _MSC_VER
      #include <Windows.h>
   #endif

   #ifdef _MSC_VER
      #define DllExport	__declspec( dllexport )
   #elif defined(__GNUC__)
      #define DllExport	__attribute__ ((visibility("default")))
   #else
      #define DllExport
   #endif
#endif

	// s < A_OK --> timeout or interrupt
#define A_RESTART	-4	// 
#define A_RETRY		-3	// your obj is contested
#define A_INTERRUPTED	-2	// kicked by another VM
#define A_TIMED_OUT	-1	// you are out of time for this op
#define A_FAIL		 0	// a "clean" failure to complete
#define A_OK		 1	// success!
#define A_USER		10	// start your success return codes here

void atomicConstruct(void);

    /* Note on OS yields in spins:  Probably not a good idea as the OS
     * doesn't know that the intent is to give cycles to another thread that
     * is actually doing work.  As far as the OS is concerned, a spin IS
     * real work so it may well just give the cycles to another spinner.
     * I think (ie guess) that graduated sleeps are the best I can do.
     */
    
#ifdef _MSC_VER
   typedef long int volatile CAtomicInt;   // An int I can change atomically

	// these return the previous value
   #define CAI_SET(ai,n)   InterlockedExchange(ai,n)
   #define CAI_INIT(ai)	   CAI_SET(ai,0)
   #define CAI_INIT2(ai,n) CAI_SET(ai,n)
   #define CAI_ZERO(ai)    CAI_SET(ai,0)
   #define CAI_ONE(ai)     CAI_SET(ai,1)

	// these return the new value
   #define CAI_INC(ai)	   InterlockedIncrement(ai)
   #define CAI_DEC(ai)	   InterlockedDecrement(ai)

	// current value
   #define CAI_VALUE(ai)   ( *(ai) )

	// returns the previous value
   #define CAP_SET(pDst,ptr) InterlockedExchangePointer(pDst,ptr)
   	// compare and exchange pointers (CAS for pointers): returns old val
   	// compare2dst should be a const or known not to change
   #define CAP_XP(pDst,val,compare2dst)	\
	InterlockedCompareExchangePointer(pDst,val,compare2dst)

	// SwitchToThread() == Sleep(0), context switch
   #define SWITCH_THREADS    SwitchToThread()
   #define SPIN_STALL	     SWITCH_THREADS	// let somebody else do work

   #ifdef _DEBUG	// /Oi
      long _InterlockedCompareExchange(long volatile * Destination,
	   long Exchange,long Comparand);
      #pragma intrinsic (_InterlockedCompareExchange)
   #endif

      // lock is CAtomicInt aka volatile int
   #define SPIN_LOCK_TRY_TO_ACQUIRE(lock)		\
      ( 0 == _InterlockedCompareExchange(lock,1,0) )	// 1 if acquired
   #define SPIN_LOCK_ACQUIRE(lock)			\
      while (_InterlockedCompareExchange(lock,1,0)) SPIN_STALL

   #define SPIN_LOCK_WAIT_WHILE_LOCKED(lock)	\
      while (CAB_VALUE(lock)) SPIN_STALL
   #define SPIN_LOCK_WAIT_UNTIL_LOCKED(lock)	\
      while (!CAI_VALUE(lock)) SPIN_STALL

#if 0
   #define SPIN_LOCK_ACQUIRE2(lock)					     \
   {  int n = 255;							     \
      while (--n) if (0 == InterlockedCompareExchange(lock,1,0)) break;      \
      if (!n) while (InterlockedCompareExchange(lock,1,0)) SwitchToThread(); \
   }
#else
   #define SPIN_LOCK_ACQUIRE2(lock)				     \
   {  int n = 255; 						     \
      while (--n) if (SPIN_LOCK_TRY_TO_ACQUIRE(lock)) break;	     \
      if (!n) SPIN_LOCK_ACQUIRE(lock);				     \
   }
#endif

#elif defined(__GNUC__)		// these issue a full memory barrier 

   typedef int volatile	CAtomicInt;	// An int I can change atomically
      
	// these return the previous value
   #define CAI_INIT(ai)    __sync_lock_test_and_set(ai,0)
   #define CAI_INIT2(ai,n) CAI_SET(ai,n)
   #define CAI_ZERO(ai)    CAI_INIT(ai)
   #define CAI_ONE(ai)     CAI_SET(ai,1)
	// on non-Intel, CAI_SET might only be valid if n==1. 
	// Only an acquire barrier
   #define CAI_SET(ai,n)   __sync_lock_test_and_set(ai,n)

	// these return the new value
   #define CAI_INC(ai)	   __sync_add_and_fetch(ai,1)
   #define CAI_DEC(ai)	   __sync_sub_and_fetch(ai,1)

	// current value
   #define CAI_VALUE(ai)   ( *(ai) )

	// returns the previous value
   #define CAP_SET(pDst,ptr) __sync_lock_test_and_set(pDst,ptr)
   	// compare and exchange pointers (CAS for pointers): returns old val
   	// compare2dst should be a const or known not to change
   #define CAP_XP(pDst,val,compare2dst)	\
		__sync_val_compare_and_swap(pDst,compare2dst,val)
 
// __sync_synchronize() : full memory barrier 

//   #define SWITCH_THREADS	     usleep(1)
   #define SWITCH_THREADS	     sched_yield()
//pthread_yield if VM->isThread
//   #define SPIN_STALL	     SWITCH_THREADS	// let somebody else do work
   #define SPIN_STALL	     usleep(1)		// huge win

      // lock is CAtomicInt aka volatile int
   #define SPIN_LOCK_TRY_TO_ACQUIRE(lock)		\
      __sync_bool_compare_and_swap(lock,0,1)	// 1 if lock acquired
   #define SPIN_LOCK_ACQUIRE(lock)			\
      while (!__sync_bool_compare_and_swap(lock,0,1)) SPIN_STALL

   #define SPIN_LOCK_WAIT_WHILE_LOCKED(lock)	\
      while (CAI_IS_SET(lock)) SPIN_STALL
   #define SPIN_LOCK_WAIT_UNTIL_LOCKED(lock)	\
      while (!CAI_VALUE(lock)) SPIN_STALL

   #define SPIN_LOCK_ACQUIRE2(lock)				     \
   {  int n = 255; 						     \
      while (--n) if (SPIN_LOCK_TRY_TO_ACQUIRE(lock)) break;	     \
      if (!n) SPIN_LOCK_ACQUIRE(lock);				     \
   }

#else
   typedef char I_dont_have_any_atomic_ops[-1];
#endif

DllExport int	  getMillis(Instance *seconds, unsigned long *millis, pVM);

typedef CAtomicInt SpinLock;		// zero is unlocked, one is locked

#define SPIN_LOCK_LOCK(lock)	   CAI_SET(lock)	// ptr
#define SPIN_LOCK_RELEASE(lock)    CAI_ZERO(lock)	// ptr
#define SPIN_LOCK_IS_LOCKED(lock)  CAI_VALUE(lock)	// ptr

DllExport void spinLockInit(    SpinLock *);
DllExport int  spinLockAcquire( SpinLock *);
DllExport int  spinLockAcquire2(SpinLock *, int canInterrupt, pVM);
DllExport int  spinLockAcquire3(SpinLock *, int n);
DllExport void spinLockRelease( SpinLock *);

DllExport void aslAcquire(SpinLock *, pVM);
DllExport void aslRelease(SpinLock *);

typedef struct
{
   SpinLock	writing;	// locked if writing
   CAtomicInt	readers;	// number of readers
} WriteLock;

#define WL_WRITER	0
#define WL_READER	1

DllExport void writeLockInit(WriteLock *);
DllExport void writeLockAcquire(WriteLock *,int readerOrWriter,pVM);
DllExport void writeLockElevate(WriteLock *,pVM);
DllExport void writeLockRelease(WriteLock *,int readerOrWriter);


#define TSQMAX	2048	// size of queue buffer

typedef struct
{
   SpinLock   lock;
   CAtomicInt n,head,tail;
   Instance  *queue[TSQMAX]; // also Atomic but don't know how to express that
} IQueue;

typedef struct { IQueue incoming, outgoing; } TSQueue;

typedef struct
{
   TSQueue  *tsq;
   Instance *i;
} TSQPayload;

DllExport void     tsQueueInit(TSQueue *);
DllExport int      tsQueueRead(TSQPayload *);
DllExport int      tsQueueWrite(TSQPayload *);
DllExport void     tsQueueClear(TSQueue *);
DllExport int      tsQueueFlush(TSQueue *);
DllExport void     tsQueueMark(TSQueue *);
DllExport unsigned tsQueueHasData(TSQueue *);
DllExport int	   tsQueueFull(TSQueue *);


#if 0
typedef struct
{
   CAtomicInt     n;
   pthread_cond_t cond;
//pthread_mutex_t mutex;
} ACInt;

#define  AC_INT_VALUE(aci) CAI_VALUE(&((aci)->n))
#define  AC_INT_ZERO(aci)  acIntSet(aci,0)
#define  AC_INT_ONE(aci)   acIntSet(aci,1)

DllExport void acIntInit(ACInt *,int);
DllExport void acIntSet(ACInt *,int);
DllExport int  acIntWaitFor(ACInt *, int v, int block, unsigned long millis);
#endif


DllExport int atomicWaitFor(int (*fcn)(void *payload), void *payload,
			Instance *seconds, pVM);

DllExport int asyncRead(Instance *,int (*fcn)(void *payload), void *payload,
			Instance *seconds, pVM);
DllExport int asyncWrite(Instance *,int (*fcn)(void *payload), void *payload,
			 Instance *seconds, pVM);
DllExport int asyncWantN(Instance *, Instance *, int cv,CAtomicInt *,int n, 
			Instance *seconds, pVM);
DllExport int asyncBlock(Instance *,int (*fcn)(void *payload), void *payload, pVM);
DllExport void asyncReadHappened(Instance *);
DllExport void asyncWriteHappened(Instance *);
DllExport void asyncWroteN(Instance *,int);
DllExport void asyncEventHappened(Instance *i,int flags, int n);
DllExport void asyncClosed(Instance *);

DllExport Instance *asyncWaitList(Instance *list, Instance *,pVM);

DllExport int asyncSleep(Instance *seconds, pVM vm);

DllExport void caiSet(CAtomicInt *,int n);
DllExport int  caiWaitFor(CAtomicInt *,int want,Instance *timeout,pVM);
DllExport int  caiWait4(CAtomicInt *,int want,int block,unsigned long millis);


DllExport int lockAcquire(SpinLock *, Instance *timeout, pVM);
DllExport int intWaitFor(CAtomicInt *, long value,
			int waitForever, unsigned long millis, pVM);

DllExport long int atomicGet(Instance *);

#endif // __ZKL_ATOMIC_H
