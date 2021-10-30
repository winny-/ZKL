/* thread.c : zkl threads
 * Note: Almost everything in here has to be thread safe
 * I wish this better was documented:  On MS VS, make SURE to link with the
 *   multithreaded libraries (libcmt.lib or /MT), or you are in for a world
 *   of hurt (I'm guessing that malloc isn't thread safe in the single
 *   threaded libraries).  printf isn't. Also, apparently I have to use
 *   _beginThread() instead of CreateThread() (although CreateThread seems
 *   to work):
 *      A thread in an executable that calls the C run-time library (CRT)
 *      should use the _beginthreadex and _endthreadex functions for thread
 *      management rather than CreateThread and ExitThread; this requires
 *      the use of the multi-threaded version of the CRT.  If a thread
 *      created using CreateThread calls the CRT, the CRT may terminate the
 *      process in low-memory conditions.
 * MS also says:  The identifiers are valid from the time the thread is
 *   created until the thread has been terminated.  Note that no thread
 *   identifier will ever be 0.
 * 
 * Perf note:  There doesn't seem to be any benefit to thread pooling (ie
 * don't release a finished thread to the system only to turn around and
 * create a thread) (on Windows XP anyway).  I did a simple test:  when a
 * thread finished, instead of exiting, wait on a lock.  Launch released
 * that lock to let the thread start running again.
 * 
 * Copyright (c) 2006,2007,2008-12,2013 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

//#define _WIN32_WINNT	0x0500	// for thread pools, GetProcessIdOfThread

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008
#define _GNU_SOURCE		// pthread _np functions

#define __GC_INTERNALS
#include <stdio.h>
#include <string.h>

#ifdef _MSC_VER
   #include <process.h>		// _beginthread, _beginthreadex
   typedef int pthread_t;
#elif defined(__unix__)
   #define PTHREADS
   #define USE_THREAD_POOLS 0   // Linux/BSD: doesn't seem to make a difference
   #include <pthread.h>		// pthread_*
   #if USE_THREAD_POOLS
      #include "thr_pool.h"
      #include <errno.h>
   #endif
#endif

#define __NOT_A_DLL
#define __FCN_INTERNALS

#include "zklObject.h"
#include "zklAtomic.h"
#include "zklFcn.h"
#include "zklClass.h"
#include "zklList.h"
#include "zklNumber.h"

#define LIFT_OFF_FCN_NAME	"liftoff"
#define SPLASH_DOWN_FCN_NAME	"splashdown"

static unsigned liftoffId, splashdownId;

typedef struct Thread
{
   Instance	*klass;		// the class which called launch 
   Instance	*fcn;		// a copy, a floater
   Instance	*arglist;	// NOT a MList
   Instance	*ri;		// where to put result if wanted
   pVM	         vm;
   pthread_t     pid;
   unsigned int  splash:1;	// 1 if run splashdown fcn
   unsigned int  rtnVM:1;	// 1 if .launch() wants a thread id
   unsigned int  rtnR:1;	// 1 if write result to straw
   CAtomicInt	 markMe;	// tell the VM how to handle marking
   struct Thread *next;
} Thread;	// 32 bytes on 32 bit Visual C

static Thread	*threads = 0;
static SpinLock  threadLock;

#if USE_THREAD_POOLS
   static thr_pool_t *threadPool = 0;
#endif

    // 6 to compile parser, 165+ when running tests
static int runningThreads, highWater;
static IBucketHeader thrBuckets;

   // A VM calls this to mark the thread. You keep out!
void markThread(void *pSelf)
{
   Thread *self = pSelf;

   if (pSelf < (void *)10 || self->markMe != 2)
   {
      if (self->markMe == 0) return;	// dead thread
      return;	// not a Thread [yet]
   }

   instanceMark(self->fcn);	// will mark klass (fcn.container)
   instanceMark(self->klass);	// probably won't mark fcn (unless Eve)
   instanceMark(self->arglist);
   instanceMark(self->ri);
}

int numThreads(int *max)
{
   if (max) *max = highWater;
   return runningThreads;
}

    /* Returns markMe:
     *   0: Dead thread, don't mark
     *   1: Thread embryo, no pid (yet), wait for me to hatch
     *   2: Running thread.
     *   9: You sent me garbage
     */
int threadPID(void *self, pthread_t *pid)
{
   int markMe;

   if (!self)		  return 9;	// not a Thread
   if (self == (void *)1) return 1;	// thread embryo
   if (self < (void *)10) return 9;	// not a Thread

   #if defined(PTHREADS)	// no thread pid for you
      markMe = ((Thread *)self)->markMe;
   #else
      SPIN_LOCK_ACQUIRE(&threadLock);  // Get the next two atomically (liftoff)
	 markMe = ((Thread *)self)->markMe;
	 if (pid) *pid = ((Thread *)self)->pid;
      SPIN_LOCK_RELEASE(&threadLock);
   #endif
   return markMe;
}

static Thread *launch(Instance *klass,Instance *fcn,pArglist,int,pVM);

    /* class.launch(args): Create a Thread and start running it
     * Input: 
     *    Class/fcn: A class that contains fcn, run fcn
     *    Class/0:   A class instance with a "liftoff" fcn.
     *       If fcn klass.splashdown exists, it is called.
     *    flags: 
     *       1: Create a straw the will hold the result (when thread finishes).
     *       2: Return a Ref that will hold the VM when the thread starts.
     */
#define THREAD_RTN_RESULT	1	// you get none or one of these two
#define THREAD_RTN_VM		2

#define THREAD_SPLASHDOWN   0x100	// only I do this
Instance *threadCreate(Instance *klass, Instance *fcn, pArglist arglist, 
   int flags, pVM vm)
{
   #if 0  // sanity check, #define __LIST_INTERNALS
      {
	 LSize_t    n;
	 Instance **args, *i;
	 args = listTable(arglist,&n);
	 while(n--){ i = *args++;
	    if(!IS_PtrInt(i) && IS_IMEM(i))
	       vmHalt("threadCreate: MList");
	 }
      }
   #endif

   #if 0
      // only thread safe objects
   {
      Instance *i=0;
      int       n;

      for (n = 0; (i = listGet(arglist,n)); n++)
      {
	 if(!I_IS_THREAD_SAFE(i))
	 {
	    char buf[100];
	    sprintf(buf,"Only thread safe objects passed to threads: %s",iname(i));
	    vmThrow(vm,E_TYPE_ERROR,buf);
	 }
      }
   }
   #endif

   if (!fcn)
   {
	   // Create [floating] copy of liftoff fcn
      fcn = classFindFcnById(klass,liftoffId,1,0,vm);
      if (!fcn)
      {
	 char buf[100];
	 sprintf(buf,"launch(): Class \"%s\" must have a \"%s\" fcn",
		   className(klass),LIFT_OFF_FCN_NAME);
	 vmThrow(vm,E_NOT_FOUND,buf);
      }
      flags |= THREAD_SPLASHDOWN;
   }

   return launch(klass,fcn,arglist,flags,vm)->ri;
}

    /* Run class.splashdown(happyDeath,exception)
     * Runs in the thread that ran class.liftoff();
     * Thread and VM have been freed at this point so I need a new VM.
     * Exception and klass is protected by liftoff()\
     * 
     * klass is protected by caller, which also protects fcn (not a GCable obj)
     */
static void splashdown(Instance *klass,Instance *exception)
{
   MLIST(mlist,2);
   ZKL_Fcn   fcn;  // the splashdownFcn bits
   Instance *splashdownFcn, *arglist;

   if (!klass) return;		// fcn.launch() == no splashdown
   splashdownFcn = classFindFcnById(klass,splashdownId,1,&fcn,NoVM);
   if (!splashdownFcn) return;		// no splashdown fcn

   arglist = exception ? mlistBuild(mlist,BoolFalse,exception,ZNIL) :
			 mlistBuild(mlist,BoolTrue,Void,ZNIL);
   // No parent, create a new root VM
   rogueFcnRun(splashdownFcn,arglist,0);  // can throw
}

static void freeThread(Thread *self)
{
   vmFree(self->vm);		  // Free VM now to keep GC from deadlocking.
   CAI_ZERO(&self->markMe);	  // dead thread, vmFree can mark
   spinLockAcquire(&threadLock);  // Free Thread
   {
      Thread *prev, *ptr;

      self->pid = 0;	  // redundant but looks good
      runningThreads--;

	// I KNOW self is in the list
      for (prev = 0, ptr = threads; ptr != self;
	   prev = ptr, ptr = ptr->next) ;
      if (prev) prev->next = ptr->next;
      else      threads    = ptr->next;

      bucketFree(&thrBuckets,(Instance *)self);
   }
   SPIN_LOCK_RELEASE(&threadLock);
}

void display_pthread_attr(pthread_t,char *prefix);

    // Run class.liftoff(args) in its Thread
    // Many GC cycles may have passed since launch()
#ifdef _MSC_VER
   //static void __cdecl liftoff(void *pSelf)	// _beginthread
static unsigned __stdcall liftoff(void *pSelf)	// _beginthreadex
#elif defined(PTHREADS)
static void *liftoff(void *pSelf)		// Pthreads
#endif
{
   Thread   *self = pSelf;	// so the signature is an "offical" thread sig
   Instance *result;
   int	     status, rtnR=self->rtnR, splash=self->splash;
   Fence     fence;
   Instance *refSet(Instance *self, Instance *value, pVM vm);	// miscObj.c

//HANDLE handle = GetCurrentThread();	// 0xfffffffe
//   self->handle = OpenThread(THREAD_ALL_ACCESS,0,self->pid);
// n = GetProcessIdOfThread(GetCurrentThread());	// Vista

   #ifdef PTHREADS
      self->pid = pthread_self();	// informational
   #elif defined(_MSC_VER)
      SPIN_LOCK_ACQUIRE(&threadLock);
         self->pid = GetCurrentThreadId();
	 CAI_SET(&self->markMe,2);	// NOW we can proceed with GC
      SPIN_LOCK_RELEASE(&threadLock);
   #endif

   // all kinds of bad things happen if VM is returned before thread starts
   if(self->rtnVM) refSet(self->ri,vm2Instance(self->vm),self->vm);
	// run the VM, let vmRunFcn() mark (if called from splashdown())
   result = vmRunFcn(self->vm,self->fcn,self->arglist,&status);
	// protect klass & exception until splashdown is finished
//!!! I don't like this
   vmSetFence(NoVM,&fence,0,self->klass);  // for when thread struct goes away
      if (status != VM_DONE)	// get the thrown exception
	 fence.i1 = vmRegX(self->vm);
      fence.i2 = result;
      fence.i3 = self->ri;	// Straw for holding future value

      freeThread(self);		// VM is dead, free it

      switch (status)
      {
         //if splashdown throws, it still returns here
	 case VM_DONE:			// Finished with no errors
	    if (rtnR)   strawSet(fence.i3,result,1);
	    if (splash) splashdown(fence.i,0);  // not called for futures
	    break;
	 case VM_ERROR:			// thread died a painful death
	 case VM_HALT:
	    if (rtnR)   pipeBreakIt(fence.i3,fence.i1);
	    if (splash) splashdown(fence.i,fence.i1); // not called for futures
	    break;
	 default:
	    vmHalt("launch: should not get here");
	    break;
      }
   vmRemoveFence(&fence,0);
   // __exception not used anymore
//!!!???? result is orphan???

   return 0;		// exit thread or return thread to thread pool
}

static Thread *launch(    //  klass & fcn will be protected
    Instance *klass,Instance *fcn,pArglist arglist, int flags, pVM spawningVM)
{
   Thread   *self;
   pVM       vm;
   Fence     fence;
   Instance *args=0, *ri=Void;
   int       rtnVM=0, rtnR=0;

   Instance *refCreate(void *,int, pVM);	// miscObj.c

   vmSetFence(spawningVM,&fence,0,klass); fence.i1 = fcn;
      if(flags & THREAD_RTN_RESULT){ ri = strawCreate(spawningVM);      rtnR  = 1; }
      if(flags & THREAD_RTN_VM)    { ri = refCreate(Void,0,spawningVM); rtnVM = 1; }
      fence.i2 = ri;
      args = arglistDup(arglist,0,spawningVM);  // maybe a DIP
      fence.i3 = args;
      // create a root VM that won't GC until liftoff() (on Windows)
      vm = vmCreate(NoVM,NoBlock,VM_THREAD2BE,spawningVM);
      spinLockAcquire(&threadLock);	// so GC won't deadlock
	 self = (Thread *)ibucketAllocate(&thrBuckets,
				0,I_INVISIBLE,1,spawningVM); // throws
	 self->next = threads; threads = self;
	 if (++runningThreads > highWater) highWater = runningThreads;
	 self->klass   = klass;
	 self->fcn     = fcn;
	 self->pid     = 0;		// redundant but looks good
	 self->vm      = vm;
	 self->ri      = ri;		// Straw or Ref or 0 or Void
	 self->splash  = (flags & THREAD_SPLASHDOWN)!=0;
	 self->rtnVM   = rtnVM;
	 self->rtnR    = rtnR;
	 self->arglist = args;
	 CAI_INIT2(&self->markMe,1);	// needs marking, GC waits for this
      SPIN_LOCK_RELEASE(&threadLock);
      // GC waits for this event, followed by liftoff():
      #if defined(PTHREADS)	// Windows gets to wait for liftoff
         CAI_SET(&self->markMe,2);	// NOW we can proceed with GC
      #endif
      vmIsAThread(vm,self);	// VM now knows this is a Thread
   vmRemoveFence(&fence,0);

//!!!???? orphan???
//instanceIsOrphan(fcn);	// vm marked

   #if USE_THREAD_POOLS
      if (-1 == thr_pool_queue(threadPool,liftoff,self))
      {
	 char buf[200];
	 freeThread(self);
	 sprintf(buf,"launch: thr_pool_queue() failed (%d)\n",errno);
	 vmThrow(spawningVM,E_VM_ERROR,buf);
      }
   #elif defined(_MSC_VER)
   {
   #if 0
      uintptr_t	thread = _beginthread(liftoff,0,self);
      if (thread == -1L)
	 printf("   launch: _beginThread failed (%s)\n",strerror(errno));
   #else
      uintptr_t thread = _beginthreadex(0,0,liftoff,self,0,0);
      if (!thread)
      {
	 char buf[200];
	 freeThread(self);
	 sprintf(buf,"launch: _beginThread failed (%s)\n",strerror(errno));
	 vmThrow(spawningVM,E_VM_ERROR,buf);
      }
      CloseHandle((HANDLE)thread);
   #endif
   }
   #elif defined(PTHREADS)
   {
      // chrt -m for list of priorities
      int s;
      pthread_attr_t attr;

      s = pthread_attr_init(&attr);
      if (s != 0) printf("pthread_attr_init pooh: %d\n",s);

      // Linux default attributes: joinable, system scope, SCHED_OTHER
      s = pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_DETACHED);
      if (s) printf("pthread_attr_setdetachstate pooh: %d\n",s);

      	// PTHREAD_SCOPE_SYSTEM, PTHREAD_SCOPE_PROCESS (not supported on Linux)
   #ifndef __minix__
      s = pthread_attr_setscope(&attr,PTHREAD_SCOPE_SYSTEM);
      if (s != 0) printf("pthread_attr_setscope pooh: %d\n",s);
   #endif
      // Linux: thread affinity is all cores

//!!!!???? __pthread_create_2_1 713 Segmentation fault
//      if ((s = pthread_create(&self->pid,&attr,liftoff,self)) != 0)
      if ((s = pthread_create(&self->pid,0,liftoff,self)) != 0)
      {
	 char buf[200];
	 freeThread(self);
	 sprintf(buf,"launch: pthread_create failed (%d)\n",s);
	 vmThrow(spawningVM,E_VM_ERROR,buf);
      }
//      s = pthread_detach(self->pid);
//      if (s) printf("pthread_detach() didn't: %d\n",s);

      // will fail when created thread exits before display is done
      //display_pthread_attr(self->pid,"\t");

      pthread_attr_destroy(&attr);

#if 0
{ // SCHED_BATCH(3)(not supported on Linux), SCHED_OTHER(0), SCHED_NORMAL(0)
  // SCHED_FIFO(1), SCHED_RR(2) // these need a priority of 1-99 & sudo
   struct sched_param param;
   param.sched_priority = 1;
   s = pthread_setschedparam(self->pid, SCHED_FIFO, &param);
   if (s != 0)
      if (s) printf("pthread_setschedparam() didn't: %d\n",s);
}
#endif

   }
   #endif // PTHREADS

   // and the thread is off and running
   return self;
}


void threadConstruct(void)
{
   spinLockInit(&threadLock);
	// NO GC on these buckets
   bucketReserve(sizeof(Thread),11,&thrBuckets,0,NoVM);

   #if USE_THREAD_POOLS
      threadPool = thr_pool_create(3,1000,60,0);
      if (!threadPool)
      {
	 char buf[200];
	 sprintf(buf,"thr_pool_create() didn't (%s)\n",strerror(errno));
	 vmThrow(NoVM,E_VM_ERROR,buf);
      }
   #endif
   liftoffId    = addStaticNametoGlobalTable(LIFT_OFF_FCN_NAME,NoVM);
   splashdownId = addStaticNametoGlobalTable(SPLASH_DOWN_FCN_NAME,NoVM);
}



#if defined(PTHREADS) && 0
//#include <pthread_np.h>  // FreeBSD. I have no idea where the library is

   #define handle_error_en(en, msg) printf("%s pooh: %d",msg,en);

   void display_pthread_attr(pthread_t pid, char *prefix)
   {
       int s, i;
       size_t v;
       void *stkaddr;
       pthread_attr_t _attr, *attr = &_attr;
       struct sched_param sp;
       cpu_set_t cpuset;	// cpuset_t on FreeBSD

       printf("Thread attributes:\n");

       s = pthread_attr_init(attr);
       pthread_getattr_np(pid,attr);	// BSD: pthread_get_attr_np

       s = pthread_attr_getdetachstate(attr, &i);
       if (s != 0)
	   handle_error_en(s, "pthread_attr_getdetachstate");
       printf("%sDetach state        = %s\n", prefix,
	       (i == PTHREAD_CREATE_DETACHED) ? "PTHREAD_CREATE_DETACHED" :
	       (i == PTHREAD_CREATE_JOINABLE) ? "PTHREAD_CREATE_JOINABLE" :
	       "???");

       s = pthread_attr_getscope(attr, &i);
       if (s != 0)
	   handle_error_en(s, "pthread_attr_getscope");
       printf("%sScope               = %s\n", prefix,
	       (i == PTHREAD_SCOPE_SYSTEM)  ? "PTHREAD_SCOPE_SYSTEM" :
	       (i == PTHREAD_SCOPE_PROCESS) ? "PTHREAD_SCOPE_PROCESS" :
	       "???");

       s = pthread_attr_getinheritsched(attr, &i);
       if (s != 0)
	   handle_error_en(s, "pthread_attr_getinheritsched");
       printf("%sInherit scheduler   = %s\n", prefix,
	       (i == PTHREAD_INHERIT_SCHED)  ? "PTHREAD_INHERIT_SCHED" :
	       (i == PTHREAD_EXPLICIT_SCHED) ? "PTHREAD_EXPLICIT_SCHED" :
	       "???");

       s = pthread_attr_getschedpolicy(attr, &i);
       if (s != 0)
	   handle_error_en(s, "pthread_attr_getschedpolicy");
       printf("%sScheduling policy   = %s\n", prefix,
	       (i == SCHED_OTHER) ? "SCHED_OTHER" :
	       (i == SCHED_FIFO)  ? "SCHED_FIFO" :
	       (i == SCHED_RR)    ? "SCHED_RR" :
	       "???");

       s = pthread_attr_getschedparam(attr, &sp);
       if (s != 0)
	   handle_error_en(s, "pthread_attr_getschedparam");
       printf("%sScheduling priority = %d\n", prefix, sp.sched_priority);

       s = pthread_attr_getguardsize(attr, &v);
       if (s != 0)
	   handle_error_en(s, "pthread_attr_getguardsize");
       printf("%sGuard size          = %lu bytes\n", prefix, v);

       s = pthread_attr_getstack(attr, &stkaddr, &v);
       if (s != 0)
	   handle_error_en(s, "pthread_attr_getstack");
       printf("%sStack address       = %p\n", prefix, stkaddr);
       printf("%sStack size          = 0x%lx bytes\n", prefix, v);

       pthread_attr_destroy(attr);

       // affinity is set to all CPUs
       s = pthread_getaffinity_np(pid,sizeof(cpu_set_t),&cpuset);
       if (!s)
       {
	  printf("%sCPU affinity: ",prefix);
	  for(s=0; s<CPU_SETSIZE; s++) // FreeBSD doesn't have CPU_SETSIZE
	     if (CPU_ISSET(s,&cpuset)) printf("%d ",s);
       }
       printf("\n");
   }
#endif
