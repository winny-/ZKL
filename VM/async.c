/* async.c : An asynchronous system
 * 
 * Copyright (c) 2013,2014 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

/* This file is an api for multiple threads to tickle each other.  It is a
 * simple yes/no message passing with the important features being:
 * - thread 1 can signal T2 before T2 starts waiting and T2 won't miss the
 *   signal (when it does start waiting).
 *   This requires the kernal to queue or at least be sticky.
 * - The waiting thread doesn't consume cpu (ie a context switch is required).
 * - A wait can time out.
 * - A gag-me-with-a-spoon interm solution is a user space spin wait
 *   solution if OS support doesn't exist (eg Minix3).
 * 
 * Algorithm:
 * - Two (or more) threads agree on a sentinal object
 * - T1 waits on object
 *   - has object been signaled?
 *   - initialize OS machinery that will do signaling
 *   - has object been signaled?
 *   <race if OS doesn't queue>
 *   - context switch to the OS to wait for signal
 * - T2 signals object
 *   - sets object
 *   - notifies OS to send signal to wake up T1
 * - T1 wakes up.
 * T1 can be interrupted or timeout. The above is time independant, ie
 * unordered.
 * 
 * Pthread semaphores seem to be the best fit for Unix, Windows evnets are
 * the most pleasant to work with.
 * 
 * jeezus, Windows kicks Linux butt but uses way more CPU
 * I'm pretty sure this is because the thread scheduler sucks
 * 
 * pthread condition variables don't work because they don't queue; ie I
 *   can't eliminate race conditions (thread 2 signals me before I start
 *   waiting).  Yes, I've seen this.
 * pthread mutexes don't work because the only timer function is
 *   pthread_mutex_timedlock and I can't lock a mutex and then wait to lock
 *   it again (ie wait for T2 to unlock it).
 * 
 * sockets seem way too heavy weight for this task.
 * 
 * Message queues look like they would work but librt doesn't seem to be on
 * Minix3 (no librt, does have the headers).  Which also seems to be missing
 * pthread semaphores and pthread_sigqueue.
 */

#ifdef _MSC_VER
   #define _CRT_SECURE_NO_WARNINGS		// VC++ 2008
#elif defined(__unix__)
   #if defined(__minix__)
      #define SIT_AND_SPIN 1  // Minix3, no context switching
   #else	// one of the following
      #define PT_SEMAPHORE 1  // use pthread semaphores. !Minix3
      //#define SIT_AND_SPIN 1  // this works too, about as fast as PT_SEMAPHORE
      //#define _GNU_SOURCE	// pthread_sigqueue(), PT_SIG
   #endif
   #include <pthread.h>		// pthread_*, also included in zklAtomic.h
   #include <semaphore.h>
#endif

#include <stdio.h>
#include <signal.h>
#include <errno.h>

#define __NOT_A_DLL

#include "zklObject.h"
#include "zklAtomic.h"
#include "zklList.h"

extern int  abSet(Instance *,int);	// atomic.c
extern void vmProcessInterrupt(pVM);	// vm.c

#if PT_SIG
   static int mySignal;
#endif

typedef struct WaitList
{
   struct WaitList *next, *prev;
   Instance  *i;
   int	      n, all;
   pVM	      vm;
   CAtomicInt check, sv;	// since events don't have payloads
   #if _MSC_VER
      HANDLE      event;
   #elif PT_SEMAPHORE
      sem_t      *psem;
   #elif SIT_AND_SPIN
      CAtomicInt *sentinal;
   #elif PT_SIG
      pthread_t   tid;
   #endif
} WaitListNode;

static CAtomicInt waitLock;

    // circular queues
static struct WaitList
   waitingForReadList  = { &waitingForReadList, &waitingForReadList  },
   waitingForWriteList = { &waitingForWriteList,&waitingForWriteList };

   // header node (above) is dummy, points to self if empty
   // else to first node, the last node points to the header
#define FIRST(list) (list)->next
#define LAST(list)  (list)->prev

      // atomically link w at end of waitList
static void appendToList(struct WaitList *list, WaitListNode *w)
{
   SPIN_LOCK_ACQUIRE(&waitLock);  // w is on C stack
   {
      struct WaitList *end;
      end	    = list->prev;
      //end->next     = w;
      CAP_SET(&end->next,w);
      w->prev	    = end;
      w->next       = list;
      //waitList.prev = w; 
      CAP_SET(&list->prev,w);	// set in all CPU caches
   }
   SPIN_LOCK_RELEASE(&waitLock);
}

//sigpending

static void removeFromList(WaitListNode *w)
{
   SPIN_LOCK_ACQUIRE(&waitLock);
   {
      WaitListNode *prev = w->prev, *next = w->next;
      //prev->next = next; next->prev = prev;
      CAP_SET(&prev->next,next);
      CAP_SET(&next->prev,prev);
   }
   SPIN_LOCK_RELEASE(&waitLock);

   /* there is a race here: somebody could have sent me a signal between
    * when I stopped waiting and when I was unlinked
    */
#if 0
   {
      int	s;
      siginfo_t info;
      sigset_t  sigset;
      struct timespec timeout = { 0,0 };

      sigemptyset(&sigset); sigaddset(&sigset,mySignal);
      s = sigtimedwait(&sigset,&info,&timeout);
      if ( !(s == -1 && errno == EAGAIN) )
	 printf("RACERACERACERACERACERACERACE\n");
   }
#endif
}

#if PT_SEMAPHORE || SIT_AND_SPIN
    // calc abs_timeout from interval and start time
static void toInfinityAndBeyond(unsigned long millis, struct timespec *then)
{
   // 1 sec == 1,000 millis, 1 milli == 1,000,000 nanos
   // timeout routines EINVAL with tv_nsec @ 1 sec (1e9 nanos) or < 0
   // tv_nsec is long, 32 signed bits == 2.14e9 (ie just over 2 sec)
   // Minix3: tv_sec is long long, tv_nsec is long
   #define MILLIS_PER_NANO ((unsigned long)1e6)
   #define NANOS_PER_SEC   ((unsigned long)1e9)

   uint32_t	   nanos;	// at least 32 bits
   struct timespec now;
   lldiv_t         n, m=lldiv(millis,1000); //-->(millis/1000, millis%1000)

   clock_gettime(CLOCK_REALTIME,&now);
   nanos = now.tv_nsec + m.rem*MILLIS_PER_NANO;  // < 3.2 sec, fits in 32 bits
   n     = lldiv(nanos,NANOS_PER_SEC);
   then->tv_sec  = now.tv_sec + m.quot + n.quot;
   then->tv_nsec = n.rem;
//   printf("timeout sec : %lu --> %lld %ld\n",millis,then->tv_sec, then->tv_nsec);
}
#endif

#if SIT_AND_SPIN
static int sitAndSpin(CAtomicInt *sentinal, 
   int block, struct timespec *timeout, int *interrupt, pVM vm)
{
   int s = A_OK;
   vmMightStall(vm,1);		// GC me if it is time
      while(!CAI_VALUE(sentinal))
      {
	 if (interrupt && *interrupt) { s = A_INTERRUPTED; break; }
	 if (!block)	// timed out?
	 {
	    struct timespec now;
	    clock_gettime(CLOCK_REALTIME,&now);
//printf("SIT_AND_SPIN: %ld %ld : %ld %ld\n",now.tv_sec,now.tv_nsec,timeout->tv_sec,timeout->tv_nsec);
	    if (now.tv_sec > timeout->tv_sec ||
	        (now.tv_sec == timeout->tv_sec && 
		 now.tv_nsec >= timeout->tv_nsec))
	      { s = A_TIMED_OUT; break; }
	 }
	 usleep(250);
      }
   vmMightStall(vm,0);
   return s;
}
#endif


    /* asyncWaitForSignal(fcn) for C objects.
     * fcn is a C function that takes self and returns:
     *   A_FAIL:  Whatever it is that being waited for hasn't happened
     *   A_OK or >= A_USER:  It happened.  This should be postive (1
     *	    overlaps with A_OK)
     *	    Fcn stores result in payload
     *	 A_RETRY: Can't get a clear result, try again.
     *	 DON'T return anything else
     *   Must be async, fast, non blocking, no GC, no throw
     *   
     * Returns:
     *    A_OK
     *    A_TIMED_OUT
     *    A_INTERRUPTED (but the caller won't see this as the VM will handle it)
     *    n : Whatever it was that fcn returned.
     * Doesn't return A_RETRY
     * Can stall waiting for a signal, which means it can GC
     */
static int asyncWaitForSignal(
   struct WaitList *theOtherList,
   Instance *i,int (*fcn)(void *payload), void *payload,
   Instance *seconds, int canInterrupt, pVM vm)
{
   int *interrupt = canInterrupt ? vmInterruptEvent(vm) : 0;
   int  block, r, s;
   unsigned long   millis;
   WaitListNode    w;

   #if _MSC_VER
      HANDLE handle;
   #elif PT_SEMAPHORE || PT_SIG
      struct timespec timeout;	// 0==check
      sem_t	      sem;	// PT_SEMAPHORE
      //sigset_t      sigset;	// PT_SIG
      //siginfo_t     info;	// PT_SIG
   #elif SIT_AND_SPIN
      struct timespec timeout;	// 0==check
      CAtomicInt      sentinal;	// SIT_AND_SPIN
   #endif

//   do { r = fcn(payload); } while(r == A_RETRY);
   do	// this first call to fcn() gets the most retries
   {
      r = fcn(payload);
      if (r == A_RETRY) SPIN_STALL; else break;
   }while(1);
   if (r >= A_OK) return r;	// event has happened

   // Start racing: event hasn't happened but eventHappened() doesn't know
   // we are here

   block = !getMillis(seconds,&millis,vm);
//printf("timeout sec : %lu  %d\n",millis, block);

   #if PT_SEMAPHORE || SIT_AND_SPIN	// timeout is time in future
      if (!block) toInfinityAndBeyond(millis,&timeout);
   #elif PT_SIG	// timeout is time to wait
      if (!block)
      {
	 timeout.tv_sec = millis/1000; timeout.tv_nsec = (millis % 1000) * 10;
      }
   #endif
   if (millis == 0) 
      return A_TIMED_OUT;  // since we have already checked once

//sigtimedwait atomically unblocks sigset during wait, signals are queued
// sigemptyset(&mask); sigaddset(&mask,SIGUSR1);
// sigprocmask(SIG_BLOCK,&mask,*origMask)
// sigtimedwait(): unblocks SIGUSR1, waits, reblocks

//sigqueue() --> send signal w/ data to process

// signal queue is one deep (per signal #) unless RT signals are used

   #if _MSC_VER
      w.event = handle = CreateEvent(0,0,0,0);
      //!!!! handle might be zero, BAD.   GetLastError()
   #elif PT_SEMAPHORE
      sem_init((w.psem = &sem),0,0);
   #elif SIT_AND_SPIN
      CAI_INIT(&sentinal); w.sentinal = &sentinal;
   #elif PT_SIG
      w.tid = pthread_self();
   #endif
   w.i   = i;
   w.n	 = 0;
   w.all = 0;
   w.vm  = (vm && canInterrupt) ? vmRoot(vm) : (pVM)1;
   CAI_INIT(&w.sv); CAI_INIT(&w.check);
   appendToList(theOtherList,&w);

   // Now I'm offically waiting. But what if eventHappened() was called?
   do { r = fcn(payload); } while(r == A_RETRY);   // second most retries
   if (r >= A_OK)		// race happened
   {
//printf("RACERACERACERACERACERACERACERACERACERACERACERACERACERACE\n");
      removeFromList(&w);
      return r;
   }
   if (interrupt && *interrupt)	// ditto with vmInterrupt race
   {
      removeFromList(&w);
      vmProcessInterrupt(vm);	// longjmp()s
      return A_INTERRUPTED;
   }

#if PT_SIG
   // sigtimedwait()/sigwaitinfo() atomically unblocks sigset during wait
   // signals are queued (one deep for non RT signals)
   sigemptyset(&sigset); sigaddset(&sigset,mySignal);

   while(1)	// Lots of false positives
   {
      vmMightStall(vm,1);		// GC me if it is time
      	 // --> signal # that was sent or -1
	 // -1: errno == EAGAIN(11) == timeout
	 s = block ? sigwaitinfo(&sigset, &info) :
		     sigtimedwait(&sigset,&info,&timeout);
// SIGUSR1 == 10, USR2==12
//printf("s(%p) -->%d  %d\n",payload,s,errno);
      vmMightStall(vm,0);
//if (s < 0) { printf("-->errno==%d  %d\n",errno,EAGAIN); perror("errno"); }

      if (s == -1 && errno == EAGAIN)	// timeout
      {
      	 s =A_TIMED_OUT;
	 break;
      }

      // if spurious signal (s != theSig) either continue or check anyway
      // look at info for SI_KERNEL, ignore user sent signals?
if (s != -1 && s != mySignal){ printf("uhhhhhhhhhh\n"); continue; }

      // check for interrupt
      if (interrupt && *interrupt)
      {
	 s = A_INTERRUPTED;		// doesn't get here
	 break;
      }

      do { r = fcn(payload); } while(r == A_RETRY);	// rare retries
      if (r != A_FAIL)	// read/write happened or i is closed/broken
      {
	 // caller will call async?Happened() if appropriate
      	 s = r;		// event has happened
	 break;
      }

//!!!!need to adjust timeout

      /* False positives possible.  eg Pipe.write() likes to tell every
       * reader/writer that something has happened because readers want the
       * data and writers may be blocked trying to write.
       */
//printf("FALSE POSITIVE FALSE POSITIVE FALSE POSITIVE FALSE POSITIVE FALSE POSITIVE\n");
   } // no data and no timeout, try again
// PT_SIG
#elif PT_SEMAPHORE || SIT_AND_SPIN || _MSC_VER
   while(1)
   {
   #if PT_SEMAPHORE
      vmMightStall(vm,1);		// GC me if it is time
	 s = block ? sem_wait(&sem) : sem_timedwait(&sem,&timeout);
      vmMightStall(vm,0);

      if (s == -1)
      {
	 if (errno == ETIMEDOUT) { s = A_TIMED_OUT; break; }
	 // eg EINTR, interrupted by signal
printf("========1============>%d %d\n",errno,EINVAL); perror("asyncWaitForSignal");
	 continue;
      }
   #endif	// Pthread semaphore

   #if SIT_AND_SPIN
      s = sitAndSpin(&sentinal,block,&timeout,interrupt,vm);
      if (s != A_OK) break;
      CAI_INIT(&sentinal);
   #endif

   #if _MSC_VER
      vmMightStall(vm,1);		// GC me if it is time
	 s = block ? WaitForSingleObject(handle,INFINITE) :
		     WaitForSingleObject(handle,millis);
      vmMightStall(vm,0);

      if (s == WAIT_TIMEOUT) { s = A_TIMED_OUT; break; }
   #endif // Windows

      // check for interrupt. If interrupted, don't drop data
      if (interrupt && *interrupt)
      {
	 s = A_INTERRUPTED;
	 break;
      }

      do { r = fcn(payload); } while(r == A_RETRY);	// rare retries
      if (r != A_FAIL)	// read/write happened or i is closed/broken
      {
      	 s = r;		// event has happened
	 break;
      }
   #if _MSC_VER
      //!!!!adjust timeout
   #endif	// Windows
   } // while no data and no timeout, try again
#else
   Need something here
#endif	// Pthread semaphore and Windows

   removeFromList(&w);
   #if _MSC_VER
      CloseHandle(handle);
   #elif PT_SEMAPHORE
      sem_destroy(&sem);
   #endif

   if (s == A_INTERRUPTED) vmProcessInterrupt(vm);	// longjmp()s

   return s;
}

///////////////////////////////////////////////////////////////////////////

/* !!!!???  argh!  if lots of writes are happening, since events are not
 * queued, and unix is slooooooooooooow, a second write can nuke the first
 * one before I can check it. I think this is hard to do as I don't fire
 * the semaphore unless the values match and many matches are the same as
 * one match.
 * One problem might be a match & an interrupt at the same time.
 * Input:
 *   ab: 0 or AtomicBool: set, then wait: .setAndWaitFor()
 *       Always set, even if won't wait or event happened.
 * Returns: A_*
 */

static int asyncWaitForN(
   struct WaitList *theOtherList, Instance *ab,
   int cv, CAtomicInt *cai, int N,
   Instance *i, int useSec, Instance *seconds, unsigned long millis, pVM vm)
{
   int	       *interrupt = vmInterruptEvent(vm);
   int		block, s;
   WaitListNode w;

   #if _MSC_VER
      HANDLE handle;
   #elif PT_SEMAPHORE || PT_SIG
      struct timespec timeout;	// 0==check
      sem_t       sem;		// PT_SEMAPHORE
      //int       r,sv;
      //sigset_t  sigset;	// PT_SIG
      //siginfo_t info;		// PT_SIG
   #elif  SIT_AND_SPIN
      struct timespec timeout;	// 0==check
      CAtomicInt  sentinal;	// SIT_AND_SPIN
   #endif

   if (cv == N) 	// quick check
   {
      if (ab) abSet(ab,1);	// hmmm, set after event happened
      return A_OK;
   }

   // start radio silence

   block = useSec ? !getMillis(seconds,&millis,vm) : 0;
   #if PT_SIG	// timeout is time to wait
      if (!block)
      {
	 timeout.tv_sec  = millis/1000;
	 timeout.tv_nsec = (millis % 1000) * 10;
      }
   #elif PT_SEMAPHORE || SIT_AND_SPIN	// timeout is time in future
      if (!block) toInfinityAndBeyond(millis,&timeout);
   #endif
   if (!block && millis == 0)
   {
      if (ab) abSet(ab,1);  // hmmm, set but we won't wait
      return A_TIMED_OUT;  // since we have already checked once
   }

   #if _MSC_VER
      w.event = handle = CreateEvent(0,0,0,0);
      //!!!! handle might be zero, BAD.   GetLastError()
   #elif PT_SEMAPHORE
      sem_init((w.psem = &sem),0,0);
   #elif SIT_AND_SPIN
      CAI_INIT(&sentinal); w.sentinal = &sentinal;
   #elif PT_SIG
      w.tid = pthread_self();
   #endif
   w.i   = i;
   w.n	 = N;
   w.all = 0;
   w.vm	 = vm ? vmRoot(vm) : (pVM)1;
   CAI_INIT(&w.sv); CAI_INIT(&w.check);
   appendToList(theOtherList,&w);

   if (ab) abSet(ab,1);		// setAndWaitFor(): now we wait

   // end radio silence
   if (cai && CAI_VALUE(cai) == N)
   {
      removeFromList(&w);
      return A_OK;	// event has happened
   }
   if (interrupt && *interrupt)
   {
      removeFromList(&w);
      vmProcessInterrupt(vm);	// longjmp()s
      return A_INTERRUPTED;
   }

#if PT_SIG
   sigemptyset(&sigset); sigaddset(&sigset,mySignal);
   while(1)	
   {
      vmMightStall(vm,1);		// GC me if it is time
	 s = block ? sigwaitinfo( &sigset,&info) :
		     sigtimedwait(&sigset,&info,&timeout);
      vmMightStall(vm,0);

      if (s == -1 && errno == EAGAIN)	// timeout
	 { s = A_TIMED_OUT; break; }

      // if spurious signal (s != theSig) either continue or check anyway
      // look at info for SI_KERNEL, ignore user sent signals?
if (s != -1 && s != mySignal) {printf("uhhhhhhhhhh\n"); continue; }

      // check for interrupt
      if (interrupt && *interrupt) { s = A_INTERRUPTED; break; }

      #if defined(__FreeBSD__) || defined(__minix__) // if FreeBSD suported pthread_sigqueue()
         sv = info.si_value.sival_int;
      #else	// Linux
	 sv = info.si_int;	// --> info._sifields._rt.si_sigval.sival_int;
      #endif

      if (sv == N) { s = A_OK; break; }	// event has happened
//!!!!need to adjust timeout
   } // no data and no timeout, try again
// signals
#elif PT_SEMAPHORE || SIT_AND_SPIN || _MSC_VER
   while(1)
   {
   #if PT_SEMAPHORE
      vmMightStall(vm,1);		// GC me if it is time
	 s = block ? sem_wait(&sem) : sem_timedwait(&sem,&timeout);
      vmMightStall(vm,0);

      if (s == -1)
      {
	 if (errno == ETIMEDOUT) { s = A_TIMED_OUT; break; }
printf("====================>%d %d\n",errno,EINVAL); perror("asyncWaitForN");
	 // perf tools hit this a lot (eg gprof or google-profiler) with EINTR
	 continue;
      }
   #endif  // Pthread semaphore

   #if SIT_AND_SPIN
      s = sitAndSpin(&sentinal,block,&timeout,interrupt,vm);
      if (s != A_OK) break;
      CAI_INIT(&sentinal);
      break;
   #endif

   #if _MSC_VER
      vmMightStall(vm,1);		// GC me if it is time
	 s = block ? WaitForSingleObject(handle,INFINITE) :
		     WaitForSingleObject(handle,millis);
      vmMightStall(vm,0);

      if (s == WAIT_TIMEOUT) { s = A_TIMED_OUT; break; }
   #endif	// Windows

      // check for interrupt
      if (interrupt && *interrupt) { s = A_INTERRUPTED; break; }

      if (w.sv == N) { s = A_OK; break; }

   #if _MSC_VER
//!!!!adjust timeout
   #endif	// Windows
   } // while no data and no timeout, try again
#else
   Need something here
#endif	// Pthreads and Windows

   removeFromList(&w);
   #if _MSC_VER
      CloseHandle(handle);
   #elif PT_SEMAPHORE
      sem_destroy(&sem);
   #endif

   if (s == A_INTERRUPTED) vmProcessInterrupt(vm);	// longjmp()s

   return s;
}

///////////////////////////////////////////////////////////////////////////

    // List is (timeout,i,...) or (AtomicBool,timeout,i...)
    // wait for write to any in list
    // () --> sleep, (n) --> sleep(n)
    // setAndWait(ab) --> set ab after start waiting so won't miss event
    // Interrupts are processed
    // If waiting on nothing, fake up a obj so can interrupt
    // !!CALLER!! DO NOT CALL with list longer than 10!
Instance *asyncWaitList(Instance *list, Instance *ab, pVM vm)
{
   int	        *interrupt = vmInterruptEvent(vm);
   int           block, n,s,len;
   Instance	*i, *r;
   unsigned long millis;
   WaitListNode  ws[10], *wp;

   #if _MSC_VER
      HANDLE handle;
   #elif PT_SEMAPHORE || PT_SIG
      struct timespec timeout;
      sem_t	      sem;	// PT_SEMAPHORE
   #elif SIT_AND_SPIN
      struct timespec timeout;
      CAtomicInt      sentinal;	// SIT_AND_SPIN
   #endif

   n = ab ? 1 : 0;
   block = !getMillis(listGet(list,n),&millis,vm);
   #if PT_SEMAPHORE || SIT_AND_SPIN	// timeout is time in future
      if (!block) toInfinityAndBeyond(millis,&timeout);
   #elif PT_SIG	// timeout is time to wait
      if (!block)
      {
	 timeout.tv_sec = millis/1000;
	 timeout.tv_nsec = (millis % 1000) * 10;
      }
   #endif

   #if _MSC_VER
      handle = CreateEvent(0,0,0,0);
      //!!!! handle might be zero, BAD.   GetLastError()
   #elif PT_SEMAPHORE
      sem_init(&sem,0,0);
   #elif SIT_AND_SPIN
      CAI_INIT(&sentinal);
   #endif

   n = (ab == 0) ? 1 : 2;	// skip timeout or timeout & setter
   for (wp=ws, len=0; (i = listGet(list,n)); n++,wp++,len++)
   {
   set:
      wp->i   = i;
      wp->n   = 0;
      wp->all = 1;
      wp->vm  = vm ? vmRoot(vm) : (pVM)1;
      #if _MSC_VER
	 wp->event = handle;
      #elif PT_SEMAPHORE
	 wp->psem = &sem;
      #elif SIT_AND_SPIN
         wp->sentinal = &sentinal;
      #endif
      CAI_INIT(&wp->sv); CAI_INIT(&wp->check);
      appendToList(&waitingForWriteList,wp);
   }
   if (!len)	// fake something so I'll catch interrupts
   {
      i = Zero;
      goto set;
   }

   if (ab) abSet(ab,1);		// setAndWait()

#if PT_SEMAPHORE
   while(1)
   {
      vmMightStall(vm,1);
	 s = block ? sem_wait(&sem) : sem_timedwait(&sem,&timeout);
      vmMightStall(vm,0);

      if (s == -1)
      {
	 if (errno == ETIMEDOUT) { s = A_TIMED_OUT; break; }
printf("====================>%d %d\n",errno,EINVAL); perror("asyncWaitList");
	 continue;
      }

      // check for interrupt
      if (interrupt && *interrupt) { s = A_INTERRUPTED; break; }

      s = A_OK;
      break;
   } // while: OS error
// Pthread semaphore

#elif SIT_AND_SPIN
   s = sitAndSpin(&sentinal,block,&timeout,interrupt,vm);

#elif _MSC_VER
   vmMightStall(vm,1);		// GC me if it is time
      s = block ? WaitForSingleObject(handle,INFINITE) :
		  WaitForSingleObject(handle,millis);
   vmMightStall(vm,0);

   if (s == WAIT_TIMEOUT)	     s = A_TIMED_OUT;
   else if (interrupt && *interrupt) s = A_INTERRUPTED;
   else if (s == WAIT_OBJECT_0)      s = A_OK;
   else				     s = A_FAIL;
// Windows
#else
   Need something here
#endif

   for (n = 0; n < len; n++) removeFromList(&ws[n]);
   #if _MSC_VER
      CloseHandle(handle);
   #elif PT_SEMAPHORE
      sem_destroy(&sem);
   #endif

   r = Void;
   switch(s)
   {
//      case A_TIMED_OUT: case A_FAIL:   return Void;
      case A_INTERRUPTED:
         vmProcessInterrupt(vm);	// longjmp()s
	 break;
      case A_OK:
      {
	 int found = 0;
	 r = tupleCreate(len,I_OWNED,vm);
	 for (n = 0; n < len; n++)
	    if (ws[n].check) { tupleAppend(r,ws[n].i); found = 1; }

	 if (!found)
	 {
printf("EHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH?\n");
	    r = Void;
	 }
	 break;
      }
   } // switch

   return r;
}

//////////////////////////////////////////////////////////////////////////

static void _eventHappened(struct WaitList *list,
   int iIsVM, Instance *i, int checkN, int n)
{
#if PT_SIG
   int		  s, s2;
   union sigval   val;
   WaitListNode  *dst;
int found=0;

   val.sival_int = n;

   SPIN_LOCK_ACQUIRE(&waitLock);	// critical section
   {
      for (dst = FIRST(list); dst != list; dst = dst->next)
      {
	 s  = (checkN && !dst->all) ? (n == dst->n) : 1;
	 s2 = iIsVM ? (i == (Instance *)dst->vm) : (i == dst->i);
//if (iIsVM) printf("-->%d %d %p %p\n",s,s2,i,dst->vm);
	 if (s && s2)
	 {
//if (checkN)printf("POOOH %p %p %d  %d   %d %d\n",dst->i,dst->vm,n,dst->n, s,s2);
found=1;
	    s = pthread_sigqueue(dst->tid,mySignal,val); // NOT in FreeBSD
//if (s) printf("sigqueue --> %d %d\n",s,errno);
	 }
       }
   }
   SPIN_LOCK_RELEASE(&waitLock);
   // quite common not to find a target, especially with writes
//if (!found) printf("not found: %d %d\n",checkN,n);
#endif	// Pthread signals

#if _MSC_VER || PT_SEMAPHORE || SIT_AND_SPIN
   int		  s, s2;
   WaitListNode  *dst;

   SPIN_LOCK_ACQUIRE(&waitLock);	// critical section
   {
      for (dst = FIRST(list); dst != list; dst = dst->next)
      {
	 s  = (checkN && !dst->all) ? (n == dst->n) : 1;  // asyncWaitForN()
	 s2 = iIsVM ? (i == (Instance *)dst->vm) : (i == dst->i);
	 if (s && s2)
	 {
	    CAI_SET(&dst->sv,n); CAI_INC(&dst->check);
	    #if PT_SEMAPHORE
s =
	       sem_post(dst->psem);
if (s==-1) printf("OPPSOPPSOPPS %d\n",errno); 
	    #elif SIT_AND_SPIN
	       CAI_ONE(dst->sentinal);
	    #else
	       s = SetEvent(dst->event); // Windows
	    #endif
	 }
       }
   }
   SPIN_LOCK_RELEASE(&waitLock);
   // quite common not to find a target, especially with writes
#endif	// Windows & POSIX semaphores/condition variables
}

///////////////////////////////////////////////////////////////////////////

// These return A_*

    // Read something, if empty, wait for a write to happen
    // Stalls so GC can happen
    // Can be interrupted
    // Blocks or times out, you choose
int asyncRead(Instance *i,int (*fcn)(void *payload), void *payload, 
   Instance *seconds, pVM vm)
{ return asyncWaitForSignal(&waitingForWriteList,i,fcn,payload,seconds,1,vm); }

    // Ditto, but blocks and isn't interruptable
int asyncBlock(Instance *i,int (*fcn)(void *payload), void *payload, pVM vm)
{ return asyncWaitForSignal(&waitingForWriteList,i,fcn,payload,Void,0,vm); }

int asyncWantN(Instance *i, Instance *ab,
	       int cv, CAtomicInt *cai,int N,Instance *seconds, pVM vm)
{ return asyncWaitForN(&waitingForWriteList,ab,cv,cai,N,i,1,seconds,0,vm); }

    // wake waiting writers
void asyncReadHappened(Instance *i)
   { _eventHappened(&waitingForReadList,0,i,0,0); }

    // write something; if full, wait for a read to happen
int asyncWrite(Instance *i,int (*fcn)(void *payload), void *payload, 
   Instance *seconds, pVM vm)
{ return asyncWaitForSignal(&waitingForReadList,i,fcn,payload,seconds,1,vm); }

    // Wake waiting readers
void asyncWriteHappened(Instance *i)
   { _eventHappened(&waitingForWriteList,0,i,0,0); }

    // If waiting for n, kick
void asyncWroteN(Instance *i,int n)
   { _eventHappened(&waitingForWriteList,0,i,1,n); }


    // Wake readers, maybe writers too
    // flags is bits: 
    //    1 : all lists, ie both readers and writers
    //    2 : i is a VM, not an Instance
    //    4 : check n
void asyncEventHappened(Instance *i, int flags, int n)
{
   int allLists = (flags & 1), iIsVM = (flags & 2), checkN = (flags & 4);
   _eventHappened(&waitingForWriteList,iIsVM,i,checkN,n);
   if (allLists)	// everybody everywhere
      _eventHappened(&waitingForReadList,iIsVM,i,checkN,n);
}

    // i has been closed, inform everybody interested in i
void asyncClosed(Instance *i) { asyncEventHappened(i,0x1,0); }

    // Interruptable sleep(seconds)
int asyncSleep(Instance *seconds, pVM vm)
   { return asyncWantN(Zero,0,0,0,1,seconds,vm); }


    // If cai will change, hammer and brag about it
void caiSet(CAtomicInt *cai,int n)
{
   if (n != CAI_SET(cai,n))  // set cai and look at what it was
      asyncWroteN((Instance *)cai,n);	// it changed
}

    // -->A_*
int caiWaitFor(CAtomicInt *cai,int want,Instance *timeout,pVM vm)
{
   return 
      asyncWaitForN(&waitingForWriteList,0, CAI_VALUE(cai),cai,want,
		  (Instance *)cai,1,timeout,0,vm);
}

    // --> 0 (timeout), 1 (success)
int caiWait4(CAtomicInt *cai,int want,int block,unsigned long millis)
{
   int       r, useSec = 0;
   Instance *seconds = 0;

   if (block) { useSec = 1; seconds = BoolTrue; }
   r = asyncWaitForN(&waitingForWriteList,0,
		  CAI_VALUE(cai),cai,want,
		  (Instance *)cai,useSec,seconds,millis,NoVM);
   return (r == A_OK);
}

// siginfo->si_signo == signal #, si_code == SI_KERNEL/SI_USER
//sig_atomic_t == atomic int for signal handlers (only?)

void asyncConstruct(void)
{
#if PT_SIG
   int      s;
   sigset_t sigset;

   mySignal = SIGUSR1;
//   mySignal = SIGRTMIN + 5;

   // block some signals for main thread and child threads
   sigemptyset(&sigset); sigaddset(&sigset,mySignal); sigaddset(&sigset,SIGUSR2);
//   sigprocmask(SIG_BLOCK,&sigset,0);	// action is unspecified in multithreaded app
   s = pthread_sigmask(SIG_BLOCK,&sigset,0);
//printf("pthread_sigmask() --> %d %d   %d %d %d \n",s,errno,SIGRTMIN,mySignal,SIGRTMAX);
#endif
}
