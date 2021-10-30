/* pipe.c : Pipe and Straw
 * 
 * Copyright (c) 2006,2007,2008-12,2013 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#include <string.h>
#if 0
#ifdef _MSC_VER
   #include <process.h>		// system()
   #include <direct.h>		// _getcwd()
   #include <conio.h>
   #define getch	_getch
   #define getpid	_getpid
   #define tzset	_tzset
#endif
#endif
#define __NOT_A_DLL

#include "zklObject.h"
#include "zklClass.h"
#include "zklList.h"
#include "zklMethod.h"
#include "zklUtil.h"

/* ******************************************************************** */
/* *************************** Thread.Pipe **************************** */
/* ******************************************************************** */

#define E_PIPE_ERROR		"PipeError"

typedef struct
{
   Instance    instance;	// iflag == 1 --> Straw, 0 --> Pipe
   CAtomicInt  closed;		// 1 if closed, 2 if closed and broken
   Instance   *whyBroken;	// Void or Exception, Atomic
   TSQueue     queue;
} Pipe;		// 4044 bytes/32, 8256/64

   /* El-sleezeo note:  A Straw is a subset of Pipe but is also a BInstance,
    * which works because a BInstance is smaller than a Instance (and also a
    * subset of)
    */
typedef struct	// one value Pipe, very thin Pipe
{
   Instance    instance;  // Really BInstance. iflag: 1 --> Straw, 0 --> Pipe
   CAtomicInt  closed;
   Instance   *whyBroken;	// Void or Exception, Atomic
   Instance   *value;		// changes atomically, 0 if empty
} Straw;	// 24 bytes/32, 20 if were BInstance, 40/64

#define PIPE(i)		( (Pipe *)(i) )
#define QUEUE(i)	( PIPE(i)->queue )
#define IS_PIPE(i)	( !PIPE(i)->instance.iflag )

#define STRAW(i)	( (Straw *)(i) )
#define IS_STRAW(i)	( STRAW(i)->instance.iflag )

static ZKL_Object     PipeObject, StrawObject;
static IBucketHeader *strawBuckets;

int pipeID, strawID;

Instance *strawCreate(pVM vm)
{
   Straw *s = (Straw *)ibucketAllocate(strawBuckets,&StrawObject,I_OWNED,1,vm);
   s->instance.iflag = 1;
   CAI_INIT(&s->closed);
   CAP_SET(&s->whyBroken,Void);
   CAP_SET(&s->value,0);
   return containerIsCooked(strawBuckets,(Instance *)s,I_OWNED);
}

    // Straw.create() --> Straw
static Instance *Straw_create(Instance *self,pArglist arglist,pVM vm)
   { return strawCreate(vm); }

Instance *pipeCreate(pVM vm)
{
   Pipe *pipe = (Pipe *)instanceAllocate(sizeof(Pipe),&PipeObject,1,vm);
   CAI_INIT(&pipe->closed);
   CAP_SET(&pipe->whyBroken,Void);
   tsQueueInit(&pipe->queue);
   addToCollectables((Instance *)pipe,I_OWNED,vm);
   return (Instance *)pipe;
}

static void pipeMarker(Instance *self)
{
   if (IS_STRAW(self)) instanceMark(STRAW(self)->value);
   else		       tsQueueMark(&PIPE(self)->queue);
   if (PIPE(self)->whyBroken != Void) instanceMark(PIPE(self)->whyBroken);
}

    // Pipe.create() -->Pipe
static Instance *Pipe_create(Instance *self,pArglist arglist,pVM vm)
{
   if (IS_STRAW(self)) return Straw_create(self,arglist,vm);
   return pipeCreate(vm);
}

    // Pipe.open() : Reopen a closed Pipe --> self
static Instance *Pipe_open(Pipe *self,pArglist arglist,pVM vm)
{
   if (CAI_VALUE(&PIPE(self)->closed))
   {
      CAP_SET(&self->whyBroken,Void);
      CAI_ZERO(&self->closed);		// do this last for thread safety
      // in case somebody is waiting for it to reopen:
      asyncWriteHappened((Instance *)self);
   }
   return (Instance *)self;
}

    // Pipe.clear() : close and nuke data --> self
static Instance *Pipe_clear(Pipe *self,pArglist arglist,pVM vm)
{
#if 1
   int flags = 0;
   CAI_ONE(&self->closed);	// don't no one write
   if (IS_PIPE(self)) { tsQueueClear(&self->queue); flags = 0x1; }
   else		      CAP_SET(&((Straw *)self)->value,0);
   asyncEventHappened((Instance *)self,flags,0);
#else
   CAI_SET(&self->closed,1);
   if (IS_PIPE(self)) tsQueueClear(&self->queue);
   else		      CAP_SET(&((Straw *)self)->value,0);
#endif
   return (Instance *)self;
}

#define PIPE_HAS_DATA	 A_USER		// 10
#define PIPE_CLOSED	(A_USER+1)

typedef struct { Pipe  *pipe; TSQPayload *payload;  } PipePayload;
typedef struct { Straw *s;    Instance   *newValue; } StrawPayload;

    // Pipe.read(timeout=Void) --> value | Exception
    // Lots can (and does) happen between the two tests
static int _dataOrClosed(void *ppp)	// read from Pipe
{
   int s;
   PipePayload *pp = (PipePayload *)ppp;
	// Fight with write & other readers for lock
   s = tsQueueRead(pp->payload);
   if (s == 0) return PIPE_HAS_DATA;
   // Can't read doesn't mean empty
   if (CAI_VALUE(&pp->pipe->closed) && s == 1) return PIPE_CLOSED;
   if (s == 2) return A_RETRY;
   return A_FAIL;
}
static int _oneLumpOr2(void *psp)	// read from Straw
{
   static SpinLock lock = 0;

   StrawPayload *sp = (StrawPayload *)psp;
   Straw        *s  = sp->s;

   // if value is nonzero, it is data. Set it zero (ready for write)
   // Value can only change if it is zero. If not zero, it is frozen until
   //   *I* set it to zero.
   // Race: 2+ threads read non zero value at same time, happens
again:
   SPIN_LOCK_ACQUIRE(&lock);
      if ( (sp->newValue = s->value) )
      {
	 CAP_SET(&s->value,0);
	 SPIN_LOCK_RELEASE(&lock);
	 return PIPE_HAS_DATA;
      }
   SPIN_LOCK_RELEASE(&lock);
   if (CAI_VALUE(&s->closed))  // this is a race
   {
      // a write can happen between lock and closed check
      if (s->value) goto again;	   // don't miss a signal
      return PIPE_CLOSED;
   }
   return A_FAIL;
}
static Instance *pipeRead(Pipe *self,int throw,pArglist arglist,pVM vm)
{
   int		s=0;
   Instance    *i;
   Instance    *timeout = arglistTryToGet(arglist,0);
   TSQPayload   payload;
   if (IS_PIPE(self))	// Pipe
   {
      PipePayload pp = { self, &payload };
      payload.tsq = &self->queue; payload.i = 0;

	   // Pipe can close while I'm trying to read an empty Pipe
      	// Successful read: there may be readers wanting to read or 
	// or writers wanting to write but were blocked by me so notify both
      s = asyncRead((Instance *)self,_dataOrClosed,&pp,timeout,vm);
      if (s == PIPE_HAS_DATA) asyncEventHappened((Instance *)self,0x1,0);
      i = payload.i;
   }
   else			// Straw
   {
      StrawPayload pp = { STRAW(self), 0 };
      s = asyncRead((Instance *)self,_oneLumpOr2,&pp,timeout,vm);
      if (s == PIPE_HAS_DATA) asyncReadHappened((Instance *)self);
      i = pp.newValue;	// might be garbage
instanceIsOrphan(i);		// DIP, crossing thread boundry
   }
   switch(s)
   {
      // case PIPE_HAS_DATA: just return i
      case A_INTERRUPTED:	// should not see this, asyncRead() longjmps
	 return Void;   // user code won't see this; exception is thrown by VM
      case A_TIMED_OUT:
	 vmThrow(vm,E_PIPE_ERROR,"Timed out waiting for data");
      case PIPE_CLOSED:	    // closed, no more writes but might have data
	 if (CAI_VALUE(&self->closed) == 2)	// broken
	 {
	    if (self->whyBroken != Void) vmThrowE(vm,self->whyBroken);
	    vmThrow(vm,E_PIPE_ERROR,"The pipe is broken, can't read from it.");
	 }

	 /* There is a race condition here:  In _dataOrClosed(), between the
	  * two checks, the producer could have writen and closed the pipe,
	  * which, if I'm not careful, I could see as an empty closed pipe.
	  */
//!!!! this still could be wrong if contested by another reader
	 if (IS_PIPE(self) && 0==tsQueueRead(&payload)) i = payload.i;
	 else
	 {
	    if (throw) vmThrowTheEnd(vm);
	    return 0;
	 }
   } // switch

//attach to callers VM
//   instanceIsOrphan(i);		// DIP, crossing thread boundry
   return i;
}
static Instance *Pipe_read(Instance *self,pArglist arglist,pVM vm)
   { return pipeRead((Pipe *)self,1,arglist,vm); }

    // Pipe|Straw.reduce, .pump, .filter
static Instance *_pipeGet1(Instance *self,size_t n,void *X,size_t _,pVM vm)
   { return pipeRead((Pipe *)self,0,NoArglist,vm); }

static Instance *Pipe_filter(Instance *self,pArglist arglist,pVM vm)
   { return zfilter(self,0x0,_pipeGet1,0,arglist,0,vm); }
static Instance *Pipe_pump(Instance *self,pArglist arglist,pVM vm)
   { return pump(self,PMP_OK2CNT,_pipeGet1,0,arglist,0,vm); }
static Instance *Pipe_reduce(Instance *self,pArglist arglist,pVM vm)
   { return zreduce(self,_pipeGet1,0,arglist,0,vm); }


Instance *strawSet(Instance *pself, Instance *i, int close)
{
   Straw *self = (Straw *)pself;
   CAP_SET(&self->value,i);
   if (close) CAI_ONE(&self->closed);
   instanceIsOrphan(i);		// DIP1
   asyncWriteHappened(pself);
   return pself;
}

    // Pipe.write(x[,timeout]) --> self | Void (on Exception)
//!!! probably should have been Pipe.writeT(i,timeout) & .write(a,b,c...)
static int _writeOrBlow(void *ppp)	// write to Pipe
{
   int		s;
   PipePayload *pp = (PipePayload *)ppp;
	// Write if can, otherwise check for closed and retry
   if (CAI_VALUE(&pp->pipe->closed)) return PIPE_CLOSED;
   s = tsQueueWrite(pp->payload);
   if (s == 0) return PIPE_HAS_DATA;
   // If the pipe is full, it could be a reader & writer waiting on each
   // other. Or not.
   // If there is congestion, retry, otherwise the signal is effectively missed.
   if (s == 2) return A_RETRY;
   return A_FAIL;
}
static int _tempestInaTeaCup(void *psp)		// write to Straw
{
   StrawPayload *sp = (StrawPayload *)psp;
   Straw        *s  = sp->s;

   if (CAI_VALUE(&s->closed))		  return PIPE_CLOSED;
      // if value is zero, I can write. newValue != 0
   if (!CAP_XP(&s->value,sp->newValue,0)) return PIPE_HAS_DATA;
   return A_FAIL;
}
static void pipeWrite(Instance *pself,Instance *i,Instance *timeout,pVM vm)
{
   int	  s;
   Pipe  *self = (Pipe *)pself;

   #if 0
   if(!I_IS_THREAD_SAFE(i))      // only thread safe objects
   {
      char buf[100];
      sprintf(buf,"Only thread safe objects written to Pipes: %s",iname(i));
      vmThrow(vm,E_TYPE_ERROR,buf);
   }
   #endif

   if (IS_PIPE(pself))	// Pipe
   {
      TSQPayload   payload = { &self->queue,i };
      PipePayload  pp      = { self, &payload };

      s = asyncWrite(pself,_writeOrBlow,&pp,timeout,vm);
      	// Successful write: there may be readers wanting to read or 
	// or writers wanting to write but were blocked by me so notify both
      if (s == PIPE_HAS_DATA) asyncEventHappened(pself,0x1,0);
   }
   else		// Straw
   {
      StrawPayload pp = { STRAW(self), i };
      s = asyncWrite(pself,_tempestInaTeaCup,&pp,timeout,vm);
      if (s == PIPE_HAS_DATA) asyncWriteHappened(pself);
   }
   switch(s)
   {
      case A_INTERRUPTED: return;	// let interrupt propagate
      case A_TIMED_OUT:   vmThrow(vm,E_PIPE_ERROR,"Pipe full");
      case PIPE_CLOSED:
	 if (CAI_VALUE(&self->closed) == 2)
	 {
	    if (self->whyBroken != Void) vmThrowE(vm,self->whyBroken);
	    vmThrow(vm,E_PIPE_ERROR,"The pipe is broken, can't write to it.");
	 }
	 vmThrowTheEnd(vm);
	 break;
   }
}
Instance *Pipe_write(Instance *self,pArglist arglist,pVM vm)
{
   Instance *i       = arglistGet(arglist,0,"Pipe.write",vm);
   Instance *timeout = arglistTryToGet(arglist,1);

#if 0
if (I_IS_CONTAINER(i) && !I_IS_THREAD_SAFE(i))
{
   char buf[200];
   sprintf(buf,"Pipe.write: container %s not thread safe",iname(i));
   vmThrow(vm,E_TYPE_ERROR,buf);
}
#endif
   pipeWrite(self,i,timeout,vm);
   instanceIsOrphan(i);		// DIP1, for real
   return self;
}

#if 0
    // Pipe.write(x[,timeout])
    // Write only thread safe objects to pipe
static Instance *Pipe_write(Pipe *self,pArglist arglist,pVM vm)
{
   Instance *i = arglistGet(arglist,0,0,vm);
   if (!I_IS_THREAD_SAFE(i))
   {
      char buf[200];
      sprintf(buf,"Pipe.write: Object not thread safe (%s)",iname(i));
      vmThrow(vm,E_PIPE_ERROR,buf);
   }
   return Pipe_fwrite(self,arglist,vm);
}
#endif

    // Pipe.flush() --> self
static Instance *Pipe_flush(Pipe *self,pArglist arglist, pVM vm)
{
   if (IS_PIPE(self) && tsQueueFlush(&QUEUE(self)))
      asyncClosed((Instance *)self);
   return (Instance *)self;
}

    // Pipe.close() --> self, Straw.close() --> self
    // Don't close a broken pipe
Instance *Pipe_close(Instance *pi,pArglist arglist, pVM vm)
{
   Pipe *self = (Pipe *)pi;
   if (!CAI_VALUE(&self->closed))	// open
   {
      CAI_ONE(&self->closed);
      asyncClosed(pi);
   }
   return pi;
}

extern Instance *ExceptionClass;	// in vm.c

    /* Pipe.breakIt(exeception=Void) --> self
     * It would be nice to lock this but I want to be able to break a
     *   blocked pipe.
     * Not thread safe.
     * Can break a closed pipe but not an already broken one.
     */
    // does NOT throw
void pipeBreakIt(Instance *pself,Instance *pe)
{
   Pipe *self = (Pipe *)pself;
   if (CAI_VALUE(&self->closed) != 2)	// racey
   {
      CAP_SET(&self->whyBroken,pe);
      	// now that we have said WHY it is broken, break it
      CAI_SET(&self->closed,2);		// closed and broken
      asyncClosed(pself);
   }
}
static Instance *Pipe_breakIt(Instance *self,pArglist arglist, pVM vm)
{
   Instance *pe = arglistTryToGet(arglist,0);
   if (!pe) pe = Void;

   if (pe == Void || 
       classIsChildOf(pe,ExceptionClass)) // fails if ExceptionClass is 0
      pipeBreakIt(self,pe);
   else vmThrow(vm,E_PIPE_ERROR,"Pipe.breakIt: Not an Exception");
   return self;
}

    // Pipe.walker() --> walker(Pipe.read)
static Instance *Pipe_Walker(Instance *self,pArglist a, pVM vm)
   { return walkerCreate(self,_pipeGet1,0,0,vm); }

    // Pipe.toBool() --> Bool
static Instance *Pipe_toBool(Pipe *self,pArglist arglist,pVM vm)
{
#if 0
   if (IS_PIPE(self)) return boolCreate(0 != tsQueueHasData(&self->queue));
   return (STRAW(self)->value == 0) ? BoolFalse : BoolTrue;
#else
   int r = (IS_PIPE(self)) ? tsQueueHasData(&self->queue) :
			     STRAW(self)->value != 0;
   return r ? BoolTrue : BoolFalse;
#endif
}

    // Pipe.len() --> int
static Instance *Pipe_len(Pipe *self,pArglist arglist,pVM vm)
{
   if (IS_PIPE(self)) return intCreate(tsQueueHasData(&self->queue),vm);
   return (STRAW(self)->value == 0) ? Zero : One;
}

    // Pipe.wait(timeout=block, waitUntilCloased=False, throw=False)
    //  --> False|True|1
    // No data is moved
typedef struct { Pipe *pipe; int waitUntilCloased; } PQ;
static int _dataOrClosed2(void *pq)
{
   Pipe *self = ((PQ*)pq)->pipe;
   if (CAI_VALUE(&(self->closed))) return PIPE_CLOSED;

   if (!((PQ*)pq)->waitUntilCloased)
   {
      if (IS_PIPE(self))
         { if (tsQueueHasData(&self->queue)) return PIPE_HAS_DATA; }
      else if (STRAW(self)->value)	     return PIPE_HAS_DATA;
   }
   return A_FAIL;
}
static Instance *Pipe_wait(Pipe *self,pArglist arglist,pVM vm)
{
   Instance *timeout = arglistTryToGet(arglist,0);
   int	     waitUntilCloased = arglistTryToGetBool(arglist,1,0,"Pipe.wait",vm);
   PQ	     pq = { self,waitUntilCloased };

   switch(asyncRead((Instance *)self,_dataOrClosed2,&pq,timeout,vm))
   {
      case PIPE_HAS_DATA: return One;		// Data available
      case PIPE_CLOSED:   return BoolTrue;	// Pipe closed
      case A_TIMED_OUT: 
	 if (arglistTryToGet(arglist,2) == BoolTrue) vmThrow(vm,E_TIME_OUT,0);
	 break;
   }

   return BoolFalse;				// Timeout, no data
}

static const MethodTable pipeMethods[] =
{
   "toBool",		(pMethod)Pipe_toBool,
   "create",		(pMethod)Pipe_create,
   "read",		(pMethod)Pipe_read,
   "readln",		(pMethod)Pipe_read,
   "write",		(pMethod)Pipe_write,
   "writeln",		(pMethod)Pipe_write,
   "close",		(pMethod)Pipe_close,
   "breakIt",		(pMethod)Pipe_breakIt,
   "flush",		(pMethod)Pipe_flush,
   "walker",		(pMethod)Pipe_Walker,
   "len",		(pMethod)Pipe_len,
   "open",		(pMethod)Pipe_open,
   "clear",		(pMethod)Pipe_clear,
   "wait",		(pMethod)Pipe_wait,
   "reduce",		(pMethod)Pipe_reduce,
   "pump",		(pMethod)Pipe_pump,
   "filter",		(pMethod)Pipe_filter,
   0,			0
};


	//////////////////////// Pipe Properties /////////////////////
    // Pipe.whyBroken --> Void | Exception
static Instance *Pipe_whyBroken(Pipe *self,pVM vm) { return self->whyBroken; }

    // Pipe.isEmpty --> Bool
static Instance *Pipe_isEmpty(Pipe *self,pVM vm)
//   { return boolCreate(!tsQueueHasData(&self->queue)); }
{
   if (IS_PIPE(self)) return boolCreate(!tsQueueHasData(&self->queue));
   return (STRAW(self)->value == 0) ? BoolTrue : BoolFalse;
}

    // Pipe.hasData --> Bool
static Instance *Pipe_hasData(Pipe *self,pVM vm)
   { return boolNot(Pipe_isEmpty(self,vm)); }

    // Pipe.isOpen --> Bool
static Instance *Pipe_isOpen(Pipe *self,pVM vm)
   { return CAI_VALUE(&self->closed) ? BoolFalse : BoolTrue; }

    // Pipe.isClosed --> Bool
static Instance *Pipe_isClosed(Pipe *self,pVM vm)
   { return CAI_VALUE(&self->closed) ? BoolTrue : BoolFalse; }

    // Pipe.isBroken --> Bool
static Instance *Pipe_isBroken(Pipe *self,pVM vm)
   { return (CAI_VALUE(&self->closed) == 2) ? BoolTrue : BoolFalse; }

static const PropertyTable pipeProperties[] =
{
   "hasData",	(pProperty)Pipe_hasData,
   "isOpen",	(pProperty)Pipe_isOpen,
   "isEmpty",	(pProperty)Pipe_isEmpty,
   "isClosed",	(pProperty)Pipe_isClosed,
   "isBroken",	(pProperty)Pipe_isBroken,
   "whyBroken",	(pProperty)Pipe_whyBroken,
   0,		0
};


//static pMethod   in_pipe_methods(Instance *ignore, register char *str);
//static pProperty in_pipe_properties(Instance *ignore, register char *str);

void pipeConstruct(void)
{
   static IBucketHeader _strawBuckets;	// might not be used

   constructObject(&PipeObject,NativeType, pipeMethods,pipeProperties,0,NoVM);
   PipeObject.name	     = "Pipe";
   PipeObject.vaultPath      = "Thread";
   PipeObject.magicMarker    = pipeMarker;
//   PipeObject.methodSearch   = in_pipe_methods;
//   PipeObject.propertySearch = in_pipe_properties;
   PipeObject.isize	     = sizeof(Pipe);
   PipeObject.threadSafe     = 1;
   PipeObject.createReturnsSelf = 1;
   pipeID = PipeObject.id;

   constructObject(&StrawObject,NativeType, pipeMethods,pipeProperties,0,NoVM);
   StrawObject.name	      = "Straw";
   StrawObject.vaultPath      = "Thread";
   StrawObject.magicMarker    = pipeMarker;
//   StrawObject.methodSearch   = in_pipe_methods;
//   StrawObject.propertySearch = in_pipe_properties;
   StrawObject.isize	      = sizeof(Straw);
   StrawObject.threadSafe     = 1;
   StrawObject.isBInstance    = 1;
   StrawObject.createReturnsSelf = 1;
   strawID = StrawObject.id;

      // also used by CuckooTuples on 32 bit systems
   strawBuckets = ibucketHitchHike(&StrawObject,0,1101,&_strawBuckets,NoVM);

//   aPipe = pipeCreate(NoVM);
//   vaultAdd(0,aPipe,NoVM);
vaultAddData("Thread.Pipe", methodCreate(Void,0,Pipe_create, NoVM),NoVM);
vaultAddData("Thread.Straw",methodCreate(Void,0,Straw_create,NoVM),NoVM);
}


///////////////////////////////////////////////////////////////////
// zkl extractTable -n pipeMethods < pipe.c | gperf | zkl gperf -i pipe
// zkl extractTable -pn pipeProperties < pipe.c | gperf | zkl gperf -i pipe



////////////////////////////////////////////////////////////////
// zkl extractTable -pn pipeProperties < pipe.c | gperf | zkl gperf -i pipe

