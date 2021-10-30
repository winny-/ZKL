/* walker.c : Walker state machine
 * 
 * Copyright (c) 2013,2014 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#include <string.h>

#define __NOT_A_DLL
#define __FCN_INTERNALS
#define __LIST_INTERNALS

#include "zklObject.h"
#include "zklFcn.h"
#include "zklClass.h"
#include "zklData.h"
#include "zklList.h"
#include "zklMethod.h"	// Partials
#include "zklString.h"	// .parts
#include "zklUtil.h"

extern Instance *walkToTheSun(pVM vm);	// number.c

typedef struct
{
   BInstance  instance;
   Instance  *src;	// the obj I'm walking
   ZGetter    getter;	// or zero if tweak
   void      *X;	// zero, static or ALLOCATED by caller
   Instance  *peekQue, *transformers;	// zero or List
   Instance  *value, *terminus, *peeker;
   unsigned   idx:32;	// where in the sequence I really am
   unsigned   n:32;	// the cursor, can be out of sync with idx
   unsigned   mx:1;	// 1 (or !0) if X was malloc'd
   unsigned   done:1;
   unsigned   noargs:1;	 // transform gets args or not
} Walker;	// Linux/64: 88, WinXP/32: 48

static ZKL_Object    WalkerObject;
static Instance	    *walker0 = 0;
static IBucketHeader wkBuckets;

static int walkerFree(Instance *self)
{
   Walker *w = (Walker *)self;
   if (w->mx && w->X) ZFREE(w->X);
   w->X = 0; w->mx = 0;
   return 1;
}

static void walkerMarker(Instance *self)
{
   Walker *w = (Walker *)self;
// GC wise, it might be a good idea to put all this stuff in a list
// and call _markListContents
   instanceMark(w->src);
   instanceMark(w->peekQue);
   instanceMark(w->value);
   instanceMark(w->transformers);
   instanceMark(w->terminus); instanceMark(w->peeker);
}

static Instance *Walker_zero(Instance *,pArglist,pVM);

    // NO PtrInts!
Instance *walkerCreate(Instance *src, ZGetter getter,void *X,int mx, pVM vm)
{
   Walker *w = (Walker *)ibucketAllocate(&wkBuckets,&WalkerObject,I_OWNED,1,vm);
   w->src    = src;
   w->getter = getter;
   w->X	     = X;
   w->mx     = (mx != 0);
   w->value  = Void;
   #if USE_POINTER_INTS && 0
      if (IS_PtrInt(self))
	 vmThrow(vm,E_ASSERTION_ERROR,"walkerCreate(): No PtrInts");
   #endif  // USE_POINTER_INTS
   return containerIsCooked(&wkBuckets,(Instance *)w,I_OWNED);
}

    // .tweak(fcn [,terminus [,peeker [,phat]]])
    // Modify an existing Walker to transform what is read
    // Half assed inheritance
static Instance *Walker_tweak(Walker *self,pArglist arglist,pVM vm)
{
   Instance *transform = arglistTryToGet(arglist,0);
   Instance *terminus  = arglistTryToGet(arglist,1);
   Instance *peeker    = arglistTryToGet(arglist,2);
   int	     t,z=0;

   if(!transform && (Instance *)self==walker0) // Walker()-->Walker.zero()
      transform=Void;
   if(!transform) return (Instance *)self;

   t = TYPEO(transform);
   if (transform==Star || t==VoidType || t==IntType) transform = 0;  //!PtrInt
   if (terminus  == VoidVoid) 			     terminus  = 0;
   if (peeker    == VoidVoid || peeker == Void)      peeker    = 0;

   if( (self->terminus || self->peeker) && (terminus || peeker) )
      vmThrow(vm,E_ASSERTION_ERROR,
	 "Walker.tweak(): Can't re-tweak terminus or peeker");
   if ((Instance *)self == walker0)
   {
      self = (Walker *)Walker_zero(Void,NoArglist,vm); // create new forever Walker
      z    = 1;
   }
   if (transform)
   {
      Fence fence;
      if(z) vmSetFence(vm,&fence,0,(Instance *)self);
	 if (!self->transformers) self->transformers = listCreate(3,0x1,I_OWNED,vm);
	 listAppend(self->transformers,transform,vm);
      if(z) vmRemoveFence(&fence,0);
   }

   self->terminus = terminus;
   self->peeker   = peeker;
   //self->tweaked  = 1;

   return (Instance *)self;
}

#if 0
    // .tweakie(fcn,init,initialResult): 
    //  feedback loop: x=transform(x), aka reduce
//!!! this could be used for scanl
static Instance *Walker_tweakie(Walker *self,pArglist arglist,pVM vm)
{
   Instance *transform = arglistGetBT(arglist,0,DeferredType,0,vm);
   Instance *init      = arglistGet(arglist,1,0,vm);
   Instance *r         = arglistTryToGet(arglist,2);

//   self = (Walker *)walkToTheSun(vm); // create new forever Walker, number.c
   self = (Walker *)Walker_zero(Void,NoArglist,vm); // create new forever Walker
   self->transformers = listCreateX(vm,transform,ZNIL);
   self->value	      = init;
   self->tweaked      = 2;
   if(r)
   {
      Fence fence;
      vmSetFence(vm,&fence,0,(Instance *)self);
	 self->peekQue = listCreate(10,0x1,I_OWNED,vm);
	 listAppend(self->peekQue,r,vm);
      vmRemoveFence(&fence,0);
   }

   return (Instance *)self;
}
#endif

    // .create(), .create(tweak), Walker()
static Instance *Walker_create(Instance *self,pArglist arglist,pVM vm)
   { return Walker_tweak((Walker *)walker0,arglist,vm); }

    // .reset(peekQueToo=True): Try to restart the walker
    // DON'T call this from a method, probably throws
static Instance *Walker_reset(Instance *self,pArglist arglist,pVM vm)
{
   extern int fcnID;	// fcn.c

   Walker *w = (Walker *)self;

   // this is enough for random access src's
   w->n = w->idx = 0; w->done = 0; w->value = Void;
   if(w->peekQue && arglistTryToGetBool(arglist,0,1,0,vm))
      listClear(w->peekQue,vm);

   if (w->transformers)
   {	// look for self.transform.container.walkerReset and call it. ick!
      Instance *f, *ts=w->transformers;
      int       n=0;
      while((f = listGet(ts,n++)))
      {
	 if (fcnID == f->objID) // if Class, ask it to reset (eg Range)
	 {
	    Instance *i;
	    ZKL_Fcn   fcn;	// is protected by self
	    i = FCN_CONTAINER(f);
	    i = classFindFcn(i,"walkerReset",1,&fcn,vm);
	    if (i) vmCallFcnFromMethod(i,NoArglist,0,vm);
	 }
      }
   }
   // else if no __sGet, not resetable
   return self;
}

static Instance *wnext(Walker *,pVM);

    // --> 0/terminus | value
static Instance *_wnext(Walker *w,int _,pVM vm)
{
   int	     fenced=0, z=0, buffering=0, doner=0;
   Instance *r, *x=0;   // x needs to be up here, above again
   Fence     fence;

   #define MAX_READ 40
   MLIST(mlist,MAX_READ);

again:
   if (w->done) goto done;
   else r = w->getter(w->src,w->idx,w->X,w->idx,vm); // read from src

   if (!r)
   {
   done:  // idx is index into src stream; last gotten i, ie w[i] is valid
      if (!doner && w->terminus && TYPEO(w->terminus)==FcnType)
      {
	 mlistBuild(mlist,(Instance *)w,ZNIL);

	 doner = 1;	// to stop an infinite loop: T.walker().cycle()
	 r = objectRun(w->terminus,(Instance *)mlist,0,vm);  // can throw
	 if (r==VoidAgain) goto again;
	 doner   = 0;
	 w->done = 1;
	 if(fenced) vmRemoveFence(&fence,0);
	 return r;
      }
      w->done = 1;
      if(fenced) vmRemoveFence(&fence,0);
      return doner ? 0 : w->terminus;
   }

   // we have a value, tweak/transform as needed
   w->idx++;
   if (r==VoidSkip) goto again;
   if (r==VoidStop) goto done;
   if (w->transformers)	// call a transform fcn, peek queue is empty
   {
      Instance *f, *ts=w->transformers, *args;
      int       n;

      if(!fenced) vmSetFence(vm,&fence,0,0);
      fenced = 1;
      for(n = 0; (f = listGet(ts,n)); n++)
      {
	 if(buffering)
	 {
	    mlistAppendI(mlist,r,MAX_READ);
	    if(--z) goto again;
	    buffering = 0;
	 }
	 else
	 {
#if 0
	    if (2==w->tweaked) mlistBuild(mlist,w->value,ZNIL);  // tweakie
	    else	       mlistBuild(mlist,r,ZNIL);
#else
	    mlistBuild(mlist,r,ZNIL);
#endif
	    if(x)	       mlistAppendI(mlist,x,MAX_READ);
	 }
	 x    = 0;
	 args = (n==0 && w->noargs) ? NoArglist : (Instance *)mlist;
	 r    = objectRun(f,args,0,vm);  // can throw
//	 r = objectRun(f,(Instance *)mlist,0,vm);  // can throw
	 if (r==VoidSkip) goto again;
	 if (r==VoidStop) goto done;
	 if (r==VoidRead)
	 {
	    buffering = 1;
	    z = 1;
	    if ((z + TUPLE_LEN(mlist)) >= MAX_READ)	// skip
	       buffering = 0;
	    goto again;
	 }
	 if (TYPEO(r) == TupleType && TUPLE_LEN(r) > 1)
	 {
	    Instance *i = TUPLE_TABLE(r)[0];
	    if (i==VoidRead)
	    {
	       buffering = 1;
	       z = (int)arglistGetInt(r,1,"Walker(Void.Read",vm);
	       if (z < 0 || (z + TUPLE_LEN(mlist)) >= MAX_READ)	// skip
		  buffering = 0;
	       goto again;
	    }
	    if (i==VoidAgain) // reduce like, pass result back to transform
	    {
	       x = fence.i1 = TUPLE_TABLE(r)[1];
	       goto again;
	    }
	    if (i==VoidStop) { r = TUPLE_TABLE(r)[1]; w->done = 1; }
	    if (i==VoidSkip) { r = TUPLE_TABLE(r)[1]; }
	    // fall off the bottom
	 }
      } //for
   }
   if(fenced) vmRemoveFence(&fence,0);
   return r;
}

    // --> 0 | value
    // .value is set on success, unchanged on failure
    // .n is the index of output ie walk[n] is valid
static Instance *wnext(Walker *w,pVM vm)
{
   Instance *r;
   if (w->peekQue && (r = listPop(w->peekQue,0,vm)))
   {
      w->n++;
      return (w->value = r);
   }
   r = _wnext(w,0,vm);
   if (r) { w->n++; return (w->value = r); }
   return 0;
}

    // .next() --> value or TheEnd
    // .read(),
Instance *Walker_next(Instance *w,pArglist arglist,pVM vm)
{
   Instance *r = wnext((Walker *)w,vm);
   if (!r) vmThrowTheEnd(vm);
   return r;
}

    // .drop(n) -->x, same as next(n) -->self
    // .drop(*) --> drop all
    // .drop(n<=0) .drop() -->noop
    // Doesn't throw
static Instance *Walker_drop(Instance *self,pArglist arglist,pVM vm)
{
   size_t sz;
   int    n = arglistGetSize(arglist,0,-1,&sz,vm);

   if(n==1)      while(wnext((Walker *)self,vm)) ;  //(*)
   else if(n==2) while(0<sz-- && wnext((Walker *)self,vm) );
   return self;
}

    // ._next() --> Bool, called by foreach
static Instance *Walker__next(Instance *w,pArglist arglist,pVM vm)
{
   Instance *r = wnext((Walker *)w,vm);
   if (!r) return BoolFalse;
   return BoolTrue;
}

#if 0
    // -->T(bool,value)
    // foreach should call this
static Instance *Walker_nextus(Walker *w,pArglist arglist,pVM vm)
{
   Instance *r = wnext(w,vm);
   if (!r) return(tupleCreateX(vm,BoolFalse,Void,ZNIL));
   return(tupleCreateX(vm,BoolTrue,r,ZNIL));
}
#endif

    // .push(x,y,z) --> peekQue.insert(0,x,y,z)
static Instance *Walker_push(Walker *w,pArglist arglist,pVM vm)
{
   Instance *pq = w->peekQue;
   if (!pq) pq = w->peekQue = listCreate(10,0x1,I_OWNED,vm);

   listInsert(pq,0,arglist,vm);
   return (Instance *)w;
}

    // -->peeker(i|Void,EoS)
static Instance *_peekPack(Walker *w,Instance *i,pVM vm)
{
   if (w->peeker)
   {
      Fence     fence;
      Instance *r, *eos = BoolFalse;
      MLIST(mlist,2);

      if (!i) { i = Void; eos = BoolTrue; }
      mlistBuild(mlist,i,eos,ZNIL);

      vmSetFence(vm,&fence,0,(Instance *)mlist);
         r = objectRun(w->peeker,(Instance *)mlist,0,vm);
      vmRemoveFence(&fence,0);
      if (r == VoidVoid && eos) vmThrowTheEnd(vm);
      else i = r;
   }
   return i;
}
static Instance *_wpeekN(Walker *w,int n,pVM vm)
{
   unsigned  sz = 0, z;
   Instance *pq = w->peekQue, *r;

   if (n < 0 || n > 100) vmThrowTheEnd(vm);
   if (pq)
   {
      if ((r = listGet(pq,n))) return (w->value = _peekPack(w,r,vm));
      sz = listLen(pq,vm);
   }
   else pq = w->peekQue = listCreate(10,0x1,I_OWNED,vm);
   r = Void;
   for (z = n - sz + 1; z--;)
   {
      r = _wnext(w,0,vm);
      if (!r)	// hit EoS before what I wanted to peek
      {
	 if (w->peeker)
	 {
	    // assume peeker is a different transform from transform 
	    // so don't queue
	    r = _peekPack(w,0,vm);
	    return (w->value = r);
	 }
	 vmThrowTheEnd(vm);
      }
      listAppend(pq,r,vm);
   }
   return (w->value = _peekPack(w,r,vm));
}
static Instance *Walker_peekN(Instance *self,pArglist arglist,pVM vm)
{
   int  n = (int)arglistGetInt(arglist,0,"Walker.peekN",vm);
   return _wpeekN((Walker *)self,n,vm);
}
static Instance *Walker_peek(Instance *self,pArglist arglist,pVM vm)
   { return _wpeekN((Walker *)self,0,vm); }


static Instance *Walker_walker(Instance *self,pArglist arglist,pVM vm)
   { return self; }

    // this is a wrapper for impedance matching
static Instance *_wgetter(Instance *pW,size_t n,void *zero,size_t _,pVM vm)
   { return wnext((Walker *)pW,vm); }

    // this is a wrapper so I can count
static Instance *_wgetterN(Instance *pW,size_t idx,void *count,size_t sz,pVM vm)
{
   if (sz >= (unsigned)count) return 0;
   return wnext((Walker *)pW,vm);
}

#if 0
    // this is a wrapper so I can apply a predicate
static Instance *_wgetterP(Instance *pW,size_t idx,void *p,size_t sz,pVM vm)
{
   Instance *i = wnext((Walker *)pW,vm);
   MLIST(mlist,2);
   mlistBuild(mlist,i,intCreate(sz,vm),ZNIL);
   if (resultToBool(objectRun((Instance *)p,(Instance *)mlist,0,vm),vm))
      return i;
   Walker_push((Walker *)pW,(Instance *)mlist,vm);
   return 0;
}
#endif

    // .filter?([n] ...)
    // .filter(Walker)-->Walker, no count
    // If n, return n results
    // n not used in any code, but nice for lazy filters
static Instance *_wfilter(Walker *w,pArglist arglist,
   Instance *(*f)(Instance *, int, ZGetter getter, void *X,
		  Instance *arglist, int a0, pVM vm),
   pVM vm)
{
   Instance *pn = arglistTryToGet(arglist,0);
   
   if (pn)
   {
//!!! make pump lazy and nuke both of these!!
      if(TYPEO(pn) == IntType)
      {
	 int64_t n = arglistGetInt(arglist,0,0,vm);
	 if (n < 0) n = 0;
	 return f((Instance *)w,0x0,_wgetterN,(void *)(size_t)(unsigned)n,arglist,1,vm);
      }
      else if(pn==walker0 && f==zfilter)  // lazy, filter(...) only
      {
	 MLIST(mlist,10);
	 Instance *args = mlistBuild(mlist,(Instance *)w,ZNIL);
	 mlistExtend(mlist,arglist,1,10);
	 return fcnRunFromClass(Utils_Helpers,"filterW",args,vm);
      }
   }
//   return f((Instance *)w,w->sink,_wgetter,0,arglist,0,vm); 
   return f((Instance *)w,0x0,_wgetter,0,arglist,0,vm); 
}
// .filter(Walker,...)-->Walker
static Instance *Walker_filter(Instance *self,pArglist arglist,pVM vm)
   { return _wfilter((Walker *)self,arglist,zfilter,vm); }
static Instance *Walker_filter1(Instance *self,pArglist arglist,pVM vm)
   { return _wfilter((Walker *)self,arglist,zfilter1,vm); }
static Instance *Walker_filter1n(Instance *self,pArglist arglist,pVM vm)
   { return _wfilter((Walker *)self,arglist,zfilter1n,vm); }
static Instance *Walker_filter22(Instance *self,pArglist arglist,pVM vm)
   { return _wfilter((Walker *)self,arglist,zfilter22,vm); }

    // .reduce([n] ...)
    // n was useful in Rosetta Code Prime conspiracy for
    //   reducing part of a stream
static Instance *Walker_reduce(Instance *self,pArglist arglist,pVM vm)
{
   Instance *pn = arglistTryToGet(arglist,0);
   
   if (pn && TYPEO(pn) == IntType)
   {
      int64_t n = arglistGetInt(arglist,0,0,vm);
      if (n < 0) n = 0;
      return zreduce(self,_wgetterN,(void *)(size_t)(unsigned)n,arglist,1,vm); 
   }
   return zreduce(self,_wgetter,0,arglist,0,vm); 
}

    // .pump([n],sink,,...) -->sink
    // .pump(Walker,sink,...) -->Walker
static Instance *Walker_pump(Instance *self,pArglist arglist,pVM vm)
//   { return pump(self,PMP_OK2CNT,_wgetter,0,arglist,0,vm); }
{
   Instance *pn = arglistTryToGet(arglist,0);
   if(pn==walker0)  // lazy
   {
      MLIST(mlist,3);
      Instance *args = mlistBuild(mlist,self,arglist,ZNIL);
      return fcnRunFromClass(Utils_Helpers,"pumpW",args,vm);
   }
   return pump(self,PMP_OK2CNT,_wgetter,0,arglist,0,vm);
}

    // .apply(action[,args])
static Instance *Walker_apply(Instance *self,pArglist arglist,pVM vm)
   { return zapply(self,_wgetter,0, 0,(void *)(size_t)ZA_LIST, arglist,0,vm); }

    // .walk([n]) -->T|TheEnd, T.walker().walk()-->TheEnd
    // --> .pump([n],List)
    // Aggregate n items from the src into self.sink
static Instance *Walker_walk(Instance *self,pArglist arglist,pVM vm)
{
   Instance *r;
//   unsigned flags = PMP_ZERO | PMP_OK2CNT;
   unsigned flags = PMP_ZERO | PMP_OK2CNT | PMP_DFAULT_CNT | (666<<16);
   MLIST(mlist,3);

   mlistCopy(mlist,arglist,0,1); mlistAppendI(mlist,emptyTuple,3);
   r = pump(self,flags,_wgetter,0,(Instance *)mlist,0,vm);

   // T.walker().walk(), T(1,2,3).walker().walk(0) -->TheEnd
   if (!r) vmThrowTheEnd(vm);
//   if (!r) return emptyTuple;
   return r;
}

Instance *zipIt(Instance *self,pArglist arglist,char *zipper,pVM vm)
{
   Instance *args;
   MLIST(mlist,20);

   if (self==walker0) args = arglist;  // Walker.zip(a,b,c)
   else { args = mlistBuild(mlist,self,ZNIL); mlistExtend(mlist,arglist,0,20); }

   return fcnRunFromClass(Utils_Helpers,zipper,args,vm);
}

    // .zip(walkables) -->Walker, lazy zipper
static Instance *Walker_zip(Instance *self,pArglist arglist,pVM vm)
   { return zipIt(self,arglist,"zipW",vm); }

Instance *zippityDoDa(Instance *self,pArglist arglist,char *zipper,pVM vm)
{
   Instance *args, *f=arglistGet(arglist,0,"Walker.zipWith",vm);
   MLIST(mlist,20);

   args = mlistBuild(mlist,f,self,ZNIL);
   mlistExtend(mlist,arglist,1,20);  // zipWith(f,self,...)

   return fcnRunFromClass(Utils_Helpers,zipper,args,vm);
}

    // .zipWith(f,walkables) -->Walker, lazy zipper, modifies self
static Instance *Walker_zipWith(Instance *self,pArglist arglist,pVM vm)
   { return zippityDoDa(self,arglist,"zipWithW",vm); }


#if 0	// this is just too easy to do with zip
    // .enumerate([n])
static Instance *Walker_enumerate(Instance *pSelf,pArglist arglist,pVM vm)
{
   int	     stop = 0;
   int64_t   count;
   Instance *pn = arglistTryToGet(arglist,0), *i;
   Instance *result = listCreate(100,0x2,I_OWNED,vm);	// RO#
   Walker   *self = (Walker *)pSelf;
   Fence     fence;
   
   if (pn)
   {
      stop  = 1; 
      count = arglistGetInt(arglist,0,"Walker.enumerate",vm);
   }

   vmSetFence(vm,&fence,0,result);
      while(1)
      {
	 if (stop && (count-- <= 0)) break;
	 if (!(i = wnext(self,vm))) break;
         listAppend(result,
		    tupleCreateX(vm,intCreate(self->n - 1,vm),i,ZNIL), vm);
      }
   vmRemoveFence(&fence,0);
   return result;
}
#endif

#if 0
    // .permute(list) -->Walker, n! results
    // same result as foreach a in (seq) { foreach b in (seq[a..]) {(a,b)} }
#define LEFT	0
#define ZIP	1
#define RIGHT	2
static Instance *_petter(Instance *seq,size_t idx,void *esds,size_t _,pVM vm)
{
   unsigned   n,N;
   int        c,d,i,j,x;
   Instance **ss = listTable(seq,&N), *r=tupleCreate(N,I_OWNED,vm);
   char      *es=(char *)esds, *ds=(char *)esds + N;

   if (idx)	// first call, just return a result, then start fiddling
   {
      // find biggest e with d!=ZIP
      i=666; c=-1;
      for(n=0; n<N; n++) if(ds[n]!=ZIP && es[n]>c) { c=es[n]; i=n; }
      if(i==666) return 0;

      d=ds[i]-1; j=(N+i+d)%N;  // LEFT,ZIP,RIGHT --> -1,0,1
      x=es[i]; es[i]=es[j]; es[j]=x;
      x=ds[i]; ds[i]=ds[j]; ds[j]=x;	// d tracks e
      if(j==N-1 || j==0 || es[(N+j+d)%N]>c) ds[j]=ZIP;
      for(n=0; n<N; n++) if(es[n]>c){
	 int x=(i-n);
	 if (!x) x=ZIP;
	 else    x=(x<0) ? LEFT : RIGHT;
	 ds[n]=x;
      }
   }

   for(n=0; n<N; n++) TUPLE_TABLE(r)[n] = ss[es[n]];
   TUPLE_LEN(r) = N;
   return r;
}
static Instance *Walker_permute(Instance *pSelf,pArglist arglist,pVM vm)
{
   Instance *seq = arglistGetBT(arglist,0,ListType,0,vm);
   unsigned  N=listLen(seq,vm), n;
   char     *esds, *ds;

   if(!N || N>20) return Void;    // 200! is 376 digits, 100! 159, 20! 19

   esds=(char *)ZCALLOC(N+N,1);
   for(n=0; n<N; n++) esds[n]=n; // es = 0,1,2,3,4,5...N-1
   for(ds=esds+N, n=1; n<N; n++) ds[n]=LEFT;
   ds[0]=ZIP; // direction to move e: LEFT, ZIP, RIGHT
   return walkerCreate(seq,_petter,(void *)esds,N,vm);
} 
#endif

static Instance *
   crossProductGetter(Instance *ws,size_t idx,void *self,size_t _,pVM vm)
{
   Instance *r, **wst, **rt;
   unsigned  sz,n;

   wst = listTable(ws,&sz);
   if(idx==0){ for(n=sz; n--; ) if(!wnext((Walker *)wst[n],vm)) return 0; }
   else
   {
      for(n=sz; n--; )
      {
	 Walker *w = (Walker *)wst[n];
	 if(wnext(w,vm)) goto allGood;
	 Walker_reset((Instance *)w,NoArglist,vm);  // can throw
	 if(!wnext(w,vm)) return 0; // didn't reset or is empty
      }
      return 0;  // walked entire set
   }
allGood:   // go through the list of walkers and get a value from each
   r = tupleCreate(sz,I_OWNED,vm); rt = listTable(r,(unsigned *)&n);
   for(n=0; n<sz; n++) rt[n] = ((Walker *)wst[n])->value;
   TUPLE_LEN(r) = sz;
   return r;
}
    // .cproduct(x,y,z,..) --> Cartesian product Walker(x.walker(), ...)
//!!! rename this to xProduct or crossProduct
//!!!shouldn't this be self.cproduct(x) --> crossProduct(self,x.walker)???
static Instance *Walker_crossProduct(Instance *self,pArglist arglist,pVM vm)
{
   Instance *ws, *args;
   MLIST(mlist,3);

   args = mlistBuild(mlist,kStringCreate("walker",0,I_OWNED, vm),ZNIL);
   ws   = List_apply(arglist,args,vm);
#if 0
   ws=walkerCreate(ws,crossProductGetter,0,0,vm);
   ((Walker *)ws)->X = ws;
   return ws;
#else
   return walkerCreate(ws,crossProductGetter,0,0,vm);
#endif
}

    // .cycle()      --> cycle src
    // .cycle(a)     --> a.walker().cycle()
    // .cycle(a,b,c) --> arglist.walker().cycle()
static Instance *Walker_cycle(Instance *self,pArglist arglist,pVM vm)
{
   MLIST(mlist,3);

   if (arglistTryToGet(arglist,0))   // cycle(1,2,3), cycle(T(1,2,3))
      return fcnRunFromClass(Utils_Helpers,"cycle",arglist,vm);
   return fcnRunFromClass(Utils_Helpers,"__walkInCircles",mlistBuild(mlist,self,ZNIL),vm);
}

    // .chain(a,b,c) --> walk a, then b then c
static Instance *Walker_chain(Instance *self,pArglist arglist,pVM vm)
{
   MLIST(mlist,41);

   if(self==walker0) return fcnRunFromClass(Utils_Helpers,"chain",arglist,vm);

   if (TUPLE_LEN(arglist) > 40)
      vmThrow(vm,E_ASSERTION_ERROR,"Walker.chain: Limit 40");
   mlistBuild(mlist,self,ZNIL); mlistExtend(mlist,arglist,0,41);
   return fcnRunFromClass(Utils_Helpers,"chain",(Instance *)mlist,vm);
}

    // .chunk(sz [,sink=List]) --> read in chunks
static Instance *Walker_chunk(Instance *self,pArglist arglist,pVM vm)
{
   MLIST(mlist,3);

   mlistBuild(mlist,self,ZNIL); mlistExtend(mlist,arglist,0,3);
   return fcnRunFromClass(Utils_Helpers,"blowChunks",(Instance *)mlist,vm);
}

    // .zero() --> null walker
    // aka null getter
static Instance *_0Walker(Instance *i,size_t n,void *X,size_t _,pVM vm)
   { return Zero; }
static Instance *Walker_zero(Instance *self,pArglist arglist,pVM vm)
{
   Instance *w = walkerCreate(Zero,_0Walker,0,0,vm);
   ((Walker *)w)->noargs = 1;
   return w;
}

static const MethodTable walkerMethods[] =
{
   "_next",		Walker__next,	// Pinned for foreach
   "walkerNext#",	Walker__next,	// for the compiler

   "create",		Walker_create,

   "toBool",		Bool_soTrue,

   "next",		Walker_next,
   "read",		Walker_next,
   "drop",		Walker_drop,
   "push",		(pMethod)Walker_push,
   "peek",		Walker_peek,
   "peekN",		Walker_peekN,
   "walker",		Walker_walker,
   "reset",		Walker_reset,
   "walkerReset",	Walker_reset,

   "apply",		Walker_apply,
   "pump",		Walker_pump,
   "reduce",		Walker_reduce,
   "filter",		Walker_filter,
   "filter1",		Walker_filter1,
   "filter1n",		Walker_filter1n,
   "filter22",		Walker_filter22,
   "walk",		Walker_walk,
   "zip",		Walker_zip,
   "zipWith",		Walker_zipWith,

   "tweak",		(pMethod)Walker_tweak,
   "cproduct",		Walker_crossProduct,
   "cycle",		Walker_cycle,
   "chain",		Walker_chain,
   "chunk",		Walker_chunk,
   "zero",		Walker_zero,
   0,			0
};

	////////////////////////  Properties /////////////////////

    // .value, ._value
static Instance *Walker_value(Instance *self, pVM vm)
   { return ((Walker *)self)->value; }

    // .n, ._n
static Instance *Walker_n(Instance *self, pVM vm)
   { return intCreate(((Walker *)self)->n,vm); }

    // .idx
static Instance *Walker_idx(Instance *self, pVM vm)
{
   unsigned n = ((Walker *)self)->idx;
   if (n == 0) return Zero;
   return intCreate(n-1,vm);
}

    // .atEnd
    // Only know if atEnd when have tried to read past it, 
    //    ie [1].read(), atEnd==False
static Instance *Walker_atEnd(Instance *self, pVM vm)
   { return (((Walker *)self)->done) ? BoolTrue : BoolFalse; }

    // .instance
static Instance *Walker_instance(Instance *self, pVM vm)
   { return ((Walker *)self)->src; }

    // .transform
static Instance *Walker_transform(Instance *self, pVM vm)
{
   Instance *t = ((Walker *)self)->transformers;
   if (!t) t = Void;
   return t;
}

    // .terminus
static Instance *Walker_terminus(Instance *self, pVM vm)
{
   Instance *t = ((Walker *)self)->terminus;
   if (!t) t = Void;
   return t;
}

    // .peeker
static Instance *Walker_peeker(Instance *self, pVM vm)
{
   Instance *t = ((Walker *)self)->peeker;
   if (!t) t = Void;
   return t;
}

    // .tweaked --> n (zero if not tweaked)
static Instance *Walker_tweaked(Instance *self, pVM vm)
{
   Walker *w = (Walker *)self;
   if (w->transformers) return intCreate(listLen(w->transformers,vm),vm);
   return Zero;
}

    // .parts --> 
    // T(src,idx,n,value,done, peekQue, transforms,terminus,peeked)
static Instance *Walker_parts(Instance *self, pVM vm)
{
   Walker   *w = (Walker *)self;
   Instance *pq = w->peekQue, *transform = w->transformers;
   Instance *terminus = w->terminus, *peeker = w->peeker;
   if (!pq) 	   pq 	     = emptyTuple;
   if (!transform) transform = Void;
   if (!terminus)  terminus  = Zero;
   if (!peeker)    peeker    = Void;
   
   return tupleCreateX(vm,w->src,
         intCreate(w->idx,vm),intCreate(w->n,vm),w->value,
	 w->done ? kStringCreate("done",0,I_OWNED,vm) : emptyString,
         pq,transform,terminus,peeker,
	 ZNIL);
}

static const PropertyTable walkerProperties[] =
{
   "value",		Walker_value,   // Pinned for foreach
   "n",			Walker_n,
   "idx",		Walker_idx,
   "atEnd",		Walker_atEnd,
   "instance",		Walker_instance,
   "transform",		Walker_transform,
   "terminus",		Walker_terminus,
   "peeker",		Walker_peeker,
   "tweaked",		Walker_tweaked,
   "parts",		Walker_parts,
   "walkerValue#",	Walker_value,   // for foreach mixin
   0,			0
};


/* ******************************************************************** */
/* ***************************** Op Codes ***************************** */
/* ******************************************************************** */

static Instance *walkerOp(Instance *self,char *op, Instance *X,pVM vm)
{
   MLIST(mlist,4);
   return fcnRunFromClass(Utils_Helpers,"walkerOp", 
      mlistBuild(mlist,self,kStringCreate(op,0,I_OWNED, vm),X,ZNIL),vm);
}

    // walker + 1 --> walker.tweak('+(1))
static Instance *walker_add(Instance *self,Instance *X,pVM vm)
   { return walkerOp(self,"+",X,vm); }
    // walker * 10 --> walker.tweak('*(10))
static Instance *walker_mul(Instance *self,Instance *X,pVM vm)
   { return walkerOp(self,"*",X,vm); }

static Instance *walker_eq(Instance *self,Instance *X,pVM vm)
   { return walkerOp(self,"==",X,vm); }
static Instance *walker_neq(Instance *self,Instance *X,pVM vm)
   { return walkerOp(self,"!=",X,vm); }
static Instance *walker_gt(Instance *self,Instance *X,pVM vm)
   { return walkerOp(self,">",X,vm); }
static Instance *walker_gte(Instance *self,Instance *X,pVM vm)
   { return walkerOp(self,">=",X,vm); }
static Instance *walker_lt(Instance *self,Instance *X,pVM vm)
   { return walkerOp(self,"<",X,vm); }
static Instance *walker_lte(Instance *self,Instance *X,pVM vm)
   { return walkerOp(self,"<=",X,vm); }

static const OpcodeTable ops[] =
{
   OP_ADD,	walker_add,
   OP_MUL,	walker_mul,
   OP_EQ,	walker_eq,
   OP_NEQ,	walker_neq,
   OP_GT,	walker_gt,
   OP_GTE,	walker_gte,
   OP_LT,	walker_lt,
   OP_LTE,	walker_lte,
   0,		0
};

//////////////////////////////////////////////////////////////////////////

    // aka never getter
static Instance *_noWalker(Instance *i,size_t n,void *X,size_t _,pVM vm)
   { return 0; }

//static pMethod   in_walker_methods(Instance *ignore, register char *str);
//static pProperty in_walker_properties(Instance *ignore, register char *str);

void walkerConstruct(void)
{
   constructObject(&WalkerObject,WalkerType, 
		   walkerMethods,walkerProperties,ops,NoVM);
   WalkerObject.name	       = "Walker";
   WalkerObject.isBInstance    = 1;
   WalkerObject.magicMarker    = walkerMarker;
   WalkerObject.freeMe	       = walkerFree;
//   WalkerObject.methodSearch   = in_walker_methods;
//   WalkerObject.propertySearch = in_walker_properties;
   WalkerObject.isize	       = sizeof(Walker);
   WalkerObject.createReturnsSelf = 1;

   ibucketReserve(&WalkerObject,500,&wkBuckets,0,NoVM);

   walker0 = walkerCreate(Void,_noWalker,0,0,NoVM);
   vaultAdd(0,walker0,NoVM);
//vaultAddData("Utils.permuteW", methodCreate(Void,0,Walker_permute, NoVM),NoVM);
}

#if 0
/* ******************************************************************** */
/* ****************************** Getter ****************************** */
/* ******************************************************************** */

// A minimal Walker, can be used in, eg, foreach or zip
// blech too big

typedef struct
{
   BInstance  instance;
   Instance  *src, *value;	// the obj I'm walking
   ZGetter    getter;	// or zero if tweak
   void      *X;	// zero, static or ALLOCATED by caller
   unsigned   idx:32;	// where in the sequence I really am
   unsigned   mx:16;	// 1 if X was malloc'd
   unsigned   done:1;
} Getter;  // Linux/64:48, WinXP/32: !!!! no buckets of this size

static ZKL_Object GetterObject;

    // next: value or 0
static Instance *getterNext(Getter *g,pVM vm)
{
   Instance *r;

   if (g->done) return 0;
   r = g->getter(g->src,g->idx,g->X,g->idx,vm);
   if (!r)
   {
      g->done = 1;
      return 0;
   }
   g->idx++;
   return r;
}

    // Getter._next() --> Bool
static Instance *Getter__next(Instance *g,pArglist arglist,pVM vm)
{
   Instance *r = getterNext((Getter *)g,vm);
   return r ? r : BoolFalse;
}

    // Getter.next() --> value or TheEnd
static Instance *Getter_next(Instance *g,pArglist arglist,pVM vm)
{
   Instance *r = getterNext((Getter *)g,vm);
   if (!r) vmThrowTheEnd(vm);
   return r;
}

#if 0
    // zipper(longShort,i,i,i,...)
lazy or now
terminus if long, Void.Void if no fill, Void.Stop == short
call back
Data needs to know how to get (string/byte/line)
static Instance *zipper(Instance *i,pArglist arglist,pVM vm)
{
   ZGetter getters[40];
   Instance rs[40,  **table;
   unsigned n,N;
   MLIST(rs,40);

   table = listTable(arglist,&n);
   get getters: how???? probably need to be passed a Getter/Walker

   mlistCopy(rs,T,0);
   table = TUPLE_TABLE(rs); TUPLE_LEN(rs) = N;

   while(1){
      for(n=0; n<N; n++){
         table[n]=getters[n](...);
	 if(0){
	    if (stopShort) return Void; 
	    table[n] = terminus;
	 }
      }
      objectRun(f,rs)
   }
}
#endif

static const MethodTable getterMethods[] =
{
   "_next",		Getter__next,	// Pinned for foreach
   "walkerNext#",	Getter__next,	// for foreach mixin
//   "create",		Getter_create,???????????????
   "toBool",		Bool_soTrue,
   "next",		Walker_next,
   "walker",		Walker_walker,
   "reset",		Walker_reset,
   "walkerReset",	Walker_reset,

   "apply",		Walker_apply,
   "pump",		Walker_pump,
   "reduce",		Walker_reduce,
   "filter",		Walker_filter,
   "filter1",		Walker_filter1,
   "filter1n",		Walker_filter1n,
   "filter22",		Walker_filter22,
   "zip",		Walker_zip,
   "zipWith",		Walker_zipWith,

   "cycle",		Walker_cycle,
   0,			0
};


	////////////////////////  Getter Properties /////////////////////

    // .value,
static Instance *Getter_value(Instance *self, pVM vm)
   { return ((Getter *)self)->value; }

    // .idx
static Instance *Getter_idx(Instance *self, pVM vm)
{
   int n = ((Getter *)self)->idx;
   if (n == 0) return Zero;
   return intCreate(n-1,vm);
}

    // .atEnd
    // Only know if atEnd when have tried to read past it, 
    //    ie [1].read(), atEnd==False
static Instance *Getter_atend(Instance *self, pVM vm)
   { return (((Getter *)self)->done) ? BoolTrue : BoolFalse; }

    // .instance
static Instance *Getter_instance(Instance *self, pVM vm)
   { return ((Getter *)self)->src; }

static const PropertyTable getterProperties[] =
{
   "value",		Getter_value,   // Pinned for foreach
   "idx",		Getter_idx,
   "atEnd",		Getter_atend,
   "instance",		Getter_instance,
   "walkerValue#",	Getter_value,   // for foreach mixin
   0,			0
};


#endif




///////////////////////////////////////////////////////////////////
// zkl extractTable.zkl < walker.c | gperf | zkl gperf.zkl -i walker
// zkl extractTable.zkl -p < walker.c | gperf | zkl gperf.zkl -i walker



//////////////////////////////////////////////////////////////////////
/////////////////// Walker Properties ///////////////////////////////
// zkl extractTable.zkl -p < walker.c | gperf | zkl gperf.zkl -i walker


