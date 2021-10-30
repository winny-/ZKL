/* method.c : The Method   Object: a method call wrapper object
 * 	      The Property Object: a wrapper for properties and vars
 * 
 * Copyright (c) 2006,2007,2008-12,2013 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#include <stdio.h>
#include <string.h>

#define __NOT_A_DLL
#define __LIST_INTERNALS

#include "zklObject.h"
#include "zklClass.h"
#include "zklFcn.h"		// for Deferred
#include "zklList.h"		// emptyList
#include "zklMethod.h"
#include "zklNumber.h"
#include "zklString.h"

int methodID;

static ZKL_Object MethodObject;

static IBucketHeader mbuckets;

	// Compling the parser creates 1,775,973 Methods
	// 964,331 if use op resolve/call

typedef struct
{
   BInstance  instance;		// iflag==1 if unbound
//uint16_t id;	// in padding on 64 bit Linux, not in win10 32 bit
   Instance  *self;		// the instance this method is bound to
   pMethod    method;		// points into self
}Method;	// 12 bytes/32, 24/64

#define M_METHOD(instance)	( ((Method *)instance)->method )
#define M_SELF(instance)	( ((Method *)instance)->self )

Instance *methodCreate2(
Instance *self, char *methodName, pMethod method, int itype, pVM vm)
{
   Instance *m;

   if (!method)
   {
      method = searchForMethod(self,methodName,1,vm);
      if (!method)
      {
	 char buf[100];
	 sprintf(buf,"Method \"%.80s\" not found",methodName);
	 vmThrow(vm,E_NOT_FOUND,buf);
      }
   }

	// Forbid vm.createFiber(fcn { L(1).apply2(vm.yield); })
        // which munges the C stack
   if (method == (pMethod)VM_yield)
      vmThrow(vm,E_ASSERTION_ERROR,"Method(vm.yield): Can be used for evil");

   m = ibucketAllocate(&mbuckets,&MethodObject,itype,1,vm);
   M_SELF(m)   = self;
   M_METHOD(m) = method;

   return containerIsCooked(&mbuckets,m,itype);
}

Instance *methodCreate(Instance *self,char *methodName,pMethod method,pVM vm)
   { return methodCreate2(self,methodName,method,I_OWNED,vm); }

//pMethod methodMethod(Instance *self) { return M_METHOD(self); }

static void methodMarker(Instance *_self)
{
   Method *self = (Method *)_self;

   if(_self->iflag) return;

   #if GC_SANITY_CHECK
      #if USE_POINTER_INTS
         if (IS_PtrInt(self->self)) vmHalt("Method holds PtrInt");
      #endif
      if (IS_IMEM(self->self)) vmHalt("Method holds IMem");
   #endif

   instanceMark(self->self);
}

#if USE_POINTER_INTS
	// if I want to get rid of this (and I do), I need a IMem PtrInt
   void methodThunk(Instance *self, int64_t i64, pVM vm)
      { M_SELF(self) = intAllocate(i64,vm); }
#endif

///////////////////////////////// Method Methods

    // Method.toString() --> "Method(List.append)"
    // This will boo-boo if a table has multiple names for the same method
    // eg List has lots of noops, the first match is used.
    // Method name points into static C text space (ie doesn't move)
    //   unless shared libraries can be unloaded
static char *methodName(Method *m, char **objName)
{
   char	   *mName=0;
   int	    i;
   pMethod  method = M_METHOD(m);
   const MethodTable *table;

   if(((Instance *)m)->iflag)	// unbound
   {
      if(objName) *objName = "";
      return getGlobalName((unsigned)m->self,NoVM);
   }

   if(objName) *objName = iname(m->self);

   table = METHOD_TABLE(m->self);

   for (i = 0; table[i].name; i++)  // could be in a DLL
      if (table[i].method == method) { mName = table[i].name; break; }
   if (!mName)
   {
      table = objectMethods;
      for (i = 0; table[i].name; i++)
	 if (table[i].method == method) { mName = table[i].name; break; }
   }
   return mName;     // might be zero: self.BaseClass("toBool")
}
static Instance *Method_toString(Method *self,pArglist arglist,pVM vm)
{
   char *objName, *mName = methodName(self,&objName);
   if (mName) return stringCat(vm,"Method(",objName,".",mName,")",(char *)0);
      // not found, eg Class_base("ToBool")
   return stringCat(vm,"Method(",objName,".\?\?\?)",(char *)0);
}

    // Method([args]), called by vmCall() and objectRun()
    // args[0] is self
Instance *Method_call(Instance *self,pArglist arglist,pVM vm)
{
   pMethod method = M_METHOD(self);
   int	   s;
   if(!method)  // Unbound method: late bind method name to arg0
   {
      Instance *i  = arglistGet(arglist,0,"Method.unbind()(obj)",vm);
      unsigned  id = (unsigned)M_SELF(self);
      MLIST(mlist,20);

//!!!??? I would like to verify type is same as original bound
// stash method id in struct
// T.close.unbind() --> toList, not close

      #if USE_POINTER_INTS
	 if (IS_PtrInt(i))
	 {
	    i = ptrIntDoMethod(i,id,0,arglist,vm);
	    return i ? i : Void;
	 }
      #endif

      mlistCopy(mlist,arglist,1,20);
      // i.resolve(name)
      if (i->objID == deferredID)	// Deferred("","hoho")."len"
	 i = Deferred_value(i,vm);
      s = objectResolveN(&i,id,&method,0,1,vm);  // bitch if not found
      if(s==MethodType && method!=(pMethod)VM_yield)
         return method(i,(Instance *)mlist,vm);
      return Void;
   }
   return M_METHOD(self)(M_SELF(self),arglist,vm);
}

    // Method.unbind(): unbind method's self  --> new Method
static Instance *Method_unbind(Instance *self,pArglist arglist,pVM vm)
{
   char *name;

   if(self->iflag) return self;		// already unbound
   name = methodName((Method *)self,0);
   if(name){
      Instance *m = ibucketAllocate(&mbuckets,&MethodObject,I_OWNED,1,vm);
      unsigned  id;
      getGlobalId(name,&id,vm);	// names exist for all methods
      M_SELF(m)   = (Instance *)(size_t)id;
      m->iflag    = 1;
      M_METHOD(m) = 0;  // I could overload with with an arglist & iflag=1
      return containerIsCooked(&mbuckets,m,I_OWNED);
   }
   return Void;
}

    // Method.future(args):
static Instance *Method_future(Instance *self,pArglist arglist,pVM vm)
{
   MLIST(mlist,13);
   if (TUPLE_LEN(arglist) > 10)
      vmThrow(vm,E_ASSERTION_ERROR,
		"Method.future: No more than 10 static parameters");
   mlistBuild(mlist,self,ZNIL); mlistExtend(mlist,arglist,0,13);
   return fcnRunFromClass(Utils_Helpers,"backToTheFuture",(Instance *)mlist,vm);
}

static const MethodTable mMethods[] = 
{
   "toString",	(pMethod)Method_toString,
   "toBool",	(pMethod)Bool_soTrue,
   "defer",	(pMethod)Object_defer,
   "future",	(pMethod)Method_future,
   "unbind",	Method_unbind,
   0,		0
};


//////////////////////////////// Method Properties

    // Method.instance
Instance *Method_instance(Instance *self,pVM vm)
{
   if(self->iflag) return Void;
   return ((Method *)self)->self; 
}

    // Method.methodName
Instance *Method_methodName(Instance *self,pVM vm)
{
   char *name;
   if(self->iflag) 
      return stringCreate(getGlobalName((unsigned)M_SELF(self),vm),I_OWNED,vm);
   name = methodName((Method *)self,0);
   if(name) return stringCreate(name,I_OWNED,vm);
   return Void; 
}

static const PropertyTable mProperties[] = 
{
   "instance",		Method_instance,
   "methodName",	Method_methodName,
   0,		0
};

/* ******************************************************************** */
/* *********************** The Property Object ************************ */
/* ******************************************************************** */

static ZKL_Object PropertyObject;

typedef struct
{
   Instance	instance;	// Inherit from Instance
   Instance    *self;		// the instance this method is bound to
   unsigned int	isVar:1;	// 1 if var, 0 if object
   unsigned int	isProxy:1;	// 1 if proxy var
   unsigned int	n:16;		// the nth property or fcn
   pProperty	property;
} Property;	// 20 bytes

static Instance *Property_value(Property *,pVM);

static void propertyMarker(Instance *_self)
{
   Property *self = (Property *)_self;
   #if GC_SANITY_CHECK		// stinker is self->self needs thunking
      #if USE_POINTER_INTS
         if (IS_PtrInt(self->self)) vmHalt("Property holds PtrInt");
      #endif
      if (IS_IMEM(self->self)) vmHalt("Property holds IMem");
   #endif

   instanceMark(self->self);
}

    // .Property("name",var=0)
    // If class, var is: 1 (var only), 2 (property only), 0 (either)
Instance *propertyCreate(Instance *i, char *name, int var, int itype, pVM vm)
{
   char		buf[100];
   int		n = 0, isVar = 0, isProxy = 0;
   pProperty	property = 0;
   Property    *p;

   if (TYPEO(i) == ClassType && var != 2)
   {
      n = classFindVar2(i,name,&isProxy,vm);	// no parents
      if (n != -1) isVar = 1;
      else if (var == 1)
      {
	 sprintf(buf,"Class.Property(var \"%.80s\") not found",name);
	 vmThrow(vm,E_NOT_FOUND,buf);
      }
   }

   if (!isVar)
   {
      property = searchForProperty(i,name,1,vm);
      if (!property)
      {
	 sprintf(buf,"Property/variable \"%.80s\" not found",name);
	 vmThrow(vm,E_NOT_FOUND,buf);
      }
   }

   p = (Property *)instanceAllocate(sizeof(Property),&PropertyObject,1,vm);
   p->self     = i;
   p->isVar    = isVar;
   p->isProxy  = (isProxy != 0);
   p->n        = n;
   p->property = property;

   return addToCollectables((Instance *)p,itype,vm);
}

#if USE_POINTER_INTS
   void propertyThunk(Instance *self, int64_t i64, pVM vm)
      { ((Property *)self)->self = intAllocate(i64,vm); }
#endif

///////////////////////////////// Property Methods

    // Property.toString() --> "Property(List.name)" or "Var(Walker._nth)"
static Instance *Property_toString(Property *self,pArglist arglist,pVM vm)
{
   char	         *name = 0;
   int		  i;
   pProperty	  property;
   const PropertyTable *table;

   if (self->isVar)
      return stringCat(vm,"Var(",iname(self->self),".",
		classNameOfNthVar(self->self,self->n),")",(char *)0);

   property = self->property;
   table = PROPERTY_TABLE(self->self);
   for (i = 0; table[i].name; i++)
      if (table[i].property == property) { name = table[i].name; break; }
   if (!name)
   {
      table = objectProperties;
      for (i = 0; table[i].name; i++)
	 if (table[i].property == property) { name = table[i].name; break; }
   }
   if (name)
      return stringCat(vm,"Property(",iname(self->self),".",name,")",(char *)0);

   return emptyString;	// shouldn't get here
}

    // Property()
Instance *Property_call(Instance *self,pArglist arglist,pVM vm)
   { return Property_value((Property *)self,vm); }

static const MethodTable proMethods[] = 
{
   "create",	(pMethod)Property_call,
   "toString",	(pMethod)Property_toString,
   "toBool",	(pMethod)Bool_soTrue,
   0,		0
};


//////////////////////////////// Property Properties

    // Property.instance
Instance *Property_instance(Property *self,pVM vm) { return self->self; }

    // Property.value
static Instance *Property_value(Property *self,pVM vm)
{
   if (self->isVar)
   {
      Instance *i = classGetVar(self->self,self->n);
      if (self->isProxy)
	 { i = objectRun(i,emptyList,0,vm); instanceIsOrphan(i); }
      return i;
   }
   else return self->property(self->self,vm);
}

static const PropertyTable proProperties[] = 
{
   "instance",		(pProperty)Property_instance,
   "value",		(pProperty)Property_value,
   0,			0
};


/* ******************************************************************** */
/* ************************** The Op Object *************************** */
/* ******************************************************************** */

#define OP_NOT	100

struct { int offset; char *name; } opTable[] = 
{
   { OP_EQ,	"=="  },
   { OP_NEQ,	"!="  },
   { OP_LT,	"<"   },
   { OP_LTE,	"<="  },
   { OP_GT,	">"   },
   { OP_GTE,	">="  },
   { OP_NEGATE,	"--"  },
   { OP_ADD,	"+"   },
   { OP_SUB,	"-"   },
   { OP_MUL,	"*"   },
   { OP_DIV,	"/"   },
   { OP_MOD,	"%"   },
   { OP_NOT,	"not" },	// fake op
   { 0,		 0    },
};

typedef struct
{
   BInstance  instance;		// Inherit from Instance
   int	      offset;		// of op
   Instance  *X;
} OpWrapper;		// 12 bytes/32, 16/64
// might be nice to have a method ptr so can cache Op("+").fp(1)

#define OSELF(i)	( (OpWrapper *)i )

static ZKL_Object     OpObject;
static IBucketHeader *opBuckets;
static OpWrapper      op1;

static void opMarker(Instance *op) { instanceMark(((OpWrapper *)op)->X); }

///////////////////////////////// Op Methods

#if USE_POINTER_INTS
   #define PLOP(i,ZN)					\
      if (IS_PtrInt(i)) {				\
	 op = O_OP(&IntObject,offset);			\
	 PtrInt_TO_Int64(i,&ZN); i = (Instance *)&ZN;	\
      } else op = I_OP(i,offset);
#else
   #define PLOP(i,N) op = I_OP(i,offset);
#endif

    // Op.call, called by vmCall() and objectRun()
    // Op("+").call(x,y) --> x + y, Op("+",1).call(y) --> y + 1
    // call(x,y,z...):
    //   (+ a b c) --> (a + b) + c
    //   (< a b c) --> if (a < b) (b < c) == a<b<c
    //   '<(0)(1,2) --> 1<0 && 2<0
Instance *Op_call(Instance *self,pArglist arglist,pVM vm)
{
   unsigned   NA, n = 1;
   Instance **args  = listTable(arglist,&NA);
   Instance  *i     = arglistGet(arglist,0,"Op.call",vm);
   Instance  *X     = Void, *r;
   int       offset = OSELF(self)->offset;
   ZKL_Int   ZN;
   pOp	     op;

   if (offset == OP_NOT)
   {  // '--() --> !i, '--(f) --> !f(i)
      if ((X = OSELF(self)->X)) i = objectRun(X,arglist,0,vm);
      return resultToBool(i,vm) ? BoolFalse : BoolTrue;
   }

   if (offset != OP_NEGATE)
      if (!(X = OSELF(self)->X))
         { X = arglistGet(arglist,1,"Op.call",vm); n = 2; }

   PLOP(i,ZN);

   if (offset == OP_NEGATE || offset == OP_MOD) r = op(i,X,vm);
   if (offset < OP_NEGATE)	// logic ops
   {
      if (n == 1)	// '<(X)(a,b,c) --> a<X && b<X && c<X
      { 
	 for(; n <= NA; i = args[n++])
	 {
	    PLOP(i,ZN);
	    r = op(i,X,vm); 
	    if (r != BoolTrue) return BoolFalse;
	 }
      }
      else // '<(a,b,c) --> a<b<c == (a<b) && (b<c)
      {
	 while(1) {
	    if (op(i,X,vm) != BoolTrue) return BoolFalse;
	    if (n == NA) break;
	    i = X; X = args[n++];
	    PLOP(i,ZN);
	 }
      }
      return BoolTrue;
   }
   else		// math ops: '+(a,b,c)-->a+b+c, '+(1)(a,b)-->a+1+b
   {		// '-(a,b,c)-->a-b-c, '-(1)(a,b)-->a-1-b
      r = op(i,X,vm);
      while(X = args[n++], n <= NA)
      {
	 PLOP(r,ZN); // in case result changes, eg 1-->BigInt, class
	 r = op(r,X,vm);
      } 
   }
   return r;
}

    // Op(opName [,X]): + - * / % -- == != < <= > >= 
Instance *Op_create(Instance *self,pArglist arglist,pVM vm)
{
   OpWrapper *op;
   int	      i,offset;
   char	     *name, *opName,buf[200];
   Instance  *X;

   if (self == (Instance *)&op1)	// virgin --> Op.create(...)
   {
      opName = arglistGetOnlyString(arglist,0,"Op",vm);
      X      = arglistTryToGet(arglist,1);

	 // name --> opTable offset
      for(i=0; (name = opTable[i].name); i++)
	 if (0 == strcmp(name,opName)) { offset = opTable[i].offset; break; }
      if (!name)
      {
	 sprintf(buf,"Op(%.100s)",opName);
	 vmThrow(vm,E_NAME_ERROR,buf);
      }

      op = (OpWrapper *)ibucketAllocate(opBuckets,&OpObject,I_OWNED,1,vm);
      op->offset = offset;
      op->X      = ptrIntThunk(X,vm);

      return containerIsCooked(opBuckets,(Instance *)op,I_OWNED);
   }
   return Op_call((Instance *)self,arglist,vm);
}

    // Op.toString() --> "Op(+[X])"
static Instance *Op_toString(OpWrapper *self,pArglist arglist,pVM vm)
{
   int       i,offset = self->offset;
   Instance *X = self->X;

      // offset --> name
   for(i=0; offset != opTable[i].offset; i++) ;
   if (X)
   {
      X = M2_STRING(X,vm);
      return stringCat(vm,"Op(",opTable[i].name,stringText(X),")",(char *)0);
   }
   return stringCat(vm,"Op(",opTable[i].name,")",(char *)0);
}

static const MethodTable opMethods[] = 
{
   "toString",	(pMethod)Op_toString,
   "toBool",	(pMethod)Bool_soTrue,
   "create",	(pMethod)Op_create,
   "call",	(pMethod)Op_call,
   0,		0
};

static void opConstruct()
{
   static IBucketHeader _opBuckets;	// might not be used

   constructObject(&OpObject,OpType, opMethods,0,0,NoVM);
   OpObject.magicMarker = opMarker;
   OpObject.isize	= sizeof(OpWrapper);
   OpObject.isBInstance = 1;

   opBuckets = ibucketHitchHike(&OpObject,4,10,&_opBuckets,NoVM);

   instanceInit((Instance *)&op1,&OpObject,I_UNTOUCHABLE);
   op1.offset = OP_EQ;
   vaultAdd(0,(Instance *)&op1,NoVM);
}

static ZKL_Object DeferredObject;

/* ******************************************************************** */
/* ************************* The Partial Object *********************** */
/* ******************************************************************** */

extern int fcnID;	// fcn.c

int deferredID;

    // Partial [Function] Application: A closure over a arglist
    // A Partial is actually part of a Deferred
    // Note: the closed over data can't leak back out (unless a container)

typedef struct
{
   BInstance  instance;	    // flag2==1: Partial
   Instance  *f;
   Instance  *closedOverValues;	// aka Deferred->arglist
   //------- Deferred
   unsigned   n:8;		// number of dynamic args
   unsigned   chop:8;		// for fpM
   unsigned   mask:8;		// for fpM
   unsigned   useMask:1;	// for fpM
} Partial;   // 32 bytes (Linux64), a close enough intersection with Deferred

static IBucketHeader *pBuckets;

    // f(static args) --> Partial with a "hole" for the first n params, 
    //   followed by args
    // f() --> f
static Instance *partialCreate(
Instance *f,pArglist arglist,int n, int mask, unsigned offset, pVM vm)
{
   Fence    fence;
   Partial *p;

   if (mask==666) mask = 0;
   else if (!mask && (listLen(arglist,vm)<=offset)) // no closed over args
      return f;

   p = (Partial *)ibucketAllocate(pBuckets,&DeferredObject,I_OWNED,1,vm);
   vmSetFence(vm,&fence,0,0);
      p->f = fence.i1 = ptrIntThunk(f,vm);  // just in case (123).fp(456)
      p->closedOverValues = fence.i2 = arglistDup(arglist,offset,vm);
      p->instance.iflag2 = 1;
      p->n = n; p->useMask = mask;
      containerIsCooked(pBuckets,(Instance *)p,I_OWNED);
   vmRemoveFence(&fence,0);
   return (Instance *)p;
}

    // f.fp(args) -->Partial
    // f.fp() --> Partial with no args
    // Create a closure of f over args, partial application
Instance *Partial_fp(Instance *self,pArglist arglist,pVM vm)
{
   Instance *pi = partialCreate(self,arglist,0,666,0,vm);
   return pi;
}

    // f.fp0(args) --> f(args)
    // Slightly more special closure
Instance *Partial_fp0(Instance *self,pArglist arglist,pVM vm)
   { return partialCreate(self,arglist,0,0,0,vm); }

    // Slightly more special closure
Instance *Partial_fp1(Instance *self,pArglist arglist,pVM vm)
   { return partialCreate(self,arglist,1,0,0,vm); }

    // f.fp2(args) --> f(?,?,args)
    // Slightly more special closure, nice for *.reduce
Instance *Partial_fp2(Instance *self,pArglist arglist,pVM vm)
   { return partialCreate(self,arglist,2,0,0,vm); }

    // f.fpN(n,args) --> f(?1,?2 .. ?n,args),
    // Slightly more special closure
Instance *Partial_fpN(Instance *self,pArglist arglist,pVM vm)
{
   int n = (int)arglistGetInt(arglist,0,".fpN",vm);
   if (n < 0 || n > 10)
      vmThrow(vm,E_ASSERTION_ERROR,
		".fpN: N is out of range (0 .. 10)");
   return partialCreate(self,arglist,n,0,1,vm);
}

    // f.fpM(maskString,args) --> ?
    // g=f.fpM("101",5,6) --> g(7)-->f(5,7,6), g(7,8) --> f(5,7,6,8)
    // fpM("0")==f, fpM("1")==fp1, fpM("11")==fp2
    // 1's in mask are for closed args
    // "" --> fpX == I don't want no steeking args
Instance *Partial_fpM(Instance *self,pArglist arglist,pVM vm)
{
   char     *mask = arglistGetOnlyString(arglist,0,".fpM",vm);
   int 	     n = strlen(mask),chop=0xF;
   unsigned  bitMask = 0;
   Instance *p;

   if (n == 0) chop = 0;	// mask == ""
   else
   {
      char *ptr = strchr(mask,'-');
      int z, z2, chopIt=0;
      if (n > 10)
	 vmThrow(vm,E_ASSERTION_ERROR, ".fpM: mask is too long (max 10)");
      
      if (ptr) { chopIt = 1; n = chop = ptr - mask; }
      for(z2=0, z=n-1; *mask; mask++, z2++,z--)
      {
	 if (*mask == '1') bitMask |= (1<<z);
      }
      if (!bitMask && !chopIt) return self;   // "-", "0-" need to be masked
   }
   p = partialCreate(self,arglist,n,1,1,vm);
   ((Partial *)p)->mask = bitMask;
   ((Partial *)p)->chop = chop;
   return p;
}

   //!!! Op("-").fp(1,(0.0).random.fp(1)) -->
   //    L(2,"Partial",Op(-),L(1,Deferred),0,0,0) -->
   //    (1-(0.0).random.fp(1))
   /* f:=List.fp(1,2); g:=fcn(f){ f(0) }.fp(f); g(0)-->L(1,2,0) not L(1,2)(0)
    *   by definition of fp:  create a call-in-progress.  g(0) adds an arg
    *   and completes the call
    */
__inline static void	// build the arglist
buildClosure(Partial *self,pArglist arglist,Byte *mlist,unsigned mlistSz,pVM vm)
{
   unsigned int n = self->n;

   if (self->useMask) mlistBuildFromMask(mlist,n,self->mask,self->chop,
			       arglist,self->closedOverValues,mlistSz,vm);
   else if (n==0)		// --> f(closedValues,args)
   {
      mlistCopy(mlist,self->closedOverValues,0,mlistSz);
      mlistExtend(mlist,arglist,0,mlistSz);		// dynamic/new args
   }
#if 0
      case 1:		// --> f(arg0,closedValues)
      {
	 Instance *arg0 = arglistGet(arglist,0,"Partial",vm);
	 mlistBuild(mlist,arg0,ZNIL);
	 mlistExtend(mlist,self->closedOverValues,0);
	 break;
      }
#endif
   else
   {
      /* Runtime args fill first n args.  Closure values are next.  Any
       *    left over runtimers are last.
       * f.fp2(3)(5) --> f(5,?,3) --> error
       * f.fp2(3)(5,6) --> f(5,6,3)
       * f.fp2(3)(5,6,7) --> f(5,6,3,7)
       * 
       * f.fp2(3,4)(5) --> f(5,?,3,4) --> error
       * f.fp2(3,4)(5,6) --> f(5,6,3,4)
       * f.fp2(3,4)(5,6,7) --> f(5,6,3,4,7)
       */
      unsigned int len = listLen(arglist,vm);
      if (len<n) _missingArg(arglist,n,".fp",vm);
      mlistCopy(mlist,arglist,0,mlistSz);
      TUPLE_LEN(mlist) = n;		// just fill hole
      mlistExtend(mlist,self->closedOverValues,0,mlistSz);
      if (len>n) mlistExtend(mlist,arglist,n,mlistSz);
   }
}

// Note: calling a Partial can return a PtrInt

    // f([args]) --> f(bound args,arglist) | f(arglist[0],bound args) 
#ifndef _MSC_VER  // VC14 won't export this if inline
__inline 
#endif
Instance *partialCall(Instance *self,pArglist arglist,pVM vm)
{
   Instance *f = ((Partial *)self)->f;
   MLIST(mlist,50);	//!!!!   I think this is protected

   if (!f) return ((Partial *)self)->closedOverValues;	// .set(r)

   buildClosure((Partial *)self,arglist,mlist,50,vm);

#if 0
would be nice to do a tail call if I can
   if (!needResult && (((Partial *)self)->f->objID == fcnID))
   {
      Instance *vmContinue(Instance *f,pArglist,pVM);  // vm.c
      return vmContinue(f,(pArglist)mlist,vm);
   }
#endif
   return objectRun(f,(pArglist)mlist,0,vm);  // Fcn recurses via vmCallFcnFromMethod()
}

    // is this Deferred a Partial *Fcn* (not Method, etc) --> 1/0
int isPartialFcn(Instance *self)	// no PtrInts please!
{
   return (self->iflag2 && (self->objID == deferredID) &&
	   (((Partial *)self)->f->objID == fcnID));
}

static void partialConstruct()
{
   static IBucketHeader _pBuckets;	// might not be used

   ZKL_Object fake;	// A smaller Deferred
   fake       = DeferredObject;
   fake.isize = sizeof(Partial);
   pBuckets   = ibucketHitchHike(&fake,4,105,&_pBuckets,NoVM);
if (sizeof(Partial) > DeferredObject.isize) // Deferred already constructed
      vmThrow(NoVM,E_ASSERTION_ERROR,	// or two threads
              "Partial bigger than Deferred");
}

/* ******************************************************************** */
/* ************************ The Deferred Object *********************** */
/* ******************************************************************** */

    // Deferred.noop() forces evaluation and returns result
    // Deferred.isType(Deferred(f)) returns True and doesn't evaluate
    // NOT thread safe

typedef struct
{
   BInstance  instance;	// iflag==1: Once, iflag2==1: Partial
   Instance  *f;
   Instance  *arglist;	// aka Partial->closedOverValues
   //-----Partial
   Instance  *result;		// of running f
} Deferred;	// 16 bytes, 32 (Linux64)

static Deferred	      vDeferred;	// the "virgin" Deferred
static IBucketHeader *dfBuckets;

static void deferredMarker(Instance *_self)
{
   Deferred *self = (Deferred *)_self;
   if (self->f > (Instance *)1) instanceMark(self->f);
   instanceMark(self->arglist);
   if (!self->instance.iflag2)	// Partials don't have a result
      instanceMark(self->result);
}

#if USE_POINTER_INTS
   void deferredThunk(Instance *self, int64_t i64, pVM vm)
      { ((Deferred *)self)->f = intAllocate(i64,vm); }
#endif

Instance *deferredIsEvaled(Instance *d)	   // NOT PtrInt
{
   if (d->objID == deferredID && !d->iflag2 &&		// !Partial
       ((Deferred *)d)->result && !((Deferred *)d)->f)	// eval'd && !error
      return ((Deferred *)d)->result;
   return 0;
}

static Instance *_deferredEval(Deferred *,int,pVM);

// Note: evaluating a Deferred won't result in a PtrInt

__inline static Instance *deferredEval(Instance *pSelf,int zero,pVM vm)
{
   Deferred *self = (Deferred *)pSelf;
   Instance *result;

   if(self==&vDeferred) return pSelf;

   if (pSelf->iflag2)	// Deferred(fcn{ 4 }.fp(1)).noop()
      return partialCall(pSelf,NoArglist,vm);

   result = self->result;
   if (self->f == (Instance *)1 && result) vmThrowE(vm,result);
   if (result) return result;
   return _deferredEval(self,zero,vm);
}

    // Not thread safe (unless already evaluated)
    // If self throws during eval and try to eval again, be unhappy
static void _backOut(Fence *fence, Instance *e)
{
   Deferred *self = (Deferred *)(fence->i);
   if (e && !self->instance.iflag2)	// Partials need not apply
   {
         // try to be thread safe, even if half assed
      CAP_SET(&self->result,e);
      CAP_SET(&self->f,(Instance *)1);
      self->arglist  = 0;
   }
}
static Instance *_deferredEval(Deferred *self,int n,pVM vm)
{
   Fence     fence;
   Instance *result, *f = self->f;

   if (!f || n++ > 100)
      vmThrow(vm,E_ASSERTION_ERROR,	// or two threads
              "Deadlocked or recursive Deferred (or difficult birth)");

      // won't need fcn after this + deadlock/recursion check
      // var a = Deferred(fcn{b}), b = Deferred(a); a.noop();
      // var c = Deferred(fcn{c}); c.noop();
      // var ff=fcn{4}.fp(1); Deferred(fcn{ff}).noop(); ff()
      // Need to save fcn in case it is GC'd (yes, it happens).
      //   Also need to save result of fcnRun() so use a fence.
      // Result is NOT PtrInt
   vmSetFence(vm,&fence,_backOut,(Instance *)self);
      fence.i1 = f;
      if (!self->instance.iflag2)    // not a Partial, turn off evaluation
	 CAP_SET(&self->f,0);
      result = (f->objID == deferredID) ? deferredEval(f,n,vm) :
				objectRun(f,self->arglist,&fence.i2,vm);
      result = ptrIntThunk(result,vm);	// just in case

	  // Deferred(Deferred(Deferred(f{4}))) --> 4
	  // Deferred(f{Deferred(0,5)}) --> 5
	  // fcn f{Deferred(f)} Deferred(f) --> loop
	  // Deferred(fcn { fcn{4}.fp(1); }).noop(); --> Deferred to Partial
	  //    This also needs to be thunked
      if (result->objID == deferredID)	// need another fence
	 result = _deferredEval((Deferred *)result,n,vm);

      if (!self->instance.iflag2)	// !Partial, ie self->result exists
      {
	 self->arglist = 0;	 // won't be used after this
	 CAP_SET(&self->result,result);		// offically evaluated
      }
   vmRemoveFence(&fence,0);
   return result;
}

    // .set(r) -->self, set result, pretend eval
static Instance *Deferred_set(Instance *self,pArglist arglist,pVM vm)
{
   Deferred *d = (Deferred *)self;
   Instance *r = arglistGet(arglist,0,"Deferred.set",vm);

   if (d != &vDeferred)
   {
      CAP_SET(&d->f,0);	// turn off evaluation
      if (self->iflag2)	// Partial
	 ((Partial *)self)->closedOverValues = r;
      else		// Deferred
      {
	 d->result  = r;
	 d->arglist = NoArglist;
      }
   }
   return self;
}

unsigned baseClassId,fpId,fpMId, toBoolId;  // set in object.c

static Instance *Deferred_BaseClass(Instance *,pArglist,pVM);

static int deferredResolveN(Instance **_self,
unsigned id, pMethod *method, void *pFcn, int bitch, pVM vm)
{
   Deferred *self = (Deferred *)*_self;

   if (self == &vDeferred)	// virgin --> regular resolve
      return objectResolveN(_self,id,method,pFcn,bitch,vm);

   if (id==baseClassId)
   {
      if(method) *method = Deferred_BaseClass;
      else *_self = methodCreate((Instance *)self,0,Deferred_BaseClass,vm);
      return MethodType;
   }

   if (self->instance.iflag2)	// Partial
   {
	// .fp and .fp[12NM] are special as they are object methods
        // If not special cased, in f.fp().fp(), the wrong f is sent to .fp
      if(fpId<=id && id<=fpMId)
         return objectResolveN(_self,id,method,pFcn,bitch,vm);
      *_self = self->f;
      return IRESOLVEN(self->f)(_self,id,method,pFcn,bitch,vm);
   }
   *_self = deferredEval((Instance *)self,0,vm);
   return IRESOLVEN(*_self)(_self,id,method,pFcn,bitch,vm);
}

static int deferredResolve(Instance **self,
char *name, pMethod *method, void *pFcn, int bitch, pVM vm)
{
   unsigned id;

   if(getGlobalId(name,&id,vm))	// name exists
      return deferredResolveN(self,id,method,pFcn,bitch,vm);

	// Object is always the last parent (implicit)
   return objectResolve(self,name,method,pFcn,bitch,vm);
}

Instance *deferredMaybeEval(Instance *_self,unsigned id,pVM vm)
{
   Deferred *self = (Deferred *)_self;

   if (self==&vDeferred || id==baseClassId) return _self;
   if (self->instance.iflag2)	// Partial
   {
	// .fp and .fp[12NM] are special as they are object methods
        // If not special cased, in f.fp().fp(), the wrong f is sent to .fp
      if(fpId<=id && id<=fpMId) return _self;
   }
   return Deferred_value(_self,vm);
}

    // Deferred.create(fcn|class|method|Op|Deferred [,args])
    //      --> new Deferred, only works for "virgin" Deferred
    // .create([,args]) --> Eval
    // Class is handy for things like Deferred(Import,"zklBigNum")
    // If "virgin" Deferred, create a new one
    // otherwise, eval and call 
    //   as in Deferred(5)(666)-->666 or a call from objectRun()
    //   BN = Deferred(Import,"zklBigNum"); n := BN(10)
    // Or, maybe just eval, no call:
    //   konst = Deferred.once(f); n = konst(), konst + 5
static Instance *_createDeferred(pArglist arglist,int if1,int if2,pVM vm)
{
   // self is virgin Deferred --> Deferred.create(fcn)
   Deferred *df;
   Fence     fence;
   Instance *f = arglistGet(arglist,0,"Deferred",vm);

   df = (Deferred *)ibucketAllocate(dfBuckets,&DeferredObject,I_OWNED,1,vm);
   vmSetFence(vm,&fence,0,0);
      df->f = fence.i1 = ptrIntThunk(f,vm);  // just in case Deferred(123,456)
      df->result = 0;

      df->instance.iflag  = if1;
      df->instance.iflag2 = if2;
      df->arglist	  = arglistDup(arglist,1,vm);
      containerIsCooked(dfBuckets,(Instance *)df,I_OWNED);
   vmRemoveFence(&fence,0);
   return (Instance *)df;
}

    // .setArglist(...) --> self
static Instance *Deferred_setArglist(Instance *self,pArglist arglist,pVM vm)
{
   Deferred *d = (Deferred *)self;

//!!! I should probably say no for eval'd Deferreds
   	// ->arglist for Deferred, ->closedOverValues for Partial
   if (d != &vDeferred) d->arglist = arglistDup(arglist,0,vm);
   return self;
}

static Instance *
Deferred_create(Instance *pSelf,pArglist arglist,pVM vm)
{
   Deferred *self = (Deferred *)pSelf;
   Instance *i;

   if (self == &vDeferred)	// virgin --> Deferred.create(fcn)
      return _createDeferred(arglist,0,0,vm);

   if (pSelf->iflag2) return partialCall(pSelf,arglist,vm);

	// (nargs) --> result(args)(nargs) or just eval
   i = deferredEval(pSelf,0,vm);
   if (pSelf->iflag) return i;	// once
   return objectRun(i,arglist,0,vm);
}

    // .defer(self,...), as opposed to self.defer(...). See object.c
Instance *deferredCreate(Instance *f, pArglist arglist, pVM vm)
{
   Deferred *d;
   Fence     fence;

   d = (Deferred *)ibucketAllocate(dfBuckets,&DeferredObject,I_OWNED,1,vm);
   vmSetFence(vm,&fence,0,(Instance *)d);
      d->f	 = ptrIntThunk(f,vm);  // just in case Deferred(123,456)
      d->result  = 0;
      d->arglist = arglistDup(arglist,0,vm);
      containerIsCooked(dfBuckets,(Instance *)d,I_OWNED);
   vmRemoveFence(&fence,0);
   return (Instance *)d;
}

    // DON'T do tail calls, otherwise identical to .create
Instance *deferredCall(Instance *self,pArglist arglist,pVM vm)
{
   Instance *i;

   if (self == (Instance *)&vDeferred)
      return _createDeferred(arglist,0,0,vm);

   if (self->iflag2)		// Partial
      return partialCall(self,arglist,vm);

   i = deferredEval(self,0,vm);
   if (self->iflag) return i;	// once
   return objectRun(i,arglist,0,vm);
}

    // .once: Defer creation of a constant
Instance *Deferred_once(Instance *self,pArglist arglist,pVM vm)
{
   Instance *di = _createDeferred(arglist,1,0,vm);
//   di->iflag = 1;	// once
   return di;
}

    // .partial: Create a Partial, used by the Parser for onExit
static Instance *Deferred_partial(Instance *self,pArglist arglist,pVM vm)
{
   Instance *di;
   int       n = listLen(arglist,vm);

   if (n == 1) return arglistGet(arglist,0,0,vm); // Deferred.partial(f) --> f
   di = _createDeferred(arglist,0,1,vm);
//   di->iflag2 = 1;	// Partial
   return di;
}

    // .toBool() --> True if evaluated
    // This is useful in "if(deferred.BaseClass.toBool()) ..."
    // !! Can't be in the method table
static Instance *Deferred_toBool(Deferred *self,pArglist arglist,pVM vm)
{
   if (self->instance.iflag2)	// Partial, usually True 'cept (0).fp(1)
      return M2_BOOL(self->f,vm);

   return (self->result ? BoolTrue : BoolFalse);
}

    // this is for if(Deferred(...)) ..., ie the Object cache
    // where other methods use the cached toBool method
static Instance *theOther_toBool(Instance *self,pArglist arglist,pVM vm)
{
   if(self->iflag2) self = ((Deferred *)self)->f;
   else		    self = deferredEval(self,0,vm);

   return M2_BOOL(self,vm);
}



#if 0	// not useful
    // .toString() --> "Deferred" or "Partial"
    // This is useful in "if (deferred.BaseClass.toBool()) ..."
    // !! Can't be in the method table
static Instance *Deferred_toString(Deferred *self,pArglist arglist,pVM vm)
{ 
   char *text = (self->instance.iflag2 ? "Partial" : "Deferred");
   return kStringCreate(text,0,I_OWNED,vm);
}
#endif


    // Deferred.BaseClass(name)
static Instance *Deferred_BaseClass(Instance *self,pArglist arglist,pVM vm)
{
   char *name = arglistGetOnlyString(arglist,0,"BaseClass",vm);
   unsigned id;
   if(getGlobalId(name,&id,vm))
   {
      if(id==toBoolId)
	 return methodCreate(self,0,(pMethod)Deferred_toBool,vm);
   }
   objectResolve(&self,name,0,0,1,vm);
   return self;
}

static const MethodTable deferredMethods[] = 
{
   "create",	 Deferred_create,
   "once",	 Deferred_once,
   "partial",	 Deferred_partial,
   "set",	 Deferred_set,
   "setArglist", Deferred_setArglist,
//   "toBool",	 (pMethod)Deferred_toBool,	// this don't work here
   "BaseClass",  Deferred_BaseClass,

   0,		0
};


//////////////////////////////// Deferred Properties
// .resolve() takes care of Properties

    // Deferred[.BaseClass].parts --> 
    //   T(iflags,"Deferred[.once]", f, result)
    //   T(iflags,"Partial", f, result,arglist,n,mask,chop)
//!!! would be nice to follow any Deferred's all the way down
Instance *Deferred_parts(Deferred *self,pVM vm)
{
   char     *t;
   Instance *f = self->f, *arglist=self->arglist, *r;
   int	     flags = self->instance.iflag2*2 + self->instance.iflag; 

   if (f < (Instance *)2) f       = Void;
   if (!arglist)	  arglist = emptyTuple;
   if (self->instance.iflag2)			// Partial
      return tupleCreateX(vm,intCreate(flags,vm),
         kStringCreate("Partial",0,I_OWNED,vm),f,arglist,
	 intCreate(((Partial *)self)->n,vm),
	 intCreate(((Partial *)self)->mask,vm),
	 intCreate(((Partial *)self)->chop,vm),
	 ZNIL);

   t = (self->instance.iflag) ? "Deferred.once" : "Deferred";
   r = self->result; if (!r) r = Void;	// or being evaluated
   return tupleCreateX(vm,intCreate(flags,vm),
         kStringCreate(t,0,I_OWNED,vm),f,arglist,r,ZNIL);
}

    // Deferred[.BaseClass].f --> Fcn | Method | Void
Instance *Deferred_f(Instance *self,pVM vm)
{
   Instance *f = ((Deferred *)self)->f;

   if (f < (Instance *)2) f = Void;
   return f;
}

Instance *Deferred_arglist(Deferred *self,pVM vm)
{
   Instance *arglist = self->arglist;
   if (!arglist) arglist = emptyTuple;
   return arglist;
}

    // Deferred[.BaseClass].value --> eval
Instance *Deferred_value(Instance *self,pVM vm)
{
   if (self->iflag2) return ((Deferred *)self)->f;	// Partial
   return deferredEval(self,0,vm);
}

    // Deferred[.BaseClass].isCooked -->Bool
    // Partial.BaseClass.isCooked    --> False
Instance *Deferred_isCooked(Instance *self,pVM vm)
{
   if (self->iflag2 || ((Deferred *)self)->result) return BoolTrue;
   return BoolFalse;
}

    // Deferred[.BaseClass].isPartial -->Bool
Instance *Deferred_isPartial(Instance *self,pVM vm)
{
   if (self->iflag2) return BoolTrue;
   return BoolFalse;
}

static const PropertyTable deferredProperties[] = 
{
   "parts",	(pProperty)Deferred_parts,
   "f",		(pProperty)Deferred_f,
   "arglist",	(pProperty)Deferred_arglist,
   "value",	Deferred_value,
   "isCooked",	Deferred_isCooked,
   "isPartial",	Deferred_isPartial,
   0,		0
};


	///////////////////////////////////////////////////
	//////////////// Deferred Op Codes ////////////////

extern const OpcodeTable Object_OpcodeTable[];	// object.c

static __inline Instance *deferredOp(Instance *self,int op,Instance *X, pVM vm)
{
   Instance *result = self->iflag2 ? 
       partialCall(self,NoArglist,vm) : deferredEval(self,0,vm);

   #if USE_POINTER_INTS
      if (IS_PtrInt(result))
      {
	 ZKL_Int tmp;
      	 PtrInt_TO_Int64(result,&tmp); result = (Instance *)&tmp;
	 return I_OP(result,op)(result,X,vm);	// better not store tmp
      }
   #endif

   if(result==(Instance *)&vDeferred)	// eg Deferred==Deferred
      return Object_OpcodeTable[op].op(result,X,vm);
   return I_OP(result,op)(result,X,vm);	// which may recurse
}

static Instance *Deferred_eq(Instance *self,Instance *X,pVM vm)
   { return deferredOp(self,OP_EQ,X,vm); }

static Instance *Deferred_neq(Instance *self,Instance *X,pVM vm)
   { return deferredOp(self,OP_NEQ,X,vm); }

static Instance *Deferred_lt(Instance *self,Instance *X,pVM vm)
   { return deferredOp(self,OP_LT,X,vm); }

static Instance *Deferred_lte(Instance *self,Instance *X,pVM vm)
   { return deferredOp(self,OP_LTE,X,vm); }

static Instance *Deferred_gt(Instance *self,Instance *X,pVM vm)
   { return deferredOp(self,OP_GT,X,vm); }

static Instance *Deferred_gte(Instance *self,Instance *X,pVM vm)
   { return deferredOp(self,OP_GTE,X,vm); }

static Instance *Deferred_negate(Instance *self,Instance *X,pVM vm)
   { return deferredOp(self,OP_NEGATE,X,vm); }

static Instance *Deferred_add(Instance *self,Instance *X,pVM vm)
   { return deferredOp(self,OP_ADD,X,vm); }

static Instance *Deferred_sub(Instance *self,Instance *X,pVM vm)
   { return deferredOp(self,OP_SUB,X,vm); }

static Instance *Deferred_mul(Instance *self,Instance *X,pVM vm)
   { return deferredOp(self,OP_MUL,X,vm); }

static Instance *Deferred_div(Instance *self,Instance *X,pVM vm)
   { return deferredOp(self,OP_DIV,X,vm); }

static Instance *Deferred_mod(Instance *self,Instance *X,pVM vm)
   { return deferredOp(self,OP_MOD,X,vm); }


static const OpcodeTable deferredOps[] = 
{
   OP_EQ,	(pOp)Deferred_eq,
   OP_NEQ,	(pOp)Deferred_neq,
   OP_LT,	(pOp)Deferred_lt,
   OP_LTE,	(pOp)Deferred_lte,
   OP_GT,	(pOp)Deferred_gt,
   OP_GTE,	(pOp)Deferred_gte,

   OP_NEGATE,	(pOp)Deferred_negate,

   OP_ADD,	(pOp)Deferred_add,
   OP_SUB,	(pOp)Deferred_sub,
   OP_MUL,	(pOp)Deferred_mul,
   OP_DIV,	(pOp)Deferred_div,
   OP_MOD,	(pOp)Deferred_mod,

   0,		0
};


//static pMethod in_deferred_methods(Instance *ignore, register char *str);

static void deferredConstruct()
{
   static IBucketHeader _dfBuckets;	// might not be used

   constructObject(&DeferredObject,DeferredType,
		   deferredMethods,deferredProperties,deferredOps,NoVM);
   DeferredObject.magicMarker  = deferredMarker;
   DeferredObject.resolve      = deferredResolve;
   DeferredObject.resolveN     = deferredResolveN;
   DeferredObject.isize	       = sizeof(Deferred);
   DeferredObject.isBInstance  = 1;

   DeferredObject.mcache[TO_BOOL] = theOther_toBool;

   deferredID = DeferredObject.id;

   dfBuckets = ibucketHitchHike(&DeferredObject,4,108,&_dfBuckets,NoVM);

   partialConstruct();

   instanceInit((Instance *)&vDeferred,&DeferredObject,I_UNTOUCHABLE);
   // zeroed (as it is static)
   vaultAdd(0,(Instance *)&vDeferred,NoVM);
}

/* ******************************************************************** */
/* ******************************************************************** */

void methodConstruct(void)
{
   constructObject(&MethodObject,MethodType, mMethods,mProperties,0,NoVM);
   MethodObject.magicMarker = methodMarker;
   MethodObject.isize	    = sizeof(Method);
   MethodObject.isBInstance = 1;
   ibucketReserve(&MethodObject,5038,&mbuckets,0,NoVM);

   methodID = MethodObject.id;

   constructObject(&PropertyObject,PropertyType, proMethods,proProperties,0,NoVM);
   PropertyObject.name	      = "Property";
   PropertyObject.magicMarker = propertyMarker;
   PropertyObject.isize	      = sizeof(Property);

   deferredConstruct(); opConstruct();
}





///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
// zkl extractTable -n deferredMethods <method.c |gperf| zkl gperf -i deferred


