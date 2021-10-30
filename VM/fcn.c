/* fcn.c : the Fcn Object
 * 
 * Copyright (c) 2006,2007,2008-12,2013 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

/* Staticness: No copies are made of fcn, ever. Constructor and init are
 * never static (unless the class is).
 * Two types:
 *   1) No references to container and fcn->container == Void or NullClass.
 *     Doesn't need a class.  VM has no idea of self.  Nice because doesn't
 *     force a class to stick around.  Stack traces work. This model doesn't
 *     work because fcns are embedded in a class so if the class goes away,
 *     so do these fcns, even if fcn is in, eg, a register.
 *   2) No references to vars, refs to container go to Eve class. All fcns in
 *     a static class are this type of static. Eve is forever, but fcn
 *     detached from container lets that class be gc'd.
 * Kinda hard to determine: Some vars refs are OK (const) ...
 * If a class is static, all fcns in that class are static and class takes
 *   care of that case.
 * A static function can't reference: 
 *   - vars
 *   - non static classes/parents
 *     class C { fcn [static] f { C } }  C().f() --> wrong C
 *   - fcns in self (unless self.fcn/recursion)
 *     class C { fcn [static] f { g() } fcn g { C } }  C().f() --> wrong C
 *     class C { fcn f { g() } fcn g {} } f is static because g is static
 *       Both are in C Eve and mark Eve so no gc issues.
 *   - self: OK, as it is f.instance == Eve
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define __NOT_A_DLL
#define __FCN_INTERNALS
#define __CLASS_INTERNALS
#define __GC_INTERNALS		// mark macros
#define __LIST_INTERNALS
#define __STRING_INTERNALS

#include "zklObject.h"
#include "zklFcn.h"
#include "zklClass.h"
#include "zklData.h"
#include "zklList.h"
#include "zklMethod.h"
#include "zklNumber.h"
#include "zklPCBlock.h"
#include "zklString.h"

static size_t baseCount = 0;	// stats

int fcnID;

Instance *nullFcn = 0,	// contained in NullClass but created after NullClass
	 *idFcn   = 0;	// idFcn(x) --> x

static IBucketHeader fnBuckets;

/* The "same" fcn can exist in three places:
 *   - Standalone, for example, a run time helper fcn or anonymous fcn
 *   - The orginal class it was defined in
 *   - "Reparented" (ie copied) as part of another class tree (inherited)
 *   In all these cases, the only difference is the id & class (container).
 */

ZKL_Object FcnObject, FcnBaseObject;

#if GC_SANITY_CHECK && 0
   void fcnVerify(Instance *self)
   {
      instanceVerify(self,0);
      instanceVerify((Instance *)FCN_BASE(self),0);
   }
#endif

	////////////////////////////////////// GC for Fcn Bases

    /* Asm.Code is freed elsewhere, ditto default args, prototype
     * Note: A PC can point to Asm.Code and be on the stack (as a return
     * address), even if I'm freeing this FcnBase (detached embryos).
     */
static int fcnBaseFree(Instance *self)
{
   baseCount--;
   return 1;
}

#if 0
uggg. for this to work, Class would have to track class copies so the refCount
tracks
static int fcnFree(Instance *self)
{
   FcnBase *base = FCN_BASE(self);
   if (0 == CAI_DEC(base->refCount)) { fcnBaseFree(base); ZFREE(base); }
   return 1;
}
#endif

	///////////////////////////////////////
    /* A static (or newly built) Fcn might not be in a Class.  If so, its
     *   container is the NullClass, an I_UNTOUCHABLE
     * If a fcn is in a class, GC_TYPE is I_2SPECIAL
     * I_IMMORTAL containers are common (which means the container will mark
     *    the fcn and fcn can't be truely static).
     */
static void fcnMarker(Instance *_self)
{
   ZKL_Fcn  *self      = (ZKL_Fcn *)_self;
   FcnBase  *fb	       = self->fcnBase;
   Instance *container = self->container;
   int	     ct        = GC_TYPE(container);

   if (ct == I_IMMORTAL) return;

   GC_MARK((Instance *)fb);	// save a function call
   if (ct != I_UNTOUCHABLE)	// eg NullClass if I'm static
      instanceMark(container);
}

char *fcnName(Instance *self)
{
   if (!self || (TYPEO(self) != FcnType)) return "";
   return FCN_NAME(self);
}

    /* Create default args if args are missing
     * Pain in the ass note:  This can span several GC cycles if there are a
     *    lot of default args or if they are nasty (eg fcn f(n=ask("n=")){}).
     *    Also, vmCreate can stall during GC, as will vmRun.  So, I have to
     *    protect arglist if I create a new one.  I also have to worry about
     *    exceptions being thrown.
     * If arglist is incomplete, a new one is created and filled in with
     *   defaults.
     * A new arglist may or may not be created, the returned arglist might
     *   be the orginal, a MList or a copy (which will be orphan/owned).
     * You need to protect the orginal arglist, and be prepared to mark a
     *   new arglist "in progress".
     * Class context: The default arg is run with self as the fcn container.
     *   ie a default arg has the same container as the fcn it is in.
     * The defaults live in FcnBase so self.container (ie vm->self) doesn't
     *   see them. But self does. So, when vm marks, vm->self will mark self.
     */
    /* Caller needs to mark arglist
     * Notes:
     *   - Don't call fcnPrepForRun(), even if it is the obvious thing to
     *     do, it uses the wrong Code.
     *   - If vmRun() throws, it frees the VM.
     *   - I can reuse one VM (basically as a fiber) to run multiple chunks
     *     of code because, if the code runs, the VM is essentially reset.
     *     If the code doesn't run, it will throw (see above).
     *     This is a win (vs vmCreate/vmFree) because it minimizes VM locking.
     */
extern Instance *vmCallCodeFromMethod(ZKL_Code *,Instance *klass,pArglist,pVM);

#if 0  // I can't measure any diff between these three
int finalizeArglist2(Instance *fcn, Instance *arglist, int numParams, pVM vm)
{
   FcnBase  *base = FCN_BASE(fcn);
   ZKL_Code *defaultArgs = base->defaultArgs;
   int	     i;
   int	     numDefaults = base->numDefaults, firstDefault = base->d1;
   pVM	     aVM = 0;

   if (!numDefaults) return numParams;
   if ((i = (numParams - firstDefault)) < 0) return numParams;
   for (; i < numDefaults; i++)
   {
      ZKL_Code	*code = &defaultArgs[i];
      Instance	*r;
      ZKL_Block *block;
      Byte	*addr, op;

      addr = code->code;
      if (!addr) break;  // no default or end of defaults
      op = *addr;

      	// Pick some low hanging fruit and avoid running code (slow!)
      if (op == opGetKString && addr[2] == opDone)
      {
	 Instance *s = (Instance *)&((ZKL_KCString *)code->kstrings)[addr[1]];
	 listAppend(arglist,s,vm);
	 continue; 
      }
      if (addr[1] == opDone)
      {
	 r = 0;
	 switch(op)
	 {
	    case opVoid:   r = Void;		break;
	    case opFalse:  r = BoolFalse;	break;
	    case opTrue:   r = BoolTrue;	break;
	    case opZero:   r = Zero;		break;
	    case opOne:    r = One;		break;
	    case opNoText: r = emptyString;	break;
	    case opStar:   r = Star;		break;
	 }
	 if (r) { listAppend(arglist,r,vm); continue; }
      }
      else if (op == opIntB && addr[2] == opDone)
	 { listAppend(arglist,INT_CREATE(addr[1],vm),vm); continue; }
      else if (op == opIntW && addr[3] == opDone)
      {
	 int n; addr++;
	 listAppend(arglist,INT_CREATE(ADDR_GETW(addr,n),vm),vm); continue; 
      }
      else if (op == opVCache && addr[2] == opDone)
	 { listAppend(arglist,vcGet(addr[1],vm),vm); continue; }

#if 1
      if (!aVM) aVM   = vmCreate(vm,&block,0x0,vm);
      else	block = vmBlockCreate(aVM);
      blockReset(block,fcn,aVM);	// default.container == fcn.container
      pcInit(aVM,code,0);
      r = vmRun(aVM,0);			// throws
#else
      r = vmCallCodeFromMethod(code,FCN_CONTAINER(fcn),NoArglist,vm);
#endif
      listAppend(arglist,r,vm);		// throws
   }
   if (aVM) vmFree(aVM);
   return i + firstDefault;	// total number of args in arglist
}

#else
int finalizeArglist2(Instance *fcn, Instance *arglist, int numParams, pVM vm)
{
   FcnBase  *base = FCN_BASE(fcn);
   ZKL_Code *defaultArgs = base->defaultArgs;
   int	     i;
   int	     numDefaults = base->numDefaults, firstDefault = base->d1;
   Instance *klass = FCN_CONTAINER(fcn);
   Fence     fence;

   if (!numDefaults) return numParams;
   if ((i = (numParams - firstDefault)) < 0) return numParams;
vmSetFence(vm,&fence,0,klass);
   for (; i < numDefaults; i++)
   {
      ZKL_Code	*code = &defaultArgs[i];
      Instance	*r;
      Byte	*addr;

      addr = code->code;
      if (!addr) break;  // no default or end of defaults

      r = vmCallCodeFromMethod(code,klass,NoArglist,vm);
      listAppend(arglist,r,vm);
   }
vmRemoveFence(&fence,0);
   return i + firstDefault;	// total number of args in arglist
}
#endif

#if 0
int fcnHoldsCode(Instance *self,ZKL_Code *code)
{
   return CODE(self) == code;
}
#endif

/* ******************************************************************** */
/* ************************** Building a Fcn ************************** */
/* ******************************************************************** */

    /* Make a copy of a function that is in a class (ie don't copy an embryo).
     * Multiple threads will hit this for the same function.
     * There is only one copy of fcn f for all instances of class C (in
     *   ClassBase and all instances of C point to that), so if you need an
     *   f for another instance of C, you need to make a copy with a correct
     *   container pointer.
     *   Since there are no copies of static classes (only the Eve class
     *   exists), all of it's fcns have the correct container (Eve).
     * If class is static, return self (self.container == container == Eve)
     * iflag  == 1 --> runnable
     * iflag2 == 1 --> bound to class
     */
Instance *fcnCopy(Instance *self,Instance *newContainer,pVM vm)
{
   ZKL_Fcn  *newFcn;
   FcnBase  *base;
   Instance *container;

   if (!self || !newContainer || !FCN_IS_RUNNABLE(self))  // cheap checks
   {
      char buf[200];
      sprintf(buf,"Fcn(%s).copy: A zero is no hero",FCN_NAME(self));
      vmThrow(vm,E_ASSERTION_ERROR,buf);
   }

   base      = FCN_BASE(self);
   container = FCN_CONTAINER(self);

   // DO NOT short cut if containers match, class.fcns needs to make copies
   // so that GC works. Unless Class resets marks. Or other magic. Does now.
   // Also catches static classes
   if (container == newContainer) return self;		// common

   // If fcn is static, it may or may not be embedded in a class.
   // If embedded, have to allocate, I don't know where the orginal static fcn
   //   is (or even if it is), class may vaporize, taking fcn bits with it.
   if (container == NullClass)    // "free floating" static fcn, really rare
      return self;
   if (base->isStatic) newContainer = NullClass;

   newFcn = (ZKL_Fcn *)ibucketAllocate(&fnBuckets,&FcnObject,I_OWNED,1,vm);
   newFcn->fcnBase	  = base;
   newFcn->container	  = newContainer;
   newFcn->instance.iflag = 1;		// make runnable, user can't do this
   return containerIsCooked(&fnBuckets,(Instance *)newFcn,I_OWNED);
}

    /* DON'T provide a method to copy or reparent a function.  This just
     * doesn't work because there is no fcnSelf:  the compiler references
     * fcn vars via fcnN(n)/setVarI(n), which is relative to the current VM
     * class instance and might not be the right fcn (won't be in the case
     * of a copy).  A fcn needs to remain intimate with its class instance.
     * UNLESS it doesn't refence its class, in which case, the parent can be
     * the NullClass.  All that said, the nullFcn is nice as a constructor
     * because many classes don't need to be constructed and use init to do
     * whatever it is they need to get things ready to go.  A constructor is
     * needed because it has to return self if init doesn't exist (which
     * means the nullFcn has be reparented to the class).  By using copies
     * of the nullFcn, the compiler can reduce code size by not calling null
     * constructors).
     */
    // .copy() -->self
//Instance *Fcn_copy(Instance *self,Instance *arglist,pVM vm) { return self; }

    // --> size of all bytes need by fcn (code, string space, etc)
static size_t verifyAsmDotCode(Instance *asmDotCode, unsigned *mapLen, pVM vm)
{
   size_t    n,size;
   Byte	    *ptr;
   char	     buf[100];
   char	    *name    = className(asmDotCode);
   Instance *theCode = classFindVar(asmDotCode,"code",0,vm); // OK if !class
   Instance *strings = classFindVar(asmDotCode,"strings",0,vm);
   Instance *map     = classFindVar(asmDotCode,"map",0,vm);

	/* Do some light checking of the Class.  I could tighten this up by
	 * changing Asm.Code to [private] or using
	 * classFindClass()/classIsInstanceOf() or Class_container() but if
	 * somebone wants to insert malicious code they still can (by
	 * reusing a Code from another fcn).  Even if I changed to [private]
	 * and made the vars ROData, you could still Asm evil code.  So,
	 * make it reasonable because I can't keep out bad code.
	 */
   if (TYPEO(asmDotCode) != ClassType || !theCode || !strings || !map ||
       (TYPEO(theCode) != DataType) || 
       (TYPEO(strings) != DataType) ||
       (TYPEO(map)     != DataType))
      vmThrow(vm,E_ASSERTION_ERROR,"Bogus Asm.Code class");

       //!!! would be better to use isInstanceOf
   if (0 != strcmp("Code",name) && 0 != strcmp("__Code#",name))
   {
      sprintf(buf,"Not Asm.Code class: %s",iname(asmDotCode));
      vmThrow(vm,E_ASSERTION_ERROR,buf);
   }

   size = 0;
   dataText(theCode,&n); size += n;
   if (n > 0xffff)
 tooBig: vmThrow(vm,E_ASSERTION_ERROR,"Asm.Code: something too big");
   dataText(strings,&n);   size += n; if (n > 0xffff) goto tooBig;
   ptr = dataText(map,&n); size += n;	// n is supposed to be >0
   if (!n || (unsigned)(*ptr + 1) > n)  // map[0] == num entries in map
      goto tooBig;
   *mapLen = *ptr;			// num kstrings
   return size;
}

void fcnLink(ZKL_Code *zklCode, pVM);	// in object.c
//static void fcnLink(ZKL_Code *zklCode, pVM);

    // Copy a Asm.Code to ptr (which is at the end of a FcnBase)
    // verifyAsmDotCode() has already been called
    // This isn't used for static code (eg loaded from zsc or wad)
    // All kstrings (for code and default arg code) are in kstrings[]
static Byte *codeInitFromAsmDotCode(ZKL_Code *code,
   Instance *asmDotCode,Byte *ptr, ZKL_KCString **table,Instance *fb, pVM vm)
{
   Instance *theCode = classFindVar(asmDotCode,"code",   0,vm);
   Instance *strings = classFindVar(asmDotCode,"strings",0,vm);
   Instance *map     = classFindVar(asmDotCode,"map",    0,vm);
   Byte     *bits;
   size_t    z;
   ZKL_KCString *kstrings = *table;  // we use the same array for all kstrings

   bits = dataText(theCode,&z);
   code->code = ptr; code->codeSize = (uint16_t)z;	// actually 24 bits
   memcpy(ptr,bits,z); ptr += z;

   bits = dataText(strings,&z);
   code->strings = (char *)ptr; code->stringSize = (uint16_t)z;
   memcpy(ptr,bits,z); ptr += z;

   bits = dataText(map,&z); code->map = ptr; code->mapSize = (uint16_t)z;
   memcpy(ptr,bits,z); ptr += z;

   code->kstrings = (Instance *)
      buildKCStringTable(code->strings,code->stringSize,fb,code->map,
		      kstrings);
   *table = kstrings + code->map[0];	// point after this kstring[] chunk

   fcnLink(code,vm);

   return ptr;		// next Asm.Code goes here
}

    // Everything but Asm.Code
    // XXXNew function AND base are INVISIBLE
    // New function is <your type> (needs to be cooked), base is INVISIBLE
    // Note: Until the embryo is added to a class it is effectively static.
    // sizeOfNames includes the amount of space needed for code bits (if any).
static Instance *_fcnEmbryo(
   size_t sizeOfNames, int numArgs,int numDefaults, unsigned numKStrings, 
   Byte **kstrings, int fitype, pVM vm)
{
   ZKL_Fcn  *fcn;
   FcnBase  *base;
   uint32_t  size, kstringOffset;

      // Memory layout for FcnBase (sizeof(Code) == 24 on Intel/Windows):
      // namePtr .... defaultArgs[0]  (numArgs == 0)
      //	name planted in defaultArgs[0],prototype,bits
      // namePtr .... defaultArgs[0] ... [numArgs - 1]
      //		name,prototype,<main Code bits><default code bits>
   size = sizeof(FcnBase) + numDefaults*sizeof(ZKL_Code);
   kstringOffset = size;
   size += (numKStrings * sizeof(ZKL_KCString) + sizeOfNames);
//???? I think a refcounted blob works better, classFree would need to count--
   base = (FcnBase *)instanceAllocate(size,&FcnBaseObject,1,vm);
   *kstrings = ((Byte *)base + kstringOffset);
   #if 0 && USE_POINTER_INTS  // don't confuse kstrings with a PtrInt!
      if (IS_PtrInt(*kstrings))
	 vmThrow(vm,E_VM_ERROR,"_fcnEmbryo bad alignment, adjust padding");
   #endif

   // default args are zero'd

   	// prototype, code, etc have been zeroed (by instanceAllocate)
   base->numArgs     = numArgs;
   base->size	     = (uint32_t)size;	// informational only
   base->isPrivate   = 0;
   base->codeIsHere  = 0;
   base->numDefaults = numDefaults;
   base->d1	     = 0;	// which may well be wrong

   fcn = (ZKL_Fcn *)ibucketAllocate(&fnBuckets,&FcnObject,fitype,0,vm);
   if (!fcn)
   {
      instanceFree(base);
      vmThrow(vm,E_OUT_OF_MEMORY,0);
   }
   fcn->fcnBase	  = base;
   fcn->container = NullClass;

   baseCount++;

   return (Instance *)fcn;
}

    // Code is NEVER marked so it better not be freed
    // Make sure you add all defaults!
    // This isn't GC safe
Instance *fcnEmbryoWithStaticCode(
	StringTable *names, int numArgs, int numDefaults,
	ZKL_Code *code, ZKL_Code *defaults, int itype, pVM vm)
{
   ZKL_Code *fcnCode;
   FcnBase  *fcnBase;
   Instance *fcn;
   Byte	    *ptr, *kstrings;
   int	     n, d1;
   unsigned  numKStrings = code->map[0];
   ZKL_KCString *ks;

   if (names->n != (numArgs + 1))
      vmThrow(vm,E_VM_ERROR,"fcnEmbryoWithStaticCode: Name table has wrong length");

   d1 = numArgs - numDefaults;
   if (numDefaults)
      for (n = 0; n < numDefaults; n++) numKStrings += defaults[n].map[0];

   fcn	   = _fcnEmbryo(names->size,numArgs,numDefaults,numKStrings,&kstrings,itype,vm);
   fcnBase = FCN_BASE(fcn);
   ptr	   = kstrings + (numKStrings * sizeof(ZKL_KCString));
   fcnBase->nameo = ptr - (Byte *)fcnBase;
   fcnBase->d1    = d1;
   memcpy(ptr,names->strings,names->size);	// baseEnd == ptr + names->size

   fcnCode	     = FCN_CODE(fcn);
   *fcnCode	     = *code;  // struct copy, pts into wad (code, strings, map)
   ks		     = (ZKL_KCString *)kstrings;
   fcnCode->kstrings = (Instance *)buildKCStringTable(
		fcnCode->strings,fcnCode->stringSize,0,fcnCode->map,ks);
   ks += code->map[0];	// kstrings[numKStrings]

   fcnLink(code,vm);

   if (numDefaults)
      for (n = 0; n < numDefaults; n++)
      {
	 ZKL_Code *srcDA, *dstDA;

	 srcDA		 = &defaults[n];
	 dstDA		 = &FCN_DEFAULT_ARGS(fcn)[n];
	 *dstDA		 = *srcDA;	// struct copy
	 // if dead default, code==0, map=='\0'
         if(!srcDA->code) continue;	// fcn f(a,b=4,c), c isn't there, zero
	 dstDA->kstrings = (Instance *)
		buildKCStringTable(dstDA->strings,dstDA->stringSize,0,
		       dstDA->map, ks);
	 fcnLink(dstDA,vm);
	 ks += dstDA->map[0];
      }

   // cook fcn
   {
      Fence fence;
      containerIsCooked(&fnBuckets,fcn,itype);
      vmSetFence(vm,&fence,0,fcn);
         addToCollectables((Instance *)fcnBase,I_OWNED,vm);
      vmRemoveFence(&fence,0);
   }
   return fcn;
}

    // .build(names,Asm.Code, defaults=Void,private=False)
    // names: L(fcnName,prototype), eg L("foo","a","b","c")
    // defaults is a list of Asm.Code
    // Args are copied, no orphans
    // Asm determines whether or not a chuck of code is static
//!!! memoize static fcns(code,defaults): hash code length?
//WeakRefs might be nice for this
static Instance *Fcn_build(Instance *self,Instance *arglist,pVM vm)
{
   Instance *names	= arglistGet(	      arglist,0,"Fcn.embryo",vm);
   Instance *asmDotCode	= arglistGet(	      arglist,1,"Fcn.embryo",vm);
   Instance *defaults	= arglistTryToGet(    arglist,2);
   int	     isPrivate	= arglistTryToGetBool(arglist,3,0,"Fcn.embryo",vm);

   Byte	    *ptr, *kstrings, *baseEnd;
   FcnBase  *fcnBase;
   Instance *fcn;
   unsigned  size, numKStrings;
   unsigned  numArgs,i, numDefaults = 0, firstDefault = 0;
   int	     allDefaultsAreStatic = 1;

   verifyList("fcn.embryo",names,StringType,vm);
   numArgs = (int)listLen(names,vm) - 1;
   if (numArgs > 250)	// 0 --> name, no prototype
      vmThrow(vm,E_ASSERTION_ERROR,"fcn.embryo: Names list: bad length");
   size = _sumList(names);
   size += verifyAsmDotCode(asmDotCode,&numKStrings,vm);
   if (defaults)	// get size, number of and first default
   {
      if (defaults == Void) defaults = 0;
      else
      {
	 int t = TYPEO(defaults);
	 if ((t != ListType && t != TupleType) || 
	     listLen(defaults,vm) > numArgs)
	    vmThrow(vm,E_ASSERTION_ERROR,
	        "fcn.embryo: List of defaults is wrong");

         i = 0;
	 while((fcn = listGet(defaults,i++)))
	    if (fcn != Void)	// then it is an Asm.Code
	    {
	       unsigned n;
	       size += verifyAsmDotCode(fcn,&n,vm);
	       numKStrings += n;
	       if (!firstDefault) firstDefault = i;	// one based
	       if (allDefaultsAreStatic)	// findVar() isn't fast
		  allDefaultsAreStatic &=	// look for Asm.Code.isStatic
			(BoolTrue == classFindVar(fcn,"isStatic",0,vm));
	    }
	 if (firstDefault) numDefaults = numArgs - --firstDefault;
	 else defaults = 0;	// all Void --> no defaults
      }
   }

   fcn = _fcnEmbryo(size,numArgs,numDefaults,numKStrings,&kstrings,I_OWNED,vm);
   	// now copy names after the defaultArgs array
   fcnBase = FCN_BASE(fcn); baseEnd = (Byte *)fcnBase + fcnBase->size;
   ptr     = kstrings + (numKStrings * sizeof(ZKL_KCString));
   fcnBase->nameo = ptr - (Byte *)fcnBase;
   {
      int	i = 0;
      Instance *name;

      while((name = listGet(names,i++)))
	 ptr += strlen(strcpy((char *)ptr,stringText(name))) + 1;
   }
   fcnBase->isPrivate  = isPrivate;
   fcnBase->codeIsHere = 1;
   fcnBase->d1	       = firstDefault;
   fcnBase->isStatic   = allDefaultsAreStatic &&
		   (BoolTrue == classFindVar(asmDotCode,"isStatic",0,vm));

   if (fcnBase->isStatic) fcn->iflag = 1;  // static fcns are always runnable

   	// copy code & default arg code to base after names
   ptr = codeInitFromAsmDotCode(FCN_CODE(fcn),asmDotCode,ptr,
			(ZKL_KCString **)&kstrings,(Instance *)fcnBase,vm);
   if (defaults)
   {
      // Two areas at the end of the fcn base:
      //   ZKL_Code defaultArgs[numDefaults] & 
      //   Code bits (packed to ignore Void defaults) (ptr)
      Instance *df;
      int	n = 0;
      for (i = firstDefault; (df = listGet(defaults,i)); i++)
      {
	 ZKL_Code *defaultArg = &fcnBase->defaultArgs[n++];

	 // fill in ZKL_Code & copy bits (if !Void)
	 if (df == Void)   // zero'd by _fcnEmbryo()
{}//	    memset(defaultArg,0,sizeof(ZKL_Code));   // zero the default arg
	 else ptr = codeInitFromAsmDotCode(defaultArg,df,ptr,
			(ZKL_KCString **)&kstrings,(Instance *)fcnBase,vm);
      }
   }

   if(ptr!=baseEnd) vmThrow(vm,E_VM_ERROR,"Fcn base not filled in properly");

   {
      Fence fence;
      containerIsCooked(&fnBuckets,fcn,I_OWNED);
      vmSetFence(vm,&fence,0,fcn);
         addToCollectables((Instance *)fcnBase,I_OWNED,vm);
      vmRemoveFence(&fence,0);
   }

   return fcn;
}

#if 0
static void fcnLink(ZKL_Code *zklCode, pVM vm)
{
   char     *names, *ptr;
   Byte     *fixups, *code = zklCode->code, *map = zklCode->map;
   Byte     *end = map + zklCode->mapSize;
   unsigned  N;
   
   /* map layout:
    *   <byte len><bytes of map>	// always at least 1 byte
    *   may stop here
    *   <two byte index of gnames into strings>
    *   <two byte num gnmes>
    *   <bytes of link fixup addresses>: <n><addr>...<n><addr>...
    *      n is the number of addresses for this gname
    *         1 or 2 bytes: if high bit of first byte is 1, two bytes.
    *      same order as names list, addrs are 2 bytes
    *      2 bytes for num addrs because of ".testSrc" in Tests (>600)
    *   All numbers are big endian
    */
   map += map[0] + 1;		// map[0] (== num kstrings) always exists
   if(map >= end) return;	// no link table
   names  = zklCode->strings + (*map<<8 | map[1]); map+=2;
   N      = *map<<8 | map[1];			   map+=2;
   fixups = map;

   // add to global name table and update code
   for(ptr=names; N--; ptr+=(strlen(ptr) + 1)) 
   {
      unsigned idx = addNametoGlobalTable(ptr,vm);
      unsigned cnt = *fixups++;
      if(cnt & 0x80) cnt = ((cnt & 0x7f)<<8 | *fixups++);

      while(cnt--)
      {
	 unsigned addr = (fixups[0]<<8 | fixups[1]);
	 code[addr] = idx>>8; code[addr+1] = idx & 0xff;
	 fixups+=2;
      }
   }
   if(fixups!=end) vmThrow(vm,E_VM_ERROR,"Ran off end of map");
}
#endif

/* ******************************************************************** */
/* ************************** Running a Fcn *************************** */
/* ******************************************************************** */

void fcnPrepForRun(Instance *fcn, ZKL_Block *block, pVM vm)
{
   ZKL_Code *code = FCN_CODE(fcn);

   if (!FCN_IS_RUNNABLE(fcn))	// happens
      vmThrow(vm,E_ASSERTION_ERROR,"Fcn not runnable");

   blockReset(block,fcn,vm);
   pcInit(vm,code,0);
}

     /* Result is 0 if exception thrown and callingVM is 0.
      * pr can be zero if you don't care about a result.
      * Throws if can't create a VM, otherwise, doesn't.
      * Result MIGHT be 0 (when called from the loader)
      * YOU NEED TO PROTECT fcn & arglist.
      * Can GC
      * 
      * fcnRun(f,fence.i1=tupleCreateX(...),0,vm)
      * fcnRun(fcnCreate(),arglist,0,vm)
      * fcnRun(fcnCreate(),MLIST,0,vm)
      * 
      * Some times, result can be zero BUT the VM containing result dies so
      * result is unprotected.  If GC occurs, result is gone. The calling VM
      * can protect result with a fence.
      * Example: Fcn creates string and leaves it in vm, vm dies, string can
      *   be collected by the time you get it. You don't care if you are
      *   looking for a Immortal such as Star, Void or True, etc.
      *   vmSetFence(vm,&fence,0,0);
      *      result = fcnRun(fcn,arglist,&fence.i1,vm);
      *      ...
      * Methods/Properties/Ops caution: If you return result, it isn't
      *   protected because the calling VM have been already marked, you may
      *   need to orphanize.
      * DIP2.2
      * 
      * Note:  If you are running a fcn from a Method, use vmCallFcnFromMethod()
      * if you can (to avoid the overhead of creating a new VM).
      */
Instance *
fcnRun(Instance *self,pArglist arglist,Instance **pr,pVM callingVM)
{
   Instance *result;
   pVM       vm;

   vm = vmCreate(callingVM,NoBlock,0x0,callingVM);	// can GC
      result = vmRunFcn(vm,self,arglist,0);	// throws, GCs
      if (pr) *pr = result;
if (!pr)instanceIsOrphan(result);
   vmFree(vm);					// can free result
   return result;
}

    // Run a fcn in a sandboxed parentless VM
    // If pr, pr points to a fence slot
    // See also: vm.c:runFcnInSandBox()
Instance *rogueFcnRun(Instance *self,pArglist arglist,Instance **pr)
{
   Instance *result;
   pVM       vm;

   vm = vmCreate(NoVM,NoBlock,VM_MOTHERLESS_CHILD,NoVM);
      result = vmRunFcn(vm,self,arglist,0);
      if (pr) *pr = result;
if (!pr) instanceIsOrphan(result);
   vmFree(vm);					// can free result
   return result;
}

    /* Run a function that is residing in the Vault. Typically used to run a
     *   helper function.
     * You need to protect arglist
     * fcnRunith("Compiler.Asm","disFcn", mlistBuild(...), vm)
     * DIP2.2, be careful if you call this outside of a Method or Property or
     *    stall.
     */
Instance *fcnRunith(
     char *vaultClassPath,char *fcnName,pArglist arglist,pVM vm)
{
   ZKL_Fcn   _fcn;
   Instance *r, *fcn;

   fcn = classFindFcn(vaultFind(vaultClassPath,1,vm),fcnName,
				0,&_fcn,vm);
   if (!fcn)
   {
      char buf[200];
      sprintf(buf,"fcnRunith(): Can't find %.80s.%.80s",vaultClassPath,fcnName);
      vmThrow(vm,E_NOT_FOUND,buf);
   }

   if (!vm) r = fcnRun(fcn,arglist,0,NoVM);	// r might be GC'd
   else     r = vmCallFcnFromMethod(fcn,arglist,0,vm);
instanceIsOrphan(r);

   return r;
}

Instance *fcnRunFromClassId(
Instance *class,unsigned fid,pArglist arglist,pVM vm)
{
   ZKL_Fcn   _fcn;
   Instance *r, *fcn = classFindFcnById(class,fid,0,&_fcn,vm);
   if (!fcn)
   {
      char buf[200];
      sprintf(buf,"fcnRunith(): Can't find %.80s.%.80s",
              className(class),getGlobalName(fid,NoVM));
      vmThrow(vm,E_NOT_FOUND,buf);
   }
   if (!vm) r = fcnRun(fcn,arglist,0,NoVM);	// r might be GC'd
   else     r = vmCallFcnFromMethod(fcn,arglist,0,vm);
instanceIsOrphan(r);
   return r;
}

Instance *fcnRunFromClass(Instance *class,char *fcnName,pArglist arglist,pVM vm)
{
   ZKL_Fcn   _fcn;
   Instance *r, *fcn = classFindFcn(class,fcnName,0,&_fcn,vm);
   if (!fcn)
   {
      char buf[200];
      sprintf(buf,"fcnRunith(): Can't find %.80s.%.80s",className(class),fcnName);
      vmThrow(vm,E_NOT_FOUND,buf);
   }
   if (!vm) r = fcnRun(fcn,arglist,0,NoVM);	// r might be GC'd
   else     r = vmCallFcnFromMethod(fcn,arglist,0,vm);
instanceIsOrphan(r);
   return r;
}

/* ******************************************************************** */
/* *************************** Methods ******************************** */
/* ******************************************************************** */

    // .toString() -->String
static Instance *Fcn_toString(Instance *self,Instance *arglist,pVM vm)
{
//??? cache this?
   char buf[100];
   sprintf(buf,"Fcn(%s)",FCN_NAME(self));
   return stringCreate(buf,I_OWNED,vm);
}

    // (????)
static Instance *Fcn_setBreakPoint(Instance *self,Instance *arglist,pVM vm)
{
   notImplementedError(self, "setBreakPoint", vm);
   return Void;
}

static Instance *Fcn_unsetBreakPoint(Instance *self,Instance *arglist,pVM vm)
{
   notImplementedError(self, "unsetBreakPoint", vm);
   return Void;
}

    // .unasm([outputStream]) --> Compiler.Asm.disFcn(self [,outputStream])
static Instance *Fcn_unasm(Instance *self,Instance *arglist,pVM vm)
{
   MLIST(mlist,2);
   Instance *args = mlistBuild(mlist,self,ZNIL);
   Instance *pi   = arglistTryToGet(arglist,0);

   if (pi) mlistAppendI(mlist,pi,2);
   return fcnRunith("Compiler.Asm","disFcn",args,vm);
}

static Instance *Fcn_unasmN(Instance *self,Instance *arglist,pVM vm)
{
   notImplementedError(self, "unasmN", vm);
   return Void;
}

static Instance *Fcn_verifyArglist(Instance *self,Instance *arglist,pVM vm)
{
   notImplementedError(self, "verfiyArglist", vm);
   return BoolFalse;
}

int fcnIsInstanceOf(Instance *self, Instance *fcn)
{
   if (TYPEO(self) != FcnType || TYPEO(fcn) != FcnType) return 0;
   return FCN_BASE(self) == FCN_BASE(fcn);
}

    // .isInstanceOf(fcn ...) -->Bool
static Instance *Fcn_isInstanceOf(Instance *self,Instance *arglist,pVM vm)
{
   int		n;
   Instance    *f;
   for (n = 0; (f = listGet(arglist,n++)); )
      if (fcnIsInstanceOf(self,f)) return BoolTrue;
   return BoolFalse;
}

    // .launch(args for thread): Create new thread -->Void
static Instance *Fcn_launch(Instance *self,pArglist arglist,pVM vm)
   { return threadCreate(FCN_CONTAINER(self),self,arglist,2,vm); }

    // .future(args for thread): Create new thread --> 
    //  Deferred Straw with result
static Instance *Fcn_future(Instance *self,pArglist arglist,pVM vm)
{
   Instance *straw = threadCreate(FCN_CONTAINER(self),self,arglist,1,vm);
   return deferredCreate(methodCreate(straw,"read",0,vm),NoArglist,vm);
}

    // .strand(args) : co-op thread future -->future
static Instance *fcnStrand(Instance *self,char *name,pArglist arglist,pVM vm)
{
   Instance  *args;
   MLIST(mlist,11);

   if (arglistLen(arglist,vm) > 10)
      vmThrow(vm,E_ASSERTION_ERROR,"fcn.strand: No more than 10 parameters");
   args = mlistBuild(mlist,self,ZNIL);
   mlistExtend(mlist,arglist,0,11);
   return fcnRunith("Thread.StrandsHotel",name,args,vm); // --> future
}

static Instance *Fcn_strand(Instance *self,pArglist arglist,pVM vm)
   { return fcnStrand(self,"add",arglist,vm); }

    // .stranded(future|Void|True[,args]) --> payload
static Instance *Fcn_stranded(Instance *self,pArglist arglist,pVM vm)
   { return fcnStrand(self,"addNoFuture",arglist,vm); }

#if 0	// replaced by .fpN
    // .hopSkipJmp(n,args) --> self(rearranged args)
    // .fp creates a Partial with this method to make a many arg closure.
    // Take the last n args, move them to the front of the arglist,
    //   and tail call self
    // hopSkipJmp(2,c,a1,a2) -->self(a1,a2,c)
    // n == 0 or n == len - 1 --> no change to arglist
    // works for all length arglists
static Instance *Fcn_hopSkipJmp(Instance *self,pArglist arglist,pVM vm)
{
   int n       = (int)arglistGetInt(arglist,0,"hopSkipJmp",vm);
   int numArgs = arglistLen(arglist,vm);
   MLIST(mlist,50);

   if (n >= numArgs || n < 0 || numArgs > 50) 
      vmThrow(vm,E_VALUE_ERROR,"Fcn.hopSkipJmp");
   mlistCopy(mlist,arglist,numArgs - n);	// copy tail of arglist
   mlistExtend(mlist,arglist,1);		// copy front of arglist
   TUPLE_LEN(mlist) = numArgs - 1;		// truncate arglist

???   return fcnRun(self,(Instance *)mlist,0,vm);
}
#endif

static const MethodTable fcnMethods[] = 
{
   "toString",		(pMethod)Fcn_toString,
   "toBool",		(pMethod)Bool_soTrue,

   "build",		(pMethod)Fcn_build,

   "setBreakPoint",	(pMethod)Fcn_setBreakPoint,
   "unsetBreakPoint",	(pMethod)Fcn_unsetBreakPoint,
   "unasm",		(pMethod)Fcn_unasm,
   "unasmN",		(pMethod)Fcn_unasmN,
   "verifyArglist",	(pMethod)Fcn_verifyArglist,
   "isInstanceOf",	(pMethod)Fcn_isInstanceOf,
   "copy",		(pMethod)Object_noop,
   "launch",		(pMethod)Fcn_launch,
   "future",		(pMethod)Fcn_future,
   "strand",		(pMethod)Fcn_strand,
   "stranded",		(pMethod)Fcn_stranded,
   "defer",		(pMethod)Object_defer,
   0,			0
};


/* ******************************************************************** */
/* *************************** Properties ***************************** */
/* ******************************************************************** */

    // .name -->String
Instance *Fcn_name(Instance *self,pVM vm)
   { return kStringCreate(FCN_NAME(self),self,I_OWNED,vm); }

    // .fullName -->String
Instance *Fcn_fullName(Instance *self,pVM vm)
{
   Instance *s;
   Fence     fence;

   s = Class_fullName(FCN_CONTAINER(self),vm);
   vmSetFence(vm,&fence,0,s);
      s = stringCat(vm,stringText(s),".",FCN_NAME(self),(char *)0);
   vmRemoveFence(&fence,0);
   return s;
}

    // .prototype -->T(name,name...)
static Instance *Fcn_prototype(Instance *self,pVM vm)
{
   StringTable names = { FCN_NUM_ARGS(self), strchr(FCN_NAME(self),0) + 1, 0 };
   return stToList(&names,vm);
}

char *fcnProtoName(Instance *fcn,unsigned n)
{
   char *ptr;
   if(!fcn || TYPEO(fcn)!=FcnType || n>=FCN_NUM_ARGS(fcn)) return "";
   for(ptr = strchr(FCN_NAME(fcn),0) + 1; n--; ) ptr = stNextString(ptr);
   return ptr;
}

#if 0
static StringTable CodeNames =
{
   6,		// n
		// Strings: name, vaultPath, var names (sorted)
   "__Code#\0\0code\0isStatic\0map\0strings\0",
   35,		// size
};
#endif

    // Create a Asm.Code from 3 Datas & 1 Bool
    // caller holds a fence on args
static Instance *_createAsmDotCode2(
   pData code,pData strings,pData map, Instance *isStatic, pVM vm)
{
#if 1
   Instance *args, *Code;
   MLIST(mlist,5);

   Code = vaultChase("Compiler.Asm.Code",vm);
   args = mlistBuild(mlist,code,strings,map,isStatic,ZNIL);
   return classRun(Code,args,vm);

#else
   Instance *klass = classEmbryo(&CodeNames,0,0,0,0,I_OWNED,vm); //creates new base
   classCookClass(klass,vm);
   classFindVar(klass,"code",    code,    vm);
   classFindVar(klass,"strings", strings, vm);
   classFindVar(klass,"map",	 map,     vm);
   classFindVar(klass,"isStatic",isStatic,vm);
   classSetVarBits(klass,"1111", VAR_RO);	// all vars are read only
   classSetAttributes(klass,"static noChildren",vm);

   return klass;
#endif
}

    // create copies of fcn data
    // fences _createAsmDotCode2()
static Instance *_createAsmDotCode(Instance *fcn, ZKL_Code *code,pVM vm)
{
   if (code->code)
   {
      Instance *asmDotCode, *base = (Instance *)FCN_BASE(fcn);
      Instance *codeData, *stringData, *map, *isStatic;
      Fence	fence;

      vmSetFence(vm,&fence,0,0);
	 codeData   = fence.i1 = 
	    kdataCreate(code->code,    code->codeSize,  base,I_OWNED,vm);
	 stringData = fence.i2 = 
	    kdataCreate((Byte *)code->strings,code->stringSize,base,I_OWNED,vm);
	 map	    = fence.i3 =
	    kdataCreate(code->map,    code->mapSize,   base,I_OWNED,vm);
	 isStatic   = FCN_IS_STATIC(fcn) ? BoolTrue : BoolFalse;
	 dataMode(stringData,DATA_TREAT_AS_STRINGS,vm);

	 asmDotCode = _createAsmDotCode2(codeData,stringData,map,isStatic,vm);
      vmRemoveFence(&fence,0);

      return asmDotCode;
   }
   return Void;
}

    // .code -->Class
static Instance *Fcn_code(Instance *self,pVM vm)
   { return _createAsmDotCode(self,FCN_CODE(self),vm); }

    // .defaultArgs -->T(Fcn|Void,...)|T()
static Instance *Fcn_defaultArgs(Instance *self,pVM vm)
{
   int    i,n = FCN_NUM_ARGS(self), first = FCN_BASE(self)->d1;
   int	  z = FCN_BASE(self)->numDefaults;
   Instance *list;
   ZKL_Code *defaultArgs = FCN_DEFAULT_ARGS(self);
   Fence     fence;

   if (n == 0) return emptyTuple;
   list = tupleCreate(n,I_OWNED,vm);
   for (i = first; i--; ) tupleAppend(list,Void);
   vmSetFence(vm,&fence,0,list);
      for (i = z; i--; defaultArgs++)
	 tupleAppend(list,_createAsmDotCode(self,defaultArgs,vm));
      for (i = n - z - first; i--; ) tupleAppend(list,Void);
   vmRemoveFence(&fence,0);
   return list;
}

    // .container -->Class
Instance *Fcn_container(Instance *self,pVM vm) { return FCN_CONTAINER(self); }

    // .size --> L(instance size,fcnBase size)
static Instance *Fcn_size(Instance *self,pVM vm)
{
   return tupleCreateX(vm,
	intCreate(sizeof(ZKL_Fcn),vm),intCreate(FCN_BASE(self)->size,vm),ZNIL);
}

    // .codeSize --> T(code size, string table size)
static Instance *Fcn_codeSize(Instance *self,pVM vm)
{
   ZKL_Code *code = FCN_CODE(self);
   return tupleCreateX(vm,
	intCreate(code->codeSize,vm),intCreate(code->stringSize,vm),ZNIL);
}

    // .nullFcn -->nullFcn
static Instance *Fcn_nullFcn(Instance *self,pVM vm) { return nullFcn; }

    // .idFcn -->idFcn
static Instance *Fcn_idFcn(Instance *self,pVM vm)
{
   if (!idFcn)
   {
//      idFcn = classFindFcn(vaultFind("Utils.Helpers",1,vm),"idFcn",0,0,vm);
      idFcn = classFindFcn(Utils_Helpers,"idFcn",0,0,vm);
      if (!idFcn) return Void;
   }
   return idFcn;
}

    // .isPrivate -->Bool
static Instance *Fcn_isPrivate(Instance *self,Instance *vm)
   { return boolCreate(FCN_IS_PRIVATE(self)); }

    // .isStatic -->Bool
    /* To determine if a fcn is static:  No access/mod of vars, no calling
     *   fcn that isn't static (might might be arbitray obj).
     *   class C { var v=L(123); fcn f(a=v) {a.append(666)}}
     * A static fcn isn't thread safe:  might call method that modifies obj.
     * A non static fcn can be re-entrant if critical is used.
     */
static Instance *Fcn_isStatic(Instance *self,Instance *vm)
   { return boolCreate(FCN_IS_STATIC(self)); }

    // .isRunnable -->Bool
static Instance *Fcn_isRunnable(Instance *self,Instance *vm)
   { return boolCreate(FCN_IS_RUNNABLE(self)); }

    // .attributes -->String("static" &| "private")
static Instance *Fcn_attributes(Instance *self,pArglist arglist,pVM vm)
{
   char	 buf[200];

   *buf = '\0';
   if (FCN_IS_STATIC(self))   strcpy(buf,"static ");
   if (FCN_IS_PRIVATE(self))  strcat(buf,"private ");
   if (*buf) buf[strlen(buf) - 1] = '\0';
   return stringCreate(buf,I_OWNED,vm);
}


static const PropertyTable propertyTable[] = 
{
   "name",		(pProperty)Fcn_name,
   "fullName",		(pProperty)Fcn_fullName,
   "prototype",		(pProperty)Fcn_prototype,
   "defaultArgs",	(pProperty)Fcn_defaultArgs,
   "code",		(pProperty)Fcn_code,
   "container",		(pProperty)Fcn_container,
   "size",		(pProperty)Fcn_size,
   "codeSize",		(pProperty)Fcn_codeSize,
   "nullFcn",		(pProperty)Fcn_nullFcn,
   "idFcn",		(pProperty)Fcn_idFcn,
   "isPrivate",		(pProperty)Fcn_isPrivate,
   "isStatic",		(pProperty)Fcn_isStatic,
   "isRunnable",	(pProperty)Fcn_isRunnable,
   "attributes",	(pProperty)Fcn_attributes,
   0,			0
};

/* ******************************************************************** */
/* ******************************************************************** */

////////////////// nullFcn
static StringTable nullNames =
{
   1,			// n
   "nullFcn\0",		// Strings: name, vaultPath
   8,			// size
};

	// Not static: opSelf
static Byte nullConstructor[4] = { opVoid, opSetX, opSelf, opDone };

static ZKL_Code nullCode =	// a minimal constructor
{
   nullConstructor,
   "",				// no strings
   sizeof(nullConstructor),0,	// codeSize, stringSize
   1,				// empty map: (0)
   0,				// no kstrings
   (Byte *)"",			// map is one byte of zero
};


//static pMethod in_fcn_methods(Instance *ignore, register char *str);
//static pProperty in_fcn_properties(Instance *ignore, register char *str);

void fcnConstruct(void)
{
   constructObject((ZKL_Object *)&FcnBaseObject,NativeType,0,0,0,NoVM);
   FcnBaseObject.name	     = "FcnBaseObject";
   FcnBaseObject.freeMe	     = fcnBaseFree;

   constructObject((ZKL_Object *)&FcnObject,FcnType,
		   fcnMethods,propertyTable,0,NoVM);
   FcnObject.magicMarker    = fcnMarker;
//   FcnObject.methodSearch   = in_fcn_methods;
//   FcnObject.propertySearch = in_fcn_properties;
   FcnObject.isize	    = sizeof(ZKL_Fcn);
   FcnObject.isBInstance    = 1;
   ibucketReserve(&FcnObject,5005,&fnBuckets,0,NoVM);

   fcnID = FcnObject.id;

   	// nullFcn is immortal in case somebody calls nullFcn.code()
//   stCalcSize(&nullNames);	// in case I can't count
   nullFcn = fcnEmbryoWithStaticCode(&nullNames,0,0,&nullCode,0,I_IMMORTAL,NoVM);
//   nullFcn->iflag = 0;	// not static, not runnable
}



///////////////////////////////////////////////////////////
// zkl extractTable    < fcn.c | gperf | zkl gperf -i fcn
// zkl extractTable -p < fcn.c | gperf | zkl gperf -i fcn



////////////////////////////////////////////////////////
// zkl extractTable -p < fcn.c | gperf | zkl gperf -i fcn

