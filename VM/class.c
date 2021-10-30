/* class.c : the Class Object
 * 
 * Copyright (c) 2006,2007,2008-12,2013 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define __NOT_A_DLL
#define __CLASS_INTERNALS
#define __FCN_INTERNALS		// yes, I want to access Fcn guts
#define __GC_INTERNALS		// mark macros
#define __LIST_INTERNALS

#include "zklObject.h"
#include "zklFcn.h"
#include "zklClass.h"
#include "zklData.h"		// for Bits class
#include "zklList.h"
#include "zklMethod.h"
#include "zklNumber.h"
#include "zklString.h"

#include "MD5/global.h"
#include "MD5/md5.h"

#define NOT_FOUND_NAME		"__notFound"
#define INIT_FCN_NAME		"init"

static size_t baseCount = 0, iCount = 0;	// stats

Instance *NullClass;

static Instance
   *nullMethod,		// so *.isType(NullClass.Method) doesn't malloc
   *nullProperty;	// so *.isType(NullClass.Property) doesn't malloc

static ZKL_Object GlobalClassObject;	// the "every class" static data

//static pMethod   in_class_methods(Instance *ignore, register char *str);
//static pProperty in_class_properties(Instance *ignore, register char *str);

static void classAddParent(Instance *self,Instance *parent,int n,pVM vm);

    /* Notes:  Fcns are embedded into a ClassBase, they are not independent,
     *   no longer on a GC list and are no longer bound to their class
     *   instance (container), as they are shared by all instances of the
     *   class.  So, if a fcn needs to become a first class object, it needs
     *   to be detached (ie copied) and its container updated to point to
     *   the correct class.
     * This results in huge (~2x) [runtime] space savings over each class
     *   instance pointing at detached fcns, and, perhaps surprisingly,
     *   faster run times, as most fcns are run and the VM special cases
     *   that.  There is probably a space/speed trade off lesson here.
     * Static fcns:  A constructor or init fcn can never be static (unless
     *   the class is static).  Although, in practice,
     *   classRun/classPrepForRun can probably fudge things to make it work.
     *   Otherwise, it is OK; just make sure fcn.container never points to a
     *   GC'd class, it should not point to NullClass because then
     *   reflection doesn't work anymore.  This could be a win for anonymous
     *   fcns (list.apply(fcn {})) but creating a 20 byte object is probably
     *   noise compared to runtime.
     */
typedef struct		// Shared data for instances of the same class
{
   char	       *name;		// Probably points into data
   char	       *vaultPath;	// Also in Object, bits might be in data
   uint16_t    *linkTable;	// malloc'd when cooked
   CAtomicInt	refCount;	// so I know when to free this struct
   uint16_t     varABitsO;	// var attribute table(s), in data
   uint16_t	bsize;		// data collection: size of ClassBase
   uint16_t     isize;		// total instance size for copies
   uint16_t	minGN,maxGN, linkTableSz;  // link table info (to global names)
   uint8_t	initIsFcnN;	// defauls to 0, the constructor
   uint8_t	numVars,numFcns,numClasses,numParents;
   uint8_t	minName,maxName, numLinearParents;
   unsigned int	notFound:1;	// 0|1: 1 if __notFound is in self or parent
   unsigned int	isStatic:1;	// don't copy on self()
unsigned int       isScript:1;     // self is a script
   unsigned int	noChildren:1;	// 1 if you can't inherit from this class
   unsigned int	isPrivate:1;	// 1 if resolve can't see this class
   unsigned int	cpFull:2;	// 2 if all classes & parents have been added
   unsigned int	createReturnsSelf:1;  // C() --> C

   void *data[0];
   // Really Byte[] but I need to get the alignment right for PtrInts:
   //    Don't confuse Fcns with PtrInts
   // Expands to hold static data:
   // Fcn theFcns[numFcns];  // VM looks at
   // char *varNames[numVars]: sorted, might be in text[]. might not
   // char text[]; class name, vault path, var names. Optional.
   //   The patch panels map between name and object arrays
   // Byte[(1+ bit per var)*2]: bit tables for vars
}ClassBase;
  // WinXP/32: NullClass(36), Compiler(947),
  // Linux/64: NullClass(56,0), Compiler(1620,1052), Exception.Generic(104,920)


/////////////////////////// Class
   // The first instance of a class is the "Eve" class
   // Instance->iflag == 1 --> copied class (ie every class but Eve)
   // Instance->iflag2 == 1 --> this class is embedded in a copied class
   // WinXP/32: NullClass(28), Compiler(72)
   // Linux/64: NullClass(56), Compiler(144,296), Exception.Generic(64,144)


#define CLASS_BASE(c)		( (ClassBase *)((Class *)c)->classBase )
#define BASE_DATA(base)		( (Byte *)(base)->data )
#define BASE_ARRAY(base,aname)  ( BASE_DATA(base) + (base)->aname )
#define C2DATA(c)		( BASE_DATA(CLASS_BASE(c)) )

#define CLASS_NAME(c)		( CLASS_BASE(c)->name )
#define VPATH(c)		( CLASS_BASE(c)->vaultPath )
#define CONTAINER(c)		( ((Class *)c)->container )
#define INIT_FCN(c)		( CLASS_BASE(c)->initIsFcnN )
#define TOP_DOG(c)		( ((Class *)c)->topdog )

#define NUM_VARS(c)		( CLASS_BASE(c)->numVars )
#define VAR_NAMES(c)		( BASE_VAR_NAMES(CLASS_BASE(c)) )
#define BASE_VAR_NAMES(b)	( (char **)((ZKL_Fcn *)b->data + b->numFcns) )

#define NUM_FCNS(c)		( CLASS_BASE(c)->numFcns )
#define BASE_FCNS(c)		( (ZKL_Fcn *)CLASS_BASE(c)->data )
#define THE_FCNS(c)		( ((Class *)c)->fcns )
#define NTH_FCN(c,n)		( &((Class *)c)->fcns[n] )

#define IS_COOKED(c)		( CLASS_BASE(c)->isize )
#define CP_FULL(c)		( CLASS_BASE(c)->cpFull )

#define NUM_CLASSES(c)		( CLASS_BASE(c)->numClasses )
#define NUM_PARENTS(c)		( CLASS_BASE(c)->numParents )
#define NUM_LINEAR_PARENTS(c)	( CLASS_BASE(c)->numLinearParents )

#define IS_STATIC(c)		( CLASS_BASE(c)->isStatic )
#define IS_SCRIPT(c)		( CLASS_BASE(c)->isScript )
#define NO_CHILDREN(c)		( CLASS_BASE(c)->noChildren )
#define IS_PRIVATE(c)		( CLASS_BASE(c)->isPrivate )
#define CREATE_RETURNS_SELF(c)	( CLASS_BASE(c)->createReturnsSelf )

#define VAR_BIT_TABLE_SIZE(n)	( (n + 7) / 8 )
#define VAR_BIT_TABLE(c)	( C2DATA(c) + CLASS_BASE(c)->varABitsO )
#define VAR_BIT_INDEXES(n,i,b)	( i = n/8, b = 1 << (n % 8) )
#define VAR_WHICH_TABLE(c,w)	\
		( VAR_BIT_TABLE(c) + w*VAR_BIT_TABLE_SIZE(NUM_VARS(c)) )

#define NOT_FOUND(c)		( CLASS_BASE(c)->notFound )


__inline static int _isProxyVar(Class *self, int n)
{
   Byte *table = VAR_WHICH_TABLE(self,VAR_PROXY);
   int   i,b;

//   if (!table) return 0;
   VAR_BIT_INDEXES(n,i,b);
   return table[i] & b;
}

__inline static int _isROVar(Class *self, int n)
{
   Byte *table = VAR_WHICH_TABLE(self,VAR_RO);
   int   i,b;

//   if (!table) return 0;
   VAR_BIT_INDEXES(n,i,b);
   return table[i] & b;
}

static void setVarBit(Instance *self,int n,int which)
{
   Byte *table = VAR_WHICH_TABLE(self,which);
   int   i,b;

   VAR_BIT_INDEXES(n,i,b);
   if (table) table[i] |= b;
}

static int getVarBit(Instance *self,int n,int which)
{
   Byte *table = VAR_WHICH_TABLE(self,which);
   int   i,b;

   VAR_BIT_INDEXES(n,i,b);
   return ((table[i] & b) != 0);
}

#if GC_SANITY_CHECK && 0
   void classVerify(Instance *self)
   {
      unsigned i;

      instanceVerify(self,0,NoVM);
      if (TOP_DOG(self)) instanceVerify(TOP_DOG(self),0,NoVM);
      for (i = 0; i < NUM_CLASSES(self); i++) classVerify(CLASSES(self)[i]);
      for (i = 0; i < NUM_PARENTS(self); i++) classVerify(PARENTS(self)[i]);
//      for (i = 0; i < NUM_FCNS(self);    i++) fcnVerify(FCNS(self)[i]);
//      for (i = 0; i < NUM_VARS(self);    i++) instanceVerify(VARS(self)[i],1,NoVM);
   }
#endif

    // The parse tree for the Parser is ~18k classes

#if 0
static void classMarker(Instance *self)	// recursive version
{
   int	      n = NUM_VARS(self) + NUM_CLASSES(self) + NUM_PARENTS(self);
   Instance **data = self->data;
   ZKL_Fcn   *fcns;

   while (n--) instanceMark(*data++);

	/* Fcns are not on a GC list, these fcn instances only exit in this
	 *   class, don't need marking, mark doesn't need reseting but the
	 *   fcn bases need marking. If this class goes away, so do these
	 *   fcns.
	 * This is another reason why you can't just pass a fcn out of a
	 *   class, you have to make copy so GC works.
	 * If GC reclaim phase clears these marks, then no problem.
	 * I could just clear them after I mark the bases
	 */
   for (n = NUM_FCNS(self), fcns = THE_FCNS(self); n--; fcns++)
   {
	// fb = self->classBase->theFcns[n].fcnBase;
      Instance *fb = (Instance *)FCN_BASE(fcns);
      GC_MARK(fb);	// == instanceMark(fb), saves a function call
//      if (((FcnBase *)fb)->isStatic) GC_MARK((Instance *)fcns);
   }

	// Mark the following in case self was seprated from the pack
   	// Keep the class tree intact since I have pointers to these
   instanceMark(self->container);
   instanceMark(self->topdog);
}

#else

    /* Function calls/recursion is expensive, so I've unrolled this heavily
     *   recursive marker into an iterative one.  Good for a massive 0-2%
     *   speedup. Just not enough classes per call to make a difference.
     * Compiling the parser, I'm seeing like 14 calls per GC with this code
     *   looking at 337, 183, 42, 21 (the rest < 10) classes (113, 59, 14
     *   getting marked).
     * Read from a class location (var or class/parent) only once since vars
     *   can be changing (by another thread) and embryos might be in the
     *   process of being built (copies are fully constructed).
     */
static void classMarker(Instance *_self)
{
   #define MAX_SNIPPETS 350
   Class     *self = (Class *)_self;
   struct { Instance **klasses; int kz; } snippets[MAX_SNIPPETS + 5];
   Instance  *klz[1], **klasses = klz;
   int	      n, z=0, kz=1;

   if (self->instance.iflag2)
   {
      self = (Class *)self->instance.nexti;	// root of copy
      if (GC_IS_MARKED((Instance *)self)) return;
   }

   klz[0] = (Instance *)self;
   GC_CLEAR_MARK((Instance *)self); // undo the instanceMark() that got us here
   while(1)
   {
      do
      {
	 Class     *klass;
	 Instance **data;
	 ZKL_Fcn   *fcns;

	 klass = (Class *)klasses[--kz];  // container, topdog can be 0
	 if (!klass || GC_IS_MARKED((Instance *)klass)) continue;
	 if (klass == (Class *)NullClass) continue;

	 if (!klass->instance.iflag2)		// not a blob
	    GC_MARK((Instance *)klass); 	// I_OWNED or I_IMMORTAL

	 #if GC_SANITY_CHECK
	    instanceVerify((Instance *)klass,0,NoVM);
	 #endif

	   // Mark the following in case klass was seprated from the pack
	   // Keep the Class tree intact since I have back pointers into it
	   // both these can be zero
	 snippets[z].klasses = &klass->container; snippets[z].kz = 1; z++;
	 if (!klass->instance.iflag)
	    { snippets[z].klasses = &klass->topdog; snippets[z++].kz = 1; }

	 for (n = NUM_VARS(klass), data = VARS(klass); n--; data++)
	 {
	    Instance *v = *data;  // read var only once! (might be changing)
	    int	      t;

	    #if USE_POINTER_INTS
	       if (IS_PtrInt(v)) continue;   // which could also be garbage
	    #endif
	    if (GC_IS_MARKED(v)) continue;

	    t = GC_TYPE(v);
	    if (t == I_UNTOUCHABLE || t == I_INVISIBLE) continue;

	    if (TYPEO1(v) == ClassType)
	    {
	       if (v->iflag2) v = v->nexti;	// start marking at the root
	       snippets[z].klasses = (Instance **)v; snippets[z].kz = -1;
	       if (++z > MAX_SNIPPETS) vmHalt("Class marker stack overflow");
	       continue;
	    }

	    if (TYPEO1(v) == FcnType)
	    {
	       FcnBase *fb = FCN_BASE(v);
	       GC_MARK((Instance *)fb);
	       if (t != I_2SPECIAL) GC_MARK(v); // "wild" fcn, not in a class
	       v = FCN_CONTAINER(v);
	       if (GC_TYPE(v) != I_UNTOUCHABLE)	// eg NullClass
	       {
		  if (v->iflag2) v = v->nexti; // start marking at the root
		  snippets[z].klasses = (Instance **)v; snippets[z].kz = -1;
		  if (++z > MAX_SNIPPETS) vmHalt("Class marker stack overflow 2");
	       }
	       continue;
	    }

#if 0	// lots of errors
	    	// if var is evaluated Deferred, change it to real value
	    if (IS_DEFERRED(v) && (i = deferredIsEvaled(v)))
	    {
	       CAP_SET(data,i); v = i;		// i isn't PtrInt
	       t = GC_TYPE(v);
	       if (GC_IS_MARKED(v) || t == I_UNTOUCHABLE || t == I_INVISIBLE)
		  continue;
	    }
#endif
	    if (t != I_OWNED || MAGIC_MARKER(v)) instanceMark(v);
	    else GC_MARK(v);
	 } // mark vars

	 n = NUM_CLASSES(klass) + NUM_PARENTS(klass);
	 if (n)
	 {
	    snippets[z].klasses = CLASSES(klass); snippets[z].kz = n;
	    if (++z > MAX_SNIPPETS) 
	       vmThrow(NoVM,E_VM_ERROR,"Class marker overflow");
	 }

	   /* Fcns are not on a GC list (even if static), these fcn
	    *   instances only exist in this class, don't need marking, mark
	    *   doesn't need reseting but the fcn bases need marking.  If
	    *   this class goes away, so do these fcns.
	    * This is another reason why you can't just pass a fcn out of a
	    *   class, you have to make copy so GC works.
	    * If GC reclaim phase clears these marks, then no problem.
	    * I could just clear them after I mark the bases
	    */
	 for (n = NUM_FCNS(klass), fcns = THE_FCNS(klass); n--; fcns++)
	 {
	    // fb = self->classBase->theFcns[n].fcnBase;
	    Instance *fb = (Instance *)FCN_BASE(fcns);
	    GC_MARK(fb);	// == instanceMark(fb), saves a function call
	 }
      } while(kz);
      if (0 == z--) break;
      kz = snippets[z].kz;
      if (kz == -1) 
	 { klz[0] = (Instance *)snippets[z].klasses; klasses = klz; kz = 1; }
      else klasses = snippets[z].klasses; 
   } // while
}

#endif // recursive vs in-place markers

#if 0
    // Here, I need to clear the mark on the fcns, not their bases
    /* test case: fcn f{} do(10) { GarbageMan.collect() } f.name
     *   --> class __class#0 { fcn f{} }
     * startup throws away __class#0 and sticks f in a slot. Since f isn't
I think this is wrong - a copy of f is saved, not f
     * on a gc list, its mark isn't cleared. So when startup marks slots,
     * instanceMark(f) thinks f has been marked which means f doesn't mark
     * base or __class#0 --> boom. The key here is __class#0 IS on a gc list
     * and f has to mark it (as it is the only one that knows about it) so
     * when __class#0 survives, it needs to clear f's mark.
     * Immortal classes don't have this issue because the class is always
     * directly marked, not via a fcn.
     * Copies of fcns are owned (and aren't in a class any more).
     * Fcn embryos are Owned/Immortal
     * Temp fcns are I_2SPECIAL
     */
void classResetMarks(Instance *self)	// called from gc.c for live classes
{
   int	    n;
   ZKL_Fcn *fcns;

   if (!GC_IS_MARKED(self)) return;	// already cleared on this sweep
   for (n = NUM_FCNS(self), fcns = THE_FCNS(self); n--; fcns++)
{
if (GC_TYPE((Instance *)fcns) == I_IMMORTAL) continue;
if (GC_IS_MARKED((Instance *)fcns))
printf("HOHO %s:%s %d\n",CLASS_NAME(self),fcnName((Instance *)fcns),GC_TYPE((Instance *)fcns));
      GC_CLEAR_MARK((Instance *)fcns);
}
}
#endif

    /* Don't call asILayDying here, let GC do it. That way, asILayDying can
     * do whatever it wants to and doesn't have to worry about stuff
     * disappearing. After all those methods are called, then GC will call
     * this and start freeing stuff.
     */
static int classFree(Instance *self)
{
   iCount--;
   if (0 == CAI_DEC(&CLASS_BASE(self)->refCount))
   {
      baseCount--;
      ZFREE(CLASS_BASE(self)->linkTable);
      ZFREE(CLASS_BASE(self));	// varNameList will get gc'd
   }
   return 1;
}

char *className(Instance *self)
{
   if (!self || TYPEO(self) != ClassType) return "";
   return CLASS_NAME(self);
}

///////////////////////////////////
//\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ Walk a class hierarchy
///////////////////////////////////

static Instance *_findByName(Class *,char *,unsigned t,int searchParents,void *pfcn,pVM);
static Instance *_findById(Class *cls,unsigned id,void *pfcn,unsigned *_t,pVM vm);
static Instance *_findById2(Class *cls,unsigned,unsigned,int,void *,unsigned *,pVM);
static Class *_getIndex(Class *,char *,unsigned t,int searchParents,unsigned *slot,pVM);

#define LNK_VAR		0x0100	// count, not mask: 1,2,3,4
#define LNK_FCN		0x0200
#define LNK_CLASS	0x0300
#define LNK_PARENT	0x0400
#define LNK_BITS	0x0700	// mask for the above
#define LNK_LOW		0x07ff	// mask for the above
#define LNK_SHIFT	11	// the above bits
// The lower 8 bits are an index into the var/fcn/class/parent arrays
// On top of the above, if thing is in parent, is a parent index (5 bits)
// index is 1 based (so I can tell if it is there)


typedef Instance *(*_VisitP)(Class *,void *,pVM);
static Instance *_linearizeParents(
   int numParents,Instance **parents,
   _VisitP visit,void *X, pVM vm)
{
   #define MAX_BREADTH  20
   #define MAX_VISITED  50

   Instance  *i=0;
   int	      z = numParents, numVisited = 0;
   Instance  *pl1[MAX_BREADTH], **pending = pl1;

   while(1)
   {
      int	 n;
      Instance  *parent, *pl2[MAX_BREADTH], **p;
      ClassBase *visited[MAX_VISITED];

      // parents -> list of parents, z == num parents
      if (z == 0) break;

      for(p = parents, n = z; n--; p++)		// visit row
      {
	 int         n2;
	 ClassBase **pv, *pbase;

	 parent = *p; pbase = CLASS_BASE(parent);
	 for (n2 = numVisited, pv = visited; n2--; pv++)
	 {
	    // class A{} class B(A){} class C(A,B){}
	    // walk C: visit A only once even thou it there are two of them
	    // The lowest left most one
	    if (pbase == *pv) goto next;		// already visited
	 }
         if ((i = visit((Class *)parent,X,vm))) return i;	// visit
	 visited[numVisited++] = pbase;			// remember visited
	 if (numVisited == MAX_VISITED)			// overflow?
	    vmThrow(vm,E_ASSERTION_ERROR,"Class search: hierarchy too big");
      next: ;
      }

	// build list of the parents parents (the next parents to be visited)
      for(p = parents, n = z, z = 0;  n--;  p++)
      {
	 int num;
	 parent = *p;
	 if ((num = NUM_PARENTS(parent)))
	 {
	    if (z + num > MAX_BREADTH)
	       vmThrow(vm,E_ASSERTION_ERROR,"Class search: hierarchy too wide");
	    memcpy(pending + z,PARENTS(parent), num * sizeof(Instance *));
	    z += num;
	 }
      }
      parents = pending;
      pending = (pending == pl1) ? pl2 : pl1;	// switch parent lists
   } // while
   return 0;
}
static Instance *linearizeParents(Class *klass,_VisitP visit,void *X, pVM vm)
   { return _linearizeParents(NUM_PARENTS(klass),PARENTS(klass),visit,X,vm); }


	///////////////////////////////////////// vars
char *classNameOfNthVar(Instance *self, unsigned int n)
{
   if (TYPEO(self) != ClassType) return "";
   if (n >= NUM_VARS(self)) return "";
   return VAR_NAMES(self)[n];
}

static int _varIndex(Class *self,char *name, pVM vm)
{
   unsigned n;
   if(_getIndex((Class *)self,name,LNK_VAR,0,&n,vm)) return n;
   return -1;
}

	////////////////////////// classes

   // --> class | 0
Instance *classFindClass(Instance *self,char *name,int searchParents,pVM vm)
   { return _findByName((Class *)self,name,LNK_CLASS,searchParents,0,vm); }
Instance *classFindClassById(Instance *self,unsigned id,int searchParents,pVM vm)
   { return _findById2((Class *)self,id,LNK_CLASS,searchParents,0,0,vm); }


	/////////////////////////////////////// functions

    /* Warning! You need to protect fcn if a copy is made or class can go away.
     * Don't need to protect if using pfcn, self is (hopefully) protected
     * elsewhere.
     * Remember: All class instances share fcn f, so a copy that is bound to
     *   self needs to be made. Unless self is static (only one self).
     * I don't use ZKL_Fcn *pfcn because then everybody would have to
     *   #define __FCN_INTERNALS
     */
Instance *classFindFcn(Instance *self,
		char *name,int searchParents,void *pfcn,pVM vm)
   { return _findByName((Class *)self,name,LNK_FCN,searchParents,pfcn,vm); }
Instance *classFindFcnById(Instance *self,
		unsigned id,int searchParents,void *pfcn,pVM vm)
   { return _findById2((Class *)self,id,LNK_FCN,searchParents,0,0,vm); }

#if 0	// don't nuke
    // This will find names not in the link table (like "__fcn#1")
int _fcnIndex(Instance *self,char *name)
{
   unsigned   i;
   ZKL_Fcn   *fcns = THE_FCNS(self);

   for (i = 0; i < NUM_FCNS(self); i++)
      if (0 == strcmp(name,FCN_NAME(&fcns[i]))) return i;
   return -1;
}
#endif

    // vm == NoVM --> just going to peek at fcn, not use it
Instance *classFcnMatchup(Instance *self,ZKL_Code *code,pVM vm)
{
   unsigned i;

	// is class being built?
   if (!self || !code || TYPEO(self) != ClassType) return Void;

   for (i = 0; i < NUM_FCNS(self); i++)
   {
      Instance *fcn = (Instance *)NTH_FCN(self,i);
      if (FCN_CODE(fcn) == code)
      {
	 if (vm==NoVM) return fcn;
      	 return fcnCopy(fcn,self,vm);	// takes care of static class
      }
   }

   return Void;
}

	/////////////////////////////////////////////// vars

    // Returns: 0 (var not found) or value of var
    // Proxy vars not expanded, parents are searched
    // You may need to orphanize setTo
    // setTo is thread safe
Instance *classFindVarById(Instance *self,unsigned id,Instance *setTo,pVM vm)
{
   Class    *cls = (Class *)self;
   unsigned  slot;
   if((cls = (Class *)_findById2((Class *)self,id,LNK_VAR,1,0,&slot,vm)))
   {
      if (setTo)
      {
	 if (_isROVar(cls,slot))
	    vmThrow(vm,E_ASSERTION_ERROR,"Var is read only");
      	 CAP_SET(&VARS(cls)[slot],setTo);	// thread safe
      }
      return VARS(cls)[slot];
   }
   return 0;
}

Instance *classFindVar(Instance *self,char *name,Instance *setTo,pVM vm)
{
   unsigned id;

   if(!getGlobalId(name,&id,vm)) return 0;	// name doesn't exist
   return classFindVarById(self,id,setTo,vm);
}

    // Returns: Var index or -1
    // Doesn't search parents
int classFindVar2(Instance *self,char *name, int *isProxy, pVM vm)
{
   int n;
   if (!self || TYPEO(self) != ClassType || !*name) return -1;
   n = _varIndex((Class *)self,name,vm);
   if (n != -1 && isProxy) *isProxy = _isProxyVar((Class *)self,n);
   return n;
}

Instance *classGetVar(Instance *self,int n) { return VARS(self)[n]; }


	////////////////////////////////////// Parents
Instance *classFindParent(Instance *self,char *name,int searchParents,pVM vm)
   { return _findByName((Class *)self,name,LNK_PARENT,searchParents,0,vm); }

Instance *classFindParentById(
Instance *self,unsigned id,int searchParents,pVM vm)
   { return _findById2((Class *)self,id,LNK_PARENT,searchParents,0,0,vm); }

unsigned notFoundId, 
     toIntId,toFloatId,toStringId,lenId,resolveId,sSetId;  // set in object.c
extern unsigned toBoolId,sGetId;			   // vm.c


#define MIN_GN(c)     CLASS_BASE(c)->minGN
#define MAX_GN(c)     CLASS_BASE(c)->maxGN
#define LINK_TABLE(c) CLASS_BASE(c)->linkTable

#define MAX_PARENT_TREE  (0x1f - 1)	// max size of linearized parent tree

__inline static Instance *_getit(
Class *cls,unsigned t,unsigned n,void *pfcn,pVM vm)
{
   Instance *i;
   switch(t)
   {
      case LNK_VAR:
	 i = VARS(cls)[n];
	 if (_isProxyVar(cls,n))
	 {
	    Fence fence;		// ??? do I really need to fence this?
	    vmSetFence(vm,&fence,0,0);
	       i = objectRun(i,emptyList,&fence.i1,vm);
instanceIsOrphan(i);	//!!!????
	    vmRemoveFence(&fence,0);
	 }
	 return i;
      case LNK_FCN:
      {
	 ZKL_Fcn *f = NTH_FCN(cls,n);
	 if(pfcn)
	 {
	    *(ZKL_Fcn *)pfcn    = *f;
	    FCN_CONTAINER(pfcn) = (Instance *)cls;
	    i = pfcn;
	 }
	 else 
	    if(IS_STATIC(cls)) i = (Instance *)f;
	    else  i = fcnCopy((Instance *)f,(Instance *)cls,vm);
	 return i;
      }
      case LNK_CLASS:  return CLASSES(cls)[n];
      case LNK_PARENT: return PARENTS(cls)[n];
   }
   return 0;
}

    // vm is needed if LNK_VAR & var is a proxy, LNK_FCN & !pfcn
static Instance *_findById(Class *cls,
unsigned id,void *pfcn,unsigned *_t,pVM vm)
{
   unsigned  n,t, lnkParent;

   if(id>=MIN_GN(cls) && id<=MAX_GN(cls) &&	// in cls?
      (n = LINK_TABLE(cls)[id - MIN_GN(cls)]))
   {
      t = (n & LNK_BITS); lnkParent = n>>LNK_SHIFT; n &= 0xff; 
      *_t = t;
      if(lnkParent) cls = (Class *)PARENTS(cls)[lnkParent - 1];
      return _getit(cls,t,n,pfcn,vm);
   }
   return 0;	// not here
}

static Instance *_findById2(	// LNK_VAR, LNK_FCN, LNK_CLASS, LNK_PARENT
Class *cls,unsigned id,unsigned t,int searchParents,void *pfcn,
unsigned *slot, pVM vm)
{
   unsigned n, lnkParent;

   if(id>=MIN_GN(cls) && id<=MAX_GN(cls) &&	// in cls?
      (n = LINK_TABLE(cls)[id - MIN_GN(cls)]))
   {
      lnkParent = n>>LNK_SHIFT;
      if(lnkParent)
      {
         if(!searchParents) return 0;
         else cls = (Class *)PARENTS(cls)[lnkParent - 1];
      }
      if(t!=0xff && t != (n & LNK_BITS)) return 0;	// not the type wanted

      t = (n & LNK_BITS); n &= 0xff; 
      if(slot)
      {
	 *slot = n;
	 return (Instance *)cls;
      }
      return _getit(cls,t,n,pfcn,vm);
   }
   return 0;	// not here
}

static Instance *_findByName(	// LNK_VAR, LNK_FCN, LNK_CLASS, LNK_PARENT
Class *cls,char *name,unsigned t,int searchParents,void *pfcn,pVM vm)
{
   unsigned id;
   if(!getGlobalId(name,&id,vm)) return 0;	// name doesn't exist
   return _findById2(cls,id,t,searchParents,pfcn,0,vm);
}

static Class *_getSlot(
Class *cls,char *name,int searchParents,unsigned *slot,pVM vm)
{
   unsigned  n, lnkParent, id;

   if(!getGlobalId(name,&id,vm)) return 0;	// name doesn't exist

   if(id>=MIN_GN(cls) && id<=MAX_GN(cls) &&	// in cls?
      (n = LINK_TABLE(cls)[id - MIN_GN(cls)]))
   {
      lnkParent = n>>LNK_SHIFT;
      if(lnkParent && !searchParents) return 0;
      if(lnkParent) cls = (Class *)PARENTS(cls)[lnkParent - 1];
      *slot = n;
      return cls;
   }
   return 0;	// not here
}

static Class *_getIndex(	// LNK_VAR, LNK_FCN, LNK_CLASS, LNK_PARENT
Class *cls,char *name,unsigned t,int searchParents,unsigned *slot, pVM vm)
{
   unsigned n;
   cls = _getSlot(cls,name,searchParents,&n,vm);
   if(!cls) return 0;
   if(t != (n & LNK_BITS)) return 0;
   *slot = n & 0xff;
   return cls;
}

static Instance *callNotFound(	// call only if you know fcn __notFound exits
Instance *cls,char *name, unsigned id,pVM vm)
{
   // if method/property of self or Object, don't call __notFound
   if(!isIdMP(cls,id))  // not method or property
   {
      MLIST(mlist,1);
      Fence     fence;
      Instance *i, *arglist, *iname;
      ZKL_Fcn   f;
      unsigned  _;

      _findById((Class *)cls,notFoundId,&f,&_,vm);
      if(!name) name = getGlobalName(id,vm);
      iname   = stringCreate(name,I_OWNED,vm);
      arglist = mlistBuild(mlist,iname,ZNIL);
      vmSetFence(vm,&fence,0,iname);
	 // klass.__notFound(name)
	 i = fence.i1 = vmCallFcnFromMethod((Instance *)&f,arglist,0,vm);
instanceIsOrphan(i);
      vmRemoveFence(&fence,0);
	 // VoidVoid --> fcn says "I don't know what that is"
	 // let somebody else throw the error or let parent find
      if (i != VoidVoid && i != VoidStop) return(i);
   }
   return 0;	// can't call __notFound
}

    // Can GC
static Instance *resolveN(Class *self,
unsigned id,void *pFcn,unsigned *t,pVM vm)
{
   Instance *i;

   if (!IS_COOKED(self)) return 0;   // not cooked --> empty !!! init min/max

   i = _findById(self,id,pFcn,t,vm); // this class only, no proxy, no notFound
   if (i) return i;
   *t = 0;
   if(NOT_FOUND(self))	// __notFound exits
      return callNotFound((Instance *)self,0,id,vm);
   return 0;
}

typedef struct{ char *name; uint16_t n,id,lnk; }CLink;

static void filterName(char *name, 
unsigned n, CLink *table, unsigned lnk, unsigned *N)
{
   CLink *clink;
   if (!*name) return;			// don't add private vars ("")
   if (strchr(name,'#')) return;	// don't add "__fcn#1"

   clink = &table[(*N)++];	// I probably should do a overflow check here
   clink->name = name; clink->n = n; clink->lnk = lnk;	// n is linkTable[n]
}

void walkGNtable(unsigned sz, void *,			// object.c
   char *(*poot)(void *X, unsigned,unsigned,int), pVM);

static char *clspoot(void *X, unsigned n,unsigned id,int write)
{
   CLink *clink = &((CLink *)X)[n];
   if(write){ clink->id = id; return 0; }
   return clink->name;
}

static void buildLinkTable(Instance *klass,pVM vm)
{
   unsigned  n,tableLen, sz=0,min=~0,max=0;
   uint16_t *table;
   CLink     slots[3*0xff + MAX_PARENT_TREE]; // max # var, fcn, cls, parent

   unsigned numVars    = NUM_VARS(klass), 
   	    numFcns    = NUM_FCNS(klass),
	    numClasses = NUM_CLASSES(klass),
	    numParents = NUM_LINEAR_PARENTS(klass);

   char     **varNames = VAR_NAMES(klass);
   Instance **classes  = CLASSES(klass), **parents = PARENTS(klass);
   ZKL_Fcn   *fcns     = BASE_FCNS(klass);

//!!! if slots is too big, remove name & change clspoot to do the following
   for(n=0; n<numVars; n++) filterName(varNames[n],n,slots,LNK_VAR,&sz);
   for(n=0; n<numFcns; n++)
     if(!FCN_IS_PRIVATE(&fcns[n])) 
	filterName(FCN_NAME(&fcns[n]),n,slots,LNK_FCN,&sz);
   for(n=0; n<numClasses; n++) 
      if(!IS_PRIVATE(classes[n]))
	 filterName(CLASS_NAME(classes[n]),n,slots,LNK_CLASS,&sz);
   for(n=0; n<NUM_PARENTS(klass); n++) // only the first order parents
      filterName(CLASS_NAME(parents[n]),n,slots,LNK_PARENT,&sz);

   walkGNtable(sz,slots,clspoot,vm);	// get ids

   // calc size of link table
   for(n=0; n<sz; n++)
   {
      unsigned id = slots[n].id;
      if(id>max) max = id;
      if(id<min) min = id;
   }
   for(n=0; n<numParents; n++)	// parent tree
   {
      Instance *p = parents[n];		// parents has been cooked
//!!! this calc is too broad!
      if(CLASS_BASE(p)->minGN < min) min = CLASS_BASE(p)->minGN;
      if(CLASS_BASE(p)->maxGN > max) max = CLASS_BASE(p)->maxGN;
   }

   // allocate link table
   tableLen = sz ? (max - min + 1) : 0;  // NullClass
   table    = calloc(tableLen,2); // likely a bunch of dead entries

   CLASS_BASE(klass)->minGN       = min;   // if tableLen==0, min==~0
   CLASS_BASE(klass)->maxGN       = max;
   CLASS_BASE(klass)->linkTableSz = tableLen*2;
   CLASS_BASE(klass)->linkTable   = table;

   // Parents are last in line; add links in reverse order ("oldest" first)
   // stomping the older parent, which are then stomped by self
   // LNK_PARENT id is one based so I can use zero testing
   for(n=numParents; n--; )
   {
      Instance *p = parents[n];		// parents has been cooked
      unsigned  z, id, pMin = CLASS_BASE(p)->minGN;
      uint16_t *pTable = CLASS_BASE(p)->linkTable;
      
      for(z = pMin; z<=CLASS_BASE(p)->maxGN; z++)
	 if((id = pTable[z - pMin]) && id<=LNK_LOW) // no parents parent
	    table[z - min] = (id | (n + 1)<<LNK_SHIFT);
   }

   for(n=sz; n--; ) table[slots[n].id - min] = (slots[n].lnk | slots[n].n);

   if(min<=notFoundId && notFoundId<=max && 
      (table[notFoundId - min] & LNK_FCN))
      CLASS_BASE(klass)->notFound = 1;	// fcn __notFound{} is in tree
}

int classResolveN(Instance **self,
unsigned id, pMethod *method, void *pFcn, int bitch, pVM vm)
{
   Instance *i;
   unsigned  t;

   if(method) *method = 0;	// just be nice
   i = resolveN((Class *)*self,id,pFcn,&t,vm);
   if(i){ *self = i; return (t==LNK_FCN) ? FcnType : UnknownType; }

	// Object is always the last parent (implicit)
   return objectResolveN(self,id,method,pFcn,bitch,vm);
}

static int classResolve(Instance **self, 
char *name, pMethod *method, void *pFcn, int bitch, pVM vm)
{
   Instance *i;
   unsigned  id;

   if(getGlobalId(name,&id,vm))	// name exists
      return classResolveN(self,id,method,pFcn,bitch,vm);
   i = *self;
   if(NOT_FOUND(i) &&	// __notFound exits
      (i = callNotFound(i,name,id,vm))) 
   {
      *self = i;
      return UnknownType;
   }

   // else not found in this class
	// Object is always the last parent (implicit)
   return objectResolve(self,name,method,pFcn,bitch,vm);
}

    // ONLY for use with opResolveN, opCallN
static Instance *classRecallN(Instance *self,unsigned id,
		      pMethod *method,void *pfcn, pVM vm)
{
   if(IS_COOKED(self))
   {
      Instance *i = _findById((Class *)self,id,pfcn,&id,vm);
      if (i) return i;
//!!! I'm getting kinda sick of notFound
      if(NOT_FOUND(self) &&	// __notFound exits
         !isIdMP(self,id))  // not method or property
      {
	 _findById((Class *)self,notFoundId,pfcn,&id,vm);
	 return pfcn;		// maybe zero
      }
   }

	// Object is always the last parent (implicit)
   objectResolveN(&self,id,method,0,1,vm);
   return self;
}

    // ONLY for use with opRecall
Instance *classRecall(Instance *self,char *name,
		      pMethod *method,void *pfcn, pVM vm)
{
   unsigned id;

   if(getGlobalId(name,&id,vm))	// name exists
      return classRecallN(self,id,method,pfcn,vm);
//!!! I'm getting kinda sick of notFound
   if(NOT_FOUND(self) &&	// __notFound exits
      !isIdMP(self,id))  // not method or property
   {
      _findById((Class *)self,notFoundId,pfcn,&id,vm);
      return pfcn;	// maybe zero
   }

	// Object is always the last parent (implicit)
   objectResolve(&self,name,method,0,1,vm);
   return self;
}

    // Class.BaseClass.resolve(name)
    // Because things like "resolve" & "toFloat" are in self.MethodTable
static Instance *baseClassResolve(Instance *self,pArglist arglist,pVM vm)
{
   char *name = arglistGetOnlyString(arglist,0,"resolve",vm);
   if (0 == strcmp(name,"resolve"))	// BaseClass.resolve("resolve")
      return methodCreate(self,0,(pMethod)baseClassResolve,vm);
   objectResolve(&self,name,0,0,1,vm);
   return self;
}

    /* If a new class is created, it is owned and stashed in fence.
     * If not static, create a [in memory] copy of the constructor or init
     *   fcn so it points to the new class. This is sitting the C stack so
     *   it can't be visible to the VM after your code returns.
     *   It is not on any GC list (ie won't be collected).
     * Returns: Fcn (__constructor or init)
     */
Instance *classPrepForRun(Instance *self,
    Instance **fenceSlot, ZKL_Fcn *f, pVM vm)
{
   Instance *klass;
   ZKL_Fcn  *init;	// this will hold a copy of init bound to self

   	// if there are ANY fcns, init/constructor is OK
   if (0 == NUM_FCNS(self))
      vmThrow(vm,E_ASSERTION_ERROR,"Class: No constructor");

   init = NTH_FCN(self,INIT_FCN(self));	// in class base
   if (IS_STATIC(self)) return (Instance *)init;  // could be the constructor

   klass	    = classCopy(self,vm);	// class base unchanged
   *f		    = *init;		// in memory struct copy, dangling
   FCN_CONTAINER(f) = klass;		// point back to Instance vars
   *fenceSlot       = klass;		// fcn is not collectable, klass is

   return (Instance *)f;	// f is different from init
}

    /* class(...):  Create a new class (unless static), run the
     *   constructor/init and return class (unless the constructor does a
     *   returnClass).
     * Usually, class C() returns a new copy of C, which is result.  If not,
     *   the new class can be reclaimed.
     * Can GC.
     * It is up to the caller to protect arglist.
     */
Instance *classRun(Instance *klass, pArglist arglist, pVM vm)
{
   ZKL_Fcn   fcn;
   Fence     fence;
   Instance *init, *result;

   vmSetFence(vm,&fence,0,0);	// A copy of klass might be created
      init   = classPrepForRun(klass,&fence.i,&fcn,vm);
      result = vmCallFcnFromMethod(init,arglist,0,vm);
   vmRemoveFence(&fence,0);
   return result;
}

    // Can GC
Instance *classRunConstructor(Instance *class, pArglist arglist, pVM vm)
{
   Instance *constructor = (Instance *)NTH_FCN(class,0);
   if (!IS_STATIC(class))
   {
      Instance *result;
      Fence     fence;
      constructor = fcnCopy(constructor,class,vm); // set self in __constructor
      vmSetFence(vm,&fence,0,constructor);
	 result = vm ? vmCallFcnFromMethod(constructor,arglist,0,vm) :
		       fcnRun(constructor,arglist,0,vm);
      vmRemoveFence(&fence,0);
      return result;
   }
   return vm ? vmCallFcnFromMethod(constructor,arglist,0,vm) :
	       fcnRun(constructor,arglist,0,NoVM);
}

#if 0
Instance *classLookupClass(char *fullName, pVM vm)
{
   char	       *ptr, *name = fullName;
   char		buf[100];	// compiler limits individual name lengths
   Instance    *klass;
   int		findClass = 0;

   while (1)
   {
      ptr = strchr(name,'.');
      if (ptr)	// can't write into fullName, seg violation
      {
	 size_t n = ptr - name;
      	 strncpy(buf,name,n); buf[n] = '\0';
      }
      else strcpy(buf,name);
      if (findClass) klass = classFindClass(klass,buf,0);
      else	     klass = vaultFind(buf,0);
      if (!klass)
      {
	 char	buf[200];	// don't know strlen(fullName)
	 sprintf(buf,"classLookupClass(%.80s): Can't find %.80s",fullName,name);
	 vmThrow(vm,E_NOT_FOUND,buf);
      }
      if (ptr) name = ptr + 1; else break;
      findClass = 1;
   }
   return klass;
}
#endif

    /* TheVault.className() for C code
     * DO NOT call this from zkl code
     * Warning: arglist needs to be owned or immortal
     * GC:
     *  - Result is orphan or better.
     *  
     * classRunith("Walker",tupleCreateX(self,...),vm);
     */
#if 0
Instance *classRunith(char *vaultPath, Instance *arglist, pVM vm)
{
   char		buf[150];	// class names are limited to 80 characters
   Instance    *klass;

   klass = vaultFind(vaultPath,vm);
   if (!klass || TYPEO(klass) != ClassType)
   {
      sprintf(buf,"classRunith(%.100s): not a class",vaultPath);
      vmThrow(vm,E_ASSERTION_ERROR,buf);
   }
   return classRun(klass,arglist,0,1,vm);		// throws
}

Instance *	// classRunith("Walker","Walker",mlistBuild(self,...),vm);
classRunith2(char *vaultPath, char *classPath, Instance *arglist, pVM vm)
{
   char		buf[200];	// class names are limited to 80 characters
   Instance    *klass;

   klass = vaultChase2(vaultPath,classPath,vm);
   if (!klass || TYPEO(klass) != ClassType)
   {
      sprintf(buf,"classRunith2(%.100s.%.80s): not a class",
		   vaultPath,classPath);
      vmThrow(vm,E_ASSERTION_ERROR,buf);
   }
   return classRun(klass,arglist,vm);		// throws
}
#endif

    // A root class is a class that not part of another class, basically a
    // file. A static class is a quasi root but not really.
int classIsRootClass(Instance *self)
{
   if (!self) return 0;
   if (CONTAINER(self)) return 0;
   return 1;
}

/* ******************************************************************** */
/* *********************** VM Functions ******************************* */
/* ******************************************************************** */

    /* Returns 0 if n is out of range or the nth class hasn't been defined
     * yet, ie an embryo is created and accessed before all classes are
     * added.
     */
Instance *classNthClass(Instance *self,unsigned n)
{
   if (n < NUM_CLASSES(self)) return CLASSES(self)[n];
   return 0;
}

    // classUp(0) == self. 
    // Bad shit happens if blow off the top of a root class. Don't do that.
Instance *classUp(Instance *self,int n)
{
   while (n--) self = CONTAINER(self);
   return self;
}

/* ******************************************************************** */
/* *************************** Methods ******************************** */
/* ******************************************************************** */

    // Don't proxy
static Instance *toFoo(
Instance *self, char *name,int t,pMethod m, pArglist arglist,pVM vm)
{
   ZKL_Fcn   _fcn;
   Instance *fcn, *r;

   fcn = classFindFcn(self,name,1,&_fcn,vm);
   if (fcn)
   {
      r = vmCallFcnFromMethod(fcn,arglist,0,vm);// don't need to protect result
      if (TYPEO(r) != t)
      {
	 char buf[200];
	 sprintf(buf,"%s.%s() must return %s",
		 className(self),name,typeToName(t));
	 vmThrow(vm,E_TYPE_ERROR,buf);
      }
      return r;
   }
   if (m == (pMethod)1) return Zero;	// .len()
   if (m) return m(self,arglist,vm);
   return cantConvertError(self,name,vm);
}


    // .toBool() -->self.toBool()
    // The compiler calls Object.toBool, which tampolines to here
    // which bypasses fcn toBool, which lets me always typecheck
    // Except: f := class { fcn toBool {5} }.toBool; f()
//!!! shouldn't the index of fcn toBool be cached?
static Instance *Class_toBool(Instance *self,pArglist arglist,pVM vm)
   { return toFoo(self,"toBool",BoolType,Bool_soTrue, arglist,vm); }

    // .toInt() -->self.toInt()
static Instance *Class_toInt(Instance *self,pArglist arglist,pVM vm)
   { return toFoo(self,"toInt",IntType,0, arglist,vm); }

    // .toFloat() -->self.toFloat()
static Instance *Class_toFloat(Instance *self,pArglist arglist,pVM vm)
   { return toFoo(self,"toFloat",FloatType,0, arglist,vm); }

    // .BaseClass("toString") -->String
static Instance *Class_baseToString(Instance *self,pArglist arglist,pVM vm)
{
   char buf[100];

   sprintf(buf,"Class(%s)",CLASS_NAME(self));
   return stringCreate(buf,I_OWNED,vm);
}

    // .toString() -->String
static Instance *Class_toString(Instance *self,pArglist arglist,pVM vm)
   { return toFoo(self,"toString",StringType,Class_baseToString, arglist,vm); }

    // .len() -->Int | 0
static Instance *Class_len(Instance *self,pArglist arglist,pVM vm)
   { return toFoo(self,"len",IntType,(pMethod)1, arglist,vm); }


static Instance *_cmethod(
Instance *self, unsigned id, char *name,pArglist arglist,pVM vm)
{
   ZKL_Fcn   _fcn;
   Instance *f;

   f = classFindFcnById(self,id,1,&_fcn,vm);
   if (f) return vmCallFcnFromMethod(f,arglist,0,vm);
   {
      return notImplementedError(self,name,vm);
   }
}

    // .__sGet
static Instance *Class_sGet(Instance *self,pArglist arglist,pVM vm)
   { return _cmethod(self,sGetId,"__sGet",arglist,vm); }

    // .__sSet
static Instance *Class_sSet(Instance *self,pArglist arglist,pVM vm)
   { return _cmethod(self,sSetId,"__sSet",arglist,vm); }

    /* Definition: if Ci is C or C() then:
     * A.isInstanceOf(Ci) is True iff:  A == Ci or A == Ci() (however many
     *     times removed).
     * class B(A) --> 
     *    ClassA.isInstanceOf(ClassA) --> True
     *    ClassA.isInstanceOf(ClassB) --> False
     *    ClassB.isInstanceOf(ClassA) --> False
     * isInstanceOf differes from isType() only for classes and fcns.
     * A is self.
     */
int classIsInstanceOf(Instance *self,Instance *B)
{
   if (TYPEO(self) != ClassType || TYPEO(B) != ClassType) return 0;
   return (CLASS_BASE(self) == CLASS_BASE(B));
}

    // .isInstanceOf(class ...) -->Bool
static Instance *Class_isInstanceOf(Instance *self,pArglist arglist,pVM vm)
{
   int		n;
   Instance    *B;
   for (n = 0; (B = listGet(arglist,n)); n++)
      if (classIsInstanceOf(self,B)) return BoolTrue;
   return BoolFalse;
}

     /* A is a child of B iff:
      *    A or a parent of A is an instance of B
      * thus A is a child of itself
      * A is self.
      */
int classIsChildOf(Instance *self,Instance *B)
{
   if (!self || !B) return 0;
   if (TYPEO(self) != ClassType || TYPEO(B) != ClassType) return 0;
   if (CLASS_BASE(self) == CLASS_BASE(B)) return 1;

   {
      int n;
      Instance  **parents = PARENTS(self);
      ClassBase  *baseB   = CLASS_BASE(B);
      for (n=NUM_LINEAR_PARENTS(self); n--; parents++)
	 if(baseB == CLASS_BASE(*parents)) return 1;
      return 0;
   }
}

    // .isChildOf(B) -->Bool
static Instance *Class_isChildOf(Instance *self,pArglist arglist,pVM vm)
{
   Instance *B = arglistGet(arglist,0,"Class.isChildOf",vm);
   return boolCreate(classIsChildOf(self,B));
}

    // .sourceCode -->?
static Instance *Class_sourceCode(Instance *self,pArglist arglist,pVM vm)
   { return emptyString; }

    // .unasm([outputStream]) --> Compiler.Asm.disClass(self [,outputStream])
static Instance *Class_unasm(Instance *self,Instance *arglist,pVM vm)
{
   MLIST(mlist,2);
   Instance *args = mlistBuild(mlist,self,ZNIL);
   Instance *pi   = arglistTryToGet(arglist,0);

   if (pi) mlistAppendI(mlist,pi,2);
   return fcnRunith("Compiler.Asm","disClass",args,vm);
}

    // .launch(args for thread): Create new thread -->self (old thread)
static Instance *Class_launch(Instance *self,pArglist arglist,pVM vm)
{
   threadCreate(self,0,arglist,0,vm);
   return self;
}

    /* .resolve(name): Regular resolve -->value
     * .resolve(name,N,searchParents=True): N is bits: -->Bool | nonzero int
     *        1: Method		--> MethodType
     *        2: Property	--> PropertyType
     *        4: Is name var/fcn/class/parent?
     *        8: Is name var?
     *     0x10: Is name [immediate/first level] parent?
     *     0x20: Is name fcn?
     *     0x40: Is name class?
     *  .resolve(name,*):      All of the above -->Bool | nonzero Int
     *  .resolve(name,*,Bool): All of the above -->Bool | nonzero Int
     *  .resolve(name,N,Void): -->index or Void
     *  .resolve(name,False) --> self.resolve(name), throws
     *  .resolve(name,Void)  --> self.BaseClass.resolve(name), throws
     * See .whatIsThis to get the actual value
     */
static Instance *Class_resolve(Class *self,pArglist arglist,pVM vm)
{
   char	    *name = arglistGetOnlyString(arglist,0,"Class.resolve",vm);
   Instance *p1   = arglistTryToGet(arglist,1);
   Instance *i;

   if(!IS_COOKED(self))
      vmThrow(vm,E_ASSERTION_ERROR,"Class.resolve: class not cooked");
   if (p1)	// .resolve(name,?,...)
   {
      int	n, deep;
      unsigned  s;
      Instance *p3 = arglistTryToGet(arglist,2);

      if (p3 == Void)	// .resolve(name,N,Void) --> index or Void
      {
	 if (!_getSlot((Class *)self,name,0,&s,vm)) return Void;
	 n = s & 0xff; s &= LNK_BITS;
	 switch((int)convertToInt(p1,vm))
	 {
	    default: return Void;
	    case 0x40: if(s!=LNK_CLASS)  return Void; break; // class
	    case 0x20: if(s!=LNK_FCN)    return Void; break; // function
	    case 0x10: if(s!=LNK_PARENT) return Void; break; // parent
	    case 0x08: if(s!=LNK_VAR)    return Void; break; // var
	 }
	 return intCreate(n,vm);
      }

      if (p1 == BoolFalse)	// .resolve(name,False) --> ignore __notFound
      {
	 if((p1 = _findByName(self,name,0xff,1,0,vm))) return p1;
      bcResolve:
	 objectResolve((Instance **)&self,name,0,0,1,vm);
	 return (Instance *)self;
      }
      if (p1 == Void)		// .resolve(name,Void) --> BaseClass.resolve
      {
	 if (0 == strcmp(name,"resolve"))
	    return methodCreate((Instance *)self,0,(pMethod)baseClassResolve,vm);
	 goto bcResolve;
      }

      // .resolve(name,N,...) --> True/False
      n = 0; deep = 1;

      if (p1 == Star) n = 0xFF;
      else if (TYPEO(p1) == IntType) n = (int)convertToInt(p1,vm);

      if (p3) deep = resultToBool(p3,vm);

      if (n & 0xFC)	// could well be 0xFF
      {
	 i = (Instance *)_getSlot((Class *)self,name,deep,&s,vm);
	 if (i)
	 {
	    if (n & 0x04) return BoolTrue;	// found something
	    switch(s & LNK_BITS)
	    {
	       case LNK_VAR:    if (n & 0x08) return BoolTrue; break;
	       case LNK_FCN:    if (n & 0x20) return BoolTrue; break;
	       case LNK_CLASS:  if (n & 0x40) return BoolTrue; break;
	       case LNK_PARENT: if (n & 0x10) return BoolTrue; break;
	    }
	 }
	 // fallthrough
      }
      i = (Instance *)self;
      if ((n & 1) && searchForMethod(  i,name,1,vm)) return intCreate(MethodType,vm);
      if ((n & 2) && searchForProperty(i,name,1,vm)) return intCreate(PropertyType,vm);

      return  BoolFalse;
   }

   classResolve((Instance **)&self,name,0,0,1,vm);
   return (Instance *)self;
}

    // .whatIsThis(name,searchParents=True,forReal=False) 
    //   -->T(type,container,index,instance) | (Void,Void,Void,Void)
    // A slightly different take on .resolve, no proxies/Methods/Properties
static Instance *Class_whatIsThis(Class *self,pArglist arglist,pVM vm)
{
   char     *name    = arglistGetOnlyString(arglist,0, "Class.whatIsThis",vm);
   int	     d	     = arglistTryToGetBool(arglist,1,1,"Class.whatIsThis",vm);
   int	     forReal = arglistTryToGetBool(arglist,2,0,"Class.whatIsThis",vm);
   Instance *klass, *i;

   unsigned s,t=0;

   if(!IS_COOKED(self))
      vmThrow(vm,E_ASSERTION_ERROR,"Class.whatIsThis: class not cooked");

   klass = (Instance *)_getSlot(self,name,d,&s,vm);
   if (!klass) return tupleCreateX(vm,Void,Void,Void,Void,ZNIL);
   switch(s & LNK_BITS)
   {
      case LNK_VAR:    t = ClassVarType; break;
      case LNK_FCN:    t = FcnType;	 break;
      case LNK_CLASS:  t = ClassType;	 break;
      case LNK_PARENT: t = ParentType;	 break;
   }

   i = _findByName(self,name,s & LNK_BITS,d,0,vm);
   if (forReal)	// act more like .resolve
      return tupleCreateX(vm,intCreate(t,vm),klass,Void,i,ZNIL);
   //else		// more information!
   {
      if (t == ClassVarType) i = Void;	// no vars, I don't know why
      return tupleCreateX(vm,intCreate(t,vm),klass,intCreate(s&0xff,vm),i,ZNIL);
   }
}

    // .BaseClass(name) --> mini .resolve(name)
Instance *Class_BaseClass(Instance *self,pArglist arglist,pVM vm)
{
   char *name = arglistGetOnlyString(arglist,0,"Class.BaseClass",vm);
   unsigned id;

   if(getGlobalId(name,&id,vm))
   {
      if(id==toBoolId)  return methodCreate(self,0,Bool_soTrue,vm);
      if(id==toIntId)   return cantConvertError(self,"toInt",vm);
      if(id==toFloatId) return cantConvertError(self,"toFloat",vm);
      if(id==toStringId)
	    return methodCreate(self,0,(pMethod)Class_baseToString,vm);
      if(id==lenId) return Zero;
      if(id==resolveId)
	    return methodCreate(self,0,(pMethod)baseClassResolve,vm);
   }

   objectResolve(&self,name,0,0,1,vm);
   return self;
}

/* ******************************************************************** */
/* ************************** Class Creation ************************** */
/* ******************************************************************** */

#define CLASS_INSTANCE_SIZE(numVars,numClasses,totalParents) \
   ( sizeof(Class) + (numVars + numClasses + totalParents)*sizeof(Instance *) )
#define CLASS_ISIZE(self) \
   CLASS_INSTANCE_SIZE(NUM_VARS(self),NUM_CLASSES(self),NUM_LINEAR_PARENTS(self))

    /* Memory needed: Space for a new Class structure
     *   Arrarys of pointers to instance data:
     *   	vars, fcns, classes and parents
     * The ClassBase has its refCount incremented.
     * The copy is Owned.
     * 
     * Warnings!
     *  - True copy of vars:  If a var contains a copy of another class and
     *    that class has a var with a copy of this class, infinite recursion
     *    (eg the compiler and parser).
     *  - Multiple threads can be making copies of the same class (eg
     *    IndexError).  Have to take care if making changes to ClassBase.
     *  - ALL the functions point back to the original fcns in ClassBase.
     *    They are not copied with new container pointers.
     *    Class C Eve: *Fcn[n] --> Base.fcns (pointers)
     *    Class C Base: Fcn[n].container --> Eve <-- Fcns live here
     *    Class C():   *Fcn[n] --> Base.fcns --> Eve (pointers)
     *  - If the class has static parents or classes, the class (but not the
     *    static part) can be reclaimed as .topdog & .container don't point
     *    to the new class. eg class C { class [static] D{}} d = C().D;
     *  - addToCollectables() can start gc
     */
    // pack entire class tree in one memory blob
static unsigned classSizeXL(Instance *, int includeStatic,
			    unsigned *, unsigned *, unsigned *);

    // linearizeParents() helpers:
    // Count the number of parents visited (not including self)
static Instance *cntParents(Class *p,void *n,pVM vm)
{
   unsigned *z = ((unsigned *)n);
   (*z)++;
   return 0;
}
    // Add parents to parent tree
typedef struct{ unsigned n,offset; Instance **lp; } LPW;
static Instance *fillSearchTree(Class *p,void *_lpw,pVM vm)
{
   LPW      *lpw = (LPW *)_lpw;
   unsigned  n = (lpw->n)++, offset = lpw->offset;
   if(n>=offset) lpw->lp[n - offset] = (Instance *)p;
   return 0;
}

    /* This version of classCopy packs the entire class into one chunk of 
     * memory, instead of one malloc per class/parent. 
     * Class C(P) { class D {} } would otherwise be at least 3 mallocs.
     * Furthur, only the root of the copy is visible to GC, which shrinks
     * the number of objects GC has to scan.
     * Locality should be improved a bit but the class base and vars are who
     *   knows where.  At least malloc/free won't have to do as much work
     *   (and as they are choke points, that is a good thing).
     * Results: I'm seeing a bit of a speed up but a bit bigger memory foot
     *   print, overall, not much improvement.
     * This does the same amount of work to copy "lots of parts" classes as
     *   copies or contained classes, not sure if it would be a win to, for
     *   copies, to do a bit copy and fixup. Probably not, most things
     *   change.
     */
     // The root has iflag2 == 0, chunkettes have iflag2 == 1
     // All classes have ifag == 1 (copies of a class)
static Class *_classCopy(Class *self,
	Instance *topdog, Byte *putBitsHere, Class *root, int copyVars, pVM vm)
{
   Class     *newClass;
   unsigned   size;
   unsigned   i,numVars,numClasses,numParents,numLinearParents;
   Instance **classData;

   if (!self)		// eg incomplete embryos
      vmThrow(vm,E_ASSERTION_ERROR,"Class.copy: self is zero");

   if (self == (Class *)NullClass) return (Class *)NullClass;
   if (IS_STATIC(self))		   return self;

   numVars          = NUM_VARS(self);
   numClasses       = NUM_CLASSES(self);
   numParents       = NUM_PARENTS(self);
   numLinearParents = NUM_LINEAR_PARENTS(self);

   if (!putBitsHere)	// the "head" of the copy
   {	// this is the only allocation for this copy, ie no GC after this
      size = CLASS_BASE(self)->isize;
      if(!size)
	 vmThrow(vm,E_ASSERTION_ERROR,"Can't copy an uncooked class");
      newClass    = (Class *)instanceAllocate(size,&GlobalClassObject,1,vm);
      putBitsHere = (Byte *)newClass;
      root	  = newClass;
      topdog	  = (Instance *)newClass;

      newClass->topdog = 0;	// not a parent unless added as one
      iCount++;
   }
   else
   {
   #if 0 && USE_POINTER_INTS  // don't confuse a contained class with a PtrInt!
      if (IS_PtrInt(putBitsHere))
	 vmThrow(vm,E_VM_ERROR,"_classCopy bad alignment, adjust padding");
   #endif
         // Contained classes & parents are NOT put on a GC list & NOT marked.
	 // If GC sees them (in a var), they are used to mark the root
      newClass = (Class *)putBitsHere;
      instanceInit((Instance *)newClass,&GlobalClassObject,I_2SPECIAL);
      newClass->instance.iflag2 = 1;	// this is a "chunkette" of the copy
      newClass->instance.nexti  = (Instance *)root;	// re-purpose
      newClass->topdog		= topdog;
   }
   size = CLASS_INSTANCE_SIZE(numVars,numClasses,numLinearParents);
   putBitsHere += size;

   newClass->instance.iflag = 1;	// this is a copy

   newClass->classBase	= CLASS_BASE(self);
   newClass->container	= CONTAINER(self);	// 0 for roots

   classData	  = newClass->data;
   newClass->fcns = BASE_FCNS(self);
   newClass->co	  = (unsigned)((char *)&classData[numVars] - (char *)newClass);
   newClass->po	  = (unsigned)((char *)&classData[numVars + numClasses] - 
				     (char *)newClass);

   	// initialize vars to a known or GCable state
     if(copyVars) for (i = 0; i<numVars; i++) *classData++ = VARS(self)[i];
     else         for (i = numVars; i--;    ) *classData++ = Void;

   for (i = numClasses; i--; ) *classData++ = NullClass;	// classes
   for (i = numParents; i--; ) *classData++ = NullClass;	// parents

   CAI_INC(&CLASS_BASE(self)->refCount);   // needs to be thread safe

   for (i = 0; i < numClasses; i++)
   {
      Class *klass = (Class *)CLASSES(self)[i];
      if (!IS_STATIC(klass))
      {
	 klass = _classCopy(klass,0,putBitsHere,root,copyVars,vm);
	 putBitsHere += CLASS_BASE(klass)->isize;
	 CONTAINER(klass) = (Instance *)newClass;
      }
      CLASSES(newClass)[i] = (Instance *)klass;
   }
   for (i = 0; i < numParents; i++)
   {
      Class *parent = (Class *)PARENTS(self)[i];
      if (!IS_STATIC(parent))
      {
	 parent = _classCopy(parent,topdog,putBitsHere,root,copyVars,vm);
	 putBitsHere += CLASS_BASE(parent)->isize;
      }
      PARENTS(newClass)[i] = (Instance *)parent;
   }

   if(numLinearParents > numParents)	// fill out parent search tree
   {
      LPW lpw = { 0,numParents,&PARENTS(newClass)[numParents] };
      linearizeParents(newClass,fillSearchTree,&lpw,vm);
   }
   if (!newClass->instance.iflag2)	// only the root is registered
      addToCollectables((Instance *)newClass,I_OWNED,vm);

   return newClass;
}

Instance *classCopy(Instance *self,pVM vm)
   { return (Instance *)_classCopy((Class *)self,0,0,0,0,vm); }

    // .copy([duplicate)) -->Class
static Instance *Class_Copy(Instance *self,pArglist arglist,pVM vm)
{
   int copyVars = arglistTryToGetBool(arglist,0,0,"",vm);
   return (Instance *)_classCopy((Class *)self,0,0,0,copyVars,vm);
}

    // .create(args) == self()
static Instance *Class_create(Instance *self,pArglist arglist,pVM vm)
   { return classRun(self,arglist,vm); }

size_t _sumList(Instance *list)
{
   Instance *s;
   size_t    total = 0,n;

   for (n = 0; (s = listGet(list,n++)); )
      total += strlen(stringText(s)) + 1;
   return total;
}

static char *noName = "";

    /* One unfortunate fact of life:  Code will create an embryo and use it
     *   before it is complete.  GC will be poking at this before it is
     *   complete also.
     * The var names must be sorted.
     * New class is created, and returned, INVISIBLE
     */
static Class *_classEmbryo(
   size_t sizeNameSpace,int numVars, 
   int numFcns,int numClasses,int numParents, int numLinearParents,
   char **text, pVM vm)
{
   Class     *class;
   ClassBase *base;
   int	      i;
   unsigned   bsize;
   Instance **classData;
   Byte	     *ptr, *baseData;
   ZKL_Fcn   *pf;

   	// numFcns can be zero (no constructor) for "data container" classes
	// eg creating fcn default args.  See fcn.c:createAsmDotCode()
   if (numFcns < 0 || numVars < 0 || numClasses < 0 || numParents < 0)
      vmThrow(vm,E_VALUE_ERROR,"class.embryo: Negative parameter");
   if (numVars > 254 || numFcns > 255 || numClasses > 255 || numParents > 255)
      vmThrow(vm,E_VALUE_ERROR,
	"class.embryo: 255 is the max for any of: "
	"vars, functions, classes or parents.");

	// not all objects will be added to the patch panel (eg private fcns)
   bsize = sizeof(ClassBase)		+
	   numFcns * sizeof(ZKL_Fcn)	+	// Fcn theFcns[numFcns]
	   numVars*sizeof(char *)	+	// varNames[numVars]
	   sizeNameSpace		+	// char text[]
	   2*VAR_BIT_TABLE_SIZE(numVars)*sizeof(Byte);

   base  = (ClassBase *)ZCALLOC(1,bsize);
   if (!base) vmThrow(vm,E_OUT_OF_MEMORY,0);

   baseCount++;
   CAI_INIT2(&base->refCount,1);
   base->initIsFcnN = 0;
   base->numVars    = numVars;
   base->numFcns    = numFcns;
   base->numClasses = numClasses;
   base->numParents = numParents;
   base->numLinearParents = numLinearParents; // not known yet
   base->bsize	    = bsize;
   base->isize	    = 0;
   base->notFound   = 0;
   base->isPrivate  = 0;
   base->minName    = 200;	// bigger than any name, maxName == 0
   baseData	    = (Byte *)base->data;

	// setup the arrays and stub them out
   ptr = baseData;

   pf = (ZKL_Fcn *)ptr; ptr += numFcns*sizeof(ZKL_Fcn);
   for (i = numFcns; i--; pf++) *pf = *(ZKL_Fcn *)nullFcn;

   	// initialize varNames to ""
   for (i = numVars; i--; ptr += sizeof(char *)) *((char **)ptr) = noName;

   *text = (char *)ptr; ptr += sizeNameSpace;

   base->varABitsO   = ptr - baseData;

   // name, vaultPath and varNames are copied to name space by caller
   // The patch panels are filled in as fcns and classes are added
//??? tables have been zero'd

   	// now create the Class instance
   class = (Class *)instanceAllocate(
      CLASS_INSTANCE_SIZE(numVars,numClasses,numLinearParents),
      &GlobalClassObject,0,vm);
   if (!class)
   {
      ZFREE(base);
      vmThrow(vm,E_OUT_OF_MEMORY,0);
   }
   iCount++;
   class->classBase = base;
   class->topdog    = 0;
   class->container = 0;

   classData   = class->data;
   class->fcns = (ZKL_Fcn *)baseData; // initialized above, in classBase
   class->co   = (unsigned)((char *)&classData[numVars] - (char *)class);
   class->po   = (unsigned)((char *)&classData[numVars + numClasses] - 
			       (char *)class);

   	// initialize to a known state (fcns done above)
   for (i = numVars;    i--; ) *classData++ = Void;		// vars
   for (i = numClasses; i--; ) *classData++ = NullClass;	// classes
   for (i = numParents; i--; ) *classData++ = NullClass;	// parents

   base->cpFull = !numClasses + !numParents;

   return class;
}

    /* names = StringTable[className,vaultPath,varNames]
     * !!IMPORTANT!! names MUST be static - they are NOT copied
     * The var names must be sorted
     * No parents, use zero,zero
     */
Instance *classEmbryo(StringTable *names,
	unsigned numFcns,unsigned numClasses,
	unsigned numParents,Instance **parents,
	int itype, pVM vm)
{
   char	     *ptr, **varNames;
   Class     *class;
   ClassBase *base;
   unsigned   i,n, numVars = names->n - 2, numLinearParents = 0;
   
   if(parents)
   {
      _linearizeParents(numParents,parents,cntParents,&numLinearParents,vm);
      if(numLinearParents>MAX_PARENT_TREE) vmThrow(vm,E_ASSERTION_ERROR,
   	"class.embryo: Max 30 length parent tree exceeded");
      if(numLinearParents<numParents)
	vmThrow(vm,E_ASSERTION_ERROR,"class.embryo: Duplicate parent");
   }
   if (names->n != numVars + 2)
     vmThrow(vm,E_ASSERTION_ERROR,"class.embryo: Wrong number of names");

   class = _classEmbryo(0,numVars,numFcns,numClasses,
			numParents,numLinearParents,&ptr,vm);
   base  = class->classBase;

   ptr = names->strings;
   base->name	   = ptr;
   base->vaultPath = ptr = stNextString(ptr);
   ptr		   = stNextString(ptr);  // skip to start of var names
   varNames	   = BASE_VAR_NAMES(base);
   for (i = 0; i < numVars; i++)
   {
      *varNames++ = ptr;
      ptr = stNextString(ptr);
   }

   for(n=0; n<numParents; n++)   // these are place holder parents
      classAddParent((Instance *)class,parents[n],n,vm);
   // Temp fill out parent search tree with place holder parents
   // so I can look at the parent tree pre cook
   if(numLinearParents > numParents)
   {
      LPW lpw = { 0,numParents,&PARENTS(class)[numParents] };
      linearizeParents(class,fillSearchTree,&lpw,vm);
   }

   return addToCollectables((Instance *)class,itype,vm);
}

    /* New class is INVISIBLE
     * Names: L("className","vaultPath",varNames)
     * The var names must be sorted
     */
static Class *
classEmbryo2(Instance *names,int numFcns,int numClasses,int numParents,pVM vm)
{
   char	     *ptr;
   Class     *class;
   ClassBase *base;
   size_t     size, n;

   verifyList("class.embryo",names,StringType,vm);

   n     = listLen(names,vm);
   size  = _sumList(names);
   class = _classEmbryo(size,(int)n - 2,numFcns,numClasses,numParents,numParents,&ptr,vm);
   base  = class->classBase;

	// fill in the names array
   {
      char **varNames = BASE_VAR_NAMES(base);
      size_t i;
      for (i = 0; i < n; i++)
      {
	 char *s = stringText(listGet(names,i));
	 strcpy(ptr,s); 
	 if      (i == 0) base->name      = ptr;
	 else if (i == 1) base->vaultPath = ptr;
	 else *varNames++ = ptr;		// a var

	 ptr += strlen(s) + 1;
      }
   }

   return class;
}

    /* .embryo(names, numFcns, numClasses, L(parents),
     *	      [L(roBits,proxyBits),attributes])
     * names = L(className, vaultPath, varNames)
     * Careful with parent names: what the compiler uses as parent name
     * (eg "Exception.IndexError") may not be the actual parent's name.
     * It would be nice to build the entire class all at once but I need the
     *   ability to refence contained classes when building a class from a
     *   ZSC:  class A{ class B{} class C(B){} } can't build C until I
     *   have A.
     */
static Instance *Class_embryo(Instance *self,pArglist arglist,pVM vm)
{
   Instance *names	= arglistGet(	     arglist,0,"Class.embryo",vm);
   unsigned  numFcns	= (int)arglistGetInt(arglist,1,"Class.embryo",vm);
   unsigned  numClasses	= (int)arglistGetInt(arglist,2,"Class.embryo",vm);
   unsigned  numParents;
   Instance *parents	= arglistGetBT(arglist,3,ListType,"Class.embryo",vm);
   Instance *varBits	= arglistTryToGetBT( arglist,4,ListType,"Class.embryo",vm);
   char	    *attributes	= arglistTryToGetString(arglist,5,"Class.embryo",vm);
   Class    *class;

   {
      ClassBase *base;
      unsigned   numLinearParents = 0, n;
      Instance  *ps[32];	

      verifyList("class.embryo",parents,ClassType,vm);
      numParents = listLen(parents,vm);
      if(numParents>0x1f) vmThrow(vm,E_ASSERTION_ERROR,
	 "class.embryo: Max 31 length parents exceeded");

      for(n=0; n<numParents; n++) ps[n] = listGet(parents,n);
      _linearizeParents(numParents,ps,cntParents,&numLinearParents,vm);
      if(numLinearParents>MAX_PARENT_TREE) vmThrow(vm,E_ASSERTION_ERROR,
	 "class.embryo: Max 30 length parent tree exceeded");
      if(numLinearParents<numParents)
	 vmThrow(vm,E_ASSERTION_ERROR,"class.embryo: Duplicate parent");
      class = classEmbryo2(names,numFcns,numClasses,numLinearParents,vm);
      base = class->classBase;
      base->numParents       = numParents;
      base->numLinearParents = numLinearParents;
      for(n=0; n<numParents; n++) 	// place holder parents
         classAddParent((Instance *)class,ps[n],n,vm);
      // Temp fill out parent search tree with place holder parents
      // so I can look at the parent tree pre cook
      if(numLinearParents > numParents)
      {
	 LPW lpw = { 0,numParents,&PARENTS(class)[numParents] };
	 linearizeParents(class,fillSearchTree,&lpw,vm);
      }
   }

addToCollectables((Instance *)class,I_OWNED,vm);
   if (varBits)
   {
      char *bits;
      int   i;
      for (i=0; i < NUM_VAR_BITTERS && 
		(bits = arglistTryToGetString(varBits,i,0,vm)); i++) 
	classSetVarBits((Instance *)class,bits,i);
   }
   if (attributes) classSetAttributes((Instance *)class,attributes,vm);

   return (Instance *)class;
}

void classCookClass(Instance *klass, pVM vm)
{
   unsigned   n, numParents = NUM_PARENTS(klass);
   Instance **data;

   if (IS_COOKED(klass)) return;	// my work here is done

       // cook contained classes
   for(n = numParents, data = PARENTS(klass); n--; data++)
   {
      Instance *parent = *data, *clone;

      classCookClass(parent,vm);
      if (!IS_STATIC(parent))
      {
	   // The parent probably exists somewhere as a plain ole class
	   // so I need to make a copy for my own personal use.
	   // I don't know how to verify one way or the other
//if (!(parent->iflag && TOP_DOG(parent) == klass)) {	// don't copy
	 clone = classCopy(parent,vm);
      	 TOP_DOG(clone) = klass;
	 *data = clone;
      }
   }

   for(n = NUM_CLASSES(klass), data = CLASSES(klass); n--; )
      classCookClass(*data++,vm);

   // build parent list in search order (may drop some parents)
   // and append to parent list
   // Do it here because parents are cloned here
   if(NUM_LINEAR_PARENTS(klass) > numParents)	// fill out parent search tree
   {
      LPW lpw = { 0,numParents,&PARENTS(klass)[numParents] };
      linearizeParents((Class *)klass,fillSearchTree,&lpw,vm);
   }

   if (IS_STATIC(klass)) CLASS_BASE(klass)->isize = 1;	// symbolic
   else CLASS_BASE(klass)->isize = classSizeXL((Instance *)klass,0,0,0,0);

   buildLinkTable(klass,vm);
}

    // .cook(): cook self
static Instance *Class_cook(Instance *self,pArglist arglist,pVM vm)
{
   classCookClass(self,vm);
   return self;
}

    // Not thread safe
    // A copy is made and Fcn.container is changed to point to self.
void classAddFcnN(Instance *self,Instance *fcn,unsigned n,pVM vm)
{
   char    buf[200], *name = fcnName(fcn);
   ZKL_Fcn tmp, *pf;

   if (self->iflag) vmThrow(vm,E_ASSERTION_ERROR,
		"Class.addFcn: Copies of classes are read only");
   if (TYPEO(fcn) != FcnType)
      vmThrow(vm,E_ASSERTION_ERROR,"Class.addFcn: fcn isn't");
   if (n >= NUM_FCNS(self))
   {
      sprintf(buf,"%s.addFcn(%s,%d): n is out of range",
	      CLASS_NAME(self),name,n);
      vmThrow(vm,E_INDEX_ERROR,buf);
   }
   if (IS_COOKED(self)) vmThrow(vm,E_ASSERTION_ERROR,
		"Class.addFcn: Class is cooked, no changes");

   pf = NTH_FCN(self,n);
   if (pf->instance.iflag)
   {
      sprintf(buf,"%s.addFcn(%s): Fcn %d already set.",
	      CLASS_NAME(self),name,n);
      vmThrow(vm,E_ASSERTION_ERROR,buf);
   }

	// stash a copy to minimize self update time
   tmp = *(ZKL_Fcn *)fcn;	// struct copy
   fcn = (Instance *)&tmp;
   tmp.container = self;

   	// flick some bits before fcn hits self where gc can see it
   GC_CLEAR_MARK(fcn);
   GC_TYPE(fcn) = I_2SPECIAL;	// An embedded copy of the fcn
   fcn->iflag   = 1;		// make this instance of the fcn runnable
   fcn->iflag2  = 1;		// bound

   	// copy fcn bits into baseClass
   pf  = &BASE_FCNS(self)[n];
   *pf = *(ZKL_Fcn *)fcn;	//!!! arg! gc is looking at this right now
   fcn = (Instance *)pf;

   if (0 == strcmp(FCN_NAME(fcn),INIT_FCN_NAME)) INIT_FCN(self) = n;

   	// buid patch panel
   if (fcnIsInstanceOf(fcn,nullFcn)) return;	// don't add nullFcn
   if (n == 0) return;		// don't put the constructor in the patch panel
   if (FCN_IS_PRIVATE(fcn)) return;
}

    // .addFcn(fcn [,n]) -->fcn
    // Make a copy of fcn and stuff it into self
    // Not thread safe
static Instance *Class_addFcn(Instance *self,pArglist arglist,pVM vm)
{
   int64_t   n;
   Instance *fcn = arglistGetBT(arglist,0,FcnType,"Class.addFcn",vm);
   int	     s   = arglistTryToGetInt(arglist,1,&n,0,vm);

   if (s) classAddFcnN(self,fcn,(unsigned)n,vm);
   else
   {
      int      n, N = NUM_FCNS(self);
      ZKL_Fcn *fcns = THE_FCNS(self);

      for (n = 0; (n < N) && (fcns++)->instance.iflag; n++) ;
      classAddFcnN(self,fcn,n,vm);
   }
   return fcn;	// this way I don't have to orphanize fcn
}

    // DON'T add NullClass
    // Append class, IN ORDER of calls
void classAddClass(Instance *self,Instance *class,pVM vm)
{
   int	      n, N = NUM_CLASSES(self);
   Instance **data = CLASSES(self);

   if (self->iflag) vmThrow(vm,E_ASSERTION_ERROR,
		"Class.addClass: Copies of classes are read only");

      // No cycles: don't add class that contains self.
      // Don't add a class contained in this class
   if (2 != CP_FULL(class)) vmThrow(vm,E_ASSERTION_ERROR,
		"Class.addClass: Doesn't have all classes");

   if (IS_COOKED(self)) vmThrow(vm,E_ASSERTION_ERROR,
		"Class.addClass: Class is cooked, no changes");

   if (class == NullClass) return;

   for (n = 0; (n < N) && (NullClass != *data++); n++) ;
   if (n >= N)
   {
      char buf[200];
      sprintf(buf,"%s.addClass(%s,%d): Class is full of classes.",
	      CLASS_NAME(self),CLASS_NAME(class),n);
      vmThrow(vm,E_ASSERTION_ERROR,buf);
   }

      // No recursion
   if (classIsChildOf(class,self)) vmThrow(vm,E_ASSERTION_ERROR,
		"class.addClass: Can't add children of self");

//none of my classes are related to container
//none of my parents are related to container

   if (self == NullClass) return;

   if (!IS_STATIC(class))  // not static, must be a [copy of an] embryo
   {
         // Don't add same class instance to multiple classes
      if (CONTAINER(class)) vmThrow(vm,E_ASSERTION_ERROR,
	  	"class.addClass: Class is married");
      CONTAINER(class) = self;
   }
   CLASSES(self)[n] = class;

   if (n+1 == N) CP_FULL(self) += 1;	// or at least half full
}

    // .addClass(class) -->class
    /* DON'T spread class creation across threads:
     * T1:Class C, T2:d = class D. 
     * T1 marked, C.addClass(d), T2:d = Void, T2 marked, d gone
     * Unless T1 reads d from Pipe.
     */
static Instance *Class_addClass(Instance *self,pArglist arglist,pVM vm)
{
   Instance *class = arglistGetBT(arglist,0,ClassType,"Class.addClass",vm);
   classAddClass(self,class,vm);
instanceIsOrphan(class);
   return class;
}

    // Create the class hierarchy
    // No circular references please
    // Not thread safe
    // Parent will be cooked and copied when class is cooked
    // Add parents in order
static void classAddParent(Instance *self,Instance *parent,int n,pVM vm)
{
   int	N = NUM_PARENTS(self);
   char buf[200];

   if (NO_CHILDREN(parent))
   {
      sprintf(buf,"Class.addParent: Can't inherit from %s",CLASS_NAME(parent));
      vmThrow(vm,E_ASSERTION_ERROR,buf);
   }
   if (classIsInstanceOf(self,parent)) vmThrow(vm,E_ASSERTION_ERROR,
	      "Class.addParent(): Can't inherit from self");

      // block recursive parents by outlawing parents w/o all parents
   if (2 != CP_FULL(parent)) vmThrow(vm,E_ASSERTION_ERROR,
		"Class.addParent: Doesn't have all classes");

   if (!IS_STATIC(parent))
   {
      Instance *topdog = TOP_DOG(parent);
      if (topdog) vmThrow(vm,E_ASSERTION_ERROR,
	      "Class.addParent(): That class is somebody else's parent");
      // topdog is set when cooked
   }

   PARENTS(self)[n] = parent;
   if (n+1 == N) CP_FULL(self) += 1;
   return;
}

    // ("static"), trailing space OK
    // throws
static void setAttribute(Class *self, char *attr, pVM vm)
{
   if      (0 == strncmp(attr,"static",6))	IS_STATIC(self)   = 1;
   else if (0 == strncmp(attr,"public",6))	IS_PRIVATE(self)  = 0;
   else if (0 == strncmp(attr,"private",7))	IS_PRIVATE(self)  = 1;
   else if (0 == strncmp(attr,"noChildren",10)) NO_CHILDREN(self) = 1;
   else if (0 == strncmp(attr,"script",6))
      { IS_STATIC(self) = 1; NO_CHILDREN(self) = 1; IS_SCRIPT(self) = 1; }
   else if (0 == strncmp(attr,"createReturnsSelf",17))
      CREATE_RETURNS_SELF(self) = 1;
   else
   {
      char buf[200];
      sprintf(buf,"%s.attribute(%.50s): Invalid attribute",
		CLASS_NAME(self),attr);
      vmThrow(vm,E_VALUE_ERROR,buf);
   }
}

    // ("static noChildren"), breaks on space
    // throws
void classSetAttributes(Instance *self,char *attributes, pVM vm)
{
   char *ptr;

   while (isspace(*attributes)) attributes++;
   while(*attributes)
   {
      if ((ptr = strchr(attributes,' ')))
      {
	 setAttribute((Class *)self,attributes,vm);
	 while (isspace(*++ptr)) ;
	 attributes = ptr; 
      }
      else
      {
	 setAttribute((Class *)self,attributes,vm);
	 break;
      }
   }
}

    // .nthClass(n) -->class
static Instance *Class_nthClass(Instance *self,pArglist arglist,pVM vm)
{
   return CLASSES(self)[
         arglistGetIndex(arglist,0,NUM_CLASSES(self),"Class.nthClass: ",vm)];
}


		/////////////////////// vars

void classSetVarBits(Instance *self,char *bits,int which)
{
   int n, vars = NUM_VARS(self);
   if (which < NUM_VAR_BITTERS)
      for (n = 0; vars-- && *bits; n++, bits++)
         if (*bits == '1') setVarBit(self,n,which);
}

    // .varBits -->T("1010" | "", same), RO bits, proxy bits
static Instance *Class_varBits(Instance *self,pVM vm)
{
   char	     bits[260], *ptr;		// max 256 vars
   Fence     fence;
   Instance *varBits = tupleCreate(2,I_OWNED,vm);
   int	     numVars = NUM_VARS(self), n,which;

   vmSetFence(vm,&fence,0,varBits);
      for(which = 0; which < NUM_VAR_BITTERS; which++)
      {
	 for(ptr = bits, n = 0; n < numVars; n++,ptr++)
	    *ptr = '0' + getVarBit(self,n,which);   // --> '0' or '1'
	 *ptr = '\0';
//	 if (!strchr(bits,'1')) *bits='\0';
	 // remove trailing zeros
	 if (n) for(; ptr-- != bits; )
	    { if (*ptr=='0') *ptr = '\0'; else break; }

	 tupleAppend(varBits,stringCreate(bits,I_OWNED,vm));
      }
   vmRemoveFence(&fence,0);
   return varBits;
}

    // .setVar(n|name) --> vars[n|name]
    // .setVar(n|name,value) --> vars[n|name] = value
    // .setVar(name,value) is thread safe
static Instance *Class_setVar(Instance *self,Instance *arglist,pVM vm)
{
   unsigned  n;
   Instance *name   = arglistGet(arglist,0,"Class.setVar",vm);
   Instance *value  = arglistTryToGet(arglist,1);
   Instance *result = 0;

   switch(TYPEO(name))
   {
      case StringType:
	 result = classFindVar(self,stringText(name),value,vm);
	 if (!result)
	 {
	    char buf[150];
	    sprintf(buf,"Class.setVar(%s): Name not found",stringText(name));
	    vmThrow(vm,E_NOT_FOUND,buf);
	 }
	 if (value) instanceIsOrphan(value);
	 break;
      case IntType:
	 n = (unsigned)convertToInt(name,vm);
	 if (n >= NUM_VARS(self))
	    vmThrow(vm,E_INDEX_ERROR,"Class.setVar(n)");
	 if (value)
	 {
	    // const, private, fcn var?
	    if(_isROVar((Class *)self,n) || '\0'==*VAR_NAMES(self)[n])
	       vmThrow(vm,E_ASSERTION_ERROR,"Class.setVar: read only");
	    VARS(self)[n] = value; instanceIsOrphan(value); 
	 }
	 result = VARS(self)[n];
	 break;
      default: vmThrow(vm,E_TYPE_ERROR,"Class.setVar(name or number)");
   }
   return result;
}

    // .isProxyVar(varName)
static Instance *Class_isProxyVar(Class *self,Instance *arglist,pVM vm)
{
   char *name = arglistGetOnlyString(arglist,0,"Class.isProxyVar",vm);
   int   n;

   n = _varIndex(self,name,vm);
   if (n == -1) vmThrow(vm,E_NOT_FOUND,"Class.isProxy: I don't think so");
   return boolCreate(_isProxyVar(self,n));
}

    // .isROVar(varName)
static Instance *Class_isROVar(Class *self,Instance *arglist,pVM vm)
{
   char *name = arglistGetOnlyString(arglist,0,"Class.isROVar",vm);
   int   n;

   n = _varIndex(self,name,vm);
   if (n == -1) vmThrow(vm,E_NOT_FOUND,"Class.isROVar: I don't think so");
   return boolCreate(_isROVar(self,n));
}

static const MethodTable classMethods[] = 
{
   "copy",		(pMethod)Class_Copy,
   "create",		(pMethod)Class_create,
   "embryo",		(pMethod)Class_embryo,
   "addFcn",		(pMethod)Class_addFcn,
   "addClass",		(pMethod)Class_addClass,
   "cook",		(pMethod)Class_cook,

   "resolve",		(pMethod)Class_resolve,
   "BaseClass",		(pMethod)Class_BaseClass,
   "whatIsThis",	(pMethod)Class_whatIsThis,

   "toBool",		(pMethod)Class_toBool,
   "toString",		(pMethod)Class_toString,
   "toInt",		(pMethod)Class_toInt,
   "toFloat",		(pMethod)Class_toFloat,
   "isInstanceOf",	(pMethod)Class_isInstanceOf,
   "isChildOf",		(pMethod)Class_isChildOf,
   "len",		(pMethod)Class_len,
   "sourceCode",	(pMethod)Class_sourceCode,
   "unasm",		(pMethod)Class_unasm,
   "launch",		(pMethod)Class_launch,

   "nthClass",		(pMethod)Class_nthClass,

   "__sGet",		(pMethod)Class_sGet,
   "__sSet",		(pMethod)Class_sSet,

   "setVar",		(pMethod)Class_setVar,
   "isProxyVar",	(pMethod)Class_isProxyVar,
   "isROVar",		(pMethod)Class_isROVar,

   "defer",		(pMethod)Object_defer,

   0,			0
};

/* ******************************************************************** */
/* *************************** Properties ***************************** */
/* ******************************************************************** */

    // .name -->String
static Instance *Class_name(Instance *self,pVM vm)
   { return kStringCreate(CLASS_NAME(self),self,I_OWNED,vm); }

    // .fullName: A whole lotta work to avoid more than one malloc
    // -->String
Instance *Class_fullName(Instance *self,pVM vm)
{
   char	    *ptr, *name, *prefix = 0;
   size_t    n;
   Instance *klass, *vaultName, *topClass = 0, *s;

   n = 0;
   for (klass = self; CONTAINER(klass); klass = CONTAINER(klass)) ;
   vaultName = vaultBackTrace(klass,vm);
   if (vaultName != Void)
   {
      if (klass == self) return vaultName;

      topClass = klass;
      prefix   = stringText(vaultName);
      n        = strlen(prefix) + 1;
   }

   for (klass = self; klass != topClass; klass = CONTAINER(klass))
      n += strlen(CLASS_NAME(klass)) + 1;	// count the "."

   n--;		// remove overcounted "."
   s = stringAllocate(n,&ptr,I_OWNED,vm);
   	// write first part of name
   if (prefix) { strcpy(ptr,prefix); strcat(ptr,"."); }
   	// write end of name
   ptr += n;
   name = CLASS_NAME(self); ptr -= strlen(name); strcpy(ptr,name);
   	// write middle parts of name
   for (klass = CONTAINER(self); klass != topClass; klass = CONTAINER(klass))
   {
      *--ptr = '.';
      name = CLASS_NAME(klass);
      n = strlen(name); ptr -= n; strncpy(ptr,name,n);
   }

//   return addToCollectables((Instance *)s,I_OWNED,vm);
   return s;
}

    // .vaultPath -->String
static Instance *Class_vaultPath(Instance *self,pVM vm)
   { return kStringCreate(VPATH(self),self,I_OWNED,vm); }

char *classVaultPath(Instance *self) { return VPATH(self); }

    /* .topdog: Find the topdog (youngest child of self). -->Class
     * If self->topdog == 0 or self, topdog is self, else self->topdog points
     * to a younger child.
     * So parents can know who has inherited from them.
     */
Instance *Class_topDog(Instance *self,pVM vm)
{
   Instance *doggie = self, *topdog;
   while((topdog = TOP_DOG(doggie)) && (doggie != topdog)) doggie = topdog;
   return doggie;
}

    // .vm -->VM
static Instance *Class_vm(Instance *self,pVM vm)
   { return vm2Instance(vm); }

    // .container -->Class|Void
Instance *Class_container(Instance *self,pVM vm)
{
   Instance *container = CONTAINER(self);
   return container ? container : Void;
}

static Instance *_makeList(int n,Instance **data,pVM vm)
{
   Instance *list;

   if (n == 0) return emptyTuple;

   list = tupleCreate(n,I_OWNED,vm);
   while(n--) tupleAppend(list,*data++);
   return list;
}

    // .varNames -->T(name,name ...)
    // There might not be a StringTable (wad classes)
static Instance *Class_varNames(Instance *self,pVM vm)
{
   Instance *varNames;
   int	     i, n = NUM_VARS(self);

   if (n == 0) return emptyTuple;

   varNames = tupleCreate(n,I_OWNED,vm);
   for (i = 0; i < n; i++)	// StringTable
   {
      char *name = VAR_NAMES(self)[i];
      tupleAppend(varNames,kStringCreate(name,self,I_OWNED,vm));  // ClassBase
   }
   return varNames;
}

    // .vars -->T(T(name,value) ...)
static Instance *Class_vars(Instance *self,pVM vm)
{
   int	     i, n = NUM_VARS(self);
   Instance *vars;
   Instance *varNames = Class_varNames(self,vm);	// protected in self
   Fence     fence;

   if (n == 0) return emptyTuple;
   vars = tupleCreate(n,I_OWNED,vm);
   vmSetFence(vm,&fence,0,vars);
      for (i = 0; i < n; i++)
      {
	 Instance *x = tupleCreateX(vm,listGet(varNames,i),VARS(self)[i],ZNIL);
	 tupleAppend(vars,x);
      }
   vmRemoveFence(&fence,0);
   return vars;
}

    // .__constructor --> detached copy of the constructor
    //   or Void if no constructor
    // fcnCopy() takes care of static class
static Instance *Class_constructor(Instance *self,pVM vm)
{
   if (NUM_FCNS(self)) return fcnCopy((Instance *)NTH_FCN(self,0),self,vm);
   return Void;
}

    // .fcns -->T(fcn,...)
    // fcnCopy() takes care of static class
static Instance *Class_fcns(Instance *self,pVM vm)
{
   size_t    n    = NUM_FCNS(self);
   ZKL_Fcn  *fcns = THE_FCNS(self);
   Instance *list;
   Fence     fence;

   if (n == 0) return emptyTuple;

   list = tupleCreate(n,I_OWNED,vm);
   vmSetFence(vm,&fence,0,list);
      while(n--)
      {
	 Instance *f = (Instance *)fcns++;
	 f = (f->iflag) ? fcnCopy(f,self,vm) : nullFcn;
      	 tupleAppend(list,f);
      }
   vmRemoveFence(&fence,0);
   return list;
}

    // .classes -->T(class...)
static Instance *Class_classes(Instance *self,pVM vm)
   { return _makeList(NUM_CLASSES(self),CLASSES(self),vm); }

    // .parents -->T(class ...)
static Instance *Class_parents(Instance *self,pVM vm)
   { return _makeList(NUM_PARENTS(self),PARENTS(self),vm); }

    // .rootClass -->Class
Instance *Class_rootClass(Instance *self,pVM vm)
{
// if flag2, nexti is root
   while(CONTAINER(self)) self = CONTAINER(self);
   return self;
}

    // --> # bytes for the entire enchilada + the # of classes, etc therein
static unsigned classSizeXL(Instance *self, int includeStatic,
	unsigned *numClasses, unsigned *numFcns, unsigned *numVars)
{
   unsigned n, zk,zf,zv, kz=0, fz = NUM_FCNS(self), vz = NUM_VARS(self);
   unsigned isize = 0;

   if (!IS_STATIC(self) || includeStatic)
   {
      isize = CLASS_ISIZE(self);
      for (n = kz = 0; n < NUM_PARENTS(self); n++)
      {
	 isize += classSizeXL(PARENTS(self)[n],includeStatic,&zk,&zf,&zv);
	 kz += zk; fz += zf; vz += zv;
      }
      for (n = 0; n < NUM_CLASSES(self); n++)
      {
	 isize += classSizeXL(CLASSES(self)[n],includeStatic,&zk,&zf,&zv);
	 kz += zk; fz += zf; vz += zv;
      }
   }
   if (numClasses) *numClasses = kz + 1;
   if (numFcns)    *numFcns    = fz;
   if (numVars)    *numVars    = vz;
   return isize;
}

    // .size --> T(instance size, all instance size, base size, link table size)
    // Kinda bogus, doesn't include size of fcns/code, where most of pork is
static Instance *Class_size(Instance *self,pVM vm)
{
   unsigned isize = CLASS_ISIZE(self);
   unsigned pork  = classSizeXL(self,1,0,0,0);
   return tupleCreateX(vm, intCreate(isize,vm), intCreate(pork,vm),
      intCreate(CLASS_BASE(self)->bsize,vm), 
      intCreate(CLASS_BASE(self)->linkTableSz,vm), 
      ZNIL);
}

    // .NullClass -->NullClass
static Instance *Class_NullClass(Instance *self,pVM vm) { return NullClass; }

    // .isClassified --> True if the class has all classes
    // embryos always have all parents
static Instance *Class_isClassified(Instance *self,pVM vm)
   { return ((CP_FULL(self) == 2) ? BoolTrue : BoolFalse); }

    // .isCooked --> True if the class has been cooked
static Instance *Class_isCooked(Instance *self,pVM vm)
   { return (IS_COOKED(self) ? BoolTrue : BoolFalse); }

    // .isEve --> False if self is a copy of a class
static Instance *Class_isEve(Instance *self,pVM vm)
   { return (self->iflag) ? BoolFalse : BoolTrue; }

    // .isPrivate -->Bool
static Instance *Class_isPrivate(Instance *self,pVM vm)
   { return boolCreate(IS_PRIVATE(self)); }

    // .isScript -->Bool, !!!this is deprecated??????????????
static Instance *Class_isScript(Instance *self,pVM vm)
   { return boolCreate(IS_SCRIPT(self)); }
   //{ return BoolFalse; }

    // .isStatic -->Bool
static Instance *Class_isStatic(Instance *self,pVM vm)
   { return boolCreate(IS_STATIC(self)); }

    // .theInitFcnIs -->n
static Instance *theInitFcnIs(Instance *self,pVM vm)
   { return intCreate(INIT_FCN(self),vm); }

    // .signature -->MD5
static void _md5(MD5_CTX *context,char *name)
   { MD5Update(context,name,strlen(name)); }

static void _sig(Class *self, MD5_CTX *context)
{
   unsigned i;

   for (i = 0; i < NUM_VARS(self);    i++) _md5(context,VAR_NAMES(self)[i]);
   for (i = 0; i < NUM_FCNS(self);    i++) _md5(context,FCN_NAME(NTH_FCN(self,i)));
   for (i = 0; i < NUM_PARENTS(self); i++) _md5(context,CLASS_NAME(PARENTS(self)[i]));
   for (i = 0; i < NUM_CLASSES(self); i++)
   {
      Class *class = (Class *)CLASSES(self)[i];
      _md5(context,CLASS_NAME(class));
      _sig(class,context);
   }
}
#undef MD5
static Instance *Class_signature(Class *self,pVM vm)
{
   MD5_CTX context;
   Byte	   hash[40], digest[16];
   size_t  i,n;

   MD5Init(&context);
   _sig(self,&context);
   MD5Final(digest,&context);

   for (i = n = 0; i < 16; i++, n += 2)
      sprintf((char *)&hash[n],"%02x", digest[i]);

   return stringCreate((char *)hash,I_OWNED,vm);
}

    // .counts --> 
    //   T(total vars,total fcns, num parents & contained classes (in self),
    //     num selfs, total class instances out there, total class bases)
static Instance *Class_counts(Instance *self,pVM vm)
{
   unsigned kcnt,fz,vz; classSizeXL(self,1,&kcnt,&fz,&vz);
   return tupleCreateX(vm,
      intCreate(vz,vm),intCreate(fz,vm),intCreate(kcnt,vm),
      intCreate(CAI_VALUE(&CLASS_BASE(self)->refCount),vm),
      intCreate(iCount,vm),intCreate(baseCount,vm),ZNIL);
}

    // .linearizeParents -->T(self, parents in search order)
static Instance *Class_linearize(Instance *self,pVM vm)
{
   Instance *parentList;
   int	     n;

   if(!IS_COOKED(self)) // probably OK but parents are not the real ones
	    vmThrow(vm,E_ASSERTION_ERROR,"Class.linearizeParents: not cooked");
   parentList=
	tupleCreate(1 + CLASS_BASE(self)->numLinearParents,I_OWNED,vm);
   tupleAppend(parentList,self);
   for(n=0; n<NUM_LINEAR_PARENTS(self); n++)
      tupleAppend(parentList,PARENTS(self)[n]);
   return parentList;
}

    // .attributes --> 
    //    String("static" &| "noChildren" &| "private")
static Instance *Class_attributes(Instance *self,pVM vm)
{
   char buf[200];

   *buf = '\0';
   if (IS_STATIC(self))   strcpy(buf,"static ");
if (IS_SCRIPT(self))   strcat(buf,"script ");
   if (IS_PRIVATE(self))  strcat(buf,"private ");
   if (NO_CHILDREN(self)) strcat(buf,"noChildren ");
   if (CREATE_RETURNS_SELF(self)) strcat(buf,"createReturnsSelf ");
   if (*buf) buf[strlen(buf) - 1] = '\0';
   return stringCreate(buf,I_OWNED,vm);
}

#if 0
    // .isThreadSafe() --> Bool
    // Are public vars thread safe?
    // Are fcn vars thread safe?
    // roVar doesn't mean squat:
    //    class C {fcn init(y) { var [const] x = y }}(a = L(1)); a.clear();
    // TSList(L(C)) isn't TS (marking race)
static Instance *Class_isThreadSafe(Instance *self,pVM vm)
{
   char	     *name = VAR_NAMES(self)[0];
   int        n,nv = NUM_VARS(self);
   Instance  *i, *var = VARS(self)[0];

   if (!nv) return BoolTrue;
   for (n = 0; n < nv; n++)
   {
      if (!OBJECT(var)->isThreadSafe) return BoolFalse;
      var++;
   }
   return BoolTrue;
}
#endif

    // .minutia
static Instance *Class_details(Instance *self,pVM vm)
{
//   int hashSize = CLASS_BASE(self)->varABitsO - CLASS_BASE(self)->hashO;
   char buf[200]; *buf = '\0';
//   sprintf(buf,"ppsize(%d) Hash table size(%d) ",PP_SIZE(self),hashSize);
   if (!IS_COOKED(self)) strcat(buf,"Not ");
   strcat(buf,"Cooked ");
   strcat(buf, self->iflag ? "copy" : "Eve");
   if (IS_STATIC(self)) strcat(buf," static");
   if (CONTAINER(self)) strcat(buf," married");
   if (TOP_DOG(self))   strcat(buf," parent");
   if (self->iflag2)    strcat(buf," chunkette");
   return stringCreate(buf,I_OWNED,vm);
}

    // .createReturnsSelf -->Bool
static Instance *Class_createReturnsSelf(Instance *self,pVM vm)
   { return boolCreate(CREATE_RETURNS_SELF(self)); }

#if 0
    // .isThreadSafe -->Bool
static Instance *Class_isThreadSafe(Instance *self,pVM vm)
{
class base: fcns are static, parents/classes are thread safe
self: current var contents are thread safe
   if (!CLASS_BASE(self)->isThreadSafe) return BoolFalse;
   for each var
   {
      if (callProperty(var,"isThreadSafe")) return BoolFalse
   }
   return BoolTrue;
}
#endif

static Instance *Class_links(Instance *self,pVM vm)
{
   unsigned n,id,lnkParent;
   for(n=MIN_GN(self); n<=MAX_GN(self); n++)
   {
      id = LINK_TABLE(self)[n - MIN_GN(self)];
      if(id)
      {
	 lnkParent = id>>LNK_SHIFT;
	 if(lnkParent) printf("%4d: 0x%04x (%s) (%d 0x%04x)\n",
			   n,id,getGlobalName(n,vm),lnkParent,id & LNK_LOW);
	 else printf("%4d: 0x%04x (%s)\n",n,id,getGlobalName(n,vm));
      }
   }
   return Void;
}

static const PropertyTable classProperties[] =
{
   "attributes",	(pProperty)Class_attributes,
   "name",		(pProperty)Class_name,
   "fullName",		(pProperty)Class_fullName,
   "vaultPath",		(pProperty)Class_vaultPath,
   "topdog",		(pProperty)Class_topDog,
   "vm",		(pProperty)Class_vm,
   "container",		(pProperty)Class_container,
   "varNames",		(pProperty)Class_varNames,
   "vars",		(pProperty)Class_vars,
   "varBits",		(pProperty)Class_varBits,
   "fcns",		(pProperty)Class_fcns,
   "classes",		(pProperty)Class_classes,
   "parents",		(pProperty)Class_parents,
   "rootClass",		(pProperty)Class_rootClass,
   "size",		(pProperty)Class_size,
   "__constructor",	(pProperty)Class_constructor,
   "NullClass",		(pProperty)Class_NullClass,
   "isClassified",	(pProperty)Class_isClassified,
   "isCooked",		(pProperty)Class_isCooked,
   "isEve",		(pProperty)Class_isEve,
   "isPrivate",		(pProperty)Class_isPrivate,
   "isScript",		(pProperty)Class_isScript,
   "isStatic",		(pProperty)Class_isStatic,
   "theInitFcnIs",	(pProperty)theInitFcnIs,
   "signature",		(pProperty)Class_signature,
   "counts",		(pProperty)Class_counts,
   "linearizeParents",	(pProperty)Class_linearize,
   "createReturnsSelf",	Class_createReturnsSelf,

   "minutia",		(pProperty)Class_details,
   "links",		Class_links,
   0,			0
};

/* ******************************************************************** */
/* ***************************** Op Codes ***************************** */
/* ******************************************************************** */

extern const OpcodeTable Object_OpcodeTable[];	// object.c

unsigned eqId,neqId, ltId,lteId, gtId,gteId, negateId, 	// set in object.c
   addId,subId,mulId,divId,modId;

    // Tailcalls don't work, VM:opAdd doesn't want to wait
    // class A { fcn __Add(X) {} } class B(A){} class C(B) {} C + 123
static Instance *classOp(Instance *self,Instance *X,
   unsigned opNameId, int opIdx, pVM vm)
{
   MLIST(mlist,1);
   ZKL_Fcn _fcn;
   Instance *fcn = _findById2((Class *)self,opNameId,LNK_FCN,1,&_fcn,0,vm);

   if(fcn) return vmCallFcnFromMethod(fcn, mlistBuild(mlist,X,ZNIL), 0,vm);
   return Object_OpcodeTable[opIdx].op(self,X,vm);
}

Instance *Class_eq(Instance *self,Instance *X,pVM vm)
   { return classOp(self,X,eqId,OP_EQ,vm); }

static Instance *Class_neq(Instance *self,Instance *X,pVM vm)
   { return classOp(self,X,neqId,OP_NEQ,vm); }

static Instance *Class_lt(Instance *self,Instance *X,pVM vm)
   { return classOp(self,X,ltId,OP_LT,vm); }

static Instance *Class_lte(Instance *self,Instance *X,pVM vm)
   { return classOp(self,X,lteId,OP_LTE,vm); }

static Instance *Class_gt(Instance *self,Instance *X,pVM vm)
   { return classOp(self,X,gtId,OP_GT,vm); }

static Instance *Class_gte(Instance *self,Instance *X,pVM vm)
   { return classOp(self,X,gteId,OP_GTE,vm); }

static Instance *Class_negate(Instance *self,Instance *X,pVM vm)
{
   ZKL_Fcn _fcn;
   Instance *fcn = _findById2((Class *)self,negateId,LNK_FCN,1,&_fcn,0,vm);

   if (fcn) return vmCallFcnFromMethod(fcn,NoArglist,0,vm);
   return Object_negate(self,X,vm);
}

static Instance *Class_add(Instance *self,Instance *X,pVM vm)
   { return classOp(self,X,addId,OP_ADD,vm); }

static Instance *Class_sub(Instance *self,Instance *X,pVM vm)
   { return classOp(self,X,subId,OP_SUB,vm); }

static Instance *Class_mul(Instance *self,Instance *X,pVM vm)
   { return classOp(self,X,mulId,OP_MUL,vm); }

static Instance *Class_div(Instance *self,Instance *X,pVM vm)
   { return classOp(self,X,divId,OP_DIV,vm); }

static Instance *Class_mod(Instance *self,Instance *X,pVM vm)
   { return classOp(self,X,modId,OP_MOD,vm); }

static const OpcodeTable opcodeTable[] = 
{
   OP_EQ,	(pOp)Class_eq,
   OP_NEQ,	(pOp)Class_neq,
   OP_LT,	(pOp)Class_lt,
   OP_LTE,	(pOp)Class_lte,
   OP_GT,	(pOp)Class_gt,
   OP_GTE,	(pOp)Class_gte,

   OP_NEGATE,	(pOp)Class_negate,

   OP_ADD,	(pOp)Class_add,
   OP_SUB,	(pOp)Class_sub,
   OP_MUL,	(pOp)Class_mul,
   OP_DIV,	(pOp)Class_div,
   OP_MOD,	(pOp)Class_mod,

   0,		0
};


/* ******************************************************************** */
/* ******************************************************************** */

    // For the NullClass:
//static ZKL_Object NullClassObject;

static StringTable nullNames =	// class NullClass {}
{
   2,			// n
   "NullClass\0\0",	// Strings: name, vaultPath: "NullClass",""
   11,			// size
};

#if 0
Instance *BitsClass;	// class Bits { var bits }
static StringTable BitNames =
{
   3,		// n
		// Strings: name, vaultPath, var names (sorted)
   "__Bits\0\0bits\0",	// "__Bits", "", "bits"
   13,		// size
};

    // Put wad/zsc Data bits for a klass in a Bits container
    // Can GC so use a Fence
//create class base with extra pointer in data
// then shove bits into it
Instance *wadItUp(Instance *klass,Instance *bits,pVM vm)
{
   Instance *Bits = classCopy(BitsClass,vm);
   Byte     *ptr;
   size_t    size;
   CONTAINER(klass) = Bits;
   ptr		    = dataText(bits,&size);
   VARS(Bits)[0]    = kdataCreate(ptr,size,bits,I_OWNED,vm);
   return Bits;
}
#endif

void classConstruct(void)
{
   constructObject((ZKL_Object *)&GlobalClassObject,ClassType,
	classMethods,classProperties,opcodeTable,NoVM);
   GlobalClassObject.resolve	    = classResolve;
   GlobalClassObject.resolveN	    = classResolveN;
   GlobalClassObject.magicMarker    = classMarker;
   GlobalClassObject.freeMe	    = classFree;

//   stCalcSize(&nullNames);	// in case I can't count
   NullClass		= classEmbryo(&nullNames,0,0,0,0,I_UNTOUCHABLE,NoVM);
   IS_STATIC(NullClass) = 1;
   classCookClass(NullClass,NoVM);
   nullMethod		= 
      methodCreate2(NullClass,0,(pMethod)Object_Method,I_UNTOUCHABLE,0);
   nullProperty		= propertyCreate(NullClass,"name",2,I_UNTOUCHABLE,0);

   #if USE_POINTER_INTS		// don't confuse a Fcn with a PtrInt!
      if (IS_PtrInt(BASE_FCNS(NullClass)))
	 vmThrow(NoVM,E_VM_ERROR,"ClassBase.data bad alignment, adjust padding");
   #endif

//   stCalcSize(&BitNames);	// in case I can't count
//   BitsClass		= classEmbryo(&BitNames,0,0,0,I_UNTOUCHABLE,NoVM);
//   classCookClass(BitsClass,NoVM);

   notFoundId = addStaticNametoGlobalTable(NOT_FOUND_NAME,NoVM);
}

////////////////////////////////////////////////////////////////////
// gperf hash for __resolve
/* 
%language=ANSI-C
%readonly-tables
%delimiters=,
%enum
%omit-struct-type
%switch=1
%%
"BaseClass"
"resolve"
*/


////////////////////////////////////////////////////////////////////
// gperf hash for .BaseClass
/* 
%language=ANSI-C
%readonly-tables
%delimiters=,
%enum
%omit-struct-type
%switch=1
%%
"resolve"
"toBool"
"toString"
*/


///////////////////////////////////////////////////////////
// zkl extractTable    < class.c | gperf | zkl gperf -i class
// zkl extractTable -p < class.c | gperf | zkl gperf -i class



///////////////////////////////////////////////////////////
// Property table
// zkl extractTable -p < class.c | gperf | zkl gperf -i class
