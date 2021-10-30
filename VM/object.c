/* object.c : Everybodies base class
 * 
 * Copyright (c) 2006,2007,2008-12,2013 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#define __NOT_A_DLL

#include "zklObject.h"
#include "zklClass.h"
#include "zklFcn.h"
#include "zklList.h"
#include "zklMethod.h"
#include "zklNumber.h"
#include "zklString.h"
#include "zklUtil.h"
#include "dHash.h"

static void methodFixup(ZKL_Object *, const MethodTable *);
static void opFixup(ZKL_Object *,const OpcodeTable *);
static void opStomp(ZKL_Object *dst, ZKL_Object *src);

const MethodTable    objectMethods[];
const PropertyTable  objectProperties[];

static void Object_fixup(ZKL_Object *,
	const MethodTable *, const PropertyTable *, const OpcodeTable *);


CAtomicInt  _numObjects;		// stats & object id
ZKL_Object *zklObjectTable[0x100];	// ALL Objects, except Object

#define MAX_METHODS	   200
#define FIRST_PROPERTY	   MAX_METHODS
#define MAX_PROPERTIES	    55
#define MAX_GLOBAL_NAMES  4000
#define MISC_GLOBAL_SZ   30000

//stats (_numNames, miscSz):
//  names:  455 for objects, 795 for Compiler
//  miscSz: 4908 for all Tests, 3442 for Compiler
//  to repl:   ( 955, 5301)
//  compiler:  (1351, 9285)
//  all tests: (2016,13492)

static unsigned  _numNames;	  // count of all method and property names
static void     *globalNameTable; // Dictionary: (name:idx, ...)
static char      miscGlobalNames[MISC_GLOBAL_SZ];	// ick
static unsigned  miscSz = 0;
static char     *globalNameTableBackPtrs[MAX_GLOBAL_NAMES];
static WriteLock globalNameLock;

static unsigned  objectMinGN,objectMaxGN; // until I put Object into zklObjectTable
static Byte     *objectGlobalNameIds;    // until I put Object into zklObjectTable

#define OBJ_IDX(obj,idx,n)			\
   (idx>=obj->minGN && idx<=obj->maxGN &&	\
   0xFF!=(n = obj->globalNameIds[idx - obj->minGN]))
#define OBJECT_IDX(idx,n)			\
   (idx<=objectMaxGN && (n = objectGlobalNameIds[idx])!=0xFF)

    // Thread safe (and needs to be for DLLs)
void registerObject(ZKL_Object *object,pVM vm)
{
   int idx = CAI_INC(&_numObjects);
   idx--;
   if (idx > 0xff) vmThrow(vm,E_VM_ERROR,"Too many Objects!");
   object->id	       = (Byte)idx;
   zklObjectTable[idx] = object;
}

#define GF_ADD		1
#define GF_STASH	2
#define GF_DONT_LOCK	4  // called from construct() or batch mode

    // vm==NoVM: don't lock
static int 
findNameInGlobalTable(char *name, unsigned flags, unsigned *id, pVM vm)
{
   #define BIG_NUM 7654321   // more entries than possible in Dictionary (64k)

   unsigned  n = BIG_NUM;
   ENTRY    *kv;
   unsigned  lock, add=(flags&GF_ADD), stash=(flags&GF_STASH);
   unsigned  lockType = (add ? WL_WRITER : WL_READER);

   if(vm==NoVM) flags |= GF_DONT_LOCK;	// can be called from construct()
   lock=!(flags&GF_DONT_LOCK);

   if(lock) writeLockAcquire(&globalNameLock,lockType,vm);
      if(add) 	// ONE probe into hash table
      {
	 ENTRY item = { (Instance *)name,(Instance *)BIG_NUM };
	 kv = hSearch(globalNameTable,name,&item,FINDADD);
	 if(!kv)	// Dictionary full
	 {
	    if(lock) writeLockRelease(&globalNameLock,lockType);
//!!!ick - if caller locked, doesn't unlock, unlock anyway?
	    vmThrow(vm,E_OUT_OF_MEMORY,"Global name dictionary full");
	 }

	 if(BIG_NUM == (size_t)kv->value)	// new entry, update n
	 {
	    if(_numNames==MAX_GLOBAL_NAMES)
	       vmThrow(vm,E_VM_ERROR,"Too many global names");

	    if(stash)
	    {
	       size_t sz=strlen(name);
	       if((sz + miscSz + 1) >= MISC_GLOBAL_SZ)
	       {
		  if(lock) writeLockRelease(&globalNameLock,lockType);
//!!!ick - if caller locked, doesn't unlock, unlock anyway?
		  vmThrow(vm,E_OUT_OF_MEMORY,"Global misc table full");
	       }
	       name = strcpy(miscGlobalNames + miscSz,name);
	       miscSz+=(sz+1);
	       kv->key = (Instance *)name;
	    }

	    globalNameTableBackPtrs[_numNames] = name;
	    n = _numNames++;	// doesn't need to be atomic
	    kv->value = (Instance *)(size_t)n;
	 }
	 else n = (unsigned)(size_t)kv->value;	// existing entry, grab n
      }
      else	// find only
	 if((kv = hSearchFor(globalNameTable,name)))	// found
	    n = (unsigned)(size_t)kv->value;

   if(lock) writeLockRelease(&globalNameLock,lockType);
   *id = n;
   return (n!=BIG_NUM);	// -->1 if name found or added

   #undef BIG_NUM
}

    // if vm==NoVM (ie called from construct()), don't lock
int getGlobalId(char *name, unsigned *id, pVM vm)
{
   if(findNameInGlobalTable(name,0x00,id,vm)) return 1;	// name exists
   return 0;
}

int addNametoGlobalTable(char *name,pVM vm)
{
   unsigned n;
   findNameInGlobalTable(name,GF_ADD|GF_STASH,&n,vm);
   return n;
}

int addStaticNametoGlobalTable(char *name,pVM vm)
{
   unsigned n;
   findNameInGlobalTable(name,GF_ADD,&n,vm);
   return n;
}

char *getGlobalName(unsigned id,pVM vm)
{
   if(id>=_numNames) 
   {
      if(vm==NoVM) return "";	// !!!????
      vmThrow(vm,E_VM_ERROR,"globalNameTable lookup index error");
   }
   return globalNameTableBackPtrs[id];
}

#if 0
   /* By having a fixed set of pre-defined names, the compiler doesn't need
    * to store these names in the link tables, which reduces space and link
    * time. I came up with this list by looking the link tables for a bunch
    * of programs. The length limit is 256 and should only contain method or
    * property names.
    * This has to run before any other names are added to the Global Table.
    */
static void primeGlobalNames(pVM vm) // add some common Methods/Properties/names
{
   // Note! This list can NEVER shrink!
also fp* are special cased by Deferred
   static char *names[]={ "__sGet","__sSet", "fp","fp1", "print","println", 
      "walker","walkerNext#", "next", "fmt","write","len","append","split",
      "pump","apply","filter","reduce","holds",
      "append","find","sum",
      0 };
   char     **ptr;
   unsigned   n;
   for(ptr=names; ptr; ptr++)
      findNameInGlobalTable(*ptr,GF_ADD|GF_DONT_LOCK,&n,vm);
}
#endif

static void buildObjectLinkTable(	// 987 names, 435 unique
   ZKL_Object *object,
   const MethodTable *methods, const PropertyTable *properties, pVM vm)
{
   unsigned n,id,tableLen, ps=0, min=~0,max=0,sz=0;
   Byte    *table;
   struct{ uint16_t id,n; } lnkInfo[MAX_METHODS + MAX_PROPERTIES];  // 1020

   writeLockAcquire(&globalNameLock,WL_WRITER,vm);
      if (methods)   // add method names to global table
      {
	 const MethodTable *ptr;
	 for (ptr = methods; ptr->name; ptr++,sz++)
	 {
	    findNameInGlobalTable(ptr->name,GF_DONT_LOCK|GF_ADD,&id,vm);
	    if(sz>=MAX_METHODS)
	    {
	    tooBig:
	       writeLockRelease(&globalNameLock,WL_WRITER);
	       vmThrow(vm,E_VM_ERROR,"Object has too many methods or properties");
	    }
	    lnkInfo[sz].id = id; lnkInfo[sz].n = sz;
	    if(id>max) max = id;
	    if(id<min) min = id;
	 }
      }
      if (properties)   // add property names to global table
      {
	 const PropertyTable *ptr;
	 for (ptr = properties; ptr->name; ptr++,ps++,sz++)
	 {
	    findNameInGlobalTable(ptr->name,GF_DONT_LOCK|GF_ADD,&id,vm);
	    if(ps>=MAX_PROPERTIES) goto tooBig;
	    lnkInfo[sz].id = id; lnkInfo[sz].n = ps + FIRST_PROPERTY;
	    if(id>max) max = id;
	    if(id<min) min = id;
	 }
      }
   writeLockRelease(&globalNameLock,WL_WRITER);

   tableLen = sz ? (max - min + 1) : 0;  // happens: FcnBaseObject
   table    = malloc(tableLen);	// likely a bunch of dead entries
   memset(table,0xff,tableLen);	// fill with "null"

   for(n=0; n<sz; n++) table[lnkInfo[n].id - min] = (Byte)lnkInfo[n].n;
   
   if(object)
   {
      object->minGN	    = min;
      object->maxGN	    = max;
      object->globalNameIds = table;
   }
   else		// Object
   {
      objectMinGN         = min;
      objectMaxGN         = max;	// ==tableLen-1, min == 0
      objectGlobalNameIds = table;
   }
}

extern unsigned 
   pumpId, zipWithOId,					// util.c
   starId,plusTraceId,minusTraceId,bangId,dotId,zeroId,	// pcBlock.c
   textId, catchableId, sGetId,				// vm.c
   baseClassId,fpId,fpMId,toBoolId,			// method.c
   notFoundId,toIntId,toFloatId,toStringId,lenId,	// class.c
      resolveId,sSetId,
      eqId,neqId, ltId,lteId, gtId,gteId, negateId, 
      addId,subId,mulId,divId,modId;
   ;

    // Cache some names (most already defined method names) for quick lookups
    // "fp" & "fpM" are used by method.c:deferredResolveN()
void cacheSomeGlobalNames(void)
{
   // "fp*" already in table (via constructObject()), we just want the id's
   struct{ char *name; unsigned *var; } names[] =
   {
      "pump",&pumpId, "zipWithO",&zipWithOId,
      "*",&starId, "+trace",&plusTraceId, "-trace",&minusTraceId,
      "!",&bangId, ".",&dotId, "0",&zeroId,
      "text",&textId, "catchable",&catchableId,
      "__sGet",&sGetId, "__sSet",&sSetId,
      "BaseClass",&baseClassId, "fp",&fpId, "fpM",&fpMId, "toBool",&toBoolId,
      "toInt",&toIntId, "toFloat",&toFloatId,
         "toString",&toStringId, "len",&lenId, "resolve",&resolveId,
	 "__opEQ",&eqId, "__opNEQ",&neqId, "__opLT",&ltId, "__opLTE",&lteId,
	 "__opGT",&gtId, "__opGTE",&gteId, "__opNegate",&negateId,
	 "__opAdd",&addId, "__opSub",&subId, "__opMul",&mulId, 
	 "__opDiv",&divId, "__opMod",&modId,
      0,0
   }, *ptr;
   for(ptr=names; ptr->name; ptr++)
	 findNameInGlobalTable(ptr->name,GF_ADD|GF_DONT_LOCK,ptr->var,NoVM);
}

void walkGNtable(unsigned sz, void *X,
char *(*poot)(void *X, unsigned,unsigned,int), pVM vm)
{
   unsigned n, id;
   writeLockAcquire(&globalNameLock,WL_WRITER,vm);
      for(n=0; n<sz; n++) 
      {
	 char *name = poot(X,n,0,0);	// read name
	 findNameInGlobalTable(name,GF_ADD|GF_STASH|GF_DONT_LOCK,&id,vm);
	 poot(X,n,id,1);		// write id of name
      }
   writeLockRelease(&globalNameLock,WL_WRITER);
}

#if 1
    /* This is here (rather than in fcn.c) as I consider the global table
     *   details more immportant to hide.  And I want to minimize the number
     *   of times I toggle the lock.
     * I really doubt it makes any time difference but it looks like this is
     *   a bit slower. ???
     */
void fcnLink(ZKL_Code *zklCode, pVM vm)
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
   writeLockAcquire(&globalNameLock,WL_WRITER,vm);
      for(ptr=names; N--; ptr+=(strlen(ptr) + 1)) 
      {
	 unsigned id, cnt = *fixups++;
	 if(cnt & 0x80) cnt = ((cnt & 0x7f)<<8 | *fixups++);

	 findNameInGlobalTable(ptr,GF_ADD|GF_STASH|GF_DONT_LOCK,&id,vm);
	 while(cnt--)
	 {
	    unsigned addr = (fixups[0]<<8 | fixups[1]);
	    code[addr] = id>>8; code[addr+1] = id & 0xff;
	    fixups+=2;
	 }
      }
   writeLockRelease(&globalNameLock,WL_WRITER);
//printf("-->%lx  %lx %lx\n",fixups,end,end-fixups);
   if(fixups!=end) 
   vmThrow(vm,E_VM_ERROR,"Ran off end of map");
}
#endif



static int Object_toSink(Instance *self,void *ZAgg,
		 size_t lenHint, size_t sbSz,size_t *sbUsed, pVM vm)
{ return ZA_2SINK_NOOP; }

ZKL_Object *constructObject(ZKL_Object *object, int type,
   const MethodTable *methods, const PropertyTable *properties,
   const OpcodeTable *opcodes, pVM vm)
{
   memset(object,0,sizeof(ZKL_Object));

   object->name		= typeToName(type);
   object->otype	= type;
   object->vaultPath	= "";
   object->resolve	= objectResolve;
   object->resolveN	= objectResolveN;
   object->toSink	= Object_toSink;

   Object_fixup(object,methods,properties,opcodes);
   buildObjectLinkTable(object,methods,properties,vm);
   registerObject(object,vm);

   return object;
}

#if 0
ZKL_Object *constructObject2(ZKL_Object *object,int type,
   MethodTable *methods, PropertyTable *properties, OpcodeTable *opcodes)
{
   object->name  = typeToName(type);
   object->otype = type;
   Object_fixup2(object,methods,properties,opcodes);
   return object;
}
#endif

    // call this after constructObject(), used by fltk
void objectStack(
ZKL_Object *dst, const OpcodeTable *opcodes, ZKL_Object *src, ...)
{
   va_list	      ap;		// argument list pointer
   const MethodTable *table = dst->methodTable;

   va_start(ap,src);
      do
      {
	 methodFixup(dst,src->methodTable);
	 opStomp(dst,src);

      	 src = va_arg(ap, ZKL_Object *);
      } while(src);
   va_end(ap);
   methodFixup(dst,table);
   opFixup(dst,opcodes);
}

   // is id a method or property for i or Object?
int isIdMP(Instance *i, unsigned id)
{
   ZKL_Object *obj = OBJECT1(i);
   unsigned    n;
   return (OBJ_IDX(obj,id,n) || OBJECT_IDX(id,n));
}

    // No PtrInts!
pMethod searchForMethod(Instance *i, char *name, int searchObject, pVM vm)
{
   unsigned    id,n;
   ZKL_Object *obj = OBJECT1(i);

   if(!findNameInGlobalTable(name,0x00,&id,vm)) return 0;

   if(OBJ_IDX(obj,id,n) && n<MAX_METHODS) return obj->methodTable[n].method;

	// search object?
   if (searchObject && id<=objectMaxGN && 
       (n = objectGlobalNameIds[id])<MAX_METHODS)
      return objectMethods[n].method;

   return 0;
}

    // No PtrInts!
pProperty searchForProperty(Instance *i, char *name, int searchObject, pVM vm)
{
   unsigned    id,n;
   ZKL_Object *obj = OBJECT1(i);

   if(!findNameInGlobalTable(name,0x00,&id,vm)) return 0;

   if(OBJ_IDX(obj,id,n) && 
      n>=FIRST_PROPERTY) return obj->propertyTable[n - FIRST_PROPERTY].property;

	// search object?
   if (searchObject && id<=objectMaxGN && 
       (n = objectGlobalNameIds[id])>=FIRST_PROPERTY)
      return objectProperties[n - FIRST_PROPERTY].property;

   return 0;
}

char *_name(Instance *i)
{
   #if USE_POINTER_INTS
      if (IS_PtrInt(i)) return ONAME(Zero);
   #endif

   return ONAME(i); 
}

static char *_iname(Instance *i)
{
   switch(TYPEO(i))
   {
      case ClassType:  return className(i);
      case FcnType:    return fcnName(i);
   }
   return 0;
}

char *iname(Instance *i)
{
   char *ptr = _iname(i);
   if (ptr) return ptr;
   return _name(i);
}
 
    // Find a method or property by name
    /* If method != 0 and name is a method, set it to the pMethod and return
     *   self (or new self if self is a proxy). Otherwise, ignore method.
     * If method == 0, return a Method or Property.
     * NO PtrInts!
     */
static Instance *getPropertyN(Instance *i,unsigned id,int useIprop,pVM vm)
{
   Instance *r;

   #if USE_POINTER_INTS
	 // For all methods, (0).reolve(text) works
	 // but for properties (eg (5).sign), need 5
      if (IS_PtrInt(i))
      {
	 ZKL_Int i64;
	 PtrInt_TO_Int64(i,&i64);
	 if(useIprop) r = PROPERTY_TABLE(i)[id].property((Instance *)&i64,vm);
	 else         r = objectProperties[id].property((Instance *)&i64,vm);
	 switch(TYPEO(r))	// yes, this makes me want to hurl
	 {	// a Method/Propery may have been allocated
	    case MethodType:
	       methodThunk(  r,INT64V(&i64),vm); break;
	    case PropertyType:
	       propertyThunk(r,INT64V(&i64),vm); break;
	 }
      }
      else
   #endif
   if(useIprop) r = PROPERTY_TABLE(i)[id].property(i,vm);
   else		r = objectProperties[id].property(i,vm);
instanceIsOrphan(r);
   return r;
}

    /* Returns: 0 or 
     *    Fcntype|MethodType|PropertyType|UnknownType
     *    PropertyType means a id was a property, ie any type.
     *    UnknownType means just that but not one of the above.
     * Input: 
     *    self: Instance to resolve against
     *    id:   global name table id
     *    pfcn: 0 | &ZKL_Fcn
     *    method should be initialized to zero
     * Sets (if return !0):
     *    self  : result of resolve
     *    method: method if id is MethodType, self unchanged
     *    pFcn  : Filled in if return FcnType, self set to pFcn
     * Note: Can allocate
     *   Knows PtrInts
     */
int objectResolveN(Instance **self,
unsigned id, pMethod *method, void *pFcn, int bitch, pVM vm)
{
   Instance   *i = *self;
   pMethod     mFcn;
   unsigned    n2;
   ZKL_Object *obj = OBJECTI(i);  // PtrInt OK

   if(method) *method = 0;	// just to be nice
   if(OBJ_IDX(obj,id,n2))	// in self?
   {
      if(n2<MAX_METHODS)
      {
	 mFcn = obj->methodTable[n2].method;
      aMethod:
	 if(method) *method = mFcn; 
	 else 
	 {
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(i))
		  *self = methodCreate(intAllocate(PtrInt_TO_N(i),vm),0,mFcn,vm);
	    #endif
	    else *self = methodCreate(i,0,mFcn,vm);
	 }
	 return MethodType;
      }
      *self = getPropertyN(i,n2 - FIRST_PROPERTY,1,vm);		// thunks
      return PropertyType;
   }
   if(id<=objectMaxGN)	// in Object?
   {
      n2 = objectGlobalNameIds[id];// don't need to check, table not sparse
      if(n2<MAX_METHODS)
      {
	 mFcn = objectMethods[n2].method;
	 goto aMethod;
      }
      *self = getPropertyN(i,n2 - FIRST_PROPERTY,0,vm);		// thunks
      return PropertyType;
   }
   // else not found

   if(bitch)
   {
      char *name = "bogus";
      char buf[200], *iname = _iname(i), *t = _name(i);

      if(id < _numNames) name = globalNameTableBackPtrs[id];

      if (iname) sprintf(buf,"%s(%s).resolve: %.80s not found.",t,iname,name);
      else	 sprintf(buf,"%s.resolve: %.80s not found.",t,name);
      vmThrow(vm,E_NOT_FOUND,buf);
   }

   return 0;
}

int objectResolve(Instance **self,
char *name, pMethod *method, void *pFcn, int bitch, pVM vm)
{
   unsigned id;

   if(findNameInGlobalTable(name,0x00,&id,vm))	// name exists
      return objectResolveN(self,id,method,pFcn,bitch,vm);
   // else not found

   if(bitch)
   {
      char buf[200], *iname = _iname(*self), *t = _name(*self);
      if (iname) sprintf(buf,"%s(%s).resolve: %.80s not found.",t,iname,name);
      else	 sprintf(buf,"%s.resolve: %.80s not found.",t,name);
      vmThrow(vm,E_NOT_FOUND,buf);
   }

   return 0;
}

#if 0
    // Used for sleezeball inheritance
Instance *iResolveAgainst(Instance *self,
   ZKL_Object *object, char *name, pMethod *method, int justCheckSelf, pVM vm)
{
   Instance   fake;	// works for both Instance & BInstance
   pMethod    mFcn;
   pProperty  pFcn;

	// copy 4 or 8 bytes so _methodSearch() works
   fake = *self; fake.objID = object->id;

   	// search self
   if ((mFcn = object->methodSearch(&fake,name)))
      return method ? (*method = mFcn, self) : methodCreate(self,0,mFcn,vm);
   if ((pFcn = object->propertySearch(&fake,name))) return pFcn(self,vm);

   if (justCheckSelf) return 0;

   	// search base Object
   if ((mFcn = in_object_methods(0,name)))
      return method ? (*method = mFcn, self) : methodCreate(self,0,mFcn,vm);
   if ((pFcn = in_object_properties(0,name))) return pFcn(self,vm);

   {
      char buf[200], *iname = _iname(self), *t = _name(self);
      if (iname) sprintf(buf,"%s(%s).resolve: %.80s not found.",t,iname,name);
      else	 sprintf(buf,"%s.resolve: %.80s not found.",t,name);
      vmThrow(vm,E_NOT_FOUND,buf);
   }

   return 0;	// doesn't get there
}
#endif

    // You need to protect fcn & arglist against GC
    // pr is unknown for methods and properties
    // Also works for Deferred & closures
Instance *objectRun(Instance *f, pArglist arglist, Instance **pr,pVM vm)
{
   switch (TYPEO(f))
   {
      case FcnType:
	 f = vmCallFcnFromMethod(f,arglist,0,vm);
	 if (pr) *pr = f;
	 return f;
      case ClassType:	 return classRun(      f,arglist,vm);
      case MethodType:	 return Method_call(   f,arglist,vm);
      case PropertyType: return Property_call( f,arglist,vm);
      case OpType:	 return Op_create(     f,arglist,vm); // not op_call
      case DeferredType: return deferredCall(  f,arglist,vm);
      default:		// call create method, eg 4()
	 #if USE_POINTER_INTS
	    if (IS_PtrInt(f)) return Int_create(f,arglist,vm);
	    else
	 #endif
	 return I_METHOD(f,M_CREATE)(f,arglist,vm);
   }
   return Void;		// shut up the compiler
}

/* ******************************************************************** */
/* ************************* Object Methods *************************** */
/* ******************************************************************** */

    // .resolve(name [,bits])
    // .resolve(name) == .BaseClass.name
    // .resolve(name,N): N is bits:	--> Int (non zero) | Bool
    //    1: Method
    //    2: Property
    // .resolve(name,*): All of the above
    /* This should use the object resolve method but that only matters for
     * Class (currently) so remove a tiny bit of overhead, let Class
     * override this.
     */
Instance *Object_resolveName(Instance *self,pArglist arglist,pVM vm)
{
   char	    *name;
   Instance *pb;

   name = arglistGetOnlyString(arglist,0,".resolve",vm);
   pb   = arglistTryToGet(arglist,1);

   if (pb)
   {
      int n = 0;
      if (pb == Star) n = 0xF;
      if (TYPEO(pb) == IntType) n = (int)convertToInt(pb,vm);
      if ((n & 1) && searchForMethod(  self,name,1,vm)) return intCreate(MethodType,vm);
      if ((n & 2) && searchForProperty(self,name,1,vm)) return intCreate(PropertyType,vm);
      return  BoolFalse;
   }
   if(!objectResolve(&self,name,0,0,1,vm)) return 0;
   return self;
}

    // .BaseClass(name) --> i.resolve(name)
    /* The compiler vectors [almost] all calls to BaseClass through here (it
     *   doesn't always know if i.BaseClass exists). This lets proxy classes
     *   (Deferred) avoid .resolve voodoo to handle BaseClass.
     * SHOULD re-vector more than Class?
     * Compiler uses opCallOMethodN to get here so BaseClass needs to be
     *   pinned in Method list.
     */
static Instance *Object_BaseClass(Instance *self,pArglist arglist,pVM vm)
{
   char *name;

   // Classes and Deferreds have their own BaseClass method
   name = arglistGetOnlyString(arglist,0,"BaseClass",vm);
   if(!objectResolve(&self,name,0,0,1,vm)) return 0;
   return self;
}

    // .Method(name) --> Method, for symmetry with .Property()
Instance *Object_Method(Instance *self,pArglist arglist,pVM vm)
{ return methodCreate(self,arglistGetOnlyString(arglist,0,".Method",vm), 0,vm); }

static Instance *Object_methodName(Instance *,pArglist,pVM);

    // .method(name)	   --> Method | Error
    // .method(name,True)  --> Bool (just check)
    // .method(name,False) --> T(Instance_method_index,True)
    // 			   --> T(Object_method_index,  False)
    //			   --> False
    // .method(n,Bool)     --> name, symmetrical with (name,False) 
//!!! if globalNames, nuke all but .method(name)
static Instance *Object_method(Instance *self,pArglist arglist,pVM vm)
{
   char       *name;
   int	       z;
   pMethod     mFcn;
   ZKL_Object *obj = OBJECTI(self);  // PtrInt OK

   {
      Instance *i = arglistGet(arglist,0,".method",vm);
      if (TYPEO(i) == IntType) return Object_methodName(self,arglist,vm);
      name = arglistGetOnlyString(arglist,0,0,vm);
      z    = arglistTryToGetBool( arglist,1,2,".method",vm);  // 0|1|2(no arg)
   }

   if(z==0)	// (name,False) --> False | T(instance|object, index)
   {	// get rid of this!!!
      unsigned n,idx,s = findNameInGlobalTable(name,0x00,&idx,vm);
      if(!s) return BoolFalse;
      if(OBJ_IDX(obj,idx,n)) 
	 return tupleCreateX(vm,intCreate(n,vm),BoolTrue,ZNIL);
      if(OBJECT_IDX(idx,n)) 
	 return tupleCreateX(vm,intCreate(n,vm),BoolFalse,ZNIL);
      return BoolFalse;
   }

   if ((mFcn = searchForMethod(self,name,1,vm)))	// method exists
   {
      if (z == 1) return BoolTrue;		// just checking
      return methodCreate(self,0,mFcn,vm);	// .method(name)
   }

   if (z != 2) return BoolFalse;	// True|False

   return methodCreate(self,name,0,vm);	// throw NotFoundError
}

    // .property(name,just check=False) --> property value | Bool
static Instance *Object_property(Instance *self,pArglist arglist,pVM vm)
{
   char      *name	= arglistGetOnlyString(arglist,0,".property",vm);
   int	      justCheck = arglistTryToGetBool(arglist,1,0,".property",vm);
   pProperty  pFcn;

   if ((pFcn = searchForProperty(self,name,1,vm)))
   {
      if (justCheck) return BoolTrue;
      return pFcn(self,vm);
   }

   if (justCheck) return BoolFalse;

   {
      char buf[100];
      sprintf(buf,"Property \"%.80s\" not found",name);
      vmThrow(vm,E_NOT_FOUND,buf);
   }

   return Void;		// shut up the compiler
}

    // .Property(name [,var]) --> Property object
static Instance *Object_Property(Instance *self,pArglist arglist,pVM vm)
{
   int64_t var = 0;
   arglistTryToGetInt(arglist,1,&var,0,vm);
   return propertyCreate(self, 
	arglistGetOnlyString(arglist,0,".Property",vm),(int)var, I_OWNED,vm);
}

Instance *notImplementedError(Instance *i, char *whatNot, pVM vm)
{
   char *name = _iname(i), *t = _name(i), buf[100];

   if (vm == (pVM)1) return 0;
   name = _iname(i); t = _name(i);
   if (name) sprintf(buf,"%s(%s) doesn't implement \"%s\"",t,name,whatNot);
   else	     sprintf(buf,"%s doesn't implement \"%s\"",t,whatNot);
   vmThrow(vm,E_NOT_IMPLEMENTED,buf);
   return Void;		// shut up the compiler
}

Instance *cantConvertError(Instance *i, char *to, pVM vm)
{
   char buf[100];
   if (vm == (pVM)1) return 0;
   sprintf(buf,"Conversion not possible for %s: %s",_name(i),to);
   vmThrow(vm,E_NOT_IMPLEMENTED,buf);
   return Void;		// shut up the compiler
}

    // .toBool()
static Instance *Object_toBool(Instance *self,pArglist arglist,pVM vm)
   { return cantConvertError(self,"toBool",vm); }

    // .toString()
static Instance *Object_toString(Instance *self,pArglist arglist,pVM vm)
   { return stringCreate(iname(self),I_OWNED,vm); }

Instance *iToString(Instance *self,pVM vm)
{
   #if USE_POINTER_INTS		// This is used (arglistConcat())
      ZKL_Int i64;
      if (IS_PtrInt(self)) self = decantInt(self,&i64);  // test is redundant
   #endif

   return M2_STRING(self,vm);
}

    // .toInt()
static Instance *Object_toInt(Instance *self,pArglist arglist,pVM vm)
   { return cantConvertError(self,"toInt",vm); }

    // .toFloat() -->error
static Instance *Object_toFloat(Instance *self,pArglist arglist,pVM vm)
   { return cantConvertError(self,"toFloat",vm); }

    // .toList() -->T(self)
Instance *Object_toList(Instance *self,pArglist arglist,pVM vm)
   { return tupleCreateX(vm,self,ZNIL); }

    // .toData() -->error
static Instance *Object_toData(Instance *self,pArglist arglist,pVM vm)
   { return cantConvertError(self,"toData",vm); }

    // .toType(i) --> convert self to same type as i
    // eg 1.toType(1.0) --> 1.toFloat()
static Instance *Object_toType(Instance *self,pArglist arglist,pVM vm)
{
   char      buf[200], *ptr;
   Instance *i = arglistGet(arglist,0,".toType",vm);
   pMethod   _toType;

   switch(TYPEO(i))
   {
      case ListType: case TupleType:	// List, ROList#, ROList
         ptr = "toList";
	 break;
      case StringType:	// at least 4 different names for String
         ptr = "toString";
	 break;
      default:
	 strcpy(buf,"to"); ptr = strcat(buf,ONAME(i));
	 break;
   }

   _toType = searchForMethod(self,ptr,1,vm);
   if (!_toType) cantConvertError(self,buf,vm);

   return _toType(self,NoArglist,vm);
}

static Instance *Object_sGet(Instance *self,pArglist arglist,pVM vm)
   { return notImplementedError(self,"__sGet",vm); }

static Instance *Object_sSet(Instance *self,pArglist arglist,pVM vm)
   { return notImplementedError(self,"__sSet",vm); }

    // .create()
static Instance *Object_create(Instance *self,pArglist arglist,pVM vm)
   { return notImplementedError(self,"create",vm); }

    // .len() --> 0
static Instance *Object_len(Instance *self,pArglist arglist,pVM vm)
   { return Zero; }

    /* (1).isType(2) --> True
     * ClassA.isType(ClassB) --> True, self.isType(self) --> True
     * self.fcn.isType(self.fcn) --> True
     * Native types are a catch all of type I don't know about at compile
     *   time (or don't care about, like Time.Clock).  I only have to worry
     *   about the Native.isType(Native); Known isn't type Native & vice
     *   versa.
     */
int objectIsType(Instance *self,Instance *other)
{
   int myType = TYPEO(self), theirType = TYPEO(other), s;

// vector self.isType(other) 
// !IntType && method != Object_isType

   if (myType == NativeType)	// Native.isType(Native | Known)
   {
      if (theirType != NativeType) return 0;	// Native.isType(Known)
      return (self->objID == other->objID);	// not PtrInts
   }
   if (myType == theirType)     return 1;	// Known.isType(Known)
   if (theirType == NativeType) return 0;	// Known.isType(Native)
   s = 0;	// not same types, unless ...
   switch(myType)	// mickie mouse
   {
      case ListType: case TupleType:
         s = (theirType == ListType || theirType == TupleType); break;
   }
   return s;
}

    // O.isType and O.isInstanceOf
Instance *objectIsIT(Instance *self,pArglist arglist,pVM vm)
{
   int	     n;
   Instance *i;

   for (n = 0; (i = listGet(arglist,n)); n++)
      if (objectIsType(self,i)) return BoolTrue;
   return BoolFalse;
}

    // .isType([type[,type ...]]) --> True if self is one of the types
    // List & Data don't use this
static Instance *Object_isType(Instance *self,pArglist arglist,pVM vm)
   { return objectIsIT(self,arglist,vm); }

    // .isInstanceOf([type[,type ...]]) --> True if self is one of the types
    // List & Data don't use this
    /* 1.isInstanceOf(2) --> True (both are Ints)
     * fcnF.isInstanceOf(fcnG) --> False
     * This is the same as isType for everything but classes and fcns and
     *   they overload this.
     */
static Instance *Object_isInstanceOf(Instance *self,pArglist arglist,pVM vm)
   { return objectIsIT(self,arglist,vm); }

    // .isChildOf() --> False
static Instance *Object_isChildOf(Instance *self,pArglist arglist,pVM vm)
   { return BoolFalse; }

    // .noop() --> self
Instance *Object_noop(Instance *self,pArglist arglist,pVM vm) { return self; }

    // .dir() --> Utils.Helpers.objectDir(self)
Instance *Object_dir(Instance *self,pArglist arglist,pVM vm)
{
   MLIST(mlist,1);
   return
     fcnRunFromClass(Utils_Helpers,"objectDir",mlistBuild(mlist,self,ZNIL),vm);
}

    // .defer(args) --> Deferred(self,args) == self : Deferred(_,args)
Instance *Object_defer(Instance *self,pArglist arglist,pVM vm)
   { return deferredCreate(self,arglist,vm); }

    // .findGlobalName(name): is name in globalNameTable?  -->False|N
Instance *Object_findGlobalName(Instance *self,pArglist arglist,pVM vm)
{
   char     *name = arglistGetOnlyString(arglist,0,0,vm);
   unsigned  n;
   int       s = findNameInGlobalTable(name,0x00,&n,vm);
   return s ? intCreate(n,vm) : BoolFalse;
}

    // .idToGlobalName(id)  -->""|name
Instance *Object_idToGlobalName(Instance *self,pArglist arglist,pVM vm)
{
   unsigned id = (unsigned)arglistGetInt(arglist,0,0,vm);
   if(id < _numNames) 
      return stringCreate(globalNameTableBackPtrs[id],I_OWNED,vm);
   return emptyString;
}

const MethodTable objectMethods[] = 
{					        //idx cache
   "create",		Object_create,		//  0, [0]
   "BaseClass",		Object_BaseClass,	//  1,
   "toString",		Object_toString,	//  2, [1]
   "toBool",		Object_toBool,		//  3, [2]
   "toInt",		Object_toInt,		//  4, [3]
   "toFloat",		Object_toFloat,		//  5, [4]
   "toData",		Object_toData,		//  6, [5]
   "toList",		Object_toList,		//  7, [6]
	// -------- End cached methods ----------------------------------
   "len",		Object_len,
   "isType",		Object_isType,
   "isInstanceOf",	Object_isInstanceOf,
   "__sGet",		Object_sGet,
   "__sSet",		Object_sSet,
   "fp",		Partial_fp,	// these are ordered: first
   "fp0",		Partial_fp0,
   "fp1",		Partial_fp1,
   "fp2",		Partial_fp2,
   "fpN",		Partial_fpN,
   "fpM",		Partial_fpM,	// last

   "resolve",		Object_resolveName,

   "copy",		Object_noop,
   "isChildOf",		Object_isChildOf,
   "method",		Object_method,
   "Method",		Object_Method,
   "property",		Object_property,
   "Property",		Object_Property,
   "noop",		Object_noop,
   "dir",		Object_dir,
   "toType",		Object_toType,

   "print",		Console_print,
   "println",		Console_println,

   0,			0
};

    // .methodName(idx,isIMethod = False)
    // down here so sizeof(objectMethods) has a value
static Instance *Object_methodName(Instance *self,pArglist arglist,pVM vm)
{
   unsigned int mix = (unsigned int)arglistGetInt(arglist,0,"methodName",vm);
   int	        isIMethod = arglistTryToGetBool(arglist,1,0,"methodName",vm);

   if(isIMethod)
   {	// I don't have a static check for the # of methods
      const MethodTable *table = OBJECTI(self)->methodTable;
      for(; mix && table->name; table++, mix--) ;
      if (0 == mix) return kStringCreate(table->name,0,I_OWNED,vm);
      return emptyString;
   }

        // stringCreate(0) --> emptyString, which is last entry in table (0,0)
   if (mix < sizeof(objectMethods)/sizeof(MethodTable))
      return kStringCreate(objectMethods[mix].name,0,I_OWNED,vm);
   return emptyString;
}

/* ******************************************************************** */
/* ************************ Object Properties ************************* */
/* ******************************************************************** */

typedef struct { char *name; void *m; } ATable;

    // Create a tuple of two lists of names with duplicates removed.
    // The tables are the method or property tables of an object
    //   They may be the same, as in Void properties
static Instance *
_makeAList(ATable *selfTable, ATable *objectTable, Instance *self, pVM vm)
{
   Instance *list;
   int	     i,n;
   Fence     fence;
   ATable    emptyTable = { 0, 0 };

   if (selfTable == objectTable) objectTable = &emptyTable;

   for (i = n = 0; selfTable[i].name;   i++) n++;
   for (i = 0;     objectTable[i].name; i++) n++;

   list = tupleCreate(n,I_OWNED,vm);	// probably bigger than necessary
   vmSetFence(vm,&fence,0,list);
      for (i = 0; selfTable[i].name; i++)
	 tupleAppend(list,kStringCreate(selfTable[i].name,self,I_OWNED,vm));
      for (i = 0; objectTable[i].name; i++)
      {
	 char *name = objectTable[i].name;
	 int   j;

	 for (j = 0; selfTable[j].name; j++)
	    if (0 == strcmp(name,selfTable[j].name)) { name = 0; break; }
	 if (name) tupleAppend(list,kStringCreate(name,self,I_OWNED,vm));
      }
   vmRemoveFence(&fence,0);
   return list;
}

    // .methods -->list of strings
static Instance *Object_methods(Instance *self,pVM vm)
{
   return 
      _makeAList((ATable *)METHOD_TABLE(self),(ATable *)objectMethods,self,vm);
}

    // .properties -->list of strings
static Instance *Object_properties(Instance *self,pVM vm)
{
   return  _makeAList((ATable *)PROPERTY_TABLE(self),
		 (ATable *)objectProperties,self,vm);
}

    // .name -->String
static Instance *Object_name(Instance *self,pVM vm)
   { return stringCreate(iname(self),I_OWNED,vm); }

    // .fullName -->String, VaultPath.name
static Instance *Object_fullName(Instance *self,pVM vm)
{
#if 0
   Instance *fullName = vaultBackTrace(self,vm);
   if (fullName != Void) return fullName;
   return stringCreate(iname(self),I_OWNED,vm);
#else
   Instance *fullName = vaultBackTrace(self,vm);
   ZKL_Object *obj = OBJECTI(self);

   if (fullName != Void) return fullName;

   if (!*obj->vaultPath) return stringCreate(obj->name,I_OWNED,vm);
   return stringCat(vm,"TheVault.",obj->vaultPath,".",obj->name,(char *)0);
#endif
}

    // .vaultPath -->String
static Instance *Object_vaultPath(Instance *self,pVM vm)
   { return kStringCreate(VAULT_PATH(self),self,I_OWNED,vm); }

    /* .type -->String
     * Need to be carefull about container/object name:  it is almost always
     * static but if I GC shared libraries, there could be a problem.  Also,
     * if self is sitting in the C stack, pointing to it is not good.
     */
static Instance *Object_type(Instance *self,pVM vm)
   { return kStringCreate(ONAME(self),0,I_OWNED,vm); }

    // .otype -->String
    // PtrInts have been boxed
static Instance *Object_otype(Instance *self,pVM vm)
   { return kStringCreate(typeToName(TYPEO(self)),0,I_OWNED,vm); }

    // .oID -->Int object id
static Instance *Object_oID(Instance *self,pVM vm)
   { return intCreate(self->objID,vm); }

    // .itype -->Int
static Instance *Object_itype(Instance *self,pVM vm)
   { return intCreate(self->itype,vm); }

    // .typeID -->Int
static Instance *Object_typeID(Instance *self,pVM vm)
   { return intCreate(TYPEO(self),vm); }

    // .size -->Int
static Instance *Object_size(Instance *self,pVM vm)
   { return intCreate(ISIZE(self),vm); }

    // .id -->Int (address of i)
static Instance *Object_id(Instance *self,pVM vm)
   { return intCreate(I_ID(self),vm); }

    // .numObjects --> num objects created
static Instance *Object_numObjects(Instance *self,pVM vm)
   { return intCreate(CAI_VALUE(&_numObjects),vm); }

    // .isThreadSafe -->Bool
static Instance *Object_isThreadSafe(Instance *self,pVM vm)
   { return boolCreate(I_IS_THREAD_SAFE(self)); }

    // .isContainer -->Bool
static Instance *Object_isContainer(Instance *self,pVM vm)
   { return boolCreate(I_IS_CONTAINER(self)); }

    // .createReturnsSelf -->Bool
static Instance *Object_createReturnsSelf(Instance *self,pVM vm)
   { return boolCreate(OBJECTI(self)->createReturnsSelf); }

    // .globalNameStats  -->(numNames,misc size)
Instance *Object_globalNameStats(Instance *self,pVM vm)
{
   #if 0	// if HASH_STATISTICS in dHash.c is 1
      hstats(globalNameTable);
      //hdump(globalNameTable);
   #endif
   return tupleCreateX(vm,intCreate(_numNames,vm),intCreate(miscSz,vm),ZNIL);
}

const PropertyTable objectProperties[] = 
{
   "id",		(pProperty)Object_id,
   "name",		(pProperty)Object_name,
   "fullName",		(pProperty)Object_fullName,
   "type",		(pProperty)Object_type,
   "otype",		(pProperty)Object_otype,
   "oID",		(pProperty)Object_oID,
   "itype",		(pProperty)Object_itype,
   "typeID",		(pProperty)Object_typeID,
   "properties",	(pProperty)Object_properties,
   "methods",		(pProperty)Object_methods,
   "vaultPath",		(pProperty)Object_vaultPath,
   "size",		(pProperty)Object_size,
   "numObjects",	(pProperty)Object_numObjects,
   "isThreadSafe",	(pProperty)Object_isThreadSafe,
   "isContainer",	(pProperty)Object_isContainer,
   "createReturnsSelf",	Object_createReturnsSelf,
   0,			0
};

/* ******************************************************************** */
/* ***************************** Op Codes ***************************** */
/* ******************************************************************** */

Instance *Object_eq(Instance *self,Instance *X,pVM vm)
{
   if (self == X) return BoolTrue;
   return BoolFalse;
}

Instance *Object_neq(Instance *self,Instance *X,pVM vm)
{
   if (self != X) return BoolTrue;
   return BoolFalse;
}

Instance *Object_lt(Instance *self,Instance *X,pVM vm)
   { return notImplementedError(self,"<",vm); }

Instance *Object_lte(Instance *self,Instance *X,pVM vm)
   { return notImplementedError(self,"<=",vm); }

Instance *Object_gt(Instance *self,Instance *X,pVM vm)
   { return notImplementedError(self,">",vm); }

Instance *Object_gte(Instance *self,Instance *X,pVM vm)
   { return notImplementedError(self,">=",vm); }

Instance *Object_negate(Instance *self,Instance *notUsed,pVM vm)
   { return notImplementedError(self,"unary minus",vm); }

Instance *Object_add(Instance *self,Instance *x,pVM vm)
   { return notImplementedError(self,"+",vm); }

Instance *Object_sub(Instance *self,Instance *X,pVM vm)
   { return notImplementedError(self,"-",vm); }

Instance *Object_mul(Instance *self,Instance *X,pVM vm)
   { return notImplementedError(self,"*",vm); }

Instance *Object_div(Instance *self,Instance *X,pVM vm)
   { return notImplementedError(self,"/",vm); }

Instance *Object_mod(Instance *self,Instance *X,pVM vm)
   { return notImplementedError(self,"%",vm); }

const OpcodeTable Object_OpcodeTable[] = 
{
   OP_EQ,	(pOp)Object_eq,
   OP_NEQ,	(pOp)Object_neq,
   OP_LT,	(pOp)Object_lt,
   OP_LTE,	(pOp)Object_lte,
   OP_GT,	(pOp)Object_gt,
   OP_GTE,	(pOp)Object_gte,

   OP_NEGATE,	(pOp)Object_negate,

   OP_ADD,	(pOp)Object_add,
   OP_SUB,	(pOp)Object_sub,
   OP_MUL,	(pOp)Object_mul,
   OP_DIV,	(pOp)Object_div,
   OP_MOD,	(pOp)Object_mod,

   0,		0
};

/* ******************************************************************** */
/* *********************************  ********************************* */
/* ******************************************************************** */

int isCachedMethod(const char *str, int *n);


    // Update object partial table if object redefines a fixed method
    // eg List defines toString, so update ListObject.toString
    // Called twice: Once to build defaults from Object
    // and again to over write defaults
static void methodFixup(ZKL_Object *object, const MethodTable *methods)
{
   if (methods)
   {
      int		 n;
      const MethodTable *ptr;

      object->methodTable = methods;	// "real" table, maybe incomplete

      	// build cache tables, method table is not sorted
      for (ptr = methods; ptr->name; ptr++)
         if(isCachedMethod(ptr->name,&n)) object->mcache[n] = ptr->method;
   }
}

static void propertyFixup(ZKL_Object *object, const PropertyTable *properties)
   { if (properties) object->propertyTable = properties; }

static void opFixup(ZKL_Object *object,const OpcodeTable *opcodes)
{
   if (opcodes)
   {
      const OpcodeTable *ptr;
      unsigned		offset;

      for (ptr = opcodes; ptr->op; ptr++)
      {
	 offset = ptr->offset;
	 if (offset < OPCODE_MAX) O_OP(object,offset) = ptr->op;
      }
   }
}

static void opStomp(ZKL_Object *dst, ZKL_Object *src)
{
   unsigned offset = OPCODE_MAX;

   while (offset--) O_OP(dst,offset) = O_OP(src,offset);
}

static void Object_fixup(
   ZKL_Object *object,
   const MethodTable *methods, const PropertyTable *properties,
   const OpcodeTable *opcodes)
{
   methodFixup(object,objectMethods);		// build defaults
   methodFixup(object,methods);			// overwrite defaults
   propertyFixup(object,objectProperties);
   propertyFixup(object,properties);
   opFixup(object,Object_OpcodeTable); opFixup(object,opcodes);
}

#if 0
static void Object_fixup2(
   ZKL_Object *object,
   MethodTable *methods, PropertyTable *properties,OpcodeTable *opcodes)
{
   methodFixup(object,methods);
   propertyFixup(object,properties);
   opFixup(object,opcodes);
}
#endif


void objectConstruct(void)
{
   CAI_INIT(&_numObjects);

   globalNameTable = hcreate(2500,0);	// 1046 keys on start up
   writeLockInit(&globalNameLock);
   buildObjectLinkTable(0, objectMethods, objectProperties, NoVM);
}


////////////////////////////////////////////////////////////////////////
/* IMPORTANT:  Don't replace objectMethods or objectProperties with
 * the hashed tables, Object needs the ordered list (see the fixup code).
 */

	// One copy of the compile time character set check here
#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gnu-gperf@gnu.org>."
#endif


//////////////////////////////////////////////////////////////////////////
///////////////// Fixed Fixup Hash Table /////////////////////////////////
// gperf fixupList.txt
// hash --> fixupHash
// in_word_set --> int isCachedMethod(const char *str, int *n)
// wordlist --> static const struct { char *name; int tix; } wordlist[] =
// several other tweaks


#if 1
/* ANSI-C code produced by gperf version 3.0.4 */
/* Command-line: gperf fixupList.txt  */
/* Computed positions: -k'3' */

/* maximum key range = 9, duplicates = 0 */

static unsigned int
fixupHash (register const char *str, register unsigned int len)
{
  static const unsigned char asso_values[] =
    {
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14,  4, 14,  7, 14,
       0, 14, 14,  0, 14, 14,  5, 14, 14, 14,
      14, 14, 14,  0, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14,  0, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
      14, 14, 14, 14, 14, 14
    };
  return len + asso_values[(unsigned char)str[2]];
}

int isCachedMethod(const char *str, int *n)
{
  enum
    {
      TOTAL_KEYWORDS = 7,
      MIN_WORD_LENGTH = 5,
      MAX_WORD_LENGTH = 8,
      MIN_HASH_VALUE = 5,
      MAX_HASH_VALUE = 13
    };

  static const struct { char *name; int n; } wordlist[] =
    {
      {""}, {""}, {""}, {""}, {""},
      {"toInt",		TO_INT},
      {"create",   	M_CREATE},
      {"toFloat",	TO_FLOAT},
      {"toString",	TO_STRING},
      {""},
      {"toBool",	TO_BOOL},
      {"toList",	TO_LIST},
      {""},
      {"toData",	TO_DATA}
    };

  register unsigned int len = (unsigned int)strlen(str);
  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = fixupHash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strcmp (str + 1, s + 1))
	  {
	     *n = wordlist[key].n;
	     return 1;
	  }
        }
    }
  return 0;
}
#endif


///////////////////////////////////////////////////////////////////
// zkl extractTable.zkl < object.c    | gperf | zkl gperf.zkl -i object
// zkl extractTable.zkl -p < object.c | gperf | zkl gperf.zkl -i object





/////////////////////////////////////////////////////////////////
///////////////// Property Table ////////////////////////////////
/////////////////////////////////////////////////////////////////
// zkl extractTable.zkl -p < object.c | gperf | zkl gperf.zkl -i object


