/* dictionary.c : hash tables
 * 
 * Copyright (c) 2006,2007,2008-12,2013 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#include <stdio.h>
#include <string.h>

#define __NOT_A_DLL
#define __STRING_INTERNALS	// for SmallDictionary

#include "zklObject.h"
#include "zklDictionary.h"
#include "zklFcn.h"
#include "zklList.h"
#include "zklNumber.h"
#include "zklString.h"
#include "zklUtil.h"

static ZKL_Object  DictionaryObject;

ENTRY *dictionaryFindAdd(Instance *, ENTRY *, pVM);

//!!! only lock if not read only
typedef struct
{
   BInstance  instance;	   // Inherit from BInstance
   void	     *hashTable;
   WriteLock  lock;
   uint8_t    howza, howzaTmp;
} Dictionary;	// 16 Windows32, 32 bytes Linux64

#define D(i)		( (Dictionary *)i )
#define TABLE(i)	(  D(i)->hashTable )
#define LOCK(i)		( &D(i)->lock )

#define IS_RO(d)	hIsReadOnly(TABLE(d))

#define D_HOWZA(d)	( ((Dictionary *)d)->howza )
#define D_HOWZA_TMP(d)	( ((Dictionary *)d)->howzaTmp )

static IBucketHeader dcBuckets;

Instance *dictionaryCreate(size_t initialSize, int itype, pVM vm)
{
   Dictionary *d;
   void       *hashTable;

   hashTable = hcreate((unsigned)initialSize,1);	// lots-o-mallocs
   if (!hashTable)
   {
      collectGarbage(1,vm);	// wait
      hashTable = hcreate((unsigned)initialSize,1);	// try again
      if (!hashTable) vmThrow(vm,E_OUT_OF_MEMORY,0);
   }
   d = (Dictionary *)ibucketAllocate(&dcBuckets,&DictionaryObject,itype,0,vm);
   if (!d)
   {
      hdestroy(hashTable);
      vmThrow(vm,E_OUT_OF_MEMORY,0);
   }
   d->hashTable = hashTable;
   writeLockInit(&d->lock);
   d->howza = d->howzaTmp = 0;
   return containerIsCooked(&dcBuckets,(Instance *)d,itype);
}

static int dictionaryFree(Instance *self)
{
   hdestroy(TABLE(self));	// does the right thing if table is 0
   return 1;
}

    // Self is locked for reading. Probably can deadlock.
    // If you create during a walk, YOU need to fence
ENTRY *dWalk(Instance *self,DhashCb fcn, void *X, pVM vm)
{
   ENTRY *s;

   if (IS_RO(self)) return hWalk(TABLE(self),fcn,X);

   writeLockAcquire(LOCK(self),WL_READER,vm);
     s = hWalk(TABLE(self),fcn,X);
   writeLockRelease(LOCK(self),WL_READER);
   return s;
}

static int _markKV(void *dx, Instance *key, Instance *value, size_t n)
{
   instanceMark(key); instanceMark(value);
   return 1;
}
static void dMarker(Instance *self)
{
#if 1
   dWalk(self,_markKV,0,0);
#else
   hWalk(TABLE(self),_markKV,0);
#endif
}

    /* Gen a hash key for use in lookup. Hash keys are Strings.
     *   String/KString		--> unchanged
     *   Number (int & float)	--> toString
     *   default		--> id.toString
     * 
     * Output: if *buf, buf was used, else *buf == '\0'
     */
static char *_createHashKey(Instance *key,char *buf,pVM vm)
{
   *buf = '\0';
   switch (TYPEO(key))
   {
      case StringType: return stringText(key);	// or KString
      case IntType:    intToA(convertToInt(key,vm),buf); return buf;
      case FloatType:  sprintf(buf,"%f",FLOATV(key));    return buf;
      default:	       intToA(I_ID(key),buf);		 return buf;
   }
//   sprintf(buf,"Dictionary: Can't use a %s as a key",typeToName(TYPEO(key)));
//   vmThrow(vm,E_VALUE_ERROR,buf);
   return "";		// doesn't get here
}

    // Create a hash key for storage in the hash table
    // KCStrings are a problem because I don't want the fcn/container they are 
    // in to be forced to stick around (ie avoid being GC'd)
static Instance *createHashKey(Instance *key,pVM vm)
{
   char  buf[100];
   char *k;

   if (TYPEO(key) == StringType && !OBJECT1(key)->magicMarker) return key;
   k = _createHashKey(key,buf,vm);
   return stringCreate(k,I_OWNED,vm);
}

    // does it or throws
    // RO flag honored
static void dAdd(Instance *self, ENTRY *item, pVM vm)
{
   int s;
   writeLockAcquire(LOCK(self),WL_WRITER,vm);
      s = hEnter(TABLE(self),item);
      writeLockRelease(LOCK(self),WL_WRITER);
      switch(s)
      {
	 case 0: break;		// success
	 case 1: vmThrow(vm,E_ACCESS_ERROR, "This Dictionary is read only");
	 case 2: vmThrow(vm,E_OUT_OF_MEMORY,0);
      }
}

    // Key is a String.
    // If calling from a Method, you may need to orphanize key/value
void dictionaryAdd(Instance *self, Instance *key, Instance *value, pVM vm)
{
   ENTRY item;

   if (TYPEO(key) != StringType)	// or KString/KCString
      vmThrow(vm,E_TYPE_ERROR,"Dictionary.add: Key must be String");

   item.key   = key;
   item.value = value;
   dAdd(self,&item,vm);
}

static size_t dictionaryLen(Instance *self,pVM vm)
{
   size_t n;

   if (IS_RO(self)) return hkeyCount(TABLE(self));

   writeLockAcquire(LOCK(self),WL_READER,vm);
      n = hkeyCount(TABLE(self));
   writeLockRelease(LOCK(self),WL_READER);
   return n;
}

/* ******************************************************************** */
/* *************************** Methods ******************************** */
/* ******************************************************************** */

    // D.add(T(T(1,2),T(3.4)...)), add(T(k,v,k,v...)
    // Locking is done by a called routine, not the best but simple
static void addKVList(Instance *self,Instance *kvlist, int orphanize, pVM vm)
{
   unsigned  n,j;
   Instance *i;

   n = TYPEO(kvlist);
   if (!ISA_LIST(n))
      vmThrow(vm,E_VALUE_ERROR,"Dictionary add(k,v), add(T(k,v)) or T(T(k,v)");

   if (!(i = listGet(kvlist,0))) return;	// empty list
   n = TYPEO(i);
   if (n == ListType || n == TupleType)		// T(T(1,2),T(3.4)...))
   {
      verifyList("Dictionary.add/extend",kvlist,ListType,vm);
      n = 0;
      while((i = listGet(kvlist,n++)))
      {
	 Instance *key   = createHashKey(arglistGet(i,0,0,vm),vm);
	 Instance *value = arglistGet(i,1,0,vm);
	 dictionaryAdd(self,key,value,vm);
	 if (orphanize) instanceIsOrphan2(key,value);
      }
   }
   else		// T(k,v,k,v...)
   {
      n = (unsigned)listLen(kvlist,vm);

      if (n % 2 != 0)
      {
	 char buf[100];
	 sprintf(buf,"Add key,value PAIRS to Dictionary");
	 vmThrow(vm,E_VALUE_ERROR,buf);
      }

//!!! need a fence in case of no mem
      for (j = 0; j < n; j += 2)
      {
	 ENTRY item;

	 item.key   = createHashKey(arglistGet(kvlist,j,0,vm),vm);
	 item.value = arglistGet(kvlist,j+1,0,vm);
	 dAdd(self,&item,vm);
if (orphanize) instanceIsOrphan2(item.key,item.value);
      }
   }
}

    // .add( key,value, ... | L(L(key,value) ...) | L(k,v,....))
    // also .write
static Instance *Dictionary_add(Instance *self,pArglist arglist,pVM vm)
{
   int n = (int)listLen(arglist,vm);

   if (n == 1)	// .add(T(T(1,2),T(3.4)...))
   {
      addKVList(self,arglistGet(arglist,0,0,vm),1,vm);
      return self;
   }
   else		// .add(k,v,k,v...)
      addKVList(self,arglist,1,vm);
   return self;
}

    // .appendV(key,value): append value to key
    // convertUnits uses list[.id] as key
static Instance *Dictionary_appendV(Instance *self,pArglist arglist,pVM vm)
{
   Instance *key   = createHashKey(arglistGet(arglist,0,0,vm),vm);
   Instance *value = arglistGet(arglist,1,0,vm);
   ENTRY     item, *pi;
   
   item.key   = key;
   item.value = value;
   if (!(pi = dictionarySearchFor(self,ZKL_STRING(key),vm)))
   {
      item.value = listCreateX(vm,value,ZNIL); dAdd(self,&item,vm); 
      instanceIsOrphan(item.value);  // 2 threads
   }
   else if (TYPEO(pi->value)==ListType)
      { listAppend(pi->value,value,vm); dAdd(self,pi,vm); }

   return self;
}

    // .appendKV(T(key,value)): append value to key
static Instance *Dictionary_appendKV(Instance *self,pArglist arglist,pVM vm)
   { return Dictionary_appendV(self,arglistGetBT(arglist,0,ListType,0,vm),vm); }


    // .incV(key): inc value
static Instance *Dictionary_incV(Instance *self,pArglist arglist,pVM vm)
{
#if 0
   Instance *key = createHashKey(arglistGet(arglist,0,0,vm),vm);
   ENTRY     item, *pi;
   
   item.key = key;
   if (!(pi = dictionarySearchFor(self,ZKL_STRING(key),vm)))
      { item.value = One; dAdd(self,&item,vm); }
   else if (TYPEO(pi->value)==IntType)
   {
      Instance *v=pi->value;
      #if USE_POINTER_INTS
         if (IS_PtrInt(v)) v = intCreate(PtrInt_TO_N(v) + 1,vm);
	 else  // int object
      #endif
      v = intCreate(INT64V(v) + 1,vm);
      pi->value = v;
      dAdd(self,pi,vm); 
   }
   return self;
#else
   unsigned  n;
   Instance *key;
   ENTRY     item, *pi;

#if 0
   for(n=0; (key = listGet(arglist,n)); n++)
   {
      item.key = createHashKey(key,vm);
      if (!(pi = dictionarySearchFor(self,ZKL_STRING(item.key),vm)))
	 { item.value = One; dAdd(self,&item,vm); }
      else if (TYPEO(pi->value)==IntType)
      {
	 Instance *v=pi->value;
	 #if USE_POINTER_INTS
	    if (IS_PtrInt(v)) v = intCreate(PtrInt_TO_N(v) + 1,vm);
	    else  // int object
	 #endif
	 v = intCreate(INT64V(v) + 1,vm);
	 pi->value = v;
	 dAdd(self,pi,vm); 
      }
   }
#else
   #define NADA ~1
   for(n=0; (key = listGet(arglist,n)); n++)
   {
      item.key   = createHashKey(key,vm);
      item.value = (Instance *)NADA;
      if ((pi = dictionaryFindAdd(self,&item,vm)))
      {
	 if(pi->value == (Instance *)NADA)	// new entry, value-->1
	    pi->value = One;
	 else if(TYPEO(pi->value)==IntType)     // value++
	 {
	    Instance *v=pi->value;
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(v)) v = intCreate(PtrInt_TO_N(v) + 1,vm);
	       else  // int object
	    #endif
	    v = intCreate(INT64V(v) + 1,vm);
	    pi->value = v;
	 }
      }
      // else full | RO | (key,!int)
   }
#endif
   return self;
#endif
}

    // .create([initialSize | key,value, ... | L(L(key,value) ...) ])
    // -->Dictionary
Instance *Dictionary_create(Instance *self,pArglist arglist,pVM vm)
{
   Instance *d, *a=0;
   size_t    n = listLen(arglist,vm);
   int	     t = 0;

   if (n == 0) return dictionaryCreate(0,I_OWNED,vm);
   if (n == 1)
   {
      a = arglistGet(arglist,0,0,vm);
      t = TYPEO(a);
      if (t == ListType || t == TupleType) n = listLen(a,vm);
      else return
	dictionaryCreate((size_t)arglistGetInt(arglist,0,"D.create(size)",vm),I_OWNED,vm);
   }
   d = dictionaryCreate(n/2 + 1,I_OWNED,vm);
   if (a) addKVList(d,a,0,vm);
   else   addKVList(d,arglist,0,vm);

   return d;
}

    // .extend(Dictionary | List) --> self
static int _add(DX *dx, Instance *key, Instance *value, size_t n)
{
   ENTRY item = { key,value };
   dAdd(dx->x,&item,dx->vm);
   return 1;
}
static Instance *Dictionary_extend(Instance *self,pArglist arglist,pVM vm)
{
   Instance *e = arglistGet(arglist,0,0,vm);

   if (TYPEO(e) == DictionaryType)
   {
      DX dx = { self,vm };
      dWalk(e,(DhashCb)_add,&dx,vm);	// dAdd locks self, dWalk locks d
      instanceIsOrphan(e);
   }
   else addKVList(self,e,1,vm);		// List?

   return self;
}

    // .copy() --> new RW Dictionary
    // This is the same as self.pump(Dicionary())
static Instance *Dictionary_copy(Instance *self,pArglist arglist,pVM vm)
{
   DX	     dx;
   Instance *d;
   Fence     fence;
   size_t    len = dictionaryLen(self,vm);

   if (len == 0) return dictionaryCreate(0,I_OWNED,vm);
//!!!! geez, this is inefficient
   d = dictionaryCreate(len,I_OWNED,vm);
   vmSetFence(vm,&fence,0,d);
      dx.x = d; dx.vm = vm;
      dWalk(self,(DhashCb)_add,&dx,vm);		// locks self
   vmRemoveFence(&fence,0);
   return d;
}

    // .len()
static Instance *Dictionary_len(Instance *self,pArglist arglist,pVM vm)
   { return intCreate(dictionaryLen(self,vm),vm); }

static void readUnlockerF(Fence *fence, Instance *e)
   { writeLockRelease(LOCK(fence->i),WL_READER); }

    // .toString([maxKeys=20, depth=2,evalable=False])
    //     --> "Dictionary(1:2,D(3:D(...)))"
    // DX: n == recursion level/depth, a == max keys
static void dToString(Instance *, DX *, int);
#if 0
static void quoteStr(ZBText *buf,Instance *i,int quoteIt,int comma, pVM vm)
{
   if(quoteIt && TYPEO(i)==StringType)
      zbtextAppendX(buf,vm,"\"",ZKL_STRING(i),comma ? "\"," : "",ZNIL);
   else
   {
      zbtextAppendI(buf,i,vm);
      if(comma) zbtextAppendN(buf,":",1,vm);
   }
}
#endif
static int _catItem(DX *dx, Instance *key, Instance *value, size_t nthKey)
{
   pVM     vm  = dx->vm;
   ZBText *buf = (ZBText *)(dx->x);

   if (nthKey) zbtextAppendN(buf,",",1,vm);
   if (nthKey >= dx->a)	// too many keys
   {
      zbtextAppendN(buf,"...",3,vm);
      return 0;		// stop
   }

   if (TYPEO(value) == DictionaryType)	// possible recursion
   {
      if (dx->n > 0) // recursing a bit toooo far, 
      {		     // too many nested dictionaries --> "key:D(...)"
	 zbtextAppend(buf,ZKL_STRING(key),vm);	// keys are always Strings
	 zbtextAppendN(buf,":D(...)",7,vm);

      	 return 0;	// stop
      }
      // --> "<key>:D(<dictionary>)"
#if 1
      zbtextAppendX(buf,vm,ZKL_STRING(key),":",ZNIL);
#else
      quoteStr(buf,key,evalable,1,vm);
#endif
      dx->n++;		// recursion
         dToString(value,dx,1);
      dx->n--;

      return 1;		// carry on
   }
   	// "<previous item> key:value"
#if 1
   zbtextAppendX(buf,vm,ZKL_STRING(key),":",ZNIL);
   zbtextAppendI(buf,value,vm);
#else
   quoteStr(buf,key,evalable,1,vm);
   quoteStr(buf,value,evalable,0,vm);
#endif

   return 1;
}
static void dToString(Instance *self, DX *dx, int recursing)  // recursive
{
   pVM     vm  = dx->vm;
   ZBText *buf = (ZBText *)(dx->x);

//   zbtextAppend(buf,"Dictionary(",vm);
   zbtextAppend(buf, recursing ? "D(" : "Dictionary(", vm);
   dWalk(self,(DhashCb)_catItem,dx,0);	// dWalk locks self
   zbtextAppendN(buf,")",1,vm);
}

    // .toString(max keys=20), max keys = n | *
Instance *Dictionary_toString(Instance *self,pArglist arglist,pVM vm)
{
   ZBText    buf;
   DX	     dx = { (Instance *)&buf,vm,0,0 };
   Fence     fence;
   size_t    maxKeys;
   Instance *text;

   if (!arglistGetSize(arglist,0,(size_t)-1,&maxKeys,vm)) maxKeys = 20;
   dx.a = maxKeys;

   vmSetFence(vm,&fence,readUnlockerF,self);  // dwalk() locks
      fence.i1 = zbtextInit(&buf,vm);
	 dToString(self,&dx,0);
      text = zbtextClose(&buf,vm);
   vmRemoveFence(&fence,0);
   return text;
}

    // .toList() -->L(T(key,value) ... )
#if 0
static int _appendItem(DX *dx, Instance *key,Instance *value, size_t n)
{
   Instance *pair;
   pVM       vm = dx->vm;
   pair = tupleCreateX(vm,key,value,ZNIL);
   listAppend(dx->x,pair,vm);
   return 1;
}
static Instance *Dictionary_toList(Instance *self,pArglist arglist,pVM vm)
{
   DX	 dx = { 0,vm };
   Fence fence;

   vmSetFence(vm,&fence,readUnlockerF,self);
      fence.i1 = dx.x = listCreate(hkeyCount(TABLE(self)),0x2,I_OWNED,vm);
      dWalk(self,(DhashCb)_appendItem,&dx,vm);	// locks self
   vmRemoveFence(&fence,0);
   return dx.x;
}
#else
static Instance *Dictionary_toList(Instance *self,pArglist arglist, pVM vm)
{
   Instance *args;
   MLIST(mlist,2);

   args = mlistBuild(mlist,self,ZNIL);
   return fcnRunFromClass(Utils_Helpers,"dictionaryToList", args, vm);
}
#endif

    // .toBool() -->Bool
static Instance *Dictionary_toBool(Instance *self,pArglist arglist,pVM vm)
{
   long n;

   if (IS_RO(self)) n = hkeyCount(TABLE(self));
   else
   {
      writeLockAcquire(LOCK(self),WL_READER,vm);
	 n = hkeyCount(TABLE(self));
      writeLockRelease(LOCK(self),WL_READER);
   }
   return n ? BoolTrue : BoolFalse;
}

ENTRY *dictionarySearchFor(Instance *self, char *key, pVM vm)
{
   ENTRY *item;

   if (IS_RO(self)) return hSearchFor(TABLE(self),key);

   writeLockAcquire(LOCK(self),WL_READER,vm);
      item = hSearchFor(TABLE(self),key);
   writeLockRelease(LOCK(self),WL_READER);

   return item;
}

   // -->0 if table full or RO
   // -->ptr to entry in table
ENTRY *dictionaryFindAdd(Instance *self, ENTRY *kv, pVM vm)
{
   if (IS_RO(self)) return 0;

   writeLockAcquire(LOCK(self),WL_READER,vm);
      kv = hSearch(TABLE(self),stringText(kv->key),kv,FINDADD);
   writeLockRelease(LOCK(self),WL_READER);

   return kv;
}

    // .find(key,[default]) -->value|default|Void
    // D(1,Void).find(1) -->Void, D.find("foo",List)-->List()
static Instance *Dictionary_find(Instance *self,pArglist arglist,pVM vm)
{
   ENTRY *item;
   char	  buf[100];
   char	 *key;

   key  = _createHashKey(arglistGet(arglist,0,"Dictionary.find",vm),buf,vm);
   item = dictionarySearchFor(self,key,vm);
   if (!item)
   {
      Instance *defaultVal = arglistTryToGet(arglist,1);
      return defaultVal ? defaultVal : Void;
   }
   return item->value;
}

    // D[key] -->value, .get(key)
static Instance *Dictionary_sGet(Instance *self,pArglist arglist,pVM vm)
{
   ENTRY *item;
   char	  buf[100];
   char	 *key = _createHashKey(arglistGet(arglist,0,0,vm),buf,vm);

   if (!(item = dictionarySearchFor(self,key,vm)))
   {
      char buf[100];
      sprintf(buf,"Dictionary key \"%.50s\" not found",key);
      vmThrow(vm,E_NOT_FOUND,buf);
   }
   return item->value;
}

    // D[key] = value  == sSet(value,key) -->value
static Instance *Dictionary_sSet(Instance *self,pArglist arglist,pVM vm)
{
   Instance *value = arglistGet(arglist,0,0,vm);
   Instance *key   = createHashKey(arglistGet(arglist,1,0,vm),vm);

   dictionaryAdd(self,key,value,vm);
   instanceIsOrphan2(key,value);
   return value;
}

    // .holds(key) -->Bool
static Instance *Dictionary_holds(Instance *self,pArglist arglist,pVM vm)
{
   char	 buf[100];
   char *key = _createHashKey(arglistGet(arglist,0,"Dictionary.holds",vm),
			      buf,vm);
   return dictionarySearchFor(self,key,vm) ? BoolTrue : BoolFalse;
}

    // .pop(key,default=Void) -->i, also .del
//!!!!.del should return self
static Instance *Dictionary_pop(Instance *self,pArglist arglist,pVM vm)
{
   ENTRY item, *r;
   char	 buf[100];
   char *key = _createHashKey(arglistGet(arglist,0,"Dictionary.pop",vm),
			      buf,vm);
   Instance *i;

   if (IS_RO(self)) vmThrow(vm,E_ASSERTION_ERROR,"Dictionary.pop: read only");

   writeLockAcquire(LOCK(self),WL_WRITER,vm);
      r = hSearch(TABLE(self),key,&item,DEL);
   writeLockRelease(LOCK(self),WL_WRITER);

   if (r) return r->value;
   i = arglistTryToGet(arglist,1);
   if (i) return i;
   return Void;
}

    // .del(key) --> self, remove key from self
static Instance *Dictionary_del(Instance *self,pArglist arglist,pVM vm)
{
   Dictionary_pop(self,arglist,vm);
   return self;
}

    // howza([getter mode, tmp=False]) --> self|int
Instance *Dictionary_howza(Instance *self,pArglist arglist,pVM vm)
{
   int64_t i64;

   D_HOWZA_TMP(self) = D_HOWZA(self);	// reset
   if (arglistTryToGetInt(arglist,0,&i64,0,vm))
   {
      if(arglistTryToGetBool(arglist,1,0,0,vm)) 
           D_HOWZA_TMP(self) = (unsigned)i64;
      else D_HOWZA(self) = D_HOWZA_TMP(self) = (unsigned)i64;
      return self;
   }
   return intCreate(((Dictionary *)self)->howza,vm);
}

static unsigned getHowza(Instance *d)
{
   unsigned n = D_HOWZA_TMP(d);
   D_HOWZA_TMP(d) = D_HOWZA(d);		// reset
   return n;
}

static Instance *_dgetter(Instance *self,size_t n,void *pdn,size_t _,pVM vm)
{
   HNextData *dn = (HNextData *)pdn;
   ENTRY     *e  = hNext(((Dictionary *)self)->hashTable, dn);

   if (!e) return 0;
   switch(dn->howza)
   {
      case 8: return e->key;
      case 9: return e->value;
   }
   return tupleCreateX(vm,e->key,e->value,ZNIL);
}

    // D.reduce
static Instance *Dictionary_reduce(Instance *self,pArglist arglist,pVM vm)
{
   HNextData dn = { 0 }; // all zeros
//   dn.howza = D_HOWZA(self);
   dn.howza = getHowza(self);
   return zreduce(self,_dgetter,(void *)&dn,arglist,0,vm);
}

    // D.pump( ... L(k,v)
static Instance *Dictionary_pump(Instance *self,pArglist arglist,pVM vm)
{
   HNextData dn = { 0 }; // all zeros
//   dn.howza = D_HOWZA(self);
   dn.howza = getHowza(self);
   return pump(self,0x00,_dgetter,(void *)&dn,arglist,0,vm);
}

    // D.filter
static Instance *Dictionary_filter(Instance *self,pArglist arglist,pVM vm)
{
   HNextData dn = { 0 }; // all zeros
//   dn.howza = D_HOWZA(self);
   dn.howza = getHowza(self);
   return zfilter(self,0x0,_dgetter,(void *)&dn,arglist,0,vm);
}
static Instance *Dictionary_filter1(Instance *self,pArglist arglist,pVM vm)
{
   HNextData dn = { 0 }; // all zeros
//   dn.howza = D_HOWZA(self);
   dn.howza = getHowza(self);
   return zfilter1(self,0,_dgetter,(void *)&dn,arglist,0,vm);
}

    /* D.walker(howza=0)
     *   0(k,v), 8(keys), 9(values)
     */
static Instance *Dictionary_walker(Instance *self,pArglist arglist,pVM vm)
{
   if (IS_RO(self))
   {
      int64_t    i64 = D_HOWZA(self);
      HNextData *dn  = (HNextData *)ZCALLOC(sizeof(HNextData),1); // zero'd
      arglistTryToGetInt(arglist,0,&i64,0,vm);
      dn->howza      = (int)i64;

      return walkerCreate(self,_dgetter,(void *)dn,1,vm);
   }

   vmThrow(vm,E_ASSERTION_ERROR,"Dictionary walker: Only if read only");
   return Void;
}

    // Thread safe
Instance *dictionaryMakeRO(Instance *self,pVM vm)
{
   if (self && TYPEO(self) == DictionaryType)
   {
      writeLockAcquire(LOCK(self),WL_WRITER,vm);
         hMakeReadOnly(TABLE(self));
      writeLockRelease(LOCK(self),WL_WRITER);
   }
   return self;
}

    // Dictionary.makeReadOnly() -->self
static Instance *makeReadOnly(Instance *self,pArglist arglist, pVM vm)
   { return dictionaryMakeRO(self,vm); }

static Instance *SD_create(Instance *,pArglist,pVM);

static const MethodTable methods[] = 
{
   "toBool",		(pMethod)Dictionary_toBool,
   "toDictionary",	(pMethod)Object_noop,
   "toString",		(pMethod)Dictionary_toString,
   "toList",		(pMethod)Dictionary_toList,
   "create",		(pMethod)Dictionary_create,
   "copy",		(pMethod)Dictionary_copy,
   "len",		(pMethod)Dictionary_len,
   "holds",		(pMethod)Dictionary_holds,
   "__sSet",		(pMethod)Dictionary_sSet,
   "__sGet",		(pMethod)Dictionary_sGet,
   "get",		(pMethod)Dictionary_sGet,
   "add",		(pMethod)Dictionary_add,
   "write",		(pMethod)Dictionary_add,
   "extend",		(pMethod)Dictionary_extend,
   "find",		(pMethod)Dictionary_find,
   "walker",		(pMethod)Dictionary_walker,
   "makeReadOnly",	(pMethod)makeReadOnly,
   "SD",		(pMethod)SD_create,
   "reduce",		Dictionary_reduce,
   "pump",		Dictionary_pump,
   "filter",		Dictionary_filter,
   "filter1",		Dictionary_filter1,
   "howza",		Dictionary_howza,
   "pop",		Dictionary_pop,
   "del",		Dictionary_del,
   "appendV",		Dictionary_appendV,
   "appendKV",		Dictionary_appendKV,
   "incV",		Dictionary_incV,
   0,			0,
};

/* ******************************************************************** */
/* *************************** Properties ***************************** */
/* ******************************************************************** */

    // .keys --> L(key, key ...)
    // A common case: d.keys.sort()
static int _stashKey(DX *dx, Instance *key, Instance *value, size_t n)
{
   listAppend(dx->x,key,dx->vm);  // won't GC or throw (unless self grew)
   return 1;
}
Instance *Dictionary_keys(Instance *self,pVM vm)
{
//!!!! not thread safe
   Instance *keys = listCreate(hkeyCount(TABLE(self)),0x2,I_OWNED,vm);
   DX	     dx   = { keys,vm };

   dWalk(self,(DhashCb)_stashKey,&dx,vm);	// locks self
   return keys;
}

    // .values --> L(value,value ...)
static int _stashValue(DX *dx, Instance *key, Instance *value, size_t n)
{
   listAppend(dx->x,value,dx->vm);  // won't GC or throw (unless self grew)
   return 1;
}
static Instance *Dictionary_values(Instance *self,pVM vm)
{
//!!!! not thread safe
   Instance *values = listCreate(hkeyCount(TABLE(self)),0x2,I_OWNED,vm);
   DX	     dx     = { values,vm };

   dWalk(self,(DhashCb)_stashValue,&dx,vm);	// locks self
   return values;
}

static Instance *Dictionary_isReadOnly(Instance *self,pVM vm)
   { return boolCreate(IS_RO(self)); }

static const PropertyTable properties[] = 
{
   "keys",		(pProperty)Dictionary_keys,
   "values",		(pProperty)Dictionary_values,
   "isReadOnly",	(pProperty)Dictionary_isReadOnly,
   0,			0
};


/* ******************************************************************** */
/* ***************** Small fixed size hash table  ********************* */
/* *************************** Poly Graphs **************************** */
/* ******************************************************************** */
// Small fixed pseudo hash tables: < 256 keys
// PolyGraphs are SmallDictionarys with all values == True. Or no values.

    /* Search with a quickie first letter hash into sorted buckets followed
     * by a B search of the bucket.
     * http://en.wikipedia.org/wiki/Letter_frequency
     *   t:16.671, a:11.602, s:7.755, h:7.232, w:6.661, i:6.286, o:6.264,
     *   b:4.702, m:4.374, f:3.779, c:3.511, l:2.705, d:2.67, p:2.545,
     *   n:2.365, e:2, g:1.95, r:1.653, y:1.62, u:1.487, k:0.69, j:0.631,
     *   v:0.619, q:0.173, z:0.05, x:0.005
     */
unsigned char phashLetter1[] =		// a terminated string, len=22
   { 'A','B',    'F','H','I','M','O','S','T','W', '_',
     'a','b','c','f','h','i','m','o','s','t','w',  0 };
#define HASH_TABLE_LEN ( sizeof(phashLetter1) / sizeof(unsigned char) - 1 )

#define MIN_PPSIZE_FOR_HASH   3  // only alloc hash if > than MIN

int phashMap[256];     // ASCII map to phashLetter1, -1 if no mapping

static void buildHashMap(void)
{
   unsigned char *letter = phashLetter1, c;
   int   n = 0;
   memset(phashMap,-1,256*sizeof(int));
   while((c = *letter++))
   {
      unsigned char d = *letter;
      if (!d) d = 'z' + 1;
      for(; c < d; c++) phashMap[c] = n;
      n++;
   }
}

int	// hash first letter of key to a "bucket"
phashFcn(unsigned char *word,Byte *table, int ppSize, int *start,int *end)
{
   int a = 0, b = ppSize, h = phashMap[*word]; // "Foo" --> phashMap[F] --> 2
   if (h >= 0)
   {
      int x = h;
      if (table[x] == 0xff && *word == phashLetter1[x])  // no f words
	 return 0;
      while((a = table[x]) == 0xff)	// table[2] --> n | 0xFF
         if (--x == 0) { a = 0; break; }
   }
   h = phashMap[phashLetter1[h+1]];	// table[3] -> H, phashMap[H] --> 3
   if (h >= 0)	// "wow": table[22] --> 0, phashMap[0] --> -1, b --> ppSize
   {
      while((b = table[h]) == 0xff)
	 if (++h >= HASH_TABLE_LEN) { b = ppSize; break; }
      if (b == 0xfe) b = ppSize;	// table overflow check
   }
   *start = a; *end = b;
   return 1;
}

static ZKL_Object SmallDictionaryObject;

typedef struct
{
   Instance   instance;	 // iflag2 is 1 if no values (ie PolyGraph)
   unsigned int numKeys:16,	 // num key/value pairs in table
		min:16, max:16,	 // key length range
		howza:8;
   Byte       hash[HASH_TABLE_LEN];   // first character hash
   Instance  *table[0];	 // [2*len] : expands to hold pointers to keys,values
} SmallDictionary;	// ?? Windows32,  48 bytes Linux64

static void sdMarker(Instance *_self)
{
   SmallDictionary *self = (SmallDictionary *)_self;
   int	      n     = self->numKeys;
   Instance **table = self->table;
   if (!self->instance.iflag2) n *= 2;
   while (n--) instanceMark(*table++);
}

    // .create(keys [,values])
//!!! either keys are all strings (now) or all numbers
static Instance *SD_create(Instance *self,pArglist arglist,pVM vm)
{
   Instance *keys    = arglistGetBT(arglist,0,ListType,"keys",vm);
   Instance *values  = arglistTryToGetBT(arglist,1,ListType,"values",vm);
   Instance *i;
   int       numKeys = listLen(keys,vm), min = 999999L, max = 0, n;
   SmallDictionary *sd;

   if (numKeys > 255) vmThrow(vm,E_ASSERTION_ERROR,"Too many keys");
   if (values)
   {
      n = listLen(values,vm);
      if (n == 0) values = 0;	// no values
      else if (n != numKeys) vmThrow(vm,E_ASSERTION_ERROR,"Key/Value PAIRS");
   }

   verifyList("keys",keys,StringType,vm);
   n = numKeys; if (values) n *= 2;
   sd  = (SmallDictionary *)instanceAllocate(
           sizeof(SmallDictionary) + n*sizeof(Instance *),
	   &SmallDictionaryObject,1,vm);
   for (n = 0; (i = listGet(keys,n)); n++)
   {
      int len = strlen(ZKL_STRING(i));
      if (len > max) max = len;
      if (len < min) min = len;
      sd->table[n] = i; 
      if (values) sd->table[n+numKeys] = listGet(values,n);
   }
   sd->numKeys = numKeys;
   sd->min = min; sd->max = max;
   sd->instance.iflag2 = (values == 0);

   {
      Byte *hashTable = sd->hash;
      unsigned char  c = 0;
      int   n, nc = 0;

      memset(hashTable,0xff,HASH_TABLE_LEN);
      for (n = 0; n < numKeys; n++)
      {
	 unsigned char *k = (unsigned char *)ZKL_STRING(sd->table[n]), *ptr;
	 if (*k>c && (ptr=(unsigned char*)strchr((char*)&phashLetter1[nc],*k)))
	 {
	    c = phashLetter1[nc];	// previous matched character
	    nc = ptr - phashLetter1;	// current  matched character
	    hashTable[nc++] = (n < 0xff) ? n : 0xfe;
	    if (nc >= HASH_TABLE_LEN) break;
	 }
      }
   }
   return addToCollectables((Instance *)sd,I_OWNED,vm);
}

    // --> 0|value
static Instance *sdFind(SmallDictionary *self,pArglist arglist,pVM vm)
{
   char *key;
   int   a,b,c, num = self->numKeys;
   unsigned   n;
   Instance **keys      = self->table;
   Byte	     *hashTable = self->hash;
   Instance  *pk	= arglistGet(arglist,0,"SD get",vm);

   if (!num) return 0;		// empty

   if (TYPEO(pk) != StringType) return 0;
   key = ZKL_STRING(pk);

   n = strlen(key);
   if (n < self->min || n > self->max) return 0;

   if (!phashFcn((unsigned char *)key,hashTable,num,&a,&c)) return 0;
   b = (a+c)/2;

   while (a < c)		// binary search
   {
      int s = strcmp(key,ZKL_STRING(keys[b]));
      if (s == 0)
      {
	 if (self->instance.iflag2) return BoolTrue;	// PolyGraph, no values
      	 return keys[num+b];
      }

//      if ((c - a) < 2) break;
      if (s > 0) a = b + 1;
      else       c = b;
      b = (a + c)/2;
   }
   return 0;
}

    // SD.toBool() -->Bool
static Instance *SD_toBool(SmallDictionary *self,pArglist arglist,pVM vm)
   { return self->numKeys ? BoolTrue : BoolFalse; }

    // SD.len() -->Int
static Instance *SD_len(SmallDictionary *self,pArglist arglist,pVM vm)
   { return intCreate(self->numKeys,vm); }

    // SD.holds(key) -->Bool
static Instance *SD_holds(SmallDictionary *self,pArglist arglist,pVM vm)
   { return sdFind(self,arglist,vm) ? BoolTrue : BoolFalse; }

    // SD.find(key,[default]) -->value|default|Void
    // D(1,Void).find(1) -->Void
static Instance *SD_find(SmallDictionary *self,pArglist arglist,pVM vm)
{
   Instance *value = sdFind(self,arglist,vm);
   if (!value)
   {
      Instance *defaultVal = arglistTryToGet(arglist,1);
      return defaultVal ? defaultVal : Void;
   }
   return value;
}

    // SD[key] -->value
static Instance *SD_sGet(SmallDictionary *self,pArglist arglist,pVM vm)
{
   Instance *value = sdFind(self,arglist,vm);
   if (!value)
   {
      char buf[100], *key;
      Instance  *pkey;

      if (self->instance.iflag2) return BoolFalse;	// PolyGraph

      pkey = arglistGet(arglist,0,"[]",vm);
      if (TYPEO(pkey) == StringType) key = ZKL_STRING(pkey);
      else key = iname(pkey);
      sprintf(buf,"Dictionary key \"%.50s\" not found",key);
      vmThrow(vm,E_NOT_FOUND,buf);
   }
   return value;
}

#if 0
    // SD.nthPair(n) -->(key,value)
static Instance *SD_nthPair(SmallDictionary *self,pArglist arglist,pVM vm)
{
   int n = self->numKeys;
   int z = (int)arglistGetInt(arglist,0,0,vm);
   if (z >= 0 && z < n)
   {
      if (self->instance.iflag2)
	 return tupleCreateX(vm,self->table[z],BoolTrue,ZNIL);
      return tupleCreateX(vm,self->table[z],self->table[n+z],ZNIL);
   }
   vmThrowTheEnd(vm);
   return Void;
}
#endif

    // howza([getter mode]) --> self|int
Instance *SD_howza(Instance *self,pArglist arglist,pVM vm)
{
   int64_t i64;
   if (arglistTryToGetInt(arglist,0,&i64,0,vm))
   {
      ((SmallDictionary *)self)->howza = (unsigned)i64;
      return self;
   }
   return intCreate(((SmallDictionary *)self)->howza,vm);
}

static Instance *_sdgetter(
	Instance *pSelf,size_t idx,void *howza,size_t _,pVM vm)
{
   SmallDictionary *self = (SmallDictionary *)pSelf;
   size_t	    N = self->numKeys;
   Instance	   *v = BoolTrue;

   if (idx >= N) return 0;
   if (!self->instance.iflag2)	// PolyGraph: all values are True
   	v = self->table[N + idx];
   switch((size_t)howza)
   {
      case 8: return self->table[idx];
      case 9: return v;
   }
   return tupleCreateX(vm,self->table[idx],v,ZNIL);
}

#define SD_HOWZA (void *)(size_t)(((SmallDictionary *)self)->howza)

    // SD.reduce
Instance *SD_reduce(Instance *self,pArglist arglist,pVM vm)
   { return zreduce(self,_sdgetter,SD_HOWZA,arglist,0,vm); }

    // SD.pump
Instance *SD_pump(Instance *self,pArglist arglist,pVM vm)
   { return pump(self,0x00,_sdgetter,SD_HOWZA,arglist,0,vm); }

    // SD.filter
Instance *SD_filter(Instance *self,pArglist arglist,pVM vm)
   { return zfilter(self,0x0,_sdgetter,SD_HOWZA,arglist,0,vm); }
Instance *SD_filter1(Instance *self,pArglist arglist,pVM vm)
   { return zfilter1(self,0,_sdgetter,SD_HOWZA,arglist,0,vm); }

    // SD.walker() --> T(k,v)
static Instance *SD_walker(Instance *self,pArglist arglist,pVM vm)
{
   int64_t i64 = (int)SD_HOWZA;
   arglistTryToGetInt(arglist,0,&i64,0,vm);
   return walkerCreate(self,_sdgetter,(void *)i64,0,vm);
}


static const MethodTable sdMethods[] = 
{
   "toBool",		(pMethod)SD_toBool,
   "toDictionary",	(pMethod)Object_noop,
   "toList",		(pMethod)Dictionary_toList,
//   "toString",		(pMethod)Dictionary_toString,
//   "create",		(pMethod)Dictionary_create,
   "copy",		Object_noop,
   "len",		(pMethod)SD_len,
   "holds",		(pMethod)SD_holds,
   "__sGet",		(pMethod)SD_sGet,
   "get",		(pMethod)SD_sGet,
   "find",		(pMethod)SD_find,
   "walker",		(pMethod)SD_walker,
   "makeReadOnly",	Object_noop,
   "reduce",		SD_reduce,
   "pump",		SD_pump,
   "filter",		SD_filter,
   "filter1",		SD_filter1,
   "howza",		SD_howza,
   0,			0,
};

////////////////////////////
//////////////////////////////////////////////// Small Dict properties
////////////////////////////

extern Instance *partitionTuple(ZKL_Tuple *,Instance **start,size_t num,pVM);

    // SD.keys --> L(key, key ...)
    // A common case: d.keys.sort()
static Instance *SD_keys(SmallDictionary *self,pVM vm)
   { return partitionTuple((ZKL_Tuple *)self,self->table,self->numKeys,vm); }

    // SD.values --> L(value,value ...)
static Instance *SD_values(SmallDictionary *self,pVM vm)
{
   int n = self->numKeys;
   if (self->instance.iflag2)	// PolyGraph: values are all True
   {
      Instance *t = tupleCreate(n,I_OWNED,vm);
      while(n--) tupleAppend(t,BoolTrue);
      return t;
   }
   return partitionTuple((ZKL_Tuple *)self,self->table+n,n,vm);
}

    // SD.isReadOnly -->True
static Instance *SD_isReadOnly(Instance *self,pVM vm)
   { return BoolTrue; }

    // SD.size -->L(instance size)
static Instance *SD_size(SmallDictionary *self,pVM vm)
{
   int    n = self->numKeys;
   size_t size;
   if (self->instance.iflag2) n *= 2;
   size = sizeof(SmallDictionary) + n * sizeof(Instance *);
   return intCreate(size,vm);
}

static const PropertyTable sdProperties[] = 
{
   "keys",		(pProperty)SD_keys,
   "values",		(pProperty)SD_values,
   "isReadOnly",	(pProperty)SD_isReadOnly,
   "size",		(pProperty)SD_size,
   0,			0
};



/* ******************************************************************** */
/* ******************************************************************** */

//static pMethod in_dictionary_methods(Instance *ignore, register char *str);
//static pMethod in_SD_methods(Instance *ignore, register char *str);

    /* Construct Dictionary in two parts because the Vault uses it and
     * Dictionary uses the Vault
     */

void dictionaryConstructPartI(void)
{
   constructObject(&DictionaryObject,DictionaryType, methods,properties,0, NoVM);
   DictionaryObject.freeMe	 = dictionaryFree;
   DictionaryObject.magicMarker  = dMarker;
//   DictionaryObject.methodSearch = in_dictionary_methods;
   DictionaryObject.isize	 = sizeof(Dictionary);
   DictionaryObject.threadSafe   = 1;
   DictionaryObject.isBInstance  = 1;
   DictionaryObject.createReturnsSelf = 1;

	// shared with Int/Float
   ibucketReserve(&DictionaryObject,5432,&dcBuckets,0,NoVM);

   constructObject(&SmallDictionaryObject,DictionaryType,
		   sdMethods,sdProperties,0, NoVM);
   SmallDictionaryObject.name	      = "SmallDictionary";
   SmallDictionaryObject.magicMarker  = sdMarker;
//   SmallDictionaryObject.methodSearch = in_SD_methods;
   SmallDictionaryObject.isize	      = sizeof(SmallDictionary);
   SmallDictionaryObject.threadSafe   = 1;
   SmallDictionaryObject.createReturnsSelf = 1;

   buildHashMap();
}

    // Create a place holder in the TheVault so Dictionary can be found.
void dictionaryConstructPartII(void)
{
   Dictionary *D = (Dictionary *)
	     ibucketAllocate(&dcBuckets,&DictionaryObject,I_INVISIBLE,1,NoVM);
   D->hashTable = 0;	// a truely empty Dictionary and small
   writeLockInit(&D->lock);

   vaultAdd("",(Instance *)D,NoVM);
}




////////////////////////////////////////////////////////////////////
// zkl extractTable < dictionary.c | gperf | zkl gperf -i dictionary
// zkl extractTable -n sdMethods < dictionary.c | gperf | zkl gperf -i SD




////////////////////////////////////////////////////////////////////
// zkl extractTable -n sdMethods < dictionary.c | gperf | zkl gperf -i SD

