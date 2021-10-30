/* list.c : list objects
 * 
 * Copyright (c) 2006,2007,2008-12,2013 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#define THE_DEFAULT	1  // 1: Default to thread safe for user lists

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>

#define __NOT_A_DLL
#define __LIST_INTERNALS
#define __GC_INTERNALS		// mark macros
#define __PC_INTERNALS
#define __STRING_INTERNALS

#include "zklObject.h"
#include "zklClass.h"
#include "zklData.h"
#include "zklDictionary.h"
#include "zklList.h"
#include "zklFcn.h"
#include "zklMethod.h"
#include "zklNumber.h"
#include "zklString.h"
#include "zklUtil.h"

Instance *partitionTuple(ZKL_Tuple *,Instance **start,size_t num,pVM);

static void	 _justShrinkIt(TSList *self, size_t offet, size_t num);
static Instance *Tuple_copy(Instance *,pArglist,pVM);
static Instance *Tuple_flatten(Instance *,pArglist,pVM);
static Instance *tupleAdd(Instance *,Instance *,size_t,Instance **,pVM);
static Instance *initializeMList(Byte *);
static Instance *Tuple_set(Instance *,pArglist,pVM);
static Instance *Tuple_reverse(Instance *,pArglist, pVM);

ZKL_Object	  TSListObject;
static ZKL_Object ListObject;
static ZKL_Object MListObject, TupleObject, CuckooTupleObject, ROObject;

Instance *VList, *VTSList;	// the mothers of all Lists
int roListID, tsListID;

Instance *emptyList;	// aka NoArglist, Vault.T, Tuple
Instance *emptyTuple;	// Ditto. Probably points to the same thing

/* In a method, arglists are Tuples and are mungable as long as YOU ensure
 * that the orginal contents will be marked.  You CAN NOT add items, you can
 * only change items. Please don't. Methods are special (as far as GC is
 * concerned).
 */

/* OK, this is a bit confusing: There are several list [sub]types:
 *   List: A list. NOT thread safe.
 *   TSList: A locking list. Read/write, thread safe. AKA Thread.List
 *   ROList#: A List or TSList that acts like a Tuple. This is for C code
 *      that wants a Tuple but needs to build it and doesn't know the size.
 *   Tuple:  A fixed, unchangable, read only list (like String).  Thread
 *      safe.  Just for extra confusion, this type is known to zkl code as
 *      TheVault.ROList and TheVault.T
 *   CuckooTuple:  A Tuple that fits in space allocated for another type.
 *     Used for efficiency.
 *   MemList: A Tuple that sits on the C stack. A short, temporary list.
 *   Tuple Partition: A Tuple that points into a Tuple. A cheap way to slice
 *     up a Tuple. Overlays/loads ROList#.
 */

    // A Tuple Partition (or slice), for things like T(1,2,3)[1,2] --> TP(2,3)
    // This fits inside a TSList struct but has the same 
    //   Methods and Properties as Tuple
typedef struct	// A proper superset of List, stored in a TSList
{
   BInstance  instance;	// iflag will be set
   ITable     table;
   ZKL_Tuple *src;	// where the bits really are, for marking
} TP;	// 20 bytes 32 bit OS, 32 on 64 Linux (32 for TSList)

	// Compling the parser creates 648,249 Lists
IBucketHeader tsListBuckets;	// I'll share this with String, [RO]List, Tuple


#define ITABLE(list)	( LISTV(list)->table )

#define IS_LIST(i)	( TYPEO(i) == ListType )    // and TP, ROList, TSList
#define IS_TUPLE(i)	( TYPEO(i) == TupleType )
#define IS_TP(i)	( ((Instance *)i)->iflag )  // Tuple Partition

   // tiny bit quicker if I know i isn't PtrInt
#define IS_LIST1(i)	( TYPEO1(i) == ListType )
#define IS_TUPLE1(i)	( TYPEO1(i) == TupleType )

    // Is this a thread safe list that needs locking?
    // Tuples, TP are thread safe but don't lock
#define IS_LIST_THREADSAFE(list)  ( ((Instance *)list)->objID == tsListID )
#define LOCK_IT(list)		  IS_LIST_THREADSAFE(list)


/* ******************************************************************** */
/* ************************* Instance Tables ************************** */
/* ******************************************************************** */

    /* How many (Instance *)s (N) can I allocate before a size_t overflows?
     * sizeof(Instance *) == sizeof(size_t) = B (as in 2,4,8, etc)
     * Formula for max N: N * sizeof(Instance *) = 2 ^ sizeof(size_t)
     * --> N*B = 2^(8B) --> n = 2^(8B)/B --> 2^(8B) / 2^(B/2) --> 7.5 * B bits
     * --> 2^15, 2^30, 2^60 etc
     * This means that MAX_Is * sizeof(Instance *) doesn't overflow size_t
     * Boogers on machines with odd word sizes
     *  -->1,073,741,823 (30 bits). Well duh, Instance is 4 bytes
     *  -->1,152,921,504,606,846,975 (60 bits)
     */
#define MAX_Is	( ((size_t)1 << ( 7*sizeof(size_t) + sizeof(size_t)/2 )) - 1 )

    /* OK, how big is the word size I'm using for list length?
     * sizeof(LSize_t) <= sizeof(size_t) & sizeof(Instance *) > 1
     *   & maxListLength <= MAX_Is,
     *   so 2 * maxListLength doesn't overflow a size_t
     * Set all bits of a LSize_t
     * But that is too big, it can eat 16 GiB memory pretty quick
     *   I use one million length lists more than once
     *   Its not the list length, it is the objects in the list. So a list
     *   of PtrInts is "who cares", a list of Floats damn!
     */
//#define MAX_LIST_LENGTH	( (LSize_t)~1 )	// 4,294,967,294 (-2==2^32-2)
#define MAX_LIST_LENGTH		( 2<<22 )	//     4,194,304

    // Will do some calculations in listConstruct() to set true max
    // Drops to MAX_Is
static size_t maxListLength = MAX_LIST_LENGTH;


static void itInit(ITable *table)
{
   table->maxItems = table->numItems = 0;
   table->table    = 0;		// for debugging purposes
}

static void itFree(ITable *table)
{
   if (table->maxItems) ZFREE(table->table);
   table->maxItems = table->numItems = 0;
   table->table    = 0;		// for debugging purposes
   // ready to add items again
}

    // Expand a list by n slots
    // GCs, throws
    // Since this code is only used in this file, I'm going to assume that 
    // step is a reasonable number. Please don't prove me wrong.
static void itXpand(TSList *list, 	// both List & TSList
	size_t n, size_t initialSize, size_t step, int locked, pVM vm)
{
   size_t  maxItems, numItems;
   ITable *table = &list->table;
   size_t  slots, size;
   void	  *ptr;

   maxItems = table->maxItems;
   numItems = table->numItems + n;
   if (n > maxListLength || 
       numItems > maxListLength || numItems < table->numItems)  // overflow
      vmThrow(vm,E_VALUE_ERROR,"Zounds, that would be a humongous list");

	// check to see if already have enough room for n more items
   if (numItems <= maxItems)	// and 0,0 --> no allocation (common case)
   {
      table->numItems = (LSize_t)numItems;
      return;
   }

   if (maxItems == 0)		// table not allocated yet, numItems > 0
   {
      slots = initialSize;
      if (slots < numItems)		// initial size ain't big enough
	 slots = numItems + step;
      size = slots * sizeof(Instance *); // won't overflow if initialSize OK
      if (slots > maxListLength || initialSize > maxListLength || size == 0)
      {
      overflow:
	 if (locked) writeLockRelease(&list->lock,WL_WRITER);
	 vmThrow(vm,E_ASSERTION_ERROR,"Oh pooh, attempted list overflow");
      }
      ptr = ZMALLOC(size);
      if (!ptr)		// ahh crap, not enough memory
      {
	 collectGarbage(1,vm);		// wait
	 ptr = ZMALLOC(size);		// try again
	 if (!ptr)
	 {
	    itInit(table);
	 nomem:
	    if (locked) writeLockRelease(&list->lock,WL_WRITER);
	    vmThrow(vm,E_OUT_OF_MEMORY,0);
	 }
      }
   }
   else			// table full, make bigger
   {	// numItems, maxItems & step in range
      slots = (numItems - maxItems + step - 1)/step;	// <= maxListLength
      slots = slots*step + maxItems;	// < num + step - 1 < 2*maxListLength
      size  = slots * sizeof(Instance *);	// won't overflow
      if (slots > maxListLength || slots == 0) goto overflow;

      ptr = ZREALLOC(table->table,size);
      if (!ptr)		// in ANSI C, realloc may fail but table is still OK
      {
	 collectGarbage(1,vm);			// wait
	 ptr = ZREALLOC(table->table,size);	// try again
	 if (!ptr) goto nomem;
      }
   }

   table->maxItems = (LSize_t)slots;
   table->numItems = (LSize_t)numItems;
   table->table    = ptr;
}

    // --> bytes allocated
static size_t itSize(ITable *table)
   { return sizeof(ITable) + table->maxItems * sizeof(Instance *); }

/////////////////////////////////////////////////////////////////////////

#if 0
static void readUnlocker(Instance *self, Instance *e)
   { writeLockRelease(LIST_LOCK(self),WL_READER); }
#endif
static void writeUnlocker(Instance *self, Instance *e)
   { writeLockRelease(LIST_LOCK(self),WL_WRITER); }

static void readUnlockerF(Fence *fence, Instance *e)
   { writeLockRelease(LIST_LOCK(fence->i),WL_READER); }
static void writeUnlockerF(Fence *fence, Instance *e)
   { writeLockRelease(LIST_LOCK(fence->i),WL_WRITER); }

static TSList *createTSList(int itype,pVM vm)	// doesn't throw
{
   TSList *list = 
	(TSList *)ibucketAllocate(&tsListBuckets,&TSListObject,itype,0,vm);
   if (!list) return 0;
   itInit(&list->table);
   writeLockInit(&list->lock);
   return list;
}

static List *createList(int itype,pVM vm)	// doesn't throw
{
   List *list = (List *)ibucketAllocate(&tsListBuckets,&ListObject,itype,0,vm);
   if (!list) return 0;
   itInit(&list->table);
   return list;
}

static int listFree(Instance *self)
{
   if (!IS_TP(self)) itFree(&((TSList *)self)->table);
   return 1; 
}

#define STAY_AT_HOME_MARKER	1	// tiny, tiny numbers

#if STAY_AT_HOME_MARKER
    // Try to avoid function calls & recursion, all in the name of efficiency
static void _markListContents(Instance *list)
{
   struct { Instance *list; int t; } lists[260];
   int        z = 1, lockIt = 0;
   LSize_t    n = 0;
   Instance **table = 0;

   lists[0].list = list; lists[0].t = TYPEO1(list);  //!!!??? objID instead?

   // self has been marked, contents haven't
//   GC_MARK(i);
   // first pass is a noop to prime next pass
   while(1)
   {
      while(n--)	// mark list contents, stash contained lists
      {
	 int	   t,gt;
	 Instance *i = *table++;

	 if (!i) continue;
	 #if USE_POINTER_INTS
	    if (IS_PtrInt(i)) continue;   // which could also be garbage
	 #endif
	 if (GC_IS_MARKED(i)) continue;

	 gt = GC_TYPE(i);
	 if (gt == I_UNTOUCHABLE || gt == I_INVISIBLE) continue;
	 t = TYPEO1(i);
	 if (z != 255 && (t == ListType || t == TupleType))
	 {
	    lists[z].list = i; lists[z].t = t; z++;
	    if (gt != I_2SPECIAL)  // just mark MList contents, not on gc list
	       GC_MARK(i);	   // well, I *am* going to mark it
	 }
	 else	// (i = table[n]) not a list or overflow
	 {
#if 0
	    Instance *d;
	    	// if var is evaluated Deferred, change it to real value
//if tuple or tslist
	    if (IS_DEFERRED(i) && (d = deferredIsEvaled(i)))
	    {
	       CAP_SET(&table[-1],d); i = d;		// i isn't PtrInt
	       gt = GC_TYPE(i);
	       if (GC_IS_MARKED(i) || gt == I_UNTOUCHABLE || t == I_INVISIBLE)
		  continue;
	    }
#endif
	    if (gt != I_OWNED || MAGIC_MARKER(i)) instanceMark(i);
	    else GC_MARK(i);	// owned & no marker --> !container, !special
	 }
      } // while

      if (lockIt) writeLockRelease(LIST_LOCK(list),WL_READER);
      if (0 == z--) break;
      list = lists[z].list; lockIt = 0;
      if (lists[z].t == TupleType)	// and MLists
	 { table = TUPLE_TABLE(list); n = TUPLE_LEN(list); }
      else	// List, TSList, TP
      {
	 if (IS_TP(list))	// tuple partition
	 {
	    if (list->iflag2)	// container, not Tuple
	    {
	       n = 1;
	       table = (Instance **)&((TP *)list)->src; // container
	    }
	    else
	    {
	       list = (Instance *)((TP *)list)->src; // Tuple

	       if (GC_IS_MARKED(list)) n = 0;	// container already marked
	       else
	       {
		  GC_MARK(list);
		  table = TUPLE_TABLE(list); n = TUPLE_LEN(list);
	       }
	    }
	 }
	 else
	 {
	    if (LOCK_IT(list))
	    {
	       lockIt = 1;
	       writeLockAcquire(LIST_LOCK(list),WL_READER,NoVM);
	    }
	    table = LIST_TABLE(list); n = LIST_LEN(list);
	 }
      }
   } // while
}
#endif

#if !STAY_AT_HOME_MARKER
static void listMarker(List *self)
{
   LSize_t    n     = LIST_LEN(self);
   Instance **table = LIST_TABLE(self);
   while (n--) instanceMark(*table++);
}

    /* Doesn't deadlock on recursive lists because instanceMark() marks self
     *    before calling listMarker
     * Lots of contension on the WriteLock, eg with arglistPop()
     */
static void tslistMarker(TSList *self)
{
   writeLockAcquire(LIST_LOCK(self),WL_READER,0);
   {
      LSize_t	 n     = LIST_LEN(self);
      Instance **table = LIST_TABLE(self);
      while (n--) instanceMark(*table++);
   }
   writeLockRelease(LIST_LOCK(self),WL_READER);
}
#endif

#if 0
static void tsListMarker2(TSList *self,pVM vm)
static void listMarker2(TSList *self,pVM vm)
{
   pVM creator = idToVM(self->creator);
   if (creator == vm) { roListMaker(self); return; }
   if (vm) return;	// let creating VM mark me
   makeReadOnly(self);       // creator is gone, convert to read only
   roListMaker(self);
}
#endif

    // Allocate enough space for initialSize items. 
    // Length is set to zero.
    // If initialSize is 0, table isn't allocated.
    /* Flags:
     *   1 : Create threadSafe
     *   2 : RO List, but writable from C, eg you want a Tuple but need to
     *         build the list and don't want to make a copy.
     */
Instance *listCreate(size_t size, int flags, int itype, pVM vm)
{
   Instance *list;
   List	     pooh;

   itInit(&pooh.table);
   itXpand(LISTV(&pooh), size,size,0, 0,vm);

   list = (flags & 1) ? (Instance *)createTSList(itype,vm) :
		        (Instance *)createList(itype,vm);
   if (!list)
   {
      itFree(&pooh.table);
      vmThrow(vm,E_OUT_OF_MEMORY,0);
   }

   pooh.table.numItems = 0;
   ITABLE(list) = pooh.table;	// struct copy, doesn't touch lock
   if (flags & 2) list->objID = roListID;
   return containerIsCooked(&tsListBuckets,list,itype);
}

    // Called by the VM for opSquirt.
    // Copy from src list (list) to dst list (stack)
    // dst has enough room
void listSplat(Instance *list,size_t offset,size_t n,Instance **stack,pVM vm)
{
   Instance **table;
   unsigned   len;
   table = listTable(list,&len);
   if (offset + n > len)
      vmThrow(vm,E_INDEX_ERROR,"List assignment: Not enough data");
   memcpy(stack,table+offset,n*sizeof(Instance *));
}

   /* OK for result to be one of the added items but ALL items must be
    *   protected or they might be GC'd if GC is happening as this is called.
    * listCreateX(vm,&result,x,...,ZNIL)
    * New List is owned
    */
Instance *listCreateX(pVM vm,Instance *x,...)
{
   va_list   ap;		// argument list pointer
   int	     n;
   Instance *list;

   va_start(ap,x);
      for (n = 1; va_arg(ap, Instance *); ) n++;	// don't count ZNIL
   va_end(ap);			// because listCreate() can throw
   list = listCreate(n,THE_DEFAULT,I_OWNED,vm);

   va_start(ap,x);
      for (n = 0; x; x = va_arg(ap, Instance *)) LIST_TABLE(list)[n++] = x;
   va_end(ap);

   LIST_LEN(list) = n;

   return list;
}

    // New list is owned
    // -->Tuple
Instance *createTupleOfStrings(pVM vm,char *text,...)
{
   va_list   ap;		// argument list pointer
   int	     n;
   Instance *list,*s;
   Fence     fence;

   va_start(ap,text);
      for (n = 1; va_arg(ap, Instance *); ) n++;
   va_end(ap);			// because tupleCreate() can throw

   vmSetFence(vm,&fence,0,list = tupleCreate(n,I_OWNED,vm));
   va_start(ap,text);
      for (n = 0; text; text = va_arg(ap, char *))
      {
	 s = kStringCreate(text,0,I_OWNED,vm);// !!! if this throws, va_end not called
	 tupleAppend(list,s);		// this won't throw
      }
   va_end(ap);
   vmRemoveFence(&fence,0);

   return list;
}

    // You need to lock this if you care
    // !!NOT thread safe, NOT GC safe
    // !!Gap probably has garbage in it
static int listOpenGap(TSList *self,size_t idx, size_t numNewItems,pVM vm)
{
   size_t sizeItemsToMove = (LIST_LEN(self) - idx) * sizeof(Instance *);

   itXpand(self,numNewItems,25,50,0,vm);	// updates length
   memmove(&LIST_TABLE(self)[idx + numNewItems],
	   &LIST_TABLE(self)[idx],sizeItemsToMove);
   return 1;
}

    // Is list a List/Tuple of type? (ListType == List | Tuple)
    // Not thread safe
void verifyList(char *name, Instance *list, int type, pVM vm)
{
   char	  buf[100];
   size_t i,n;
   int	  two = (type == ListType);

   if (!IS_LIST(list) && !IS_TUPLE(list))
   {
      sprintf(buf,"%s expected list, not %s",name,iname(list));
      vmThrow(vm,E_TYPE_ERROR,buf);
   }
   n = listLen(list,vm);
   for (i = 0; i < n; i++)
   {
      Instance *s = listGet(list,i);
      int	t = TYPEO(s);

      if (two) { if (t != ListType && t != TupleType) goto badType; }
      else if (t != type)
      {
      badType:
	 sprintf(buf,"%s expected list of %s, contains a %s",
	         name,typeToName(type),typeToName(t));
	 vmThrow(vm,E_TYPE_ERROR,buf);
      }
   }
}

    // NOT for tuples
void listClear(Instance *self, pVM vm)
{
   if (self == emptyList) return;
   if (self)
   {
      if (!LOCK_IT(self)) LIST_LEN(self) = 0;
      else
      {
	 writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
	    LIST_LEN(self) = 0;
	 writeLockRelease(LIST_LOCK(self),WL_WRITER);
      }
   }
}


///////////////////// Slicing and Dicing ////////////////////////////

    /* Returns:
     *   n where 0 <= n <[=] len
     * Throws:
     *   IndexError
     */
static size_t constrainXPhat(Offset_t n, size_t len, int maxXOK,
   int unlockOnError, void (*unlocker)(Instance *,Instance *), 
   Instance *self, char *msg,pVM vm)
{
   size_t idx;
   if (!checkOffset(n,len,maxXOK,&idx))
   {
      if (unlockOnError) unlocker(self,0);
      offsetIndexError(msg,n,len + (maxXOK!=0),vm);
   }
   return idx;
}

/* ******************************************************************** */
/* *************************** Methods ******************************** */
/* ******************************************************************** */

    // Thread safe
size_t listLen(Instance *self,pVM vm)
{
   size_t len = 0;

   if (!self)		  return 0;
   if (self == emptyList) return 0;	// redundant
   if (IS_TUPLE1(self))   return TUPLE_LEN(self);
   if (!LOCK_IT(self))	  return LIST_LEN(self);

   writeLockAcquire(LIST_LOCK(self),WL_READER,vm);
      len = LIST_LEN(self);
   writeLockRelease(LIST_LOCK(self),WL_READER);

   return len;
}

    // .len(), thread safe
static Instance *List_len(Instance *self,pArglist arglist,pVM vm)
   { return intCreate(listLen(self,vm),vm); }

    // [TS]List.create([contents]), thread safe (assuming arglist is)
static Instance *List_create(Instance *self,pArglist arglist,pVM vm)
{
   LSize_t   n    = TUPLE_LEN(arglist);
   Instance *list = listCreate(n,IS_LIST_THREADSAFE(self),I_OWNED,vm);
   memcpy(LIST_TABLE(list),TUPLE_TABLE(arglist),n*sizeof(Instance *));
   LIST_LEN(list) = n;

   return list;
}

    // [TS]List.createLong(size [,fillFill],run=False), thread safe -->[TS]List
    // List.createLong(5,Node,True), List.createLong(5,f,True)
    // Same as List().pad(sz,fill)
Instance *List_createBig(Instance *self,pArglist arglist,pVM vm)
{
   int64_t   sz   = arglistGetInt(arglist,0,0,vm);
   Instance *fill = arglistTryToGet(arglist,1), *r;
   if (sz < 0) sz = 0;
   r = listCreate((size_t)sz,IS_LIST_THREADSAFE(self),I_OWNED,vm);
   if(fill)
   {
      if(arglistTryToGetBool(arglist,2,0,0,vm))  // createLong(n,obj,True)
      {	 // -->apply(fill())
	 // List.createLong(4000000,fcn{ 1.2 },True) hits GC
	 Fence  fence;
	 size_t n;

	 vmSetFence(vm,&fence,0,r);  // can't lock r or dead lock in GC
	    for(n=0; n<sz; n++)
	    {
	       LIST_TABLE(r)[n] = objectRun(fill,NoArglist,0,vm);
	       LIST_LEN(r)      = n+1;  // so GC will mark all new Instances
	    }
	 vmRemoveFence(&fence,0);
      }
      else
      {
	 LIST_LEN(r) = (LSize_t)sz;	// set size BEFORE sz goes to zero
	 for(; sz--; ) LIST_TABLE(r)[sz] = fill;
      }
   }
   return r;
}

    // Thread safe -->owned List
Instance *listCopy(Instance *self, pVM vm)
{
   Fence     F, *fence = 0;
   Instance *newList;
   LSize_t   n;

   if (IS_TUPLE1(self)) return Tuple_copy(self,NoArglist,vm);

   if (LOCK_IT(self))	// LOCK_IT(TP) == 0
   {
      fence = &F;
      writeLockAcquire(LIST_LOCK(self),WL_READER,vm);
      vmSetFence(vm,fence,readUnlockerF,(Instance *)self);
   }
      n = LIST_LEN(self);
      newList = listCreate(n,THE_DEFAULT,I_OWNED,vm);	// throws
      memcpy(LIST_TABLE(newList),LIST_TABLE(self), n * sizeof(Instance *));
      LIST_LEN(newList) = n;
   vmRemoveFence(fence,1);	// noop if fence == 0
   return newList;
}

    // .copy() -->new list, thread safe
    /* List.copy() -->TSList
     * Tuple.copy() --> make a writable copy
     * TP.copy()    --> make a writable copy
     * TSList.copy() -->TSList
     * *.copy(n) -->1(List), 2(TSList), 3(Tuple)
     * T.copy(3) -->self? Compiler makes copies to modify
     * *.copy(n,a,b,c,...) -->new list with arg appended
     */
static Instance *List_copy(Instance *self,pArglist arglist,pVM vm)
   { return listCopy(self,vm); }

static void _listToString( Instance *,size_t len,int depth,ZBText *,pVM);
static void _tupleToString(Instance *,size_t len,int depth,ZBText *,pVM);

static Instance *tupleToString(Instance *,size_t maxLen,int depth,pVM);

    // can GC
    // The recursive part of L|T.toString
static void _quote(Instance *x, size_t maxLen, int depth, ZBText *buf, pVM vm)
{
   if (TYPEO(x) == StringType)
   {
      zbtextAppendN(buf,"\"",1,vm);
      zbtextAppend(buf,ZKL_STRING(x),vm);
      zbtextAppendN(buf,"\"",1,vm);
      return;
   }
   if (IS_LIST(x))
   {
      (depth > 0 && maxLen) ? 
         _listToString(x,maxLen,depth-1,buf,vm) :
	 zbtextAppend(buf,listLen(x,vm) ? "L(...)" : "L()",vm);
      return;
   }
   if (TYPEO(x) == TupleType)
   {
      (depth > 0 && maxLen) ? 
         _tupleToString(x,maxLen,depth-1,buf,vm) : 
	 zbtextAppend(buf,TUPLE_LEN(x) ? "L(...)" : "L()",vm);
      return;
   }
   zbtextAppendI(buf,x,vm);
}

    // Gotta be careful of recusive lists: a = L(); a.append(a)
    // NOT thread safe (because self may not be locked)
static void
_listToString(Instance *self, size_t maxLen,int depth, ZBText *buf,pVM vm)
{
   size_t i, len = LIST_LEN(self);

   if (len == 0) { zbtextAppendN(buf,"L()",3,vm); return; }

   zbtextAppendN(buf,"L(",2,vm);
   _quote(LIST_TABLE(self)[0],maxLen,depth,buf,vm);

   for (i = 1; i < len; i++)
   {
      if (i >= maxLen)
      {
	 zbtextAppendN(buf,",...",4,vm);
	 break;
      }
      zbtextAppendN(buf,",",1,vm);
      _quote(LIST_TABLE(self)[i],maxLen,depth,buf,vm);
   }
   zbtextAppendN(buf,")",1,vm);
}

static Instance *
listToString(Instance *self, size_t maxLen,int depth, Fence *f,pVM vm)
{
   Fence  fence;
   ZBText buf;

   vmSetFence(vm,&fence,0,zbtextInit(&buf,vm));
      _listToString(self,maxLen,depth,&buf,vm);
      zbtextClose(&buf,vm);
   vmRemoveFence(&fence,0);
   return buf.string;
}

    // .toString([maxLen(N|*)=20,depth=2), not quite thread safe
    // OK for both maxLen & depth
Instance *List_toString(Instance *self,pArglist arglist,pVM vm)
{
   Instance *result;
   size_t    maxLen, depth;
   Fence     F, *fence = 0;

// probably should to all contained strings or "L()" if not owned
   if (!arglistGetSize(arglist,0,maxListLength,&maxLen,vm)) maxLen = 20;
   if (!arglistGetSize(arglist,1,100,&depth,vm))	    depth  =  2;

   if (IS_TUPLE1(self))
   {
      if (maxLen == 0)
	   result = kStringCreate(TUPLE_LEN(self) ? "L(...)" : "L()",0,I_OWNED,vm);
      else result = tupleToString(self,maxLen,(int)depth,vm);
      return result;
   }

   if (LOCK_IT(self))
   {
      fence = &F;
      writeLockAcquire(LIST_LOCK(self),WL_READER,vm);
      vmSetFence(vm,fence,readUnlockerF,self);
   }
//!!! not thread safe: L1(L2()), L2 not locked
      if (maxLen == 0)
	   result = kStringCreate(LIST_LEN(self) ? "L(...)" : "L()",0,I_OWNED,vm);
      else result = listToString(self,maxLen,(int)depth,fence,vm);
   vmRemoveFence(fence,1);
   return result;
}

    // .toBool(), thread safe but thread funky
static Instance *List_toBool(Instance *self,pArglist arglist,pVM vm)
{
   if (IS_TUPLE1(self)) { if (TUPLE_LEN(self)) return BoolTrue; }
   else if (LIST_LEN(self)) return BoolTrue;
   return BoolFalse;
}

    // .toData([mode=Data])
    // Thread safe, circular == dead lock or infinite recursion
static Instance *_toData(Instance *self,Instance *data,pVM vm)
{
   Fence     fence;
   Instance *i;
   size_t    n;

   fence.i1 = data;
   if (LOCK_IT(self))
   {
      writeLockAcquire(LIST_LOCK(self),WL_READER,vm);
      vmSetFence(vm,&fence,readUnlockerF,self);
   }
   else vmSetFence(vm,&fence,0,0);
      for (n = 0; (i = listGet(self,n)); n++)
      {
	 int t = TYPEO(i);
	 	// !!!??? can I check for deadlock? set stall flag
	 if (t == TupleType || t == ListType) _toData(i,data,vm);
	 else dataAppend(data,i,vm);
      }
   vmRemoveFence(&fence,1);
   return data;
}
static Instance *List_toData(Instance *self,pArglist arglist,pVM vm)
{
   Instance *data = dataCreate(0,I_OWNED,vm);
//   int64_t   i64;

//   if (arglistTryToGetInt(arglist,0,&i64,0,vm)) dataMode(data,(int)i64,vm);// mode
   if (arglistTryToGet(arglist,0)) Data_mode(data,arglist,vm);   // mode
      
   return _toData(self,data,vm);
}

    // .toDictionary()
    // L(L(key,value),L(k,v)....)
    // L(k,v, k,v, ....)
static Instance *List_toDictionary(Instance *self,pArglist arglist,pVM vm)
{
   Instance *d;
   Fence     F, *fence = 0;

   if (LOCK_IT(self))
   {
      fence = &F;
      writeLockAcquire(LIST_LOCK(self),WL_READER,vm);
      vmSetFence(vm,fence,readUnlockerF,self);
   }
      d = Dictionary_create(self,self,vm);
   vmRemoveFence(fence,1);	// noop if fence == 0
   return d;
}

    // List|Tuple.isType([type[,type ...]]), also .isInstanceOf
static Instance *List_isType(Instance *self,pArglist arglist,pVM vm)
{
   int	     n;
   Instance *i;
   for (n = 0; (i = listGet(arglist,n)); n++)
      switch(TYPEO(i))
      {
	 case ListType: case TupleType: return BoolTrue;
      }
   return BoolFalse;
}

    // NOT thread safe -->i | 0
    // List or Tuple ONLY!
Instance *listGet(Instance *self,unsigned n)
{
   if (IS_TUPLE1(self))	// Tuple, MList, CuckooTuple
   {
      if (TUPLE_LEN(self) <= n) return 0;
      return TUPLE_TABLE(self)[n];
   }

   // List, TSList, ROList
   if (LIST_LEN(self) <= n) return 0;
   return LIST_TABLE(self)[n];
}

static Instance *_listGetter(Instance *self,size_t idx,void *t,size_t _,pVM vm)
{
   if ((size_t)t == TupleType)
   {
      if (TUPLE_LEN(self) <= idx) return 0;
      return TUPLE_TABLE(self)[idx];
   }
   if (LIST_LEN(self) <= idx) return 0;
   return LIST_TABLE(self)[idx];
}

    // only for advanced users and subject to change/breakage
Instance **listTable(Instance *list,unsigned *sz)
{
   if (TYPEO1(list) == ListType)	// TSList, List
   {
      *sz = LIST_LEN(list);
      return LIST_TABLE(list);
   }
   // Tuple, MList
   *sz = TUPLE_LEN(list);
   return TUPLE_TABLE(list);
}

    /* L[offset [,len]] --> __sGet(offset [,len])
     * __sGet(offset) returns a singleton, otherwise, always return a list
     * __sGet(offset,num), [offset,*], [] is illegal, -->List
     * Result readOnlyness matches self, for zkl programmer sanity
     * Thread safe
     * lockType is WL_READER or WL_WRITER
     * If WL_WRITER, the get is destructive (DIP1).
     *    Ugggh, this sucks: Can't call listCreate() if write locked (or GC
     *    can dead lock).
     */
static Instance *_listSGet(TSList *self,pArglist arglist,int lockType,pVM vm)
{
   int	     s, remove = (lockType == WL_WRITER);
   size_t    offset,num;
   Instance *result;
   Fence     F, *fence = 0;

   if (LOCK_IT(self))
   {
      fence = &F;
      writeLockAcquire(LIST_LOCK(self),WL_READER,vm);
      vmSetFence(vm,fence,readUnlockerF,(Instance *)self);
   }
      s = arglistGetChunk(arglist,0,0x100, LIST_LEN(self), 
			   &offset,&num, "List[]",vm);
      if (s < 2)	// [offset] or two args & out of range
      {
	 if (num)
	 {
	    result = LIST_TABLE(self)[offset];
	    if (remove)
	    {
			// promote to a reader AND writer
	       if (fence) writeLockElevate(LIST_LOCK(self),vm);
	          _justShrinkIt(self,offset,1);
	       if (fence)
		  writeLockRelease(LIST_LOCK(self),WL_WRITER); //still a reader
	    }
	 }
	 else		// 2 args & out of range, eg [*,100], [0,0]
	    result = emptyTuple;	// not .pop() ie WL_READER

	 vmRemoveFence(fence,1);
	 return result;
      }

      // else [offset,num] -->new List, size num

      if (num == 0)
      {
	 vmRemoveFence(fence,1);
	 return emptyTuple;
      }

      if (self->instance.objID == roListID)	// ROList#
      {
	 vmRemoveFence(fence,1);  // peace of mind, fence is 0
	 if (IS_TP(self))	// TP
	 {
	    if (num == LIST_LEN(self)) return (Instance *)self;
	    return partitionTuple(((TP*)self)->src,
			LIST_TABLE(self)+offset,num,vm);
	 }
	 // fence is 0 as only TSList is locked
	 return listToTuple((Instance *)self,offset,num,vm);
      }

      // .del is used on the result, probably bad as length is uncertain
      result = listCreate(num,THE_DEFAULT,I_OWNED,vm);
      memcpy(LIST_TABLE(result),&LIST_TABLE(self)[offset],num*sizeof(Instance *));
      LIST_LEN(result) = (LSize_t)num;

      if (remove)
      {
		// promote to a reader AND writer
	 if (fence) writeLockElevate(LIST_LOCK(self),vm);
	    _justShrinkIt(self,offset,num);
	 if (fence)
	    writeLockRelease(LIST_LOCK(self),WL_WRITER);  // still a reader
      }
   vmRemoveFence(fence,1);
   return result;
}

    // .__sGet(), .get(), [], thread safe -->i
static Instance *List_sGet(TSList *self,pArglist arglist,pVM vm)
   { return _listSGet(self,arglist,WL_READER,vm); }

    // .tail(n)
    // .tail(1) --> List, NOT singleton aka [n,1]
    // .tail(n,sz): if sz==1, return that item, not a list (ie self[-1,1])
    //    L.tail(1,1)-->T
static Instance *List_tail(Instance *self,pArglist arglist,pVM vm)
{
   LSize_t   n = (LSize_t)arglistGetInt(arglist,0,0,vm), sz;
   Instance *newList, **src;
   int	     isTuple = 0;
#if 0
   int64_t   sz;
   int       gotSz = arglistTryToGetInt(arglist,1,&sz,"",vm);
#endif

   if (IS_TUPLE1(self))
   {
      sz  = TUPLE_LEN(self);
      src = TUPLE_TABLE(self);
      isTuple = 1;
   }
   else
   {
      sz  = LIST_LEN(self);
      src = LIST_TABLE(self);
   }

   if(n>=sz)
   {
      if(isTuple) return self;
      n = sz;
   }
   if(n==0) return emptyTuple;
#if 0
   if(gotSz)???
#endif
   if (isTuple) return partitionTuple((ZKL_Tuple *)self, &src[sz - n], n,vm);

   newList = tupleCreate(n,I_OWNED,vm);
   memcpy(TUPLE_TABLE(newList), &src[sz - n], n * sizeof(Instance *));
   TUPLE_LEN(newList) = n;

   return newList;

#if 0
   if (IS_TUPLE1(self))
   {
      sz = TUPLE_LEN(self);
      if(n>sz) n = sz;
      if(n==0) return emptyTuple;
      newList = tupleCreate(n,I_OWNED,vm);
      memcpy(TUPLE_TABLE(newList),
            &TUPLE_TABLE(self)[sz - n], n * sizeof(Instance *));
      TUPLE_LEN(newList) = n;
      return newList;
   }

   // !!! really should read lock, match type??
   sz = LIST_LEN(self);
   if(n>sz) n = sz;
   if(n==0) return emptyTuple;
   newList = tupleCreate(n,I_OWNED,vm);
   memcpy(TUPLE_TABLE(newList),
         &LIST_TABLE(self)[sz - n], n * sizeof(Instance *));
   TUPLE_LEN(newList) = n;
   return newList;
#endif
}

    // __sSet(value, x [,y]) -->value
    // foo[*] = a is error, use foo[0,*] = a
    // Thread safe (assuming arglist is)
static Instance *List_sSet(Instance *self,pArglist arglist,pVM vm)
{
   size_t    offset,num;
   Fence     F, *fence = 0;
   Instance *value = TUPLE_TABLE(arglist)[0];

   if (LOCK_IT(self))
   {
      fence = &F;
      writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
      vmSetFence(vm,fence,writeUnlockerF,self);
   }
      arglistGetChunk(arglist,1, 0x00, LIST_LEN(self),
		       &offset,&num, "List[]=",vm);
      if (num)
      {
	 if (num != 1)	// leave a hole of size 1 @ n
	    _justShrinkIt(LISTV(self), offset + 1, num - 1);
      }
      else	// [n,0] = x is an insert, n == len --> error
	 listOpenGap((TSList *)self,offset,1,vm);
      LIST_TABLE(self)[offset] = value;
   vmRemoveFence(fence,1);	// release lock
   instanceIsOrphan(value);
   return value;
}

    // .append(items), also .write
    // thread safe as long as arglist is
Instance *List_append(Instance *self, pArglist arglist, pVM vm)
{
   int        lockIt = LOCK_IT(self);
   size_t     n,z;
   Instance **at;
   
   if (IS_TUPLE1(arglist))
        { at = TUPLE_TABLE(arglist); n = TUPLE_LEN(arglist); }
   else { at = LIST_TABLE(arglist);  n = LIST_LEN(arglist);  }

   if (lockIt) writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
      z = LIST_LEN(self);
      itXpand(LISTV(self),n,25,60,lockIt,vm);
      memcpy(&LIST_TABLE(self)[z],at,n*sizeof(Instance *));
   if (lockIt) writeLockRelease(LIST_LOCK(self),WL_WRITER);

//!!!??????
for (z = 0; z < n; z++) instanceIsOrphan(at[z]);

   return self;
}

    // .pad(n,item): append item n times
Instance *List_pad(Instance *self, pArglist arglist, pVM vm)
{
   int        lockIt = LOCK_IT(self);
   size_t     z,n=(size_t)arglistGetInt(arglist,0,0,vm);
   Instance **table, *item=arglistGet(arglist,1,0,vm);
   
   if (lockIt) writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
      z = LIST_LEN(self);
      itXpand(LISTV(self),n,1,1,lockIt,vm);
      table = LIST_TABLE(LISTV(self)) + z;
      while(n--) *table++ = item;
   if (lockIt) writeLockRelease(LIST_LOCK(self),WL_WRITER);

   if(lockIt) instanceIsOrphan(item);//!!!??????

   return self;
}

    // Append an item, thread safe -->1 or throws
int listAppend(Instance *self, Instance *i, pVM vm)
{
   int lockIt = LOCK_IT(self);

   if (lockIt) writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);

   {
      size_t n = LIST_LEN(self);
      itXpand(LISTV(self),1,20,50,lockIt,vm);
      LIST_TABLE(self)[n] = i;
   }

   if (lockIt) writeLockRelease(LIST_LOCK(self),WL_WRITER);
if (i) instanceIsOrphan(i);	// opMarkArglist
   return 1;
}

    // Append N items of self to self, thread safe -->1 or throws
    // Doesn't check for overwrite
    // Used by VM_pasteArgs()
int listAppendN(Instance *self, size_t offset, size_t n, pVM vm)
{
   if (n)
   {
      int    lockIt = LOCK_IT(self);
      size_t i;

      if (lockIt) writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
	 i = LIST_LEN(self);
	 itXpand(LISTV(self),n,10,100,lockIt,vm);
	 memmove(&LIST_TABLE(self)[i], &LIST_TABLE(self)[offset], n*sizeof(Instance*));
      if (lockIt) writeLockRelease(LIST_LOCK(self),WL_WRITER);
   }

   return 1;
}

#if 0
    // listAppendX(list,vm,item,item,...,ZNIL), thread safe
Instance *listAppendX(Instance *self,pVM vm,Instance *x,...)
{
   va_list	ap;		// argument list pointer
   int		n;
   size_t	i;

   va_start(ap,x);
      for (n = 1; va_arg(ap, Instance *); ) n++;
   va_end(ap);
   writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
      i = LIST_LEN(self);	// stash before table grows
      if (xpand_dTable(D_TABLE(self),n,10,20))
      {
	 writeLockRelease(LIST_LOCK(self),WL_WRITER);
      	 vmThrow(vm,E_OUT_OF_MEMORY,0);
      }
      va_start(ap,x);
	 for (; x; x = va_arg(ap, Instance *)) LIST_TABLE(self)[i++] = x;
      va_end(ap);
   writeLockRelease(LIST_LOCK(self),WL_WRITER);

   return self;
}
#endif

    // List.extend(...) -->self
    // This is an in place operation, thread safe (assuming arglist is)
    // self.extend(self) works
Instance *List_extend(Instance *self,pArglist arglist,pVM vm)
{
   int	     lockIt = LOCK_IT(self);
   unsigned  n,z;
   unsigned  j,newItems;
   Instance *i, **args;

   args = listTable(arglist,&n);
   for(newItems = 0; n--; )
   {
      i = *args++;
      switch(TYPEO(i))
      {
	 case ListType:  newItems += LIST_LEN(i);  break; //!!! not thread safe
	 case TupleType: newItems += TUPLE_LEN(i); break;
	 default:	 newItems++;		   break;
      }
   }

   if (!newItems) return self;

   if (lockIt) writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
      j = LIST_LEN(self);		// do this BEFORE list gets bigger
      itXpand(LISTV(self),newItems,10,25,lockIt,vm);
      LIST_LEN(self) = j;  // shorten list in case self.extend(self)
      args = listTable(arglist,&z);
      while(z--)
      {
	 i = *args++;
	 switch(TYPEO(i))
	 {
	    case ListType:	// don't include the list, just the elements
	       //if(i==self) vmThrow(vm,E_ASSERTION_ERROR,"List.extend(self)");
	       n = LIST_LEN(i);	// !!!not thread safe
	       memcpy(&LIST_TABLE(self)[j],LIST_TABLE(i),n*sizeof(Instance *));
	       j += n;
	       break;
	    case TupleType:
	       n = TUPLE_LEN(i);
	       memcpy(&LIST_TABLE(self)[j],TUPLE_TABLE(i),n*sizeof(Instance *));
	       j += n;
	       break;
	    default: LIST_TABLE(self)[j++] = i; break;
	 }
      } // for
      LIST_LEN(self) = j;
   if (lockIt) writeLockRelease(LIST_LOCK(self),WL_WRITER);
//instanceIsOrphan(arglist);//!!!????
   return self;
}

    // List.flatten() -->T.extend(self.xplode())
static Instance *List_flatten(Instance *self,pArglist arglist,pVM vm)
{
   Fence     F, *fence = 0;
   Instance *newList;

   if (LOCK_IT(self))
   {
      fence = &F;
      writeLockAcquire(LIST_LOCK(self),WL_READER,vm);
      vmSetFence(vm,fence,readUnlockerF,(Instance *)self);
   }
      newList = Tuple_extend(emptyTuple,self,vm);
   vmRemoveFence(fence,1);	// noop if fence == 0
   return newList;
}

    // self is a List type, not a Tuple
    // arglist is a Tuple
Instance *listInsert(Instance *self,size_t idx,Instance *arglist,pVM vm)
{
   int	  numNewItems = (int)listLen(arglist,vm), lockIt = LOCK_IT(self);
   size_t len;
   Fence  F, *fence = 0;

   if (numNewItems < 1) return (Instance *)self;

   if (lockIt)
   {
      fence = &F;
      writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
      vmSetFence(vm,fence,writeUnlockerF,(Instance *)self);
   }
      len = LIST_LEN(self);
      if (idx == len) // becomes an append, I'm assuming doesn't happen often
	 itXpand((TSList *)self,numNewItems,10,25,0,vm);
      else listOpenGap((TSList *)self,idx,numNewItems,vm);	// throws

      memcpy(&LIST_TABLE(self)[idx],&TUPLE_TABLE(arglist),
	     numNewItems * sizeof(Instance *));
   vmRemoveFence(fence,1);	// release lock
   return self;
}

    // .insert(n,item,...), thread safe (assuming arglist is) -->self
    // .insert(*,item,....) --> append
    // .insert(-1,item,....) --> insert before last item
static Instance *List_insert(TSList *self,pArglist arglist,pVM vm)
{
   int	    numNewItems = (int)listLen(arglist,vm) - 1;	// arg 0 is n
   Offset_t idx;
   size_t   len;
   Fence    F, *fence = 0;

   if (numNewItems < 1) return (Instance *)self;

   if (LOCK_IT(self))
   {
      fence = &F;
      writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
      vmSetFence(vm,fence,writeUnlockerF,(Instance *)self);
   }
      len = LIST_LEN(self);
      arglistConstrainOffset(arglist,0,len,0x12,&idx,0,vm);	// throws
      if (idx == len) // becomes an append, I'm assuming doesn't happen often
	 itXpand(self,numNewItems,10,25,0,vm);
      else listOpenGap(self,(size_t)idx,numNewItems,vm);	// throws

      memcpy(&LIST_TABLE(self)[idx],&TUPLE_TABLE(arglist)[1],
	     numNewItems * sizeof(Instance *));
   vmRemoveFence(fence,1);	// release lock

//!!!!????
instanceIsOrphan(arglist);

   return (Instance *)self;
}

    // Are a & b the same type? Is a.isType(b) True?
    // man oh man this is ugly, all because I let two (or more) types
    //   pretend to be the same (List/Tuple, Data/KData)
static int _cmpTypes(Instance *a, Instance *b, pVM vm)
{
   int ta = TYPEO(a), tb = TYPEO(b), x,y;

   if (ta == tb) return 1;
   if (ta == IntType) return 0;		// PtrInts

   x = (ta == ListType) || (ta == TupleType);	// a "list"?
   y = (tb == ListType) || (tb == TupleType);
   if (x || y) return (x && y);

   return 0;
}

    // _findNoLock(x): See _find() for semantics.
static int _findNoLock(TSList *self,Instance *x, size_t *idx, pVM vm)
{
   pOp	      opEqual;
   size_t     n,i;
   Instance **table = LIST_TABLE(self);

   #if USE_POINTER_INTS
      ZKL_Int	i64;
      x = decantInt(x,&i64);	// on the off chance x is Int, else noop
   #endif

   opEqual = I_OP(x,OP_EQ);
   n       = LIST_LEN(self);
   for (i = 0; n--; i++)
   {
      Instance *y = *table++;
      if (opEqual(x,y,vm) == BoolTrue && _cmpTypes(y,x,vm))
      {
	 *idx = i;
      	 return 1;
      }
   }
   return 0;
}

    /* _find(x [,offset=0 [,len=*]]), thread safe
     * For this op, equality is defined as: uhhhhhh
     * Oh fuck it, equality is same type & whatever object decides == is and
     * type sameness.  Which, of course, is broken for NativeTypes
     * X is self[n] if X == self[n] and self[n].isType(X)
     * I don't want L(3).find("3")/L(1).holds(1.0)/L(True).holds(1) to work.
     */
static int _find(Instance *self,Instance *arglist, Instance *mop, size_t *idx, pVM vm)
{
   int 	      numArgs  = (int)TUPLE_LEN(arglist);
   size_t     offset   = 0, num;
   Instance  *x	 = arglistGet(arglist,0,0,vm);
   Instance **table;
   pOp	      opEqual;
   Fence      F, *fence = 0;
MLIST(mlist,2);

   #if USE_POINTER_INTS
      ZKL_Int i64;
      x = decantInt(x,&i64);
   #endif
//   op = eq ? I_OP(x,OP_EQ) : I_OP(x,OP_GTE);
   opEqual = I_OP(x,OP_EQ);
mlistBuild(mlist,Void,ZNIL);

   if (LOCK_IT(self))
   {
      fence = &F;
      writeLockAcquire(LIST_LOCK(self),WL_READER,vm);
      vmSetFence(vm,fence,readUnlockerF,(Instance *)self);
   }
      if (numArgs != 1)		// .find(x,offset), .find(x,offset,num)
	 arglistGetChunk(arglist,1,0x103, LIST_LEN(self),&offset,&num, 0,vm);
      else num = (size_t)LIST_LEN(self);

      for (table = &LIST_TABLE(self)[offset]; num--; offset++)
      {
	 Instance *y = *table++;
if (mop)
{ 
   TUPLE_TABLE(mlist)[0]=y;
   if (Op_call(mop,(Instance *)mlist,vm) == BoolTrue) goto foundIt; 
}
else
	 if (opEqual(x,y,vm) == BoolTrue && _cmpTypes(y,x,vm))
	 {
foundIt:
	    vmRemoveFence(fence,1);
	    *idx = offset;
	    return 1;
	 }
      }
   vmRemoveFence(fence,1);
   *idx = offset;
   return 0;
}

static int _tfind(Instance *,Instance *,size_t *,pVM);

    // .find(x,offset=0,len=*) -->n|Void, thread safe
static Instance *List_find(Instance *self,pArglist arglist,pVM vm)
{
   size_t n;

   if (!(IS_TUPLE1(self) ? 
       _tfind(self,arglist,&n,vm) : _find(self,arglist,0,&n,vm))) return Void;
   return intCreate(n,vm);
}

#if 0
    // .findBop(Op,offset=0,len=*) -->n|Void, thread safe
static Instance *List_findBop(Instance *self,pArglist arglist,pVM vm)
{
   size_t n;
  _find(self,arglist,arglistTryToGetBT(arglist,0,OpType,0,vm),&n,vm);
   return intCreate(n,vm);
}
#endif

    // .index(x,offset=0,len=*), thread safe -->n|IndexError
static Instance *List_index(Instance *self,pArglist arglist,pVM vm)
{
   size_t n;

   if (!(IS_TUPLE1(self) ?
       _tfind(self,arglist,&n,vm) : _find(self,arglist,0,&n,vm)))
   {
      char buf[100];
      Instance *x = arglistGet(arglist,0,0,vm);
      sprintf(buf,"%s.index(%s): not found",iname(self),iname(x));
      vmThrow(vm,E_INDEX_ERROR,buf);
   }
   return intCreate(n,vm);
}

    // .holds(item,offset=0,len=*), thread safe -->Bool
static Instance *List_holds(Instance *self,pArglist arglist,pVM vm)
{
   size_t n;
   return boolCreate(
      IS_TUPLE1(self) ? _tfind(self,arglist,&n,vm) : _find(self,arglist,0,&n,vm));
}

    // thread safe. NO range checking is done
void listReplace(Instance *self, size_t n, Instance *newValue, pVM vm)
{
   if (!LOCK_IT(self)) LIST_TABLE(self)[n] = newValue;
   else
   {
      writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
         LIST_TABLE(self)[n] = newValue;
      writeLockRelease(LIST_LOCK(self),WL_WRITER);
   }
instanceIsOrphan(newValue);
}

    // .set(n,newValue), thread safe.  self[n] = newValue -->self
    /* In the "isn't that interesting dept": The instance being discarded
     * needs protecting. c1, c2 are containers. 
     * z=L(c1), z.set(0,c2), c2.add(c1)
     * There is a nice big window where c1 isn't owned/marked
     */
static Instance *List_set(Instance *self,pArglist arglist,pVM vm)
{
   int	     n	      = (int)arglistGetInt(arglist,0,"List.set",vm);
   int	     lockIt   = LOCK_IT(self);
   Instance *newValue = arglistGet(arglist,1,0,vm), *oldValue;
   size_t    offset,len;

   if (lockIt) writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
      len      = LIST_LEN(self);
      offset   = constrainXPhat(n,len,0, lockIt,writeUnlocker,self,"List.set",vm);
      oldValue = LIST_TABLE(self)[offset];
      LIST_TABLE(self)[offset] = newValue;
   if (lockIt) writeLockRelease(LIST_LOCK(self),WL_WRITER);
   instanceIsOrphan(oldValue);		// yes, it has happened
instanceIsOrphan(newValue);

   return self;
}

#if 0
    // .get(n), thread safe.  --> self[n]
static Instance *List_get(Instance *self,pArglist arglist,pVM vm)
{
   int	     n	    = (int)arglistGetInt(arglist,0,"List.set",vm);
   int	     lockIt = LOCK_IT(self);
   Instance *v;
   size_t    offset,len;

   if (lockIt) writeLockAcquire(LIST_LOCK(self),WL_READER,vm);
      len     = LIST_LEN(self);
      offset  = constrainXPhat(n,len,0, lockIt,readUnlocker,self,"List.get",vm);
      v	      = LIST_TABLE(self)[offset];
   if (lockIt) writeLockRelease(LIST_LOCK(self),WL_READER);
   return v;
}
#endif

    // .clear(), thread safe -->self
    // .clear(x,y,z) --> clear().append(x,y,z)
static Instance *List_clear(Instance *self,pArglist arglist,pVM vm)
{
   listClear(self,vm);
   return List_append(self,arglist,vm);
}

    // Thread safe
void listTruncateTo(Instance *self,size_t n,pVM vm)
{
   int lockIt = LOCK_IT(self);

   if (lockIt) writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
      if (n == 0) 		   LIST_LEN(self) = 0;
      else if (n < LIST_LEN(self)) LIST_LEN(self) = (LSize_t)n;
   if (lockIt) writeLockRelease(LIST_LOCK(self),WL_WRITER);
}

    // Don't lock because others call this from inside a lock
    // NOT thread safe, doesn't GC
static void _justShrinkIt(TSList *self, size_t offset, size_t num)
{
   size_t size, len = LIST_LEN(self);

   if (num == 0) return;

   if (offset + num >= len)
   {
      LIST_LEN(self) = (LSize_t)offset;
      return;
   }

   size = (len - offset - num) * sizeof(Instance *);
   memmove(&LIST_TABLE(self)[offset], &LIST_TABLE(self)[offset+num], size);

   LIST_LEN(self) = (LSize_t)(len - num);
//!!! orphans!
}

void listDelete(Instance *self,size_t offset,size_t num,pVM vm)
{
   if (!LOCK_IT(self)) _justShrinkIt((TSList *)self,offset,num);
   else
   {
      writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
	 _justShrinkIt((TSList *)self,offset,num);
      writeLockRelease(LIST_LOCK(self),WL_WRITER);
   }
}

    // .del(n, [num | *] = 1), thread safe
//.del(Void) --> noop would be nice!!!!
static Instance *List_del(TSList *self,pArglist arglist,pVM vm)
{
   size_t idx,num;
   Fence  F, *fence = 0;

   if (LOCK_IT(self))
   {
      fence = &F;
      writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
      vmSetFence(vm,fence,writeUnlockerF,(Instance *)self);
   }
      arglistGetChunk(arglist,0,0x102,LIST_LEN(self),&idx,&num,"List.del",vm);
      _justShrinkIt(self,idx,num);
   vmRemoveFence(fence,1);
   return (Instance *)self;
}

    // A special List case: pop last element from a List as stack
    // Destructive read, thread safe but not GC safe so use a fence
    // Destructive reads have the DIP1 problem. 
Instance *stackPop(Instance *self,pVM vm)
{
   int	     lockIt = LOCK_IT(self);
   Instance *i = 0;
   LSize_t   len;

   if (lockIt) writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
      if ((len = LIST_LEN(self)))	// don't pop an empty list
      {
	 len--;
	 i = LIST_TABLE(self)[len];
	 LIST_LEN(self) = len;	// i now gone from self
      }
   if (lockIt) writeLockRelease(LIST_LOCK(self),WL_WRITER);
instanceIsOrphan(i);
   return i;
}

    /* .pop(offset): Destructive read, thread safe -->i
     * This has the DIP1 problem:  Two threads, one [shared] list (eg Pipe),
     *   destructive read and GC means the popped instance might not get
     *   marked.  You can SOMETIMES fence this issue, but, in general, you
     *   can't.
     * You need to call instanceIsOrphan()
     * Returns zero if n is out of range.
     */
Instance *listPop(Instance *self,size_t offset, pVM vm)
{
   int	     lockIt = LOCK_IT(self);
   Instance *i;

   if (lockIt) writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
      if (offset >= LIST_LEN(self)) i = 0;
      else
      {
	 i = LIST_TABLE(self)[offset];
	 _justShrinkIt(LISTV(self),offset,1);	// i removed from self
      }
   if (lockIt) writeLockRelease(LIST_LOCK(self),WL_WRITER);

instanceIsOrphan(i);
   return i;
}

    // .pop(offset=self.len()-1, num=1): Destructive read, thread safe
static Instance *List_pop(TSList *self,pArglist arglist,pVM vm)
{
   Instance *result;
   if (TUPLE_LEN(arglist) == 0)	// .pop()
   {
      result = stackPop((Instance *)self,vm);
      if (!result) vmThrow(vm,E_INDEX_ERROR,"List.pop(): Empty list");
   }
   else result = _listSGet(self,arglist,WL_WRITER,vm);
   instanceIsOrphan(result);
   return result;
}

    // .read(), .readln() --> .pop(0)
    // distructive
static Instance *List_read(Instance *self,pArglist arglist,pVM vm)
{
   Instance *result = listPop(self,0,vm);
   if (!result) vmThrowTheEnd(vm);
   instanceIsOrphan(result);
   return result;
}

    /* Remove the tail of a VM arglist.  The tail is defined as everything
     *   after the last marker, which has to exist.
     * A marker is 0 (which isn't an Instance).
     * It is an error if the marker doesn't exist.
     * Returns the tail (orphan), you have to protect arglist WHILE I'm
     *   running.  You shouldn't have to but all this thread stuff makes my
     *   head hurt.
     * Why make a copy of the arglist, instead of just having a pointer into
     *   the middle?  Basically, because that chunk isn't a list.  Also, in
     *   cases like f(1,g(2,3),4), marshalling args would probably be a pain
     *   to manage.
     * !IMPORTANT!  This is NOT called during GC and is single threaded.
     *   Once the tail is detached, it needs to be protected as it is no
     *   longer part of arglist.
     */
Instance *
arglistPop(pArglist self, Byte *mlist,unsigned mlistSz, Instance **pi, pVM vm)
{
   int	      i,newLen,len = (int)LIST_LEN(self);
   Instance **table = LIST_TABLE(self);

   if (len == 0)	// error
      vmHalt("arglistPop: Empty list");

   for (i = len; 0 <= --i; )	// scan backwards for marker (0). Yes, "0<="
      if (table[i] == 0)
      {
	 LSize_t   sz = len - ++i;
	 Instance *tail;

	 newLen = i - 1;	// remember to get rid of the marker
	 if (sz == 0) tail = NoArglist;	// new arglist is empty
	 else
	 {
	    if(sz<=mlistSz) tail=mlistCopy(mlist,(Instance *)self,i,mlistSz);
	    else
	    {
#if 1
	       tail = tupleCreate(sz,I_OWNED,vm);  // can throw
	       memcpy(TUPLE_TABLE(tail),&table[i],sz*sizeof(Instance *));
	       TUPLE_LEN(tail) = sz;
#else
lots of routines assume pArglist is Tuple
	       tail = partitionTuple((ZKL_Tuple *)self,&table[i],sz,vm);
#endif
	    }
	    *pi = tail;   // so tail can be marked once arglist is truncated
	 }
	 LIST_LEN(self) = newLen;	// tail is now an orphan
	 return tail;
      }
      // error, marker not found
   vmHalt("arglistPop: No marker");
   return 0;
}

    /* Cases: VM arglist --> window: [offset,count]
     * Zero is the mark between arglists (opMarkArglist)
     * L()		--> [0,0]
     * L(0)		--> [1,0]
     * L(0,0)		--> [2,0]
     * L(..,0n)		--> [len,0]	  == [n,0]
     * L(X)		--> [0,1]
     * L(...Xn)		--> [0,len]	  == [0,n]
     * L(0,X1)		--> [1,1]
     * L(0,X1,X2)	--> [1,2]
     * L(0,X1,..,Xn)	--> [1,len-1]     == [1,n-1]
     * L(0,0,X2)	--> [2,1]
     * L(..0i,..,Xn)	--> [i+1,len-i-1] == [i+1,n-i-1]
     * !IMPORTANT! NOT called during GC and single threaded.
     * Arglist isn't changed.
     */
void argWindow(Instance *arglist,int *poffset, int *pnumArgs,pVM vm)
{
   int	      offset = 0, numArgs = 0;
   int	      i,len = (int)LIST_LEN(arglist);
   Instance **table = LIST_TABLE(arglist);

   if (len == 0) {}				// L()
   else if (table[len-1] == 0) offset = len;	// L(..,0)
   else
   {
      for (i = len; 0 <= --i; )	// scan backwards for marker
	 if (table[i] == 0)	// L(...0a,....,Xn)
	 {
	    offset  = i + 1;
	    numArgs = len - i - 1;
	    break;
	 }
      if (i < 0)		// marker not found: L(...Xn)
      {
	 offset  = 0;
	 numArgs = len;
      }
   }
   if (offset > 0xFFff || numArgs > 0xFFff)  // only got 16 bits
   {
      char buf[200];
      sprintf(buf,"Arglist overflow: %d %d",offset,numArgs);
      vmThrow(vm,E_VM_ERROR,buf);
//      vmThrow(vm,E_VM_ERROR,"Arglist overflow");
   }
   *poffset  = offset;		// Order important
   *pnumArgs = numArgs;
   return;
}

    // Thread safe
    // !!!jeezus, this locks and unlocks waaaay too often.
static int listRemove(Instance *self, Instance *x, pVM vm)
{
   int    lockIt = LOCK_IT(self), s = 0;
   size_t n;

	// don't let list change after find
   if (lockIt) writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
      while (_findNoLock(LISTV(self),x,&n,vm))
      {
	 _justShrinkIt(LISTV(self),n,1);
	 s = 1;
      }
   if (lockIt) writeLockRelease(LIST_LOCK(self),WL_WRITER);
   return s;
}

    // .remove(x,...) search for an entry and nuke it, thread safe
    // -->self
static Instance *List_remove(Instance *self,pArglist arglist,pVM vm)
{
   Instance *x;
   LSize_t   i;

   for (i = 0; (x = listGet(arglist,i)); i++) listRemove(self,x,vm);
   return self;
}

    // .removeEach(list,...) Remove each item in list from self
    // -->self
static Instance *List_removeEach(Instance *self,pArglist arglist,pVM vm)
{
   Instance *x;
   LSize_t   i;

   for (i = 0; (x = listGet(arglist,i)); i++)
   {
      switch(TYPEO(x))
      {
	 case ListType: case TupleType: List_remove(self,x,vm); break;
	 default: listRemove(self,x,vm); break;
      }
   }
   return self;
}

#if 0
    /* .removeIf(f,static args): -->self
     * If f(self[i]) returns:
     *   True: remove self[i]
     *   0   : Don't remove self[i] and stop
     *   1   : Remove self[i] and stop
     *   *   : Remove rest of the list
     * Thread safe, GC makes this more painful than it should be.
     * Mungs arglist
     */
static Instance *List_removeIf(Instance *self,pArglist arglist,pVM vm)
{
   Fence     fence;
   int	     lockIt = LOCK_IT(self);
   size_t    i,n;
   Instance *fcn = arglistGetBT(arglist,0,FcnType,"List.removeIf",vm);

   if (lockIt)
   {
      writeLockAcquire(LIST_LOCK(self),WL_READER,vm);
      vmSetFence(vm,&fence,readUnlockerF,self);
   }
   else vmSetFence(vm,&fence,0,0);
      fence.i1 = fcn;		// because I'll stomp arglist[0]
      n = LIST_LEN(self);
      for (i = 0; i < n; i++)
      {
	 Instance *r;

//!!! BAD
TUPLE_TABLE(arglist)[0] = LIST_TABLE(self)[i];
	 r = fcnRun(fcn,arglist,0,vm);	// can't be write locked here
	 if (r == Star)		// nuke the rest of the list
	 {
			// promote to a reader AND writer
	    if (lockIt) writeLockElevate(LIST_LOCK(self),vm);
	       LIST_LEN(self) = (LSize_t)i;
	    if (lockIt)
	       writeLockRelease(LIST_LOCK(self),WL_WRITER);  // --> reader
	    break;
	 }
//	 if (r == BoolTrue || r == One)
	 if (r == BoolTrue || IS_ONE(r))
	 {
	    if (lockIt) writeLockElevate(LIST_LOCK(self),vm);
	       _justShrinkIt(LISTV(self),i,1);
	    if (lockIt) writeLockRelease(LIST_LOCK(self),WL_WRITER);
	    n--; i--;
	 }
//	 if (r == Zero || r == One) break;
	 if (IS_ZERO(r) || IS_ONE(r)) break;
      }
   vmRemoveFence(&fence,1);
   return self;
}
#endif

    /* .xplode([offset=0,n=*]) : Shove self (or parts of self) onto the arg
     *   stack.
     * DO NOT call this from another VM or thread. That way, I know this is
     *   single threaded and only have to worry about GC.
     * thread safe
     * -->VoidPlus
     */
static Instance *List_xplode(TSList *self,Instance *arglist,pVM vm)
{
   size_t numArgs = TUPLE_LEN(arglist);
   size_t offset,num;

      	// n = L().xplode() is bogus
   if (!voidPlusOK(vm)) return Void;

   if (numArgs) arglistGetChunk(arglist,0,0x101,LIST_LEN(self),&offset,&num,0,vm);
   else	      { num = LIST_LEN(self); offset = 0; } // .xplode(), 99.9999%

   if (num)
   {
      size_t n;
//      Fence  F, *fence = 0;

      arglist = vmArglist2(vm);		// a List, not Tuple
#if 0
      if (LOCK_IT(self))	// single thread
      {
	 fence = &F;
	 writeLockAcquire(LIST_LOCK(self),WL_READER,vm);
	 vmSetFence(vm,fence,readUnlockerF,(Instance *)self);
      }
#endif
	 n = LIST_LEN(arglist);
	 itXpand(LISTV(arglist),num,10,100,0,vm);
	 memcpy(&LIST_TABLE(arglist)[n],&LIST_TABLE(self)[offset],
		num * sizeof(Instance *));
//      vmRemoveFence(fence,1);
   }
   return VoidPlus;		// out of band signal to opAddToArgs
}

    // .swap(x|*,y|*|-1) : swap two entries, thread safe
    // * is always an index error
static Instance *List_swap(Instance *self,pArglist arglist,pVM vm)
{
   Instance *tmp;
   Offset_t  a,b;
   size_t    len;
   Fence     F, *fence = 0;

   if (LOCK_IT(self))
   {
      fence = &F;
      writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
      vmSetFence(vm,fence,writeUnlockerF,self);
   }
      len = LIST_LEN(self);
      arglistConstrainOffset(arglist,0,len, 0x00,&a,0,vm);
      arglistConstrainOffset(arglist,1,len, 0x00,&b,0,vm);
      tmp = LIST_TABLE(self)[a]; LIST_TABLE(self)[a] = LIST_TABLE(self)[b];
      LIST_TABLE(self)[b] = tmp;
   vmRemoveFence(fence,1);
   return self;
}

    // .walker():
Instance *List_walker(Instance *self,pArglist a, pVM vm)
   { return walkerCreate(self,_listGetter,(void *)(size_t)TYPEO1(self),0,vm); }

    // .sort([cmpFcn]), not thread safe
Instance *Sequence_sort(Instance *self,pArglist arglist, pVM vm)
{
   char     *fcnName = "qiSort";
   Instance *args;
   MLIST(mlist,2);

   if (!listLen(self,vm)) return self;

//??? just call sort and let it figure which version?
   args = (0==listLen(arglist,vm)) ?
      mlistBuild(mlist,self,ZNIL) :		 	// sort()
      mlistBuild(mlist,self,listGet(arglist,0),ZNIL);	// sort(cmpFcn)

   if (IS_TUPLE(self) || self->objID == roListID || IS_TP(self))
      fcnName = "qtSort";     // "out of place" sort

   return fcnRunFromClass(Utils_Helpers,fcnName,args,vm);
}

    // .concat(separator="",prefix="",suffix="") -->String
    // Not thread safe
static Instance *List_concat(Instance *self,pArglist arglist, pVM vm)
{
#if 1
   Instance    *args;
   Instance    *sep = arglistTryToGet(arglist,0);
   MLIST(mlist,5);

   args   = mlistBuild(mlist,self,(sep ? sep : emptyString),ZNIL);
   mlistExtendN(mlist,arglist,1,5);
   return fcnRunFromClass(Utils_Helpers,"concat", args, vm);
#else
   char      *sep = arglistTryToGetString(arglist,0,".concat",vm), *s;
   Instance **table;
   ZBText     buf;
   unsigned   ln, sn;

   if (!sep) sep = "";
   sn = strlen(sep);

   table = listTable(self,&ln);
   if (!ln) return emptyString;
   zbtextInit(&buf,vm);

   if((s = arglistTryToGetString(arglist,1,0,vm))) zbtextAppend(&buf,s,vm);
   zbtextAppendI(&buf,*table++,vm);
   while(--ln)
      { zbtextAppendN(&buf,sep,sn,vm); zbtextAppendI(&buf,*table++,vm); }
   if((s = arglistTryToGetString(arglist,2,0,vm))) zbtextAppend(&buf,s,vm);

   return zbtextClose(&buf,vm);
#endif
}

    // .enumerate(), not thread safe
    // self.walker().tweak(fcn(i,n){ return(n,i) }).walk()
static Instance *List_enumerate(Instance *self,pArglist arglist, pVM vm)
{
   Fence      fence;
   Instance **table, *e;
   unsigned   n,N;

   table = listTable(self,&N);

   if (N == 0) return emptyTuple;
   vmSetFence(vm,&fence,0,(e = tupleCreate(N,I_OWNED,vm)));
      for (n = 0; N--; n++,table++)
	 tupleAppend(e,tupleCreateX(vm,intCreate(n,vm),*table,ZNIL) );
   vmRemoveFence(&fence,0);
   return e;
}

    // .callProperty(propertyName) --> Utils.Helpers.callMethod(name,self)
Instance *Sequence_callProperty(Instance *self,pArglist arglist, pVM vm)
{
   MLIST(mlist,2);
   arglist = mlistBuild(mlist,arglistGet(arglist,0,0,vm),self,ZNIL);
   return fcnRunFromClass(Utils_Helpers,"callProperty",arglist,vm);
}

    // .callMethod(methodName, static args)
    // --> Utils.Helpers.callMethod(name,self,static args)
Instance *Sequence_callMethod(Instance *self,pArglist arglist, pVM vm)
{
   Instance *args;
   MLIST(mlist,11);

   if (TUPLE_LEN(arglist) > 10)
      vmThrow(vm,E_ASSERTION_ERROR,
		"List.callMethod: No more than 10 static parameters");

   args = mlistBuild(mlist,arglistGet(arglist,0,0,vm),self,ZNIL);
   mlistExtend(mlist,arglist,1,11);

   return fcnRunFromClass(Utils_Helpers,"callMethod",args,vm);
}

    /* List.filter(Fcn|Method|Op|PFA|methodName [,static args])
     * .filter(): f --> f(x) --> x
     * .filter(Void): f --> f(x) --> x
     * .filter(Method) : Vault.contents.filter(self.isType) to get classes
     * _filter (what == FILTER_SAVE_RESULTS): Return all items that pass
     *   filter.
     * _filter (what == FILTER_INDEX): Return the index (in self) of the
     *    first item that passes filter or False. Stops at first pass.
     * _filter (what == FILTER_SAVE_RESULTS && pResult2):  Same as
     *    _filter(FILTER_SAVE_RESULTS) but the fails are also saved.
     */

    // .filter --> List(items that passed) 
static Instance *List_filter(Instance *self,pArglist arglist, pVM vm)
{
   return zfilter(self,0x0,_listGetter,(void *)(size_t)TYPEO1(self),
		   arglist,0,vm); 
}

    // .filter1 --> <item that passed>|False == .filter()[0]
static Instance *List_filter1(Instance *self,pArglist arglist, pVM vm)
{
   return zfilter1(self,0,_listGetter,(void *)(size_t)TYPEO1(self),
		   arglist,0,vm);
}

    // .filter1n --> idx|False
static Instance *List_filter1n(Instance *self,pArglist arglist, pVM vm)
{
   return zfilter1n(self,0,_listGetter,(void *)(size_t)TYPEO1(self),
		    arglist,0,vm); 
}

    // .filterNs --> indexes
static Instance *List_filterNs(Instance *self,pArglist arglist, pVM vm)
{
   return zfilterNs(self,0,_listGetter,(void *)(size_t)TYPEO1(self),
		    arglist,0,vm); 
}

    // .filter22 -->T(L(pass),L(fail))
static Instance *List_filter22(Instance *self,pArglist arglist, pVM vm)
{
   return zfilter22(self,0x0,_listGetter,(void *)(size_t)TYPEO1(self),
		    arglist,0,vm); 
}

   /* Run items in list: self[n](args)
    * stop: 0 (don't stop), 1 (stop high), 2 (stop low).
    * phat:
    *    Stop == 0:
    *       0: Return Void
    *       1: Return a Tuple of results
    *    Stop == 1|2:
    *       0: Return Bool
    *       1: Return idx
    *       2: Return T(idx,self[idx](args))
//!!! utilize this
!!!would be nice package this for repeats (as in Walker)
    */
static Instance *
_listRun(Instance *self,pArglist arglist,int stop,int phat,pVM vm)
{
   Fence     fence;
   Instance *result, **table, *r = 0;
   LSize_t   i,n;
   int	     zstop,_;
   unsigned  actionType[MAX_ACTIONS];

   _scanAction(0,self,0,&_,actionType,vm);  // scan list/self of actions

   table = listTable(self,&n);
   vmSetFence(vm,&fence,0,0);
      if (!stop)	// no stopping me
      {
	 result = 0;
	 if (phat) result = fence.i1 = tupleCreate(n,I_OWNED,vm); 
	 for (i = 0; i < n; i++)
	 {
	    Instance *f = *table++;
	    // funky: T("foo").run()-->ZR_SKIP since ?.foo doesn't exist
	    r = _zrun(0,f,actionType[i],&zstop,arglist,vm);
	    if(zstop || r==VoidVoid) r = Void;  // placeholder
	    if (result) tupleAppend(result,r);
	 }
	 if (!result) result = Void;
      }
      else	// stoppable
      {
	 result = BoolFalse;
	 for (i = 0; i < n; i++)
	 {
	    Instance *f = *table++;
	    int ftype   = actionType[i], b;
	    r	        = _zrun(0,f,ftype,&zstop,arglist,vm);

	    if (zstop && zstop!=ZR_STOP) continue;  // everything but Void.Stop
	    b = resultToBool(r,vm);
	    if ((stop == 1 && b)   ||	// stop high
		(stop == 2 && !b))	// stop low
	    {
	       result = BoolTrue;
	       break;
	    }
	    if (zstop == ZR_STOP) break;	// Void.Stop
	 } // for
	 if (result == BoolTrue)
	 {
	    if (phat == 1) result = intCreate(i,vm);
	    else if (phat == 2)
	    {
	       if (r == VoidVoid) r = Void;
	       result = tupleCreateX(vm,intCreate(i,vm),r,ZNIL);
	    }
	 }
      }
   vmRemoveFence(&fence,0);
   return result;
}

    /* L(fcns, methods, properties).runNFilter([a,b,static args])
     *    Run each fcn/method until one returns "passes" or "fails"
     *    Think select()
     * .runNFilter()	  : Stop on True, return True if found stopper
     * .runNFilter(True)  : Same as ()
     * .runNFilter(False) : Stop on False, return True if stops
     * .runNFilter(B,1)   : Return False or n, where L[n] is item that stopped
     * .runNFilter(B,2)   : Return False or T(n,result)
     * .runNFilter(B,0)   : Same as .runNFilter(B)
     * .runNFilter(B,N,staticArgs)
     */
static Instance *runNFilter(Instance *self,pArglist arglist,pVM vm)
{
   Instance *a1 = arglistTryToGet(arglist,0);
   Instance *a2 = arglistTryToGet(arglist,1);
   int	     stop=1, phat=0;
   int 	     numArgs = TUPLE_LEN(arglist);
   MLIST(mlist,10);

   if (a1 == BoolFalse) stop = 2;
   if (a2 && TYPEO(a2) == IntType) phat = (int)convertToInt(a2,vm);

   if (numArgs > 2)
   {
      if (numArgs > 12)
	 vmThrow(vm,E_ASSERTION_ERROR,
		"List.runNFilter: No more than 10 static parameters");
      arglist = mlistCopy(mlist,arglist,2,10);
   }
   else arglist = NoArglist;

   return _listRun(self,arglist,stop,phat,vm);
}

    // .run([save results=False, [static args]]) -->Tuple|Void
    // Run all items in self, maybe return result else return Void
    // L(f,g).run(True,a) --> L(f(a),g(a))
//!!!??? == L(f,g).apply[2](fcn(f){f(vm.pasteArgs(1))})
static Instance *List_run(Instance *self,pArglist arglist,pVM vm)
{
   Instance *a0 = arglistTryToGet(arglist,0);
   int 	     n  = TUPLE_LEN(arglist);
   MLIST(mlist,10);

   if (n)
   {
      if (n > 10)
	 vmThrow(vm,E_ASSERTION_ERROR,
		"List.run: No more than 10 static parameters");
      arglist = mlistCopy(mlist,arglist,1,10);
   }
   return _listRun(self,arglist,0,a0==BoolTrue,vm);
}

#if 0
    // T("set","len").feed(T(1,2,3))
???"set", etc are feed T(1,2,3), fcns are feed(1,2,3)
static Instance *List_feed(Instance *self,pArglist arglist,pVM vm)
{
   Fence     fence;
   Instance *result, **table, *r;
   LSize_t   i,n;
   int	     stop,_;
   int	     actionType[MAX_ACTIONS];

   _scanAction(0,self,0,&_,actionType,vm);

   table = listTable(self,&n);
   vmSetFence(vm,&fence,0,0);
      result = fence.i1 = tupleCreate(n,I_OWNED,vm); 
      for (i = 0; i < n; i++)
      {
	 Instance *f = *table++;
	 int	   ft = actionType[i];
	 if ((ft & 0xFF)==StringType)
	      r = _zrun(arglist,f,ft,&stop,NoArglist,vm);
	 else r = _zrun(0,f,ft,&stop,arglist,vm);
	 if (stop) break;	// Void.Stop
	 if (r == VoidVoid) r = Void;	// Void.Skip?, add placeholder
	 tupleAppend(result,r);
      }
   vmRemoveFence(&fence,0);
   return result;
}
#endif

    // .reduce(f [,init [,static args]]) --> f(sum,item,static args)
    // --> sum = init; foreach x in (self) { sum = f(sum,x,static args) }
    // No init, init = table[0]
    // L().reduce(f) --> Void
    // NOT thread safe
static Instance *List_reduce(Instance *self,Instance *arglist,pVM vm)
   { return zreduce(self,_listGetter,(void *)(size_t)TYPEO1(self),arglist,0,vm); }

    // .pump
Instance *List_pump(Instance *self,Instance *arglist,pVM vm)
   { return pump(self,PMP_OK2CNT,_listGetter,(void *)(size_t)TYPEO1(self),arglist,0,vm); }

    // .apply(f [,static args]) -->T(...)
static int _laggregator(void *pDst,Instance *r,pVM vm)
   { tupleAppend((Instance *)pDst,r); return 1; }
Instance *List_apply(Instance *self,pArglist arglist, pVM vm)
{
   int	     lockIt = LOCK_IT(self);
   Fence     fence;
   Instance *r;

   if (lockIt)
   {
      writeLockAcquire(LIST_LOCK(self),WL_READER,vm);
      vmSetFence(vm,&fence,readUnlockerF,self);
   }
   else vmSetFence(vm,&fence,0,0);
      r = tupleCreate(listLen(self,vm),I_OWNED,vm);
      fence.i1 = r;
      zapply(self,_listGetter,(void *)(size_t)TYPEO1(self),
		_laggregator,(void *)r, arglist,0,vm);
   vmRemoveFence(&fence,1);
   return r;
}

    // .apply2(f [,static args]) -->Void
    // Isn't this just .reduce(_,i)?
Instance *List_apply2(Instance *self,pArglist arglist, pVM vm)
{
   zapply(self,_listGetter,(void *)(size_t)TYPEO1(self),0,0,arglist,0,vm);
   return Void;
}

    // list.merge(list) --> self
    // list.merge(a,b,c) --> list.merge(arglist)
    // Assumes both self and list are sorted
    // If throws, self is valid but in funky state
Instance *List_merge(Instance *self,pArglist arglist, pVM vm)
{
   unsigned  ns, nl, n, z;
  Instance **ts, **tl, **sptr, *pi;

   if((pi = arglistTryToGet(arglist,0)) && !(IS_LIST(pi) || IS_TUPLE(pi)))
        tl = listTable(arglist,&nl);
   else tl = listTable(arglistGetBT(arglist,0,ListType,"List.merge",vm),&nl);

   if(!nl) return self;

   listOpenGap((TSList *)self,0,nl,vm);  // shift self right
   sptr = listTable(self,&ns); ns -= nl;
   // fill gap with Void in case < throws
   for(ts = sptr, z = nl; z--; ) *ts++ = Void;
   ts = sptr + nl;

   for(n = 0; 1; n++)
   {
      Instance *a = *ts, *b = *tl, *s;
      if(nl == 0)  // append rest of self
      {
	 memmove(sptr,ts,ns*sizeof(Instance *));
	 break;
      }
      if(ns == 0)  // append rest of list
      {
	 memcpy(sptr,tl,nl*sizeof(Instance *));
	 break;
      }
      if(ns && nl)
      {
	 #if USE_POINTER_INTS
	    Instance *c = a;
	    ZKL_Int   i64;
	    c = decantInt(a,&i64);
	    s = I_OP(c,OP_LT)(c,b,vm);
	 #else
	    s = I_OP(a,OP_LT)(a,b,vm);
	 #endif

	 if (s == BoolTrue)	// this will recurse for lists
	      { *sptr++ = a; ts++; ns--; }
	 else { *sptr++ = b; tl++; nl--; }
      }
   }
   return self;
}

    // String -->List (not Tuple)
    // Only call this from a Method so GC can't bugger things
    // eg "HOHO".split("")
// or text.pump(List)
Instance *stringXplode(char *text, size_t max, pVM vm)
{
   Instance *result;
   size_t    i, len = strlen(text);
   Fence     fence;

   if (len > max) len = max;
   result = listCreate(len,0x01,I_OWNED,vm);
   vmSetFence(vm,&fence,0,result);
      for (i = 0; i < len; i++)
	 LIST_TABLE(result)[i] = stringCreate2(text++,1,vm);	// ShortString
      LIST_LEN(result) = (LSize_t)len;
   vmRemoveFence(&fence,0);
   return result;
}

    // .sum(initialValue=0), NOT thread safe
    // fcn sum(list,v=0) { return(list.reduce(Op("+"),v)); }
static Instance *List_sum(Instance *self,pArglist arglist, pVM vm)
{
   MLIST(mlist,2);
   Instance *pi = arglistTryToGet(arglist,0);

   if (!pi) pi = Zero;
   mlistBuild(mlist,self,pi,ZNIL);
   return fcnRunFromClass(Utils_Helpers,"sumList",(Instance *)mlist,vm);
}

    // .shuffle() --> self|new list
Instance *List_shuffle(Instance *self,pArglist arglist, pVM vm)
{
   MLIST(mlist,1);
   mlistBuild(mlist,self,ZNIL);
   return fcnRunFromClass(Utils_Helpers,"shuffle",(Instance *)mlist,vm);
}

//__inline long min(long a,long b){ return (a<b) ? a : b; }
//__inline long max(long a,long b){ return (a>b) ? a : b; }

    // List.reverse(a=0,b=*), thread safe -->self
static Instance *List_reverse(Instance *self,pArglist arglist, pVM vm)
{
   int    lockIt = LOCK_IT(self);
#if 0
   size_t i,j,len;
   if (lockIt) writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
      len = LIST_LEN(self);
      if (len > 1)
	 for (i = 0, j = len - 1; i < j; i++,j--)
	 {
	    Instance *tmp       = LIST_TABLE(self)[i]; 
	    LIST_TABLE(self)[i] = LIST_TABLE(self)[j];
	    LIST_TABLE(self)[j] = tmp;
	 }
#else
#if 0
   long i,j,len;
   int64_t a=0,b;  // b is count
   int     B=arglistTryToGetInt(arglist,1,&b,0,vm);
   arglistTryToGetInt(arglist,0,&a,0,vm);

   if (lockIt) writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
      len = LIST_LEN(self); 
      i=(size_t)max(0,a); 
      if(!B) j=len-1; else j=min(len-1,(size_t)b+i-1);
      for (; i < j; i++,j--)
      {
	 Instance *tmp       = LIST_TABLE(self)[i]; 
	 LIST_TABLE(self)[i] = LIST_TABLE(self)[j];
	 LIST_TABLE(self)[j] = tmp;
      }
#else
   size_t i,j;
   if (lockIt) writeLockAcquire(LIST_LOCK(self),WL_WRITER,vm);
      arglistMaybeOL(arglist,0, LIST_LEN(self), &i,&j,vm);
      if(j>1)
	 for (j+=i-1; i < j; i++,j--)
	 {
	    Instance *tmp       = LIST_TABLE(self)[i]; 
	    LIST_TABLE(self)[i] = LIST_TABLE(self)[j];
	    LIST_TABLE(self)[j] = tmp;
	 }
#endif
#endif
   if (lockIt) writeLockRelease(LIST_LOCK(self),WL_WRITER);
   return self;
}

    // arglist.calcChunk(i,max,[flags]) -->L(offset,length)
    // self is the arglist
    // eg L(1,20).calcChunk(0,5) -->L(1,4)
static Instance *List_calcChunk(Instance *self,pArglist arglist, pVM vm)
{
   int      i     = (int)   arglistGetInt(arglist,0,".calcChunk",vm);
   size_t   max   = (size_t)arglistGetInt(arglist,1,".calcChunk",vm);
   int64_t  flags = 0;
   size_t   offset,length;

   arglistTryToGetInt(arglist,2,&flags,0,vm);
   arglistGetChunk(self,i,(unsigned)flags,max, &offset,&length, 0,vm);

   return tupleCreateX(vm, intCreate(offset,vm), intCreate(length,vm),ZNIL);
}

    // List.makeReadOnly() -->Tuple
    // NO PtrInts!
Instance *List_makeReadOnly(Instance *self,pArglist arglist, pVM vm)
{
   int       lockIt;
   Instance *r;

if (TYPEO1(self) == TupleType && IS_IMEM(self)) printf("ICK\n");
   if (TYPEO1(self) == TupleType) return self;
   if (LIST_LEN(self) == 0) return emptyTuple;

   lockIt = LOCK_IT(self);
   if (lockIt) writeLockAcquire(LIST_LOCK(self),WL_READER,vm);
      r = listToTuple(self,0,LIST_LEN(self),vm);
   if (lockIt) writeLockRelease(LIST_LOCK(self),WL_READER);
   return r;
}

   // List.zip(obj,obj...)
   // --> Utils.Helpers.stringZipWith(T,self,obj,...)
   // T.zip(...) --> Utils.zip(arglist.xplode())
static Instance *List_zip(Instance *self,pArglist arglist,pVM vm)
{ 
   if(self==emptyTuple) self=0;
   return zipObjs(self,emptyList,arglist,vm);   // util.c
}


static const MethodTable listMethods[] =
{
   "create",		List_create,
   "createLong",	List_createBig,
   "copy",		List_copy,
   "toString",		List_toString,
   "toBool",		List_toBool,
   "toData",		List_toData,
   "toDictionary",	List_toDictionary,
   "toList",		Object_noop,
   "isType",		List_isType,
   "isInstanceOf",	List_isType,

   "holds",		List_holds,
   "append",		List_append,
   "push",		List_append,
   "write",		List_append,	// Stream
   "writeln",		List_append,	// Stream
   "read",		List_read,	// Stream
   "readln",		List_read,	// Stream
   "extend",		List_extend,
   "insert",		(pMethod)List_insert,
   "find",		List_find,
   "index",		List_index,
   "__sGet",		(pMethod)List_sGet,
   "get",		(pMethod)List_sGet,
   "tail",		List_tail,
   "__sSet",		List_sSet,
   "set",		List_set,
   "len",		List_len,
   "clear",		List_clear,
   "pop",		(pMethod)List_pop,
   "remove",		List_remove,
   "removeEach",	List_removeEach,
   "swap",		List_swap,
   "xplode",		(pMethod)List_xplode,
   "del",		(pMethod)List_del,
   "flatten",		List_flatten,
   "makeReadOnly",	List_makeReadOnly,

   "close",		Object_noop,	// Stream
   "flush",		Object_noop,	// Stream

   "concat",		List_concat,
   "sum",		List_sum,
   "shuffle",		List_shuffle,
   "reverse",		List_reverse,
   "sort",		Sequence_sort,
   "walker",		List_walker,
   "callProperty",	Sequence_callProperty,
   "callMethod",	Sequence_callMethod,
   "filter",		List_filter,
   "filter1",		List_filter1,
   "filter1n",		List_filter1n,
   "filterNs",		List_filterNs,
   "filter22",		List_filter22,
   "runNFilter",	runNFilter,
   "run",		List_run,
   "apply",		List_apply,
   "apply2",		List_apply2,
   "reduce",		List_reduce,
   "pump",		List_pump,
   "enumerate",		List_enumerate,
   "zip",		List_zip,
   "zipWith",		zipWithObjs,
   "merge",		List_merge,

   "calcChunk",		List_calcChunk,
   "pad",		List_pad,
   0,			0
};

/* ******************************************************************** */
/* **************************** Properties **************************** */
/* ******************************************************************** */

    // Thread safe
    // --> bytes
size_t listSize(Instance *self,pVM vm)	// sizeof(header) + sizeof(items)
{
   size_t size;

   if (!LOCK_IT(self)) return itSize(&ITABLE(self));

   writeLockAcquire(LIST_LOCK(self),WL_READER,vm);
      size = itSize(&ITABLE(self));
   writeLockRelease(LIST_LOCK(self),WL_READER);

   return size;
}

    // .size -->L(size of items, instance size)
static Instance *List_size(Instance *self,pVM vm)
{
   size_t size;

   if (IS_TUPLE1(self))
   {
      size = TUPLE_LEN(self) * sizeof(Instance *);
      return tupleCreateX(vm,
		intCreate(size,vm),intCreate(sizeof(ZKL_Tuple),vm), ZNIL);
   }

   size = listSize(self,vm) - sizeof(ITable);
   return
      tupleCreateX(vm,intCreate(size,vm),intCreate(sizeof(TSList),vm), ZNIL);
}

static Instance *List_isReadOnly(Instance *self,pVM vm)
{
   if (OBJECT1(self) == &TSListObject) return BoolFalse;
   if (OBJECT1(self) == &ListObject)   return BoolFalse;
   return BoolTrue;
}

static const PropertyTable listProperties[] =
{
   "size",		(pProperty)List_size,
   "isReadOnly",	(pProperty)List_isReadOnly,
   0,			0
};


/* ******************************************************************** */
/* ***************************** Op Codes ***************************** */
/* ******************************************************************** */

    //-->1: ==, 0: !=
static int _cmp(Instance *self,Instance **table,size_t n, Instance *X, pVM vm)
{
   Instance **tableX;
   size_t     nX;

   if (self == X) return 1;

   switch(TYPEO(X))
   {
 //!!!!???? do I need to lock X?
      case ListType:  tableX = LIST_TABLE(X);  nX = LIST_LEN(X);  break;
      case TupleType: tableX = TUPLE_TABLE(X); nX = TUPLE_LEN(X); break;
      default: return 0;
   }

   if (n != nX) return 0;
   while(n--)
   {
      Instance *a = *table++, *b = *tableX++;
      pOp 	opEqual;
      #if USE_POINTER_INTS
	 ZKL_Int i64;
      #endif

      if (a == b) continue;

      #if USE_POINTER_INTS
         a = decantInt(a,&i64);
      #endif

      opEqual = I_OP(a,OP_EQ);
      if (opEqual(a,b,vm) == BoolFalse)	// this will recurse for lists
         return 0;

//      if (!_cmpTypes(a,b,vm)) return 0;
   }
   return 1;
}

    // if can't pull an list, they ain't equal
static Instance *List_eq(Instance *self,Instance *X,pVM vm)
   { return boolCreate(_cmp(self,LIST_TABLE(self),LIST_LEN(self),X,vm)); }

static Instance *List_neq(Instance *self,Instance *X,pVM vm)
   { return boolCreate(!_cmp(self,LIST_TABLE(self),LIST_LEN(self),X,vm)); }

static Instance *List_add(Instance *self,Instance *X,pVM vm)
{
   listAppend(self,X,vm);
   return self;
}

static Instance *List_sub(TSList *self,Instance *X,pVM vm)
{
   listRemove((Instance *)self,X,vm);
   return (Instance *)self;
}

static Instance *List_mul(Instance *self,Instance *X,pVM vm)
{
   int64_t N;
   if(iWantInt(X,&N,vm) && (N==0 || N>1))
   {
      int       n;
      Instance *r;
      Fence     fence;
      if(N==0) return emptyTuple;
      r=listCreate((size_t)N,THE_DEFAULT,I_OWNED,vm);
      vmSetFence(vm,&fence,0,r);
         for(n=0; n<N; n++) listAppend(r,listCopy(self,vm),vm);
      vmRemoveFence(&fence,0);
      return r;
   }
   return (Instance *)self;
}

//!!!??? < --> self[0]<X[0]???

static const OpcodeTable opcodeTable[] =
{
   OP_EQ,	(pOp)List_eq,
   OP_NEQ,	(pOp)List_neq,

   OP_ADD,	(pOp)List_add,
   OP_SUB,	(pOp)List_sub,
   OP_MUL,	(pOp)List_mul,
   0,		0
};


/* ******************************************************************** */
/* *********************** ROList# : Fake Tuple *********************** */
/* ******************************************************************** */

// Looks like a List, acts like a Tuple

static const MethodTable roMethods[] =	// and tuple partitions
{
   "create",		Tuple_create,
   "append",		Tuple_append,
   "extend",		Tuple_extend,
   "flatten",		Tuple_flatten,
   "copy",		List_copy,
   "toString",		List_toString,
   "toBool",		List_toBool,
   "toData",		List_toData,
   "toDictionary",	List_toDictionary,
   "isType",		List_isType,
   "isInstanceOf",	List_isType,

   "holds",		List_holds,
   "find",		List_find,
   "index",		List_index,
   "__sGet",		(pMethod)List_sGet,
   "get",		(pMethod)List_sGet,
   "tail",		List_tail,
   "set",		Tuple_set,
    "len",		List_len,
   "xplode",		(pMethod)List_xplode,
   "reverse",		Tuple_reverse,

   "close",		Object_noop,	// Stream
   "flush",		Object_noop,	// Stream

   "apply",		List_apply,
   "apply2",		List_apply2,
   "filter",		List_filter,
   "filter1",		List_filter1,
   "filter1n",		List_filter1n,
   "filter22",		List_filter22,
   "filterNs",		List_filterNs,
   "reduce",		List_reduce,
   "pump",		List_pump,
   "runNFilter",	runNFilter,
   "run",		List_run,
   "concat",		List_concat,
   "sum",		List_sum,
   "shuffle",		List_shuffle,
   "walker",		List_walker,
   "callProperty",	Sequence_callProperty,
   "callMethod",	Sequence_callMethod,
   "sort",		Sequence_sort,
   "enumerate",		List_enumerate,
   "zip",		List_zip,
   "zipWith",		zipWithObjs,

   "calcChunk",		List_calcChunk,
   "makeReadOnly",	Object_noop,
   0,			0
};

static Instance *TP_add(Instance *self,Instance *X,pVM vm)
   { return tupleAdd(self,X,LIST_LEN(self),LIST_TABLE(self),vm); }

static const OpcodeTable roOps[] =
{
   OP_EQ,	(pOp)List_eq,
   OP_NEQ,	(pOp)List_neq,
   OP_ADD,	(pOp)TP_add,
   0,		0
};

void tupleConstruct(void);

/////////////////////// List toSink
#define ZA_LBUF_SZ	300
typedef struct{ MLIST(buf,ZA_LBUF_SZ); MLIST(sav,2); } SList;  // 2464
   // buf is write buffer; sav is (overflow,buf) and fenced

static int listSinkWrite(void *X,Instance *i,pVM vm)
{
   SList *sl = (SList *)X;
   mlistAppendI(sl->buf,i,ZA_LBUF_SZ);  // fenced in sav[1]
   if (TUPLE_LEN(sl->buf) == ZA_LBUF_SZ)
   {  // internal buffer full, append (overflow,buf) to (), ie create tuple
      TUPLE_TABLE(sl->sav)[0] = Tuple_extend(emptyTuple,(Instance *)sl->sav,vm);
      mlistCopy(sl->buf,emptyTuple,0,ZA_LBUF_SZ);  // clear internal buffer
   }
   return 1;
}
static Instance *listSinkClose(void *X,pVM vm)	// soft close
{
   SList *sl = (SList *)X;
   // sav[0] is [empty]Tuple, sav[1] is MList
   if (0 == TUPLE_LEN(sl->buf)) return TUPLE_TABLE(sl->sav)[0];
   return Tuple_extend(emptyTuple,(Instance *)sl->sav,vm);
}

static int listToSink(
   Instance *self,void *_aggie,size_t _,size_t sbsz,size_t *sbused, pVM vm)
{
   ZAgg *aggie = (ZAgg *)_aggie;

      // List() --> append
   if((self != VList && self != VTSList) && TYPEO1(self)==ListType)
   {
      if(!aggie) return ZA_2SINK_OK;	// sbUsed query

      aggie->how = ZA_2M; aggie->i = self;
      aggie->dst.msink.write = List_append;
      aggie->dst.msink.close = Object_noop;

      return ZA_2SINK_OK;
   }
   else // shove through a buffer
   {
      SList *sl;

      if(!aggie){ *sbused=sizeof(SList); return ZA_2SINK_OK; } // sbUsed query

      if(sbsz < sizeof(SList)) return ZA_SANDBOX_TOO_SMALL;

      aggie->how = ZA_C;

      sl = (SList *)&aggie->sandbox;	// address of SList to be
      mlistCopy(sl->buf,emptyTuple,0,ZA_LBUF_SZ);
      mlistBuild(sl->sav, arglistDup(self,0,vm), (Instance *)sl->buf, ZNIL);
      aggie->dst.csink.X     = sl;
      aggie->dst.csink.write = listSinkWrite;
      aggie->dst.csink.close = listSinkClose;
      aggie->i		     = (Instance *)sl->sav;
      *sbused		     = sizeof(SList);

      return ZA_2SINK_OK;
   }
}

    /* Create a place holder in the TheVault so List can be found.
     * Don't need a GC marker because TheVault will mark it
     */
//static pMethod in_list_methods(  Instance *ignore, register char *str);
//static pMethod in_ro_methods(Instance *ignore, register char *str);

void listConstruct(void)
{
   if (MAX_Is < MAX_LIST_LENGTH) maxListLength = MAX_Is;

   constructObject(&ListObject,ListType,	// not thread safe, nonlocking
		   listMethods,listProperties,opcodeTable, NoVM);
   ListObject.freeMe	   = listFree;
//   ListObject.methodSearch = in_list_methods;
   ListObject.isize	   = sizeof(List);
   ListObject.threadSafe   = 0;
   ListObject.isBInstance  = 1;
   ListObject.toSink	   = listToSink;
   ListObject.createReturnsSelf = 1;

   TSListObject		     = ListObject;	// thread safe, locking
   TSListObject.name	     = "TSList";
   TSListObject.isize	     = sizeof(TSList);
   TSListObject.threadSafe   = 1;
   TSListObject.createReturnsSelf = 1;
   registerObject(&TSListObject,NoVM);
   tsListID = TSListObject.id;

   	// ROList#/TP
   constructObject(&ROObject,ListType,	// read only, nonlocking
		   roMethods,listProperties,roOps, NoVM);
   ROObject.name	 = "ROList#";		// for internal use
//   ROObject.methodSearch = in_ro_methods;
   ROObject.freeMe	 = listFree;
   ROObject.isize	 = sizeof(TSList);	// maybe, maybe not
   ROObject.threadSafe   = 1;
   ROObject.isBInstance  = 1;
   ROObject.toSink	 = listToSink;
   roListID = ROObject.id;

   #if !STAY_AT_HOME_MARKER
      ListObject.magicMarker   = listMarker;
      ROObject.magicMarker     = listMarker;
      TSListObject.magicMarker = tslistMarker;
   #else
      ListObject.magicMarker   = _markListContents;
      ROObject.magicMarker     = _markListContents;
      TSListObject.magicMarker = _markListContents;
   #endif

   ibucketReserve(&TSListObject,10001,&tsListBuckets,0,NoVM);
   ibucketPoach(&tsListBuckets,&ListObject,NoVM);
   ibucketPoach(&tsListBuckets,&ROObject,NoVM);

      // VList is visible in case some bozo adds to it, don't want corruption
   VList = listCreate(0,THE_DEFAULT,I_IMMORTAL,NoVM);
   vaultAddData("List",VList,NoVM);	// might be "List" or "TSList"
   vaultAddData("L",   VList,NoVM);	// syntactic sugar

   VTSList = listCreate(0,0x1,I_IMMORTAL,NoVM);
   vaultAddData("Thread.List",VTSList,NoVM);

   tupleConstruct();
}


/* ******************************************************************** */
/* ************** MList : In Memory Arg Lists (Tuples) **************** */
/* ******************************************************************** */

#define SIZEOF_MLIST MLIST_SIZE(0)

    /* A MList "prototype", ie a empty MList. Don't let anyone use this
     * directly, it will get locked while memcpy is happening, which is bad.
     */
//static Byte mlistProto[MLIST_SIZE(0)];
static MList mlistProto;

static Instance *initializeMList(Byte *mlist)	// only called once, for proto
{
   memset(mlist,0,MLIST_SIZE(0));
   instanceInit((Instance *)mlist,&MListObject,I_2SPECIAL);
   TUPLE_LEN(mlist) = 0;

   return (Instance *)mlist;
}

    // dst <-- srcList[offset], srcList can be List or Tuple
Instance *mlistCopy(Byte *dst, Instance *srcList, size_t srcOffset,unsigned sz)
{
   LSize_t    srcLen;
   Instance **srcTable = listTable(srcList,&srcLen);

   *(MList *)dst = mlistProto;		// struct copy

   if (srcLen <= srcOffset) srcLen = 0;
   else
   {
      srcLen -= (LSize_t)srcOffset;
      if(srcLen >= (unsigned)sz) srcLen = sz;
      memcpy(TUPLE_TABLE(dst),&srcTable[srcOffset],srcLen*sizeof(Instance *));
   }
   TUPLE_LEN(dst) = srcLen;

   return (Instance *)dst;
}

#if 0
   // dst is already initialized!
Instance *mlistCopy2(Byte *dst, int dstO, Instance *srcList, int srcO)
{
   Instance **srcTable;
   LSize_t    srcLen;

   if (IS_LIST1(srcList))
   {
      srcTable = LIST_TABLE(srcList);
      srcLen   = LIST_LEN(srcList);
   }
   else
   {
      srcTable = TUPLE_TABLE(srcList);
      srcLen   = TUPLE_LEN(srcList);
   }
   if (srcLen <= srcO) srcLen = 0;
   else
   {
      srcLen -= (LSize_t)srcO;
      memcpy(TUPLE_TABLE(dst)+dstO,&srcTable[srcO],srcLen*sizeof(Instance *));
   }
   TUPLE_LEN(dst) = dstO + srcLen;

   return (Instance *)dst;
}
#endif

    // Append the contents of a List/Tuple to an already initialized MList
Instance *
mlistExtendN(Byte *dst, Instance *srcList, size_t srcOffset, unsigned sz)
{
   LSize_t    dlen = TUPLE_LEN(dst);
   unsigned   n;
   Instance **srcTable = listTable(srcList,&n);

   if (srcOffset < n)	// only do work is if there are items to copy
   {
      n -= (LSize_t)srcOffset;
      if(n+dlen > sz) n = sz - dlen;
      memcpy(&TUPLE_TABLE(dst)[dlen], &srcTable[srcOffset],
	     n * sizeof(Instance *));
      TUPLE_LEN(dst) = n + dlen;
   }

   return (Instance *)dst;
}

    // Append the contents of a List/Tuple to an already initialized MList
    // You know how big you created mlist, don't overflow
Instance *
mlistExtend(Byte *dst, Instance *srcList, size_t srcOffset, unsigned sz)
   { return mlistExtendN(dst,srcList,srcOffset,sz); }

    // Append an instance to an already initialized MList
Instance *mlistAppendI(Byte *mlist, Instance *i, unsigned sz)
{
   LSize_t n = TUPLE_LEN(mlist);
   if(n<sz)
   {
      TUPLE_TABLE(mlist)[n] = i;
      TUPLE_LEN(mlist)      = n + 1;
   }
else
printf("POOT %d %d\n",n,sz);
   return (Instance *)mlist;
}

Instance *mlistBuild(Byte *mlist, Instance *x,...)
{
   va_list    ap;		// argument list pointer
   int	      n;
   Instance **table;

//   memcpy(mlist,mlistProto,SIZEOF_MLIST);
   *(MList *)mlist = mlistProto;	// struct copy

   table = TUPLE_TABLE(mlist);
   va_start(ap,x);
      for (n = 0; x; x = va_arg(ap, Instance *),n++) *table++ = x;
   va_end(ap);
   TUPLE_LEN(mlist) = n;

   return (Instance *)mlist;
}

    // Build a MList Tuple from two source Tuples using a bit mask.
    // If mask bit is 0 use source 0, else srouce 1
Instance *mlistBuildFromMask(Byte *mlist, unsigned maskLen, 
unsigned mask,unsigned chop, Instance *src0,Instance *src1, unsigned sz, pVM vm)
{
   int	      bit = 1 << (maskLen-1); // len==3, bit-->4, eg mask of 001 or 101
   LSize_t    z0 = TUPLE_LEN(src0), z1 = TUPLE_LEN(src1);
   LSize_t    n0,n1,nd;
   Instance **td, **t0=TUPLE_TABLE(src0), **t1=TUPLE_TABLE(src1);

   *(MList *)mlist = mlistProto;	// struct copy --> emptyTuple
   if (chop == 0) return (Instance *)mlist;
   td = TUPLE_TABLE(mlist);
   for(n0=n1=nd=0; bit; nd++, bit >>= 1)
   {
      if (mask & bit)
      {
	 if (n1 == z1) _missingArg(src1,n1,".fpM from create",vm);
	 td[nd] = t1[n1++];
      }
      else
      {
	 if (n0 == z0) _missingArg(src0,n0,".fpM from call",vm);
	 td[nd] = t0[n0++];
      }
   }
   TUPLE_LEN(mlist) = nd;
   if (n0 < z0) mlistExtend(mlist,src0,n0,sz);
if (chop < TUPLE_LEN(mlist)) TUPLE_LEN(mlist) = chop;
   return (Instance *)mlist;
}

/* ******************************************************************** */
/* ****************************** Tuples ****************************** */
/* ******************************************************************** */

#if !STAY_AT_HOME_MARKER
    // !!!Problem?: Two cores mark the same Tuple at the same time
static void tupleMarker(ZKL_Tuple *self)
{
   LSize_t    n     = self->n;
   Instance **table = self->table;
   while (n--) instanceMark(*table++);
}
#endif

    // will a n item Tuple fit in a TSList? (3 items: (24-12)/4)
#define STUBBY(n)	\
    ( n <= (sizeof(TSList) - sizeof(ZKL_Tuple)) / sizeof(Instance *) )

Instance *tupleCreate(size_t n,int itype,pVM vm)
{
   ZKL_Tuple *tuple;
   size_t     size = sizeof(ZKL_Tuple) + n*sizeof(Instance *);
   IBucketHeader *bh;

   if (0 == n) return emptyTuple;	// it happens

   if (n > maxListLength) vmThrow(vm,E_VALUE_ERROR,
	"A list that long would streach to the moon and back");

#if 1
   tuple = (ZKL_Tuple *)iHazBucket(&CuckooTupleObject,
	     size, 4*sizeof(Instance *),
	     itype,&bh,vm);
   if (tuple)	// 1-8 on /64, (1-3, 5-9, 11-15) /32
   {
      tuple->n = 0;
      containerIsCooked(bh,(Instance *)tuple,itype);
   }
#else
   if (STUBBY(n))
   {
      tuple = (ZKL_Tuple *)
	      ibucketAllocate(&tsListBuckets,&CuckooTupleObject,itype,1,vm);
      tuple->n = 0;
      containerIsCooked(&tsListBuckets,(Instance *)tuple,itype);
   }
#endif
   else
   {
      tuple = (ZKL_Tuple *)instanceAllocate(size,&TupleObject,1,vm);
      tuple->n = 0;
      addToCollectables((Instance *)tuple,itype,vm);
   }

   return (Instance *)tuple;
}

    // Create a Tuple from a table of Instances
Instance *tableToTuple(Instance **table,size_t n,pVM vm)
{
   Instance *t;
   if (0 == n) return emptyTuple;
   t = tupleCreate(n,I_OWNED,vm);	// --> emptyTuple if n == 0
   memcpy(TUPLE_TABLE(t),table,n*sizeof(Instance *));
   TUPLE_LEN(t) = (LSize_t)n;
   return t;
}

    // List[offset,n] -->Tuple, NOT thread safe, owned
    // Works for Lists, MLists, makes a copy of a Tuple, ick!!!
Instance *listToTuple(Instance *list,size_t offset,size_t n,pVM vm)
{
   Instance *tuple, **ptr;

   if (0 == n) return emptyTuple;	// cheap
   ptr = IS_TUPLE1(list) ? &TUPLE_TABLE(list)[offset] : &LIST_TABLE(list)[offset];

   tuple = tupleCreate(n,I_OWNED,vm);	// --> emptyTuple if n == 0
   memcpy(TUPLE_TABLE(tuple),ptr, n*sizeof(Instance *));
   TUPLE_LEN(tuple) = (LSize_t)n;
   return tuple;
}

    // src is a Tuple or a container with a *static* array of Instances
Instance *partitionTuple(ZKL_Tuple *src,Instance **start,size_t num,pVM vm)
{
   TP *tp = (TP *)ibucketAllocate(&tsListBuckets,&ROObject,I_OWNED,1,vm);
   itInit(&tp->table);
   tp->instance.iflag = 1;
   tp->table.numItems = tp->table.maxItems = (LSize_t)num;
   tp->table.table    = start;
   tp->src	      = src;
if (TYPEO1(src) != TupleType) tp->instance.iflag2 = 1;
   return containerIsCooked(&tsListBuckets,(Instance *)tp,I_OWNED);
}

    /* ONLY call this in conjunction with tupleCreate() in a Method or
     * Property. Otherwise, GC can bite you on the ass.
     * And you had better have created the the tuple with enough room.
     */
void tupleAppend(Instance *self,Instance *i)
{
   LSize_t n = TUPLE_LEN(self);
   TUPLE_TABLE(self)[n] = i;
   TUPLE_LEN(self)      = n + 1;
}

    // Tuple.extend(...) -->Tuple, ditto on TP
    // Tuple.append == T.extend(T(self))
    // ONE allocation
//!!!???? T(self,arglist).pump(List,fcn(i){i.isType(List) and return(Void.Write,i); i})
Instance *Tuple_extend(Instance *self,pArglist arglist,pVM vm)
{
   unsigned  n,N, numArgs;
   Instance *nt, **args, **table, **myTable, **argTable;
   size_t    size;

   argTable = listTable(arglist,&numArgs);

   for(args = argTable, n = numArgs, size = 0; n--; )
   {
      Instance *i = *args++;
      switch(TYPEO(i))
      {
	 case ListType:  size += LIST_LEN(i);  break;
	 case TupleType: size += TUPLE_LEN(i); break;
	 default:	 size++;	       break;
      }
   }

   if (!size) return self;	// T(1,2,3).extend()

   if (IS_TUPLE1(self)) { n = TUPLE_LEN(self); myTable = TUPLE_TABLE(self); }
   else       /* TP */  { n = LIST_LEN(self);  myTable = LIST_TABLE(self);  }
   size += n;
   nt = tupleCreate(size,I_OWNED,vm);

   table = TUPLE_TABLE(nt);
   memcpy(table,myTable,n*sizeof(Instance *));
   table += n;
   for(args = argTable, N = numArgs; N--; )
   {
      Instance *i = *args++;
      switch(TYPEO(i))
      {
	 case ListType:
	    n = LIST_LEN(i);	// !!!not thread safe
	    memcpy(table,LIST_TABLE(i),n*sizeof(Instance *));
	    table += n;
	    break;
	 case TupleType:
	    n = TUPLE_LEN(i);
	    memcpy(table,TUPLE_TABLE(i),n*sizeof(Instance *));
	    table += n;
	    break;
	 default: *table++ = i; break;
      }
   } // for

   TUPLE_LEN(nt) = (LSize_t)size;
   return nt;
}

Instance *Tuple_append(Instance *self,pArglist arglist,pVM vm)
{
   MLIST(mlist,1);
   Instance *args = mlistBuild(mlist,arglist,ZNIL);
   return Tuple_extend(self,args,vm);
}

    // T(a,b,c).flatten() --> T.extend(self) or T.extend(list.xplode())
//!!!???? self.pump(List,fcn(i){if(T.isType(i))return(Void.Write,i); i})
static Instance *Tuple_flatten(Instance *self,pArglist arglist,pVM vm)
   { return Tuple_extend(emptyTuple,self,vm); }

Instance *tupleCreateX(pVM vm,Instance *x,...)
{
   va_list	ap;		// argument list pointer
   int		n;
   Instance    *list, **table;

   va_start(ap,x);
      for (n = 1; va_arg(ap, Instance *); ) n++;
   va_end(ap);			// because tupleCreate() can throw
   list  = tupleCreate(n,I_OWNED,vm);
   table = TUPLE_TABLE(list);

   va_start(ap,x);
      for (; x; x = va_arg(ap, Instance *)) *table++ = x;
   va_end(ap);
   TUPLE_LEN(list) = n;

   return list;
}

static void
_tupleToString(Instance *self, size_t maxLen,int depth, ZBText *buf,pVM vm)
{
   size_t     i, len = TUPLE_LEN(self);
   Instance **table = ((ZKL_Tuple *)self)->table;

   if (len == 0) { zbtextAppendN(buf,"L()",3,vm); return; }

   zbtextAppendN(buf,"L(",2,vm);
   _quote(table[0],maxLen,depth,buf,vm);

   for (i = 1; i < len; i++)
   {
      if (i >= maxLen)
      {
	 zbtextAppendN(buf,",...",4,vm);
	 break;
      }
      zbtextAppendN(buf,",",1,vm);
      _quote(table[i],maxLen,depth,buf,vm);
   }
   zbtextAppendN(buf,")",1,vm);
}

static Instance *
tupleToString(Instance *self, size_t maxLen,int depth,pVM vm)
{
   Fence  fence;
   ZBText buf;

   vmSetFence(vm,&fence,0,zbtextInit(&buf,vm));
      _tupleToString(self,maxLen,depth,&buf,vm);
      zbtextClose(&buf,vm);
   vmRemoveFence(&fence,0);
   return buf.string;
}

    // Tuple.create([contents]), thread safe (assuming arglist is)
Instance *Tuple_create(Instance *self,pArglist arglist,pVM vm)
{
   LSize_t	n = TUPLE_LEN(arglist);
   ZKL_Tuple   *tuple;
   Instance   **aTable = TUPLE_TABLE(arglist);

   if (n == 0) return emptyTuple;

   tuple    = (ZKL_Tuple *)tupleCreate(n,I_OWNED,vm);
   tuple->n = n;
   memcpy(tuple->table,aTable,n*sizeof(Instance *));
   return (Instance *)tuple;
}

#if 0
     // Copy a Tuple to a List
static Instance *tupleCopyToList(Instance *self,int threadSafe,pVM vm)
{
   LSize_t   n    = TUPLE_LEN(self);
   Instance *list = listCreate(n,threadSafe,I_OWNED,vm);
   memcpy(LIST_TABLE(list),TUPLE_TABLE(self), n * sizeof(Instance *));
   LIST_LEN(list) = n;
   return list;
}
#endif

    // Tuple.copy()  -->[TS]List, make a writable copy
static Instance *Tuple_copy(Instance *self,pArglist arglist,pVM vm)
{
   LSize_t   n    = TUPLE_LEN(self);
   Instance *list = listCreate(n,THE_DEFAULT,I_OWNED,vm);

   memcpy(LIST_TABLE(list),TUPLE_TABLE(self), n * sizeof(Instance *));
   LIST_LEN(list) = n;
   return list;
}

    // offset==0: Mlist-->Typle, Tuple-->self, List-->Tuple
    // else -->new Tuple
Instance *arglistDup(pArglist arglist,unsigned offset,pVM vm)
{
   LSize_t   n;
   Instance *tuple, **args;

//???BAD   if (IS_LIST(arglist)) listToTuple(arglist,0,LIST_LEN(arglist),vm);
   if (offset==0 && !IS_IMEM(arglist) && IS_TUPLE1(arglist)) return arglist;

   args = listTable(arglist,&n);
   if (offset >= n) return NoArglist;
   n -= offset;
   	// Mlist --> copy bits as they are sitting on the C stack
   tuple = tupleCreate(n,I_OWNED,vm);
   memcpy(TUPLE_TABLE(tuple),args + offset, n*sizeof(Instance *));
   TUPLE_LEN(tuple) = n;
   return tuple;
}

#if 0
Instance *tupleMakeGCSafe(Instance *tpl,pVM vm)
{
   LSize_t   n = TUPLE_LEN(tpl), z;
   Instance *table = TUPLE_TABLE(tpl), *nt, *ntt;

   for (z = 0; z < n; z++)
      if (IS_CONTAINER(table[z] && !OBJECT(table[z])->threadSafe) break;
   if (z == n) return arglistDup(tpl,vm);	// gc safe unless MList

   nt  = tupleCreate(n,I_OWNED,vm);
   ntt = TUPLE_TABLE(nt);
   vmSetFence(vm,&fence,0,nt);
      for (z = 0; z < n; z++)
      {
	 ntt[z] = IS_CONTAINER(table[z] ? makeGCSafe(table[z],vm) : table[z];
	 TUPLE_LEN(nt)++;
      }
   vmRemoveFence(&fence,0);
   return nt;
}
#endif

    // Tuple.toBool(), thread safe
static Instance *Tuple_toBool(ZKL_Tuple *self,pArglist arglist,pVM vm)
{
   if (self->n) return BoolTrue;
   return BoolFalse;
}

    // _tfind(x,offset=0,len=*), thread safe
static int _tfind(Instance *self,Instance *arglist, size_t *idx, pVM vm)
{
   int	      numArgs = TUPLE_LEN(arglist);
   size_t     offset  = 0, num;
   Instance  *x	= arglistGet(arglist,0,0,vm);
   Instance **table;
   pOp	      opEqual;

   #if USE_POINTER_INTS
      ZKL_Int i64;
      x = decantInt(x,&i64);
   #endif
   opEqual = I_OP(x,OP_EQ);

   if (numArgs != 1)		// .find(x,offset), .find(x,offset,len)
      arglistGetChunk(arglist,1,0x103, TUPLE_LEN(self), &offset,&num, 0,vm);
   else num = TUPLE_LEN(self);

   for (table = &TUPLE_TABLE(self)[offset]; num--; offset++)
   {
      Instance *y = *table++;
      if (opEqual(x,y,vm) == BoolTrue && _cmpTypes(y,x,vm)) 
      {
	 *idx = offset;
	 return 1;
      }
   }
   return 0;
}

    // Tuple.__sGet(), [], thread safe
static Instance *Tuple_sGet(ZKL_Tuple *self,pArglist arglist,pVM vm)
{
   int	      s;
   size_t     offset, num;
   Instance  *result;
   ZKL_Tuple *t;

   s = arglistGetChunk(arglist,0,0x100, self->n, &offset,&num, "List[]",vm);

   if (s < 2)	// [offset]
   {
      if (num) result = self->table[offset];
      else result = emptyTuple;	// 2 args & out of range, eg [*,100], [0,0]
      return result;
   }

   if (!num) return emptyTuple;
   if (num == TUPLE_LEN(self)) return (Instance *)self;

   if (!STUBBY(num) && sizeof(TP) <= sizeof(TSList))   // won't be a TP
      return partitionTuple(self,self->table+offset,num,vm);

   t = (ZKL_Tuple *)tupleCreate(num,I_OWNED,vm);
   memcpy(t->table,&self->table[offset],num*sizeof(Instance *));
   t->n = (LSize_t)num;

   return (Instance *)t;
}

#if 0
    // Tuple.get(n), thread safe --> self[i]
static Instance *Tuple_get(Instance *self,pArglist arglist,pVM vm)
{
   Instance **table;
   size_t    n;
   if (IS_TUPLE1(self))
   {
      n    = arglistGetIndex(arglist,0,TUPLE_LEN(self),"T.get",vm);
      table = TUPLE_TABLE(self);
   }
   else // ROList or TP
   {
      n     = arglistGetIndex(arglist,0,LIST_LEN(self),"T.get",vm);
      table = LIST_TABLE(self);
   }
   return table[n];
}
#endif

    // Tuple.set(n,v), thread safe -->T, creates a copy of self
static Instance *Tuple_set(Instance *self,pArglist arglist,pVM vm)
{
   Instance *t, *v = arglistGet(arglist,1,0,vm);
   size_t    n;
   if (IS_TUPLE1(self))
   {
      n = arglistGetIndex(arglist,0,TUPLE_LEN(self),"T.set",vm);
      t = tableToTuple(TUPLE_TABLE(self),TUPLE_LEN(self),vm);
   }
   else // ROList or TP
   {
      t = tableToTuple(LIST_TABLE(self),LIST_LEN(self),vm);
      n = arglistGetIndex(arglist,0,LIST_LEN(self),"T.set",vm);
   }
   TUPLE_TABLE(t)[n] = v;
   return t;
}

    // Tuple.len(), thread safe
static Instance *Tuple_len(ZKL_Tuple *self,pArglist arglist,pVM vm)
   { return INT_CREATE(self->n,vm); }

    // Tuple.xplode([offset=0,n=*])
static Instance *Tuple_xplode(ZKL_Tuple *self,Instance *arglist,pVM vm)
{
   size_t numArgs = TUPLE_LEN(arglist);
   size_t offset, num;

      	// n = T().xplode() is bogus
   if (!voidPlusOK(vm)) return Void;

   if (numArgs) arglistGetChunk(arglist,0,0x101, self->n, &offset,&num, 0,vm);
   else  { num = self->n; offset = 0; }		// .xplode()

   if (num)
   {
      size_t n;

      arglist = vmArglist2(vm);		// a List, not Tuple
      n       = LIST_LEN(arglist);
      itXpand(LISTV(arglist),num,10,100,0,vm);
      memcpy(&LIST_TABLE(arglist)[n],&self->table[offset],
		num * sizeof(Instance *));
   }
   return VoidPlus;		// out of band signal to opAddToArgs
}

    // Tuple.reverse(), thread safe
static Instance *Tuple_reverse(Instance *self,pArglist arglist, pVM vm)
{
   LSize_t    len;
   Instance **srcTable, **dstTable;
   if (IS_TUPLE1(self))
   {
      len = TUPLE_LEN(self);
      srcTable = &TUPLE_TABLE(self)[len - 1];
   }
   else		// ROList# or TP
   {
      len = LIST_LEN(self);
      srcTable = &LIST_TABLE(self)[len - 1];
   }

   if (len < 2) return self;
   self = tupleCreate(len,I_OWNED,vm); dstTable = TUPLE_TABLE(self);
   TUPLE_LEN(self) = len;
   while(len--) *dstTable++ = *srcTable--;

   return self;
}

static const MethodTable tupleMethods[] =
{
   "create",		Tuple_create,
   "append",		Tuple_append,
   "extend",		Tuple_extend,
   "flatten",		Tuple_flatten,
   "copy",		Tuple_copy,
   "toString",		List_toString,
   "toBool",		(pMethod)Tuple_toBool,
   "toData",		List_toData,
   "toDictionary",	List_toDictionary,
   "toList",		Object_noop,
   "isType",		List_isType,
   "isInstanceOf",	List_isType,

   "holds",		List_holds,
   "find",		List_find,
   "index",		List_index,
   "__sGet",		(pMethod)Tuple_sGet,
   "get",		(pMethod)Tuple_sGet,
   "tail",		List_tail,
   "set",		Tuple_set,
   "len",		(pMethod)Tuple_len,
   "xplode",		(pMethod)Tuple_xplode,
   "reverse",		Tuple_reverse,

   "close",		Object_noop,	// Stream
   "flush",		Object_noop,	// Stream

   "apply",		List_apply,
   "apply2",		List_apply2,
   "filter",		List_filter,
   "filter1",		List_filter1,
   "filter1n",		List_filter1n,
   "filter22",		List_filter22,
   "filterNs",		List_filterNs,
   "reduce",		List_reduce,
   "pump",		List_pump,
   "runNFilter",	runNFilter,
   "run",		List_run,
   "concat",		List_concat,
   "sum",		List_sum,
   "shuffle",		List_shuffle,
   "walker",		List_walker,
   "callProperty",	Sequence_callProperty,
   "callMethod",	Sequence_callMethod,
   "sort",		Sequence_sort,
   "enumerate",		List_enumerate,
   "zip",		List_zip,
   "zipWith",		zipWithObjs,

   "calcChunk",		List_calcChunk,
   "makeReadOnly",	Object_noop,
   0,			0
};


		//////////////////////////////////////// Tuple OPs
static Instance *Tuple_eq(Instance *self,Instance *X,pVM vm)
   { return boolCreate(_cmp(self,TUPLE_TABLE(self),TUPLE_LEN(self),X,vm)); }

static Instance *Tuple_neq(Instance *self,Instance *X,pVM vm)
   { return boolCreate(!_cmp(self,TUPLE_TABLE(self),TUPLE_LEN(self),X,vm)); }

    // T(a,b,c) + d -->T(a,b,c,d) aka extend
static Instance *
tupleAdd(Instance *self,Instance *x,size_t n,Instance **table,pVM vm)
{
   Instance  *tuple;

   tuple = tupleCreate(n+1,I_OWNED,vm);
   memcpy(TUPLE_TABLE(tuple),table,n*sizeof(Instance *));
   TUPLE_TABLE(tuple)[n] = x;
   TUPLE_LEN(tuple)      = (LSize_t)(n + 1);

   return tuple;
}

static Instance *Tuple_add(Instance *self,Instance *X,pVM vm)
   { return tupleAdd(self,X,TUPLE_LEN(self),TUPLE_TABLE(self),vm); }

static const OpcodeTable tupleOps[] =
{
   OP_EQ,	(pOp)Tuple_eq,
   OP_NEQ,	(pOp)Tuple_neq,
   OP_ADD,	(pOp)Tuple_add,
   0,		0
};

//static pMethod in_tuple_methods(Instance *ignore, register char *str);

void tupleConstruct(void)
{
   static ZKL_Tuple ATuple;

   constructObject(&TupleObject,TupleType,
   		   tupleMethods,listProperties,tupleOps, NoVM);
//   TupleObject.methodSearch = in_tuple_methods;
   TupleObject.isize	    = sizeof(ZKL_Tuple);
   TupleObject.threadSafe   = 1;
   TupleObject.toSink	    = listToSink;
   TupleObject.createReturnsSelf = 1;

   constructObject(&MListObject,TupleType,
   		   tupleMethods,listProperties,tupleOps,NoVM);
//   MListObject.methodSearch = in_tuple_methods;
   MListObject.freeMe	    = I_IS_IN_MEM;
   MListObject.name	    = "MemList";
   MListObject.threadSafe   = 1;
   MListObject.toSink	    = listToSink;
   initializeMList((Byte *)&mlistProto);

   #if !STAY_AT_HOME_MARKER
      TupleObject.magicMarker = tupleMarker;
      MListObject.magicMarker = tupleMarker;
   #else
      TupleObject.magicMarker = _markListContents;
      MListObject.magicMarker = _markListContents;
   #endif

   CuckooTupleObject	         = TupleObject;		// struct copy
   CuckooTupleObject.name        = "CuckooROList";
   CuckooTupleObject.isBInstance = 1;	// stuffed into an Instance
   registerObject(&CuckooTupleObject,NoVM);
   ibucketPoach(&tsListBuckets,&CuckooTupleObject,NoVM);

   emptyTuple = emptyList =
      instanceInit((Instance *)&ATuple,&TupleObject,I_UNTOUCHABLE); // don't GC
   ATuple.n = 0;
   vaultAdd("",emptyTuple,NoVM);
   vaultAddData("T",emptyTuple,NoVM);	// syntactic sugar
}


///////////////////////////////////////////////////////////////////
// zkl extractTable.zkl < list.c | gperf | zkl gperf.zkl -i list
// zkl extractTable -n roMethods < list.c | gperf | zkl gperf -i ro
// zkl extractTable -n tupleMethods  < list.c | gperf | zkl gperf -i tuple



///////////////////////////////////////////////
/////////////////////////////////////////////// RO List & tuple partitions
// zkl extractTable -n roMethods < list.c | gperf | zkl gperf -i ro


//////////////////////////////////////////////
////////////////////////////////////////////// Tuple
// zkl extractTable -n tupleMethods < list.c | gperf | zkl gperf -i tuple

