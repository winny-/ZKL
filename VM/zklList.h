/* list.h : header file for list stuff
 * 
 * Copyright (c) 2006,2007-13,2014 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#ifndef __ZKL_LIST_H
#define __ZKL_LIST_H

// #include "zklObject.h" implied here

    // About 30 bits for list length, 16 bits also work
typedef uint32_t LSize_t;

typedef struct	// Fixed size list, read only, create only, AKA ROList
{
   Instance  instance;	// Inherit from Instance
   LSize_t   n;
   Instance *table[0];
} ZKL_Tuple;	// /32: 12 bytes (a TSList can hold a 4 Instance Tuple)
		// /64: 24 bytes, TSList can hold len 1 Tuple

typedef ZKL_Tuple MList;

#define MLIST_SIZE(n) ( sizeof(MList) + (n*sizeof(Instance *)) )
#define MLIST(name,n) Byte name[MLIST_SIZE(n)] // n length Tuple, all garbage

#define ISA_LIST(t) ( (t) == ListType || (t) == TupleType )

#ifdef __LIST_INTERNALS

typedef struct
{
   LSize_t    numItems, maxItems;	// limited to 31 bits
   Instance **table;
} ITable;	// Instance Table, 12 bytes

   // List & TSList can have Object: TSListObject, ListObject or ROListObject
typedef struct	// Read/write list, not thread safe
{
   BInstance instance;   // Inherit from Instance
   ITable    table;
} List;		// 16 bytes

typedef struct	// Read/write list, thread safe, overlaps List
{
   BInstance instance;   // Inherit from Instance
   ITable    table;
   WriteLock lock;
} TSList;	// 24 bytes


#define LISTV(list)	( (TSList *)list )
#define LIST_TABLE(list)( LISTV(list)->table.table )
#define LIST_LEN(list)	( LISTV(list)->table.numItems )
#define LIST_LOCK(list)	( &LISTV(list)->lock )

#define TUPLEV(t)	( (ZKL_Tuple *)t )
#define TUPLE_LEN(t)	( TUPLEV(t)->n )
#define TUPLE_TABLE(t)	( TUPLEV(t)->table )

Instance **listTable(Instance *list,unsigned *sz);

#endif // __LIST_INTERNALS


#ifdef __cplusplus
extern "C" {
#endif

#ifdef __NOT_A_DLL

extern DllExport Instance *emptyList;		// aka List, L
extern DllExport Instance *emptyTuple;		// aka ROList, NoArglist
extern DllExport Instance *VList, *VTSList;	// the mothers of all Lists
extern DllExport int       roListID, tsListID;

void	  listConstruct(void);
Instance *stackPop(Instance *,pVM);
int	  listCheckExplode(Instance *,Instance *list,pVM);
Instance *arglistPop(pArglist, Byte *mlist,unsigned mlistSz, Instance**, pVM);
Instance *String_apply(Instance *,pArglist,pVM);
Instance *Fcn_highPass(Instance *fcn,pArglist,pVM);
size_t	  listSize(Instance *,pVM);
void	  argWindow(Instance *arglist,int *poffset,int *pnumArgs,pVM);
Instance *stringXplode(char *text, size_t max, pVM);
int	  listAppendN(Instance *, size_t offset, size_t n, pVM);

Instance *arglistDup(pArglist,unsigned offset, pVM);
void	  listSplat(Instance *,size_t offset,size_t n,Instance **,pVM);

Instance *mlistBuildFromMask(Byte *, unsigned maskLen, 
		unsigned mask, unsigned chop, 
		Instance *src0,Instance *src1, unsigned sz, pVM);

#endif // __NOT_A_DLL


DllExport Instance *mlistBuild(Byte *mlist, Instance *x,...);
DllExport Instance *mlistCopy(Byte *dst, Instance *src, size_t offset,unsigned sz);
DllExport Instance *mlistExtend(Byte *dst, Instance *src, size_t offset, unsigned sz);
DllExport Instance *mlistExtendN(Byte *dst, Instance *src, size_t srcOffset, unsigned sz);
DllExport Instance *mlistAppendI(Byte *, Instance *i, unsigned sz);
DllExport Instance *Tuple_append(Instance *,pArglist,pVM);

DllExport Instance *listCreate(
			size_t initialSize, int flags, int itype, pVM);
DllExport Instance *listCreateX(pVM,Instance *,...);
DllExport int	    listAppend(Instance *,Instance *,pVM);
DllExport Instance *List_extend(Instance *,pArglist,pVM);
DllExport Instance *Tuple_extend(Instance *,pArglist,pVM);
DllExport Instance *List_apply(Instance *,pArglist, pVM);
DllExport Instance *List_apply2(Instance *,pArglist, pVM);
DllExport size_t    listLen(Instance *,pVM);
DllExport Instance *List_toString(Instance *,pArglist,pVM);
DllExport void	    listClear(Instance *,pVM);
DllExport void	    listTruncateTo(Instance *,size_t n,pVM);
DllExport void	    listDelete(Instance *,size_t offset,size_t num,pVM);
DllExport void	    listReplace(Instance *, size_t n,Instance *newValue,pVM);
DllExport Instance *listPop(Instance *,size_t n,pVM);
DllExport int	    listRemoveI(Instance *, Instance *, pVM);
DllExport void	    verifyList(char *name, Instance *, int type, pVM);
DllExport Instance *listCopy(Instance *, pVM);
DllExport Instance *listToTuple(Instance *,size_t offset,size_t n,pVM);
DllExport Instance *createTupleOfStrings(pVM,char *s,...);
DllExport Instance *List_makeReadOnly(Instance *,pArglist, pVM);
DllExport Instance *List_pump(Instance *,Instance *,pVM);
DllExport Instance *listInsert(Instance *,size_t idx,Instance *,pVM);
DllExport Instance *Tuple_create(Instance *,pArglist,pVM);

DllExport Instance *tupleCreate(size_t,int itype,pVM);
DllExport Instance *tupleCreateX(pVM,Instance *,...);
DllExport void	    tupleAppend(Instance *,Instance *);


     // Not thread safe
DllExport Instance *listGet(Instance *,unsigned n);

DllExport Instance *Sequence_sort(Instance *,pArglist, pVM);
DllExport Instance *Sequence_callProperty(Instance *,pArglist, pVM);
DllExport Instance *Sequence_callMethod(Instance *,pArglist, pVM);


    // Actually in walker.c:
DllExport Instance *zipIt(Instance *,pArglist,char *zipper,pVM);
DllExport Instance *zippityDoDa(Instance *,pArglist,char *zipper,pVM);


#ifdef __cplusplus
};		// end of extern "C"
#endif

#endif // __ZKL_LIST_H
