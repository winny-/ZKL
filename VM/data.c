/* data.c : the Data object : A container of bytes
 * Supports both Stream and Sequence semantics
 * Acts as a bunch of strings, lines or just a jumble of bytes.
 * 
 * Copyright (c) 2006,2007,2008-12,2013 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define __DATA_INTERNALS
#define __NOT_A_DLL

#include "zklObject.h"
#include "zklData.h"
#include "zklFcn.h"
#include "zklList.h"
#include "zklNumber.h"
#include "zklString.h"
#include "zklUtil.h"

    /* Thread safety: Not.
     * Since Data are not containers, they don't have to worry about GC
     *   threads.
     * Read only-able: No. Even if no additions or gap movement, the
     * cursor[s] are not thread safe.
     */

#define DEFAULT_SIZE	      500

static ZKL_Object DataObject;

#define DATA(d)		( (Data *)d )
#define D_SIZE(d)	( ((Data *)d)->allocatedSpace )
#define D_LEN(d)	( ((Data *)d)->len )
#define D_BITS(d)	( ((Data *)d)->bits )
#define D_CURSOR(d)	( ((Data *)d)->cursor )
#define D_GAP_START(d)	( ((Data *)d)->gapStart )
#define D_GAP_SIZE(d)	( ((Data *)d)->gapSize )
#define D_TREAT_AS(d)	( ((Data *)d)->treatAs )
#define D_HOWZA(d)	( ((Data *)d)->howza )
#define D_HOWZA_TMP(d)	( ((Data *)d)->howzaTmp )
#define D_OBEY_CURSOR(d)( ((Data *)d)->startAtCursor )
#define IS_KDATA(d)	( d->iflag )

static void clearMarks(Data *);
static void closeGap(Data *);
static void makeRoomFor(Data *,size_t,pVM);

size_t dataCursorPos(Instance *d) { return D_CURSOR(d); }
size_t dataCursorSet(Instance *d, size_t pos)
{
   size_t sz = D_LEN(d);
   if (pos > sz) pos = sz;
   D_CURSOR(d) = pos;
   return pos;
}
size_t dataSize (Instance *d){ return D_LEN(d);  } // length of contained data
size_t dataSpace(Instance *d){ return D_SIZE(d); } // ammount I can hold

static void dataZero(Data *self)
{
   self->len	  = 0;
   self->gapStart = 0;
   self->gapSize  = self->allocatedSpace;	// aka all space available
   self->cursor	  = 0;
   clearMarks(self);
}

static IBucketHeader dBuckets;

    // Can GC
    // mode is Int, howza is GET_LINES
Instance *dataCreate(size_t size, int itype, pVM vm)
{
   DSize_t sz = (DSize_t)size + 1;	// leave space for a terminating '\0'
   Data   *d;
   Byte   *ptr;

//!!! round up !careful, that would break dataWillFill

   if (sz < size)	// check size
      vmThrow(vm,E_VALUE_ERROR,"Can't create a Data that big");

   ptr = ZMALLOC(sz);	// leave room for a '\0'
   if (!ptr)
   {
      collectGarbage(1,vm);	// wait
      ptr = ZMALLOC(sz);	// try again
      if (!ptr) vmThrow(vm,E_OUT_OF_MEMORY,0);
   }

   d = (Data *)ibucketAllocate(&dBuckets,&DataObject,itype,0,vm);
   if (!d)
   {
      ZFREE(ptr);
      vmThrow(vm,E_OUT_OF_MEMORY,0);
   }
   d->allocatedSpace = (DSize_t)size;	// Don't include optional '\0'
   dataZero(d);
   d->bits	= ptr;
   d->treatAs   = DATA_TREAT_AS_DATA;
   d->z		= 1;	// I has zero
   d->howza = d->howzaTmp = 1;
   d->startAtCursor = 0;

   return (Instance *)d;
}

    // data is zero'd, CALL dataSetSize()!
    /* Two reasons to call this:
     * 1) You want to clear data. It is empty after this call.
     * 2) You are going to stuff data into buffer through a pointer eg fread.
     *    YOU have ensured data is big enough.
     *    YOU WILL call dataSetSize() (after filling) to make the bits
     *    visible.
     */
void *dataWillFill(Instance *self)
{
   dataZero((Data *)self);
//   D_GAP_START(self) = D_SIZE(self);
//   D_GAP_SIZE(self)  = 0;
   return D_BITS(self);
} 

    // .fill(byte [,sz])
    // ????.fill(byte [offset,sz])
    // .fill(Data)
    // Only change size if grows, maybe shrink to sz
static Instance *Data_fill(Instance *self,pArglist arglist,pVM vm)
{
   Instance *filler = arglistGet(arglist,0,"Data.fill",vm);
   int64_t   sz = D_SIZE(self);

   if(arglistTryToGetInt(arglist,1,&sz,0,vm))
   {
      if(sz<=0) return self;
      makeRoomFor((Data *)self,(size_t)sz,vm);
   }
   switch(TYPEO(filler))
   {
      case IntType:
      {
	 int fill = (int)convertToInt(filler,vm);
	 memset(dataWillFill(self),fill,(size_t)sz);
	 dataSetSize(self,(size_t)sz);
	 break;
      }
      case DataType:
      {
	 size_t fillSz, z=(size_t)sz;
	 Byte *fill=dataText(filler,&fillSz), *ptr=dataWillFill(self);

	 if(filler==self) vmThrow(vm,E_ASSERTION_ERROR,"Data.fill(self): No");
	 while(z >= fillSz){ memcpy(ptr,fill,fillSz);  ptr+=fillSz; z-=fillSz; }
	 memcpy(ptr,fill,z);
	 dataSetSize(self,(size_t)sz);
	 break;
      }
   }
   return self;
}

//Byte *dataBits(Instance *self) { return D_BITS(self); }

void dataSetSize(Instance *self, size_t size)
{
   D_LEN(self) = D_GAP_START(self) = (DSize_t)size;
   D_GAP_SIZE(self) = (DSize_t)(D_SIZE(self) - size);
}

    // Can GC
    // mode is Int, howza is GET_LINES
Instance *dataCreate2(Byte *bytes, size_t size, pVM vm)
{
   Instance *d = dataCreate(size,I_OWNED,vm);
   memcpy(dataWillFill(d),bytes,size);
   dataSetSize(d,size);
   return d;
}

static int dataFree(Instance *_self)
{
   Data *self = (Data *)_self;
   ZFREE(self->bits);
   self->bits = 0;		// just in case
   return 1;
}

    // Prep Data as one big string: close the gap and place a zero at the end
Byte *dataText(Instance *self,size_t *len)
{
   if (!IS_KDATA(self))			// KData has no gap
   {
      closeGap((Data *)self);
      D_BITS(self)[D_LEN(self)] = '\0';		// might be redunant
   }
   if (len) *len = D_LEN(self);
   return D_BITS(self);
}

    // Return pointer to &self[offset] such that the gap is out of the way
    // Doesn't '\0' terminate
    // No range checking is done
Byte *dataChunkAt(Instance *self,size_t offset,size_t size)
{
   Byte *ptr = D_BITS(self) + offset;

   if (!IS_KDATA(self))
   {
      DSize_t gapStart = D_GAP_START(self);
      DSize_t gapSize  = D_GAP_SIZE(self);

      if (gapStart <= offset) return (ptr + gapSize); // gap starts before chunk
      if (offset + size <= gapStart) return ptr;     // gap is after chunk
      closeGap((Data *)self);			    // chunk contains gap
   }
   return ptr;
}

#if 0
    // Return address cursor points to. No gap movement.
static Byte *cursorToPtr(Instance *self)
{
   DSize_t  cursor = D_CURSOR(self);
   Byte	   *ptr    = D_BITS(self) + cursor;

   if (D_GAP_START(self) <= cursor) return (ptr + D_GAP_SIZE(self)); 
   return ptr;
}
#endif

/////////////////////////////////////////////////////////////////////
///////////////////////// Marks /////////////////////////////////////
/////////////////////////////////////////////////////////////////////

static void clearMarks(Data *self)
{
   int      z;
   DSize_t *mark = self->marks;

   for(z = DNUM_MARKS; z--; mark++) *mark = 0;
}

    // ....M......
    // ..O+++++..M......
static void adjustMarksInsert(Data *self,size_t offset,size_t n)
{
   int      z;
   DSize_t *mark = self->marks;

   for(z = DNUM_MARKS; z--; mark++)
      if (*mark > offset) *mark += (DSize_t)n;
}

    // ....M...... | .......M..
    // ..D.. n=big | ..D..... n=2
    // ..M..	   | .....M..
static void adjustMarksDelete(Data *self,size_t offset,size_t n)
{
   int      z;
   DSize_t *mark = self->marks, len = self->len;

   for(z = DNUM_MARKS; z--; mark++)
   {
      if (*mark > offset)
      {
	 if (*mark < offset + n) *mark = (DSize_t)offset;
	 else			 *mark -= (DSize_t)n;
      }
      if (*mark > len) *mark = len;
   }
}

//!!!??? mark[0] = cursor? swapMarks(a,b)?, setMark returns old value?
// swap --> fcn swap(d,a,b) { d.setMark(a,d.setMark(b,d.getMark(a))) }
    // setMark(n,offset=cursor), marks[n] = cursor
    // --> previous mark offset
static Instance *Data_setMark(Instance *self,pArglist arglist,pVM vm)
{
   DSize_t *M = &((Data *)self)->marks[
		arglistGetIndex(arglist,0,DNUM_MARKS,"Data.setMark",vm)];
   DSize_t  old = *M;
   Offset_t offset;
   if (!arglistConstrainOffset(arglist,1,D_LEN(self), 0x12,&offset, 0,vm))
      offset = D_CURSOR(self);
   *M = (DSize_t)offset;
   return intCreate(old,vm);

//   ((Data *)self)->marks[arglistGetIndex(arglist,0,10,vm)] = (DSize_t)offset;

//   ((Data *)self)->marks[arglistGetIndex(arglist,0,10,vm)] = D_CURSOR(self);
//   return self;
}

static Instance *Data_getMark(Instance *self,pArglist arglist,pVM vm)
{
   return intCreate(((Data *)self)->marks[
	   arglistGetIndex(arglist,0,DNUM_MARKS,"Data.getMark",vm)],vm);
}

#if 0
static Instance *Data_allocateMarks(Instance *self,pArglist arglist,pVM vm)
{
   malloc array of DSize_t
}
#endif

     /////////////////////////////////////////////////////////////////
     ///////////////// Gap Management ////////////////////////////////
     /////////////////////////////////////////////////////////////////

    // ie move the gap to the end
static void closeGap(Data *self)
{
   Byte    *src, *dst;
   DSize_t  size;

   if (self->gapStart == self->len) return;
   dst  = D_BITS(self) + self->gapStart;
   src  = dst + self->gapSize;
   size = self->allocatedSpace - self->gapSize - self->gapStart;
   memmove(dst,src,size);
   self->gapStart = self->len;
}

    /* Cases
     *            | gap2 | | gap3 |
     * |----------------------------------------------|
     *    | gap1 |    | newGap |          | gap4 |
     *                n
     */
static void moveGapTo(Data *self, size_t offset)
{
   Byte    *src, *dst;
   DSize_t  gapStart = self->gapStart, gapSize, size;

   if (0 == D_LEN(self))   return;	// all gap, all the time
   if (offset == gapStart) return;	// gap already in place

   gapSize = self->gapSize;
   if (gapSize == 0)		// I can move no bits really fast
   {
      self->gapStart = (DSize_t)offset;
      return;
   }
//!!! do da math: move smaller of gap or bits
   if (gapStart < offset)	// move bits left (1,2)
   {
      dst  = D_BITS(self) + gapStart;
      src  = dst + gapSize;		// gapEnd
      size = (DSize_t)(offset - gapStart);
   }
   else				// move bits right (3,4)
   {
      src  = D_BITS(self) + offset;
      dst  = src + gapSize;   // == gapStart + gapSize - size == gapEnd - size
      size = (DSize_t)(gapStart - offset);
   }

   if (src != dst && size) memmove(dst,src,size);
   self->gapStart = (DSize_t)offset;
}

#if 0
static void openGap(Data *self,size_t n)
{
   Byte    *src, *dst;
   DSize_t  size;

   if (0 == D_LEN(self))    return;	// all gap, all the time
   if (n == self->gapStart) return;	// gap already in place
   closeGap(self);	//!!! move gap instead: profile to see if I care
   if (n == D_LEN(self)) return;

   src  = D_BITS(self) + n;
   dst  = src + self->gapSize;
   size = self->len - n;	// == gapStart - n
   memmove(dst,src,size);
   self->gapStart = n;
}
#endif

    /* Is there enough room for n more bytes? Allocate space if not.
     * Assume continued growth, add some padding.
     * Can GC, does overflow checking
     * WARNING! YOU need to protect self
     * 0 <= x,y <= M, z = x+y, if z < x or z < y then overflow
     */
#define K 1000
static void makeRoomFor(Data *self,size_t n,pVM vm)
{
   Byte     *ptr;
   DSize_t  size = D_SIZE(self), len = D_LEN(self), z;

   z = (DSize_t)(n + len);
   if (z < len || z < n)
overflow:
      vmThrow(vm,E_ASSERTION_ERROR,"Overflow trying to make room in Data");

   if (z < size) return;	// already have enough space, NOT <=

   closeGap(self);   // put the gap at end of bits, simple to increase gap
   z = (DSize_t)(n + size + K);
   if (z < n || z < size || z < K) goto overflow;
   n += K; size = z;
   ptr = ZREALLOC(self->bits,size);
   if (!ptr)
   {
      collectGarbage(1,vm);		// wait
      ptr = ZREALLOC(self->bits,size);	// try again
      if (!ptr) vmThrow(vm,E_OUT_OF_MEMORY,0);
   }
   self->bits		= ptr;
   self->allocatedSpace	= size;
   self->gapSize += (DSize_t)n;
}

/* ******************************************************************** */
/* *************************** Methods ******************************** */
/* ******************************************************************** */

    // .toBool()
Instance *Data_toBool(Instance *self,pArglist arglist,pVM vm)
{
   if (D_LEN(self)) return BoolTrue;
   return BoolFalse;
}

    // .toString()
Instance *Data_toString(Instance *self,pArglist arglist,pVM vm)
{
   char buf[100], len[30];
//   sprintf(buf,"%s(%d)",ONAME(self),D_LEN(self));
   sprintf(len,"%u",D_LEN(self));
   sprintf(buf,"%s(%s)",ONAME(self),fmtCommaize(len,3,',')); //!!!thousandsMark
   return stringCreate(buf,I_OWNED,vm);
}

    // .toList(offset=0) --> list of strings, starting at offset
Instance *Data_toList(Instance *self,pArglist arglist,pVM vm)
{
   int isData = !IS_KDATA(self);

   if (D_TREAT_AS(self) == DATA_TREAT_AS_STRINGS)
   {
	/* This code relies on the fact that dataText() appends a "fake"
	 * '\0' to the end of the data, which is at bits[len].  Also,
	 * somebody may have lied about this data being strings.
	 */
      Instance *list;
      size_t	len,num;
      char     *ptr = (char *)dataText(self,&len), *end = ptr+len, *p;
      Fence	fence;
      int64_t	i64;

      if (!len) return emptyTuple;

      if (arglistTryToGetInt(arglist,0,&i64,0,vm))
      {
	 size_t offset = (size_t)i64;
	 if (offset > len) offset = len;
	 ptr += offset; len -= offset;
	 if (!len) return emptyTuple;
      }

      num = 1; p = ptr;
      while ((p = memchr(p,'\0',len)) && p < end)  // count strings
	{ num++; p++; }
      list = listCreate(num,0x1,I_OWNED,vm);  // L("one","two").pop() is useful
      vmSetFence(vm,&fence,0,list);
	 while (num--)
	 {
	    Instance *str = isData ? stringCreate(ptr,I_OWNED,vm) : 
				     kStringCreate(ptr,self,I_OWNED,vm);
	    p = memchr(ptr,'\0',len);
	    listAppend(list,fence.i1 = str,vm);
	    ptr = p + 1;
	    if (ptr == end) break;	// don't count implicit \0
	 }
      vmRemoveFence(&fence,0);
      return list;
   }
   return tupleCreateX(vm,self,ZNIL);	// L(self)
}

    // Convert bytes to big or little endian ([offset=0,len=8,unsigned=True])
static Instance *
_littleBig(Instance *self,pArglist arglist,pVM vm,int bigEndian)
{
   uint64_t  x = 0;
   Byte	    *ptr;
   int       un = arglistTryToGetBool(arglist,2,1,"Data.?Endian",vm);
   size_t    offset = 0,len = D_LEN(self), size,nz;

   if (!listLen(arglist,vm)) size = (len < 8) ? len : 8;
   else 	arglistGetChunk(arglist,0,0x00, len, &offset,&size, 0,vm);

   ptr = dataChunkAt(self,offset,size);
   nz  = size;
   if (bigEndian) while (nz--) x = (x << 8) | *ptr++;
   else		// little endian
   {
      int n = 0;
      while (nz--) { x |= ((uint64_t)*ptr++ << n); n += 8; }
   }

   if (!un && size != 8)	// signed int
   {	// I'm assuming 2s complement
      int bits    = (int)(8 * size);
      int signBit = ( (x & ((uint64_t)1)<<(bits - 1)) != 0 );
      if (signBit)
      {
	 uint64_t mask = (((uint64_t)-1) >> bits) << bits;
      	 x |= mask;
      }
   }

   return intCreate(x,vm);
}

    // .toBigEndian([offset=0,len=8,unsigned=True]) --> int
Instance *Data_toBigEndian(Instance *self,pArglist arglist,pVM vm)
   { return _littleBig(self,arglist,vm,1); }

    // .toLittleEndian([offset=0,len=8,unsigned=True]) --> int
Instance *Data_toLittleEndian(Instance *self,pArglist arglist,pVM vm)
   { return _littleBig(self,arglist,vm,0); }

int dataGetMode(Instance *self) { return D_TREAT_AS(self); }

void dataMode(Instance *self,int mode, pVM vm)
{
   switch(mode)
   {
      case DATA_TREAT_AS_DATA: break;
      case DATA_TREAT_AS_STRINGS:
	 if (IS_KDATA(self) && !((Data *)self)->z)
	     vmThrow(vm,E_VALUE_ERROR,"ConstData.mode(x): Not stringable");
	 break;
      default:
      {
	 char buf[100];
	 sprintf(buf,"Data.Mode(%d) is invalid. ",mode);
	 vmThrow(vm,E_VALUE_ERROR,buf);
      }
   }
   D_TREAT_AS(self) = mode;
}

    // .mode([mode]), mode is String or Int
static int _mode(Instance *self,pArglist arglist,int a0,pVM vm)
{
   Instance *i = arglistTryToGet(arglist,a0);
   if (i) 
   {
      switch(TYPEO(i))
      {
	 case StringType: dataMode(self,DATA_TREAT_AS_STRINGS,vm); break;
	 case IntType:    dataMode(self,DATA_TREAT_AS_DATA,vm);    break;
      }
      return 1;
   }
   return 0;
}

Instance *Data_mode(Instance *self,pArglist arglist,pVM vm)
{
   if (_mode(self,arglist,0,vm)) return self;
   if (DATA_TREAT_AS_STRINGS == D_TREAT_AS(self)) return emptyString;
   return Zero;
}

static void copyFlags(Data *src, Data *dst)
{
   D_TREAT_AS(dst) = D_TREAT_AS(src);
   D_HOWZA(dst) = D_HOWZA_TMP(dst) = D_HOWZA(src);
}

    // .howza([getter mode, tmp=False]) --> self|int
Instance *Data_howza(Instance *self,pArglist arglist,pVM vm)
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
   return intCreate(((Data *)self)->howza,vm);
}

    // .startAtCursor() --> self
static Instance *Data_startAtCursor(Instance *self,pArglist arglist,pVM vm)
   { D_OBEY_CURSOR(self) = 1; return self; }

size_t dataLen(Instance *self) { return D_LEN(self); }

    // .len() --> # bytes
    // .len(0) --> .len() --> bytes
    // .len(1) --> lines
    // .len(2) --> strings
    // .len(True,setLenTo) --> Adjust size. 
    //     Can grow bigger than len but less than size. This is so external 
    //     code can stuff the buffer (eg CInvoke).
Instance *Data_len(Instance *self,pArglist arglist,pVM vm)
{
   Instance *i = arglistTryToGet(arglist,0);
   if (i && TYPEO(i) == IntType)
   {
      int fromCursor = (arglistTryToGet(arglist,1) == BoolTrue);
      DSize_t cursor = D_CURSOR(self);

      switch(convertToInt(i,vm))
      {
	 case 0:	// bytes
	    if (fromCursor) return intCreate(D_LEN(self) - cursor,vm);
	    return intCreate(D_LEN(self),vm);
	 case 1: 	// lines
	 {
	    long int n;
	    if (!fromCursor) D_CURSOR(self) = 0;
	    for(n = 0; dataNextLine(self,0,0); n++) ;
	    D_CURSOR(self) = cursor;
	    return intCreate(n,vm);
	 }
	 case 2:	// strings
	 {
	    long int n;
	    if (IS_KDATA(self) && D_TREAT_AS(self) != DATA_TREAT_AS_STRINGS)
	       return Zero;
	    if (!fromCursor) D_CURSOR(self) = 0;
	    for(n = 0; dataNextString(self,0); n++) ;
	    D_CURSOR(self) = cursor;
	    return intCreate(n,vm);
	 }
      }
      return Zero;
   }

   if (i == BoolTrue)
   {
      int64_t len = arglistGetInt(arglist,1,0,vm);
      if (IS_KDATA(self) || len > D_SIZE(self)) vmThrow(vm,E_VALUE_ERROR,0);
      D_LEN(self)      = (DSize_t)len;
      D_TREAT_AS(self) = DATA_TREAT_AS_DATA;
   }
   return intCreate(D_LEN(self),vm);
}

    // .copy()
static Instance *Data_copy(Instance *self,pArglist arglist,pVM vm)
{
   Instance *new;

   closeGap((Data *)self);
   new = dataCreate2(D_BITS(self),D_LEN(self),vm);
   D_CURSOR(new) = D_CURSOR(self);
   copyFlags((Data *)self,(Data *)new);
   memcpy(&DATA(new)->marks,&DATA(self)->marks,DNUM_MARKS*sizeof(DSize_t));
   
   return new;
}

    // .create: Data(size=0 [, mode=Int|String [,someData ...]])
    // Data(Void [,someData ...]) --> Data(0,Int [,someData ...])
    // size == 0, use a default or size of someData
Instance *Data_create(Instance *self,pArglist arglist,pVM vm)
{
   int	     a = 2;
   int64_t   i64;
   Instance *data, *someData, *pSz = arglistTryToGet(arglist,0);
   size_t    size = DEFAULT_SIZE;

   if (!pSz) goto noargs;

   if (Void == pSz)
   {
      data = dataCreate(DEFAULT_SIZE,I_OWNED,vm);
      a = 1; 
      goto stuff; 
   }

   if (!iWantInt(pSz,&i64,vm)) vmThrow(vm,E_TYPE_ERROR,"Data(Void,bytes) or Data(size,Int|String,bytes)");
   if (i64 > DEFAULT_SIZE) size = (size_t)i64;

noargs:
   data = dataCreate(size,I_OWNED,vm);
   _mode(data,arglist,1,vm);	// OK if no arg

stuff:
   if (arglistLen(arglist,vm) > a)
   {
      Fence fence;
//!!!???? don't need fence since no gc possible?
      vmSetFence(vm,&fence,0,data); // data can grow depending on arglist
         for (; (someData = listGet(arglist,a)); a++)
	    dataAppend(data,someData,vm);
      vmRemoveFence(&fence,0);
   }

   return data;
}

    /* Insert or append data.
     * Excutive decision: if trying to insert at the end of the data, turn
     *   the op into an append.
     * Can GC
     * Cursor doesn't move.
     * Input:
     *    data:   self
     *    offset: Where to insert the data. D_LEN to append.
     *    bytes:  Ptr to bytes of data
     *    size:   Number of bytes
     *    append: 0 (insertion, verify offset) else ignore offset and append
     *    vm:
     * Output:
     *   Num bytes inserted/appended
     */
size_t dataInsertBytes(Instance *datai,
    Offset_t offset, Byte *bytes, size_t size, int append, pVM vm)
{
   Data    *self = (Data *)datai;
   DSize_t  len  = self->len, idx;

   if (size == 0) return 0;	// eg insert ""

   {
      Byte *ptr = D_BITS(self);

      if (bytes >= ptr && bytes < (ptr + len))	// not good if realloc bits
	 vmThrow(vm,E_ASSERTION_ERROR,"Data: Recursive write, not good");
   }

	// force append if at end of data
   if (offset == len || append) idx = len;
   else idx = (DSize_t)constrainOffset(offset,self->len,1, "Data.?",vm);

   makeRoomFor(self,size,vm);	// Can GC
   moveGapTo(self,idx);
   memcpy(D_BITS(self) + idx, bytes, size);
   self->len	  += (DSize_t)size;
   self->gapStart += (DSize_t)size;
   self->gapSize  -= (DSize_t)size;
   adjustMarksInsert(self,idx,(DSize_t)size);

#if 0
   if (overstrike)
   {
      size_t	z   = idx + size;	// overstrike might be partial insert
      size_t	len = self->len;
      if (z > self->allocatedSpace)	// need more space, gap closed
	 makeRoomFor(self, z - len, vm);
      else if (!gapOverlap(self,n,size)) // no need to move/adjust the gap
      {				      // because I'm not going to write in it
	 memcpy(D_BITS(self) + idx, bytes, size);
	 return size;
      }

      closeGap(self);				// have to move the gap
      memcpy(D_BITS(self) + idx, bytes, size);
      if (z > len)	// wrote into the gap
      {
	 self->len     = self->gapStart = z;
	 self->gapSize = self->allocatedSpace - z;
      }
   }
#endif
   return size;
}

static size_t	// Cursor doesn't move
dataInsert(Data *self, Offset_t offset, Instance *x, int append, pVM vm)
{
   Byte	  *ptr;
   size_t  size = 0;
   Byte	   c;

top: ;
   switch (TYPEO(x))
   {
      case StringType:
	 ptr  = (Byte *)stringText(x);
	 size = strlen((char *)ptr);
	 if (self->treatAs == DATA_TREAT_AS_STRINGS) size++;
	 break;
      case IntType:
	 c = (Byte)convertToInt(x,vm);	// lowest byte ??? endian-ness?
	 ptr = &c; size = 1;
	 break;
      case BoolType: c=(x==BoolTrue); ptr=&c; size=1; break;
      case DataType:
	 if (x->iflag)	// KData
	 {
	    ptr  = D_BITS(x);
	    size = D_LEN((Data *)x);
	    break;
	 }
	 // else Data
	 ptr  = D_BITS(x);
	 size = D_LEN((Data *)x);
	 closeGap((Data *)x);		// gonna copy the entire Data
	 break;
//      case ListType:	// !!! circular --> boom, NOT thread safe
      case TupleType:	// eg arglist, circular --> boom, thread safe
      {
	 size_t	   i,s;
	 Instance *t;
	 for (i = 0; (t = listGet(x,i)); i++)
	 {
	    s = dataInsert(self,offset,t,append,vm);
	    size   += s;
	    offset += (Offset_t)s;
	 }
	 return size;
      }
      default:	// append(x.toData(mode)), List wants mode
      {
	 MLIST(mlist,1);
	 mlistBuild(mlist,(self->treatAs) ? emptyString : Zero,ZNIL);
//	 x = OBJECT1(x)->toData(x,(Instance *)mlist,vm);
	 x = I_METHOD(x,TO_DATA)(x,(Instance *)mlist,vm);
	 // x should now be a Data or KData
	 // damn, it would be nice to just replace self with x if
	 // self is empty (and x is Data).
	 goto top;
#if 0
	 char	buf[100];
	 sprintf(buf,"Can't add type %s to Data",iname(x));
	 vmThrow(vm,E_TYPE_ERROR,buf);
#endif
      }
   }
   return dataInsertBytes((Instance *)self,offset,ptr,size,append,vm);
}

    // Can GC, cursor doesn't move
void dataAppend(Instance *self, Instance *i, pVM vm)
{
   if (!self) return;
   dataInsert((Data *)self,0,i,2,vm);
}

    /* .append([a,b,c,...]): Append stuff to end of Data
     * .append(0) always sticks '\0' into data
     * .append("") appends '\0' if STRINGS or nothing if DATA
     * Cursor doesn't move
     */
static Instance *Data_append(Instance *self,pArglist arglist,pVM vm)
{
   dataAppend(self,arglist,vm);
   return self;
}

#if 0
    // (int...)
static Instance *Data_appendBytes(Instance *self,pArglist arglist,pVM vm)
{
   char	  *bytes = arglistGetString(arglist,0,0,vm);
   size_t  num   = INT(arglistGetInt(arglist,1,0,vm));
   dataInsertBytes(self,(long)D_LEN(self),bytes,num,0,vm);
   return self;
}
#endif

    // .insert(offset,item,...)  --> self
static Instance *Data_insert(Instance *self,pArglist arglist,pVM vm)
{
   Offset_t  offset;
   Instance *i;
   int	     n;

   	// no-op if no items, Data().insert() --> error
   arglistConstrainOffset(arglist,0,D_LEN(self),0x2,&offset,"Data.insert",vm);
   for (n = 1; (i = listGet(arglist,n)); n++)
      offset += dataInsert((Data *)self,(Offset_t)offset,i,0,vm);

   return self;
}

    /* Shrink the bits in Data, ie increase the gap size.
     * Assume that a shrink will be followed by a insert; move the gap to
     *    area to be deleted and just increase the gap size.
     * No bounds checking, bogus values will cause core dumps
     */
static void dataShrink(Data *self, size_t offset, size_t size)
{
   DSize_t len = self->len, diff;
   DSize_t gapStart = self->gapStart;
   DSize_t end = offset+size;

   if (size == 0) return;
   if (size >= len) { dataZero((Data *)self); return; }

   // if offset is at edge of gap or overlaps gap, just change gap size
   if (offset == gapStart)		// grow gap right
   {
   }
   else if (offset < gapStart && end >= gapStart) // overlap left edge
   {  // move gap edge left and recurse
      diff = gapStart - offset;
      self->gapStart = offset;
      self->gapSize += diff;
      len -= diff; self->len = len;

      if (self->cursor > len) self->cursor = len;
      adjustMarksDelete(self,offset,diff);
      dataShrink(self,offset,size-diff);
      return;
   }
   else   // no other possibilities to move just gap
   {
      moveGapTo(self,offset);
   }
   self->gapSize += (DSize_t)size;
   len -= (DSize_t)size;
   self->len = len;
   if (self->cursor > len) self->cursor = len;
   adjustMarksDelete(self,offset,size);
}

    // .del(offset, [num | *] = 1) == (n, [num | len])
    // -->self
static Instance *Data_shrink(Instance *self,pArglist arglist,pVM vm)
{
   size_t num, offset;
   arglistGetChunk(arglist,0,0x00, D_LEN(self), &offset,&num, 0,vm);
   dataShrink((Data *)self,offset,num);
   return self;
}

    // [offset [,len=1]] --> .__sGet(offset,len)
    // Cursor doesn't move, similar to .read(cursor,n)
Instance *Data_sGet(Instance *self,pArglist arglist,pVM vm)
{
   Byte     *ptr;
   int	     s;
   size_t    offset, size;
   Instance *new;

   s   = arglistGetChunk(arglist,0,0x00, D_LEN(self), &offset,&size, 0,vm);
   ptr = dataChunkAt(self,offset,size);
   if (D_TREAT_AS(self) == DATA_TREAT_AS_STRINGS)
      return stringCreate2((char *)ptr,size,vm);

   if (s==1) return INT_CREATE(*ptr,vm);
   new = dataCreate2(ptr, size, vm);
   copyFlags((Data *)self,(Data *)new);
   return new;
}

    // .charAt(offset [,len=1]) -->String
    // Cursor doesn't move
Instance *Data_charAt(Instance *self,pArglist arglist,pVM vm)
{
   Byte   *ptr;
   size_t  offset, size;

   arglistGetChunk(arglist,0,0x00, D_LEN(self), &offset,&size, 0,vm);
   ptr = dataChunkAt(self,offset,size);
   return stringCreate2((char *)ptr,size,vm);
}

    // .bytes(offset,len=1 | *), almost the same as read or []
    // .bytes() --> all bytes
    // .bytes(offset)   --> Int (1 byte)
    // .bytes(offset,1) --> Int
    // .bytes(offset,n) --> List of Ints
    // .bytes(offset,0) --> T
Instance *Data_bytes(Instance *self,pArglist arglist,pVM vm)
{
   size_t offset, size;

   if (listLen(arglist,vm) == 0)	// ()
      { offset = 0; size = D_LEN(self); }
   else
   {
      arglistGetChunk(arglist,0,0x00, D_LEN(self), &offset,&size, 0,vm);
      if (size == 1) return intCreate(*dataChunkAt(self,offset,1),vm);
   }

   if (size == 0) return emptyList;

   {	// sleeze ball alert: a byte fits in a PtrInt, ie no allocation/GC
      Byte     *ptr  = dataChunkAt(self,offset,size);
      Instance *list = tupleCreate(size,I_OWNED,vm);

      while(size--) tupleAppend(list,INT_CREATE(*ptr++,vm));
      return list;
   }
}

    /* a[offset,len] = v  --> a.__sSet(v,offset,len) --> v
     * Cursor doesn't move.
     * Note:  if there is room for the new data, the gap is moved, at
     * most, once.
     */
static Instance *Data_sSet(Instance *self,pArglist arglist,pVM vm)
{
   size_t    offset,size;
   Instance *v = arglistGet(arglist,0,0,vm);

   arglistGetChunk(arglist,1,0x00, D_LEN(self), &offset,&size, "Data[]",vm);

//!!! if new.len == old.len --> overwrite, don't delete, ie don't move gap if don't have to
   if (size) dataShrink((Data *)self,offset,size);
   dataInsert((Data *)self,(Offset_t)offset,v,0,vm);
   return v;
}

    // .set(idx,byte(s)|Data|String)
    // NO gap movement, overstrike
static Instance *Data_set(Instance *self,pArglist arglist,pVM vm)
{
   Offset_t  idx;
   size_t    len=D_LEN(self);
   int	     n,asz=arglistLen(arglist,vm);
   Instance *x=arglistGet(arglist,1,0,vm);

   if(2==arglistGetOffset(arglist,0,len,0x0,&idx,"Data.set",vm))
   {
      Byte    *bits=D_BITS(self);
      DSize_t  gapStart=D_GAP_START(self), gapSize=D_GAP_SIZE(self);
      
      switch(TYPEO(x))
      {
	 case StringType:
	 {
	    char *ptr = stringText(x), c;
	    for(; (c=*ptr++) && idx<len; idx++)
	    {
	       if(idx<gapStart) bits[idx]	 =(Byte)c;
	       else		bits[idx+gapSize]=(Byte)c;
	    }
	    break;
	 }
	 case IntType:
	    for(n=1; n<asz && idx<len; idx++,n++)
	    {
	       int z = (int)arglistGetInt(arglist,n,0,vm);
	       if(idx<gapStart) bits[idx]	   =(Byte)z;
	       else	        bits[idx+gapSize]=(Byte)z;
	    }
	    break;
      }
   }

   return self;
}

    // .replace(src,repl), replace them all
static Instance *Data_replace(Instance *self,pArglist arglist,pVM vm)
{
   MLIST(mlist,3);

   mlistBuild(mlist,self,ZNIL); mlistExtendN(mlist,arglist,0,3);
   fcnRunFromClass(Utils_Helpers,"dReplace",(Instance *)mlist,vm);
   return self;
}

    // .swap(x|*,y|*|-1) : swap two entries
    // * is always an index error
static Instance *Data_swap(Instance *self,pArglist arglist,pVM vm)
{
   Byte     *aptr, *bptr, tmp;
   Offset_t  a,b;
   size_t    len;

   len = D_SIZE(self);
   arglistConstrainOffset(arglist,0,len, 0x00,&a,0,vm);
   arglistConstrainOffset(arglist,1,len, 0x00,&b,0,vm);
   aptr = dataChunkAt(self,(size_t)a,1); bptr = dataChunkAt(self,(size_t)b,1);
   tmp = *aptr; *aptr = *bptr; *bptr = tmp;
   return self;
}

    /* .pop(offset=0,len=1) --> [offset,len]; .del(offset,len)
     * Cursor doesn't move unless it has to.
     */
//!!!??? Data(0,Int,1,2,3).pop(2,3) --> error?
static Instance *Data_pop(Instance *self,pArglist arglist,pVM vm)
{
   Byte     *ptr;
   Instance *result;
   int	     s;
   size_t    offset, size;

   s   = arglistGetChunk(arglist,0, 0x00, D_LEN(self), &offset,&size, 0,vm);
   ptr = dataChunkAt(self,offset,size);

   if (D_TREAT_AS(self) == DATA_TREAT_AS_STRINGS)
      result = stringCreate2((char *)ptr,size,vm);
   else
   {
      if (s==1) result = INT_CREATE(*ptr,vm);
      else
      {
	 result	= dataCreate2(ptr,size,vm);	// two args
	 copyFlags((Data *)self,(Data *)result);
      }
   }
   if (size) dataShrink((Data *)self,offset,size);
   return result;
}

    // .read() --> Data
    // .read(n|*) --> Data
    // .read(n|*,Data) --> Data
    // .read(n|*,Data,throw=True) --> Data
    // As in File.read()
    /* Read from Data, starting at the cursor, cursor moves
     * .read()      : read rest of data to Data
     * .read(<= 0)  : Data(0)
     * .read(n)     : Data
     * .read(> len) : same as ()
     * .read(*)     : same as ()
     * self is Data or KData
!!!??? if STRINGS, return string?
     */
Instance *Data_read(Instance *self,pArglist arglist,pVM vm)
{
   Byte	    *bits;
   DSize_t   offset = D_CURSOR(self);
   DSize_t   len    = D_LEN(self);
   size_t    size;
   Instance *result, *pd = arglistTryToGet(arglist,1);
   int	     throw = arglistTryToGetBool(arglist,2,1,"Data.read",vm);

   if (offset >= len)
      { if(throw) vmThrowTheEnd(vm); else return emptyKData; }

   arglistGetSize(arglist,0,len,&size,vm);
   if (offset + size > len) size = len - offset;
   bits = dataChunkAt(self,offset,size);

   if (pd && pd != VData && TYPEO(pd) == DataType)  // use their buffer
   {
      size_t z = dataSpace(pd);
      if (size > z) size = z;

      memcpy(dataWillFill(pd),bits,size);
      dataSetSize(pd,size);
      result = pd;
   }
   else
   {
      result = dataCreate2(bits,size,vm);
      copyFlags((Data *)self,(Data *)result);
   }

   D_CURSOR(self) += (DSize_t)size;

   return result;
}

    // Read from Data, starting at the cursor, cursor moves
    // .read1(): read 1 bytes --> Int
Instance *Data_read1(Instance *self,pArglist arglist,pVM vm)
{
   int c;
   if (D_CURSOR(self) >= D_LEN(self)) vmThrowTheEnd(vm);
   c = D_BITS(self)[D_CURSOR(self)++];
   return intCreate(c,vm);
}

    // Starting at the cursor, move cursor to start of next line or EoD.
    // A line ends at \n, \0, \n\0 but not \0\n. \n's are included in result.
    // Returns ptr to starting cursor, 0 if at EoD.
    // Doesn't roll off end.
    // Warning! might not be \0 terminated
char *dataNextLine(Instance *self, int strip, size_t *lineLen)
{
   DSize_t  len = D_LEN(self), num;
   DSize_t  cursor = D_CURSOR(self);
   Byte	   *ptr, *start;
   size_t   size;

   dataText(self,0);
   if (cursor >= len) return 0;

   start = ptr = &D_BITS(self)[cursor];
   num   = len - cursor;

   for (size = 0; num-- && *ptr; ptr++, size++)	// look for \0 or \n
      if (*ptr == '\n') { size++; break; }	// include the \n

   cursor = (DSize_t)(ptr - D_BITS(self) + 1);
   if (*ptr == '\n' && ptr[1] == '\0') cursor++;  // if \n\0, skip over \0 too
   if (cursor > len) cursor = len;
   D_CURSOR(self) = cursor;

   if (strip) start = (Byte *)stripWS((char *)start,size,&size,1);

   if (lineLen) *lineLen = size;
   return (char *)start;
}

    // Starting at offset, move forward n lines. Cursor doesn't move.
    // Returns: 1 (OK), 0 (couldn't move all n lines)
    // cursor: !0 --> where new cursor would be
static int 
dataNthLine(Instance *self, size_t offset, size_t nlines, DSize_t *cursor)
{
   size_t  len;
   Byte	  *ptr, *start, *end;

   dataText(self,&len);
   if (offset >= len) { if (cursor) *cursor = (DSize_t)len;    return !nlines; }
   if (!nlines)       { if (cursor) *cursor = (DSize_t)offset; return 1; }

   start = ptr = D_BITS(self) + offset;
   end   = D_BITS(self) + len;
   for(; 1; ptr++)
   {
      if (*ptr == '\0')
      {
	 if (--nlines == 0 || ptr == end) break;
      }
      if (*ptr == '\n')
      {
	 if (ptr[1] == '\0') ptr++;  // if \n\0, skip over \0 too
      	 if (--nlines == 0) break;
      }
   }
   if (cursor)
   {
      offset = ptr - D_BITS(self) + 1;
      if (offset > len) offset = len;
      *cursor = (DSize_t)offset;
   }
   return (nlines == 0);
}

    /* .readln(): This is a text op, read from the cursor
     * .readln(numLines=1)
     * .readln() == readln(1) --> read one line --> string
     * .readln(n>1) --> list of lines/strings
     * Cursor moves.
     * self is Data or KData
     */
Instance *Data_readln(Instance *self,pArglist arglist,pVM vm)
{
   Instance *result;
   char	    *ptr;
   size_t    size, n;
   int	     s, isKData = IS_KDATA(self);
   Fence     fence;

   // dataNextLine() is OK with KData

   s = arglistGetSize(arglist,0,(size_t)-1,&n,vm);
   if (n == 0) return emptyString;

   ptr = dataNextLine(self,0,&size);
   if (!ptr) vmThrowTheEnd(vm);

   if (0 == s)		 	 // .readln() --> read one line
   {
      if (isKData && ptr[size] == '\0')
	 return kStringCreate(ptr,self,I_OWNED,vm);
      return stringCreate2(ptr,size,vm);
   }

   result = listCreate(20,0x2,I_OWNED,vm);
   vmSetFence(vm,&fence,0,result);
      while (1)		// read at least one line
      {
	 Instance *str;
	 if (isKData && ptr[size] == '\0')
	      str = kStringCreate(ptr,self,I_OWNED,vm);
	 else str = stringCreate2(ptr,size,vm);

	 listAppend(result,fence.i1 = str,vm);

	 if (!--n) break;
	 ptr = dataNextLine(self,0,&size);
	 if (!ptr) break;
      }
   vmRemoveFence(&fence,0);
   return result;
}

    // .readNthLine(n): Read the nth line.
    // .readNthLine(n,offset): Read the nth line, from offset.
    //      --> T(line,offset to start of next line)
    // The first line is 0.
    // Cursor doesn't move
Instance *Data_readNthLine(Instance *self,pArglist arglist,pVM vm)
{
   char     *ptr;
   int       isKData = IS_KDATA(self), s;
   Instance *result;
   int64_t   i64;
   int       nthLine = (int)arglistGetInt(arglist,0,"Data.readNthLine",vm);
   DSize_t   offset = 0, end;
   size_t    size;

   if (isKData && !((Data *)self)->z)
      vmThrow(vm,E_ASSERTION_ERROR,
	      "ConstData.readNthLine(): ConstData is not Strings");

   if ((s = arglistTryToGetInt(arglist,1,&i64,0,vm)))	// offset
      offset = (DSize_t)i64;

   if (nthLine > 0) dataNthLine(self,offset,nthLine,&offset);
   dataNthLine(self,offset,1,&end);
   ptr  = (char *)D_BITS(self) + offset;
   size = end - offset;

   if (size == 0) result = emptyString;
   else if (isKData && ptr[size-1] == '\0')
	result = kStringCreate(ptr,self,I_OWNED,vm);
   else result = stringCreate2(ptr,size,vm);
   if (!s) return result;
   return tupleCreateX(vm,result,intCreate(end,vm),ZNIL);
}

    // If you use this with KData, you had better know it is Strings!
    // Unless it is an empty KData (ie cursor == len)
    // Uses the cursor
    // Returns 0 if at EoD, points into data, \0 terminated
char *dataNextString(Instance *self, size_t *lineLen)
{
   size_t  cursor = D_CURSOR(self), len;
   Byte	  *ptr, *string = &D_BITS(self)[cursor];

   dataText(self,&len);
   if (cursor >= len) return 0;
   ptr    = memchr(string,'\0',len+1); // since I know there is a trailing '\0'
   cursor = (DSize_t)(ptr - D_BITS(self) + 1);
   if (cursor > len) cursor = len;
   D_CURSOR(self) = (DSize_t)cursor;

   if (lineLen) *lineLen = ptr - string;
   return (char *)string;
}

#if 0
    // Move cursor to end of word
    // Gap doesn't move
????????what are the word chars?
static void dataNextWord(Instance *self)
{
   DSize_t  len = D_LEN(self), n = D_CURSOR(self);
   DSize_t  gapStart = 0;
   char    *bits = (char *)D_BITS(self), *gbits = bits, c;

   if (n >= len) return;		// end of buffer

   if (!IS_KDATA(self))
      { gapStart = D_GAP_START(self); gbits += D_GAP_SIZE(self); }
if in word, read until no a word char
else read until word char, read until no a word char
   for (; n < len; n++)
   {
      c = (n >= gapStart) ? gbits[n] : bits[n];
      if (c == '\0') break;
      if (c == '\n')	// EoL, if \n\0, leave cursor on \0
      {
	 DSize_t n2 = n + 1;
	 if (n2 == len) break;	// EoB
	 c = (n2 >= gapStart) ? gbits[n2] : bits[n2];
	 if (c == '\0') n++;
	 break;
      }
   }
   D_CURSOR(self) = n;
}
#endif

    // .readString() : same as readln(1) but ignores \n
    // .readString(offset) : read string at offset, ignore cursor
Instance *Data_readString(Instance *self,pArglist arglist,pVM vm)
{
   char     *ptr;
   int       isKData = IS_KDATA(self);
   int64_t   offset, 
             x=arglistTryToGetInt(arglist,0,&offset,"Data.readString",vm);
   Instance *r;
   size_t    cursor = D_CURSOR(self);

   if (isKData && !((Data *)self)->z)
      vmThrow(vm,E_ASSERTION_ERROR,
	      "ConstData.readString(): ConstData is not Strings");

   if (x) dataCursorSet(self,(size_t)offset);
   ptr = dataNextString(self,0);
   if (!ptr) vmThrowTheEnd(vm);
//   if (isKData) r = kStringCreate(ptr,self,I_OWNED,vm);
//   else		r = stringCreate(ptr,I_OWNED,vm);
   r = isKData ? kStringCreate(ptr,self,I_OWNED,vm) : 
                 stringCreate(ptr,I_OWNED,vm);
   if (x) D_CURSOR(self) = cursor;
   return r;
}

    // write/insert at the cursor, cursor moves
static void _Data_write(Data *self,pArglist arglist,int ln,pVM vm)
{
   DSize_t cpos = (DSize_t)D_CURSOR(self);

//   if (listLen(arglist,vm) == 0) return;

   cpos += (DSize_t)dataInsert(self,cpos,arglist,0,vm);
   if (ln)
   {
      dataInsertBytes((Instance *)self,cpos,(Byte *)"\n",1,0,vm);
      cpos += 1;
   }
   D_CURSOR(self) = cpos;
}

    // .write(stuff) at the cursor, cursor moves
static Instance *Data_write(Instance *self,pArglist arglist,pVM vm)
{
   _Data_write((Data *)self,arglist,0,vm);
   return self;
}

    // .writeln(stuff)
static Instance *Data_writeln(Instance *self,pArglist arglist,pVM vm)
{
   _Data_write((Data *)self,arglist,1,vm);
   return self;
}

    // Move to start of line: Move cursor to beginning of string/line
    // Gap doesn't move
static void cntlA(Instance *self)
{
   DSize_t  n = D_CURSOR(self);
   DSize_t  gapStart = 0;
   char    *bits = (char *)D_BITS(self), *gbits = bits;

   if (n == 0) return;		// Beginning of buffer

   if (!IS_KDATA(self))
      { gapStart = D_GAP_START(self); gbits += D_GAP_SIZE(self); }
   while(n--)
   {
      char c = (n >= gapStart) ? gbits[n] : bits[n];
      if (c == '\n' || c == '\0') break;
   }
   D_CURSOR(self) = n + 1;
}

    /* Starting at the cursor, move cursor to start of previous line or BoD.
     * Assumes gap not in the way.
     * A line ends at \n, \0, \n\0 but not \0\n
     * The cursor is left at the start of a line.
     * Returns 1 if cursor moves.
     * CYA if you call this with KData
     */
static int prevLine(Instance *self)
{
   DSize_t  cursor, oc = D_CURSOR(self);
   DSize_t  gapStart = 0;
   char    *bits = (char *)D_BITS(self), *gbits = bits;

   cntlA(self);
   cursor = D_CURSOR(self);
   if (cursor == 0) return (oc != cursor);	// BoD

   if (!IS_KDATA(self))
      { gapStart = D_GAP_START(self); gbits += D_GAP_SIZE(self); }
   while(--cursor)
   {
      char c = (cursor >= gapStart) ? gbits[cursor] : bits[cursor];
      if (c == '\n') break;
      if (c == '\0')	// '\0' or '\n\0'
      {
	 if (cursor)
	 {
	    DSize_t n = cursor - 1;
	    c = (n >= gapStart) ? gbits[n] : bits[n];
	    if (c == '\n') cursor--;
	 }
	 break;
      }
   }
   D_CURSOR(self) = cursor;
   cntlA(self);
   return (D_CURSOR(self) != oc);
}

    // Move to end of line: Move cursor to end of string/line
    // Gap doesn't move
static void cntlE(Instance *self)
{
   DSize_t  len = D_LEN(self), n = D_CURSOR(self);
   DSize_t  gapStart = 0;
   char    *bits = (char *)D_BITS(self), *gbits = bits, c;

   if (n >= len) return;		// end of buffer

   if (!IS_KDATA(self))
      { gapStart = D_GAP_START(self); gbits += D_GAP_SIZE(self); }
   for (; n < len; n++)
   {
      c = (n >= gapStart) ? gbits[n] : bits[n];
      if (c == '\0') break;
      if (c == '\n')	// EoL, if \n\0, leave cursor on \0
      {
	 DSize_t n2 = n + 1;
	 if (n2 == len) break;	// EoB
	 c = (n2 >= gapStart) ? gbits[n2] : bits[n2];
	 if (c == '\0') n++;
	 break;
      }
   }
   D_CURSOR(self) = n;
}

    // Where is the cursor in the line? One based, cursor doesn't move
    // Gap doesn't move
    // one	two	one\ntwo
    // 012345678901	01234567 [4] kinda doesn't exit
    // 123456789012
static size_t cursorPos(Instance *self,int expandTabs)
{
   DSize_t  oc = D_CURSOR(self), pos, n;
   DSize_t  gapStart = 0;
   char    *bits = (char *)D_BITS(self), *gbits = bits, c;

   cntlA(self); n = D_CURSOR(self); D_CURSOR(self) = oc;
   if (!expandTabs) return (oc - n + 1);	// one based

   if (!IS_KDATA(self))
      { gapStart = D_GAP_START(self); gbits += D_GAP_SIZE(self); }

   for (pos = 0; n < oc; n++,pos++)
   {
      c = (n >= gapStart) ? gbits[n] : bits[n];
      if (c == '\t') pos |= 7;
   }
   return (pos + 1);
}

    // Move cursor to position within line/string (which is one based)
    // Gap doesn't move
static void cursorSetPos(Instance *self,size_t pos,int expandTabs)
{
   DSize_t  a,b,w, oc = D_CURSOR(self);

   if (pos) pos--;	// let's do this zero based

   cntlE(self); b = D_CURSOR(self); D_CURSOR(self) = oc;  // EoL
   cntlA(self); a = D_CURSOR(self);			  // BoL
   w = b - a;		// width of this line

   if (!expandTabs)
   {
      a += (DSize_t)pos;
      if (a > b) a = b;
      D_CURSOR(self) = a;
   }
   else		// expand tabs
   {
      DSize_t  gapStart = 0, n,z;
      char    *bits = (char *)D_BITS(self), *gbits = bits, c;

      if (!IS_KDATA(self))
	 { gapStart = D_GAP_START(self); gbits += D_GAP_SIZE(self); }

      for (n = a, z = 0; w--; n++,z++)
      {
	 c = (n >= gapStart) ? gbits[n] : bits[n];
	 if (c == '\t') z |= 7;
	 if (z >= pos) break;
      }
      D_CURSOR(self) = n;
   }
}

    /* .seek() --> cursor
     * .seek(offset): Set cursor to offset, returns new cursor. IndexError
     * .seek(offset,n): Set cursor to offset, move n lines. TheEnd
     * .seek(offset,0): Beginning of line
     * .seek(Void,n): Move cursor n lines.
     * Offset & n can be negative.
     * Returns: current cursor
     */
void dataSeek(Instance *self,Offset_t offset, pVM vm)
   { D_CURSOR(self) = (DSize_t)constrainOffset(offset,D_LEN(self),1,0,vm); }

Instance *Data_seek(Instance *self,pArglist arglist,pVM vm)
{
   int64_t i64;

   if (arglistTryToGetInt(arglist,1,&i64,0,vm))	// .seek(Void|offset,n)
   {
      Instance *i = arglistGet(arglist,0,"Data.seek",vm);
      long int  n = (long)i64;

      if (IS_KDATA(self) && !((Data *)self)->z)
	 vmThrow(vm,E_ASSERTION_ERROR,
		 "ConstData.seek(): ConstData is not Strings");

      if (i != Void) dataSeek(self,(Offset_t)convertToInt(i,vm),vm);
      if (n > 0)
      {
#if 0
	 while (0 < n-- && dataNextLine(self,0,0)) ;
	 if (n != -1) vmThrowTheEnd(vm);
#else
	 DSize_t *cursor = &D_CURSOR(self);
	 if (!dataNthLine(self,*cursor,(size_t)n,cursor)) vmThrowTheEnd(vm);
#endif
      }
      else if (n < 0)
      {
	 while (0 > n++ && prevLine(self)) ;
	 if (n != 1) vmThrowTheEnd(vm);
      }
      else cntlA(self);
   }
   else		// .seek([offset])
   {
      Offset_t idx;
      if (arglistConstrainOffset(arglist,0,D_LEN(self),0x12,&idx,0,vm))
		D_CURSOR(self) = (DSize_t)idx;
   }
   return intCreate(D_CURSOR(self),vm);
}

    // .inlineCursor([False]) --> position in line
    // .inlineCursor(True)    --> position in line, expanding tabs
    // The following all return self
    // .inlineCursor(pos) --> set character cursor in line, no tab expansion
    // .inlineCursor(pos,True) --> set character cursor in line, expand tabs
    // .inlineCursor(0|1) --> Move cursor to start of line.
    // .inlineCursor(*,expandTabs=False) --> Move cursor to end of line
Instance *Data_inlineCursor(Instance *self,pArglist arglist,pVM vm)
{
   Instance *i0 = arglistTryToGet(arglist,0);
   Instance *i1 = arglistTryToGet(arglist,1);
   int	     expandTabs = (i1 == BoolTrue);
   int64_t   n;
   size_t    pos;

   // this code doesn't care where the gap is, only if there a gap

   if (!i0 || i0 == BoolFalse) return intCreate(cursorPos(self,0),vm);
   if (i0 == BoolTrue)	       return intCreate(cursorPos(self,1),vm);

   if (i0 == Star) cntlE(self);
   else
   {
      n = convertToInt(i0,vm);
      pos = (n < 2) ? 0 : (size_t)n;
      cursorSetPos(self,pos,expandTabs);
   }
   return self;
}

#if 1
// Boyer-Moore string search algorithm, Wikipedia
#define ALPHABET_LEN 256
#define NOT_FOUND patlen
//#define MAX(a,b) ((a < b) ? b : a)  // zklUtil.h
 
// delta1 table: delta1[c] contains the distance between the last
// character of pat and the rightmost occurrence of c in pat.
// If c does not occur in pat, then delta1[c] = patlen.
// If c is at string[i] and c != pat[patlen-1], we can
// safely shift i over by delta1[c], which is the minimum distance
// needed to shift pat forward to get string[i] lined up 
// with some character in pat.
// this algorithm runs in alphabet_len+patlen time.
static void make_delta1(int *delta1, uint8_t *pat, int32_t patlen) {
    int i;
    for (i=0; i < ALPHABET_LEN; i++) {
        delta1[i] = NOT_FOUND;
    }
    for (i=0; i < patlen-1; i++) {
        delta1[pat[i]] = patlen-1 - i;
    }
}
 
// true if the suffix of word starting from word[pos] is a prefix 
// of word
static int is_prefix(uint8_t *word, int wordlen, int pos) {
    int i;
    int suffixlen = wordlen - pos;
    // could also use the strncmp() library function here
    for (i = 0; i < suffixlen; i++) {
        if (word[i] != word[pos+i]) {
            return 0;
        }
    }
    return 1;
}
 
// length of the longest suffix of word ending on word[pos].
// suffix_length("dddbcabc", 8, 4) = 2
static int suffix_length(uint8_t *word, int wordlen, int pos) {
    int i;
    // increment suffix length i to the first mismatch or beginning
    // of the word
    for (i = 0; (word[pos-i] == word[wordlen-1-i]) && (i < pos); i++);
    return i;
}
 
// delta2 table: given a mismatch at pat[pos], we want to align 
// with the next possible full match could be based on what we
// know about pat[pos+1] to pat[patlen-1].
//
// In case 1:
// pat[pos+1] to pat[patlen-1] does not occur elsewhere in pat,
// the next plausible match starts at or after the mismatch.
// If, within the substring pat[pos+1 .. patlen-1], lies a prefix
// of pat, the next plausible match is here (if there are multiple
// prefixes in the substring, pick the longest). Otherwise, the
// next plausible match starts past the character aligned with 
// pat[patlen-1].
// 
// In case 2:
// pat[pos+1] to pat[patlen-1] does occur elsewhere in pat. The
// mismatch tells us that we are not looking at the end of a match.
// We may, however, be looking at the middle of a match.
// 
// The first loop, which takes care of case 1, is analogous to
// the KMP table, adapted for a 'backwards' scan order with the
// additional restriction that the substrings it considers as 
// potential prefixes are all suffixes. In the worst case scenario
// pat consists of the same letter repeated, so every suffix is
// a prefix. This loop alone is not sufficient, however:
// Suppose that pat is "ABYXCDBYX", and text is ".....ABYXCDEYX".
// We will match X, Y, and find B != E. There is no prefix of pat
// in the suffix "YX", so the first loop tells us to skip forward
// by 9 characters.
// Although superficially similar to the KMP table, the KMP table
// relies on information about the beginning of the partial match
// that the BM algorithm does not have.
//
// The second loop addresses case 2. Since suffix_length may not be
// unique, we want to take the minimum value, which will tell us
// how far away the closest potential match is.
void make_delta2(int *delta2, uint8_t *pat, int32_t patlen) {
    int p;
    int last_prefix_index = patlen-1;
 
    // first loop
    for (p=patlen-1; p>=0; p--) {
        if (is_prefix(pat, patlen, p+1)) {
            last_prefix_index = p+1;
        }
        delta2[p] = last_prefix_index + (patlen-1 - p);
    }
 
    // second loop
    for (p=0; p < patlen-1; p++) {
        int slen = suffix_length(pat, patlen, p);
        if (pat[p - slen] != pat[patlen-1 - slen]) {
            delta2[patlen-1 - slen] = patlen-1 - p + slen;
        }
    }
}
 
static uint8_t* boyer_moore(
uint8_t *string, size_t stringlen, uint8_t *pat, size_t patlen) {
//!!! gack! int i & j iterates over a size_t?!?
    int i;
    int delta1[ALPHABET_LEN];
//    int *delta2 = (int *)malloc(patlen * sizeof(int));

    int dbuf[1000], *delta2=dbuf;
    int freeIt=0;

    if (patlen>=1000){
       delta2 = (int *)malloc(patlen * sizeof(int));
       freeIt=1;
    }

    make_delta1(delta1, pat, patlen);
    make_delta2(delta2, pat, patlen);
 
    // The empty pattern must be considered specially
    if (patlen == 0) return 0;
 
    i = patlen-1;
    while (i < (int)stringlen) {
        int j = patlen-1;
        while (j >= 0 && (string[i] == pat[j])) {
            --i;
            --j;
        }
        if (j < 0) {
            if (freeIt) free(delta2);
            return (string + i+1);
        }
 
        i += MAX(delta1[string[i]], delta2[j]);
    }
    if (freeIt) free(delta2);
    return 0;
}
#endif

    // .findString(text,offset=0,len=*) --> offset|Void
    // Match a string, including trailing '\0'
    // Entire text has to fit into window.
    // .findString("") is a special case: find end of string
    // Cursor doesn't move
Instance *Data_findString(Instance *self,pArglist arglist,pVM vm)
{
#if 0
   char	  *text   = arglistGetOnlyString(arglist,0,"Data.findString",vm);
   size_t  tlen   = strlen(text);
   size_t  len;
   char	  *start  = (char *)dataText(self,&len), *end,*wend;
   char	  *ptr;
   size_t  offset = 0;

   if (IS_KDATA(self) && !((Data *)self)->z)
      vmThrow(vm,E_ASSERTION_ERROR,
	      "ConstData.findString(): ConstData is not Strings");

   if (listLen(arglist,vm) != 1)	// .findString(text,offset [,len])
      arglistGetChunk(arglist,1,0x103, len, &offset,&len, 0,vm);

   if (!len) return Void;

   start += offset; end = start + len;
   if (tlen == 0)	// .findString("") --> find end of string
   {
      ptr = (char *)memchr(start,'\0',end - start);
      if (ptr && ptr != end)	// EOB doesn't count
      {
	 len = ptr - start;
	 return intCreate(offset + len,vm);
      }
      return Void;
   }

   wend = start + len - tlen;
//   if (wend < start) return Void;
   for (ptr = start; ptr < wend; )
   {
      char *m = strstr(ptr,text);	// remember that phantom '\0'
      if (m)
      {
	 if (m > wend) return Void;
	 if (m[tlen] == '\0')
	 {
	    offset = offset + (m - start);
	    return intCreate(offset,vm);
	 }
		// else partial match == no match eg "a" doesn't match "ab"
	 ptr = m + tlen;
	 continue;
      }
      	// find start of next string
      ptr = (char *)memchr(ptr,'\0', end - ptr) + 1;  //!!!rawmemchr() is faster
      if (ptr == (char *)1) break;	// blew past the end of data
   }
   return Void;
#else

   char	  *text   = arglistGetOnlyString(arglist,0,"Data.findString",vm);
   size_t  tlen   = strlen(text);
   size_t  len;
   char	  *start  = (char *)dataText(self,&len), *end;
   char	  *ptr;
   size_t  offset = 0;

   if (IS_KDATA(self) && !((Data *)self)->z)
      vmThrow(vm,E_ASSERTION_ERROR,
	      "ConstData.findString(): ConstData is not Strings");

   if (listLen(arglist,vm) != 1)	// .findString(text,offset [,len])
      arglistGetChunk(arglist,1,0x103, len, &offset,&len, 0,vm);

   if (!len) return Void;

   start += offset; end = start + len;
   if (tlen==0)	// .findString("") --> find end of string ie find a zero
   {
      ptr = (char *)memchr(start,'\0',end - start);
      if (ptr && ptr != end)	// EOB doesn't count
      {
	 len = ptr - start;
	 return intCreate(offset + len,vm);
      }
      return Void;
   }

   ptr = (char *)boyer_moore((uint8_t *)start,len,(uint8_t *)text,tlen+1);
   if(!ptr) return Void;
   return intCreate(ptr-start+offset,vm);
#endif
}

    // .find(text|Data, offset=0,num=*) --> offset | Void
Instance *Data_find(Instance *self,pArglist arglist,pVM vm)
{
#if 0
   size_t  len;
   char   *start  = (char *)dataText(self,&len), *end = start + len;
   Byte	  *text   = (Byte *)arglistGetOnlyString(arglist,0,"Data.find",vm);
   size_t  tlen   = strlen((char *)text);
   size_t  offset = 0;

   if (listLen(arglist,vm) != 1)	// .find(text,offset [,len])
      arglistGetChunk(arglist,1,0x103, D_LEN(self), &offset,&len, 0,vm);

   if (!tlen) return Data_findString(self,arglist,vm);  // .find("")

   if (len == 0) return Void;

   {   	// This is almost the same as findString except partial matches work
      char   *ptr, *wend;

      if (IS_KDATA(self) && !((Data *)self)->z)
	 vmThrow(vm,E_ASSERTION_ERROR,
		 "ConstData.find(): ConstData is not Strings");

      start += offset; end = start + len;

      wend = start + len - tlen;
//      if (wend < start) return Void;
      for (ptr = start; ptr <= wend; )
      {
	 char *m = strstr(ptr,(char *)text);	// search this string
	 if (m)
	 {
	    if (m > wend) return Void;
	    offset = offset + (m - start);
	    return intCreate(offset,vm);
	 }
	 	// didn't find text, move to start of next string
	 ptr = (char *)memchr(ptr,'\0', end - ptr) + 1;
	 if (ptr == (char *)1) break;	// blew past the end of data
      }
   }
   return Void;

#else
   size_t    len, offset = 0;
   char     *start = (char *)dataText(self,&len), *ptr;
   Instance *pi;

   if (listLen(arglist,vm) != 1)	// .find(pattern,offset [,len])
      arglistGetChunk(arglist,1,0x103, D_LEN(self), &offset,&len, 0,vm);

   pi = arglistGet(arglist,0,"Data.find",vm);
   if (TYPEO(pi)==DataType)
      ptr = (char *)boyer_moore((uint8_t *)(start+offset),len,
                    D_BITS(pi),D_LEN(pi));
   else   // not finding bytes == find text
   {
      char   *text = arglistGetOnlyString(arglist,0,"Data.find",vm);
      size_t  tlen = strlen(text);

      if (!tlen)  // .find("") == .find(Data(Void,0))
	 return Data_findString(self,arglist,vm);

      ptr = (char *)boyer_moore((uint8_t *)(start+offset),len,
		      (uint8_t *)text,tlen);
   }
   if(!ptr) return Void;
   return intCreate(ptr-start,vm);
#endif
}

    // .index(x)
Instance *Data_index(Instance *self,pArglist arglist,pVM vm)
{
   Instance *i = Data_find(self,arglist,vm);
   if (Void == i) vmThrow(vm,E_INDEX_ERROR,0);
   return i;
}

    // .matches(pattern,offset=0,flags=1)
static Instance *Data_matches(Instance *self,pArglist arglist,pVM vm)
{
   size_t   offset  = 0,len;
   char    *text    = (char *)dataText(self,&len);
   char    *pattern = arglistGetOnlyString(arglist,0,0,vm);
   int64_t  flags   = 1;

   if (arglistTryToGetInt(arglist,1,&flags,0,vm))
      offset = (size_t)constrainOffset((Offset_t)flags,len,0,0,vm);
   arglistTryToGetInt(arglist,2,&flags,0,vm);
   return boolCreate(wildmat(text+offset,pattern,(int)flags));
}

    // .clear([stuff])
Instance *Data_clear(Instance *self,pArglist arglist,pVM vm)
{
   dataZero((Data *)self);
   return Data_append(self,arglist,vm);
}

#define GET_BYTES	     30
#define GET_CHARS	     32
#define GET_LINES	     33
#define GET_STRIPPED_LINES   34
#define GET_STRINGS	     35
#define GET_STRIPPED_STRINGS 36

static void *int2howza(Instance *d,int n,int useTmp)
{
   if(useTmp) n = D_HOWZA_TMP(d);
   D_HOWZA_TMP(d) = D_HOWZA(d);		// reset
   switch(n)
   {
      case  0: n = GET_BYTES;		  break; // Byte stream
      default:
      case  1: n = GET_LINES;		  break; // Lines, ala file
      case 11: n = GET_STRIPPED_LINES;	  break; // stripped lines
      case  2: n = GET_STRINGS;		  break; // Strings
      case 12: n = GET_STRIPPED_STRINGS;  break; // stripped strings
      case  3: n = GET_CHARS;		  break; // Characters
   }
   if(!D_OBEY_CURSOR(d)) D_CURSOR(d) = 0;
   D_OBEY_CURSOR(d) = 0;
   return (void *)(size_t)n;
}

#define d2howza(d) int2howza(d,((Data *)d)->howza,1)

static Instance *_dataGetter(Instance *self,size_t n,void *t,size_t _,pVM vm)
{
   char *ptr;
   int   isKData = IS_KDATA(self), strip = 0;

   switch((int)t)
   {
      case GET_STRIPPED_STRINGS:
         strip = 1;
	 // fall through
      case DATA_TREAT_AS_STRINGS:
      case GET_STRINGS:	// strings
      {
	 size_t sz, nsz;
	 ptr = dataNextString(self,&sz);
	 if (!ptr) return 0;
	 if (strip)
	 {
	    ptr = stripWS(ptr,sz,&nsz,1);
	    if(nsz != sz) return stringCreate2(ptr,nsz,vm);
	 }
	 if (isKData) return kStringCreate(ptr,self,I_OWNED,vm);
	 return stringCreate(ptr,I_OWNED,vm);
      }
      case GET_STRIPPED_LINES:
         strip = 1;
	 // fall through
      case DATA_TREAT_AS_DATA:
      case GET_LINES:		//  lines
      {
	 size_t sz;
	 ptr = dataNextLine(self,strip,&sz);
	 if (!ptr) return 0;

	 if (isKData && ptr[sz] == '\0')	// KData is const
	    return kStringCreate(ptr,self,I_OWNED,vm);
	 return stringCreate2(ptr,sz,vm);
      }
      case GET_CHARS:				// characters
	 n+=D_CURSOR(self);
	 if (n >= D_LEN(self)) return 0;
	 ptr = (char *)dataChunkAt(self,n,1);
	 return stringCreate2(ptr,1,vm);
      case GET_BYTES:				// bytes
      {
	 Byte b;
	 n+=D_CURSOR(self);
	 if (n >= D_LEN(self)) return 0;
	 b = *dataChunkAt(self,n,1);
	 return intCreate(b,vm);
      }
   } //switch
   return 0;
}

   // Data.reduce(...)
Instance *Data_reduce(Instance *self,pArglist arglist, pVM vm)
{
// IS_KDATA(self) is OK because if DATA_TREAT_AS_DATA, I won't want strings
   return zreduce(self,_dataGetter,d2howza(self),arglist,0,vm);
}

   // Data.filter(...), mode defauts to same Data mode
Instance *Data_filter(Instance *self,pArglist arglist, pVM vm)
{
// IS_KDATA(self) is OK because if DATA_TREAT_AS_DATA, I won't want strings
   return zfilter(self,
       (D_TREAT_AS(self)==DATA_TREAT_AS_DATA) ? ZA_DATA_D : ZA_DATA_S,
       _dataGetter, d2howza(self), arglist,0,vm);
}

   // Data.pump(...)
Instance *Data_pump(Instance *self,pArglist arglist, pVM vm)
{
// IS_KDATA(self) is OK because if DATA_TREAT_AS_DATA, I won't want strings

   return pump(self,PMP_OK2CNT,_dataGetter,d2howza(self),arglist,0,vm);
}

#if 0
typedef struct {
   int start,stop,step;
   int n,offsets[20];
   Instance *sink;
} DChunk;
static Instance *_dataChunker(Instance *self,size_t n,void *chk,size_t _,pVM vm)
{
   char *ptr;
   int   isKData = IS_KDATA(self);
   DChunk *chunk = (DChunk *)chk;

   Byte b;
   if (n >= D_LEN(self)) return 0;
   b = *dataChunkAt(self,n,1);
   return intCreate(b,vm);

   return 0;
}

   // Data.pumpChunks(Void.Read,T(start,stop,step,a,b,c,d..., sink, ...)
Instance *Data_pumpChunks(Instance *self,pArglist arglist, pVM vm)
{
   
}
#endif

    // .walker(howza=howza()):
    //    0 (bytes), 1 (lines), 2 (strings), 3 (characters)
    //  +10: stripped (for 1 & 2)
Instance *Data_walker(Instance *self,pArglist arglist, pVM vm)
{
   int64_t n;

   if (!arglistTryToGetInt(arglist,0,&n,0,vm))	// .walker()
      n = ((Data *)self)->howza;
   return walkerCreate(self, _dataGetter,int2howza(self,(int)n,0), 0,vm);
}

    // .shuffle --> List.shuffle --> Utils.Helpers.shuffle
Instance *List_shuffle(Instance *,pArglist,pVM);

static const MethodTable dataMethods[] = 
{
   "toData",		Object_noop,
   "toString",		Data_toString,
   "toBool",		Data_toBool,
   "toList",		Data_toList,
   "toBigEndian",	Data_toBigEndian,
   "toLittleEndian",	Data_toLittleEndian,

   "create",		Data_create,
   "copy",		Data_copy,
   "len",		Data_len,

   "open",		Data_create,
   "close",		Object_noop,	// Stream
   "flush",		Object_noop,	// Stream
   "append",		Data_append,
   "extend",		Data_append,
   "write",		Data_write,
   "writeln",		Data_writeln,
   "read",		Data_read,
   "read1",		Data_read1,
   "readln",		Data_readln,
   "readNthLine",	Data_readNthLine,
   "readString",	Data_readString,
   "__sGet",		Data_sGet,
   "get",		Data_sGet,
   "charAt",		Data_charAt,
   "__sSet",		Data_sSet,
   "set",		Data_set,
   "fill",		Data_fill,
   "bytes",		Data_bytes,
   "pop",		Data_pop,


   "insert",		Data_insert,
   "del",		Data_shrink,
   "clear",		Data_clear,
   "replace",		Data_replace,
   "swap",		Data_swap,
   "shuffle",		List_shuffle,

   "mode",		Data_mode,
   "howza",		Data_howza,
   "startAtCursor",	Data_startAtCursor,

   "seek",		Data_seek,
   "inlineCursor",	Data_inlineCursor,
   "find",		Data_find,
   "findString",	Data_findString,
   "index",		Data_index,
   "matches",		Data_matches,

   "getMark",		Data_getMark,
   "setMark",		Data_setMark,

   "walker",		Data_walker,
   "reduce",		Data_reduce,
   "pump",		Data_pump,
   "filter",		Data_filter,
   0,			0
};

/* ******************************************************************** */
/* *************************** Properties ***************************** */
/* ******************************************************************** */

    // .text --> String
//!!! if this was readonly, could use kcString
static Instance *Data_text(Instance *self, pVM vm)
   { return stringCreate((char *)dataText(self,0),I_OWNED,vm); }

    // .cursor --> Int
Instance *Data_cursor(Instance *self,pVM vm)
   { return intCreate(D_CURSOR(self),vm); }

    // .pork --> Int
Instance *Data_pork(Instance *self,pVM vm)
   { return intCreate(D_SIZE(self),vm); }

static const PropertyTable properties[] = 
{
   "text",	Data_text,
   "cursor",	Data_cursor,
   "pork",	Data_pork,
   "isReadOnly",(pProperty)Bool_nope, // really a pMethod but doesn't use args
   0,		0
};

/* ******************************************************************** */
/* ***************************** Op Codes ***************************** */
/* ******************************************************************** */

static Instance *Data_add(Instance *self,Instance *X,pVM vm)
{
//!!!! I'm not so sure about this one: d + "one" modifies d
   dataAppend(self,X,vm);
   return self;
}

    // Works for Data and KData
static int _eq(Instance *self,Instance *X)
{
   int st = TYPEO1(self), xt = TYPEO(X);

   if (xt != DataType) return 0;
   if (D_LEN(self) != D_LEN(X)) return 0;
   if (st == DataType && !self->iflag) closeGap((Data *)self); 
   if (!X->iflag) closeGap((Data *)X);
   return (0 == memcmp(D_BITS(self),D_BITS(X),D_LEN(self)));
}

Instance *Data_eq(Instance *self,Instance *X,pVM vm)
   { return boolCreate(_eq(self,X)); }

Instance *Data_neq(Instance *self,Instance *X,pVM vm)
   { return boolCreate(!_eq(self,X)); }

static const OpcodeTable opcodes[] = 
{
   OP_EQ,	(pOp)Data_eq,
   OP_NEQ,	(pOp)Data_neq,
   OP_ADD,	(pOp)Data_add,
   0,		0
};


Instance *VData;		// a place holder so Data can be found

//////////////////////// Data Sink
static int dataToSink(
   Instance *self,void *_aggie,size_t lenHint, size_t sbsz,size_t *_, pVM vm)
{
   ZAgg *aggie = (ZAgg *)_aggie;
   int how     = ZA_DATA_D;

   if(!aggie) return ZA_2SINK_OK; 	// sbUsed query

   if((size_t)self<10){ how = (size_t)self; self = VData; }

   aggie->how = ZA_2M;

   if (self == VData)  // mother Data
   {
      //!!!??? I could buffer data in sandbox and copy on close
      self = dataCreate(500,I_OWNED,vm);
      if (how == ZA_DATA_S) dataMode(self,DATA_TREAT_AS_STRINGS,vm);
   }
   aggie->i = self;
   aggie->dst.msink.write = Data_append;
   aggie->dst.msink.close = Object_noop;

   return ZA_2SINK_OK;
}

//static pMethod in_data_methods(Instance *ignore, register char *str);

void dataConstruct(void)
{
   constructObject(&DataObject,DataType, dataMethods, properties, opcodes, NoVM);
   DataObject.freeMe	   = dataFree;
//   DataObject.methodSearch = in_data_methods;
   DataObject.threadSafe   = 0;
   DataObject.isize	   = sizeof(Data);
   DataObject.toSink	   = dataToSink;
   DataObject.createReturnsSelf = 1;

   DataObject.isBInstance  = 1;
   ibucketReserve(&DataObject,812,&dBuckets,0,NoVM);	// CuckooTuple also

   VData = dataCreate(0,I_UNTOUCHABLE,NoVM);
   vaultAdd("",VData,NoVM);
}



///////////////////////////////////////////////////////////////////
// zkl extractTable.zkl < data.c | gperf | zkl gperf.zkl -i data


