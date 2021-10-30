/* arglist.c : Arglist utilities.
 * Look into arglists but don't change them.
 * Arglists are Tuples, but this code works with Lists too.
 * 
 * Copyright (c) 2006,2007,2008-12,2013 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

// Statement: arglists are less then 32k (15 bits) in length
// Not that I chack

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#include <stdio.h>

#define __NOT_A_DLL
#define __STRING_INTERNALS

#include "zklObject.h"
#include "zklList.h"
#include "zklMethod.h"
#include "zklNumber.h"
#include "zklString.h"
#include "zklUtil.h"


Instance *NoArglist;

void arglistConstruct(void) { NoArglist = emptyTuple; }

int arglistLen(pArglist arglist,pVM vm) { return (int)listLen(arglist,vm); }

    // 0 (or "") is OK for msg
//!!!??? every method & fcn have a prototype and then I can
// say the name of the missing arg. How to backtrack to that prototype?
void _missingArg(pArglist arglist,int n,char *msg,pVM vm)
{
   char buf[100];
   int  z = 0;
   if (msg && *msg) z = sprintf(buf,"%s: ",msg);
   sprintf(buf+z,"arglist[%d]: Not there",n);
   vmThrow(vm,E_MISSING_ARG,buf);
}

Instance *arglistGet(pArglist arglist, int n, char *msg, pVM vm)
{
   Instance *pi = listGet(arglist,n);
   if (!pi) _missingArg(arglist,n,msg,vm);
   return pi;
}

    // Try to get the nth arg from the arglist. If that arg isn't there,
    // return 0.
Instance *arglistTryToGet(pArglist arglist,int n)
   { return listGet(arglist,n); }


//////////////////////////////////////////////////////////////// TO_*

    // Important! You have to protect against GC if a conversion is done
    // Can GC
#if 0
Instance *arglistTryToGetT(pArglist arglist, int n, int toFcnOffset, pVM vm)
{
   Instance *i = listGet(arglist,n);
   if (i) return convertTo(i,toFcnOffset,vm);
   return 0;
}
#endif

Instance *arglistGetT(pArglist arglist,int n, int TO_nm, char *msg,pVM vm)
   { return convertTo(arglistGet(arglist,n,msg,vm),TO_nm,vm); }

Instance *convertTo(Instance *i,int TO_nm,pVM vm)
{
   pMethod method;

   #if USE_POINTER_INTS
      if (IS_PtrInt(i))
         return ptrIntDoMethod(i,0,I_METHOD(Zero,TO_nm),NoArglist,vm);
   #endif // USE_POINTER_INTS
   method = I_METHOD(i,TO_nm);
   return method(i,NoArglist,vm);
}

//////////////////////////////////////////////////////////////// By type

    // eg FcnType, DataType, NOT NativeType (see BI versions)

static void _typeError(char *msg, Instance *i, int n, char *type, pVM vm)
{
   char buf[100];
   int  z = 0;
   if (msg && *msg) z = sprintf(buf,"%s: ",msg);
   sprintf(buf+z,"arglist[%d]: Expected %s, got %s",n,type,typeToName(TYPEO(i)));
   vmThrow(vm,E_TYPE_ERROR,buf);
}

    // This is vile but I'm stumped
static Instance *_typeCheck(int type,Instance *i, int n, char *msg, pVM vm)
{
   int t = TYPEO(i), s;
   switch(type)
   {
      default:       s = (t != type); 			    break;
      case ListType: s = (t != ListType && t != TupleType); break;
      case DeferredType: 
	 s = (t != FcnType && (t==DeferredType && !isPartialFcn(i))); break;
   }
   if (s) _typeError(msg,i,n,typeToName(type),vm);
   return i;
}

Instance *arglistGetBT(pArglist arglist,int n,int type,char *msg,pVM vm)
{
   Instance *pi = arglistGet(arglist,n,msg,vm);
   return _typeCheck(type,pi,n,msg,vm);
}

Instance *arglistTryToGetBT(pArglist arglist,int n,int type, char *msg, pVM vm)
{
   Instance *pi = arglistTryToGet(arglist,n);
   if (pi) return _typeCheck(type,pi,n,msg,vm);
   return 0;
}

//////////////////////////////////////////////////////////////// String

    // No conversions: try to get a String

char *arglistGetOnlyString(pArglist arglist,int n,char *msg,pVM vm)
   { return ZKL_STRING(arglistGetBT(arglist,n,StringType,msg,vm)); }

char *arglistTryToGetString(pArglist arglist,int n, char *msg, pVM vm)
{
   Instance *ps = arglistTryToGetBT(arglist,n,StringType,msg,vm);
   if (ps) return ZKL_STRING(ps);
   return 0;
}

////////////// Returns String, you need to protect result

    // Convert as needed to get a String
Instance *arglistGetString(pArglist arglist,int n,char *msg,pVM vm)
   { return arglistGetT(arglist,n,TO_STRING,msg,vm); }

    // Creates String, so you gotta protect result
Instance *arglistConcat(int n,pArglist arglist,pVM vm)
{
   Instance *text;
   Fence     fence;
   ZBText    buf;

   vmSetFence(vm,&fence,0,zbtextInit(&buf,vm));
      for ( ; (text = listGet(arglist,n)); n++) zbtextAppendI(&buf,text,vm);
      zbtextClose(&buf,vm);
   vmRemoveFence(&fence,0);
   return buf.string;
}

//////////////////////////////////////////////////////////////// Bool

    // Strict: False/0 --> 0, True/1 --> 1, no arg --> default, else error
int arglistTryToGetBool(pArglist arglist, int n, int B, char *msg, pVM vm)
{
   Instance *pi = listGet(arglist,n);
   if (!pi) return B;
   if (IS_ZERO(pi) || pi == BoolFalse) return 0;
   if (IS_ONE(pi)  || pi == BoolTrue)  return 1;
   _typeError(msg,pi,n,"Bool/1/0",vm);
   return 0;
}

    // False/0 --> 0, True/1 --> 1
int arglistGetBool(pArglist arglist, int n, char *msg, pVM vm)
{
   Instance *pi = arglistGet(arglist,n,msg,vm);
   if (IS_ZERO(pi) || pi == BoolFalse) return 0;
   if (IS_ONE(pi)  || pi == BoolTrue)  return 1;
   _typeError(msg,pi,n,"Bool/1/0",vm);
   return 0;
}


//////////////////////////////////////////////////////////////// Numbers

    // Get Int: Int/Float/Bool --> int64
    // If you want loosie goosie conversion, use ???

int64_t arglistGetInt(pArglist arglist, int n, char *msg,pVM vm)
{
   Instance *pi = arglistGet(arglist,n,msg,vm);
   int64_t   N;
   if (!iWantInt(pi,&N,vm)) _typeError(msg,pi,n,"Int",vm);
   return N;
}

    // only set N if valid int
int arglistTryToGetInt(pArglist arglist,int n,int64_t *N, char *msg,pVM vm)
{
   Instance *pi = arglistTryToGet(arglist,n);
   if (!pi) return 0;
   if (!iWantInt(pi,N,vm)) _typeError(msg,pi,n,"Int",vm);
   return 1;
}

   // same as above but with default value
int64_t arglistTryToGetIntD(pArglist arglist,int n,int64_t d, char *msg,pVM vm)
{
   Instance *pi = arglistTryToGet(arglist,n);
   int64_t N;

   if (!pi) return d;
   if (!iWantInt(pi,&N,vm)) _typeError(msg,pi,n,"Int",vm);
   return N;
}

    // get Float: Int/Float --> double
double arglistGetFloat(Instance *arglist, int n, char *msg, pVM vm)
{
   Instance *pi = arglistGet(arglist,n,msg,vm);
   double    f;
   if (!iWantFloat(pi,&f,vm)) _typeError(msg,pi,n,"Float",vm);
   return f;
}

int arglistTryToGetFloat(pArglist arglist,int n,double *f, char *msg, pVM vm)
{
   Instance *pi = arglistTryToGet(arglist,n);
   if (!pi) return 0;
   if (!iWantFloat(pi,f,vm)) _typeError(msg,pi,n,"Float",vm);
   return 1;
}

//////////////////////////////////////////////////////////////// Instance

    // Get by Instance,
    // if an instance doesn't have a predefined type (ie natives).

    // PtrInts OK
Instance *arglistGetBI(pArglist arglist,int n, Instance *i, char *msg, pVM vm)
{
   Instance *pi = arglistGet(arglist,n,msg,vm);
   if (!objectIsType(i,pi)) _typeError(msg,pi,n,typeToName(TYPEO(i)),vm);
   return pi;
}

Instance *arglistTryToGetBI(
pArglist arglist,int n, Instance *i, char *msg, pVM vm)
{
   Instance *pi = arglistTryToGet(arglist,n);
   if (!pi) return 0;
   if (!objectIsType(i,pi)) _typeError(msg,pi,n,typeToName(TYPEO(i)),vm);
   return pi;
}

//////////////////////////////////////////////////////////////// Object

    // By Object, PtrInts OK
Instance *arglistGetBObj(pArglist arglist,int n, 
   ZKL_Object *obj, char *msg, pVM vm)
{
   Instance *pi = arglistGet(arglist,n,msg,vm);
   int idx = OBJECTI(pi)->id;
   if (idx != obj->id) _typeError(msg,pi,n,obj->name,vm);
   return pi;
}

    // By Obj ID, PtrInts OK
Instance *arglistGetBOID(pArglist arglist,int n, int oid, char *msg, pVM vm)
{
   Instance *pi = arglistGet(arglist,n,msg,vm);
   int idx = OBJECTI(pi)->id;
   if (idx != oid) _typeError(msg,pi,n,zklObjectTable[oid]->name,vm);
   return pi;
}

///////////////////// Slicing and Dicing ////////////////////////////

    /* Look for size = arglist[i], 
     * Returns:
     *    0: arglist[i] doesn't exist, size = len
     *    1: If arglist[i] == "*", size = len.
     *    2: size = min(len,(size_t)arglist[i])
     *    3: arglist[i] < 0, size == 0
     * Mungs: size (see above)
     * Throws:
     *   Conversion errors
     */
int arglistGetSize(pArglist arglist,int i, size_t len, size_t *size, pVM vm)
{
   Instance *pi = arglistTryToGet(arglist,i);

   if (pi)
   {
      int64_t n;
      if (pi == Star) { *size = len; return 1; }
      n = convertToInt(pi,vm);
      if (n < 0) { *size = 0; return 3; }
      *size = (len < n) ? len : (size_t)n;
      return 2;
   }
   *size = len;
   return 0;
}

void offsetIndexError(char *msg, Offset_t n,size_t len,pVM vm)
{
   char buf[100];
   int  z = 0;
   if (len) len--;
   if (msg && *msg) z = sprintf(buf,"%s: ",msg);
   #ifdef _MSC_VER
      sprintf(buf+z,"Offset %I64d out of range[0 .. %ld]",(int64_t)n,len);
   #else	// assume gcc
      sprintf(buf+z,"Offset %lld out of range[0 .. %zd]",(long long)n,len);
   #endif
   vmThrow(vm,E_INDEX_ERROR,buf);
}

    // Verify 0 <= arglist[i] < len
size_t arglistGetIndex(pArglist arglist,int i, size_t len, char *msg, pVM vm)
{
   int64_t idx = arglistGetInt(arglist,i,msg,vm);
   if (idx < 0 || idx >= len) offsetIndexError(msg,(Offset_t)idx,len,vm);
   return (size_t)idx;
}

    /* Look for offset = arglist[i], 
     *   where offset is an integer offset into a container.
     *   If no offset arg, --> 0|len
     * See also arglistConstrainOffset
     * Input:
     *   arglist: [...,offset,....]
     *   i: index of offset in arglist
     *   len: length of container (if offset == *)
     *   Flags: ARG_OFF_EQ_LEN_OK is assumed
     *      ARG_NO_LEN_IS_STAR: No arg, offset-->len
     *      ARG_OFF_OPTIONAL:   arglist[i] optional. eg .seek([offset])
     *      ARG_CONSTRAIN:      offset is 0<=n<=len
     *   offset: Where to put the found offset
     *   msg: Error msg. Eg "List.pop"
     *   vm:
     * Returns:
     *   0: arglist[i] doesn't exist, offset = 0
     *   1: If arglist[i] == "*", offset = len.
     *   2: offset = (Offset_t)arglist[i]
     * Mungs: offset (see above)
     * Throws:
     *   Conversion errors
     */
int arglistGetOffset(pArglist arglist,int n, size_t len,
      int flags, Offset_t *offset, char *msg, pVM vm)
{
   Instance *pi = arglistTryToGet(arglist,n);

   if (pi)	// offset exists
   {
      Offset_t n;
      if (pi == Star) { *offset = (Offset_t)len; return 1; }
      n = (Offset_t)convertToInt(pi,vm);
      if(flags & ARG_CONSTRAIN)
         if(n<0){ n+=len; n=MAX(0,n); n=MIN(n,len); }  // 0<=n<=len
      *offset = n;
      return 2;
   }
   *offset = (flags & ARG_NO_LEN_IS_STAR) ? len : 0;
   if (!(flags & ARG_OFF_OPTIONAL)) _missingArg(arglist,n,msg,vm);
   return 0;
}

    // Get offset/length, maybe from arglist
    // no offset --> 0
void arglistMaybeOL(pArglist arglist,int a0, size_t len,
      size_t *offset, size_t *count, pVM vm)
{
   int64_t idx=0,cnt;
   int     B=arglistTryToGetInt(arglist,a0+1,&cnt,0,vm);
   arglistTryToGetInt(arglist,a0,&idx,0,vm);
   if(idx<0) idx+=len; idx=MAX(0,idx); idx=MIN(idx,len);  // 0<=idx<=len
   len-=(size_t)idx;  // items after idx
   if(!B) cnt = len;
   else { cnt = MAX(0,cnt); cnt = MIN(cnt,len); }  // 0<=cnt<=len-idx
   *offset = (size_t)idx; *count = (size_t)cnt;
}


    /* Check ?[n]
     * Returns:
     *   0: offset out of range, offset unknown
     *   1: 0 <= offset <[=] length
     * If maxOK, then offset == len is OK
     * Throws: Nada
     * Note: offset is treated different from constrainOffLen
     */
__inline int checkOffset(Offset_t n,size_t length,int maxOK, size_t *offset)
{
   size_t idx;

#if OFFSET_T_IS_BIG
   if (n < 0) n += length;
   if (n < 0) return 0;		//!!!??? error?
   idx = (size_t)n;
#else			// Offset_t is int32_t
   idx = (size_t)n;
   if (n < 0) idx = length + n;
#endif

   if (idx > length) return 0;		// eg -n > length, L()[1]
   if (idx == length && !maxOK) return 0;
   *offset = idx;
   return 1;
}

Offset_t verifyOffset(Offset_t n,size_t length,int maxOK, char *msg, pVM vm)
{
   size_t offset;
   if(!checkOffset(n,length, maxOK,&offset)) offsetIndexError(msg,n,length,vm);
   return offset;
}

__inline Offset_t
constrainOffset(Offset_t n,size_t len,int maxOK, char *msg,pVM vm)
{
   size_t idx;
   if (!checkOffset(n,len,maxOK,&idx))
      offsetIndexError(msg,n,len + (maxOK!=0),vm);

   return idx;
}

    /* Flags:
     *   ARG_NO_LEN_IS_STAR (0x02): OK if offset == len.
     *   ARG_OFF_OPTIONAL 1 (0x10): arglist[i] optional. eg .seek([offset])
     * Returns: Same as arglistGetOffset
     */
int arglistConstrainOffset(pArglist arglist,int n,
		size_t len, unsigned flags, Offset_t *offset, char *msg,pVM vm)
{
   Offset_t idx;
   int	    s;
   s       = arglistGetOffset(arglist,n,len,flags & ARG_OFF_OPTIONAL,&idx,0,vm);
   *offset = constrainOffset(idx,len,flags & ARG_OFF_EQ_LEN_OK,msg,vm);
   return s;
}

   /* Two args: s[offset,number of items] == sequence chunk
    * This is getting or setting one or more elements of the sequence.
    * In this case, the subscripts are truely contained within the
    *   boundaries of the sequence.
    * ""[?,?] == ""
    * [a,b] = chunk starting at a, of b items, ie [1,2] == two items
    * []      --> error
    * [0,*]   --> [0,len] == entire sequence
    * [0,-1]  --> everything but last element
    * [n,-1]  --> everything from n on, excluding the last element
    * [*,*]   --> [len,len] == [len,0] or error
    * [-1,*]  --> [-1,len] == [len - 1, 1] == last element
    * [n,*]   --> [n,len] == [n, len - n] == everything after n (including n)
    * [a,0]   --> don't do anything
    * [len,x] --> [len,0] (for appending)
    * [N,y]   --> [len,0] for N > len
    * [-N,*]  --> [0,len] for -N == len, error if -N>len
    * [-N,M]  --> [0,len] for -N == len & M > len, error if -N>len
    * [N,0]   --> Insert, eg L(1,3)[1,0] = 2 --> L(1,2,3)
    *
    * Input:
    *   It is OK for y and length to be the same thing
    * Returns:
    *   0 : off or len or both out of range, idx,num undefined
    *   1 : 0 <= idx <[=] length, 0 <= num <= length
    * Throws:
    *   Nada
    * Note: offset is treated different from constrainOffset()
    */
int constrainOffLen(Offset_t off,Offset_t len,size_t size, int maxOK,
		    size_t *idx, size_t *num)
{
   size_t i;

   	///////// offset
#if OFFSET_T_IS_BIG
   if (off < 0) off += size;
   if (off < 0) return 0; //off = 0;	// you get one try !!!??? error case?
   i = (size_t)off;
#else			// Offset_t is int32_t
   i = (size_t)off;
   if (off < 0)
   {
      if ((size_t)-off > size) i = 0;	// size - off < 0
      else		       i = size + off;
   }
#endif
   if (i > size) i = size;	// different
   if (i == size && !maxOK) return 0;
   *idx = i;

   	////////// length
        // don't special case size == 0 or len == 0 as they can be valid
//   if (!containLen && len <= 0) return 0;
#if OFFSET_T_IS_BIG
   if (len < 0)
   {
      len = size - i + len;
      if (len < 0) return 0;
   }
   else if (i + len >= size) len = size - i;
   *num = (size_t)len;
#else
   {
      size_t n = (size_t)len;
      if (len < 0)
      {
	 n = size - i + n;
	 if (n > size) return 0;
      }
      else if (i + n >= size) n = size - i;
      *num = n;
   }
#endif

   return 1;
}

    /* Calculate a chunk from a arglist. See above for how -1 and * are
     * handled.
     *
     * []    == error
     * [i]   == [i,1]	This case should generate an index error because
     * 			Walker uses it.
     * [i,n] == [i,n]
     * [i,*] == 
     *
     * Returns:
     *   0 : Two args and something messed up but you don't want me to throw
     *       (args are ints however & num = 0).
     *       L()[1,*], L()[0,1], Data.find("foo",1000000000000)
     *   1 : One  arg:  0 <= n < len, num = 1
     *   2 : Two  args: 0 <= n < len, 0 <= num <= len
     *       Or one arg if flags indicate long length
     * Throws:
     *   IndexError:
     *   other: If can't convert arg to int
     * 
     * Input:
     *   arglist containing offset,len into object of length len
     *   i: index into arglist of offset (len is next arg), usually 0 or 1
     *   flags: Bits:
     *      [offset]: len not present, what is value of num?
     *         bit 0 == 0 : Constrain offset, num --> 1
     *         ARG_NO_LEN_IS_STAR: Constrain offset, pretend [offset,*]: num-->*
     *         ARG_OFF_EQ_LEN_OK: Don't throw if offset is out of bounds, 
     *			   offset & num --> 0, return 0.
     *         ?bit 2 == 1: OK if offset < 0, just leave it at zero.
     *         ?bit 3 == 1: OK if offset == len.
     *         ?bit 4 == 1: Void --> (len-->0)
     *      [offset,len] && len == 0
     *         bit  9 == 0: throws (0x0100)
     *         bit  9 == 1: num == 0 is OK, returns 0
     *      [?,length]:
     *         bit 10 == 1: len has to be good as is (>0, can get all len)
     *   len: length of object. OK if len & num are the same location
     *   offset: calculated offset into object
     *   num: calculated number of items to get from object
     *   msg: What to say on error (0 is OK), eg "Data.pop"
     *   VM
     * Modifies:
     *   offset : 
     *   num : 
     */
int arglistGetChunk(pArglist arglist, int i, unsigned flags,
		    size_t len, size_t *offset, size_t *num, char *msg, pVM vm)
{
   Offset_t idx,sz;

   	// The compiler catches this case (for []) but not for other methods
   if (0 == arglistGetOffset(arglist,i,len,ARG_OFF_OPTIONAL,&idx,0,vm))	// no args
      vmThrow(vm,E_INDEX_ERROR,"[] is illegal");

	// offset is handled different if 2 args:
	//   L(1)[2] --> error, L(1)[2,*] --> L()
   if (0 == arglistGetOffset(arglist,i+1,len,ARG_OFF_OPTIONAL,&sz,msg,vm)) // one arg: offset
   {
      if (!checkOffset(idx,len,0,offset))	// offset is wacked
      {
	 if (!(flags & ARG_OFF_EQ_LEN_OK)) offsetIndexError(msg,idx,len,vm);
	 *offset = *num = 0;
	 return 0;
      }

      if (flags & ARG_NO_LEN_IS_STAR)	// List.xplode(1) --> .xplode(1,*)
      {
	 sz = (Offset_t)len;
	 // now fall through & constrain length
      }
      else { *num = 1; return 1; }	// List[0] --> List[0,1]
   }

   	// we have two args
   if (!constrainOffLen(idx,sz,len,0, offset,num))	// and a boo-boo
   {
      char buf[100];
      int  z;

      if (flags & ARG_LEN_EQ_0_OK) // empty selection: L()[0,1], L(1)[0,-10]
         { *offset = *num = 0; return 0; }
      if (len) len--;
      z = 0;
      if (msg && *msg) z = sprintf(buf,"%s: ",msg);
      #ifdef _MSC_VER	// L()[1,2]=3
         sprintf(buf+z,"offset,length [%I64d,%I64d] out of range[0 .. %ld]",
		 (int64_t)idx,(int64_t)sz,len);
      #else	// assume gcc
         sprintf(buf+z,"offset,length [%lld,%lld] out of range[0 .. %zd]",
		 (long long)idx,(long long)sz,len);
      #endif
      vmThrow(vm,E_INDEX_ERROR,buf);
      // doesn't get here
   }
   return 2;
}
