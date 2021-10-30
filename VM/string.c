/* string.c : String Objects
 * 
 * Copyright (c) 2006,2007,2008-14,2015 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>
#include <ctype.h>
#if defined(__unix__)
   #include <errno.h>
#endif
#if defined(_MSC_VER) && _MSC_VER<=1500   // XP only
   #include <float.h>	// for _control87/strtodDG
   double strtodDG(const char *s00, char **se);  // dtoa.c, XP only
#endif

#define __NOT_A_DLL
#define __STRING_INTERNALS

#include "zklObject.h"
#include "zklData.h"
#include "zklFcn.h"
#include "zklList.h"
#include "zklNumber.h"
#include "zklString.h"

static ZKL_Object StringObject, KCStringObject, KStringObject, KMStringObject;
static ZKL_Object CuckooSObj;
//static ShortLongObj, ShortKCSObj;

static ZKL_String theEmptyString;
Instance *emptyString = (Instance *)&theEmptyString;
Instance *Star;

size_t utf8Len(UChar *utf,pVM);

// Instance->iflag is used to signal String or KString

/* ******************************************************************** */
/* ****************************** String ****************************** */
/* ******************************************************************** */

    // How much padding does the compiler add to a CuckooString stuct?
    // 3 for MSVC 32 bit, 3 (Linux clang 64 bit)
#define PADDING	 ( sizeof(CuckooString) - sizeof(BInstance) - 1 )

    // Make sure I got my numbers right (remember that implicit '\0')
typedef struct { BInstance instance; UChar padding[PADDING+1]; } PaddedString;
typedef char Adjust_PADDING_to_match_sizeof_CuckooString
   [2*((sizeof(PaddedString) == sizeof(CuckooString))!=0)-1];

typedef struct { BInstance instance; UChar padding[PADDING+2]; } TooMuch;
typedef char PADDING_too_small
   [2*((sizeof(TooMuch) > sizeof(CuckooString))!=0)-1];

    // How much padding does the compiler add to a ZKL_String stuct?
    // 3 for MSVC 32 bit, 7 (Linux GCC 64 bit)
#define SPADDING	 ( sizeof(ZKL_String) - sizeof(Instance) - 1 )

static IBucketHeader  kcBuckets;	// GCC won't let me forward def this
static IBucketHeader  kBuckets;
//static IBucketHeader *bigBuckets;	// will point to list.c:tsBucketList
//static size_t	      bigBucketSize = 0;

    // New string is "", len doesn't include '\0' (ie I add 1 to len)
    // --> ZKL_String | CuckooString (K strings are allocated elsewhere)
    // text is modified to point to the start of the char * buffer
Instance *stringAllocate(size_t len, char **text, int itype, pVM vm)
{
   // remember that a ZKL_String has 1 character preallocated (for the '\0')
   CuckooString *ps;  // is a BInstance + text + '\0' ie CuckooString is an overlay
   size_t sz = len + sizeof(BInstance) + 1;   // len + 5 (/64)

   if ((ps = (CuckooString *)iHazBucket(&CuckooSObj,sz,4,itype,0,vm))) {}
   else if (len <= (sizeof(ZKL_KString) - sizeof(CuckooString) + PADDING))
      ps = (CuckooString *)ibucketAllocate(&kBuckets,&CuckooSObj,itype,1,vm);
   else		// nope, need a malloc
   {
      ZKL_String *s;
      if (len <= SPADDING) len = 0;  // SPADDING chars will fit in a ZKL_String
      else len -= SPADDING;

      s = (ZKL_String *)
	     instanceAllocate(sizeof(ZKL_String) + len, &StringObject, 1,vm);
      I_FLAG(s) = 1;			// for ZKL_STRING macro
      *(*text = s->value) = '\0';	// usually redundant but safe

      return addToCollectables((Instance *)s,itype,vm);
   }
   *(*text = ps->value) = '\0';	// usually redundant but safe
   I_FLAG2(ps) = 1;		// not malloc'd
   return (Instance *)ps;
}

     // Return a pointer to the text in a [P|K|KC]String
char *stringText(Instance *s) { return ZKL_STRING(s); }

Instance *stringCreate(char *text, int itype, pVM vm)
{
   char     *ptr;
   Instance *s;

   if (!text) vmThrow(vm,E_ASSERTION_ERROR,"stringCreate(NULL)");
   if (!*text) return emptyString;
   if (text[0]=='*' && text[1]=='\0') return Star;

	// String already has space for the '\0'
   s = stringAllocate(strlen(text),&ptr,itype,vm);
   strcpy(ptr,text);
   return s;
}

    // len doesn't include \0, that is implicit (ie I add it)
    // result is owned
Instance *stringCreate2(char *bytes,size_t len, pVM vm)
{
   char     *ptr;
   Instance *s;

   if (len == 0) return emptyString;

   s = stringAllocate(len,&ptr,I_OWNED,vm);
   if (!s) return emptyString;	//!!!! uhhhh, shouldn't this throw no mem?
   memcpy(ptr,bytes,len); ptr[len] = '\0';
   return s;
}

    // new string is OWNED
Instance *stringCat(pVM vm, char *s1, ...)
{
   va_list   ap;		// argument list pointer
   size_t    n;
   char	    *s, *ptr;
   Instance *str;

   va_start(ap,s1);
      for (n = 0, s = s1; s; s = va_arg(ap, char*)) n += strlen(s);
   va_end(ap);

   if (n == 0) return emptyString;
   str = stringAllocate(n,&ptr,I_OWNED,vm);

   va_start(ap,s1);
      for (s = s1; s; s = va_arg(ap, char*)) strcat(ptr,s);
   va_end(ap);

   return str;
}

size_t stringLen(Instance *self) { return strlen(ZKL_STRING(self)); }

/* ******************************************************************** */
/* *************************** Methods ******************************** */
/* ******************************************************************** */

    // .create(value = "" [,val,val...])
    // .create() --> ""
    // .create("string") --> arg0
Instance *String_create(Instance *self,pArglist arglist,pVM vm)
{
   int n = (int)listLen(arglist,vm);

   if (n == 0) return emptyString;
   if (n == 1) return arglistGetString(arglist,0,0,vm);
   return arglistConcat(0,arglist,vm);
}

static Instance *String_toBool(Instance *self,pArglist arglist,pVM vm)
   { return boolCreate(*ZKL_STRING(self)); }

Instance *stringToInt(char *numStr, pVM vm)
{
   #ifdef _MSC_VER
      int64_t x = _atoi64(numStr);
   #elif defined(__unix__)
      int64_t x = atoll(numStr);
   #endif
   return intCreate(x,vm);
}

static void _badChar(Instance *self, char c, int base, pVM vm)
{
   char buf[100];

   sprintf(buf,"\"%.50s\".toInt(%d) contains an invalid character: %c",
	ZKL_STRING(self),base,c);
   vmThrow(vm,E_VALUE_ERROR,buf);
}

    /* "123".toInt(base=10) where base is 2 .. 36
     * Format: [sign]digits, ONLY valid digits are legal, no whitespace, etc
     * To UTF-8:
     *    bytes  :=Data(Void,utf);	  // ASCII (8 bit) text to bytes
     *    utf_int:=bytes.toBigEndian();  // bytes to int, first byte is biggest
     *  or 
     *    utf_int:=utf.reduce(fcn(s,c){ 0x100*s + c.toAsc() },0);
     */
//!!! "\U1D11E;".toInt(-8) --> 0xf09d849e == Data(Void,utf).toBigEndian();

//!!! shit: what about "-0x123"? are all non decimal numbers unsigned?
//??? shouldn't "1e2".toInt() work?
static Instance *String_toInt(Instance *self,pArglist arglist,pVM vm)
{
   UChar  *ptr, *str = (UChar *)ZKL_STRING(self);
   int	   base = 10;
   int64_t n;

   if (!*str || !strcmp("-",(char *)str))
      vmThrow(vm,E_VALUE_ERROR,"String.toInt(\"\"): not a number");

   if (arglistTryToGetInt(arglist,0,&n,0,vm))
   {
      base = (int)n;
#if 1
      if(base == -8)
      {
	 UChar c;
	 n=0; while((c=*str++)) n = (n << 8) | c;
	 return intCreate(n,vm);	// .toString(-8) to verify
      }
#endif
      if (base < 2 || base > 36)
      {
	 char buf[100];
	 sprintf(buf,"String.toInt(%d): range is 2 to 36",base);
	 vmThrow(vm,E_VALUE_ERROR,buf);
      }
   }

   #if defined(_MSC_VER) || 1
      if (base == 10)
      {
	 ptr = str;
	 if (*ptr == '-') ptr++;
	 for (; *ptr; ptr++)
	 {
	    char c = *ptr;
	    if (c < '0' || c > '9') { _badChar(self,c,10,vm); return 0; }
	 }

	 #if defined(_MSC_VER)
	    n = _atoi64(str);
	 #else
	    n = strtoll((char *)str,(char **)&ptr,10);
	 #endif
	 return intCreate(n,vm);
      }

      {	// 'A'..'Z' == 0x41..0x5A, 'a'..'z' == 0x61..0x7A
	 UChar c, bz, bZ, b9;
	 int   z;

	 if (base < 10)
	 {
	    b9 = '0' - 1 + base;	// '2' ... '9'
	    bZ = 'A' - 1; bz = 'a' - 1;
	 }
	 else
	 {
	    b9 = '9';
	    bZ = 'A' - 11 + base; bz = 'a' - 11 + base;
	 }

	 n = 0;
	 for (ptr = str; (c = *ptr); ptr++)  // this also works for base<0
	 {
	    if      ('0' <= c && c <= b9) z = c - '0';
	    else if ('A' <= c && c <= bZ) z = c - 'A' + 10;    // 'A' == 10
	    else if ('a' <= c && c <= bz) z = c - 'a' + 10;    // 'a' == 10
	    else { _badChar(self,c,base,vm); return 0; }
	    n = n*base + z;
	 }
      }
   #elif defined(__unix__)
      errno = 0;
      n = strtoll(str,(char **)&ptr,base);
      if (errno || *ptr || isspace(*str))
      {
	 // Uggg, if str == "ffFFffFFffFFffFF", errno == ERANGE but 
	 // x is LLONG_MAX, not -1
	 if (errno != ERANGE)
	 {
	    char buf[100];
	    sprintf(buf,"String.toInt(%s,%d): Could not convert",str,base);
	    vmThrow(vm,E_VALUE_ERROR,buf);
	 }
      }
   #endif
   return intCreate(n,vm);
}

    /* format (from MS atof doc):
     * [whitespace] [sign] [digits] [.digits] [ {d | D | e | E }[sign]digits]
     * A whitespace consists of space or tab characters, which are ignored;
     * sign is either plus (+) or minus (-); and digits are one or more
     * decimal digits. If no digits appear before the decimal point, at
     * least one must appear after the decimal point. The decimal digits may
     * be followed by an exponent, which consists of an introductory letter
     * (d, D, e, or E) and an optionally signed decimal integer.
     * This is not a simple RE
     */
     // atof == strtod(nptr,0) without error checking
Instance *stringToFloat(char *numStr, pVM vm)
#if defined(_MSC_VER) && _MSC_VER<=1500   // XP only
   { return floatCreate(strtodDG(numStr,0),vm); }
#else
   { return floatCreate(atof(numStr),vm); }
#endif

    // .toFloat()
    // Hmmm, "0x1", "0X1", "0x1.2" work, "NaN"
    // 0x1.2788cfc6fb618f4cp-1 works on Linux/Win10, not on WinXP
static Instance *String_toFloat(Instance *self,pArglist arglist,pVM vm)
{
   char   *str = ZKL_STRING(self), *endp;
   double  f;

   if (!*str) vmThrow(vm,E_VALUE_ERROR,"\"\" is not a number");

   #if defined(_MSC_VER) && _MSC_VER<=1500   // XP only
      f = strtodDG(str,&endp);
   #else
      f = strtod(str,&endp);
   #endif
   if (*endp || isspace(*str))
   {
      char buf[100];
      sprintf(buf,"string.toFloat(%.50s) contains invalid characters",str);
      vmThrow(vm,E_VALUE_ERROR,buf);
   }
   return floatCreate(f,vm);
}

    // .toAsc(txt) --> txt[0].toInt(), ""-->0
static Instance *String_toAsc(Instance *self,pArglist arglist,pVM vm)
   { return intCreate(*(Byte *)ZKL_STRING(self),vm); }

    // .toData([mode=Int]) --> Data
static Instance *String_toData(Instance *self,pArglist arglist,pVM vm)
{
   char     *ptr = ZKL_STRING(self);
   Instance *data = dataCreate2((Byte *)ptr,strlen(ptr),vm);
   Data_mode(data,arglist,vm);
   return data;
}

    // [], .__sGet: self[offset,len=1] self --> .__sGet(offset[,len])
    // [offset], [offset,1] --> character, [offset,len > 1] --> string
    // "123"[100] --> IndexError, "123"[100,3] --> ""
    // num==1 is real common
    // .get(offset [,len=1]), .charAt(offset [,len=1]), 
static Instance *String_sGet(Instance *self,pArglist arglist,pVM vm)
{
   size_t n, num, len = stringLen(self);

   if (!arglistGetChunk(arglist,0,0x100, len, &n,&num, 0,vm))
	return emptyString;
   if (n==0 && num==len) return(self);
   if ((n+num) >= len)  // [n,*] --> just point into the string
      return kStringCreate(&ZKL_STRING(self)[n],self,I_OWNED,vm);
   return stringCreate2(&ZKL_STRING(self)[n],num,vm);
}

    // .tail(n)
static Instance *String_tail(Instance *self,pArglist arglist,pVM vm)
{
   size_t n  = (LSize_t)arglistGetInt(arglist,0,0,vm);
   size_t sz = stringLen(self);

   if(n==0)  return emptyString;
   if(n>=sz) return self;
   return kStringCreate(&ZKL_STRING(self)[sz - n],self,I_OWNED,vm);
}

    // .set(offset,len,text) --> new string
static Instance *String_set(Instance *self,pArglist arglist,pVM vm)
{
   char     *dst, *src=ZKL_STRING(self);
   char	    *text = arglistGetOnlyString(arglist,2,0,vm);
   Instance *str;
   size_t    n, num, len = stringLen(self), tn=strlen(text);;

   if (!arglistGetChunk(arglist,0,0x100, len, &n,&num, 0,vm))
      return stringCat(vm,src,text,(char *)0);
   if (num==0) return self;
   str=stringAllocate(len - num + tn, &dst,I_OWNED,vm);
   strncpy(dst,src,n); strcpy(dst+n,text); strcpy(dst+n+tn,src+n+num);
   return str;
}

    // .len([8|-8])
static Instance *String_len(Instance *self,pArglist arglist,pVM vm)
{
   char *ptr = ZKL_STRING(self);
   if (arglistTryToGet(arglist,0)) return intCreate(utf8Len((UChar *)ptr,vm),vm);
   return intCreate( strlen(ptr),vm );
}

    // .fmt(args)
static Instance *String_fmt(Instance *self,pArglist arglist,pVM vm)
   { return sfmt(ZKL_STRING(self),arglist,vm); }

    // .holds(text [,text2 ...]) --> True if textN is in self (ie or)
static Instance *String_holds(Instance *self,pArglist arglist,pVM vm)
{
   int i,len = (int)listLen(arglist,vm);

   for (i = 0; i < len; i++)
   {
      char *ptr = arglistGetOnlyString(arglist,i,"String.holds",vm);
      if (strstr(ZKL_STRING(self),ptr)) return BoolTrue;
   }
   return BoolFalse;
}

    // --> 0 (not found), 1 (found)
static int _find(Instance *self,pArglist arglist, size_t *N, pVM vm)
{
   char   *pattern = arglistGetOnlyString(arglist,0,"String.find/index",vm);
   char   *ptr, *string = ZKL_STRING(self);
   int	   n;
   size_t  matchStart, offset = 0, len = strlen(string);

   if (!*pattern) return 0;

   if (listLen(arglist,vm) > 1)
   {
      n = arglistGetChunk(arglist,1,0x103, len, &offset,&len, 0,vm);
//      if (n == 1) num = (Offset_t)len;   // one arg == search rest of string
   }

   if (!len) return 0;

   ptr = strstr(string + offset, pattern);	// could use boyer_moore here
   if (!ptr) return 0;
   matchStart = ptr - string;
   if ( (matchStart + strlen(pattern)) > (offset + len) ) return 0;
   *N = matchStart;
   return 1;
}

    // .find(pattern,start=0 ,sz=*) --> n|Void
static Instance *String_find(Instance *self,pArglist arglist,pVM vm)
{
   size_t offset;

   if (!_find(self,arglist,&offset,vm)) return Void;
   return intCreate(offset,vm);
}

    // .index(pattern,offset=0,sz=*)
    // same as find but throws and error instead of returning Void
static Instance *String_index(Instance *self,pArglist arglist,pVM vm)
{
   size_t offset;

   if (_find(self,arglist,&offset,vm)) return intCreate(offset,vm);
   vmThrow(vm,E_INDEX_ERROR,0);	// since I don't know how long pattern is
   return Void;		// shut up the compiler
}

    // .rfind(pattern,start=*)
    // resulting span may cross start
static Instance *String_rfind(Instance *self,pArglist arglist,pVM vm)
{
   char    *pattern = arglistGetOnlyString(arglist,0,"String.rfind",vm);
   char    *ptr, *ptr2, *string = ZKL_STRING(self), *end;
   size_t   n,sz = strlen(string);
   Offset_t off;

   if (!*pattern) return Void;

   arglistGetOffset(arglist,1, sz,
      (ARG_NO_LEN_IS_STAR | ARG_CONSTRAIN | ARG_OFF_OPTIONAL), &off, 0,vm);
   n = (size_t)off;
   end = string + n;

#if 0
   n    = strlen(string);
   plen = strlen(pattern);

   if (plen > n) return Void;
   ptr = string + n - plen;
   while (1)
   {
      if (strstr(ptr,pattern))
      {
	 n = ptr - string;
	 return intCreate(n,vm);
      }
      if (ptr-- == string) break;
   }
   return Void;
#else
   ptr = ptr2 = strstr((char *)string,pattern);
   while (ptr2)
   {
      ptr = ptr2;
      ptr2 = strstr((char *)ptr + 1,pattern);
      if(ptr2 && ptr2>end) break;
   }
   if (!ptr) return Void;

   n = ptr - string;
   return intCreate(n,vm);
#endif
}

static Instance *stringSplit(Instance *self, 
   char *splitter, size_t maxSplit, pVM vm)
{
   Instance *list  = listCreate(10,0x1,I_OWNED,vm);
   size_t    slen  = strlen(splitter);
   char     *str   = ZKL_STRING(self);
   char     *ptr;
   Fence     fence;

   vmSetFence(vm,&fence,0,list);
      while ((ptr = strstr((char *)str,splitter)))
      {
	 size_t	n = ptr - str;
	 listAppend(list, stringCreate2((char *)str,n,vm), vm);
	 str = ptr + slen;
	 if (--maxSplit == 0) break;
      }
	   // if sep was the last thing in the string, append ""
      listAppend(list, stringCreate((char *)str,I_OWNED,vm), vm);
   vmRemoveFence(&fence,0);
   return list;
}

    // .split([sep [,maxsplit]] == .split([String|Void [,int | *]])
    // --> L(...), not Tuple, pop is handy
//!!! .splistAtAnyOf(...), splitOnWS()
static Instance *String_split(Instance *self,pArglist arglist,pVM vm)
{
   Instance *list, *split;
   char	    *str = ZKL_STRING(self);
   size_t    maxSplit;
   Fence     fence;

   arglistGetSize(arglist,1,(size_t)-1,&maxSplit,vm);
   if (maxSplit == 0) return listCreateX(vm,self,ZNIL);

   if ((split = arglistTryToGet(arglist,0)) && split != Void)
   {
      char *splitter = arglistGetOnlyString(arglist,0,"String.split",vm);

      if (!*splitter)		// .split("")
	 return stringXplode(str,maxSplit,vm);

      return stringSplit(self,splitter,maxSplit,vm);
   }
   	// otherwise, split at white space, squishing adjacent space
	// different from split(" "): "1  2" --> L("1","2") not L("1","","2")
   list = listCreate(10,0x1,I_OWNED,vm);
   vmSetFence(vm,&fence,0,list);
   {
      char *ptr;
      while (*str && isspace(*str)) str++;	// skip leading space
      for (ptr = str; *str; str++)
      {
	 if (isspace(*str))
	 {
	    split = stringCreate2(ptr, str - ptr, vm);
	    listAppend(list,split,vm);
	    while (isspace(*++str)) ;		// skip space, stop at '\0'
	    ptr = str;
	    if (!*str) break;			// EoL

	    if (--maxSplit == 0)		// stop before EoL
	    {
	       str += strlen(str);
	       break;
	    }
	 }
      }
      if (*ptr)		// not at EoS but str is
      {
	 split = stringCreate2(ptr, str - ptr, vm);
	 listAppend(list,split,vm);
      }
   }
   vmRemoveFence(&fence,0);
   return list;
}

    // .span(open,close,outerMostSpan=1) --> T(offset,len),
    // --> Void (bad count), --> T (not found)
    // open == close --> Void
static Instance *String_span(Instance *self,pArglist arglist,pVM vm)
{
   char *open      = arglistGetOnlyString(arglist,0,0,vm);
   char *close     = arglistGetOnlyString(arglist,1,0,vm);
   int   outerMost = arglistTryToGetBool(arglist,2,1,0,vm);
   char  c, o1 = *open, c1 = *close;
   char *src = ZKL_STRING(self), *ptr = src;
   int   n,z, off,len, osz = strlen(open)-1, csz = strlen(close)-1;

   for (z = 0; (c = *ptr); ptr++)
   {
      if (c == o1)
      {
	 if (!osz || !strncmp(open,ptr,osz+1))
	 {
	    n = ptr - src;
	    if (!z || !outerMost) off = n;	// start of a span
	    z++;		// span starts
	    ptr += osz;		// skip over rest of open
	 }
      }
      else if (c == c1 && (!csz || !strncmp(close,ptr,csz+1)))	// span end
      {
	 if (!z) return Void;	// close token with no open
	 n = ptr - src;
	 if (!outerMost)
	 {
	    len = n - off + csz + 1;
	    return tupleCreateX(vm,intCreate(off,vm),intCreate(len,vm),ZNIL);
	 }
	 if (--z == 0)		// complete span
	 {
	    len = n - off + csz + 1;
	    return tupleCreateX(vm,intCreate(off,vm),intCreate(len,vm),ZNIL);
	 }
	 ptr += csz;
      }
   }
   if (z) return Void;
   return emptyTuple;
}

//!!!.sub(off,len,text) --> replace a chunk of text
// == s.replace(chunk,new,1)

    // .isSpace() --> Bool
static Instance *String_isSpace(Instance *self,pArglist arglist,pVM vm)
{
   UChar *ptr = (UChar *)ZKL_STRING(self);

   if (!*ptr) return BoolFalse;		// "" isn't space
   while (*ptr)
      if (!isspace(*ptr++)) return BoolFalse;
   return BoolTrue;
}

#if 1
    // .replace(searchFor, replaceWith [, count])
    // Replace all ocurrences of searchFor with replaceWith
    // strstr("abc","")-->"abc"
static Instance *String_replace(Instance *self,pArglist arglist,pVM vm)
{
   char     *searchFor   = arglistGetOnlyString(arglist,0,"String.replace",vm);
   Instance *rs          = arglistGetString(arglist,1,"String.replace",vm);
   char     *ptr, *text  = ZKL_STRING(self), *replaceWith = ZKL_STRING(rs);
   size_t    szSearchFor = strlen(searchFor), szNew = strlen(replaceWith);
   size_t    n, counting;
   int64_t   count;
   Fence     fence;
   ZBText    zbtext;

   counting = arglistTryToGetInt(arglist,2,&count,0,vm);
   if (counting && count < 0) return self;
   if (!*text || !*searchFor || !(ptr = strstr((char *)text,searchFor))) return self;

   vmSetFence(vm,&fence,0,zbtextInit(&zbtext,vm));
      fence.i = rs;
      do
      {
	 if (counting && --count < 0) break;
	 n = ptr - text;
	 zbtextAppendN(&zbtext,(char *)text,n,vm);
	 ptr += szSearchFor; text = ptr;
	 zbtextAppendN(&zbtext,replaceWith,szNew,vm);
      } while((ptr = strstr(ptr,searchFor)));

      zbtextAppend(&zbtext,(char *)text,vm);
      zbtextClose(&zbtext,vm);
   vmRemoveFence(&fence,0);
   return zbtext.string;
}
#else
    // .replace(searchFor, replaceWith [, src,dst ...])
    // Replace all ocurrences of searchFor with replaceWith
    // strstr("abc","")-->"abc"
static Instance *String_replace(Instance *self,pArglist arglist,pVM vm)
{
   char     *searchFor, *replaceWith;
   char     *ptr, *text = ZKL_STRING(self);
   size_t    szSearchFor, szNew;
   int       n, numArgs = arglistLen(arglist,vm);
   Fence     fence;
   ZBText    zbtext;

   vmSetFence(vm,&fence,0,zbtextInit(&zbtext,vm));
      for(n = 0; n<numArgs; n+=2)
      {
	 zbtextInit(&zbtext,vm);
	 searchFor   = arglistGetOnlyString(arglist,n,  "String.replace",vm);
	 replaceWith = arglistGetOnlyString(arglist,n+1,"String.replace",vm);
	 szSearchFor = strlen(searchFor); szNew = strlen(replaceWith);

	 if (!*text || !*searchFor || !(ptr = strstr((char *)text,searchFor))) continue;
	 do
	 {
	    unsigned n = ptr - text;
	    zbtextAppendN(&zbtext,(char *)text,n,vm);
	    ptr += szSearchFor; text = ptr;
	    zbtextAppendN(&zbtext,replaceWith,szNew,vm);
	 } while((ptr = strstr(ptr,searchFor)));

	 zbtextAppend(&zbtext,(char *)text,vm);
	 fence.i1 = zbtextFlush(&zbtext,vm); text = ZKL_STRING(zbtext.string);
      }
   vmRemoveFence(&fence,0);
   return zbtextClose(&zbtext,vm);
}
#endif

    // .translate(srcChars,dstChars)
    // Replace characters in srcChars with those in dstChars
static Instance *String_translate(Instance *self,pArglist arglist,pVM vm)
{
   char     *transSrc = arglistGetOnlyString(arglist,0,"String.",vm);
   char	    *transDst = arglistGetOnlyString(arglist,1,"String.",vm);
   Instance *r;
   int	     n;
   char      table[256];
   char     *src = ZKL_STRING(self), *dst;

   if (!*src) return emptyString;
   r = stringAllocate(strlen(src),&dst,I_OWNED,vm);

	// build a translation table
   for(n=0; n<256; n++) table[n] = n;
   while(*transSrc && *transDst) table[*transSrc++] = *transDst++;

	// translate string
   while(*src) *dst++ = table[*src++];
   *dst='\0';
   return r;
}

    // .del(offset, [num | *] = 1) == (n, [num | len])
static Instance *String_del(Instance *self,pArglist arglist,pVM vm)
{
   char     *str = ZKL_STRING(self), *ptr;
   Instance *r;
   size_t    num, offset, len = strlen(str);

   arglistGetChunk(arglist,0,0x00, len, &offset,&num, 0,vm);
   if (!num) return self;
   if (num == len) return emptyString;
   r = stringAllocate(len-num,&ptr,I_OWNED,vm);
   strncpy(ptr,str,offset); strncpy(ptr+offset,str+offset+num,len-offset-num);
   ptr[len-num] = '\0';
   return r;
}

    // .insert(offset,item,...)  parity with Data
    // vm.arglist.pump(Sink(str[0,n])).write(str[n,*]).close()
static Instance *String_insert(Instance *self,pArglist arglist,pVM vm)
{
#if 1
   MLIST(mlist,2);
   arglist = mlistBuild(mlist,self,arglist,ZNIL);
   return fcnRunFromClass(Utils_Helpers,"stringInsert",arglist,vm);
#else
   char     *str = ZKL_STRING(self);
   Offset_t  offset;
   int	     n,sz=strlen(str),N=listLen(arglist,vm);
   Instance *text;
   Fence     fence;
   ZBText    zbtext;

   	// no-op if no items, Data().insert() --> error
   arglistConstrainOffset(arglist,0,sz,0x2,&offset,"String.insert",vm);
   if (N==1) return self;
   vmSetFence(vm,&fence,0,zbtextInit(&zbtext,vm));
      zbtextAppendN(&zbtext,str,offset,vm);
      for (n=1 ; (text = listGet(arglist,n)); n++)
         zbtextAppendI(&zbtext,text,vm);
      zbtextAppendN(&zbtext,str+offset,sz-offset,vm);
      zbtextClose(&zbtext,vm);
   vmRemoveFence(&fence,0);
   return zbtext.string;
#endif
}

static Instance *String_append(Instance *self,Instance *arglist,pVM vm)
{
   MLIST(mlist,2);
   arglist = mlistBuild(mlist,self,arglist,ZNIL);
   return fcnRunFromClass(Utils_Helpers,"stringAppend",arglist,vm);
}

     // remove white space from front and rear of string
     // OK if len == sz
     // OK if no \0
     // whichEnd: -1 (strip start only), 0 (both ends), 1 (end only)
char *stripWS(char *start,size_t len, size_t *sz, int whichEnd)
{
#if 0
   char *end = start + len;

   while(start<end && isspace(*start)) start++;
   if (start == end) { *sz = 0; return start; }

   for(end--; isspace(*end) && start<end; end--) ;

   *sz = (end - start + 1);
   return start;

#else

   char *end;

   if(!len){ *sz = 0; return start; }
   end = start + len - 1;  // we have at least one (!=\0) char, point to last 1

   if(whichEnd<=0) while(start<=end && isspace(*start)) start++; // left: -1,0
   if(whichEnd>=0) while(start<=end && isspace(*end))   end--;   // right: 0,1

   *sz = (end - start + 1);
   return start;
#endif
}

    /* .strip(whichEnd=0) : remove white space from the front and rear of
     * a string.
     */
Instance *String_strip(Instance *self,pArglist arglist,pVM vm)
{
   char	  *str = ZKL_STRING(self);
   size_t  len = strlen(str), nl;
   char	  *start = str;
   int64_t whichEnd = 0;

   arglistTryToGetInt(arglist,0,&whichEnd,"String.strip",vm);

   start = stripWS(start,len,&nl,(int)whichEnd);
   if (len == nl) return self;
   return stringCreate2(start,nl,vm);
}

    // .inCommon("foo",...) == (self - (self - x))
    // Result can't be longer than 256 characters
static Instance *String_inCommon(Instance *self,pArglist arglist,pVM vm)
{
#if 0
   unsigned int	n,c;
   char *ptr, commons[256], buf[258];
   char *str = ZKL_STRING(self), *dst = buf;

   if (!*str) return emptyString;

	// build table of "other" characters
   memset(commons,0,256);
   for (n = 0; (ptr = arglistTryToGetString(arglist,n,0,vm)); n++)
      for (; *ptr; ptr++) commons[*ptr] = '1';

	// copy characters in self & table to new string
   while((c = *(unsigned char *)str++)) if (commons[c]) *dst++ = c;
   *dst='\0';
   return stringCreate(buf,I_OWNED,vm);
#else
   unsigned int	 n,c;
   unsigned char *ptr, commons[256], buf[258];
   unsigned char *str = (unsigned char *)ZKL_STRING(self), *dst = buf;

   if (!*str) return emptyString;

	// build table of "other" characters
   memset(commons,0,256);
   for (n = 0; (ptr = (unsigned char *)arglistTryToGetString(arglist,n,0,vm)); n++)
      for (; *ptr; ptr++) commons[*ptr] = '1';

	// copy characters in self & table to new string
   while((c = *str++)) if (commons[c]) *dst++ = c;
   *dst='\0';
   return stringCreate((char *)buf,I_OWNED,vm);
#endif
}

    // .unique() --> remove redundant characters from self
    // Returns unique characters in same order
static Instance *String_unique(Instance *self,pArglist arglist,pVM vm)
{
   UChar   c, commons[256];
   UChar  *str = (UChar *)ZKL_STRING(self);
   ZBText  zbtext;

   if (!*str) return emptyString;    // not needed but quick

   memset(commons,0,256);
   zbtextInit(&zbtext,vm);  // zbtext will hold 256 chars with no GC
   while((c=*str++))
      if(!commons[c])
	 { zbtextAppendN(&zbtext,(char *)&c,1,vm); commons[c] = 1; }
   return zbtextClose(&zbtext,vm);
}

    // .sort()
static Instance *String_sort(Instance *self,pArglist arglist,pVM vm)
{
   int       chars[256],n,z;
   UChar    *str = (UChar *)ZKL_STRING(self),c;
   Instance *sorted;

   if (!*str) return emptyString;    // not needed but quick

   for(n=0; n<256; n++) chars[n]=0;
   while((c=*str++)) chars[c]++;
   for(n=z=0; n<256; n++) z+=chars[n];
   sorted=stringAllocate(z,(char **)&str,I_OWNED,vm);

   for(n=0; n<256; n++) if((z = chars[n])) while(z--) *str++ = n;
   *str = '\0';
   return sorted;
}

    // "zbbccd".counts() --> ( "b",2", "c",2, "d",1, "z",1 )
    // "zbbccd".counts("z") --> ( "z",1 )
static Instance *String_counts(Instance *self,pArglist arglist,pVM vm)
{
   int	     n,sz,c, counts[256];
   char     *str = ZKL_STRING(self), *rs=arglistTryToGetString(arglist,0,0,vm);
   Instance *list;
   Fence     fence;

   if (!*str && !rs) return emptyTuple;  // "".counts("a")-->("a",0)

   memset(counts,0,256*sizeof(int));
   while((c = *str++)) counts[c]++;
   for(n=sz=0; n<256; n++) if(counts[n]) sz++;
   if(!rs)
   {
      vmSetFence(vm,&fence,0,list=tupleCreate(sz*2,I_OWNED,vm));
	 for(c=0; c<256; c++)
	    if((n=counts[c]))
	    {
	       tupleAppend(list, stringCreate2((char *)&c,1,vm) );
	       tupleAppend(list, intCreate(n,vm) );
	    }
      vmRemoveFence(&fence,0);
   }
   else
   {
      vmSetFence(vm,&fence,0,list=tupleCreate(strlen(rs)*2,I_OWNED,vm));
         while((c = *rs++))
	 {
	    n = counts[c];
	    tupleAppend(list, stringCreate2((char *)&c,1,vm) );
	    tupleAppend(list, intCreate(n,vm) );
	 }
      vmRemoveFence(&fence,0);
   }
   return list;
}

    // .prefix("foo","bar",...), find common prefix
    // "foobar".prefix("foo") --> 3, "foo".fits("foobar") --> 3
static Instance *String_prefix(Instance *self,pArglist arglist,pVM vm)
{
#if 0
   char *str = arglistGetOnlyString(arglist,0,"String.fits",vm);
   char *src = ZKL_STRING(self);
   int   n   = 0;
   while(*str && *src)
   {
      if (*str != *src) break;
      src++; str++; n++;
   }
   if (n && !*src) return BoolTrue;
   return intCreate(n,vm);
#else
   int   n, N, ns, z;
   char *src = ZKL_STRING(self);

   for(ns = listLen(arglist,vm), z = 0, N = strlen(src); z < ns; z++)
   {
      char *str = arglistGetOnlyString(arglist,z,"String.prefix",vm);
      src = ZKL_STRING(self);
      n   = 0;
      while(*str && *src && n <= N)
      {
	 if (*str != *src) break;
	 src++; str++; n++;
      }
      if(n == 0) return Zero;
      if(n < N) N = n;
   }
   return intCreate(N,vm);
#endif
}

    // .matches(pattern,flags=1 (ignore case),selfIsWild=False) --> Bool
    // == "".glob.unbind().fp1(pattern)
//!!!??? -->T(start,len)
static Instance *
stringGlob(Instance *self,pArglist arglist,int selfIsWild,pVM vm)
{
   int64_t flags = 1;
   int     r;
   char   *str   = arglistGetOnlyString(arglist,0,"String.matches",vm);
   arglistTryToGetInt(arglist,1,&flags,0,vm);
//!!!??? can I return the matched text?
   if(selfIsWild) r = wildmat(str,ZKL_STRING(self),(int)flags);
   else           r = wildmat(ZKL_STRING(self),str,(int)flags);
   return boolCreate(r);
}
static Instance *String_matches(Instance *self,pArglist arglist,pVM vm)
   { return stringGlob(self,arglist,0,vm); }

    // .glob(string, [flags=1])
    // well, shit, I did't match flags with File.glob
static Instance *String_glob(Instance *self,pArglist arglist,pVM vm)
   { return stringGlob(self,arglist,1,vm); }

    // .toUpper()
static Instance *String_toUpper(Instance *self,Instance *arglist,pVM vm)
{
   MLIST(mlist,1);
   arglist = mlistBuild(mlist,self,ZNIL);
   return fcnRunFromClass(Utils_Helpers,"toUpperCase",arglist,vm);
}

    // .toLower()
static Instance *String_toLower(Instance *self,Instance *arglist,pVM vm)
{
   MLIST(mlist,1);
   arglist = mlistBuild(mlist,self,ZNIL);
   return fcnRunFromClass(Utils_Helpers,"toLowerCase",arglist,vm);
}

    // .reverse()
static Instance *String_reverse(Instance *self,Instance *arglist,pVM vm)
{
   char	    *ptr = ZKL_STRING(self), *qtr, c;
   unsigned  sz  = strlen(ptr),n;
   Instance *r;

   if(sz<2) return self;
   r   = stringCreate2(ptr,sz,vm);
   ptr = ZKL_STRING(r); qtr = ptr+sz-1;
   for(n = sz/2; n--; ptr++,qtr--) { c = *ptr; *ptr = *qtr; *qtr = c; }
   return r;
}

static Instance *_stringGetNth(Instance *self,size_t n,void *X,size_t _,pVM vm)
{
   char c = ZKL_STRING(self)[n];
   if (!c) return 0;
   return stringCreate2(&c,1,vm); 
}

    // .howza([getter mode]) --> self|int
    // basically a noop as I can't stash state
Instance *String_howza(Instance *self,pArglist arglist,pVM vm)
{
   if (arglistTryToGet(arglist,0)) return self;
   return intCreate(3,vm);
}

    // .walker([n=3]), n: 0 (bytes/characters), 1 (lines), 2 (strings), 3 (characters)
static Instance *String_Walker(Instance *self,pArglist arglist, pVM vm)
{
   int64_t   n=3;
   arglistTryToGetInt(arglist,0,&n,0,vm);
   switch(n)
   {
      case 0: case 1: case 2: case 11: case 12: //let Data do the heavy lifting
      {
	 Instance *d;
	 char	  *ptr;
	 // 0,1,2. Don't include trailing '\0' in size.
	 ptr = ZKL_STRING(self);
	 d   = kdataCreate((Byte *)ptr,strlen(ptr),self,I_OWNED,vm);
	 return Data_walker(d,arglist,vm);
      }
   }
   return walkerCreate(self,_stringGetNth,0,0,vm);	 // characters
}

    // String.reduce, .pump
//!!!??? .pump(RE, match) - walk and match
Instance *String_pump(Instance *self,pArglist arglist,pVM vm)
   { return pump(self,PMP_OK2CNT,_stringGetNth,0,arglist,0,vm); }

Instance *String_reduce(Instance *self,pArglist arglist,pVM vm)
   { return zreduce(self,_stringGetNth,0,arglist,0,vm); }

    // String.apply
    // String.apply(T(...)) --> pump(String,arglist)
    // String.apply(...)    --> pump(String,T(arglist))
static Instance *S_apply(Instance *self,pArglist arglist,pVM vm)
   { return zapply(self,_stringGetNth,0, 0,(void *)ZA_STRING,arglist,0,vm); }

    // .filter(f) -->String
    // .filter(Void.Write,sink,f)
static Instance *String_filter(Instance *self,pArglist arglist, pVM vm)
   { return zfilter(self,ZA_STRING,_stringGetNth,(void *)0,arglist,0,vm); }

   // .zip(obj,obj...) 
   // --> Utils.Helpers.stringZipWith(String,self,obj,...)
static Instance *String_zip(Instance *self,pArglist arglist,pVM vm)
   { return zipObjs(self,emptyString,arglist,vm); }  // util.c


static const MethodTable stringMethods[] = 
{
   "create",		(pMethod)String_create,
   "toString",		(pMethod)Object_noop,
   "toBool",		(pMethod)String_toBool,
   "toInt",		(pMethod)String_toInt,
   "toFloat",		(pMethod)String_toFloat,

   "__sGet",		(pMethod)String_sGet,
   "get",		(pMethod)String_sGet,
   "tail",		(pMethod)String_tail,
   "charAt",		(pMethod)String_sGet,
   "set",		(pMethod)String_set,
   "len",		(pMethod)String_len,
   "holds",		(pMethod)String_holds,
   "fmt",		(pMethod)String_fmt,
   "find",		(pMethod)String_find,
   "rfind",		(pMethod)String_rfind,
   "index",		(pMethod)String_index,
   "split",		(pMethod)String_split,
   "span",		(pMethod)String_span,
   "isSpace",		(pMethod)String_isSpace,
   "toAsc",		(pMethod)String_toAsc,
   "toData",		(pMethod)String_toData,
   "strip",		(pMethod)String_strip,
   "del",		(pMethod)String_del,
   "insert",		(pMethod)String_insert,
   "append",		(pMethod)String_append,
   "replace",		(pMethod)String_replace,
   "translate",		String_translate,
   "inCommon",		(pMethod)String_inCommon,
   "matches",		(pMethod)String_matches,
   "glob",		(pMethod)String_glob,
   "prefix",		(pMethod)String_prefix,
   "unique",		(pMethod)String_unique,
   "reverse",		(pMethod)String_reverse,
   "sort",		(pMethod)String_sort,
   "counts",		(pMethod)String_counts,
   "zip",		String_zip,
   "zipWith",		zipWithObjs,

   "walker",		(pMethod)String_Walker,
   "apply",		(pMethod)S_apply,
   "reduce",		(pMethod)String_reduce,
   "pump",		(pMethod)String_pump,
   "filter",		(pMethod)String_filter,
   "howza",		(pMethod)String_howza,

   "toLower",		(pMethod)String_toLower,
   "toUpper",		(pMethod)String_toUpper,

   "close",		(pMethod)Object_noop,		// for Stream

   0,			0
};

/* ******************************************************************** */
/* *************************** Properties ***************************** */
/* ******************************************************************** */

    // .container
static Instance *String_container(Instance *self,pVM vm)
{
   if (MAGIC_MARKER(self))	// KCString
      return ((ZKL_KCString *)self)->container;
   return Void;
}

static const PropertyTable properties[] = 
{
   "text",	(pProperty)Object_noop,	// bit of parity with Data --> self
   "container",	(pProperty)String_container,
   "isReadOnly",(pProperty)Bool_soTrue,  // yeah it is a Method
   0,		0
};

/* ******************************************************************** */
/* ***************************** Op Codes ***************************** */
/* ******************************************************************** */

    // If it ain't a string, they ain't equal
static Instance *String_eq(Instance *self,Instance *X,pVM vm)
{
   if (TYPEO(X) != StringType) return BoolFalse;
   return boolCreate( 0 == strcmp(ZKL_STRING(self),ZKL_STRING(X)) );
}

static Instance *String_neq(Instance *self,Instance *X,pVM vm)
{
   if (TYPEO(X) != StringType) return BoolTrue;
   return boolCreate( strcmp(ZKL_STRING(self),ZKL_STRING(X)) );
}

static Instance *String_lt(Instance *self,Instance *X,pVM vm)
{
   char *x = ZKL_STRING(convertTo(X,TO_STRING,vm));
   return boolCreate( strcmp(ZKL_STRING(self),x) < 0);
}

static Instance *String_lte(Instance *self,Instance *X,pVM vm)
{
   char *x = ZKL_STRING(convertTo(X,TO_STRING,vm));
   return boolCreate( strcmp(ZKL_STRING(self),x) <= 0);
}

static Instance *String_gt(Instance *self,Instance *X,pVM vm)
{
   char *x = ZKL_STRING(convertTo(X,TO_STRING,vm));
   return boolCreate( strcmp(ZKL_STRING(self),x) > 0);
}

static Instance *String_gte(Instance *self,Instance *X,pVM vm)
{
   char *x = ZKL_STRING(convertTo(X,TO_STRING,vm));
   return boolCreate( strcmp(ZKL_STRING(self),x) >= 0);
}

    // opAdd: concat
static Instance *String_add(Instance *self,Instance *X,pVM vm)
{	// or just use a ZBText
   char     *ptr, *p2;
   Instance *xs = convertTo(X,TO_STRING,vm);	// might alloc or throw
   char     *s  = ZKL_STRING(self);
   size_t    n  = strlen((char *)s);
   Fence     fence;
   Instance *newString;

   vmSetFence(vm,&fence,0,xs);
      p2 = ZKL_STRING(xs);
      newString = stringAllocate(n + strlen(p2),&ptr,I_OWNED,vm);
      strcpy(ptr,s); strcpy(ptr+n,p2);
   vmRemoveFence(&fence,0);
   return newString;
}

    // opSub: remove, from self, all occurences of the characters in X
Instance *String_sub(Instance *self,Instance *X,pVM vm)
{
   UChar     omit[256], *str = (UChar *)ZKL_STRING(self);
   Instance *newString;
   UChar    *ptr;

   if (TYPEO(X) != StringType)
      vmThrow(vm,E_TYPE_ERROR,"String minus: text only");

//!!! would be nice to first see is self will actually change
   memset(omit,0,256);
   for (ptr = (UChar *)ZKL_STRING(X); *ptr; ptr++) omit[*ptr] = '1';

   newString = stringAllocate(strlen((char *)str),(char **)&ptr,I_OWNED,vm);
   for (; *str; str++)
      if (!omit[*str]) *ptr++ = *str;
   *ptr = '\0';
   return newString;
}

    // opMul: create n copies of self
static Instance *String_mul(Instance *self,Instance *X,pVM vm)
{
   char     *s   = ZKL_STRING(self);
   size_t    len = strlen((char *)s);
   int	     n;
   Instance *newString;
   char     *ptr;

   if (TYPEO(X) != IntType)
      vmThrow(vm,E_TYPE_ERROR,"Syntax: String * integer");

   n = (int)convertToInt(X,vm);
   if (n == 1) return self;
   if (n < 1) return emptyString;
   newString = stringAllocate(len*n,&ptr,I_OWNED,vm);
   while (n--){ strcpy(ptr,s); ptr+=len; }
   return newString;
}

static const OpcodeTable stringOps[] = 
{
   OP_EQ,	(pOp)String_eq,
   OP_NEQ,	(pOp)String_neq,

   OP_LT,	(pOp)String_lt,
   OP_LTE,	(pOp)String_lte,
   OP_GT,	(pOp)String_gt,
   OP_GTE,	(pOp)String_gte,

   OP_ADD,	(pOp)String_add,
   OP_SUB,	(pOp)String_sub,
   OP_MUL,	(pOp)String_mul,

   0,		0
};

/* ******************************************************************** */
/* *********************** KString & KCString ************************* */
/* ********** String Constants pointing into other Objects ************ */
/* ******************************************************************** */

    /* Pointer strings are pointers to strings that never move or are
     * "relatively" constant.
     * 
     * And, the buffer can't move because there are external pointers into
     * it.
     * 
     * KStrings will never be reclaimed (by GC) and are not roots (ie not
     * immortal), as opposed to KCStrings.
     */

static ZKL_KCString kcStringProto;
static ZKL_KString  kStringProto, _Star;

    // If static string in C code, container == 0 
    // both iflags == 0
Instance *kStringCreate(char *text, Instance *container, int itype, pVM vm)
{
   int it;

   if (!text || !*text) return emptyString;
   if (text[0]=='*' && text[1]=='\0') return Star;
   
   if (container &&   // need a KCString? UNTOUCHABLE, 2SPECIAL
      ((it = container->itype) == I_OWNED || it == I_INVISIBLE))
   {
      ZKL_KCString *kcs = (ZKL_KCString *)
	    ibucketAllocate(&kcBuckets,&KCStringObject,itype,1,vm);
      kcs->ptr	     = text;
      kcs->container = container;
      return containerIsCooked(&kcBuckets,(Instance *)kcs,itype);
   }
   else	  // KString will work nicely and save a bit of space.
   {
      ZKL_KString *ks = (ZKL_KString *)
	    ibucketAllocate(&kBuckets,&KStringObject,itype,1,vm);
      ks->ptr     = text;
      return (Instance *)ks;
   }
}

    // KMStrings: KStrings with malloc'd text that needs to be 
    // hooked into GC. I can do this one of two ways:
    //   - Seperate object
    //   - Set iflag2 on KString and add a KString free
static int kmStringFree(Instance *self) 
    { free(((ZKL_KString *)self)->ptr); return 1; }

    // --> KString with malloc'd contents
Instance *kmStringCreate(char *text, pVM vm)
{
   ZKL_KString *kms;

   if (!text || !*text) return emptyString;
   if (text[0]=='*' && text[1]=='\0') return Star;

   if (!KMStringObject.id)	// !!!NOT thread safe
   {
      KMStringObject	    = KStringObject;	// struct copy
      KMStringObject.name   = "MallocString";
      KMStringObject.freeMe = kmStringFree;
      registerObject(&KMStringObject,vm);
   }

   kms = (ZKL_KString *)ibucketAllocate(&kBuckets,&KMStringObject,I_OWNED,1,vm);
   kms->ptr = text;
   return (Instance *)kms;
}

   // Initialize an in memory KString. Don't use this for the usual case.
Instance *kStringInit(ZKL_KString *ks, char *text)
{
   *ks     = kStringProto;	// struct copy, UNTOUCHABLE
   ks->ptr = text;
   return (Instance *)ks;
}

#if 0
   // if "" or "*", return preallocated String, otherwise build an 
   //   in memory KString.
   // container = 0, type = special
Instance *kstringBuild(ZKL_KString *ks, char *text)
{
   if (!*text) return emptyString;
   if (text[0]=='*' && text[1]=='\0') return Star;
   return kStringInit(ks,text);
}
#endif

    /* StringTable --> KCStrings[], used by the VM for opGetKString(n)
     * strings == StringTable.strings: "text\0text\0..."
     * map[0] is the number of items in map, map[i] is an index, ie
     *   strings[map[i]]
     * The KCStrings in the table aren't GCable, but if GC sees them, mark
     *   the container (ie if detached from FcnBase, mark FcnBase as a
     *   keepalive).
     * Container is a FcnBase, it can be zero if the strings are immortal
     *   (eg in a wad)
     */
//!!!for tables of size 1, it is probably worth it to use a bucket
ZKL_KCString *
buildKCStringTable(char *strings, size_t size, Instance *container, 
   Byte *map, ZKL_KCString *table)
{
   char	    *s;
   unsigned  i,n,z;

   n = *map++;		// get count and point to first index
   if (!size || !n) return table;

   for (i = 0; i < n; i++)
   {
      ZKL_KCString *kcs = &table[i];
      *kcs = kcStringProto;		// struct copy, 2SPECIAL

	// map isn't sorted, but table is, so untangle
      for (z = map[i], s = strings; z--; ) s = stNextString(s);
      kcs->ptr	     = s;
      kcs->container = container;
   }

   return table;
}

static void kcStringMarker(Instance *self)
   { instanceMark(((ZKL_KCString *)self)->container); }

static void kstringConstruct(void)	// String has been constructed
{
   KStringObject	     = StringObject;		// struct copy
   KStringObject.name	     = "KonstString";
   KStringObject.isize	     = sizeof(ZKL_KString);
   KStringObject.isBInstance = 1;
   KStringObject.createReturnsSelf = 1;
   registerObject(&KStringObject,NoVM);

   ibucketReserve(&KStringObject,4555,&kBuckets,0,NoVM);

   instanceInit((Instance *)&kStringProto,&KStringObject,I_UNTOUCHABLE);
   Star = kStringInit(&_Star,"*");

   		/////

   KCStringObject	      = KStringObject;	// struct copy
   KCStringObject.name	      = "ConstString";
   KCStringObject.magicMarker = kcStringMarker;
   KCStringObject.isize	      = sizeof(ZKL_KCString);
   KCStringObject.isBInstance = 1;
   KCStringObject.createReturnsSelf = 1;
   registerObject(&KCStringObject,NoVM);

   ibucketReserve(&KCStringObject,5555,&kcBuckets,0,NoVM);


      // Need to always mark container. EG var R = "foo" -> Fcn
      // R is only reference to fcn, if don't mark, fcn is gone
   instanceInit((Instance *)&kcStringProto,&KCStringObject,I_2SPECIAL);
   kcStringProto.container = 0;
}

/* ******************************************************************** */
/* ****************************** UTF-8 ******************************* */
/* ******************************************************************** */
// http://en.wikipedia.org/wiki/UTF-8
// http://www1.tip.nl/~t876506/utf8tbl.html
// http://www.cl.cam.ac.uk/~mgk25/unicode.html
// http://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt
// http://bjoern.hoehrmann.de/utf-8/decoder/dfa/

// Windows:  "EF BB BF" (Byte Order Mark U+FEFF encoded as UTF-8)
// at the start of a file [sometimes] implies UTF-8

    // Return num UTF-8 characters in string. Validate string. Decode it too.
//!!!??? replace bad character with U+FFFD?
size_t utf8Len(UChar *utf,pVM vm)
{
   static uint32_t ranges[]={ 0x7f,0x7ff,0xffff,0x1FFFFF,0x3FFFFFF,0x7FFFFFFF };

   UChar  c,d;
   size_t n = 0;

   while((c = *utf++))
   {
      if (0 == (c & 0x80)) n++;
      else
      {
	 uint32_t u=0;	// 32 bits (4 bytes), 31 bits max are used
	 // Number of high 1 bits is number of bytes
	 // 110x|xxxx 2, 1110|xxxx 3, 
	 // 1111|0xxx 4, 1111|10xx 5, 1111|110x 6 bytes
	 int bytes, z=0; // bytes after c, which is >=1 && <6
	 d=c; while( (d<<=1) & 0x80 ) z++; bytes = z; // one less than total bytes

	 if (!z || z>3) // 2 to 4 UTF-8 bytes compress to 4 bytes
	 {
	 invalid:
	    vmThrow(vm,E_VALUE_ERROR,"Invalid UTF-8 string");
	 }
//	 if (z==1) { if (c==0xC0 || c==0xC1) goto invalid; }
//	 if (z==3 && c>=0xf4) goto invalid;
	 u = c & (0xff>>(z+2));	// strip bits off top
	 while(z--)
	 {
	    c = *utf++;
//	    if (!c || (c & 0xC0)!=0x80 || c == 0x80) goto invalid;
	    if (!c || (c & 0xC0)!=0x80) goto invalid;
	    u = (u << 6) | (c & 0x3F);
	  }
	  if (!(ranges[bytes-1]<u && u<=ranges[bytes]) ||
	      (u>=0xD800 && u<=0xDFFF)	 	       ||
	      u==0xfffe || u==0xffff) goto invalid;
	  n++;
      }
   } // while

   return n;
}

///////////////////////////// String Sink
static int stringToSink(
   Instance *self,void *_aggie,size_t _,size_t sbsz,size_t *sbused, pVM vm)
{
   ZAgg *aggie = (ZAgg *)_aggie;

   if(!aggie){ *sbused=sizeof(ZBText); return ZA_2SINK_OK; } // sbUsed query

   if(sbsz < sizeof(ZBText)) return ZA_SANDBOX_TOO_SMALL;

   aggie->how = ZA_C;
   aggie->dst.csink.X     = &aggie->sandbox;
   aggie->dst.csink.write = (CSinkWrite)zbtextAppendI;	// pointers is pointers
   aggie->dst.csink.close = (CSinkClose)zbtextClose;
   aggie->i	          = zbtextInit2(aggie->dst.csink.X,self,vm); 
   *sbused		  = sizeof(ZBText);
   return ZA_2SINK_OK;
}

/* ******************************************************************** */
/* ******************************************************************** */

//static pMethod in_string_methods(Instance *ignore, register char *str);

void stringConstructPartI(void)
{
   constructObject(&StringObject,StringType,
		   stringMethods,properties,stringOps,NoVM);
//   StringObject.methodSearch = in_string_methods;
   StringObject.isize	     = sizeof(ZKL_String);
   StringObject.threadSafe   = 1;
   StringObject.toSink	     = stringToSink;
   StringObject.createReturnsSelf = 1;

   kstringConstruct();

      // Poach another bucket to pack a String into.
   CuckooSObj		  = StringObject;	// struct copy
   CuckooSObj.name        = "CuckooString";
   CuckooSObj.isize       = sizeof(CuckooString);  // not really
   CuckooSObj.isBInstance = 1;
   CuckooSObj.createReturnsSelf = 1;
   registerObject(&CuckooSObj,NoVM);

	// Create static strings
   instanceInit(emptyString, &StringObject,I_UNTOUCHABLE);
   theEmptyString.value[0] = '\0';
   emptyString->iflag      = 1;
}

void stringConstructPartII(void)
{
#if 0
   extern IBucketHeader tsListBuckets;	// list.c

	// pack a short string into a TSList
   bigBuckets    = &tsListBuckets; // list.c uses String before I'm called
   bigBucketSize = bigBuckets->blobSize - sizeof(ZKL_String) + PADDING;

   ShortLongObj	      = ShortKSObj;		// struct copy
   ShortLongObj.name  = "ShortLongString";
   ShortLongObj.isize = (short)bigBucketSize;
   registerObject(&ShortLongObj,NoVM);
   ibucketPoach(bigBuckets,&ShortLongObj,NoVM);
#endif

   vaultAdd("",emptyString,NoVM);

   #if defined(_MSC_VER) && _MSC_VER<=1500  // XP only
      _control87(PC_53, MCW_PC);	// for strtodDG
   #endif
}



///////////////////////////////////////////////////////////////////
// zkl extractTable.zkl < string.c | gperf | zkl gperf.zkl -i string


