/* number.c : The Integer and Float Objects
 * 
 * Copyright (c) 2006,2007,2008-12,2013 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008
#define __STDC_LIMIT_MACROS		// stdint.h

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include <time.h>
#include <limits.h>
#if defined(__unix__)
   #include <fcntl.h>	// open /dev/random
   #include <locale.h>	// for localeconv()
#endif

#ifdef _MSC_VER
   #define hypot	_hypot
#endif

#define __NOT_A_DLL

#include "zklObject.h"
#include "zklFcn.h"
#include "zklList.h"
#include "zklMethod.h"
#include "zklNumber.h"
#include "zklString.h"

ZKL_Object IntObject;	// not static because used externally for PtrInts
static ZKL_Object FloatObject;

ZKL_Int   _Zero;
ZKL_Int	  _One, _MinusOne;
ZKL_Float _FZero, _FOne;

Instance
   *Zero     = (Instance *)&_Zero,     *One  = (Instance *)&_One,
   *MinusOne = (Instance *)&_MinusOne,
   *FZero    = (Instance *)&_FZero,    *FOne = (Instance *)&_FOne;

static IBucketHeader *nBuckets;

/* ******************************************************************** */
/* ******* Convert IEEE754 double to & from 4 big endian bytes ******** */
/* ******************************************************************** */

    // The following pack code is from:
    // Brian "Beej Jorgensen" Hall, http://www.retran.com/beej
#define pack754_64(f)   (pack754((f),   64, 11))
#define unpack754_64(i) (unpack754((i), 64, 11))

static uint64_t pack754(long double f, unsigned bits, unsigned expbits)
{
	long double fnorm;
	int shift;
	long long sign, exp, significand;
	unsigned significandbits = bits - expbits - 1; // -1 for sign bit

	if (f == 0.0) return 0; // get this special case out of the way

	// check sign and begin normalization
	if (f < 0) { sign = 1; fnorm = -f; }
	else { sign = 0; fnorm = f; }

	// get the normalized form of f and track the exponent
	shift = 0;
	while(fnorm >= 2.0) { fnorm /= 2.0; shift++; }
	while(fnorm < 1.0) { fnorm *= 2.0; shift--; }
	fnorm = fnorm - 1.0;

	// calculate the binary form (non-float) of the significand data
	significand = (long long)(fnorm * ((1LL<<significandbits) + 0.5f));

	// get the biased exponent
	exp = shift + ((1<<(expbits-1)) - 1); // shift + bias

	// return the final answer
	return (sign<<(bits-1)) | (exp<<(bits-expbits-1)) | significand;
}

static long double unpack754(uint64_t i, unsigned bits, unsigned expbits)
{
	long double result;
	long long shift;
	unsigned bias;
	unsigned significandbits = bits - expbits - 1; // -1 for sign bit

	if (i == 0) return 0.0;

	// pull the significand
	result = (long double)(i&((1LL<<significandbits)-1)); // mask
	result /= (1LL<<significandbits); // convert back to float
	result += 1.0f; // add the one back on

	// deal with the exponent
	bias = (1<<(expbits-1)) - 1;
	shift = ((i>>significandbits)&((1LL<<expbits)-1)) - bias;
	while(shift > 0) { result *= 2.0; shift--; }
	while(shift < 0) { result /= 2.0; shift++; }

	// sign it
	result *= (i>>(bits-1))&1? -1.0: 1.0;

	return result;
}

static int IEEE754ian=0;
static void setIEEE754ness()
{
   double   x=(double)0.1;
   uint64_t n;
   memcpy((void *)&n,(void *)&x,sizeof(n));
   IEEE754ian = (n==0x3fb999999999999a) ? 0 : 2;
//IEEE754ian=2;
//printf("IEEE754ian %d\n",IEEE754ian);
}

#if 0
  uint32 part0 = static_cast<uint32>(value);
  uint32 part1 = static_cast<uint32>(value >> 32);

  target[0] = static_cast<uint8>(part0);
  target[1] = static_cast<uint8>(part0 >>  8);
  target[2] = static_cast<uint8>(part0 >> 16);
  target[3] = static_cast<uint8>(part0 >> 24);
  target[4] = static_cast<uint8>(part1);
  target[5] = static_cast<uint8>(part1 >>  8);
  target[6] = static_cast<uint8>(part1 >> 16);
  target[7] = static_cast<uint8>(part1 >> 24);
#endif

    // Convert 4 bytes to little endian IEEE754 double
    // 3 cases:
    //   Intel: copy bytes
    //   big endian 754: swap byte order
    //   else: do it the hard way
double fromIEEE754(void *ptr)  // 4 bytes-->double
{
   switch(IEEE754ian)
   {
      case 0:  // eg Intel x86, small endian IEEE754
      {
	 double x;
	 memcpy((void *)&x,ptr,sizeof(x));
	 return x;
      }
      case 1:
//uint64_t le64toh(uint64_t little_endian_64bits);
      case 2: return unpack754_64(*(uint64_t *)ptr);
   }
   return 0;
}
uint64_t toIEEE754(double x)  // double-->4 bytes
{
#if 0 // or
   union{ int64_t bits; double val; } F;
   F.val = x;
   return(F.bits);
#endif

   switch(IEEE754ian)
   {
      case 0:  // eg Intel x86, small endian IEEE754
      {
	 uint64_t n;
	 memcpy((void *)&n,(void *)&x,sizeof(n));
	 return n;
      }
      case 1:
      case 2: return pack754_64(x);
   }
   return 0;
}
static Instance *Float_toIEEE754(Instance *self,pArglist arglist,pVM vm)
   { return intCreate(toIEEE754(FLOATV(self)),vm); }
static Instance *Int_fromIEEE754(Instance *self,pArglist arglist,pVM vm)
    { return floatCreate(fromIEEE754(&INT64V(self)),vm); }


/* ******************************************************************** */
/* ************************** Integers ******************************** */
/* ******************************************************************** */

   // 283,087 Ints in play at one point when compiling the parser;
   // 648,249 no GC
   // Compling the parser creates 2,543,729 Ints

   ////////////////////// Pointer Ints //////////////////////

#if USE_POINTER_INTS

Instance *intToPtrInt(PtrInt pint, ZKL_Int *newInt, pVM vm)
{
   #if GC_SANITY_CHECK
      if (!IS_PtrInt(pint)) vmHalt("Not a PtrInt");
   #endif
   *newInt = _Zero;		// struct copy
   newInt->value = pint >> 1;	// preserve sign bit
   return (Instance *)newInt;
}

int64_t convertToInt(Instance *n, pVM vm)
{
   pMethod method;

   if (IS_PtrInt(n)) return PtrInt_TO_N(n);
   if (TYPEO1(n) == IntType) return INT64V(n);
//!!!??? iWantInt()? 1 + "123"?, 1 + BigNum(4)
   method = I_METHOD(n,TO_INT);
   n = method(n,NoArglist,vm);	// Int or PtrInt
   if (IS_PtrInt(n)) return PtrInt_TO_N(n);
   return INT64V(n);
}

Instance *decantInt(Instance *n,ZKL_Int *i64)
{
   if (IS_PtrInt(n)) { PtrInt_TO_Int64(n,i64); return (Instance *)i64; }
   return n;
}

   // Run a Int Method on a PtrInt.
   // methodName is ignored unless method==0, then it must be the name of
   //   the method. In this case, arglist[0] is assumed to be self, ie pi
Instance *ptrIntDoMethod(Instance *pi,unsigned methodId,pMethod method,
   pArglist arglist, pVM vm)
{
   ZKL_Int   i64;
   Instance *i = (Instance *)&i64, *r;
   MLIST(margz,15);

   PtrInt_TO_Int64(pi,&i64); i64.instance.iflag = 1; // tag as temp

   if(!method) // lookup by name
   {
      unsigned s;
      s = objectResolveN(&i,methodId,&method,0,0,vm);
      if(!s)		  return 0; // unknown
      if(s==PropertyType) return i; // property

      arglist = mlistCopy(margz,arglist,1,15); // (self,args) --> (args)
   }

   if (method == Object_toList)  // (7).toList(), T(7).apply("toList")
	  return tupleCreateX(vm,pi,ZNIL);

   //!!! any container that captures pi/i has gotta box it
   // i = intAllocate(PtrInt_TO_N(pi),vm);  // or just always box
   // fence.i2 = i;
   r = method(i,arglist,vm); // eg T(123).apply("len")
   if (r == i) r = pi;	     // eg (7).copy()
   else switch(TYPEO(r))
   {
      case DeferredType:  // (7).fp("copy")
	 deferredThunk(r,INT64V(&i64),vm); break;
      case MethodType:	  // L(5,6,7).apply("Method","noop")
	 methodThunk(  r,INT64V(&i64),vm); break;
      case PropertyType:  // L(123).apply("Property","name")
	 propertyThunk(r,INT64V(&i64),vm); break;
      // walker uses Zero as self
   }
   return r;
}

#if 0
    // ZKL_Int --> PtrInt
#define SQUEEZE_INT(self,n)				 \
   (-((int64_t)1 << 30) <= n && n < (int64_t)1 << 30) ?  \
   INT_TO_INSTANCE(n) : (self)
#endif

#else	// don't USE_POINTER_INTS

int64_t convertToInt(Instance *n, pVM vm)
{
   pMethod method;

   if (TYPEO1(n) == IntType) return INT64V(n);
   method = I_METHOD(n,TO_INT);
   n = method(n,NoArglist,vm);
   return INT64V(n);
}

//Instance *decantInt(Instance *n,ZKL_Int *i64) { return n; }

//#define SQUEEZE_INT(self,n) (self)

#endif	// USE_POINTER_INTS

static Instance *_intCreate(int64_t n, pVM vm)
{
   ZKL_Int *i;

   #if USE_POINTER_INTS
//      if (-((int64_t)1 << 30) <= n && n < (int64_t)1 << 30)
      if (INT_FITS_IN_PtrInt(n)) return INT_TO_INSTANCE(n);
   #endif

   i = (ZKL_Int *)ibucketAllocate(nBuckets,&IntObject,I_OWNED,1,vm);
   i->value = n;
   return (Instance *)i;
}

Instance *intCreate(int64_t n, pVM vm)
{
   if (n ==  0) return Zero;
   if (n ==  1) return One;
   if (n == -1) return MinusOne;

   return _intCreate(n,vm);
}

Instance *intAllocate(int64_t x, pVM vm)
{
   ZKL_Int *i;

   if (x ==  0) return Zero;
   if (x ==  1) return One;
   if (x == -1) return MinusOne;

   i = (ZKL_Int *)ibucketAllocate(nBuckets,&IntObject,I_OWNED,1,vm);
   i->value = x;
   return (Instance *)i;
}

////////////////////// Int Methods ////////////////////////

    /* Base conversions. I'm going to copy MS convention here: 
     * to decimal is signed, any other base is unsigned.
     * Undecided what is best, probably both.
     */

#ifdef _MSC_VER
   char *intToA(int64_t x, char *buf) { return _i64toa(x,buf,10); }
#else	// Linux/gcc
	   // (base) where base is 2 .. 36
//   #define letterOffset	 7	// A,B,C ... Z : n + ('A' - '0' - 10)
   #define letterOffset		39 	// a,b,c ... z

   static char *uintToBase(uint64_t n,char *buf,int base)
   {
      char  tmp[100];
      char *ptr = &tmp[99];
      int   z;

      *ptr = '\0';
//      if (n < 0) { minus = 1; n = -n; }
      do
      {
	 if ((z = (int)(n % base)) > 9) z += letterOffset;
	 *--ptr = z + '0';
	 n /= base;
      } while (n > 0);
//      if (minus) *--ptr = '-';
      return strcpy(buf,ptr);
   }

      // buf needs to hold at least 21 characters
   static char *intToString(int64_t n,char *buf) // base 10
   {
      char  tmp[31];	// INT64_MIN==21 chars (with \0)
      char *ptr = &tmp[30];
      int   minus = 0;

      if (n==INT64_MIN) return strcpy(buf,"-9223372036854775808");
      *ptr = '\0';
      if (n < 0) { minus = 1; n = -n; } // -MIN doesn't exist
      do
      {
	 lldiv_t mr=lldiv(n,10); n = mr.quot;
	 *--ptr = mr.rem + '0';
      } while (n > 0);
      if (minus) *--ptr = '-';
      return strcpy(buf,ptr);
   }

   char *intToA(int64_t n, char *buf)
//      { sprintf(buf,"%lld",(long long)n); return buf; }
      { return intToString(n,buf); }
#endif

    // ([base]) where base is 2 .. 36
char *i64ToBaseB(int64_t i64,char *buf,int base, pVM vm)
{
   if (base < 2 || base > 36)
   {
      sprintf(buf,"base(%d): range is 2 .. 36",base);
      vmThrow(vm,E_VALUE_ERROR,buf);
   }
   #ifdef _MSC_VER
      _i64toa(i64,buf,base);
   #else
      if (base == 10) return intToA(i64,buf);
      uintToBase(i64,buf,base);
   #endif

   return buf;
}

    // Int.toBase([base]) where base is 2 .. 36 or -8 (UTF-8)
static Instance *Int_toBase(Instance *self,pArglist arglist,pVM vm)
{
   char buf[100];	// 64 bits is 64 base 2 digits
   int	base = (int)arglistGetInt(arglist,0,"Int.toBase",vm);

   if (base==-8)	// -->UTF-8
   {
      MLIST(mlist,3);
      mlistBuild(mlist,self,self,Zero,ZNIL);
      return fcnRunith("Compiler.Tokenizer","intToUTF8",(Instance *)mlist,vm);
   }

   return stringCreate(i64ToBaseB(INT64V(self),buf,base,vm),I_OWNED,vm);
}

    /* Int.toString() --> convert to base 10 integer
     * Int.toString(base) where base is 2 .. 36 or -8 (UTF-8)
     * Hmmmmm: .toString("format"), .toString(...,commaize)
* .toString(base,n,c) n is commas-every-n, c is comma char
for sfmt.c:fmtCommaize
     */
Instance *Int_toString(Instance *self,pArglist arglist,pVM vm)
{
   char	buf[50]; // 64 bit int is max 20 digits w/"-"
   if (listLen(arglist,vm)) return Int_toBase(self,arglist,vm);
//   return stringCreate(_i64toa(INT64V(self),buf,10),I_OWNED,vm);
   return stringCreate(intToA(INT64V(self),buf),I_OWNED,vm);
}

#if 0	//!!!
    // Int|Float.thousandsMark()    --> ","
    // Int|Float.thousandsMark("_") --> "_" or UTF-8
Instance *Num_commaIs(Instance *self,pArglist arglist,pVM vm)
{
extern char *thousandsMark; //
   char *mark;
   if ((mark=arglistTryToGetString(arglist,0,0,vm)))
      thousandsMark=mark;
   return stringCreate(thousandsMark,I_OWNED,vm);
}
#endif

     // Int.toBool()
static Instance *Int_toBool(Instance *self,pArglist arglist,pVM vm)
   { return (INT64V(self) != 0) ? BoolTrue : BoolFalse; }

static Instance *Int_toInt(Instance *self,pArglist arglist,pVM vm)
{
   #if USE_POINTER_INTS   // self might be a PtrInt stuffed in a C register
      return _intCreate(INT64V(self),vm);
   #endif
   return self;
}

#if 0
    // compiler can prune this to a constant
    // Sparc is big endian int & little endian float
int isBigEndian()  // taken from GNU autoconf
{
    union {
        long int l;
        char c[sizeof (long int)];  // long int never 1 byte
    } u;

    u.l = 1;
    return (u.c[sizeof(long int)-1] == 1); 
}
int is_big_endian(void)
{
    union {
        uint32_t i;
        char c[4];
    } bint = {0x01020304};

    return bint.c[0] == 1; 
}
#endif

    // Int.toFloat([754])
static Instance *Int_toFloat(Instance *self,pArglist arglist,pVM vm)
   { return floatCreate((double)INT64V(self),vm); }

    // Int.toChar() --> character
static Instance *Int_toChar(Instance *self,pArglist arglist,pVM vm)
{
   UChar text[2];
   int n = (int)INT64V(self);
   if ((n < 0) || (n > 255))
   {
      char buf[100];
      sprintf(buf,"Int.toChar(%d): value must be in range [0,255]",n);
      vmThrow(vm,E_VALUE_ERROR,buf);
   }
   text[0] = (UChar)n; text[1] = '\0';
   return stringCreate((char *)text,I_OWNED,vm);
}

    // Int.create(n=self), works if self is a PtrInt
Instance *Int_create(Instance *self,pArglist arglist,pVM vm)
{
   Instance *i = arglistTryToGet(arglist,0);
   if (i) return intCreate(convertToInt(i,vm),vm); // don't short cut if result==i
//   return Zero;
   return self;
}

    // Int.abs([n])
static Instance *Int_abs(Instance *self,pArglist arglist,pVM vm)
{
   int64_t n = INT64V(self), i64;

   if (arglistTryToGetInt(arglist,0,&i64,0,vm)) n = i64;
   if (n < 0) return intCreate(-n,vm);
   #if USE_POINTER_INTS   // self might be a PtrInt stuffed in a C register
      return _intCreate(n,vm);
   #endif
   return self;
}

    // Int.pow(p) --> self^p
    // n>=0 only
static Instance *Int_pow(Instance *self,pArglist arglist,pVM vm)
{
   uint64_t n = INT64V(self), z=1;
   int      p = (int)arglistGetInt(arglist,0,0,vm);
   if(p<0) return Zero;
   while(p--) z*=n;
   return _intCreate(z,vm);
}

    // Int.shiftLeft(n)
static Instance *Int_shiftLeft(Instance *self,pArglist arglist,pVM vm)
{
   uint64_t n = INT64V(self);
   int64_t  s = arglistGetInt(arglist,0,"Int.shiftLeft",vm);
   if(s<0) return Zero;
   return intCreate(n << s,vm);
}

    // Int.shiftRight(n)
static Instance *Int_shiftRight(Instance *self,pArglist arglist,pVM vm)
{
   uint64_t n = INT64V(self);
   int64_t  s = arglistGetInt(arglist,0,"Int.shiftRight",vm);
//   if(s<0) return Zero;
   return intCreate(n >> s,vm);
}

    // Int.bitOr(mask...)
static Instance *Int_bitOr(Instance *self,pArglist arglist,pVM vm)
{
   int      i,numArgs = (int)listLen(arglist,vm);
   uint64_t x, n = INT64V(self);
   for (i = 0; i < numArgs; i++)
   {
      x = arglistGetInt(arglist,i,0,vm);
      n |= x;
   }
   return intCreate(n,vm);
}

    // Int.bitAnd(mask...)
static Instance *Int_bitAnd(Instance *self,pArglist arglist,pVM vm)
{
   int      i,numArgs = (int)listLen(arglist,vm);
   uint64_t m, n = (uint64_t)INT64V(self);
   for (i = 0; i < numArgs; i++)
   {
      m = (uint64_t)arglistGetInt(arglist,i,0,vm);
      n &= m;
   }
   return intCreate(n,vm);
}

    // Int.bitXor(mask...)
static Instance *Int_bitXor(Instance *self,pArglist arglist,pVM vm)
{
   int      i,numArgs = (int)listLen(arglist,vm);
   uint64_t x, n = INT64V(self);
   for (i = 0; i < numArgs; i++)
   {
      x = arglistGetInt(arglist,i,0,vm);
      n ^= x;
   }
   return intCreate(n,vm);
}

    // Int.BitNot()
static Instance *Int_bitNot(Instance *self,pArglist arglist,pVM vm)
{
   uint64_t n = INT64V(self);
   return intCreate(~n,vm);
}

#if 0
    // Int.__sGet: Get a byte: 0x12345[0] == 0x45, [1] == 0x23, [2] == 1
    // Endianness a issue here? Don't think so
    // ???? wouldn't it be more useful to get a digit?
static Instance *Int_sGet(Instance *self,pArglist arglist,pVM vm)
{
   int n = (int)arglistGetInt(arglist,0,"Int[n]",vm);
   if (n < 0 || n > 7)
      vmThrow(vm,E_INDEX_ERROR,"Int[n]: n is 0 to 7");
   return intCreate( (INT64V(self) >> (n<<3)) & 0xff, vm);
}
#endif

    // Int.toBigEndian(# bytes) --> T(...)
    // Convert lower n bytes to list of bytes in endian order
static Instance *
_bigLittle(Instance *self,pArglist arglist,pVM vm, int bigEndian)
{
   Instance *tuple;
   int	     bytes = (int)arglistGetInt(arglist,0,0,vm);
   uint64_t  n = INT64V(self);

   if (bytes < 1 || bytes > 8)
      vmThrow(vm,E_VALUE_ERROR,"Int.to?Endian: n is 1 to 8");
   tuple = tupleCreate(bytes,I_OWNED,vm);
   if (bigEndian)
   {
      unsigned int x, z = 8*(bytes - 1);
      while(bytes--)
      {
	 x = (unsigned int)((n >> z) & 0xFF); z -= 8;
	 tupleAppend(tuple,intCreate(x,vm));
      }
   }
   else		// little endian
   {
      unsigned int x;
      while(bytes--)
      {
	 x = (unsigned int)(n & 0XFF); n = (n >> 8);
	 tupleAppend(tuple,intCreate(x,vm));
      }
   }
   return tuple;
}

    // Int.toBigEndian([offset=0,len=8]) --> int
static Instance *Int_toBigEndian(Instance *self,pArglist arglist,pVM vm)
   { return _bigLittle(self,arglist,vm,1); }

    // Int.toLittleEndian([offset=0,len=8]) --> int
static Instance *Int_toLittleEndian(Instance *self,pArglist arglist,pVM vm)
   { return _bigLittle(self,arglist,vm,0); }


static SpinLock rlock;		// serialize random number generator

    // http://en.wikipedia.org/wiki/Xorshift
    // http://www.jstatsoft.org/v08/i14/paper
    // Pseudo random number generator designed by George Marsaglia.
    // This code is in the paper's Summary and Wikipedia article.
    // --> [0,2^32-1]
    // seeded in numberConstruct()
#define RMAX  (~(uint32_t)0)  // 32 bits, completely fits in double (52 bits)

static unsigned long _x = 123456789;
static unsigned long _y = 362436069;
static unsigned long _z = 521288629;
static unsigned long _w =  88675123; 

static uint32_t prng_xor128(int n)	// period 2^128 - 1
{ 
   unsigned long t;
 
   SPIN_LOCK_ACQUIRE(&rlock);	//??? would races make this more random?
      while(n--){
	 t = (_x^(_x<<11));
	 _x = _y; _y = _z; _z = _w;
	 _w = (_w^(_w>>19)) ^ (t^(t>>8));
      }
   SPIN_LOCK_RELEASE(&rlock);
   return _w;
}

    /* Int.random([start, stop])
     * start.random(stop) --> Int.random(self,stop)
     * Return a random number, optionally in [start,stop)
     *   If start is float, result is float
     */
/*
  dist:=L(0,0,0,0,0,0,0,0,0,0);
  do(0d10_000_000){ n:=(0).random(10); dist[n]=dist[n]+1 } N:=dist.sum();
  dist.apply('wrap(n){"%.2f%%".fmt(n.toFloat()/N*100)});
  -->10.02%,10.00%,10.00%,9.99%,10.00%,10.00%,10.00%,10.01%,9.99%,10.00%
*/
static Instance *Int_rand(Instance *self,pArglist arglist,pVM vm)
{
   uint32_t R = prng_xor128(1);
   int      n = arglistLen(arglist,vm);

   if (n)
   {
      double a,b,width, r;
      a = arglistGetFloat(arglist,0,".random",vm); 
      if (n==1) { b = a; a = (double)INT64V(self); }
      else        b = arglistGetFloat(arglist,1,".random",vm);

      if (b < a) { r = b; b = a; a = r; }   // force b >= a
      width = b - a;
      r = a + ((double)R / (RMAX + 1.0) * width);
      if (n==1 || TYPEO(arglistGet(arglist,0,0,vm)) == IntType) 
	 return intCreate((int64_t)floor(r),vm);
      return floatCreate(r,vm);
   }

   return intCreate(R,vm);
}

typedef struct { int64_t n; unsigned count, star; } Plus1;
static Instance *_plus1(Instance *self,size_t idx,void *puss,size_t sz,pVM vm)
{
   // don't change contents of Plus1 so walker.reset() works
   // idx is [0..], sz is the number of instances written
   Plus1 *p1 = (Plus1 *)puss;
   if (!p1->star && (idx >= p1->count)) return 0;
   return intCreate(p1->n + idx,vm);
}

   // Fill in a Plus1 for n.pump([count],...)
   // Returns: index of first arg for pump
static int setPlus1(Plus1 *p, Instance *pn, pArglist arglist, pVM vm)
{
   Instance *s = arglistGet(arglist,0,"Int.filter/pump/reduce/walker",vm);
   p->n = INT64V(pn);
   if(s==Star){ p->star = 1; p->count = 0; return 1; }  // .pump(*,sink,f
   else
   {
      int64_t count=0;

      p->star = 0;
      if (!iWantInt(s,&count,vm))  // .pump(sink,f
      {
	 count = INT64V(pn);
	 p->count = (count<0) ? 0 : (unsigned)count;
	 p->n     = 0;
	 return 0;
      }
      else			   // .pump(n,sink,f
      {
	 p->count = (count<0) ? 0 : (unsigned)count;
	 return 1;
      }
   }
}

    // Int.pump(count,sink,f,...) --> [self..self+count-1].pump(sink,...)
    // Int.pump(sink,f,...) --> [0..self-1].pump(sink,...) == (0).pump(self,...)
//!!!??? if count, alloc list as sink and pass write method?
static Instance *Int_pump(Instance *self,pArglist arglist,pVM vm)
{
   Plus1 p1;
   return pump(self,0x00,_plus1,(void *)&p1,arglist,
	       setPlus1(&p1,self,arglist,vm), vm);
}

    // Int.reduce([count],f): [self..count].reduce(f)
    // [0..self-1] or [self..self+count-1]
static Instance *Int_reduce(Instance *self,pArglist arglist,pVM vm)
{
   Plus1 p1;
   return zreduce(self,_plus1,(void *)&p1,arglist,
   		  setPlus1(&p1,self,arglist,vm), vm);
}

    // Int.filter([count],f): [self..count].filter(f)
    // [0..self-1] or [self..self+count-1]
static Instance *Int_filter(Instance *self,pArglist arglist,pVM vm)
{
   Plus1 p1;
   return zfilter(self,0x0,_plus1,(void *)&p1,arglist,
   		  setPlus1(&p1,self,arglist,vm), vm);
}

    // Int.filter1([count],f): [self..count].filter(f)
    // [0..self-1] or [self..self+count-1]
static Instance *Int_filter1(Instance *self,pArglist arglist,pVM vm)
{
   Plus1 p1;
   return zfilter1(self,0x0,_plus1,(void *)&p1,arglist,
		   setPlus1(&p1,self,arglist,vm), vm);
}

    // Int.walker() --> [0..self-1]
    // Int.walker(count|*) --> [self..self+count-1], [self..]
//!!!??? step + up and down
static Instance *Int_walker(Instance *self,pArglist arglist,pVM vm)
{
   Plus1 *p1 = (Plus1 *)ZCALLOC(1,sizeof(Plus1));  //!!! check

   if (arglistLen(arglist,vm))	// walker(count|*)
      setPlus1(p1,self,arglist,vm);
   else		// everything else is zero
      p1->count = (INT64V(self) < 0) ? 0 : (unsigned)INT64V(self);
   return walkerCreate(Zero,_plus1,(void *)p1,sizeof(Plus1),vm);
}

Instance *walkToTheSun(pVM vm)	// 0,1,2,...
{
   Plus1 *p1 = (Plus1 *)ZCALLOC(1,sizeof(Plus1));  //!!! check
   p1->star  = 1;	// everything else is zero
   return walkerCreate(Zero, _plus1,(void *)p1,sizeof(Plus1),vm);
}

    // Bit Twiddling Hacks By Sean Eron Anderson
    // http://graphics.stanford.edu/~seander/bithacks.html

    // log2 is also the MSB (highest bit set)
    // Uggh: log(0xffff)/log(2) is really really close to 16 but isn't
unsigned int intLog2(uint64_t v)
{
   const uint64_t b[]={ 0x2, 0xC, 0xF0, 0xFF00, 0xFFFF0000, 0xFFFFFFFF00000000 };
   const unsigned int S[]={ 1, 2, 4, 8, 16, 32 };
   int i;

   unsigned int r = 0; // result of log2(v) will go here
   for (i = 5; i >= 0; i--) // unroll for speed...
   {
      if (v & b[i])
      {
	 v >>= S[i];
	 r |= S[i];
      } 
   }
   return r;
}

    // .log2([n]): --> log2 of unsigned self, 0.log2() --> 0
    // unsigned is good for MSB
static Instance *Int_log2(Instance *self,pArglist arglist,pVM vm)
{
#if 0
   int64_t n = INT64V(self), i64;
   if (arglistTryToGetInt(arglist,0,&i64,0,vm)) n = i64;
//   if (n <= 0) return Zero;
   return INT_CREATE(intLog2(n),vm);
#else
   return INT_CREATE(intLog2(INT64V(self)),vm);
#endif
}

    // .lsb(): --> index of lowest bit, 2.pow(n.lsb())-->mask
    // mask=n.bitAnd(n.bitNot()+1)
#define lsb64(n) ( (uint64_t)(n) & -(int64_t)(n) ) //== mask
static Instance *Int_lsb(Instance *self,pArglist arglist,pVM vm)
   { return INT_CREATE(intLog2(lsb64(INT64V(self))),vm); }


    // Int.len() --> # bytes in self
    // Int.len(base) --> # digits in base (16,10,2)
    //   10 is signed, others are unsigned
    //   This is the same as self.toString(b).len()
static Instance *Int_len(Instance *self,pArglist arglist,pVM vm)
{
   int64_t n = INT64V(self), i64;
   int	   base = 0, s = 0;

   if (!n) return One;	// (0).len(?) has 1 digit/byte in all bases
   if (arglistTryToGetInt(arglist,0,&i64,0,vm))
   {
      base = (int)i64;
      if (n < 0) { n = -n; s = 1; }	// -MIN == MIN, ie sign big stays set
   }

   switch(base)
   {
      case 2:  return INT_CREATE(1+intLog2(n),vm);
      case 16: return INT_CREATE(1+intLog2(n)/4,vm);
      case 10:	// signed
	 #if defined(__GNUC__) && !defined(__clang__)
	 // GCC 4.6.3 is fucked: n == -MIN: n<0, n==0, n>0 all false, n!=MIN
#if 0
	 {
	    int z;	// Christ, this is weird
//printf("--> %d %d %d %d\n",n<0 , n==0 , n>0, (n<0) + (n==0) + (n>0));
	    if (0 == (z=(n<0) + (n==0) + (n>0)))
	       return INT_CREATE(20,vm);
	    else {}	// required, as is z=...
	 }
#endif
	    if (n == -INT64_MIN) // yes, I *know* this is integer overflow
	       return INT_CREATE(20,vm);
         #else
	    if (n < 0) return INT_CREATE(20,vm);	// (-MIN == MIN) < 0
	 #endif
	 return INT_CREATE(1 + s + (int)log10((double)n),vm);
   }
   return INT_CREATE(1 + intLog2(n)/8,vm);	// bytes
}

    // Int.max(a,b,c) --> max of self & arglist --> Int
    // Int.max(List)  --> max in list --> Int == List[0].max(List.xplode())
    //    == arglist.reduce(a[0].max)
Instance *intMinMax(Instance *self,pArglist arglist,int what,pVM vm)
{
   int64_t   min,max;
   int       sz = arglistLen(arglist,vm), t, a=0,b=0, n;
   Instance *a0 = arglistTryToGet(arglist,0);

   if (a0 && ( (t=TYPEO(a0))==ListType || t==TupleType) )
   {
      Instance *slf = arglistTryToGet(a0,0);
      if(slf) return intMinMax(slf,a0,what,vm); 
      sz = 0;	// n.min(T) --> n
//      return intMinMax(arglistGet(a0,0,"Int.min/max(List)",vm),a0,what,vm);
   }

   iWantInt(self,&min,vm); max = min;
   for(n=0; n<sz; n++)	// might be zero
   {
      int64_t z = arglistGetInt(arglist,n,"Int.min/max",vm);
      if(z < min){ min = z; a = n; }
      if(z > max){ max = z; b = n; }
   }
   if(what==0) return intCreate(min,vm);
   if(what==1) return intCreate(max,vm);
   if(what==2) return tupleCreateX(vm,intCreate(min,vm),intCreate(max,vm),ZNIL);
   return tupleCreateX(vm,intCreate(a,vm),intCreate(b,vm),ZNIL);
}
static Instance *Int_min(Instance *self,pArglist arglist,pVM vm)
   { return(intMinMax(self,arglist,0x0,vm)); }
static Instance *Int_max(Instance *self,pArglist arglist,pVM vm)
   { return(intMinMax(self,arglist,0x1,vm)); }
static Instance *Int_minMax(Instance *self,pArglist arglist,pVM vm)
   { return(intMinMax(self,arglist,0x2,vm)); }
static Instance *Int_minMaxNs(Instance *self,pArglist arglist,pVM vm)
   { return(intMinMax(self,arglist,0x3,vm)); }

    // Int.clamp(min,max) --> min<=i<=max
static Instance *Int_clamp(Instance *self,pArglist arglist,pVM vm)
{
   int64_t min = arglistGetInt(arglist,0,"Int.clamp",vm),
	   max = arglistGetInt(arglist,1,"Int.clamp",vm), 
	   z;
   iWantInt(self,&z,vm);
   if (z > max)      z = max;
   else if (z < min) z = min;
   return intCreate(z,vm);
}

    // Int.divr(n) --> T(self/n,remainder)
    // Int.divr(a,b,c,...) --> T(self/a,remainder/b,remainder/c,remainder)
static Instance *Int_divr(Instance *self,pArglist arglist,pVM vm)
{
   #ifdef _MSC_VER	// lldiv_t is in a later version of Windows
      int64_t z=INT64V(self), n=arglistGetInt(arglist,0,"Int.divr",vm);
      return(tupleCreateX(vm,intCreate(z/n,vm),intCreate(z%n,vm),ZNIL));
   #else
    #if 1
      lldiv_t mr=lldiv(INT64V(self),arglistGetInt(arglist,0,"Int.divr",vm));
      return(tupleCreateX(vm,intCreate(mr.quot,vm),intCreate(mr.rem,vm),ZNIL));
    #else	// perl6's polymod
      int       n,N = (int)listLen(arglist,vm);
      lldiv_t   mr;
      Instance *r = tupleCreate(N+1,I_OWNED,vm);
      int64_t   a = INT64V(self);
      for(n=0; n<N; n++)
      {
	 mr = lldiv(a,arglistGetInt(arglist,n,"Int.divr",vm));
	 tupleAppend(r,intCreate(mr.rem,vm));
	 a = mr.quot;
      }
      tupleAppend(r,intCreate(a,vm));
      return r;
    #endif
   #endif
}

    // Int.gcd(n) -->Int
static Instance *Int_gcd(Instance *self,pArglist arglist,pVM vm)
{
   int64_t u=INT64V(self), v=arglistGetInt(arglist,0,"Int.gcd",vm), t;
   while(v) { t = u; u = v; v = t % v; }
   return intCreate(u < 0 ? -u : u,vm);
}

    // Int.split(base=10) -->T(digit,...) eg (-123).split()-->T(1,2,3)
static Instance *Int_split(Instance *self,pArglist arglist,pVM vm)
{
   int       sz=0, b=10, ns[70];  // 64 bits == 20 decimal digits max, 64 base 2
   int64_t   n;
   Instance *r;

   if (arglistTryToGetInt(arglist,0,&n,0,vm) && n>1 && n<37) b = (int)n;

   n=INT64V(self);
   if (n==0) return emptyTuple;
   if(n<0)   n = -n;
   while(n){ ns[sz++] = n%b; n/=b; }
   r=tupleCreate(sz,I_OWNED,vm);
   while(sz--) tupleAppend(r,intCreate(ns[sz],vm));
   return r;
}

    // Int.toList() -->T(0,1,..self) == self.walker().walk(), self.pump(List)
    // start.toList(stop) == self.walker(stop-start).walk()
static Instance *Int_toList(Instance *self,pArglist arglist,pVM vm)
{
   int       sz = 0;
   int64_t   start = 0, stop = INT64V(self);
   Instance *r;
   Fence  fence;

   if(arglistTryToGetInt(arglist,0,&stop,0,vm)) start = INT64V(self);

   sz = (int)(stop - start); if(sz<0) return emptyTuple;
   vmSetFence(vm,&fence,0,r = tupleCreate(sz,I_OWNED,vm));
      while(sz--) tupleAppend(r,intCreate(start++,vm));
   vmRemoveFence(&fence,0);
   return r;
}


static const MethodTable intMethods[] = 
{
   "create",		(pMethod)Int_create,
   "toString",		(pMethod)Int_toString,
   "toBool",		(pMethod)Int_toBool,
   "toInt",		(pMethod)Int_toInt,
   "toFloat",		(pMethod)Int_toFloat,
   "toChar",		(pMethod)Int_toChar,
   "abs",		(pMethod)Int_abs,
   "shiftLeft",		(pMethod)Int_shiftLeft,
   "shiftRight",	(pMethod)Int_shiftRight,
   "bitOr",		(pMethod)Int_bitOr,
   "bitAnd",		(pMethod)Int_bitAnd,
   "bitXor",		(pMethod)Int_bitXor,
   "bitNot",		Int_bitNot,
//   "__sGet",		(pMethod)Int_sGet,
   "toBigEndian",	(pMethod)Int_toBigEndian,
   "toLittleEndian",	(pMethod)Int_toLittleEndian,
   "random",		(pMethod)Int_rand,
   "walker",		Int_walker,
   "filter",		Int_filter,
   "filter1",		Int_filter1,
   "reduce",		Int_reduce,
   "pump",		Int_pump,
   "log2",		Int_log2,
   "lsb",		Int_lsb,
   "len",		Int_len,
   "min",		Int_min,
   "max",		Int_max,
   "minMax",		Int_minMax,
   "minMaxNs",		Int_minMaxNs,
   "clamp",		Int_clamp,
   "divr",		Int_divr,
   "gcd",		Int_gcd,
   "pow",		Int_pow,
   "split",		Int_split,
   "toList",		Int_toList,

   "fromIEEE754",	Int_fromIEEE754,
   0,			0,
};


////////////////////// Int Properties ////////////////////////

    // Int.MIN
static Instance *Int_MIN(Instance *self, pVM vm)
#ifdef INT64_MIN	// stdint.h
   { return intCreate(INT64_MIN,vm); }
#elif defined(_I64_MIN)
   { return intCreate(_I64_MIN,vm); }
#else
   { return intCreate(LLONG_MIN,vm); }
#endif

    // Int.MAX
static Instance *Int_MAX(Instance *self, pVM vm)
#ifdef INT64_MAX	// stdint.h
   { return intCreate(INT64_MAX,vm); }
#elif defined(_I64_MAX)
   { return intCreate(_I64_MAX,vm); }
#else
   { return intCreate(LLONG_MAX,vm); }
#endif

    // Int.sign: >0 --> 1, 0 --> 0, <0 --> -1
static Instance *Int_sign(Instance *self, pVM vm)
{	// n < 0 ? -1 : n > 0;
   int64_t n = INT64V(self);
   if (n < 0) return MinusOne;
   if (n > 0) return One;
   return Zero;
}

    // Int.isEven: is self even?
static Instance *Int_isEven(Instance *self, pVM vm)
   { return (INT64V(self)&1) ? BoolFalse : BoolTrue; }

    // Int.isOdd
static Instance *Int_isOdd(Instance *self, pVM vm)
   { return (INT64V(self)&1) ? BoolTrue : BoolFalse; }

    // Int.numDigits --> num digits in self. Zero has one digit.
    // Am I sure? "0" is one character
static Instance *Int_numDigits(Instance *self, pVM vm)
{
   int64_t n = INT64V(self);
   int     cnt = 1;
//   if(!n) return Zero;
   while(n/=10) cnt++;
   return intCreate(cnt,vm);
}

#if 0
   // or num1s()==1
int isPowerOfTwo(unsigned int n){ return ((n != 0) && !(n & (n - 1))); }
int isPowerOfTwo(unsigned int n){ return ((n != 0) && ((n & (~n + 1)) == n)); }
#endif

    // Count number of 1 bits in an int
    // Counting bits set, Brian Kernighan's way
    // http://graphics.stanford.edu/
    //    ~seander/bithacks.html#CountBitsSetKernighan
static Instance *Int_num1s(Instance *self,pVM vm)
{
   uint64_t v = INT64V(self); // count the number of bits set in v
   unsigned c;	     // c accumulates the total bits set in v
   for (c = 0; v; c++)
   {
      v &= v - 1; // clear the least significant bit set
   }
   return INT_CREATE(c,vm);
}

#if 0
//Round up to the next highest power of 2

unsigned int v; // compute the next highest power of 2 of 32-bit v

v--;
v |= v >> 1;
v |= v >> 2;
v |= v >> 4;
v |= v >> 8;
v |= v >> 16;
v++;

  v--;
  for (int i = 1; i < sizeof(v) * CHAR_BIT; i *= 2) {
    v |= v >> i;
  }
  return ++v;
#endif

    // Int.nextPowerOf2 -->Int
static Instance *Int_nextPowerOf2(Instance *self,pVM vm)
{
   int64_t v = INT64V(self);
   v--;
   for (int i=1; i<sizeof(v)*CHAR_BIT; i*=2) v|=v>>i;  // limits.h,8
   return intCreate(v+1,vm);
}

    // .text == toChar: byte to char
static Instance *Int_text(Instance *self,pVM vm)
   { return Int_toChar(self,NoArglist,vm); }

    // .minutia
static Instance *Int_minutia(Instance *self,pVM vm)
{
   printf("Self is a %sint\n",IS_PtrInt(self) ? "pointer " : "");
#if USE_POINTER_INTS
   #if PTRSZ_IS_64
      return kStringCreate("63 bit pointer ints",0,I_OWNED,vm);
   #elif PTRSZ_IS_32
      return kStringCreate("31 bit pointer ints",0,I_OWNED,vm);
   #endif
#endif
   return Void;
}

static const PropertyTable intProperties[] =
{
   "MIN",		(pProperty)Int_MIN,
   "MAX",		(pProperty)Int_MAX,
   "sign",		(pProperty)Int_sign,
   "isEven",		(pProperty)Int_isEven,
   "isOdd",		(pProperty)Int_isOdd,
   "numDigits",		(pProperty)Int_numDigits,
   "num1s",		Int_num1s,
   "nextPowerOf2",	Int_nextPowerOf2,
   "text",		Int_text,
   "minutia",		Int_minutia,
   0,			0
};

////////////////////// Int Op Codes //////////////////////////

    // Float/Int/Bool/PtrInt/AtomicInt --> int64
__inline int iWantInt(Instance *x, int64_t *n, pVM vm)
{
   #if USE_POINTER_INTS
      if (IS_PtrInt(x)) { *n = PtrInt_TO_N(x); return 1; }
   #endif
   switch(TYPEO1(x))
   {
      case IntType:       *n = INT64V(x);          break;
      case FloatType:     *n = (int64_t)FLOATV(x); break;
      case AtomicIntType: *n = atomicGet(x);	   break;
      case DeferredType:  return iWantInt(Deferred_value(x,vm),n,vm);
//!!!??? BigNum?
      default: return 0;
   }
   return 1;
}

    // if can't get an int, they ain't equal
Instance *Int_eq(Instance *self,Instance *X,pVM vm)
{
   int64_t x;
   if (!iWantInt(X,&x,vm)) return BoolFalse;
   return (INT64V(self) == x) ? BoolTrue : BoolFalse;
}
#if USE_POINTER_INTS
Instance *PtrInt_eq(Instance *self,Instance *X,pVM vm)
{
   int64_t x;
   if (!iWantInt(X,&x,vm)) return BoolFalse;
   return (PtrInt_TO_N(self) == x) ? BoolTrue : BoolFalse;
}
#endif

static Instance *Int_neq(Instance *self,Instance *X,pVM vm)
{
   int64_t x;
   if (!iWantInt(X,&x,vm)) return BoolTrue;
   return (INT64V(self) != x) ? BoolTrue : BoolFalse;
}
#if USE_POINTER_INTS
Instance *PtrInt_neq(Instance *self,Instance *X,pVM vm)
{
   int64_t x;
   if (!iWantInt(X,&x,vm)) return BoolTrue;
   return (PtrInt_TO_N(self) != x) ? BoolTrue : BoolFalse;
}
#endif

//!!!! shouldn't these be iWantInt??? 123 + "1"???
static Instance *Int_lt(Instance *self,Instance *X,pVM vm)
{
   int64_t x = convertToInt(X,vm);
   return (INT64V(self) < x) ? BoolTrue : BoolFalse;
}

static Instance *Int_lte(Instance *self,Instance *X,pVM vm)
{
   int64_t x = convertToInt(X,vm);
   return (INT64V(self) <= x) ? BoolTrue : BoolFalse;
}

static Instance *Int_gt(Instance *self,Instance *X,pVM vm)
{
   int64_t x = convertToInt(X,vm);
   return (INT64V(self) > x) ? BoolTrue : BoolFalse;
}

static Instance *Int_gte(Instance *self,Instance *X,pVM vm)
{
   int64_t x = convertToInt(X,vm);
   return (INT64V(self) >= x) ? BoolTrue : BoolFalse;
}

static Instance *Int_add(Instance *self,Instance *X,pVM vm)
{
   int64_t x = convertToInt(X,vm);
   return intCreate(INT64V(self) + x,vm);
}

static Instance *Int_sub(Instance *self,Instance *X,pVM vm)
{
   int64_t x = convertToInt(X,vm);
   return intCreate(INT64V(self) - x,vm);
}

static Instance *Int_mul(Instance *self,Instance *X,pVM vm)
{
   int64_t x = convertToInt(X,vm);
   return intCreate(INT64V(self) * x,vm);
}

static Instance *idiv(int64_t n,Instance *X,pVM vm)
{
   int64_t x = convertToInt(X,vm);

   #ifdef _MSC_VER		// use MSVC execption handling 
      __try { return intCreate(n / x, vm); }
      __except(GetExceptionCode() == EXCEPTION_INT_DIVIDE_BY_ZERO ? 
		   EXCEPTION_EXECUTE_HANDLER : EXCEPTION_CONTINUE_SEARCH)
      {
	 vmThrow(vm,E_MATH_ERROR,"INF (number is infinite), Int divide by zero");
      }
      return Zero;		// shut up the compiler
  #else
//!!!! core dumper on Unix: (-9223372036854775807-1) / -1
      if (x == 0) vmThrow(vm,E_MATH_ERROR,
			  "INF (number is infinite), Int divide by zero");
      return intCreate(n / x, vm);
   #endif
}
static __inline Instance *Int_div(Instance *self,Instance *X,pVM vm)
   { return idiv(INT64V(self),X,vm); }
#if USE_POINTER_INTS
Instance *PtrInt_div(Instance *self,Instance *X,pVM vm)
   { return idiv(PtrInt_TO_N(self),X,vm); }
#endif

static __inline Instance *imod(int64_t n,Instance *X,pVM vm)
{
   int64_t x = convertToInt(X,vm);
   if (x == 0) return Zero;
   return intCreate(n % x,vm);
}
static __inline Instance *Int_mod(Instance *self,Instance *X,pVM vm)
   { return imod(INT64V(self),X,vm); }
#if USE_POINTER_INTS
Instance *PtrInt_mod(Instance *self,Instance *X,pVM vm)
   { return imod(PtrInt_TO_N(self),X,vm); }
#endif

static Instance *Int_negate(Instance *self,Instance *notUsed,pVM vm)
   { return intCreate(-INT64V(self),vm); }

static const OpcodeTable intOpcodes[] = 
{
   OP_EQ,	(pOp)Int_eq,
   OP_NEQ,	(pOp)Int_neq,

   OP_LT,	(pOp)Int_lt,
   OP_LTE,	(pOp)Int_lte,
   OP_GT,	(pOp)Int_gt,
   OP_GTE,	(pOp)Int_gte,

   OP_ADD,	(pOp)Int_add,
   OP_SUB,	(pOp)Int_sub,
   OP_MUL,	(pOp)Int_mul,
   OP_DIV,	(pOp)Int_div,
   OP_MOD,	(pOp)Int_mod,

   OP_NEGATE,	(pOp)Int_negate,

   0,		0
};


/* ******************************************************************** */
/* ****************************** Float ******************************* */
/* ******************************************************************** */

#ifdef __unix__
   #define _finite(n)	finite(n) 
   #define _isnan(n)	isnan(n)
#endif

Instance *floatCreate(double x, pVM vm)
{
   ZKL_Float *i;

   if (x == 0.0) return FZero;
   if (x == 1.0) return FOne;
#if 0
   if (!_finite(x))
   {
      if (_isnan(x)) vmThrow(vm,E_MATH_ERROR,"NaN (Not a number)");
      vmThrow(vm,E_MATH_ERROR,"INF (number is infinite)");
   }
#endif

   i = (ZKL_Float *)ibucketAllocate(nBuckets,&FloatObject,I_OWNED,1,vm);
   i->value = x;
   return (Instance *)i;
}

///////////////////////// Float Methods /////////////////////////////

    // toString([precision])
    // IEEE754 64 bit max exponents: -383, +384	so make buf 500 or so,
    //    700 if you want commas
    // "%f".fmt(1e308).len() --> 316, is the largest float I parse
    // DON'T use %E and comma (I only test for %e)
char *floatToString(Instance *self,char *buf,int precision,char feg, char comma)
{
#if 1
   char format[10];	// _CVTBUFSIZE

   sprintf(format,"%%.%d%c",precision,feg);  // eg "%.6f"
   sprintf(buf,format,FLOATV(self));
   if(comma && !strchr(buf,'e'))
   { //src.reverse().pump(List,T(Void.Read,2,False),String).concat(",").reverse();
      char b2[700], *src=buf, *dst=b2, *dot;
      int  n;
      if((dot = strchr(buf,'.'))) *dot = '\0';
      if(*buf=='-'){ src++; *dst++ = '-'; }
      if((n = strlen(src)) > 3)
      {
	 if(!(n %= 3)) n = 3;
	 strncpy(dst,src,n); src += n; dst += n;
	 for(n=0; *src; n++){
	    if(!(n%3)) *dst++ = comma;
	    *dst++ = *src++;
	 }
	 *dst = '\0';
	 if(dot){ *dot = '.'; strcpy(dst,dot); }
	 strcpy(buf,b2);
      }
      else if(dot) *dot = '.';
   }
   return buf;
#else
   _gcvt(FLOATV(self),p,buf);
   s = _ecvt_s(buf,_CVTBUFSIZE,FLOATV(self),*n,&dot,&sign);
   	copy and modify
   s = _fcvt_s(buf,_CVTBUFSIZE,FLOATV(self),&n,&dot,&sign);
   	copy and modify

   return stringCreate(ptr,I_OWNED,vm);
#endif
}

    // Float.toString([precision[,format]]
static Instance *Float_toString(Instance *self,pArglist arglist,pVM vm)
{
   char	    buf[700];	// _CVTBUFSIZE?
   char	    format = 'g';
   int	    p = 6;
   int64_t  i64;
   char	   *pf = arglistTryToGetString(arglist,1,0,vm);

   if (arglistTryToGetInt(arglist,0,&i64,0,vm)) p = (int)i64;
   if (pf)
   {
      format = *pf;
      if (!strchr("feEgG",format))
         vmThrow(vm,E_VALUE_ERROR,
	    "Float.toString([precision,format]), format one of f,e,E,g,G");
   }
   return stringCreate(floatToString(self,buf,p,format,0),I_OWNED,vm);
}

    // Float.toBool()
static Instance *Float_toBool(Instance *self,pArglist arglist,pVM vm)
   { return (0.0 != FLOATV(self) ? BoolTrue : BoolFalse); }

    // Float.toInt([754]), Intel x86 is little endian
static Instance *Float_toInt(Instance *self,pArglist arglist,pVM vm)
   { return intCreate((int64_t)FLOATV(self),vm); }

    // Float.create(f=self)
static Instance *Float_create(Instance *self,pArglist arglist,pVM vm)
{
   Instance *i = arglistTryToGet(arglist,0);
   if (i) return floatCreate(FLOATV(convertTo(i,TO_FLOAT,vm)),vm);
//   return FZero;
   return self;
}

    // Float.len() --> # bytes in self
static Instance *Float_len(Instance *self,pArglist arglist,pVM vm)
   { return INT_CREATE(sizeof(FLOATV(self)),vm); }	// bytes

    // Float.abs()
static Instance *Float_abs(Instance *self,pArglist arglist,pVM vm)
{
   double n = FLOATV(self);
   if (n < 0.0) return floatCreate(-n,vm);
   return self;
}

    // Float.ceil()
static Instance *Float_ceil(Instance *self,pArglist arglist,pVM vm)
   { return floatCreate(ceil(FLOATV(self)),vm); }

    // Float.floor()
static Instance *Float_floor(Instance *self,pArglist arglist,pVM vm)
   { return floatCreate(floor(FLOATV(self)),vm); }

    // Float.round()
static Instance *Float_round(Instance *self,pArglist arglist,pVM vm)
#if 0 //defined(_MSC_VER) && _MSC_VER<=1500   // XP only, hack
{
   double n = FLOATV(self);
   if(n>0) modf(n + 0.5,&n);
   else    modf(n - 0.5,&n);
   return floatCreate(n,vm);
}
#else
   { return floatCreate(round(FLOATV(self)),vm); }
#endif

	////////////////////////////////// Trig

    // .sin(), .asin()
static Instance *Float_sin(Instance *self,pArglist arglist,pVM vm)
   { return floatCreate(sin(FLOATV(self)),vm); }

static Instance *Float_asin(Instance *self,pArglist arglist,pVM vm)
   { return floatCreate(asin(FLOATV(self)),vm); }

    // .cos(), .acos()
static Instance *Float_cos(Instance *self,pArglist arglist,pVM vm)
   { return floatCreate(cos(FLOATV(self)),vm); }

static Instance *Float_acos(Instance *self,pArglist arglist,pVM vm)
   { return floatCreate(acos(FLOATV(self)),vm); }


    // .tan(), .atan()
static Instance *Float_tan(Instance *self,pArglist arglist,pVM vm)
   { return floatCreate(tan(FLOATV(self)),vm); }

static Instance *Float_atan(Instance *self,pArglist arglist,pVM vm)
   { return floatCreate(atan(FLOATV(self)),vm); }

    // .atan2(x) --> atan2(self/x)
static Instance *Float_atan2(Instance *self,pArglist arglist,pVM vm)
{
   double x = arglistGetFloat(arglist,0,0,vm);
   return floatCreate(atan2(FLOATV(self),x),vm);
}

    // .sinh(), .cosh(), .tanh()
static Instance *Float_sinh(Instance *self,pArglist arglist,pVM vm)
   { return floatCreate(sinh(FLOATV(self)),vm); }

static Instance *Float_cosh(Instance *self,pArglist arglist,pVM vm)
   { return floatCreate((FLOATV(self)),vm); }

static Instance *Float_tanh(Instance *self,pArglist arglist,pVM vm)
   { return floatCreate(tanh(FLOATV(self)),vm); }

	/////////////////////////// Log, etc
    // .exp(), .log(), .log10()
static Instance *Float_exp(Instance *self,pArglist arglist,pVM vm)
   { return floatCreate(exp(FLOATV(self)),vm); }

static Instance *Float_log(Instance *self,pArglist arglist,pVM vm)
   { return floatCreate(log(FLOATV(self)),vm); }

static Instance *Float_log10(Instance *self,pArglist arglist,pVM vm)
   { return floatCreate(log10(FLOATV(self)),vm); }

    // .pow(p) --> self^p
static Instance *Float_pow(Instance *self,pArglist arglist,pVM vm)
{
   double p = arglistGetFloat(arglist,0,0,vm);
   return floatCreate(pow(FLOATV(self),p),vm);
}

    // .sqrt()
static Instance *Float_sqrt(Instance *self,pArglist arglist,pVM vm)
{
   if (FLOATV(self) < 0.0) vmThrow(vm,E_MATH_ERROR,"sqrt: value less than 0.0");
   return floatCreate(sqrt(FLOATV(self)),vm);
}

    // .hypot(n) --> hypot(self,n) --> (self^2 + n^2).sqrt()
static Instance *Float_hypot(Instance *self,pArglist arglist,pVM vm)
{
   return floatCreate(hypot(FLOATV(self),arglistGetFloat(arglist,0,0,vm)),vm);
}

    // .modf(fractionalPartAlwaysPositive=False)
//!!!OK, this is a fuckup, (2e+24).modf() overflows int
static Instance *Float_modf(Instance *self,pArglist arglist,pVM vm)
{
   int    alwaysPositive = arglistTryToGetBool(arglist,0,0,".modf",vm);
   double whole, frac;

   frac = modf(FLOATV(self), &whole);
   if (alwaysPositive) frac = fabs(frac);
   return tupleCreateX(vm,
		intCreate((int64_t)whole,vm),floatCreate(frac,vm), ZNIL);
}

     // .frexp(): return L(x,n) s.t. self == (x * 2^n), x in [0.5,1)
static Instance *Float_frexp(Instance *self,pArglist arglist,pVM vm)
{
   int		n;
   double	frac;

   frac = frexp(FLOATV(self),&n);
   return tupleCreateX(vm, floatCreate(frac,vm),intCreate((int64_t)n,vm),ZNIL);
}

	////////////////////////////////// Degree/radian conversions
#define E  2.71828182845904523536
#define PI 3.141592653589793238462643383279502884197169399375

#define RADS_TO_DEG(rads) ( rads * (180.0 / PI) )
#define DEG_TO_RADS(deg)  ( deg  * (PI / 180.0) )

    // .toRad(), .toDeg()
static Instance *Float_toRad(Instance *self,pArglist arglist,pVM vm)
   { return floatCreate(DEG_TO_RADS(FLOATV(self)),vm); }

static Instance *Float_toDeg(Instance *self,pArglist arglist,pVM vm)
   { return floatCreate(RADS_TO_DEG(FLOATV(self)),vm); }


	///////////////////////////////// Polar conversions
    // .toPolar(y) -->(r,angle), angle is (-pi..pi]
static Instance *Float_toPolar(Instance *self,pArglist arglist,pVM vm)
{
   double x     = FLOATV(self);
   double y     = arglistGetFloat(arglist,0,0,vm);
   double r     = hypot(x,y);
   double theta = 0;

   if(x>0)		  theta = atan(y/x);
   else if (x<0  && y>=0) theta = atan(y/x) + PI;
   else if (x<0  && y<0)  theta = atan(y/x) - PI;
   else if (x==0 && y>0)  theta = PI/2;
   else if (x==0 && y<0)  theta = -PI/2;
   else if (x==0 && y==0) theta = 0;
   return tupleCreateX(vm,floatCreate(r,vm),floatCreate(theta,vm),ZNIL);
}

    // .toRectangular(angle) -->(x,y)
static Instance *Float_toRectangular(Instance *self,pArglist arglist,pVM vm)
{
   double theta = arglistGetFloat(arglist,0,0,vm);
   double x     = FLOATV(self) * cos(theta);
   double y     = FLOATV(self) * sin(theta);

   return tupleCreateX(vm,floatCreate(x,vm),floatCreate(y,vm),ZNIL);
}

#if 0	// This would be nice, needs C runtime DLL to link
	// Handle math errors
int _matherr(struct _exception *except)
{
   printf("boom: %d\n",except->type);
   return 0;
}
#endif

    // .closeTo(y,tolerance) --> abs(self-n) <= tolerance
    // if tolerance<0, use relative comparison
    // https://en.wikipedia.org/wiki/Relative_change_and_difference
static Instance *Float_closeTo(Instance *self,pArglist arglist,pVM vm)
{
   double y     = arglistGetFloat(arglist,0,0,vm);
   double tol   = arglistGetFloat(arglist,1,0,vm);
   double x     = FLOATV(self), max, xabs=fabs(x), yabs=fabs(y);
   double delta = fabs(x - y);

   if(tol>=0 || x==0 || y==0) return ( delta <= tol ) ? BoolTrue : BoolFalse;
//!!! probably bad if max is close to zero
   max = xabs; if(max<yabs) max = yabs;
   return (delta/max <= -tol) ? BoolTrue : BoolFalse;
}

    // Float.max(a,b,c) --> max of self & arglist --> Float
static Instance *floatMinMax(Instance *self,pArglist arglist,int what,pVM vm)
{
   double    min,max;
   int       sz = arglistLen(arglist,vm), t, a=0,b=0, n;
   Instance *a0 = arglistTryToGet(arglist,0);

   if (a0 && ( (t=TYPEO(a0))==ListType || t==TupleType) )
      return floatMinMax(arglistGet(a0,0,"Int.min/max",vm),a0,what,vm);

   iWantFloat(self,&min,vm); max = min;
   for(n=0; n<sz; n++)	// might be zero
   {
      double x = arglistGetFloat(arglist,n,"Float.min/max",vm);
      if(x < min){ min = x; a = n; }
      if(x > max){ max = x; b = n; }
   }
   if(what==0) return floatCreate(min,vm);
   if(what==1) return floatCreate(max,vm);
   if(what==2) return tupleCreateX(vm,floatCreate(min,vm),floatCreate(max,vm),ZNIL);
   return tupleCreateX(vm,intCreate(a,vm),intCreate(b,vm),ZNIL);
}
static Instance *Float_min(Instance *self,pArglist arglist,pVM vm)
   { return(floatMinMax(self,arglist,0x0,vm)); }
static Instance *Float_max(Instance *self,pArglist arglist,pVM vm)
   { return(floatMinMax(self,arglist,0x1,vm)); }
static Instance *Float_minMax(Instance *self,pArglist arglist,pVM vm)
   { return(floatMinMax(self,arglist,0x2,vm)); }
static Instance *Float_minMaxNs(Instance *self,pArglist arglist,pVM vm)
   { return(floatMinMax(self,arglist,0x3,vm)); }


    // Float.clamp(min,max) --> min<=f<=max
static Instance *Float_clamp(Instance *self,pArglist arglist,pVM vm)
{
   double min = arglistGetFloat(arglist,0,"Float.clamp",vm),
	  max = arglistGetFloat(arglist,1,"Float.clamp",vm), 
	  z;
   iWantFloat(self,&z,vm);
   if (z > max)      z = max;
   else if (z < min) z = min;
   return floatCreate(z,vm);
}

    // (start).random(stop)
    //???(x).random() --> (0.0).random(1.0)? (0.0).random(x)?
static Instance *Float_rand(Instance *self,pArglist arglist,pVM vm)
{
   MLIST(mlist,2);
   mlistBuild(mlist,self,arglistGet(arglist,0,"Float.random",vm),ZNIL);
   return Int_rand(Zero,(Instance *)mlist,vm);
}


static const MethodTable floatMethods[] = 
{
   "create",		(pMethod)Float_create,
   "toString",		(pMethod)Float_toString,
   "toBool",		(pMethod)Float_toBool,
   "toInt",		(pMethod)Float_toInt,
   "toFloat",		(pMethod)Object_noop,
   "random",		(pMethod)Float_rand,
   "len",		Float_len,

   "abs",		(pMethod)Float_abs,
   "closeTo",		(pMethod)Float_closeTo,
   "ceil",		(pMethod)Float_ceil,
   "floor",		(pMethod)Float_floor,
   "round",		(pMethod)Float_round,


   "toDeg",		(pMethod)Float_toDeg,
   "toRad",		(pMethod)Float_toRad,
   "toPolar",		(pMethod)Float_toPolar,
   "toRectangular",	(pMethod)Float_toRectangular,

   "sin",		(pMethod)Float_sin,
   "asin",		(pMethod)Float_asin,
   "cos",		(pMethod)Float_cos,
   "acos",		(pMethod)Float_acos,
   "tan",		(pMethod)Float_tan,
   "atan",		(pMethod)Float_atan,
   "atan2",		(pMethod)Float_atan2,

   "sinh",		(pMethod)Float_sinh,
   "cosh",		(pMethod)Float_cosh,
   "tanh",		(pMethod)Float_tanh,

   "exp",		(pMethod)Float_exp,
   "log",		(pMethod)Float_log,
   "log10",		(pMethod)Float_log10,
   "pow",		(pMethod)Float_pow,

   "sqrt",		(pMethod)Float_sqrt,
   "hypot",		(pMethod)Float_hypot,
   "modf",		(pMethod)Float_modf,
   "frexp",		(pMethod)Float_frexp,

   "min",		Float_min,
   "max",		Float_max,
   "minMax",		Float_minMax,
   "minMaxNs",		Float_minMaxNs,
   "clamp",		Float_clamp,

   "toIEEE754",		Float_toIEEE754,
   0,			0
};


////////////////////// Float Properties //////////////////////////

static ZKL_Float  _pi, _e;
static Instance  *pi = (Instance *)&_pi, *e = (Instance *)&_e;

static Instance *Float_pi(Instance *self, pVM vm) { return pi; }

static Instance *Float_e(Instance *self, pVM vm)  { return e; }

    // Float.MIN
static Instance *Float_MIN(Instance *self, pVM vm)
   { return floatCreate(DBL_MIN,vm); }

    // Float.MAX
static Instance *Float_MAX(Instance *self, pVM vm)
   { return floatCreate(DBL_MAX,vm); }

#if 1
//math.h: INFINITY, NAN, nanl()
//static double a_nan,a_inf;

    // Float.Inf, Float.NaN
static Instance *Float_Inf(Instance *self, pVM vm)
//   { return floatCreate(a_inf,vm); }
   { return floatCreate(INFINITY,vm); }
#endif

    // Float.sign: >0 --> 1, 0 --> 0, <0 --> -1
static Instance *Float_sign(Instance *self, pVM vm)
{
   double f = FLOATV(self);
   if (f < 0) return MinusOne;
   if (f > 0) return One;
   return Zero;
}

static Instance *Float_text(Instance *self, pVM vm)
   { return Float_toString(self,NoArglist,vm); }


static const PropertyTable floatProperties[] =
{
   "pi",	(pProperty)Float_pi,
   "e",		(pProperty)Float_e,
   "sign",	(pProperty)Float_sign,
   "text",	(pProperty)Float_text,
   "MIN",	(pProperty)Float_MIN,
   "MAX",	(pProperty)Float_MAX,
   "inf",	(pProperty)Float_Inf,
   0,		0
};

////////////////////////// Float Op Codes ///////////////////////////

    // Float/Int/PtrInt --> double
__inline int iWantFloat(Instance *x, double *f, pVM vm)
{
   #if USE_POINTER_INTS
      if (IS_PtrInt(x)) { *f = (double)PtrInt_TO_N(x); return 1; }
   #endif
   switch(TYPEO1(x))
   {
      case IntType:      *f = (double)INT64V(x);	break;
      case FloatType:    *f = FLOATV(x);		break;
      case DeferredType:  return iWantFloat(Deferred_value(x,vm),f,vm);
      default: return 0;
   }
   return 1;
}

    // if can't Get an float, they ain't equal
Instance *Float_eq(Instance *self,Instance *X,pVM vm)
{
   double x;
   if (!iWantFloat(X,&x,vm)) return BoolFalse;
   return (FLOATV(self) == x) ? BoolTrue : BoolFalse;
}

static Instance *Float_neq(Instance *self,Instance *X,pVM vm)
{
   double x;
   if (!iWantFloat(X,&x,vm)) return BoolTrue;
   return (FLOATV(self) != x) ? BoolTrue : BoolFalse;
}

static Instance *Float_lt(Instance *self,Instance *X,pVM vm)
{
   Instance *x = convertTo(X,TO_FLOAT,vm);
   return (FLOATV(self) < FLOATV(x) ? BoolTrue : BoolFalse);
}

static Instance *Float_lte(Instance *self,Instance *X,pVM vm)
{
   Instance *x = convertTo(X,TO_FLOAT,vm);
   return (FLOATV(self) <= FLOATV(x) ? BoolTrue : BoolFalse);
}

static Instance *Float_gt(Instance *self,Instance *X,pVM vm)
{
   Instance *x = convertTo(X,TO_FLOAT,vm);
   return (FLOATV(self) > FLOATV(x) ? BoolTrue : BoolFalse);
}

static Instance *Float_gte(Instance *self,Instance *X,pVM vm)
{
   Instance *x = convertTo(X,TO_FLOAT,vm);
   return (FLOATV(self) >= FLOATV(x) ? BoolTrue : BoolFalse);
}


static Instance *Float_add(Instance *self,Instance *X,pVM vm)
{
   Instance *x = convertTo(X,TO_FLOAT,vm);
   return floatCreate(FLOATV(self) + FLOATV(x),vm);
}

static Instance *Float_sub(Instance *self,Instance *X,pVM vm)
{
   Instance *x = convertTo(X,TO_FLOAT,vm);
   return floatCreate(FLOATV(self) - FLOATV(x),vm);
}

static Instance *Float_mul(Instance *self,Instance *X,pVM vm)
{
   Instance *x = convertTo(X,TO_FLOAT,vm);
   return floatCreate(FLOATV(self) * FLOATV(x),vm);
}

static Instance *Float_div(Instance *self,Instance *X,pVM vm)
{
   Instance *x = convertTo(X,TO_FLOAT,vm);
   return floatCreate(FLOATV(self) / FLOATV(x),vm);
}

static Instance *Float_mod(Instance *self,Instance *X,pVM vm)
{
   Instance *y = convertTo(X,TO_FLOAT,vm);
   return floatCreate(fmod(FLOATV(self),FLOATV(y)),vm);
}

static Instance *Float_negate(Instance *self,Instance *X,pVM vm)
{
   #ifdef _MSC_VER
   	// how or why this is different from -x I don't know
      return floatCreate(_chgsign(FLOATV(self)),vm);
   #endif
   return floatCreate(-FLOATV(self),vm);
}

static const OpcodeTable floatOpcodes[] = 
{
   OP_EQ,	(pOp)Float_eq,
   OP_NEQ,	(pOp)Float_neq,

   OP_LT,	(pOp)Float_lt,
   OP_LTE,	(pOp)Float_lte,
   OP_GT,	(pOp)Float_gt,
   OP_GTE,	(pOp)Float_gte,

   OP_ADD,	(pOp)Float_add,
   OP_SUB,	(pOp)Float_sub,
   OP_MUL,	(pOp)Float_mul,
   OP_DIV,	(pOp)Float_div,
   OP_MOD,	(pOp)Float_mod,

   OP_NEGATE,	(pOp)Float_negate,

   0,		0
};



//static pMethod in_int_methods(  Instance *ignore, register char *str);
//static pMethod in_float_methods(Instance *ignore, register char *str);
//static pProperty in_int_properties(Instance *ignore, register char *str);

void numberConstruct(void)
{
   static IBucketHeader _nBuckets;

   #if USE_POINTER_INTS
      if (PtrInt_NUM_BYTES > sizeof(Instance*)) 
         vmHalt("PtrInt bigger than void*, fix zklNumber.h and recompile");
   #endif

   constructObject(&IntObject, IntType, intMethods, intProperties,
		   intOpcodes, NoVM);
//   IntObject.methodSearch   = in_int_methods;
//   IntObject.propertySearch = in_int_properties;
   IntObject.isize	    = sizeof(ZKL_Int);
   IntObject.threadSafe     = 1;
   IntObject.isBInstance    = 1;

   constructObject(&FloatObject, FloatType, floatMethods, floatProperties,
		   floatOpcodes, NoVM);
//   FloatObject.methodSearch = in_float_methods;
   FloatObject.isize	    = sizeof(ZKL_Float);
   FloatObject.threadSafe   = 1;
   FloatObject.isBInstance  = 1;

   nBuckets = ibucketHitchHike(&FloatObject,0,5002,&_nBuckets,NoVM);
   ibucketPoach(nBuckets,&IntObject,NoVM);

		// Create immortal constants, which are UNTOUCHABLE
   _Zero.value	   =   0; instanceInit(Zero,	 &IntObject,  I_UNTOUCHABLE);
   _One.value	   =   1; instanceInit(One,	 &IntObject,  I_UNTOUCHABLE);
   _MinusOne.value =  -1; instanceInit(MinusOne, &IntObject,  I_UNTOUCHABLE);
   _FZero.value	   = 0.0; instanceInit(FZero,	 &FloatObject,I_UNTOUCHABLE);
   _FOne.value	   = 1.0; instanceInit(FOne,	 &FloatObject,I_UNTOUCHABLE);
   _pi.value	   = PI;  instanceInit(pi,	 &FloatObject,I_UNTOUCHABLE);
   _e.value	   = E;   instanceInit(e,	 &FloatObject,I_UNTOUCHABLE);

   vaultAdd("", Zero,NoVM);	// add Int   to Vault
   vaultAdd("",FZero,NoVM);	// add Float to Vault

   spinLockInit(&rlock);

   setIEEE754ness();
#if 0
   a_nan = strtod("NaN", NULL);
   a_inf = strtod("Inf", NULL);
#endif

   #if __unix__	// seed the random number generator
   {
      // /dev/urandom is the preferred source of cryptographic randomness 
      // on UNIX-like systems.  http://www.2uo.de/myths-about-urandom/
      // Also: Linux: getrandom(), BSD: getentropy()

      int randomData = open("/dev/urandom", O_RDONLY);
      unsigned n;
      if (randomData == -1) n = time(0);
      else
      {
	 (void)read(randomData, &n, sizeof(n));	// oh, shut up GCC
	 close(randomData);
      }
      prng_xor128(n%1000);
   }
   #else
	// UuidCreate(&uuid) | time?
      prng_xor128(time(0)%1000);
   #endif

#if 0
   {  // man 7 locale
      struct lconv *numFormats = localeconv();
      printf("-->%s&%s<\n",	// ??? "." & ""?
	numFormats->decimal_point, numFormats->thousands_sep);
      also mon_* for money formats
   }
#endif
}



//////////////////////////////////////////////////////////////////
// zkl extractTable -n intMethods   < number.c | gperf | zkl gperf -i int
// zkl extractTable -n floatMethods < number.c | gperf | zkl gperf -i float



/////////////////////////////////////////////////////////////////
///////////////// Int Properties ////////////////////////////////
// zkl extractTable.zkl -p < number.c | gperf | zkl gperf.zkl -i int




//////////////////////////////////////////////////////////////////
///////////////// Float Methods /////////////////////////////////
// zkl extractTable -n floatMethods < number.c | gperf | zkl gperf -i float


