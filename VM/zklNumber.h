/* number.h : header file for Numbers
 * 
 * Copyright (c) 2006,7,8,9,10,2011 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#ifndef __ZKL_NUMBER_H
#define __ZKL_NUMBER_H

// #include "zklObject.h" implied here

    // Vital! A pointer int CAN NOT be 0
#define USE_POINTER_INTS 1	// 1 to pack integers into a pointer

typedef struct
{
   BInstance instance;
   int64_t   value;
} ZKL_Int;	// 16 bytes

typedef struct
{
   BInstance instance;
   double    value;
} ZKL_Float;	// 16 bytes

#define INT64V(i)	( ((ZKL_Int *)i)->value )
#define FLOATV(i)	( ((ZKL_Float *)i)->value )



#ifdef __cplusplus
extern "C" {
#endif

#ifdef __NOT_A_DLL

extern DllExport Instance  *Zero, *One, *MinusOne, *FZero, *FOne;
extern DllExport ZKL_Int    _Zero;	// used by PtrInt_TO_Int64
extern DllExport ZKL_Object IntObject;

void	  numberConstruct(void);
Instance *Int_toString(Instance *,pArglist,pVM);
Instance *ptrIntDoMethod(Instance *,unsigned,pMethod,pArglist, pVM);

#endif // __NOT_A_DLL

Instance *intAllocate(int64_t, pVM);	// for ptrIntThunk


DllExport Instance *intCreate( int64_t, pVM);
DllExport Instance *floatCreate(double, pVM);
DllExport Instance *Int_create(Instance *,pArglist,pVM);
DllExport char	   *intToA(int64_t, char *);
DllExport int64_t   convertToInt(Instance *x, pVM vm);
DllExport int	    iWantInt(  Instance *, int64_t *, pVM);
DllExport int	    iWantFloat(Instance *, double  *, pVM);
DllExport double    fromIEEE754(void *);

DllExport char	   *i64ToBaseB(int64_t i64,char *buf,int base, pVM);
DllExport char     *floatToString(Instance *,char *,int precision,char feg,char);

DllExport Instance *Int_eq(  Instance *,Instance *X,pVM);
DllExport Instance *Float_eq(Instance *,Instance *X,pVM);

DllExport unsigned int intLog2(uint64_t n);

    /* Pointer ints, 31 or 63 bits.  Assumes Instances are allocated on even
     *    boundaries, so if a pointer is odd, it is a Int31.  Since this is
     *    a "bastard" type, and the VM & type system really don't anything
     *    about it, it must morph into a Int before it is sent to a method,
     *    etc 
     * Same size as pointer.
     * Other word sizes would work but I don't see those machines. And
     *   compiling on a 16 bit machine doesn't make much sense.
     */
#if USE_POINTER_INTS	// only tested on Intel/AMD 32/64 Linux/FreeBSD/Windows

#if __unix__	// clang/GCC
   #if __x86_64__ || __ppc64__
      #define PTRSZ_IS_64 1
   #else
      #define PTRSZ_IS_32 1
   #endif
#else
   #define PTRSZ_IS_32 1	// a "lowest" common denominator
   #define PTRSZ_IS_64 0
#endif

    // Note: on Intel CPUS, char * can point to odd addresses
#if PTRSZ_IS_32
   typedef int32_t PtrInt;		// 31 bit int. Bit 0 is 1
   #define INT_FITS_IN_PtrInt(n) \
      ( -((int64_t)1 << 30) <= n && n < (int64_t)1 << 30 )
   #define PtrInt_NUM_BYTES 4
#elif PTRSZ_IS_64
   typedef int64_t PtrInt;		// 63 bit int. Bit 0 is 1
   #define INT_FITS_IN_PtrInt(n) \
      ( -((int64_t)1 << 62) <= n && n < (int64_t)1 << 62 )
   #define PtrInt_NUM_BYTES 8
#endif


#define IS_PtrInt(ptr)	    ( (unsigned)((size_t)(ptr) & 1) )
//#define PtrInt_TO_N(ptrInt) (PtrInt)((int32_t)(ptrInt) >> 1)
#define PtrInt_TO_N(ptrInt) (PtrInt)((PtrInt)(ptrInt) >> 1)
#define PtrInt_TO_Int64(ptrInt,i)	\
	( *(ZKL_Int *)(i) = _Zero, ((ZKL_Int *)i)->value = PtrInt_TO_N(ptrInt) )
#define INT_TO_INSTANCE(n) (Instance *)(((size_t)(n) << 1) | 1)

#define INT_CREATE(n,vm) INT_TO_INSTANCE(n)	// if you KNOW n fits

#define PTR_INT_ZERO INT_TO_INSTANCE(0)		// 1

#define OBJECTI(i)   ( IS_PtrInt(i) ? &IntObject : OBJECT1(i) )
#define TYPEO(i)     ( IS_PtrInt(i) ? IntType    : TYPEO1(i) )
#define TYPEOI(i)    ( IS_PtrInt(i) ? PtrIntType : TYPEO1(i) )
#define AS_I(i)      ( IS_PtrInt(i) ? Zero : (Instance *)i )
#define AI_OBJ_ID(i) ( AS_I(i)->objID )		// PtrInt safe I_OBJ_ID

#define IS_ZERO(i)	( i==Zero || (size_t)i == 1 )
#define IS_ONE(i)	( i==One  || (size_t)i == 3 )

#define INT_VAL(i)	( IS_PtrInt(i) ? PtrInt_TO_N(i) : INT64V(i) )

DllExport Instance *decantInt(Instance *, ZKL_Int *);

Instance *PtrInt_div(Instance *ptrInt,Instance *X,pVM);
Instance *PtrInt_mod(Instance *ptrInt,Instance *X,pVM);

Instance *PtrInt_eq( Instance *ptrInt,Instance *X,pVM);
Instance *PtrInt_neq(Instance *ptrInt,Instance *X,pVM);

    // PtrInt --> Int, anything else is unchanged, 0 --> 0
#define ptrIntThunk(n,vm)	\
   ( (IS_PtrInt(n)) ? intAllocate(PtrInt_TO_N(n),vm) : n )

#else	// don't USE_POINTER_INTS

#define IS_PtrInt(ptr)	0
#define OBJECTI(i)	OBJECT1(i)
#define TYPEO(i)	TYPEO1(i)
#define TYPEOI(i)	TYPEO1(i)
#define AS_I(i)		( (Instance *)i )

#define INT_CREATE(n,vm) intCreate(n,vm)

#define IS_ZERO(i)	( (i)==Zero )
#define IS_ONE(i)	( (i)==One  )

#define INT_VAL(i)	( INT64V(i) )

#define ptrIntThunk(n,vm)	(n)

#endif	// USE_POINTER_INTS

#ifdef __cplusplus
};		// end of extern "C"
#endif

#endif // __ZKL_NUMBER_H
