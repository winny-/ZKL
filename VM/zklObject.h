/* zklObject.h:  The main header file for the zkl Programming Language
 *     Virtural Machine.
 *
 * Copyright (c) 2006,2007,2008-13,2014 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#ifndef __ZKL_OBJECT_H
#define __ZKL_OBJECT_H

#define LANGUAGE_NAME	"zkl"

#define VERSION_MAJOR	  1
#define VERSION_MINOR	 14
#define VERSION_MYSTERY	  7	// release #, patch level or something
#define RELEASE_DATE	"2020-02-02"

#define ZKLX_PROTOCOL	010012	// shared library protocol: vv.MM.mm

#if _WIN32_WINNT < 0x0400
//   #define _WIN32_WINNT 0x0400 	// 0x0400 to get SwitchToThread (winbase.h)
   #define _WIN32_WINNT 0x0403 	// 0x0400 to get SwitchToThread (winbase.h)
#endif

#ifdef _MSC_VER
   #include <Windows.h>
    /* Note:  I'm REALLY annoyed that I have to include windows.h here but
     * if I try to include it in zklAtomic.h (where it should be) VC7 throws
     * a huge hissy fit.
     */
#endif

    /* In general, this file is a mess, mostly because the structs are
     * recursive and everything depends on everything else so everybody and
     * their dog is included here.  At least the compilers are pretty quick
     * although windows.h really slows things down.
     */

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 * WARNING! Because of the recursive nature of many of these types, you
 * can't always rely on the C type system to catch function prototype
 * mismatches.  Many times this is a good thing but be careful of the var
 * args (...) functions, you might forget to include the vm arg.
 * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 */

#include <stdlib.h>
// more includes below
#include <stdint.h>		// C99

#ifndef BYTE_DEFINED	// For those cases where there is a GCC conflict
   typedef uint8_t	Byte;
#endif
typedef unsigned char	UChar;


    //!!! replace this with static_assert() at some point
#define ASSERT_SIZE(type,size)	\
   typedef char type##_not_##size##_bytes[2*((sizeof(type) == size)!=0)-1]

ASSERT_SIZE(Byte,    1);
ASSERT_SIZE(uint8_t, 1);
ASSERT_SIZE(int16_t, 2);
ASSERT_SIZE(int32_t, 4);
ASSERT_SIZE(uint32_t,4);
ASSERT_SIZE(int64_t, 8);
ASSERT_SIZE(uint64_t,8);
#undef ASSERT_SIZE


    /* Offset_t needs to be one bit bigger than the biggest container size
     *   (which I say is 32 unsigned bits, eg Data) because an offset can be
     *   negative ("123"[-1]).
     * If the Offset_t is the same size as max container size, I can do some
     *   overflow testing, etc but you will loose one bit (half whatever
     *   size type the container size is) but that is unlikely to be a problem.
     */
typedef int64_t Offset_t;	// or int32_t
#define OFFSET_T_IS_BIG	    1	// bigger than 32 bits (int32_t)

/* ******************************************************************** */
/* ****************** Dynamic (Instance) Object Data ****************** */
/* ******************************************************************** */

typedef struct Instance
{
   unsigned int	    objID:8;		// index into zklObjectTable[]
   unsigned int	    itype:4, gcMark:1;	// gc flags
   unsigned int	    iflag:1, iflag2:1;	// user flags. Set once!
   struct Instance *nexti;		// Instance chain
}Instance;	// 8 bytes, should be 6

typedef struct   // Bucket Instance, an Instance without nexti
{
   unsigned int	objID:8;	    // index into zklObjectTable[]
   unsigned int itype:4, gcMark:1;  // gc flags
   unsigned int	iflag:1, iflag2:1;  // user flags.  BEFORE GC sees!
   unsigned int	bpflag:1;	    // bucket partition flag
   unsigned int	nextb:16;	    // bucket chain
}BInstance;	// 4 bytes


typedef Instance *pArglist;	// actually ZKL_Tuple *, sometimes List *

    // actually VM *, give the compiler somethig to typecheck
struct FakeVM { int bogus; };
typedef struct FakeVM *pVM;

#define ZNIL	( (Instance *)0 )  // because sizeof(pointer) != sizeof(int)
#define NoVM	( (pVM)0 )

/* ******************************************************************** */
/* ************************ Static Object Data ************************ */
/* ******************************************************************** */

typedef Instance* (*pMethod)(Instance *self,pArglist,pVM vm);
typedef struct
{
   char	   *name;
   pMethod  method;
} MethodTable;	// 8 bytes


typedef Instance* (*pProperty)(Instance *self,pVM vm);
typedef struct
{
   char	     *name;
   pProperty  property;
}PropertyTable;


typedef Instance* (*pOp)(Instance *self,Instance *X,pVM vm);
typedef struct
{
   unsigned offset;	// OP_?, see below
   pOp	    op;	
}OpcodeTable;

    // Types so I can cast and shut up GCC
typedef int       (*_FreeMe)(Instance *);
typedef void      (*_MagicMarker)(Instance *);
typedef pMethod   (*_MethodSearch)(Instance *,char *);
typedef pProperty (*_PropertySearch)(Instance *,char *);
typedef int       (*_Resolve)(Instance **self,
	char *name, pMethod *, void *pFcn,int bitch, pVM);
typedef int       (*_ResolveN)(Instance **self, 
	unsigned id, pMethod *, void *pFcn, int bitch, pVM);


    // An Object is the "base" of all instances and are usually static
    // and created at start up. In the base system, there are less than 50
    // of these.
    // There is some cruft in here, in the interest of backward compatibility 
    // with DLLs (eg id could be Byte). Not enough wasted space to care (yet).
    // !!Change ZKLX_PROTOCOL if modify this structure so DLLs will know
typedef struct
{
   char	       *name;		// defaults to type name
   char	       *vaultPath;	// defaults to ""
   unsigned int	id:8;		// index into objectTable, same as in Instance
   unsigned int	otype:8;	// one of the types in zklOpcodes.h
   unsigned int	isize:16;	// size of an Instance
   unsigned int	minGN:16, maxGN:16;  // of global names table (globalNameIdxs)
   unsigned int threadSafe:1, isBInstance:1;
   unsigned int createReturnsSelf:1; // eg List() --> List
	// mystery flags: flags I might want to use at some point
   unsigned int mf1:1,mf2:1,mf3:1,mf4:1,mf5:1,mf6:1,mf7:1;

   _FreeMe	freeMe;
   _MagicMarker magicMarker;
   _Resolve	resolve;
   _ResolveN	resolveN;

   pMethod mcache[7];     // Some cached methods, for quick reference from C

	// The Op table
   pOp	eq,neq, lt,lte, gt,gte, negate, add,sub,mul,div,mod;

   const MethodTable   *methodTable;
   const PropertyTable *propertyTable;

   int (*toSink)(Instance *self,void *ZAgg,
		 size_t, size_t sbSz,size_t *sbUsed, pVM);
   int (*__A)(int);	// future proofing, just a fcn pointer
   
   Byte *globalNameIds;	// byte indexes into global name table
}ZKL_Object;	// Linux/64: 280

    /* Indexes into object->mcache so I can build the method cache from a
     * Method table.  Note that these methods might not be in
     * object->methodTable.
     */
#define M_CREATE		 0
#define TO_STRING		 1
#define TO_BOOL			 2
#define TO_INT			 3
#define TO_FLOAT		 4
#define TO_DATA			 5
#define TO_LIST			 6
#define MCACHE_MAX		 7	// 1 + last cached method index

#define OP_EQ			 0
#define OP_NEQ			 1
#define OP_LT			 2
#define OP_LTE			 3
#define OP_GT			 4
#define OP_GTE			 5
#define OP_NEGATE		 6
#define OP_ADD			 7
#define OP_SUB			 8
#define OP_MUL			 9
#define OP_DIV			10
#define OP_MOD			11
#define OPCODE_MAX		12	// 1 + last pOp

    // OBJECTI, AS_I, TYPEO, TYPEOI are in zklNumber.h
    // Warning! If using OBJECT1, result is doo-doo for PtrInts
#define OBJECT1(i)		zklObjectTable[((Instance *)i)->objID]

    // Not safe for PtrInts
#define ONAME1(i)		( OBJECT1(i)->name )
#define TYPEO1(i)		( OBJECT1(i)->otype )
#define FREE_ME(i)		( OBJECT1(i)->freeMe )
#define MAGIC_MARKER(i)		( OBJECT1(i)->magicMarker )
#define IS_CONTAINER(i)		( MAGIC_MARKER(i) != 0 )
#define METHOD_SEARCH(i)	( OBJECT1(i)->methodSearch )
#define PROPERTY_SEARCH(i)	( OBJECT1(i)->propertySearch )
#define IRESOLVE(i)		( OBJECT1(i)->resolve )
#define IRESOLVEN(i)		( OBJECT1(i)->resolveN )

#define O_METHOD(object,offset)	( (object)->mcache[offset] )
#define I_METHOD(i,TO_)		( O_METHOD(OBJECT1(i),TO_) )

#define M2_STRING(i,vm)		( I_METHOD(i,TO_STRING)(i,NoArglist,vm) )
#define M2_BOOL(  i,vm)		( I_METHOD(i,TO_BOOL)  (i,NoArglist,vm) )
#define M2_INT(   i,vm)		( I_METHOD(i,TO_INT)   (i,NoArglist,vm) )
#define M2_FLOAT( i,vm)		( I_METHOD(i,TO_FLOAT) (i,NoArglist,vm) )
#define M2_DATA(  i,vm)		( I_METHOD(i,TO_DATA)  (i,NoArglist,vm) )

#define I_FLAG(i)		( ((Instance *)i)->iflag )
#define I_FLAG2(i)		( ((Instance *)i)->iflag2 )
#define I_OBJ_ID(i)		( ((Instance *)i)->objID )

#define O_OP(object,offset)	( (&(object)->eq)[offset] )
#define I_OP(i,offset)		( O_OP(OBJECT1(i),offset) )

#define I_ID(i)			( (size_t)i )

#define I_IS_IN_MEM		( (_FreeMe)2 )
#define IS_IMEM(i)		( FREE_ME(i) == I_IS_IN_MEM )

#define GC_DONT_FREE_ME		( (_FreeMe)3 )


    // Safe for PtrInts
#define I_IS_CONTAINER(i)	( OBJECTI(i)->magicMarker != 0 )
#define I_IS_THREAD_SAFE(i)	( OBJECTI(i)->threadSafe )
#define ONAME(i)		( OBJECTI(i)->name )
#define VAULT_PATH(i)		( OBJECTI(i)->vaultPath )
#define METHOD_TABLE(i)		( OBJECTI(i)->methodTable )
#define PROPERTY_TABLE(i)	( OBJECTI(i)->propertyTable )
#define ISIZE(i)		( OBJECTI(i)->isize )


/* ******************************************************************** */
/* **************************** Prototypes **************************** */
/* ******************************************************************** */

#ifdef _MSC_VER
   #define DllExport	__declspec( dllexport )
   #define DllImport	__declspec( dllimport )
#elif defined(__GNUC__)
   #define DllExport	__attribute__ ((visibility("default")))
   #define DllImport
#else
   #define DllExport
   #define DllImport
#endif

	///////////////////////////////////////////////  bool.c
#define BOOLV(i)  (i == BoolTrue)

#ifdef __NOT_A_DLL
extern DllExport Instance *BoolFalse, *BoolTrue;

void boolConstruct(void);
#endif	// __NOT_A_DLL


#ifdef __cplusplus
extern "C" {
#endif

DllExport Instance *boolCreate(int);
DllExport Instance *boolNot(Instance *);
DllExport Instance *Bool_soTrue(Instance *,pArglist,pVM);
DllExport Instance *Bool_nope(  Instance *,pArglist,pVM);


	////////////////////////////////////////////////////  loader.c
DllExport Instance *readRootClass(char *fileName, char *path, pArglist, pVM);
DllExport Instance *loadWad(Byte *ptr,Byte **zero,pArglist,pVM);

#ifdef __cplusplus
};		// end of extern "C"
#endif


////////////////////////////// Other include files //////////////
#include "zklArglist.h"		// everybody
#include "zklMemory.h"		// instanceAllocate, GC_SANITY_CHECK, etc
#include "zklOpcodes.h"		// StringType, etc
#include "zklNumber.h"		// OBJECTI, TYPEO
/////////////////////////////////////////////////////////////////



	/////////////////////////////////////////////////////  object.c
#ifdef __cplusplus
extern "C" {
#endif

#ifdef __NOT_A_DLL
extern DllExport ZKL_Object *zklObjectTable[];

extern const MethodTable   objectMethods[];
extern const PropertyTable objectProperties[];

void	  objectConstruct(void);
Instance *Object_Method(Instance *,pArglist,pVM);
Instance *Object_toList(Instance *,pArglist,pVM);
#endif	// __NOT_A_DLL

DllExport ZKL_Object *constructObject(ZKL_Object *,int type,
	const MethodTable *, const PropertyTable *,const OpcodeTable *, pVM);
DllExport void registerObject(ZKL_Object *,pVM);
DllExport void objectStack(ZKL_Object *dst,
			   const OpcodeTable *, ZKL_Object *src, ...);
DllExport int       objectResolve(Instance **,char *name, pMethod *, void *pfcn, int throw, pVM);
DllExport int	    objectResolveN(Instance **,unsigned id, pMethod *, void *pFcn, int bitch, pVM);
DllExport Instance *Object_resolveName(Instance *,pArglist,pVM);
DllExport Instance *iResolve(Instance *,char *name, pMethod *method, pVM);
DllExport Instance *iResolveAgainst(Instance *,ZKL_Object *,
		char *,pMethod *,int justCheckSelf,pVM);
DllExport pMethod   searchForMethod(Instance *,   char *name, int searchObject,pVM);
DllExport pProperty searchForProperty(Instance *, char *name, int searchObject,pVM);
DllExport Instance *objectRun(Instance *fcn,pArglist,Instance **result,pVM);
DllExport char	   *iname(Instance *);
DllExport int	    objectIsType(Instance *self,Instance *other);
DllExport Instance *objectIsIT(Instance *,pArglist,pVM);
DllExport Instance *iToString(Instance *,pVM);
DllExport Instance *Object_noop(Instance *,pArglist,pVM);
DllExport Instance *Object_defer(Instance *,pArglist,pVM);
DllExport Instance *Object_dir(Instance *,pArglist,pVM);

DllExport Instance *Object_eq(	  Instance *,Instance *X,pVM);
DllExport Instance *Object_neq(	  Instance *,Instance *X,pVM);
DllExport Instance *Object_lt(	  Instance *,Instance *X,pVM);
DllExport Instance *Object_lte(	  Instance *,Instance *X,pVM);
DllExport Instance *Object_gt(	  Instance *,Instance *X,pVM);
DllExport Instance *Object_gte(	  Instance *,Instance *X,pVM);
DllExport Instance *Object_negate(Instance *,Instance *X,pVM);
DllExport Instance *Object_add(	  Instance *,Instance *X,pVM);
DllExport Instance *Object_sub(	  Instance *,Instance *X,pVM);
DllExport Instance *Object_mul(	  Instance *,Instance *X,pVM);
DllExport Instance *Object_div(	  Instance *,Instance *X,pVM);
DllExport Instance *Object_mod(	  Instance *,Instance *X,pVM);

DllExport Instance *notImplementedError(Instance *, char *whatNot, pVM);
DllExport Instance *cantConvertError(   Instance *, char *to, pVM);

DllExport char     *getGlobalName(unsigned id,pVM);
DllExport int 	    addNametoGlobalTable(char *,pVM);
DllExport int	    addStaticNametoGlobalTable(char *name,pVM);
DllExport int	    getGlobalId(char *name, unsigned *id, pVM);
DllExport int	    isIdMP(Instance *, unsigned id);

#ifdef __cplusplus
};		// end of extern "C"
#endif



	////////////////////////////////////////////////////  void.c
#ifdef __cplusplus
extern "C" {
#endif

#ifdef __NOT_A_DLL
extern DllExport Instance *Void;

extern Instance *VoidPlus,
   *VoidVoid, *VoidStop, *VoidSkip, *VoidWrite, 
   *VoidRead, *VoidDrop, *VoidAgain, *VoidFilter, *VoidXplode, *VoidRecurse;

void   voidConstruct(void);
#endif	// __NOT_A_DLL


DllExport Instance *voidCreate(void);
DllExport int	    voidByType(pArglist,int a0,char *msg,pVM);

#ifdef __cplusplus
};		// end of extern "C"
#endif


	////////////////////////////////////////////////////  vault.c
#ifdef __cplusplus
extern "C" {
#endif

#ifdef __NOT_A_DLL
void	  vaultConstruct(void);
Instance *vcGet(int,pVM);
#endif	// __NOT_A_DLL

DllExport void	    vaultAdd(char *vaultPath, Instance *classOrNative, pVM);
DllExport void	    vaultAddData(char *vaultPath, Instance *data, pVM);
DllExport Instance *vaultFind(char *vaultPath, int bitch, pVM);
DllExport Instance *vaultBackTrace(Instance *klass,pVM);
DllExport Instance *vaultChase( char *vaultPath, pVM);
DllExport Instance *vaultChase2(char *vaultPath,char *path, pVM);

#ifdef __cplusplus
};		// end of extern "C"
#endif


	////////////////////////////////////////////////////  vm.c
    // Convenience define: if you want fcn internals, you need pc internals
#if defined(__FCN_INTERNALS) && !defined(__PC_INTERNALS)
   #define __PC_INTERNALS
#endif

#ifdef __PC_INTERNALS
   #include "zklPCBlock.h"		// Block (vm.c)
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __NOT_A_DLL

#define VM_DONE			0	// == didn't longjmp()
#define VM_ERROR		1
#define VM_HALT			2
#define VM_STOP			3
#define VM_FOUND_CATCH		5
#define VM_OK		       10	// used by threads
#define VM_YIELD	       20	// fibers

extern Instance *argV, *classPath, *libPath, *includePath, *Utils_Helpers;

#ifdef __PC_INTERNALS
   BlockStack  *vmBlockStack(pVM);
   ZKL_Block   *vmBlockCreate(pVM);
   ZKL_PC      *vmPC(pVM);
#endif

int		vmIsFiber(pVM);
int		vmIsThread(pVM);
void		vmIsAThread(pVM, void *);
void		cacheExceptions(void);
Instance       *vmRegX(pVM);
Instance       *vmRun(pVM, int *status);
Instance       *vmArglist2(pVM);
void		vmResetMarks(void);
void		markVMs(void);
Instance       *VM_yield(Instance *,pArglist,pVM);
int		voidPlusOK(pVM);
Instance       *vm2Instance(pVM);
pVM		vmRoot(pVM);
int		vmIsRunning(pVM);
void		vmSetX(pVM,Instance *);

#endif	// __NOT_A_DLL

#ifdef __PC_INTERNALS
   #define VM_MOTHERLESS_CHILD	1	// make this a root VM
   #define VM_THREAD2BE		2	// threads are roots too
   DllExport pVM    vmCreate(pVM callingVM,ZKL_Block **,int flags,pVM);
#endif

DllExport void	    zklConstruct(int argc, char* argv[]);
DllExport void	    vmFree(pVM);
DllExport Instance *vmRunFcn(pVM, Instance *fcn, pArglist, int *status);
DllExport void	    vmPanic(pVM, char *msg);
DllExport void	    vmHalt(char *);
DllExport void	    vmThrow(pVM, char *excpetionName, char *text);
DllExport void	    vmThrowE(pVM, Instance *eClass);
DllExport void	    vmThrowTheEnd(pVM);
DllExport unsigned  vmID(pVM);
DllExport int	    vmMightStall(pVM,int mightStall);
DllExport pVM	    vmLeaf(pVM);
DllExport int	    resultToBool(Instance *,pVM);
DllExport void	    runFcnInSandBox(Instance *,pArglist,pVM);
DllExport int	   *vmInterruptEvent(pVM);
DllExport Instance *vmCallFcnFromMethod(Instance *f,pArglist,int zero,pVM);

typedef struct Fence	// Hook into exceptions, see vm.c for what and how
{
   struct Fence *prev, *next;
   void	      (*fcn)(struct Fence *, Instance *e);
   pVM		vm;
   Instance    *i,*i1,*i2,*i3;	// for GC
   int		mrc;	// for check against vm->mrc
} Fence;

DllExport void fenceGarbageSelf(Fence *, Instance *);
DllExport void vmSetFence(pVM, Fence *, void (*fcn)(Fence *,Instance *), Instance *);
DllExport void vmRemoveFence(Fence *, int runit);

#ifdef __cplusplus
};		// end of extern "C"
#endif


	////////////////////////////////////////////////////  misc

#ifdef __NOT_A_DLL
		// thread.c
Instance *threadCreate(Instance *klass,Instance *fcn, pArglist, int flags, pVM);
int	  numThreads(int *max);
#endif	//__NOT_A_DLL

DllExport Instance *Console_print(  Instance *,pArglist,pVM);	// miscObj.c
DllExport Instance *Console_println(Instance *,pArglist,pVM);	// miscObj.c

DllExport Instance *pipeCreate(pVM);
DllExport void	    pipeBreakIt(Instance *,Instance *pe);
DllExport Instance *Pipe_write(Instance *,pArglist,pVM);
DllExport Instance *strawCreate(pVM);
DllExport Instance *strawSet(Instance *, Instance *i, int close);
DllExport Instance *Pipe_close(Instance *,pArglist, pVM);



	// Exception names
#define E_NAME_ERROR		"NameError"
#define E_COMPILER_ERROR	"CompilerError"
#define E_NOT_IMPLEMENTED	"NotImplementedError"
#define E_VM_ERROR		"VMError"
#define E_LOADER_ERROR		"LoaderError"
#define E_TYPE_ERROR		"TypeError"
#define E_VALUE_ERROR		"ValueError"
#define E_INDEX_ERROR		"IndexError"
#define E_NOT_FOUND		"NotFoundError"
#define E_OUT_OF_MEMORY		"OutOfMemory"
#define E_THE_END		"TheEnd"
#define E_IO_ERROR		"IOError"
#define E_MATH_ERROR		"MathError"
#define E_ASSERTION_ERROR	"AssertionError"
#define E_ACCESS_ERROR		"AccessError"
#define E_MISSING_ARG		"MissingArg"
#define E_TIME_OUT		"Timeout"

#endif /* __ZKL_OBJECT_H */
