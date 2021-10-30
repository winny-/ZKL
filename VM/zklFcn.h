/* zklFcn.h : header file for Fcns
 * 
 * Copyright (c) 2006,2007,2008-12,2013 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#ifndef __ZKL_FCN_H
#define __ZKL_FCN_H

// #include "zklObject.h" implied here

#include "zklPCBlock.h"	// need __PC_INTERNALS, zklObject.h sucks this in
#include "zklUtil.h"	// StringTable

#ifdef __FCN_INTERNALS
    // Warning: I have no problem changing internals at any time

typedef struct  // there is one of these per set of "identical" fcns
{
   Instance  instance;		// Native
   unsigned  size:16;		// data collection
   unsigned  nameo:16;		// points into the middle of defaultArgs
   unsigned  numArgs:8, numDefaults:8, d1:8;  // d1 is first default arg
   unsigned  isPrivate:1;	// 1 if Class.resolve can't see this
   unsigned  isStatic:1;	// 1 if fcn doesn't reference container/vars
   unsigned  codeIsHere:1;	// 1 if code stored at end of this struct
   ZKL_Code  code;  // SHARED by all copies of this Fcn, ptrs into end of struct
   ZKL_Code  defaultArgs[0];	// Expands. Names are also in here
      // ZKL_Code    defaultArgs[numDefaults]: Zero'd if no default
      // ZKL_KString kstrings[numKStrings]:  // for code and default args
      // char name[strlen(name) + 1];
      // char prototype[strlen(each prototype name) + protoLen];
      // if not in a wad: code bits for code and default args:
      //    fcn code bits, strings, maps
      //    default arg bits, Void defaults aren't here
}FcnBase; // fcn f{} == 40 bytes (84/64), nullFcn == 48 (80 Linux/64)

typedef struct
{
   BInstance  instance;
   FcnBase   *fcnBase;		// the shared data
   Instance  *container;	// The class instance fcn is bound to
} ZKL_Fcn;		// 12 bytes, 24 on Linux/64

#define FCN_BASE(f)	    ( ((ZKL_Fcn *)f)->fcnBase )

#define FCN_NAME(f)	    (   (char *)FCN_BASE(f) + FCN_BASE(f)->nameo )
#define FCN_NUM_VARS(f)	    (   FCN_BASE(f)->numVars )
#define FCN_NUM_ARGS(f)	    (   FCN_BASE(f)->numArgs )
#define FCN_DEFAULT_ARGS(f) (   FCN_BASE(f)->defaultArgs )
#define FCN_NUM_DEFAULTS(f) (   FCN_BASE(f)->numDefaults )
#define FCN_IS_PRIVATE(f)   (   FCN_BASE(f)->isPrivate )
#define FCN_IS_STATIC(f)    (   FCN_BASE(f)->isStatic )
#define FCN_CODE(f)	    (  &FCN_BASE(f)->code )

#define FCN_IS_RUNNABLE(f)  (   (f)->iflag )
#define FCN_CONTAINER(f)    ( ((ZKL_Fcn *)f)->container )

#endif // __FCN_INTERNALS

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __NOT_A_DLL
extern DllExport Instance *nullFcn;	// created AFTER NullClass

void 	  fcnConstruct(void);
int	  finalizeArglist2(Instance *self,
			Instance *arglist, int numParams, pVM);
Instance *fcnEmbryoWithStaticCode(
		StringTable *names, int numArgs, int numDefaults,
		ZKL_Code *code, ZKL_Code *defaults, int itype, pVM vm);
void	  fcnPrepForRun(Instance *, ZKL_Block *block, pVM);

#endif // __NOT_A_DLL

DllExport char	   *fcnName(Instance *);
DllExport Instance *fcnCopy(Instance *,Instance *container,pVM);
DllExport int	    fcnIsInstanceOf(Instance *, Instance *fcn);
DllExport Instance *fcnRun(Instance *, pArglist,
				Instance **result, pVM callingVM);
DllExport Instance *rogueFcnRun(Instance *,pArglist,Instance **result);
DllExport Instance *fcnRunith(char *vaultClassPath,char *fcnName,pArglist,pVM);
DllExport Instance *fcnRunFromClass(Instance *class,char *fcnName,pArglist,pVM );
DllExport Instance *fcnRunFromClassId(Instance *class,unsigned fid,pArglist,pVM);
DllExport Instance *Fcn_container(Instance *,pVM);
DllExport Instance *Fcn_loop(Instance *,pArglist,pVM);
DllExport Instance *Fcn_reduce(Instance *,pArglist,pVM);

#ifdef __cplusplus
};		// end of extern "C"
#endif

#endif // __ZKL_FCN_H
