/* zklImport.h : Global variables for DLLs
 * 
 * Copyright (c) 2006,7,8,9,10,2011 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#ifndef __ZKL_IMPORTS_H
#define __ZKL_IMPORTS_H

#ifndef __NOT_A_DLL	// only DLLs need this stuff

// #include "zklObject.h" implied here

#ifdef __cplusplus
extern "C" {
#endif

extern DllImport ZKL_Object *zklObjectTable[];
extern DllImport ZKL_Object  IntObject;

extern DllImport Instance *BoolFalse, *BoolTrue;
extern DllImport Instance *Void, *VoidPlus,
			  *VoidVoid, *VoidStop, *VoidSkip, *VoidWrite;

extern DllImport Instance *emptyString, *Star;
extern DllImport Instance *emptyList, *emptyTuple, *NoArglist;

extern DllImport Instance  *Zero, *One, *MinusOne, *FZero, *FOne;
extern DllImport ZKL_Int    _Zero;	// used by PtrInt_TO_Int64

extern DllImport Instance *NullClass, *nullFcn;

extern DllImport Instance *emptyKData, *VData;

#ifdef __cplusplus
};		// end of extern "C"
#endif

#endif // !__NOT_A_DLL
#endif // __ZKL_IMPORTS_H
