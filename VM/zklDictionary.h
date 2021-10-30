/* dictionary.h : zkl stuff on top of dHash.h
 * 
 * Copyright (c) 2006,7,8,9,10,2011 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#ifndef __ZKL_DICTIONARY_H
#define __ZKL_DICTIONARY_H

#include "dHash.h"

typedef struct { Instance *x; pVM vm; int n; size_t a; } DX;

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __NOT_A_DLL

void dictionaryConstructPartI(void);
void dictionaryConstructPartII(void);

Instance *Dictionary_create(Instance *,pArglist,pVM);
Instance *Dictionary_toString(Instance *self,pArglist arglist,pVM);
Instance *Dictionary_keys(Instance *, pVM);
//void 	  Dictionary_keys(Instance *,Instance **result,pVM);

#endif	// __NOT_A_DLL


DllExport Instance *dictionaryCreate(size_t initialSize, int itype, pVM);
DllExport void	    dictionaryAdd(Instance *,Instance *key,Instance *value,pVM);
DllExport ENTRY    *dictionarySearchFor(Instance *, char *key, pVM);
DllExport ENTRY    *dWalk(Instance *,DhashCb fcn, void *X, pVM);
DllExport Instance *dictionaryMakeRO(Instance *,pVM);

#ifdef __cplusplus
};		// end of extern "C"
#endif

#endif // __ZKL_DICTIONARY_H
