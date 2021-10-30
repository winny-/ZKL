/* zklMethod.h : Methods, Properties, Op, Deferred, Partial
 * 
 * Copyright (c) 2006,7,8,9,10,11,2012 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#ifndef __ZKL_METHOD_H
#define __ZKL_METHOD_H

#ifdef __NOT_A_DLL
extern int deferredID, methodID;

Instance *deferredIsEvaled(Instance *);
void	  methodConstruct(void);
void	  methodThunk(Instance *, int64_t, pVM);
Instance *propertyCreate(Instance *, char *name, int var, int itype, pVM);
void	  propertyThunk(Instance *, int64_t, pVM);
void	  deferredThunk(Instance *, int64_t, pVM);

#define IS_DEFERRED(i)  ( !(i)->iflag2 && (i)->objID == deferredID )

int       isPartialFcn(Instance *);
Instance *partialCall(Instance *,pArglist,pVM);
#endif	// __NOT_A_DLL


#ifdef __cplusplus
extern "C" {
#endif

DllExport Instance *methodCreate(Instance *, char *name, pMethod method, pVM);
DllExport Instance *methodCreate2(
	Instance *, char *name, pMethod method, int itype, pVM);
DllExport Instance *Method_call(Instance *,pArglist,pVM);

DllExport Instance *Property_call(Instance *,pArglist,pVM);

DllExport Instance *Method_instance(Instance *,pVM);
DllExport pMethod   methodMethod(Instance *);

DllExport Instance *Op_create(Instance *,pArglist,pVM);
DllExport Instance *Op_call(Instance *,pArglist,pVM);

DllExport Instance *deferredCreate(Instance *f, pArglist arglist, pVM);
DllExport Instance *deferredCall(Instance *,pArglist,pVM);
DllExport Instance *Deferred_value(Instance *,pVM);
DllExport Instance *Partial_fp( Instance *,pArglist,pVM);
DllExport Instance *Partial_fp0(Instance *,pArglist,pVM);
DllExport Instance *Partial_fp1(Instance *,pArglist,pVM);
DllExport Instance *Partial_fp2(Instance *,pArglist,pVM);
DllExport Instance *Partial_fpN(Instance *,pArglist,pVM);
DllExport Instance *Partial_fpM(Instance *,pArglist,pVM);

DllExport Instance *Deferred_f(Instance *,pVM);

#ifdef __cplusplus
};		// end of extern "C"
#endif

#endif // __ZKL_METHOD_H
