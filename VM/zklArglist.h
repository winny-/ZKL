/* arglist.h : Method/Fcn arglist getters
 * 
 * Copyright (c) 2006,7,8,9,10,2011 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */
#ifndef __ZKL_ARGLIST_H
#define __ZKL_ARGLIST_H

#define ARG_NO_LEN_IS_STAR	0x001
#define ARG_OFF_EQ_LEN_OK	0x002
#define ARG_CONSTRAIN		0x008
#define ARG_OFF_OPTIONAL	0x010

#define ARG_LEN_EQ_0_OK		0x0100
#define ARG_LEN_HAS_2B_GOOD	0x0200


#ifdef __cplusplus
extern "C" {
#endif

#ifdef __NOT_A_DLL
extern DllExport Instance *NoArglist;

void arglistConstruct(void);
#endif

	// Note: 0 or "" is fine for msg

DllExport int	    arglistLen(pArglist, pVM);
DllExport Instance *arglistGet(pArglist,int n, char *msg, pVM);
DllExport Instance *arglistTryToGet(pArglist,int n);

	// eg TO_STRING
DllExport Instance *arglistGetT(pArglist,int n, int TO_nm, char *msg,pVM);
DllExport Instance *convertTo(Instance *, int TO_nm, pVM);

	// Get by Type (eg DataType, FcnType)
DllExport Instance *arglistGetBT(pArglist,int,int type,char *msg,pVM);
DllExport Instance *arglistTryToGetBT(pArglist,int,int type,char *msg,pVM);

	// Get by Object
Instance *arglistGetBObj(pArglist,int n, ZKL_Object *, char *, pVM);
DllExport Instance *arglistGetBOID(pArglist,int n, int oid, char *, pVM);
	
	// Get Strings
DllExport char	   *arglistGetOnlyString(pArglist,int n,char *,pVM);
DllExport char	   *arglistTryToGetString(pArglist,int n, char *msg, pVM);
DllExport Instance *arglistGetString(pArglist,int n, char *msg,pVM);

DllExport Instance *arglistConcat(int n, pArglist,pVM);

	// Get Bools
DllExport int	    arglistGetBool(pArglist,int n, char *msg,pVM);
DllExport int	    arglistTryToGetBool(pArglist,int n, int B, char *,pVM);

	// Get Ints
DllExport int64_t   arglistGetInt(pArglist,int n, char *msg,pVM);
DllExport int	    arglistTryToGetInt(pArglist,int n,int64_t *, char *,pVM);
DllExport int64_t   arglistTryToGetIntD(pArglist,int,int64_t d, char *,pVM);

	// Get Floats
DllExport double    arglistGetFloat(pArglist, int n, char *msg, pVM);
DllExport int	    arglistTryToGetFloat(pArglist,int,double *, char *,pVM);

	// Get by Instance (eg d := Data(), get(d)
DllExport Instance *arglistGetBI(pArglist,int, Instance *i, char *msg, pVM);
DllExport Instance *arglistTryToGetBI(pArglist,int, Instance *i, char *, pVM);

	// slicing and dicing
DllExport void	    offsetIndexError(char *msg,Offset_t n,size_t len,pVM);
DllExport size_t    arglistGetIndex(pArglist,int i,size_t len, char *,pVM);
DllExport int	    arglistGetOffset(pArglist, int i,
			size_t len, int flags, Offset_t *N, char *,pVM);
DllExport int	    arglistGetSize(pArglist, int i,
			size_t len, size_t *size, pVM vm);
DllExport int	    arglistGetChunk(pArglist arglist, int i, unsigned flags,
			size_t len, size_t *offset, size_t *num, char *,pVM vm);
DllExport int       checkOffset(Offset_t, size_t length, int maxOK, size_t *);
DllExport Offset_t  constrainOffset(Offset_t x,size_t len,int maxOK, char *,pVM);
DllExport int	    constrainOffLen(Offset_t off,Offset_t len,size_t size,
			int maxOK, size_t *idx, size_t *num);
DllExport Offset_t  verifyOffset(Offset_t,size_t length,int maxOK, char *msg, pVM);
DllExport int	    arglistConstrainOffset(pArglist arglist,int i,
			size_t len, unsigned flags, Offset_t *offset,char*,pVM);
DllExport void	     arglistMaybeOL(pArglist,int a0, size_t len,
			size_t *offset, size_t *count, pVM);

DllExport void	    _missingArg(pArglist,int n,char *msg,pVM vm);

#ifdef __cplusplus
};		// end of extern "C"
#endif

#endif // __ZKL_ARGLIST_H
