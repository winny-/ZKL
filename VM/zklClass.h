/* zklClass.h : header file for Classes
 * 
 * Copyright (c) 2006,7,8,9,10,2011 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#ifndef __ZKL_CLASS_H
#define __ZKL_CLASS_H

// #include "zklObject.h" implied here

#include "zklUtil.h"	// StringTable

#ifdef __CLASS_INTERNALS

   #define VAR_RO		0
   #define VAR_PROXY		1
   #define NUM_VAR_BITTERS	2

   typedef struct		// a Class Instance
   {
      Instance  instance;
      unsigned  co:16, po:16; // offsets into data for classes, parents
      void     *classBase;	 // really a ClassBase *
      ZKL_Fcn  *fcns;		 // --> classBase->data
      Instance 	// really Class but it is a pain to cast, cast, cast
	  *container,	 // if this is a class in a class
	  *topdog;	 // "youngest" child of class hierarchy, if parent
      Instance *data[0]; // this expands to hold the arrays (instance data)
	   // Instance *vars[numVars];		// compiler ordered/sorted
	   // Instance *classes[numClasses];	// insertation order
	   // Instance *parents[numParents];	// insertation order
   }Class;	// 56 bytes Linux/64

   #define VARS(c)     ( ((Class *)c)->data )
   #define CLASSES(c)  ( (Instance **)((char *)c + ((Class *)c)->co) )
   #define PARENTS(c)  ( (Instance **)((char *)c + ((Class *)c)->po) )

#endif // __CLASS_INTERNALS


#ifdef __cplusplus
extern "C" {
#endif

#ifdef __NOT_A_DLL
extern DllExport Instance *NullClass;

void	   classConstruct(void);
Instance  *classUp(Instance *,int n);
char	  *classVaultPath(Instance *);
int	   classFindVar2(Instance *,char *name,int *isProxy, pVM);
Instance **classFindVarSlot(Instance *,char *name, int searchParents,pVM);
Instance  *classFindParent( Instance *,char *name, int searchParents,pVM);
char	  *classNameOfNthVar(Instance *, unsigned int);
Instance  *classDestructor(Instance *, Instance *pfcn);

Instance *classRecall(Instance *,char *name,pMethod *,void *pfcn, pVM);
int       classResolveN(Instance **self,
		unsigned idx, pMethod *, void *pFcn, int bitch, pVM);

Instance *classGetVar(Instance *,int n);
void	  classSetVarOnce(Instance *,int,Instance *);
void	  classSetVarBits(Instance *,char *bits,int which);

void	  classResetMarks(Instance *);

Instance *Class_BaseClass(Instance *,pArglist,pVM);

#ifdef __PC_INTERNALS
   Instance *classFcnMatchup(Instance *,ZKL_Code *,pVM);
#endif

#ifdef __FCN_INTERNALS
   Instance *classPrepForRun(Instance *,Instance **fence, ZKL_Fcn *fcn, pVM);
#endif

size_t	   _sumList(Instance *list);
int	   _classIndex(Instance *,char *name);
int	   _fcnIndex(Instance *,char *name);

void	   classVerify(Instance *);
#endif // __NOT_A_DLL

DllExport Instance  *classRunConstructor(Instance *,pArglist,pVM);
DllExport Instance *classRun(Instance *, pArglist,pVM);
DllExport Instance *Class_fullName(Instance *, pVM);
DllExport Instance *classCopy(Instance *,pVM);
DllExport Instance *classFindVar(Instance *,char *name, Instance *setTo,pVM);
DllExport Instance *classFindVarById(Instance *,unsigned id,Instance *setTo,pVM);
DllExport char	   *className(Instance *);
DllExport Instance *classEmbryo(StringTable *names,
		unsigned numFcns,unsigned numClasses,
		unsigned numParents,Instance **parents,
		int itype, pVM);
DllExport void	    classAddFcnN(Instance *,  Instance *fcn,  unsigned n, pVM);
DllExport void	    classAddClass(Instance *,Instance *class,pVM);
DllExport void	    classCookClass(Instance *,pVM);
DllExport Instance *classFindClass(Instance *,char *name, int searchParents,pVM);
DllExport Instance *classFindClassById(Instance *,unsigned idx,int searchParents,pVM);
DllExport Instance *classFindParentById(Instance *,unsigned idx,int searchParents,pVM);
DllExport Instance *classFindFcn(  Instance *,char *name,
				  int searchParents,void *pfcn,pVM);
DllExport Instance *classFindFcnById(Instance *,
		unsigned idx,int searchParents,void *pfcn,pVM);

DllExport Instance *classNthClass(Instance *,unsigned n);
DllExport int	    classIsInstanceOf(Instance *self,Instance *B);
DllExport int	    classIsChildOf(Instance *self,Instance *B);
DllExport Instance *Class_topDog(Instance *,pVM);
DllExport Instance *makeVarList(int n,Instance **data,pVM);
DllExport Instance *Class_container(Instance *,pVM);
DllExport int	    classIsRootClass(Instance *);
DllExport Instance *Class_eq(Instance *,Instance *,pVM);
DllExport Instance *Class_rootClass(Instance *,pVM);
DllExport void	    classSetAttributes(Instance *,char *attributes, pVM);

#ifdef __PC_INTERNALS
   DllExport Instance *classSearchForFcn(Instance *,ZKL_Code *, int deepSearch);
#endif

#ifdef __cplusplus
};		// end of extern "C"
#endif

#endif // __ZKL_CLASS_H
