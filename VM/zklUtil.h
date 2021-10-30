/* zklUtil.h : odds and ends
 * 
 * Copyright (c) 2006,2007,2008-12,2013 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#ifndef __ZKL_UTIL_H
#define __ZKL_UTIL_H

#include <stdio.h>	// FILE

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))


				///// String Tables
typedef struct { int n; char *strings; size_t size; } StringTable;

#include "zklObject.h"	// recursive include, references StringTable
#include "zklAtomic.h"	// CAtomicBool

				///// String Tables
DllExport void	    stCalcSize(StringTable *);
DllExport char	   *stNextString(char *);
DllExport Instance *stToList(StringTable *,pVM);

DllExport Instance *zipWithObjs(Instance *,pArglist arglist,pVM);
DllExport Instance *zipObjs(Instance *self,Instance *f,pArglist,pVM);

int wildmat(char *text,char *pattern,int flags);		// wildmat.c

			//////////////////// fxpand.c
typedef int (*fxpfi)(char *, void *);
int fxpand(char *name,int flags,fxpfi processFname, void *X);
   #define FXP_ONLY_ONE		    0x1
   #define FXP_NO_PATH		    0x2
   #define FXP_NO_TRAILING_SLASH    0x4
   #define FXP_NO_DIRS		    0x8
   #define FXP_ONLY_DIRS	   0x10
   #define FXP_IGNORE_CASE	   0x20
   #define FXP_DF		   0x40

   #define FXP_USER_MASK	 0xFFFF	// valid user flags, YOU mask

   #define FXP_GLOB_LIST	0x10000	// nm is a list: nm\0nm\0nm\0\0


			//////////////////// util.c
DllExport FILE	  *searchForFile(char *fileName, pVM);

#ifdef _MSC_VER
   DllExport HMODULE  searchForLib(char *libName, pVM);
#elif defined(__unix__)
   DllExport void    *searchForLib(char *libName, pVM);
#endif

int	irand(void);
void	isrand(unsigned int seed);

    // ZGetter(self, read index [0..], X, num objs written [0..], vm)
    // idx increases monotonically from zero, return zero to stop
    // Caller MUST NOT call after zero is returned
typedef Instance *(*ZGetter)(Instance*,size_t idx,void *X,size_t sz,pVM);

   // ZAggregator constants, zero,one NOT used!
#define ZA_LIST		2	// -->Tuple
#define ZA_STRING	3
#define ZA_DATA_D	4
#define ZA_DATA_S	5
#define ZA_NADA		6
///////////////// the above are only for zfilter & zapply

#define ZA_C	       10	// call C code, user modifies ZAgg
#define ZA_2M	       11	// sink is two methods, user modifies ZAgg

#define ZA_OBJ		7	// for my use: Pipe, File, Socket, etc
#define ZA_METHOD	8	// for my use: eg List().write
#define ZA_RUN		9	// for my use: Fcn, Deferred, Method, etc

#define MAX_ACTIONS    20	// your code: unsigned actionTypes[MAX_ACTIONS];

#define ZA_2SINK_OK		0  // ZAgg filled in
#define ZA_2SINK_NOOP		1  // I don't Implement .toSink but try .write
#define ZA_2SINK_FAIL		3  // tried and failed
#define ZA_2SINK_NO_CAN_DO	4  // I will not be written to!
#define ZA_SANDBOX_TOO_SMALL	5
#define ZA_CLOSED		6  // Tried to open a closed sink, or you throw


typedef int       (*CSinkWrite)(void *X,Instance *i,pVM);
typedef Instance *(*CSinkClose)(void *X,pVM);

typedef struct {	// the thing that goes with ZA_C
   void *X;	// your context, you manage, alloc, free, etc
   CSinkWrite write;
   CSinkClose close;
}CSink;	// Linux/64: 24, Win10/32: ??

typedef struct {  // the thing that goes with ZA_M, wrapper holds self
   pMethod write, close;
}MSink;

typedef struct
{
   unsigned int how:8;		// ZA_* selector, you fill this in
   unsigned int rdepth:16;	// recursion count, for internal use
   unsigned int closed:1;	// for internal use
	// mystery flags: flags I might want to use at some point
   unsigned int mf1:1,mf2:1,mf3:1,mf4:1;
   union			// you fill this in if ZA_C or ZA_2M
   {
      MSink msink;	// ZA_OBJ, eg Pipe.write
      CSink csink;	// C code sink
   }dst;
   size_t    sz;	// objects written, for internal use
   Instance *i;	   // aka self, will be fenced, you fill this in, optional
   unsigned char sandbox[0];  // your own struct, this is aligned
}ZAgg;	// ZAggie without the sandbox


    // pump constants
#define PMP_ZERO	 0x4	// if no result, return 0 (vs Instance)
#define PMP_OK2CNT	 0x8	// .pump([cnt],sink,action,..), enable cnt
#define PMP_DFAULT_CNT	0x10	// if no count, use this. n<<16, you get 15 bits

DllExport Instance *pump(Instance *, unsigned flags, ZGetter,void *X,
   pArglist,int a0, pVM);

DllExport Instance *zapply(Instance *self, ZGetter,void *X, CSinkWrite,void *Y,
   pArglist,int a0, pVM);

DllExport Instance *zfilter(Instance *,  int how, ZGetter,void *X,
   pArglist,int a0, pVM);
DllExport Instance *zfilter22(Instance *,int how, ZGetter,void *X,
   pArglist,int a0, pVM);
DllExport Instance *zfilter1(Instance *, int _,     ZGetter,void *X,
   pArglist,int a0, pVM);
DllExport Instance *zfilter1n(Instance *,int _,     ZGetter,void *X,
   pArglist,int a0, pVM);
DllExport Instance *zfilterNs(Instance *,int _,     ZGetter,void *X,
   pArglist,int a0, pVM);

DllExport Instance *zreduce(Instance *, ZGetter,void *X, pArglist,int a0, pVM);
DllExport Instance *zreduce2(Instance *,ZGetter,void *X, pArglist,int a0, pVM);

DllExport Instance *sinkCreate(size_t sbSz, ZAgg **, pVM);

#ifdef __NOT_A_DLL	// probably fine in DLLs
   unsigned  _ftyper(Instance *f,int zero, pVM);
   int       _scanAction(Instance *f,pArglist,int a0,
			 int *_oneF,unsigned actionType[], pVM);
   Instance *_zrun(Instance *i,Instance *f,unsigned ftype,
		   int *_stop,pArglist,pVM);
      // _zrun stop codes, see _zrun() for details on return codes
   #define ZR_OK	 0 // everything hunky dory, if(stop) break; works
   #define ZR_SKIP	 1 // skip this, and the rest of the actions
   #define ZR_STOP	 2 // stop the loop
   #define ZR_READ	 3 // read from src, append to arglist
   #define ZR_WRITE	 4 // write to result, skip
   #define ZR_RECURSE	 6 // recurse, ick
   #define ZR_DROP	 7 // remove this action from action list
   #define ZR_AGAIN	 8 // loop on this action
   #define ZR_I		 9 // restore src instance (as result)
#endif

			////////////////////////////////// util.c
#if 0
	///// Trip Wires
typedef struct TripWire
{
   struct TripWire *next;
//   CAtomicInt	    tripWire;
//   ACInt	    tripWire;
} TripWire;

typedef struct		// PThreads makes these cache safe
{
   int	     wire;
   union
   {
      ACInt wires[2];
      ACInt tripWire1,tripWire2;
   } wires;
//   ACInt     tripWire1,tripWire2;
//   TripWire *next;
   SpinLock  lock;
} TripWireRoot;

DllExport void	    tripWireRootInit(TripWireRoot *);
DllExport void	    setTripWire(TripWireRoot *);
DllExport void	    tripTheLightFantastic(TripWireRoot *);
#endif


			////////////////////////////////// walker.c
DllExport Instance *walkerCreate(Instance *, ZGetter, void *X, int mx, pVM);


#endif // __ZKL_UTIL_H
