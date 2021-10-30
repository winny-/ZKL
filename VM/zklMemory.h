/* memory.h : memory management, header for gc.c and allocate.c
 * 
 * Copyright (c) 2006,7,8,9,10,2011 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#ifndef __memory_h
#define __memory_h

	// Values for instanceAllocate, Instance->itype
#define I_UNTOUCHABLE	1  // INVISIBLE forever
#define I_IMMORTAL	2  // Can never die and neither do contents
#define I_INVISIBLE	3  // Won't be a known instance but that can change
#define I_OWNED		4  // Somebody owns this instance
#define I_SPECIAL	5  // UNTOUCHABLE but mark if seen
#define I_2SPECIAL	6  // UNTOUCHABLE but mark contents if seen

#define GC_NUM_TYPES	7	// sync with table in gc.c


#define GC_SANITY_CHECK	1

#include "zklObject.h"	// recursive include
#include "zklAtomic.h"	// SpinLock

    // dlmalloc can be quite slow on Linux
    // Seems to be wash on Windows10, dlmalloc much faster on WinXP
#if (defined(_MSC_VER) && _MSC_VER<=1500)
   #define __DLMALLOC    // Use dlmalloc; add zmalloc.c to Makefile
   //#define __NEDMALLOC // medmalloc; add nedmalloc/nedmalloc.c to Makefile
   //#define __USE_HEAPS // nedmalloc; use multiple heaps spread amongst threads
#endif

#ifdef __DLMALLOC
   #define __USE_SPACES		0	// Sigh, not an improvment
   #define __ZM_BULK_FREE	0	// argh! slower than regular free

   #undef __NEDMALLOC
   #undef __USE_HEAPS

   #if __USE_SPACES
      #define ZMALLOC	zmalloc
      #define ZCALLOC	zcalloc
      #define ZREALLOC	dlrealloc
      #define ZFREE	dlfree
   #else
#if 0
#define ZMALLOC	zmalloc
#define ZCALLOC	zcalloc
#define ZREALLOC  zrealloc
#endif
      #define ZMALLOC	dlmalloc
      #define ZCALLOC	dlcalloc
      #define ZREALLOC	dlrealloc
      #define ZFREE	dlfree
   #endif
#endif

#ifdef __NEDMALLOC
   #undef __DLMALLOC
   #include "nedmalloc/nedmalloc.h"
   #ifdef __USE_HEAPS
      #ifdef __NOT_A_DLL
	 #define ZMALLOC(size)	    nedpmalloc(_zmheap,size)
	 #define ZCALLOC(num,size)  nedpcalloc(_zmheap,num,size)
	 #define ZREALLOC(ptr,size) nedprealloc(_zmheap,ptr,size)
	 #define ZFREE(ptr)	    nedpfree(_zmheap,ptr)
      #else	// call my glue code (different from the __DLMALLOC stuff)
	 #define ZMALLOC	zmalloc
	 #define ZCALLOC	zcalloc
	 #define ZREALLOC	zrealloc
	 #define ZFREE		zfree		    
      #endif
   #else	// no fancy heap stuff
      #define ZMALLOC	nedmalloc
      #define ZCALLOC	nedcalloc
      #define ZREALLOC	nedrealloc
      #define ZFREE	nedfree
   #endif	// __USE_HEAPS
#endif

#ifndef ZMALLOC
   #define ZMALLOC	malloc
   #define ZCALLOC	calloc
   #define ZREALLOC	realloc
   #define ZFREE	free
#endif

#define instanceFree(ip) ZFREE(ip)


typedef struct RootMarker
{
   void (*marker)(void);
   struct RootMarker *next;
} RootMarker;

typedef struct IBucketHeader
{
   void	       *buckets;
   unsigned int	blobSize;	// each bucket contains blobs of the same size
   unsigned int	blobsPerBucket;	// that are allocated per bucket
   unsigned int	numBuckets;	// that are in use
   unsigned int bucketSize;	// avoid some simple math
   int		pleaseGCme;	// 1 if sweeps itself (BInstances)
   int		private;	// if nobody else can use these buckets
   SpinLock	lock;
   #if __IB_BULK_FREE
      void *freeList;
      void *toBeFreedList;	// waiting to move the the free list
      void *lastb;		// last blob in toBeFreedList
   #endif
   struct IBucketHeader *next;
} IBucketHeader;


//////////////////////////////////////////////////////// Prototypes

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __NOT_A_DLL

	////////////////// allocate.c
void	  allocateConstruct(void);

	////////////////// gc.c
void	  gcConstruct(void);
void	  startGarbageMan(void);

	////////////////// zmalloc.c
void zmallocConstruct(void);
void zibFree(Instance *);
void ziBulkFree(void);


#ifdef __USE_HEAPS
   extern nedpool *_zmheap;
#endif
#endif	// __NOT_A_DLL

#ifdef __DLMALLOC
   void zmallocStats(void);

   DllExport void *dlmalloc(size_t);
   DllExport void *zmalloc( size_t);
   DllExport void *dlcalloc(size_t num, size_t size);
   DllExport void *zcalloc( size_t num, size_t size);
   DllExport void *dlrealloc(void *,size_t);
   DllExport void *zrealloc( void *, size_t size);
   DllExport void dlfree(void *);
#endif

#ifdef __USE_HEAPS
   DllExport void *zmalloc(size_t);
   DllExport void *zcalloc(size_t num, size_t size);
   DllExport void *zrealloc(void *,size_t);
   DllExport void zfree(void *);
#endif

	//////////////////////////////////// gc.c
#ifdef __GC_INTERNALS
   extern CAtomicInt gcState;
   extern CAtomicInt markingInProgress;

   void immortalBInstance(Instance *);
#endif

DllExport void	    collectGarbage(int waitUntilCollected, pVM);
DllExport void	    instanceMark(Instance *);
DllExport void	    instanceMarkN(int n, ...);
DllExport Instance *addToCollectables(Instance *,int itype,pVM);

DllExport void      gcRegisterMarker(RootMarker *);

DllExport void      instanceIsOrphan(Instance *);
DllExport void	    instanceIsOrphan2(Instance *, Instance *);
DllExport void	    instanceIsOrphanWait(Instance *,pVM);

DllExport void	    instanceVerify(Instance *,int mightBeInt,pVM);


	/////////////////////////////////// allocate.c
DllExport Instance   *instanceAllocate(size_t size,ZKL_Object *,int throwE,pVM);
DllExport Instance   *instanceInit(Instance *self, ZKL_Object *, int type);

#ifdef __USE_HEAPS
   DllExport void *zmalloc(size_t size);
   DllExport void *zcalloc(size_t num, size_t size);
   DllExport void *zrealloc(void *ptr,size_t size);
   DllExport void  zfree(void *ptr);
#endif

DllExport IBucketHeader *bucketReserve(int blobSize,
		int numInstancesPerBlock, IBucketHeader *, int isPrivate, pVM);
DllExport IBucketHeader *ibucketReserve(ZKL_Object *,
		int numInstancesPerBlock, IBucketHeader *, int isPrivate, pVM);
DllExport IBucketHeader *ibucketShare(ZKL_Object *, int slop);
DllExport IBucketHeader *ibucketHitchHike(ZKL_Object *,
     int slop, int numInstancesPerBlock, IBucketHeader *, pVM);
DllExport void		 ibucketPoach(IBucketHeader *, ZKL_Object *, pVM);

DllExport Instance *ibucketAllocate(IBucketHeader *,
			ZKL_Object *,int itype,int bitch,pVM);
DllExport void      ibucketFree(IBucketHeader *, Instance *);
DllExport void	     bucketFree(IBucketHeader *, void     *blob);
DllExport Instance *containerIsCooked(IBucketHeader *, Instance *, int itype);
DllExport Instance *iHazBucket(ZKL_Object *, int sz,int slop, 
			int itype,IBucketHeader **, pVM);


#ifdef __GC_INTERNALS
   Instance *ibucketStats(Instance *,pArglist,pVM);
   void	     ibucketSweep(void);
   void	     ibucketFreeThemAll(void);
#endif	// __GC_INTERNALS

#ifdef __cplusplus
};		// end of extern "C"
#endif


#ifdef __GC_INTERNALS
   typedef struct
   {
      BInstance  instance;
      Instance  *ref;	// to object of interest or Void
   } WeakRef;	// 8 bytes


    // defines for Instance gc flags
   #define GC_TYPE(i)		( (i)->itype )

   #define THE_MARK(i)		( (i)->gcMark )

   #define GC_CLEAR_MARK(i)	( THE_MARK(i) = 0 )
   #define GC_MARK(i)		( THE_MARK(i) = 1 )
   #define GC_IS_MARKED(i)	( THE_MARK(i) )
#endif	// __GC_INTERNALS

#endif /* __memory_h */
