/* zmalloc: a front end to dlmalloc
 */

#include "zklObject.h"		// Instance
#include "zklAtomic.h"
//#include "zklMemory.h"


//#define __DLMALLOC

#define USE_DL_PREFIX	1
#define USE_LOCKS	1
#if __USE_SPACES
   #define FOOTERS		1  // so free() can find it's mspace
   #define MSPACES		1
   //#define ONLY_MSPACES	1
#endif

#ifdef WIN32
   #include <malloc.h>
#endif

#define DEBUG 0
#define DEFAULT_GRANULARITY_ALIGNED 0

//#define USAGE_ERROR_ACTION(m,p) vmHalt("dlmalloc: Internal error")
#define ABORT vmHalt("dlmalloc: Internal error")

#include "nedmalloc/malloc.c.h"		// dlmalloc


#if DEBUG
   void zmallocCheck() { check_malloc_state(gm); }
   void checkChunk(void *ptr,size_t n) { check_malloced_chunk(gm,ptr,n); }
#endif

#if __USE_SPACES
    // The idea is more heaps == less thread contention per heap
    // But not so good in practice (I think buckets are the factor).
#define NUM_SPACES	5		// up to about 5
static mspace spaces[NUM_SPACES];	// mspace is a void *

mspace *getSpace(void)
{
   int	   n,z;
   mspace  space;
   mstate  ms;		// *malloc_state

   n = rand() % NUM_SPACES;
   z = 256;		// I see max 2 passes compiling the parser

   while(z--)
   {
      for (; n < NUM_SPACES; n++)
      {
	 space = spaces[n];
	 ms    = (mstate)space;
	 if (ms->mutex.l) continue;	// dlmalloc has lock, very fast check
	 return space;
      }
      n = 0;
   }
   n = rand() % NUM_SPACES;
   return spaces[n];
}

void *zmalloc(size_t size)
   { return mspace_malloc(getSpace(),size); }

void *zcalloc(size_t num, size_t size)
   { return mspace_calloc(getSpace(),num,size); }

void zmallocStats(void)
{
   int n;
   for (n = 0; n < NUM_SPACES; n++)
   {
      mspace_malloc_stats(spaces[n]);
      fprintf(stderr, "---------------------\n");
   }
}

#else	// Don't use spaces

	// These are here in case the macros got wonky
void *zmalloc(size_t size)	       { return dlmalloc(size); }
#if 0
void *zmalloc(size_t size){
void *p; check_malloc_state(gm);
p = dlmalloc(size); 
check_malloc_state(gm); return p;
}
#endif

void *zcalloc(size_t num, size_t size) { return dlcalloc(num,size); }
#if 0
void *zcalloc(size_t num, size_t size) {
void *p;check_malloc_state(gm);
p = dlcalloc(num,size);
check_malloc_state(gm); return p;
}
#endif

void zmallocStats(void) { dlmalloc_stats(); }

#endif	// __USE_SPACES


#if __ZM_BULK_FREE
    // Only called from the GC thread to free instances allocated by
    //   instanceAllocate()
    // No locking
static Instance *toBeFreedList = 0;
void zibFree(Instance *i)
{
   i->nexti = toBeFreedList;
   toBeFreedList = i;
}

    // Compiling the parser, 27 hits averaging 11624 (5,635 - 30,628)
    /* OK, I don't get it, this is [slightly] slower than lock/unlock for
     * every free. Windows & Linux. Single stepping seems to show what I'd
     * expect (check thread id, inc lock count, no expensive ops). Doesn't
     * sync with what I see with bulk bucket free.
     */
void ziBulkFree(void)
{
   Instance *i = toBeFreedList, *nexti;

   #if __USE_SPACES
      int    n;
      mspace space;
      mstate ms;		// *malloc_state
   #endif

   if (!i) return;	// yes, this happens
   #if __USE_SPACES
      for (n = 0; n < NUM_SPACES; n++)   // grab dllocks for all spaces
      {
	 space = spaces[n];
	 ms    = (mstate)space;
	 PREACTION(ms);		// win32_acquire_lock()
      }
   #else
	 PREACTION(gm);		// win32_acquire_lock()
   #endif
   	// now, do an essentially lock free free
      for(i = toBeFreedList; i; i = nexti) { nexti = i->nexti; dlfree(i); }
   #if __USE_SPACES
      for (n = 0; n < NUM_SPACES; n++)   // release dllocks
      {
	 space = spaces[n];
	 ms    = (mstate)space;
	 POSTACTION(ms);
      }
   #else
      POSTACTION(gm);
   #endif

   toBeFreedList = 0;
}
#endif

void zmallocConstruct(void)
{
   #if __USE_SPACES
      int n;
      for (n = NUM_SPACES; n--; )
	 spaces[n] = create_mspace(0,1);
   #endif
}
