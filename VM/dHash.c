/*
Lots of tweaks by C Durland

From: ejp@ausmelb.oz.AU (Esmond Pitt)
Date: Tue, 7 Mar 1989 22:06:26 GMT
Subject: v06i042: dynamic hashing version of hsearch(3)
Message-ID: <1821@basser.oz>
Newsgroups: comp.sources.misc
Sender: msgs@basser.oz

Posting-number: Volume 6, Issue 42
Submitted-By: Esmond Pitt <ejp@ausmelb.oz.AU>
Archive-name: dynamic-hash

-- 
Esmond Pitt, Austec (Asia/Pacific) Ltd
...!uunet.UU.NET!munnari!ausmelb!ejp,ejp@ausmelb.oz
*/
/*
** Dynamic hashing, after CACM April 1988 pp 446-457, by Per-Ake Larson.
** Coded into C, with minor code improvements, and with hsearch(3) interface,
** by ejp@ausmelb.oz, Jul 26, 1988: 13:16;
** also, hcreate/hdestroy routines added to simulate hsearch(3).
**
** These routines simulate hsearch(3) and family, with the important
** difference that the hash table is dynamic - can grow indefinitely
** beyond its original size (as supplied to hcreate()).
**
** Performance appears to be comparable to that of hsearch(3).
** The 'source-code' options referred to in hsearch(3)'s 'man' page
** are not implemented; otherwise functionality is identical.
**
** Compilation controls:
**   DEBUG controls some informative traces, mainly for debugging.
**   HASH_STATISTICS causes HashAccesses and HashCollisions to be maintained;
**     when combined with DEBUG, these are displayed by hdestroy().
**  TEST will compile as a standalone program for testing.
**
** Problems & fixes to ejp@ausmelb.oz. WARNING: relies on pre-processor
** concatenation property, in probably unnecessary code 'optimisation'.
*/

//#define DEBUG		1
#define HASH_STATISTICS 0

#include	<stdio.h>
//#include	<search.h>
#include	<assert.h>
#include	<string.h>
#include	<stdlib.h>

#define __NOT_A_DLL
#include	"dHash.h"	// which includes zklObject.h & zklMemory.h
#include	"zklString.h"
#ifndef ZMALLOC			// from zklMemory.h
   #define ZCALLOC	calloc
   #define ZFREE	free
#endif


/*
** Constants
*/

#define SegmentSize		256
#define SegmentSizeShift	8	/* log2(SegmentSize)	*/
#define DirectorySize		256	//!!!! ick, should be dynamic
#define DirectorySizeShift	8	/* log2(DirectorySize)	*/
#define Prime1			37
#define Prime2			1048583L
#define DefaultMaxLoadFactor	5

/*
** Fast arithmetic, relying on powers of 2,
** and on pre-processor concatenation property
*/

#define MUL(x,y)	((x) << (y##Shift))
#define DIV(x,y)	((x) >> (y##Shift))
#define MOD(x,y)	((x) & ((y) - 1))

/*
** local data templates
*/

typedef struct element
{
    /*
    ** The user only sees the first two fields,
    ** as we pretend to pass back only a pointer to ENTRY.
    ** {S}he doesn't know what else is in here.
    */
    Instance	   *Key;	/* one of the String types */
    Instance	   *Value;
    struct element *Next;	/* secret from user	*/
} Element, *Segment;	// 24/64

typedef struct
{
   unsigned int  p:32;	/* Next bucket to be split	*/
   unsigned int  maxp:32;	/* upper bound on p during expansion	*/
   unsigned long KeyCount;		/* current # keys	*/
   unsigned int  SegmentCount:32;	/* current # segments	*/
   unsigned int  MinLoadFactor:16;
   unsigned int  MaxLoadFactor:16;
   unsigned int  readOnly:1, isZKL:1;
   Segment *Directory[DirectorySize];	// 2048/64, 256 ptrs
   #if HASH_STATISTICS
      long int	HashAccesses, HashCollisions, HashDuplicates;
      int	HashChain;
   #endif	/* HASH_STATISTICS */
} HashTable;	// bytes: 1040/32, 2072/64

typedef unsigned long	Address;

/*
** Internal routines
*/

static Address	Hash(HashTable *,unsigned char *Key);
static void	ExpandTable(HashTable *);

/*
** Code
*/

long hkeyCount(void *Table)
{
   if (!Table) return 0;
   return ((HashTable *)Table)->KeyCount;
}

void *hcreate(unsigned int Count, int isZKL)
{
    unsigned int  i;
    HashTable	 *Table;

    /*
    ** Adjust Count to be nearest higher power of 2,
    ** minimum SegmentSize, then convert into segments.
    */
    i = SegmentSize;
    while (i < Count) i <<= 1;
    Count = DIV(i,SegmentSize);
    if (Count > DirectorySize) Count = DirectorySize;

    Table = (HashTable*)ZCALLOC(sizeof(HashTable),1);
    if (Table == NULL) return 0;
    Table->isZKL = isZKL;
    /*
    ** resets are redundant - done by calloc(3)
    **
    Table->SegmentCount = Table->p = Table->KeyCount = 0;
    */
    /*
    ** Allocate initial 'i' segments of buckets
    */
    for (i = 0; i < Count; i++)
    {
	Table->Directory[i] = (Segment*)ZCALLOC(sizeof(Segment),SegmentSize);
	if (Table->Directory[i] == NULL)
	{
	    hdestroy(Table);
	    return(0);
	}
	Table->SegmentCount++;
    }
    Table->maxp = MUL(Count,SegmentSize);
    Table->MinLoadFactor = 1;
    Table->MaxLoadFactor = DefaultMaxLoadFactor;
    #if DEBUG
	fprintf(
		stderr,
		"[hcreate] Table %p Count %d maxp %d SegmentCount %d\n",
		Table,
		Count,
		Table->maxp,
		Table->SegmentCount
		);
    #endif	/* DEBUG */
    #if HASH_STATISTICS
        Table->HashAccesses = Table->HashCollisions = Table->HashDuplicates = 0;
	Table->HashChain = 0;
    #endif	/* HASH_STATISTICS */
    return Table;
}

    /* Returns 0 if the entire table walked, otherwise the entry where
     * walking stopped.
     */
ENTRY *hWalk(void *table, DhashCb walkie, void *X)
{
   HashTable   *Table = (HashTable *)table;
   unsigned	i,j,s;
   Element     *ptr;
   Segment     *seg;
   size_t	n = 0;

   if (!Table) return 0;	// that was easy

   for (i = 0; i < Table->SegmentCount; i++)
   {
      seg = Table->Directory[i];
      for (j = 0; j < SegmentSize; j++)
      {
	 ptr = seg[j];
	 while (ptr)
	 {
	    s = walkie(X, ptr->Key,ptr->Value, n++);
	    if (!s) return (ENTRY *)ptr;	// all done before I'm done
	    ptr = ptr->Next;
	 }
      }
   }
   return 0;		/* walked the entire table */
}

ENTRY *hNext(void *table, HNextData *data)
{
   HashTable   *Table = (HashTable *)table;	// effing GCC
   unsigned	i,j;
   Element     *ptr;
   Segment     *seg;

   if (!Table || data->done==42) return 0;	// that was easy

   if (data->resume)
   {
      i = data->i; j = data->j; 
      if (i >= Table->SegmentCount) { data->done = 42; return 0; }
      seg = Table->Directory[i];
      ptr = (Element *)data->pe;
      goto next;
   }

   data->resume = 1; // only hit this path once
   for (i = 0; i < Table->SegmentCount; i++)
   {
      seg = Table->Directory[i];
      for (j = 0; j < SegmentSize; j++)
      {
	 ptr = seg[j];
      next:
	 while (ptr)
	 {
	    data->i = i; data->j = j; data->pe = ptr->Next;
	    return (ENTRY *)ptr;
	 }
      }
   }
   data->done = 42;
   return 0;		/* walked the entire table */
}

void hdestroy(void *table)
{
    HashTable   *Table = (HashTable *)table;	// effing GCC
    unsigned	i,j;
    Element    *p, *q;
    Segment    *seg;

    if (Table != NULL)
    {
	for (i = 0; i < Table->SegmentCount; i++)
	{
	    seg = Table->Directory[i];
	    if (seg != NULL)	/* test probably unnecessary */
	    {
		for (j = 0; j < SegmentSize; j++)
		{
		    p = seg[j];
		    while (p != NULL)
		    {
			q = p->Next;
			ZFREE((char*)p);
			p = q;
		    }
		}
		ZFREE(seg);	/* free(Table->Directory[i]); */
	    }
	}
	ZFREE(Table);
	Table = NULL;
	#if HASH_STATISTICS && DEBUG
	    fprintf(
		    stderr,
		    "[hdestroy] Accesses %ld Collisions %ld\n",
		    Table->HashAccesses,
		    Table->HashCollisions
		    );
	#endif
    }
}


#if HASH_STATISTICS

void hdump(void *_table)
{
   HashTable   *Table = (HashTable *)_table;
   int		i,j, collide;
   Element      *ptr;
   Segment      *seg;
   int		isZKL;

   if (!Table)
   {
      fprintf(stderr,"No Hash table!\n");
      return;
   }

   isZKL = Table->isZKL;

   fprintf(stderr,
	"Hash table: p (%d) maxp (%d) KeyCount (%ld) SegmentCount (%d)\n",
	Table->p,Table->maxp,Table->KeyCount,Table->SegmentCount);
   fprintf(stderr,"Hash table: MinLoadFactor (%d), MaxLoadFactor (%d)\n",
	Table->MinLoadFactor, Table->MaxLoadFactor);

   for (i = 0; i < Table->SegmentCount; i++)
   {
      seg = Table->Directory[i];
      for (j = 0; j < SegmentSize; j++)
      {
	 collide = 0;
	 ptr = seg[j];
	 while (ptr)
	 {
	    char *key = (isZKL ? stringText(ptr->Key) : (char *)ptr->Key);
	    if (collide)
	       fprintf(stderr,"<%d>le[%d][%3d]: %s\n", collide,i,j,key);
	    else fprintf(stderr, "Table[%d][%3d]: %s\n",i,j,key);
	    ptr = ptr->Next;
	    collide++;
	 }
      }
   }
}

void hstats(void *_table)
{
   HashTable   *Table = (HashTable *)_table;

   fprintf(stdout,
      "Hash stats: Accesses (%ld) Collisions (%ld = %ld%%)\n",
         Table->HashAccesses, Table->HashCollisions, 
         (Table->HashCollisions * 100) / Table->HashAccesses);
   fprintf(stdout,
      "Number of keys (%ld) Duplicates (%ld = %ld%%) Longest chain (%d)\n",
      Table->KeyCount, 
      Table->HashDuplicates, (Table->HashDuplicates * 100) / Table->KeyCount,
      Table->HashChain);
}

#endif /* HASH_STATISTICS */


static ENTRY *
_hsearch(void *table,char *key, ENTRY *item,ACTION action)
{
   HashTable   *Table = (HashTable *)table;	// effing GCC
   Address	h;
   Segment     *CurrentSegment;
   int		SegmentIndex;
   int		SegmentDir;
   Segment     *p;
   Segment	q, prev;
   int		isZKL;

   #if HASH_STATISTICS
      int chain_length = 0;
      Table->HashAccesses++;
   #endif

   assert(Table != NULL);	/* Kinder really than return(NULL);	*/

   	// "" is an OK key
   if (!key) return NULL;	/* no key, no action */

   h			= Hash(Table,(unsigned char *)key);
   SegmentDir		= DIV(h,SegmentSize);
   SegmentIndex		= MOD(h,SegmentSize);
	/* valid segment ensured by Hash() */
   CurrentSegment	= Table->Directory[SegmentDir];
   assert(CurrentSegment != NULL);	/* bad failure if tripped	*/
   p = &CurrentSegment[SegmentIndex];
   q = *p; prev = 0;
   isZKL = Table->isZKL;

   while (q &&   /* Follow the collision chain */
      (isZKL ? strcmp(stringText(q->Key),key) : strcmp((char *)q->Key,key)) )
   {
      prev = q;
      p = &q->Next;
      q = *p;
      #if HASH_STATISTICS
         Table->HashCollisions++;
	 if (action == ENTER) Table->HashDuplicates++;
	 if (Table->HashChain < ++chain_length) Table->HashChain = chain_length;
      #endif
   } // while
#if 0	// If duplicate and ENTER, creates a new ENTRY
   if ((q != NULL)			||	/* found */
       (action == FIND)			||	/* not found, search only */
       (q = (Element*)ZCALLOC(sizeof(Element),1))
		== NULL)			/* not found & no room */
   {
   //if (duplicateKey) *duplicateKey = TRUE;
      return((ENTRY *)q);
   }
#else		// ENTER changes existing ENTRY
   if (q != NULL)		// found
   {
      if (action==FIND || action==FINDADD) return((ENTRY *)q);
      if (action==DEL)
      {
	 if (!prev) CurrentSegment[SegmentIndex] = q->Next; // at head of chain
	 else prev->Next = q->Next;
	 Table->KeyCount--;
	 item->value = q->Value;
	 ZFREE(q);
	 return item;
      }

	// ENTER --> change ENTRY
      q->Key   = item->key;  /* if old key is in class/fcn, GC can't reclaim */
      q->Value = item->value;
      return((ENTRY *)q);
   }
   // Not found
   // if FIND or no room for new ENTRY, return NULL
   if ((action == FIND) || (action == DEL) ||
       ((q = (Element*)ZCALLOC(sizeof(Element),1)) == NULL))
	return NULL;
#endif

   /* Not found and ENTER */
   *p = q;			/* link into chain	*/
   q->Key = item->key; q->Value = item->value;  /* Initialize the new element */
// q->Next = NULL;		/* cleared by calloc(3) */

    /*
    ** Table over-full?
    */
   if (++Table->KeyCount / MUL(Table->SegmentCount,SegmentSize) >
       Table->MaxLoadFactor)
		ExpandTable(Table);	/* doesn't affect q	*/

//   if (duplicateKey) *duplicateKey = FALSE;
   return (ENTRY *)q;
}

ENTRY *hSearch(void *table, char *key, ENTRY *item, ACTION action)
   { return _hsearch(table,key,item,action); }

ENTRY *hSearchFor(void *table, char *key)
{
   if (!table) return NULL;
   return _hsearch(table,key,0,FIND);
}

    // --> 0 (did it), 1 (read only), 2 (no mem)
int hEnter(void *table, ENTRY *item)
{
   if (!table) return 1;	// black hole
   if (((HashTable *)table)->readOnly) return 1;
   if (!_hsearch(table,stringText(item->key), item,ENTER)) return 2;
   return 0;
}

void hMakeReadOnly(void *table)
   { if (table) ((HashTable *)table)->readOnly = 1; }

int hIsReadOnly(void *table)
   { return (!table || ((HashTable *)table)->readOnly); }

/* ******************************************************************** */
/* ************************ Internal Routines ************************* */
/* ******************************************************************** */

/* With a dictionary of 23k keys, 
 * these are pretty much the same, slight edge to djb2
 * Wash on globalNameTable (1046 keys, 5141 accesses)
 * Compiling the compiler: win to stock hash
 *   1290 keys, 216,104 accesses,  1631 (0%) collisions vs 2735 (1%) collisions
 * Slight win for djb2 in all tests: Collisions: (32934 = 1%) vs (43960 = 1%)
 */

#if 1
static Address Hash(HashTable *Table, unsigned char *key)
{
    Address	   h,address;
    unsigned char *k = key;

    h = 0;
    /*
    ** Convert string to integer
    */
    while (*k)
	h = h*Prime1 ^ (*k++ - ' ');
    h %= Prime2;
    address = MOD(h,Table->maxp);	// short
    if (address < (Address)Table->p)	// p is a short
	address = MOD(h,(Table->maxp << 1));	/* h % (2*Table->maxp)	*/
    return address;
}
#else
  // djb2 hash. http://www.cse.yorku.ca/~oz/hash.html
static Address Hash(HashTable *Table, unsigned char *str)
{
   Address  address;
   unsigned c;
   unsigned long hash = 5381;

   while ((c = *str++))
      hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

//   return hash;
   address = MOD(hash,Table->maxp);	// short
   if (address < (Address)Table->p)	// p is a short
      address = MOD(hash,(Table->maxp << 1));	/* h % (2*Table->maxp)	*/
   return address;
}
#endif

static void ExpandTable(HashTable *Table)
{
    Address	NewAddress;
    int		OldSegmentIndex,NewSegmentIndex;
    int		OldSegmentDir,NewSegmentDir;
    Segment	*OldSegment,*NewSegment;
    Element	*Current,**Previous,**LastOfNew;
    int		isZKL = Table->isZKL;

    if (Table->maxp + Table->p < MUL(DirectorySize,SegmentSize))
    {
	/*
	** Locate the bucket to be split
	*/
	OldSegmentDir	= DIV(Table->p,SegmentSize);
	OldSegment	= Table->Directory[OldSegmentDir];
	OldSegmentIndex = MOD(Table->p,SegmentSize);
	/*
	** Expand address space; if necessary create a new segment
	*/
	NewAddress	= Table->maxp + Table->p;
	NewSegmentDir	= DIV(NewAddress,SegmentSize);
	NewSegmentIndex = MOD(NewAddress,SegmentSize);
	if (NewSegmentIndex == 0)
	{
	   Table->Directory[NewSegmentDir] = 
		(Segment*)ZCALLOC(sizeof(Segment),SegmentSize);
	   Table->SegmentCount++;
	}
	NewSegment = Table->Directory[NewSegmentDir];
	/*
	** Adjust state variables
	*/
	Table->p++;
	if (Table->p == Table->maxp)
	{
	    Table->maxp <<= 1;	/* Table->maxp *= 2	*/
	    Table->p = 0;
	}
//	Table->SegmentCount++;
	/*
	** Relocate records to the new bucket
	*/
	Previous   = &OldSegment[OldSegmentIndex];
	Current    = *Previous;
	LastOfNew  = &NewSegment[NewSegmentIndex];
	*LastOfNew = NULL;
	while (Current != NULL)
	{
//	    if (Hash(Table,(unsigned char *)stringText(Current->Key)) == NewAddress)
	    if (NewAddress == Hash(Table,
	         isZKL ? (unsigned char *)stringText(Current->Key) :
		         (unsigned char *)Current->Key ))
	    {
		/*
		** Attach it to the end of the new chain
		*/
		*LastOfNew = Current;
		/*
		** Remove it from old chain
		*/
		*Previous = Current->Next;
		LastOfNew = &Current->Next;
		Current = Current->Next;
		*LastOfNew = NULL;
	    }
	    else
	    {
		/*
		** leave it on the old chain
		*/
		Previous = &Current->Next;
		Current = Current->Next;
	    }
	}
    }
    else	// 64k is max maxp
       vmThrow(NoVM,E_ASSERTION_ERROR,"Dictionary is full"); //--> vmHalt()
}



/* ******************************************************************** */
/* ******************************* TEST ******************************* */
/* ******************************************************************** */

#ifdef TEST

/* 
 * cc -Aa -DTEST -DHASH_STATISTICS -DDEBUG dhash.c
 */


/* This example taken from hsearch(1), the hsearch man page */

#include <stdio.h>
#include <search.h>


HashTable *Table = NULL;

struct info {       /* this is the info stored in the table */
     int age, room; /* other than the key. */
};

#define NUM_EMPL    50	/* # of elements in search table */

main()
{
	/* space to store strings */
   char string_space[NUM_EMPL*20];

	/* space to store employee info */
   struct info info_space[NUM_EMPL];

	/* next avail space in string_space */
   char *str_ptr = string_space;

	/* next avail space in info_space */
   struct info *info_ptr = info_space;
   ENTRY item, *found_item;
	/* name to look for in table */

   char name_to_find[30];
   int i = 0;

	/* create table */
   Table = hcreate(NUM_EMPL);
   printf("name age room\n");
   while (scanf("%s%d%d", str_ptr, &info_ptr->age,
      &info_ptr->room) != EOF && i++ < NUM_EMPL)
   {
      if (0 == strcmp("q",str_ptr)) break;
		/* put info in structure, and structure in item */
      item.key = str_ptr;
      item.data = (char *)info_ptr;
      str_ptr += strlen(str_ptr) + 1;
      info_ptr++;

		/* put item into table */
      (void) hsearch(Table,item, ENTER);
   }

		/* access table */
   item.key = name_to_find;
   printf("Search for: \n");
   while (scanf("%s", item.key) != EOF)
   {
      if ((found_item = hsearch(Table,item, FIND)) != NULL)
      {
		/* if item is in the table */
	 (void)printf("found %s, age = %d, room = %d\n",
		found_item->key,
		((struct info *)found_item->value)->age,
		((struct info *)found_item->value)->room);
      }
      else
      {
	 (void)printf("no such employee %s\n", name_to_find);
      }
   }
}

#endif	/* TEST */
