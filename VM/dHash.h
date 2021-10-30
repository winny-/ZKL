/* dhash.h : Header file for dhash.c
 * Very close to <search.h> - hsearch(3C) support minus the stuff for
 *   lsearch(3C) and tsearch(3C).
 */

    /* Notes
     * An entry isn't added if the key already exists in the table.
     * If you want to force the new value:  Adding the new value and set the
     *   data field.  You'll do extra work in the case where the key doesn't
     *   exist but not much.
     */

#ifndef __DHASH_H_INCLUDED
#define __DHASH_H_INCLUDED

#include "zklObject.h"	// for Instance *

    // key is one of the String objects
typedef struct { Instance *key; Instance *value; } ENTRY; // from search.h
typedef enum   { FIND, ENTER, FINDADD, DEL, } ACTION;	  // from search.h

typedef int (*DhashCb)(void *x, Instance *key, Instance *value, size_t n);

typedef struct
{
   int   resume, howza, i,j, done;
   void *pe;
} HNextData;


#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

void   *hcreate(unsigned sz, int isZKL);
void	hdestroy(void *table);
ENTRY  *hsearch(void *table, ENTRY *, ACTION);
ENTRY  *hSearch(void *table, char *key, ENTRY *, ACTION);
ENTRY  *hSearchFor(void *table, char *key);
int     hEnter(void *table, ENTRY *);
ENTRY  *hWalk(void *table, DhashCb, void *);
ENTRY  *hNext(void *Table, HNextData *);
long	hkeyCount(void *table);
void    hMakeReadOnly(void *table);
int     hIsReadOnly(void *table);

	/* if HASH_STATISTICS is on */
void	hdump(void *);
void	hstats(void *);

#ifdef __cplusplus
}
#endif	// __cplusplus

#endif /* __DHASH_H_INCLUDED */
