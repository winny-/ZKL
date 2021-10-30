/* zklString.h : Header file for Strings
 * 
 * Copyright (c) 2006,7,8,9,10,2011 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#ifndef __ZKL_STRING_H
#define __ZKL_STRING_H

// #include "zklObject.h" implied here

#include "zklList.h"	// MLIST

#ifdef __STRING_INTERNALS

    // This struct is padded by the compiler, I account for that in string.c
    // I'd rather leave value in the struct for ease of coding.
    // I can't force the MSVC compiler to pack any tighter, it should pack
    // down to 8 bytes (by moving value up a few bytes).
typedef struct		// String
{
   Instance   instance;	// inherit from Instance, iflag == 1
   char	      value[1];	// always have a '\0', padded
} ZKL_String;	// "String", 12 bytes + strlen(text) - padding (3)

typedef struct	// short ConstString  w/ no container: save space and GC time
{
   BInstance  instance;	// inherit from BInstance, both iflags == 0
   char	     *ptr;
} ZKL_KString;	// "KonstString", 8 bytes 

typedef struct	// ConstString that points into some Instance
{
   BInstance  instance;	// inherit from BInstance, both iflags == 0
   char	     *ptr;
   Instance  *container;	// eg FcnObject that holds Code
} ZKL_KCString;	// "ConstString", 12 bytes

typedef struct	// string packed into somebody else's bucket
{
   BInstance instance;	// iflag2 == 1
   char      value[1];	// always have a '\0', padded
} CuckooString; // 8 bytes - padding --> space for 5 characters (including '\0')
	// which means 5 or 9 characters when overlaid on a K or KC String

    // works for both KString & KCString
#define ZKL_KSTRING(s)	( ((ZKL_KString *)s)->ptr )

    // Works ONLY for String, PString, K[C]String, chokes on PtrInts, etc
    // Basically a wash: flag in Instance or Object
    // --> char * (ptr to text)
#define ZKL_STRING(s)	\
   ( I_FLAG(s) ? ((ZKL_String *)s)->value : \
     ( I_FLAG2(s) ? ((CuckooString *)s)->value : ZKL_KSTRING(s) ) )

//   ( I_FLAG(s) ? ((ZKL_String *)s)->value : ZKL_KSTRING(s) )

#endif	// __STRING_INTERNALS


    // Code for this stuff is in sfmt.c
    // This struct is used to build Strings on the fly
#define ZBTEXT_BUF_SIZE	800	// > 256
typedef struct
{
   char	     buffer[ZBTEXT_BUF_SIZE + 1];	// leave space for '\0'
   size_t    bufSize;		// num chars in buffer
   Instance *string;
   MLIST(cya,2);	// a list to hold allocated data so you can fence it
} ZBText;	// 864 bytes Linix/64

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __NOT_A_DLL

extern DllExport Instance *emptyString, *Star;

#ifdef __STRING_INTERNALS
ZKL_KCString *buildKCStringTable(char *,size_t size, Instance *container,
		Byte *map, ZKL_KCString *table);
Instance *kStringInit(ZKL_KString *, char *);
#endif

void	  stringConstructPartI(void);
void	  stringConstructPartII(void);

Instance *sfmt(char *format, pArglist, pVM);	// sfmt.c

#endif	// __NOT_A_DLL

DllExport Instance *stringCreate(char *, int itype, pVM);
DllExport Instance *stringCreate2(char *,size_t len,pVM);
DllExport char	   *stringText(Instance *);
DllExport Instance *stringCat(pVM,char *s1, ...);
DllExport Instance *stringAllocate(size_t len, char **text, int itype, pVM);
DllExport Instance *stringToInt(char *, pVM);
DllExport Instance *stringToFloat(char *,pVM);
DllExport Instance *String_sub(Instance *,Instance *X,pVM);
DllExport char	   *stripWS(char *start,size_t len, size_t *sz, int whichEnd);
DllExport Instance *String_strip(Instance *,pArglist,pVM);
DllExport Instance *String_create(Instance *,pArglist,pVM);

DllExport Instance *kStringCreate(char *text,Instance *container,int itype,pVM);
DllExport Instance *kmStringCreate(char *, pVM);

DllExport Instance *zbtextInit( ZBText *,pVM);
DllExport Instance *zbtextInit2(ZBText *,Instance *text,pVM);
DllExport void	    zbtextAppend(ZBText *,char *,pVM);
DllExport void	    zbtextAppendN(ZBText *,char *,size_t,pVM);
DllExport void	    zbtextAppendI(ZBText *, Instance *, pVM);
DllExport void	    zbtextAppendX(ZBText *,pVM, char *,...);
DllExport Instance *zbtextFlush(ZBText *,pVM);
DllExport Instance *zbtextClose(ZBText *,pVM);

DllExport char	   *fmtCommaize(char *buf,int spacing,char c);

#ifdef __cplusplus
};		// end of extern "C"
#endif

#endif // __ZKL_STRING_H
