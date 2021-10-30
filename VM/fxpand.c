/* fxpand.c : fxpand(): expand file names
 * Input:
 *   name:  A file name which can contain: ?, \, [], [^] and *
 *     If name ends with "." (eg *.) then only names with no extensions will
 *       be matched.
 *     If name begins with ~ then:  "~/" expands to "$(HOME)/" and "~name"
 *	 expands to "<home directory of name>".
 *     MS-DOS:  \ is same as /.  Does not quote.
 *     UNIX: \ quotes the next character.
 *     ATARI (from jwahar r.  bammi (bammi@cadence.com)):
 *       Like MS-DOS, \ == /, except that we have the POSIX opendir() etc.
 *     if FXP_GLOB_LIST, name is a list of globs: nm\0nm\0nm\0\0
 *        Zero terminated.
 *        The first glob is name (as above), the rest of the globs are globs
 *          to match the file name (no path). Big OR.
 *          eg "*\0*mp3\0*wma\0*ogg\0\0"
 *   onlyone: If 1, return the first match otherwise all matches are
 *     returned.  For example, this controls how "*" is expanded.
 *   nopath:  If 1, only return the matching file names.  Otherwise
 *     return the path and name.  For example, "/foo/bar" --> "bar".
 *   slash_dir:  If 1, append a slash to the end of a file name if the
 *     file is a directory.  For example, input of "dir" would generate
 *     "dir/".
 *   flags:
 *     FXP_ONLY_ONE (1):  If 1, return the first match otherwise all matches
 *	 are returned.  For example, this controls how "*" is expanded.
 *     FXP_NO_PATH (2):  If 1, only return the matching file names.
 *	 Otherwise return the path and name.  For example, "/foo/bar" -->
 *	 "bar".
 *     FXP_NO_TRAILING_SLASH (4):  If 1, DON'T append a slash to the end of
 *	  a file name if the file is a directory.  For example, input of
 *	  "dir" normally generates "dir/", but if this flag is set, the
 *	  result is "dir".
 *     FXP_NO_DIRS (8): Don't match directories.
 *     FXP_ONLY_DIRS (0x10): Only match directories.
 *     FXP_IGNORE_CASE (0x20): Unix only. Ignore case when matching names
 *     FXP_DF (0x40): File name (last part of path) doesn't need to exist.
 *	  Usefull for things like expanding ~/foo to a real file name you
 *	  can open. No wildcards in name. Also works for directories.
 *   processFname:  A function which is passed the expanded file name and a
 *       pointer a mystry object (which you pass in).
 *     Returns:  0 if all OK, >1 an error to be returned by fxpand()
 *		 >1 can be used to as proxy for FXP_ONLY_ONE.
 * Returns:
 *   0:  All OK
 *   1:  Something screwed up.  Most likely the name is bad.
 *   n:  A code returned by processFname().
 * 
 * Notes:
 *   When compiled on Apollo, this routine also works with the Domain/OS
 *     "//" notation.  This is mostly luck - I don't collapse "/"s and a
 *     relaxed check lets this work.
 *   Input error checking is pretty grim.
 * Unix Notes:
 *   When wildcard matching, hidden files (those that start with ".")  are
 *     skipped unless you ask to see them.  To match ".fred", you could use
 *     ".f*".  To match "fred/.sam/geo", you would need something like
 *     "fred/.s* /g*".
 *   When appending slashes (slash_dir), expanding something like "*" can be
 *     very slow.  This is because I have to stat() to find out if the file
 *     is a directory and stat() can take a long time to stat files over nfs
 *     mounts, follow links, etc.
 * 
 * C Durland  craigd@zenkinetic.com
 */

/* 1989, 1990, 1991 Public Domain
 *   Distributed "as is", without warranties of any kind, but comments,
 *     suggestions and bug reports are welcome.
 * Modified 2006 for zkl, which probably broke everything but the Windows
 *   and Linux/Unix code.
 */


#ifdef _MSC_VER
   #define WINDOZ
   #define _CRT_SECURE_NO_WARNINGS		// VC++ 2008
#endif

#ifdef __hpux		/* for ANSI C on HP-UX */
   #define _HPUX_SOURCE
#endif  /* __hpux */

#ifdef __apollo			/* for ANSI C on Apollo BSD */
   #define _BSD_SOURCE
#endif	/* __apollo */

#if defined(__unix__)
   #define _POSIX_SOURCE
   #define _POSIX_C_SOURCE 200112L	// FreeBSD lstat & readdir_r
   #define __USE_BSD	// GCC: for DT* & lstat
#endif	// __unix__: Linux, FreeBSD, minix


#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "zklUtil.h"	// for protos

//#ifndef FXP_ONLY_ONE	// in case these are in a header file
   typedef int (*fxpfi)(char *, void *);

   #define FXP_ONLY_ONE		    0x1
   #define FXP_NO_PATH		    0x2
   #define FXP_NO_TRAILING_SLASH    0x4
   #define FXP_NO_DIRS		    0x8
   #define FXP_ONLY_DIRS	   0x10
   #define FXP_IGNORE_CASE	   0x20
   #define FXP_DF		   0x40

   #define FXP_USER_MASK	 0xFFFF	// valid user flags, YOU mask

   #define FXP_GLOB_LIST	0x10000	// nm is a list: nm\0nm\0nm\0\0
//#endif

//#define FXP_MASK	        0xFF	// Windows CYA
//#define HAS_DRIVE	       0x100	// added to FXP_ flags, Windows only


static int getDpart(char **start, char *word, int *eval);

#if 0
static char    *name_list;
static int	prepend_blank;

static int stuff_name(name) char *name;
{
  if (prepend_blank) strcat(name_list," ");
  else prepend_blank = 1;

  strcat(name_list,name);
  return 0;
}
#endif


/* ******************************************************************** */
/* ***************************** Windows ****************************** */
/* ******************************************************************** */
#ifdef WINDOZ

static int procz(fxpfi,void *, int flags,int isDir, char *path,size_t path_len);

#define SLASH	"/"		/* "/" or "\\", your preference */

    /* A note about MS-DOS:  Its file find routines are brain dead:  If you
     *   ask for a directory, you will get all file types.  Also, since
     *   there is a file type==0, you can't just use AND to filter out
     *   unwanted types.  What a pain in the butt!
     */

#include <dos.h>

#ifdef LATTICE		/* from dos.h */

typedef struct FILEINFO File_Info;	/* in dos.h */

#define FIND_DIRECTORY(info, name, attr)	dfind(info, name, attr)
#define NEXT_DIRECTORY(info)			dnext(info)

#define ATTR		attr
#define NAME		name

#elif _MSC_VER		/* MS Visual C++ */

#else		/* JPI Topspeed C, Turbo C, hopefully others */

#include <dir.h>

typedef struct ffblk File_Info;		/* in dos.h */

#define FIND_DIRECTORY(info, name, attr)	findfirst(name, info, attr)
#define NEXT_DIRECTORY(info)			findnext(info)

#define ATTR		ff_attrib
#define NAME		ff_name

#endif /* LATTICE */

#if _MSC_VER		/* MS Visual C++ */

#include <direct.h>
#include <io.h>
#include <sys/types.h>
#include <sys/stat.h>

/* _finddata_t.attrb (struct for _findnext()):
 *    _A_NORMAL	 0x00
 *    _A_RDONLY	 0x01
 *    _A_HIDDEN	 0x02
 *    _A_SYSTEM	 0x04
 *    _A_SUBDIR	 0x10	Subdirectory
 *    _A_ARCH	 0x20	Archive
 */
#define FATTR (_A_ARCH | _A_RDONLY | _A_SUBDIR)		// file attributes

int fxpand(char *name, int flags, fxpfi processFname, void *X)
{
   char   path[_MAX_PATH], *ptr, *qtr, *globList = 0;
   int    atEnd, eval, s, hasDrive = 0;
   int    onlyone = flags & FXP_ONLY_ONE, recursed = 0;
   size_t path_len;

   if(flags & FXP_GLOB_LIST)	// name is a list of strings
      globList = strchr(name,'\0') + 1;

#if 0
   if (name_heap)	/* store the found file names in name_heap */
   {
      processFname	= stuff_name;
      name_heap[0]	= '\0';
      name_list		= name_heap;
      prepend_blank	= 0;
   }
#endif

   *path = '\0';
   if (*name == '~')
   {
      name++;
      if (ptr = getenv("HOMEDRIVE")) strcpy(path,ptr);
      if (ptr = getenv("HOMEPATH"))  strcat(path,ptr);
      for(ptr = path; *ptr; ptr++) if (*ptr == '\\') *ptr = '/';
   }
   else if (name[1] == ':')
   {
      strncpy(path,name,2);
      path[2] = '\0';
      name += 2;
      hasDrive = 1;
   }

   atEnd = 0;
   while(!atEnd && !recursed)
   {
      char 	  word[_MAX_PATH];
      struct stat statBuf;

      atEnd    = getDpart(&name,word,&eval);
      path_len = strlen(path);
      if (eval)		// wildcards in need of expansion
      {
	 struct _finddata_t	de;
	 intptr_t		handle;
	 int			found = 0, needAdot = 0;

if (word[strlen(word)-1] == '.') needAdot = 1;
	 ptr = path + path_len;
	 strcpy(ptr,"*.*");

	 if (!atEnd && !*name)		// "foo*/" --> all dirs
	 {
	     flags |= FXP_ONLY_DIRS;
	     atEnd = 1;
	 }

	 handle = _findfirst(path,&de);
	 if (-1 == handle) return 1;	// No files found
	 *ptr = '\0';		// get rid of "*.*"
	 do	// look at all entries in this directory
	 {
	    char tbuf[_MAX_PATH];

	    if (!atEnd)		// only care about directories
	       { if (!(de.attrib & _A_SUBDIR)) continue; }
            else if (de.attrib &&    // de.attrib==0 --> a regular old file
		   (de.attrib & FATTR) == 0) continue;

	    ptr = qtr = de.name;
/*lowercase(ptr);	/* death on roller skates?!!!? */
if (needAdot && !strchr(ptr,'.')) ptr = strcat(strcpy(tbuf,ptr),".");
	    if (*qtr != '.' && wildmat(ptr,word,1))  // ignore . & .. 
	    {
	       if(globList)	// good if match any in globList
	       {
		  char *glob;
		  for(glob=globList; *glob; glob = strchr(glob,'\0') + 1)
		     if(wildmat(ptr,glob,1)) break;
		  if(!*glob) continue;	// no match
	       }
	       if (!atEnd)	// something like foo/<you are here>/bar
	       {
		  char poof[_MAX_PATH];

		  strcat(path,qtr); strcat(path,SLASH);

		  recursed++;
		  strcpy(poof,path); strcat(poof,name);
		  fxpand(poof,flags,processFname,X);
		  path[path_len] = '\0';	// next please
	       }
	       else	// something like foo/<you are here>\0
	       {
		  found = 1;
		  strcpy(path + path_len, qtr);
		  s = procz(processFname,X, flags,(de.attrib & _A_SUBDIR),
				  path,path_len);
		  path[path_len] = '\0';
		  if (s)
		  {
		     _findclose(handle);
		     return s;
		  }
		  if (onlyone) break;
	       }
	    }
	 } while (!_findnext(handle,&de));
	 _findclose(handle);
	 if (!found) return 1;
      }
      else // don't eval
      {
	 strcpy(path + path_len, word);
	 if (atEnd)	// all done
	 {
	    char   c;
	    int    isDir, n = (int)strlen(path) - 1;

	    if (n < 0) return 1;	// ""
	    	// "/" --> "/" (always), "foo/" --> "foo"
	    c = path[n];
	    if (c == '/' || c == '\\')	// trailing slash
	    {
		       // "/" or "C:/"?  What a PITA
	       if (n == 0 || (n == 2 && hasDrive))
		  return procz(processFname,X,flags | FXP_NO_TRAILING_SLASH,
			       1,path,0);
	       path[n] = '\0';
	       path_len = n - 1;	// since trailing slash == dir
	    }
	    if (stat(path,&statBuf)) return 1;	// path not found
	    isDir = (statBuf.st_mode & _S_IFDIR);
	    return procz(processFname,X, flags,isDir,path,path_len);
	 }
	 strcat(path,SLASH);
      } 
   } // while
   return 0;
}


    /* Notes:
     *   For some (unknown to me) reason, FA_DIRECTORY matches everything,
     *     not just directories.
     *   Not used by this code anymore but I call it in file.c (zkl)
     * WARNING:
     *   This routine changes the DTA.  If you are in the middle of a
     *     FIND_DIRECTORY/NEXT_DIRECTORY loop, this will mess that up.
     */
int is_dir(char *path, struct stat *psbuf)
{
#if 0
   struct _finddata_t	de;
   intptr_t		handle;
   int			s;

   s = 0;
   handle = _findfirst(path,&de);
   if (-1 != handle)
   {
      s = (de.attrib & _A_SUBDIR);
   }
   _findclose(handle);
   return s;
#else
   struct stat sbuf;

   if (!psbuf)
   {
   	// trailing "/" on directory [only] name is an error (ENOENT)
        // unless (of course), it is "/"
      if (stat(path,&sbuf)) return 0;  // -1 (errno==ENOENT -> can't find file)
      psbuf = &sbuf;
   }
	// make sure it's a directory
   return (psbuf->st_mode & _S_IFDIR);
#endif
}

#else	/* not _MSC_VER - the other MS-DOS compilers */

#define FATTR (FA_ARCHIVE | FA_RD_ONLY | FA_DIRECTORY)	/* file attributes */

int fxpand(char *name, int flags, fxpfi processFname, void *X)
{
  char path[128], word[100], *ptr, *qtr, tbuf[128];
  File_Info de;
  int atEnd, eval, path_len, s;
  int  onlyone = flags & FXP_ONLY_ONE;

#if 0
  if (name_heap)	/* store the found file names in name_heap */
  {
    processFname = stuff_name;
    *(name_list = name_heap) = '\0';
    prepend_blank = 0;
  }
#endif

  *path = '\0';
  if (*name == '~')
  {
    name++;
    if (ptr = getenv("HOME")) strcpy(path,ptr);
  }
  else
    if (name[1] == ':')
	{ strncpy(path,name,2); path[2] = '\0'; name += 2; }


  atEnd = 0;
  while (!atEnd)
  {
    atEnd = getDpart(&name,word,&eval);
    path_len = strlen(path);
uppercase(word);	/* Since the directory entries are uppercase */
    if (eval)			/* wildcards in need of expansion */
    {
       int found = 0, needAdot = 0;

if (word[strlen(word)-1] == '.') needAdot = 1;
      ptr = path +path_len; strcpy(ptr,"*.*");

      if (FIND_DIRECTORY(&de,path,FATTR)) return 1;	/* No files found */
      *ptr = '\0';		/* get rid of "*.*" */
      do		/* look at all entries in this directory */
      {
	if (!atEnd)			/* only care about directories */
	  { if (de.ATTR != FA_DIRECTORY) continue; }
        else
	  if (de.ATTR &&	/* if de.ATTR==0, it's a regular old file */
	      (de.ATTR & FATTR) == 0) continue;

	ptr = qtr = de.NAME;
if (needAdot && !strchr(ptr,'.')) ptr = strcat(strcpy(tbuf,ptr),".");
	if (*qtr != '.' && wildmat(ptr,word,1))	/* ignore . & .. */
	{
	  found = 1;
	  if (!atEnd)		/* something like foo/<you are here>/bar */
	  {
	    strcat(strcat(path,qtr), SLASH);
	    break;
	  }
	  else			/* something like foo/<you are here> */
	  {
	    strcpy(path +path_len, qtr);
	    s = procz(processFname,X,flags, (de.ATTR == FA_DIRECTORY),
			path, path_len);
	    path[path_len] = '\0';
	    if (s) return s;
	    if (onlyone) break;
	  }
	}
      } while (!NEXT_DIRECTORY(&de));
      if (!found) return 1;
    }
    else
    {
      strcpy(path + path_len, word);
      if (atEnd)	/* all done */
I need to fix this;
	return procz(processFname,X, flags,666,path,path_len);
/* ??? if (!is_dir(path)) return 1; /* Make sure path is real */
    } 
  } // while
  return 0;
}

    /* 
     * Notes:
     *   For some (unknown to me) reason, FA_DIRECTORY matches everything,
     *     not just directories.
     * WARNING:
     *   This routine changes the DTA.  If you are in the middle of a
     *     FIND_DIRECTORY/NEXT_DIRECTORY loop, this will mess that up.
     */
int is_dir(char *path)
{
  File_Info de;

  return (!FIND_DIRECTORY(&de,path,FA_DIRECTORY) && (de.ATTR == FA_DIRECTORY));
}

#endif	/* _MSC_VER */


    /* Input:
     *   processFname:  pointer to function to call
     *   path:  Full path name
     *   pathLen:  offset of file name in path.  Used if don't want to pass
     *     the path to processFname.
     * Returns:
     *   Whatever processFname returns.
     */
static int procz(
   fxpfi processFname,void *X,int flags,int isDir, char *path,size_t pathLen)
{
   if (isDir)
   {
      if (flags & FXP_NO_DIRS) return 0;
      if (!(flags & FXP_NO_TRAILING_SLASH))
      {
	 size_t n = strlen(path);
	 if (n == 0 || path[n-1] != '/') strcpy(path+n,"/");
      }
   }
   else if (flags & FXP_ONLY_DIRS) return 0;
   if (flags & FXP_NO_PATH) path += pathLen;
   return (*processFname)(path,X);
}

	/* MS-DOS stuff for getDpart() */
#define ASLASH		case '/': case '\\'
#define GOTTA_EVAL	case '?': case '[': case '*'
#define SPAZ		1

#endif	/* WINDOZ */

/* ******************************************************************** */
/* ************************ Unix, Linux, Atari ************************ */
/* ******************************************************************** */

#if ATARI    /* Atari has Posix opendir(), etc */
   #undef  POSIX_OS
   #define POSIX_OS 1	/* turn on POSIX and __unix__ */
#endif	/* ATARI */


#ifdef __unix__	// and Linux, BSD, minix, Atari

#define BADGETPW 0	/* 1 if system getpw* routines are screwed up */

#include <limits.h>	// BSD: NAME_MAX
#include <sys/types.h>
#include <pwd.h>
#include <sys/stat.h>

	/* Posix, SysV: HP-UX, Apollo SysV, DEC, Atari  */
	/* defined(POSIX) is a DECism */
#if POSIX_OS || SYSV_OS || defined(POSIX) || defined(__FreeBSD__) || defined(__minix__)
   #include <dirent.h>
#else		/* Linux, Pure BSD: Apollo bsd4.3 */
   #include <sys/dir.h>
#endif

#if defined(__FreeBSD__) || defined(__minix__)
   #include <unistd.h>
#endif

static int procz(fxpfi, void *, int flags, char *path, size_t path_len);


#define _MAX_PATH 512

static int strcpyS(char *dst,int offset, char *src,int maxSrc)
{
   if (offset + strlen(src) >= maxSrc) return 0;
   strcpy(dst+offset,src);
   return 1;
}

    /* cases to check:
     *   "~", "~fred", "/", "~/", ""
     * Beware of recursive links: /usr/share/recovery-mode -->
     *    /lib/recovery-mode points to itself
     */
int fxpand(char *name, int flags, fxpfi processFname, void *X)
{
   char path[_MAX_PATH], word[NAME_MAX], *ptr, *qtr, *globList = 0;;
   DIR *dir;
   #if POSIX_OS || SYSV_OS || defined(POSIX) || defined(__FreeBSD__) || defined(__minix__)
      struct dirent *dtr, entry;
   #else		// Apollo bsd4.3, (some)DEC
      struct direct *dtr, entry;
   #endif
   int
      atEnd, eval, s, onlyone = flags & FXP_ONLY_ONE, recursed = 0,
      skip_dot_files,		// ignore names starting with "."
      ignore_case = (flags & FXP_IGNORE_CASE) != 0;	// 1 | 0
   size_t path_len;
   struct passwd *pd;

   if(flags & FXP_GLOB_LIST)	// name is a list of strings
      globList = strchr(name,'\0') + 1;

#if 0
   if (name_heap)	/* store the found file names in name_heap */
   {
      processFname = stuff_name;
      *(name_list = name_heap) = '\0';
      prepend_blank = 0;
   }
#endif

   *path = '\0';
   if (*name == '~')		/* csh/ksh home directory expansion */
   {
      name++;
      if (*name == '/' || *name == '\0')	/* ~/foo/bar or ~ */
      {

	 if ((ptr = getenv("HOME"))) strcpy(path,ptr);
	 else		/* no $HOME, see if the OS knows */
	 {
#if BADGETPW
	    return 1;	/* !!! a sleeze */
#else
	    if ((pd = getpwuid(getuid())) == NULL) return 1;
	    strcpy(path,pd->pw_dir);
#endif	/* BADGETPW */
	 }
      }
      else			/* ~fred --> user freds' home directory */
      {
	 atEnd = getDpart(&name,word,&eval);
	 if (eval) return 1;	    // no wildcards allowed in user name
	 name--;
	 if (!atEnd) word[strlen(word)-1] = '\0'; // remove "/" from  "~fred/"
#if BADGETPW
	 if (!getpwhome(word)) return 1;
	 strcpy(path,word);
#else
	 if ((pd = getpwnam(word)) == NULL) return 1;
	 strcpy(path,pd->pw_dir);
#endif	/* BADGETPW */
      }
   } // ~

	// at this point, maybe: strlen(path)!=0 && strlen(name)==0
   atEnd = 0;
   while(!atEnd && !recursed)
   {
      atEnd = getDpart(&name,word,&eval);
      skip_dot_files = (*word != '.');	/* ".fred" means look at dot files */
      path_len = strlen(path);
      if (eval)		// wildcards in need of expansion
      {
	 int found = 0, needAdot = 0;

	 if (!atEnd && !*name)		// "foo*/" --> all dirs
	 {
	     flags |= FXP_ONLY_DIRS;
	     atEnd = 1;
	 }

if (word[strlen(word)-1] == '.') needAdot = 1;
	 if ((dir = opendir(path_len == 0 ? "." : path)) == NULL) return 1;
	 while(1)	// look at all entries in this directory
	 {
	    char tbuf[_MAX_PATH];

//	    if ((dtr = readdir(dir)) == NULL) break;
	    readdir_r(dir,&entry,&dtr);	// check for errors!!!
	    if (!dtr) break;		// done
//if (dtr->d_type == DT_LNK) continue;	// don't do sym links
//DT_DIR, DT_REG
	    ptr = qtr = dtr->d_name;
	    if (skip_dot_files && *ptr == '.') continue;
if (needAdot && !strchr(ptr,'.')) ptr = strcat(strcpy(tbuf,ptr),".");

	    if (wildmat(ptr,word,ignore_case))
	    {
	       if(globList)	// good if match any in globList
	       {
		  char *glob;
		  for(glob=globList; *glob; glob = strchr(glob,'\0') + 1)
		     if(wildmat(ptr,glob,ignore_case)) break;
		  if(!*glob) continue;	// no match
	       }
	       if (!atEnd)	// something like foo/<you are here>/bar
	       {
		  char poof[1000];

		  strcpy(path+path_len,qtr);
		  recursed++;		// "*/*/foo"
		  strcpy(poof,path); strcat(poof,"/"); strcat(poof,name);
		  fxpand(poof,flags,processFname,X);
		  path[path_len] = '\0';	// next please

			// make sure it's a real directory
//		  if (is_dir(path)) { strcat(path,"/"); found = 1; break; }
	       }
	       else		// something like foo/<you are here>\0
	       {
		  found = 1;
		  strcpy(path+path_len, qtr);
		  s = procz(processFname,X,flags, path,path_len);
		  path[path_len] = '\0';
		  if (s) { closedir(dir); return s; }
		  if (onlyone) break;
	       }
	    }
	 } // while(1)
	 closedir(dir);
	 if (!found) return 1;
      }
      else	// No wildcards: something like: .../bar/... or .../bar
      {		// word may == "" (for input like "~fred")
//	 strcpy(path + path_len, word);
//!!!how to detect cycle? don't follow sym links to symlinks?
	 if (!strcpyS(path,path_len, word,_MAX_PATH)) continue; //!!!???
	 if (atEnd)	// all done
	 {
	    char   c;
	    int    n = (int)strlen(path) - 1;
	    struct stat sbuf;

	    if (n < 0) return 1;	// ""
	    	// "/" --> "/", "foo/" --> "foo"
	    c = path[n];
	    if (c == '/')	// trailing slash
	    {
	       if (n == 0)	// "/", what a PITA
		  return procz(processFname,X, flags | FXP_NO_TRAILING_SLASH,
			       path,0);
	       path[n] = '\0';
	    }
#if 1
	    if (!(FXP_DF & flags) && stat(path,&sbuf))
	       return 1;	// file not found
#else
	    if (stat(path,&sbuf)) return 1;	// path not found
#endif
	    return procz(processFname,X, flags, path,path_len);
	 }
      }
   } // while
   return 0;
}

int is_dir(char *path, struct stat *psbuf)
{
   struct stat sbuf;

   if (!psbuf)
   {
      // don't follow sym lnks
      if (lstat(path,&sbuf)) return 0;	// doesn't exist
      psbuf = &sbuf;
   }
	// make sure it's a directory
   #ifdef S_ISDIR
      return S_ISDIR(psbuf->st_mode);
   #else
      return ((psbuf->st_mode & 0170000) == 040000);
   #endif	/* S_ISDIR */
}

    /* Input:
     *   processFname:  pointer to function to call
     *   path:  Full path name
     *   pathLen:  offset of file name in path.  Used if don't want to pass
     *     the path to processFname.
     * Returns:
     *   Whatever processFname returns.
     * Notes:
     *   Need to check path[pathLen] before appending a slash because:  if
     *     don't want a path (pathLen != 0) and no name (eg expanding "~"),
     *     would processFname("/") which is not what is expected.
     */
static int
procz(fxpfi processFname,void *X, int flags, char *path,size_t pathLen)
{
	// gotta check if directory?
   if ((flags & (FXP_NO_DIRS | FXP_ONLY_DIRS)) ||
       !(flags & FXP_NO_TRAILING_SLASH))
   {
      if (is_dir(path,0))
      {
	 size_t n = strlen(path);
	 if (flags & FXP_NO_DIRS) return 0;
	 if (!(flags & FXP_NO_TRAILING_SLASH) && (n && path[n-1]))
	    strcat(path,"/");
      }
      else if (flags & FXP_ONLY_DIRS) return 0;
   }
   if (flags & FXP_NO_PATH) path += pathLen;
   return (*processFname)(path,X);
}


#if BADGETPW
	/* Get the home directory out of the password file.
	 * Only use this if the system getpw... routines are
	 *   screwed up.
	 */
static int getpwhome(char *name)
{
  char buf[256], *ptr;
  FILE *fptr;
  int n, s = 0;

  if ((fptr = fopen("/etc/passwd","r")) == NULL) return 0;
  while (fgets(buf,255,fptr))
  {
    for (ptr = buf; *ptr != ':'; ptr++) ; *ptr = '\0';
    if (strcmp(name,buf)) continue;
    for (n = 0; n < 4; ptr++) if (*ptr == ':') n++;
    while (*ptr != ':') *name++ = *ptr++;   *name = '\0';
    s = 1;
    break;
  }
  fclose(fptr);
  return s;
}
#endif	/* BADGETPW */

#if ATARI	/* Atari stuff for getDpart(), same as MS-DOS */
   #define ASLASH	case '/': case '\\'
   #define GOTTA_EVAL	case '?': case '[': case '*'
#else
	/* UNIX stuff for getDpart() */
   #define ASLASH	case '/'
//!!! this is shit: "tm\P" shouldn't match Tmp but \ is a glob char
//!!! windows: fuck 'em, file names don't have specials, \* is doo-doo
   #define GOTTA_EVAL	case '?': case '[': case '*' : case '\\'
#endif	/* ATARI */

#define SPAZ		0

#endif	/* __unix__ || ATARI */



////////////////////////////////////////////////// Everybody


   /* Get the next part of the filename (ie the stuff between "/"s).  The
    *   parts of "\foo\*.c" are: "/", "foo", "*.c".
    * Input:
    * Output:
    *   eval:  1 if part contains wildcards needing expansion.
    *   word:  The part.
    *   start: Points after the part (after the "/" or '\0').
    * Returns:
    *   1: Hit the end of the filename else 0.
    */
static int getDpart(char **start, char *word, int *eval)
{
   char *ptr   = *start;
   int   atEnd = 1;

   *eval = 0;
   while(1)
   {
      switch (*word++ = *ptr++)
      {
	 ASLASH:	// macro expands to "case ..."
	    if (*eval || SPAZ) word--;	// remove trailing "/"
	    while(1)			// get rid of multiple slashes
	       switch(*ptr)
	       {
		  default: 
		  case '\0': goto pop;
		  ASLASH:    ptr++; break;
	       }
	 pop: ;
	    atEnd = 0;
	    // fall through
	 case '\0': *word = '\0';
	    *start = ptr;
	    return atEnd;
	 GOTTA_EVAL: *eval = 1; break;	// glob wild cards, wildmat(): eg: ? [ *
      }
   }
}


#ifdef TEST
/* **************** TEST ******************************************** */

//#include <const.h>
#include "dTable.h"
declare_and_init_dTable(ntable,char *);

extern char *savestr();

add_to_table(void *X, char *name)
{
  static int n = 0;

  xpand_dTable(&ntable, 1, 50, 25);
  ntable.table[n++] = savestr(name);
  return 0;
}

char name_heap[3000];
main()
{
  char buf[80];
  int j, nopath,flags = 0;

  printf("No path? "); gets_s(buf,80); nopath = atoi(buf);
  if (nopath) flags |= FXP_NO_PATH;

  printf(": "); gets_s(buf80);
  if (fxpand(buf,flags,add_to_table,0)) puts("blew up");
  else
  {
     for (j = 0; j < sizeof_dTable(&ntable); j++)
	printf("table[%d]: %s\n",j,ntable.table[j]);
  }
}
#endif
