/* loader.c : Load a compiled file
 * Basically a clone of Asm.readRootClass, but with minimal memory allocation
 * 
 * Copyright (c) 2006,2007,2008-12,2013 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#include <string.h>
#ifdef _MSC_VER
   #include <io.h>		// _filelength()
#endif
#ifdef __unix__			// fstat
   #include <sys/stat.h>
#endif

#define __NOT_A_DLL
#define __FCN_INTERNALS

#include "zklObject.h"
#include "zklFcn.h"
#include "zklClass.h"
#include "zklData.h"
#include "zklList.h"

extern Byte wad[];	// wad.c or noWad.c

#define READ_WORD(buf)	   ( buf[0]<<8 | buf[1] )		// 2 bytes
#define READ_BIG_WORD(buf) ( buf[0]<<16 | buf[1]<<8 | buf[2] )	// 3 bytes
static int readBigWord(FILE *f)
{
   Byte	  buf[4];
   size_t n;
   n = fread(buf,1,3,f);
   return READ_BIG_WORD(buf);
}

static Byte *wadReadRootClass(Byte *ptr, char *fileName, char *vpath,
	pArglist arglist, Instance **cp, pVM);
static unsigned int glanceAtHeader(FILE *f, char *fileName, pVM);

    // path is 0 for don't add, 1 for use __vaultPath from class
    // If arglist != 0, run the constructor. Arglist is usually NoArglist
Instance *readRootClass(char *fileName, char *vpath, pArglist arglist, pVM vm)
{
   Instance    *klass, *data;
   FILE	       *f = searchForFile(fileName,vm);  //!!!??? why not wildcard?
   Byte	       *ptr;
   unsigned int	n;
   size_t	fileLen;
   #ifdef __unix__
      struct stat stats;
   #endif

   if (!f)
   {
      char buf[100];
      sprintf(buf,"Loader: Can't find %.80s",fileName);
      vmThrow(vm,E_NOT_FOUND,buf);
   }

   n = glanceAtHeader(f,fileName,vm);

	/* OK, we know it is a ZSC or wad file, so read the entire file into
	 * memory and process it.
	 */
   rewind(f);
   #if _MSC_VER
      fileLen = _filelength(_fileno(f));
   #elif defined(__unix__)
      if (0 == fstat(fileno(f),&stats)) fileLen = stats.st_size;
      else 				fileLen = 0;
   #endif
   data = dataCreate(fileLen,I_IMMORTAL,vm);
   ptr  = dataWillFill(data);
   dataSetSize(data,fread(ptr,1,fileLen,f));

   fclose(f);

   if (n == WAD_MAGIC_COOKIE) klass = loadWad(ptr,0,arglist,vm);
   else	 // ZSC
   {
      wadReadRootClass(ptr,fileName,vpath,arglist,&klass,vm);
#if 0
messes with building wad
      // class Bits { var bits }.bits = data; klass.container = Bits
      // then data doesn't need to be immortal
      // Vault not built yet
   //!!!??? wad is root,root,... only one root marks data, 
   // it dies and boom on the rest
      {
	 extern Instance *wadItUp(Instance *klass,Instance *bits,pVM);
	 Fence fence;
	 vmSetFence(vm,&fence,0,data);
	    fence.i1 = klass;
	    wadItUp(klass,data,vm);
	 vmRemoveFence(&fence,0);
      }
#endif
   }

   return klass;
}

static unsigned int glanceAtHeader(FILE *f, char *fileName, pVM vm)
{
   char		 buf[200], tmp[60], *ptr;
   unsigned long len, N;	// better be more than 24 bits
   size_t	 n;

   n = fread(buf,2,1,f);	// "#!" or "  "
   if (buf[0] == '#' && buf[1] == '!')	// skip over bang line
   {
      N = 100;
      while (N--)
      {
	 n = fread(buf,1,1,f);
	 if (*buf=='\n') break;
      }
      if (N == 0)	// didn't find a \n in time
      {
	 sprintf(buf,"%.80s is not a %s object file (bang line too long)!",
		 fileName,LANGUAGE_NAME);
	 vmThrow(vm,E_LOADER_ERROR,buf);
      }
   }

   N   = readBigWord(f);
   len = readBigWord(f);
   N   = (N + len) & 0xffFFff;		// N + len = 0x"zsc" or 0x1"zsc"
   if (N != ZSC_MAGIC_COOKIE && N != WAD_MAGIC_COOKIE)
   {
      sprintf(buf,"%.80s is not a %s object file! (bad header length)",
	      fileName,LANGUAGE_NAME);
      vmThrow(vm,E_LOADER_ERROR,buf);
   }

   n = fread(tmp,1,50,f);	// suck in a few bytes of the header
   switch(N)
   {
      case ZSC_MAGIC_COOKIE: ptr = ZSC_PROTOCOL; break;
      case WAD_MAGIC_COOKIE: ptr = WAD_PROTOCOL; break;
      default: ptr = "BAD"; break;	// should never get here
   }

   if (0 == strcmp(ptr,tmp)) return N;

   sprintf(buf,"%.80s has protocol mismatch (wanted %s, got %.10s)",
	      fileName,ptr,tmp);
   vmThrow(vm,E_LOADER_ERROR,buf);

   return 0;	// shut up the compiler
}

static char *pluck(char *path,char *name)
{
   char *ptr = strchr(path, '.');
   if (ptr)
   {
      int n = (int)(ptr - path);
      strncpy(name,path,n); name[n] = '\0';
      return ptr + 1;
   }
   strcpy(name,path);
   return 0;
}

    /* Get a parent class. This works because the following is true:
     *   The compiler will only inherit from an existing class, ie the class
     *      exists and I've created it.
     *   Unless it was, eg, sitting in the Vault. And now it isn't.
     * Otherwise, I could be trying to find a class that doesn't exist.
     */
static Instance *_findParent(char *classPath,Instance *rootClass, pVM vm)
{
   char      name[85], *path = classPath;
   Instance *klass;

   if (*classPath == '*')
   {
      path  = pluck(classPath,name);
      klass = rootClass;

      while(path && klass)
      {
	 path  = pluck(path,name);
	 klass = classNthClass(klass,atoi(name));
      }
   }
   else		// TheVault.Walker, Exception.IndexError
      klass = vaultChase(classPath, vm);
//      klass = vaultFind(classPath, vm);

   if (!klass)
   {
      char buf[150];
      sprintf(buf,"_findParent: Failure getting %.80s for class %s",
		classPath,iname(rootClass));
      vmThrow(vm,E_LOADER_ERROR,buf);
   }
   return klass;
}

static Byte *wadReadChunk(Byte *ptr, size_t *size)
{
   *size = READ_BIG_WORD(ptr); ptr += 3;
   return ptr;
}

static Byte *wadRead2BChunk(Byte *ptr, size_t *size)
{
   *size = READ_WORD(ptr); ptr += 2;
   return ptr;
}

static Byte *wadReadHeader(Byte *ptr, char *fileName, pVM vm)
{
   char		  buf[200], *protocol;
   Byte		 *header;
   unsigned long  len, N;	// better be more than 24 bits
   Byte		 *p;

	// header starts with "#!" or "  "
   p = ptr; ptr += 2;
   if (p[0] == '#' && p[1] == '!')	// skip over bang line
   {
      N = 100;
      while (N--) if (*ptr++=='\n') break;

      if (N == 0)	// didn't find a \n in time
      {
	 sprintf(buf,"%.80s is not a %s object file (bang line too long)!",
		 fileName,LANGUAGE_NAME);
	 vmThrow(vm,E_LOADER_ERROR,buf);
      }
   }

   N   = READ_BIG_WORD(ptr); ptr += 3;
   len = READ_BIG_WORD(ptr); ptr += 3;
   N   = (N + len) & 0xffFFff;		// N + len = 0x"zsc" or 0x1"zsc"
   if (N != 0 && N != ZSC_MAGIC_COOKIE)
   {
      sprintf(buf,"%.80s is not a %s serialized class! (bad header length)",
	      fileName,LANGUAGE_NAME);
      vmThrow(vm,E_LOADER_ERROR,buf);
   }
   header	= ptr; ptr += len;
   protocol	= (char *)header;
   p		= (Byte *)stNextString(protocol);	// Infomercial

   if (0 != strcmp(ZSC_PROTOCOL,protocol))
   {
      sprintf(buf,"%.80s has protocol mismatch (wanted %s, got %s)",
	      fileName,ZSC_PROTOCOL,protocol);
      vmThrow(vm,E_LOADER_ERROR,buf);
   }
   return ptr;
}

    // no GC in here
static Byte *wadReadCode(Byte *ptr, ZKL_Code *code, pVM vm)
{
   int	  tag;
   size_t size;

   tag = *ptr++;
   if (tag != CODE_TAG)
      vmThrow(vm,E_LOADER_ERROR,"Expecting a Code tag! Invalid wad");

   ptr		    = wadReadChunk(ptr,&size);
   code->strings    = (char *)ptr;
   code->stringSize = (uint16_t)size;
   ptr += size;

   ptr = code->map = wadRead2BChunk(ptr,&size);
   code->mapSize   = (uint16_t)size;
   ptr += size;

   code->code	  = ptr = wadReadChunk(ptr,&size); ptr += size;
   code->codeSize = (uint16_t)size;	// should be 24 bits
   code->kstrings = 0;

   return ptr;
}

static Byte *	// can GC
wadReadFcn(Byte *ptr, Instance *classInstance, Instance **fp, pVM vm)
{
   int		tag, isPrivate = 0, isStatic = 0;
   int		n,numArgs,numDefaults,protoLen;
   char	       *name;
   ZKL_Code	code, defaults[64];
   Instance    *fcn;
   size_t	size;
   StringTable	names;

   tag = *ptr++;
   if (tag != FCN_TAG)
      vmThrow(vm,E_LOADER_ERROR,"Expecting a Fcn tag! Invalid wad");

   protoLen = *ptr++;
   ptr	    = wadReadChunk(ptr,&size);
   name	    = (char *)ptr; ptr += size;

   while (*name == '+')		// + == optional stuff
   {
      if      (0 == strcmp("+P",name)) isPrivate = 1;
      else if (0 == strcmp("+S",name)) isStatic  = 1;
      name = stNextString(name);
   }
   names.n       = protoLen + 1;
   names.strings = name;
   stCalcSize(&names);

   	/* Create the function. Doesn't need to be immortal because it will
	 * added to a immortal class. A copy anyway but that will mark the
	 * static data (which is in FcnBase) and allow fcn to be reclaimed.
	 */
   ptr	   = wadReadCode(ptr,&code,vm);
   numArgs = *ptr++;

	// find first real default arg
   for (n = 0; n < numArgs && !(*ptr); n++, ptr++) ;
   numDefaults = numArgs - n;

   for (n = 0; n < numDefaults; n++)	// default args
   {
      if (*ptr++) ptr = wadReadCode(ptr,&defaults[n],vm);
      else 	// no default
	{ defaults[n].map = (Byte *)""; defaults[n].code = 0; }
   }

   fcn = fcnEmbryoWithStaticCode(&names,numArgs,numDefaults,&code,
				 defaults, I_OWNED,vm);
   FCN_IS_PRIVATE(fcn) = isPrivate;
   FCN_IS_STATIC(fcn)  = isStatic;
   if (isStatic) fcn->iflag = 1;	// and runnable

   *fp = fcn;
   return ptr;
}

static Byte *
wadReadClass(Byte *ptr, Instance *theRootClass, Instance **cp, pVM vm)
{
   int		i,tag, numVars,numFcns,numClasses,numParents;
   Instance    *klass, *rootClass=theRootClass, *parents[32]; // max 31 parents
   Byte	       *stringTable;
   char	       *attributes = 0;
   char	       *varBits[5] = { 0,0,0,0,0 };  // more than enough
   size_t	size;
   StringTable	names;

   tag = *ptr++;

   if (tag != CLASS_TAG)
      vmThrow(vm,E_LOADER_ERROR,"Expecting a Class tag! Invalid wad");

   ptr	       = wadReadChunk(ptr,&size);
   stringTable = ptr; ptr += size;
   numVars     = *ptr++;
   numFcns     = *ptr++;
   numClasses  = *ptr++;
   numParents  = *ptr++;
   ptr++;	// mystery byte

   if (numParents)	    // get the parents
   {
      char *path;
      int   n;
      ptr  = wadReadChunk(ptr,&size);
      path = (char *)ptr; ptr += size;
      for(n=0; n<numParents; n++)
      {
	 parents[n] = _findParent(path,rootClass,vm);
	 path       = stNextString(path);
      }
   }

   i = 0;
   while (*stringTable == '+')	// + == optional stuff
   {
      if (0 == strncmp("+attributes:",(char *)stringTable,12))
	 attributes = (char *)(stringTable + 12);
      if (0 == strncmp("+varBits:",(char *)stringTable,9) && (i < 5))
	 varBits[i++] = (char *)(stringTable + 9);
      stringTable = (Byte *)stNextString((char *)stringTable);
   }
   names.strings = (char *)stringTable;		// className, vaultPath, var names
   names.n       = numVars + 2;
   stCalcSize(&names);

   klass = classEmbryo(&names,
      numFcns,numClasses,numParents,parents,I_IMMORTAL,vm);

   if (attributes) classSetAttributes(klass,attributes,vm);
   for (i = 0; varBits[i]; i++) classSetVarBits(klass,varBits[i],i);

   for (i = 0; i < numFcns; i++)
   {
      Instance *fcn;
      ptr = wadReadFcn(ptr,klass,&fcn,vm);
      classAddFcnN(klass,fcn,i,vm);
   }

   	// first read class is the root
   rootClass = theRootClass ? theRootClass : klass;

	// get the contained classes
   while(numClasses--)
   {
      Instance *c;
      ptr = wadReadClass(ptr,rootClass,&c,vm);
      classAddClass(klass,c,vm);
   }

   *cp = klass;
   return ptr;
}

    /* path is 0 for don't add, 1 to use class.vaultPath, string for
     * a path itself,
     */
static Byte *
wadReadRootClass(Byte *ptr, char *fileName, char *vpath, pArglist arglist,
		 Instance **cp, pVM vm)
{
   Instance *klass;

   ptr = wadReadHeader(ptr,fileName,vm);
   ptr = wadReadClass(ptr,0,&klass,vm);

   classCookClass(klass,vm);

   if (vpath)
   {
      vaultAdd(vpath,klass,vm);
      cacheExceptions();	// oh well, gotta do it somewhere
   }

   if (arglist) classRunConstructor(klass,arglist,vm);
//!!!fence

   *cp = klass;
   return ptr;
}

static Instance * _runNgun(Instance *classList, int startAt, char *runList, pArglist, pVM);

    // Returns: last class in wad or package main
//!!!??? if NoVM, create VM?
Instance *loadWad(Byte *ptr, Byte **eptr, pArglist arglist, pVM vm)
{
   unsigned long  len, N;	// better be more than 24 bits
   int		  numZSCs;
   Byte		 *zscStart;
   char          *runList;
   Instance	 *rootClass = 0;
   Instance	 *classList, *packageMain = 0;
   Fence	  fence;

   if (!ptr) ptr = wad;
   if (*ptr == 0) return 0;

   ptr += 2;		// "  " for compatbility with ZSC

   N   = READ_BIG_WORD(ptr); ptr += 3;
   len = READ_BIG_WORD(ptr); ptr += 3;
   N   = (N + len) & 0xffFFff;		// N + len = 0x"wad" or 0x1"wad"
   if (N != 0 && N != WAD_MAGIC_COOKIE)
      vmThrow(vm,E_LOADER_ERROR,"Not a wad! (bad header length)");

   zscStart = ptr + len;
   if (0 != strcmp((char *)ptr,WAD_PROTOCOL))
   {
      char buf[100];
      sprintf(buf,"Wad has protocol mismatch (wanted %s, got %s)",
	      WAD_PROTOCOL,ptr);
      vmThrow(vm,E_LOADER_ERROR,buf);
   }

   if (!arglist) arglist = NoArglist;

   ptr      = (Byte *)stNextString((char *)ptr);// Infomercial
   ptr      = (Byte *)stNextString((char *)ptr);// Time stamp
   ptr      = (Byte *)stNextString((char *)ptr);// num ZSCs
   numZSCs  = atoi((char *)ptr);
   ptr      = (Byte *)stNextString((char *)ptr);// dataSize
   runList  = 				// Constructors to run: "00010010101"
	      stNextString((char *)ptr);
// flags    = stNextString(ptr);	// reserved
// dir      = stNextString(ptr);	// name:offset:size

   	// It can be "wasteful" to queue classes that won't be constructed
	// but the number is going to be small so who cares? Besides, if the
	// list has to grow, you lose any advantage by trying to be clever.
   classList = tupleCreate(numZSCs,I_OWNED,vm);
   vmSetFence(vm,&fence,0,classList);
   {
      int   package = (strchr(runList,'5') != 0);	   // 1 or 0
      char *addToVault = (char *)(size_t)!package; // don't add package contents
      int   z1 = 0, zs = 0;
      for (ptr = zscStart; numZSCs--; zs++)
      {
	 if (*ptr == DATA_TAG)	// a bag-O-bits, run == 0
	 {
	    char     *name;
	    int	      N;

	    name = (char *)++ptr;
	    ptr  = (Byte *)stNextString((char *)ptr);
	    N    = READ_BIG_WORD(ptr);
	    ptr += 3;
	    rootClass = kdataCreate(ptr,N,0,I_OWNED,vm);
	    if (addToVault) vaultAddData(name,rootClass,vm);
	    ptr += N;
	    // Packge: Data will be added to __wadList
	 }
	 else if (*ptr == WAD_TAG)	// nested Wad, run == 0
	 {
	    // run constructors, stuff Vault on classes read so far in case
	    // nested Wad has dependencies
	    _runNgun(classList,z1,runList,arglist,vm); z1 = zs+1;
	    // WAD_TAG, 3 bytes len
	    rootClass = loadWad(ptr+4,&ptr,NoArglist,vm);
	 }
	 else			// a Class
	    ptr = wadReadRootClass(ptr,"<wad>",addToVault,0,&rootClass,vm);

	 // stuff everything read into classList so it tracks runList
	 tupleAppend(classList,rootClass);
      }
      packageMain = _runNgun(classList,z1,runList,arglist,vm);
   }
   vmRemoveFence(&fence,0);

   if (eptr) *eptr = ptr;
   if (packageMain) return packageMain;
   return rootClass;	// !!!???? classList?
}

static Instance *_runNgun(
Instance *classList, int startAt, char *runList, pArglist arglist, pVM vm)
{
   Instance *i, *packageMain = 0;
   runList += startAt;
   for (; (i = listGet(classList,startAt++)); )	// run list
   {
      Instance *runConstructor;
      int       addToVault = 0, run = *runList++;

//      if (i == Void) continue;
      if (run > '9'){ addToVault = 1; run -= 0x10; }  // for packages
      switch (run)
      {
	 default:  runConstructor = 0;		break;  // eg Data, script
	 case '1': runConstructor = NoArglist;  break;
	 case '2': runConstructor = arglist;    break;  // script, !!!nuke main is script

	 // if package, stuff entire class list into __wadList
	 case '5':			// package main: __constructor(args)
	    packageMain = i;
	    // fall through		// sets __wadList maybe
	 case '3':			// package: no construction
	    classFindVar(i,"__wadList",classList,vm); // set __wadList if exists
	    runConstructor = 0;
	    break;
	 case '4':			// package: __constructor()
	    classFindVar(i,"__wadList",classList,vm); // set __wadList if exists
	    runConstructor = NoArglist;
	    break;
      }
      if (runConstructor) classRunConstructor(i,runConstructor,vm);
      if (addToVault)     vaultAdd((char *)2,i,vm);
   }
   // need to construct packageMain to wadList into vars
   if (packageMain) classRunConstructor(packageMain,arglist,vm);
   return packageMain;
}
