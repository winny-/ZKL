/* file.c : The File object
 * 
 * Copyright (c) 2006,2007,2008-15,2016 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#if defined(__unix__)
   #include <utime.h>		// utime()
   #include <sys/file.h>	// open
#else	// MSVC anyway
   #include <sys/utime.h>	// utime()
   #include <direct.h>		// _mkdir()
   #include <io.h>		// _mktemp_s()
#endif

#ifdef _MSC_VER
   #define fileno	 _fileno
   #define popen	 _popen
   #define pclose	 _pclose
   #define S_ISREG(mode) ( (mode) & _S_IFREG )
#endif

#define __NOT_A_DLL
#define __STRING_INTERNALS

#include "zklObject.h"
#include "zklData.h"
#include "zklFcn.h"
#include "zklList.h"
#include "zklNumber.h"
#include "zklString.h"
#include "zklUtil.h"

extern int is_dir(char *path,struct stat *);	// fxpand.c

static ZKL_Object FileObject;

static IBucketHeader *flBuckets, bucketHeader;

typedef struct
{
   BInstance  instance;
   Instance  *fileName;	 // String of some form or another
   FILE	     *f;	 // Different struct on different OSes
   unsigned   howza:8;
   unsigned   isSpecial:1; // special file, eg Pipe, stdin
   unsigned   isPipe:1;    // Pipe from popen
   unsigned   isStdio:1;   // stdin, stdout, stderr
} File;		// 32 bytes Linux/64, 16 Windows

#define FFILE(i)	( (File *)i )
#define FFF(i)		( ((File *)i)->f )
#define FNAME(i)	stringText(((File *)i)->fileName)
#define IS_STDIO(i)	( ((Instance *)i)->iflag )
#define IS_PIPE(i)	( ((File *)i)->isPipe )
#define IS_SPECIAL(i)	( ((File *)i)->isSpecial )

static void fileMarker(Instance *self)
   { instanceMark(((File *)self)->fileName); }

static void fileClose(File *self)
{
   FILE *f = FFF(self);
   if (f)
   {
      if (IS_PIPE(self))        pclose(f);
      else if (!IS_STDIO(self)) fclose(f);   // don't close std*
      FFF(self) = 0;
   }
}

DllExport FILE *fileFILE(Instance *);

FILE *fileFILE(Instance *self)
{
   if (TYPEO(self) != FileType) return 0;
   return FFF(self);
}

static FILE *getFile(Instance *self, pVM vm)
{
   FILE *f = FFF(self);
   if (!f)
      vmThrow(vm,E_IO_ERROR,"File is not open, can't read from or write to it");
   return f;
}

static void _ioError(char *methodName, char *fileName, char *msg, pVM vm)
{
   char	buf[200];	// filename can be long, as in File(glob("*.c"))

   if (!msg) msg = strerror(errno);
   sprintf(buf,"File.%s(%.100s): %s",methodName,fileName,msg);
   vmThrow(vm,E_IO_ERROR,buf);
}

static Instance *createFile(FILE *f,Instance *fname,int special, pVM vm)
{
   File *file;

   file = (File *)ibucketAllocate(flBuckets,&FileObject,I_OWNED,1,vm);
   file->fileName  = fname;
   file->f	   = f;		// can be zero
   file->howza	   = 1;
   file->isSpecial = special;
   return containerIsCooked(flBuckets,(Instance *)file,I_OWNED);
}

static Instance *fileCreate(Instance *fileName, char *mode, pVM vm)
{
   FILE	*f;	// Can create a File not attached to a file
   char *fname = stringText(fileName);
   struct stat stats;

   if (!*fname)	// MSVC pukes on fopen("",...);
      vmThrow(vm,E_NAME_ERROR,"File(\"\") doesn't work so well!");
   if (!*mode) vmThrow(vm,E_VALUE_ERROR,"File(name,\"\"): Invalid mode");

   f = fopen(fname,mode);
   if (!f)
   {
      if (errno == EMFILE)		// too many open files
      {
	 collectGarbage(1,vm);	// see if there are any dead Files
	 f = fopen(fname,mode);
	 if (f) goto opened;
      }
      if (errno == 0)	// MSVC, wtf? "No Error"
      {
	 char buf[200];
	 sprintf(buf,"File(%.100s,\"%s\"): I'm guessing invalid mode",
		 fname,mode);
	 vmThrow(vm,E_VALUE_ERROR,buf);
      }
      _ioError("open",fname,0,vm);
   }
opened:
   fstat(fileno(f),&stats);	//!!! check for -1
   return createFile(f,fileName,!S_ISREG(stats.st_mode),vm);
}

Instance *doPopen(Instance *cmd, char *type, pVM vm)
{
   char *shcmd = stringText(cmd);
   File *f;
   FILE	*p;

   if (!*type) 
      vmThrow(vm,E_VALUE_ERROR,"popen(name,\"\"): Invalid mode");

   p = popen(shcmd,type);
   if (!p) _ioError("popen",shcmd,0,vm);  // errno not set on mem alloc fail

   f = (File *)ibucketAllocate(flBuckets,&FileObject,I_OWNED,1,vm);
   f->fileName  = cmd;
   f->f	        = p;
   f->howza     = 1;
   f->isSpecial = 1;
   f->isPipe    = 1;
   f->instance.iflag2 = 1;
   return containerIsCooked(flBuckets,(Instance *)f,I_OWNED);
}

/* ******************************************************************** */
/* *************************** Methods ******************************** */
/* ******************************************************************** */

    // .open(fileName,mode="r"), .create --> File
    // NOTE! if you open an existing File, you still get a new one
static Instance *File_open(File *self,pArglist arglist,pVM vm)
{
   Instance *fileName = arglistGetBT(arglist,0,StringType,"File.open",vm);
   char	    *mi	      = arglistTryToGetString(arglist,1,"File.open",vm);
   char	    *mode     = "r";
   Instance *file;

   if (mi) mode = mi;
   file = fileCreate(fileName,mode,vm);
//!!! cannonize the filename
   return file;
}

    // .close() --> flush
static Instance *File_close(File *self,pArglist arglist,pVM vm)
{
   if (IS_STDIO(self))	// unless and until I can remap
      vmThrow(vm,E_IO_ERROR,"Can't close stdin, stdout or stderr");

   fileClose(self);
   return (Instance *)self;
}

    // .flush() --> self
static Instance *File_flush(Instance *self,pArglist arglist,pVM vm)
{
   FILE *f = FFF(self);
   if (f && (fflush(f) == EOF)) _ioError("flush",(char *)FNAME(self),0,vm);
   return self;
}

    // .toBool() --> True if file is open
static Instance *File_toBool(Instance *self,pArglist arglist,pVM vm)
{
   FILE *f = FFF(self);
   if (f) return BoolTrue;
   return BoolFalse;
}

    // .toString --> "File(name)"
static Instance *File_toString(Instance *self,pArglist arglist,pVM vm)
   { return stringCat(vm,"File(",FNAME(self),")",(char *)0); }

static Instance *filePrint(Instance *self,pArglist arglist,int ln, pVM vm)
{
   int	 i,n = (int)listLen(arglist,vm);
   FILE	*f = getFile(self,vm);

   for (i = 0; i < n; i++)
   {
      Instance *pi = arglistGetString(arglist,i,0,vm);
      fputs(stringText(pi),f);
      if (ferror(f)) _ioError("writeln",(char *)FNAME(self),"fputs failed",vm);
   }
   if(ln) fputs("\n",f);
   return self;
}

    // .writeln(text,...): text only method --> self
    // .println
static Instance *File_writeln(Instance *self,pArglist arglist,pVM vm)
   { return filePrint(self,arglist,1,vm); }

    // .write(text): text only method --> self
    // .print
static Instance *File_print(Instance *self,pArglist arglist,pVM vm)
   { return filePrint(self,arglist,0,vm); }

static size_t _writeBytes(File *self,void *bytes,size_t size, pVM vm)
{
   FILE *f = self->f;
   size = fwrite(bytes,1,size,f);
   if (ferror(f)) _ioError("write",(char *)FNAME(self),"fwrite failed",vm);
   return size;
}

static size_t fileWrite(Instance *self,pArglist arglist,pVM vm)
{
   Instance    *x;
   int		n;
   size_t	bytesWritten = 0;

   x = listGet(arglist,0);
   if (!x) return 0;

   getFile(self,vm);

   n = 0; do
   {
      void     *ptr;
      size_t	size;

      switch (TYPEO(x))
      {
	 case DataType:
	    ptr = dataText(x,&size);
	    bytesWritten += _writeBytes((File *)self,ptr,size,vm);
	    break;
	 case ListType:	//!!! not thread safe
	 case TupleType:
	    bytesWritten += fileWrite(self,x,vm);
	    break;
	 case StringType:
	    ptr  = stringText(x);
	    size = strlen(ptr);
	    bytesWritten += _writeBytes((File *)self,ptr,size,vm);
	    break;
	 case IntType:
	 {
	    Byte c = (Byte)convertToInt(x,vm);	// lowest byte
	    bytesWritten += _writeBytes((File *)self,&c,1,vm);
	    break;
	 }
	 default:
	 {
	    char	buf[100];
	    sprintf(buf,"File.write: Can't handle %s",iname(x));
	    vmThrow(vm,E_TYPE_ERROR,buf);
	 }
      }
   } while ((x = listGet(arglist,++n)));

   return bytesWritten;
}

    // .write...)
static Instance *File_write(Instance *self,pArglist arglist,pVM vm)
   { return intCreate(fileWrite(self,arglist,vm),vm); }

    // Read a line from a file.
    // I'm assuming shortish lines, otherwise, use a ZBText
    // strip is 0 (don't strip), 1 (strip left), 2 (both ends), 3 (right)
    // Can GC
#define MAX_READ 500	// longest line I'll read before concat-ing
static Instance *_readLine(File *self,
	int strip, int *eof,Instance **fenceSlot,pVM vm)
{
   char      buf[MAX_READ]; //!!!??? ZBText??
//   FILE	    *f    = self->f;
   FILE	    *f    = getFile((Instance *)self,vm);
   Instance *text = emptyString;
   int	     s;
   size_t    n;

   *eof = 0;
   while(1)
   {
      vmMightStall(vm,1);	// I have no idea how long fgets() will take
	 s = !fgets(buf,MAX_READ,f);	// f could be stdin or very slow
      vmMightStall(vm,0);
      if (s)
      {
	 if (feof(f))
	 {
	    if (text == emptyString) *eof = 1;
	    break;
	 }

	 if (ferror(f)) _ioError("readln",(char *)FNAME(self),"fgets failed",vm);
	 break;
      }
      text       = stringCat(vm,stringText(text),buf,(char *)0);
      *fenceSlot = text;	// protect text

      n = strlen(buf);
      if (n < (MAX_READ - 1) || buf[n-1] == '\n') break;
   }
   if(strip)
   {
      char   *str = ZKL_STRING(text);
      size_t  len = strlen(str), nl;

      str = stripWS(str,len,&nl,1);
      if (len != nl) return stringCreate2(str,nl,vm);
   }
   return text;
}

    // .readln(n=1|*) --> List | String
static Instance *File_readln(Instance *self,pArglist arglist,pVM vm)
{
   Fence     fence;
   Instance *text, *list;
   int	     eof = 0, s;
   size_t    n;

   getFile(self,vm);

   s = arglistGetSize(arglist,0,(size_t)-1,&n,vm);
   if (s == 0)		// readln() --> one line
   {
      vmSetFence(vm,&fence,0,0);
	 text = _readLine((File *)self,0,&eof,&fence.i1,vm);
      vmRemoveFence(&fence,0);

      if (eof) vmThrowTheEnd(vm);

      return text;
   }

   if (n == 0) return emptyList;

   list = listCreate((s == 1) ? 400 : n, 0x1,I_OWNED,vm);
   vmSetFence(vm,&fence,0,list);
      while (n--)
      {
	 text = _readLine((File *)self,0,&eof,&fence.i1,vm);
	 if (eof) break;		// read nothing
	 listAppend(list,text,vm);
      }
      if (eof && (listLen(list,vm) == 0)) vmThrowTheEnd(vm);
   vmRemoveFence(&fence,0);

   return list;
}

    // .ask() --> String
static Instance *File_ask(Instance *self,pArglist arglist,pVM vm)
   { return File_readln(self,NoArglist,vm); }

   // [unix] pipe.read() or pipe.read(*) when I don't know file size
   // also stdin and "special" files
static Instance *drainPipe(Instance *self,FILE *p,pVM vm)
{
   #define BUFSZ 10000
   Byte      buffer[BUFSZ+1];
   Fence     fence;
   Instance *data = dataCreate(1000,I_OWNED,vm);
   size_t    n,sz=0;

   vmSetFence(vm,&fence,0,data);
      while(!feof(p))
      {
	 vmMightStall(vm,1); // the read could be realllly slow
	    n = fread(buffer,1,BUFSZ,p); sz+=n;
	 vmMightStall(vm,0);
	 dataInsertBytes(data,0,buffer,n,1,vm);
      }
   vmRemoveFence(&fence,0);
   if (sz == 0) vmThrowTheEnd(vm);
   if (ferror(p)) _ioError("read",(char *)FNAME(self),"fread failed",vm);

   return data;
}

    // .read() --> Data
    // .read(n|*) --> Data
    // .read(n|*,Data) --> Data
    // .read(n|*,Data,throw=True) --> Data
static Instance *File_read(Instance *self,pArglist arglist,pVM vm)
{
   FILE	     *f = getFile(self,vm);
   Fence      fence;
   Instance *_n = arglistTryToGet(arglist,0);
   Instance *data, *pd=arglistTryToGetBT(arglist,1,DataType,"File.read",vm);
   size_t    n,sz;
   long int  fileSize = 1L<<31;	// off_t on Linux, which is signed
   int	     z, throw = arglistTryToGetBool(arglist,2,1,"File.read",vm);

   if(pd && pd==VData) pd = 0;	// you gotta create your own Data
   if(_n==Star) _n = 0;		// blech, but gotta do some checking

	/* read([*]) --> read the rest of the file.  I just grab the file
	 * size and read.  We could have already read some of the file and
	 * MS text file munging makes the size suspect but it SHOULD be >=
	 * what we will actually read.
	 * Note: pipe (popen) has zero size, also stdin 
	 *    so read() & read(*) --> read(1) but read(*,Data) -->
	 *    read(data.sz,data).
	 */
#if 0
   if (!IS_PIPE(self)) // not a pipe from popen & not reusing buffer
   {
      struct stat stats;
      fstat(fileno(f),&stats);	//!!! check for -1
      if(S_ISREG(stats.st_mode))   // regular file? special file size == 0
	 fileSize = stats.st_size; // On Windows, size might be > amount read
      else 	   // special file: you need to specify size
      {
	 Instance *i = arglistTryToGet(arglist,0);
	 if (!i || i==Star) fileSize = 1;
      }
   }
#else
   if (IS_SPECIAL(self))  // special file: you need to specify size
   { 
      if(!pd && !_n) fileSize = 1; 	// read(?,Data): leave big
   }
   else if(!pd)  // decide how much Data to allocate
   {
      struct stat stats;
      fstat(fileno(f),&stats);	//!!! check for -1
      fileSize = stats.st_size; // On Windows, size might be > amount read
   }
#endif

	// if nothing to read, don't wait for fread() to fail
   if(fileSize==0)
   {
      if(pd) dataWillFill(pd);
      vmThrowTheEnd(vm);
   }

//!!! use ftell() to cap n
   z = arglistGetSize(arglist,0,fileSize,&n,vm);  // limit read to fileSize
   if (n==0)	// they said to read 0 bytes
   {
      if(pd){ dataWillFill(pd); return(pd); }
      return emptyKData;
   }

   if (pd)  // use their buffer
   {
      size_t sz = dataSpace(data = pd);
      if (n > sz) n = sz;  //!!!??? dataINeedThisMuchSpace(d,n)?
   }
   else
   {
      // pipe.read() or pipe.read(*), stdin.read(), stdin.read(*)?
      if((IS_PIPE(self) | IS_SPECIAL(self)) && z < 2)
	 return(drainPipe(self,f,vm));
      data = dataCreate(n,I_OWNED,vm);
   }

   vmSetFence(vm,&fence,0,data);
      vmMightStall(vm,1); // the read could be realllly slow
	 sz = fread(dataWillFill(data),1,n,f);
      vmMightStall(vm,0);
   vmRemoveFence(&fence,0);
   dataSetSize(data,sz);
//   if (sz==0 && feof(f)) vmThrowTheEnd(vm);
   if (sz==0 && feof(f)){ if(throw) vmThrowTheEnd(vm); else return data; }
   if (ferror(f)) _ioError("read",(char *)FNAME(self),"fread failed",vm);

   return data;
}

    // .read1() --> Int
static Instance *File_read1(Instance *self,pArglist arglist,pVM vm)
{
   FILE	        *f = getFile(self,vm);
   size_t	 n;
   unsigned char c;

   vmMightStall(vm,1);
      n = fread((void *)&c,1,1,f); // the read could be realllly slow
   vmMightStall(vm,0);
   if ((n == 0) && feof(f)) vmThrowTheEnd(vm);
   if (ferror(f)) _ioError("read",(char *)FNAME(self),"fread failed",vm);

   return intCreate(c,vm);
}

    // .toData() --> read()
static Instance *File_toData(Instance *self,pArglist arglist,pVM vm)
{
   Instance *data = File_read(self,NoArglist,vm);
   fileClose((File *)self);
   return data;
}

    // howza([getter mode]) --> self|int
Instance *File_howza(Instance *self,pArglist arglist,pVM vm)
{
   int64_t i64;
   if (arglistTryToGetInt(arglist,0,&i64,0,vm))
   {
      ((File *)self)->howza = (unsigned)i64;
      return self;
   }

   return intCreate(((File *)self)->howza,vm);
}

typedef struct  // for running file through Data buffer
{
   Instance     *data;
   unsigned int  n;
} PData;

static Instance *_fileReadOne(Instance *,size_t,void *,size_t,pVM);

    // File.reduce, .pump, .filter
    // Most times have to alloc new container as I don't know what result
    //   is going to be used for, it may be stashed, ie can't reuse
static Instance *_fileReadLine(Instance *self,
				size_t idx,void *pdata,size_t _,pVM vm)
{
   Instance *text;
   int 	     strip = 0;

   if (pdata==(void *)1) { strip = 1; pdata = 0; }

   if (pdata) // reuse buffer
   {
      FILE   *f = FFF(self);
      size_t  n = ((PData *)pdata)->n;  // ??? do I care if data grows?
      text = ((PData *)pdata)->data;
      if(!f) return 0;
      vmMightStall(vm,1);
	 n = fread(dataWillFill(text),1,n,f);  // reset buffer and fill it
      vmMightStall(vm,0);
      dataSetSize(text,n);
      if ((n == 0) && feof(f)) return 0; // EoF
      if (ferror(f)) _ioError("read",(char *)FNAME(self),"fread failed",vm);
      return text;
   }
   else
   {
      Fence fence;
      int   eof=0;

      vmSetFence(vm,&fence,0,0);
	 text = _readLine((File *)self,strip,&eof,&fence.i,vm);
      vmRemoveFence(&fence,0);

      if (eof) return 0;
      return text;
   }
}

static ZGetter howza2getter(int howza, size_t *X)
{
   int x = 0;
   ZGetter getter = _fileReadLine;
   switch(howza)
   {
      case  0: x = 1; /* fallthough */	       // Byte stream
      case  3: getter = _fileReadOne;	break; // Characters
      default:
      case  1: case 2:			break; // Lines, Strings
      case 11: case 12: x = 1;		break; // stripped
   }
   *X = x;
   return getter;
}

    // File.pump(...)
    // File.pump(Data(),...): Use the Data as the buffer, N is Data.len()
    // File.pump(Data,...):   Use a Data as the sink
    // File.pump(N,...):      Read N bytes into a Data each pass
static Instance *File_pump(Instance *self,pArglist arglist,pVM vm)
{
   Instance *pi = arglistTryToGet(arglist,0);
   if (pi)	// got an arg, what is it?
   {
      Instance	   *r;
      int	    t = TYPEO(pi);
      unsigned int  n;
      PData	    pdata;
      Fence         fence;
      if (t == IntType)  // .pump(N,... which could be useful since Data(4)!=4
      {
	 n = (unsigned int)arglistGetInt(arglist,0,0,vm);	// bytes to get
	 if(n==0) n = 5000;
	 pdata.n    = n;
	 pdata.data = dataCreate(n,I_OWNED,vm);
	 vmSetFence(vm,&fence,0,pdata.data);
	    r = pump(self,0x00,_fileReadLine,&pdata,arglist,1,vm);
	 vmRemoveFence(&fence,0);
	 return r;
      }
      if (t == DataType && pi != VData)  // .pump(Data(),...
      {
	 pdata.n    = dataSpace(pi);
	 pdata.data = pi;
	 return pump(self,0x00,_fileReadLine,&pdata,arglist,1,vm);
      }
   }
   {
      size_t X=0;
      return pump(self,0x00,
           howza2getter(((File *)self)->howza,&X),(void *)X,arglist,0,vm);
   }
}
static Instance *File_filter(Instance *self,pArglist arglist,pVM vm)
{
   size_t X = 0;
   return zfilter(self,0x0,
	   howza2getter(((File *)self)->howza,&X),(void *)X,arglist,0,vm);
}
static Instance *File_reduce(Instance *self,pArglist arglist,pVM vm)
{
   size_t X=0;
   return zreduce(self,
	   howza2getter(((File *)self)->howza,&X),(void *)X,arglist,0,vm); 
}

static Instance *  // read byte or character
_fileReadOne(Instance *self,size_t n,void *bytes,size_t _,pVM vm)
{
   FILE		*f = getFile(self,vm);
   unsigned char c;
   size_t	 i;

//   if(!f) return 0;	// getFile does this
retry:
   vmMightStall(vm,1);
      i = fread(&c,1,1,f);
   vmMightStall(vm,0);
   if (i==0)	// nothing read: closed, error or ?
   {
      if(feof(f)) return 0;
      if (ferror(f)) _ioError("read",(char *)FNAME(self),"fread failed",vm);
printf("WTF????\n");
      goto retry;
   }
   return bytes ? intCreate(c,vm) : stringCreate2((char *)&c,1,vm);
}

    // .walker([n=1]), n: 0 (bytes), 1 (lines) == 2 (strings), 3 (characters)
static Instance *File_Walker(Instance *self,pArglist arglist, pVM vm)
{
   int64_t n = ((File *)self)->howza;
   size_t  X;
   ZGetter getter;

   arglistTryToGetInt(arglist,0,&n,0,vm);
   getter = howza2getter((int)n,&X);
   return walkerCreate(self,getter,(void *)X, 0,vm);
}

/* ******************************************************************** */
/* *********************** File Name Utilities ************************ */
/* ******************************************************************** */

	/* File.splitFileName(filename)
	 * Returns: List(drive,path,name,extension)
	 * 
	 * Drive is "" or contains a trailing ":"
	 * Path has a trailing "/"

	 * Path: (^ shows what pathend() points to)
	 *   "foo/bar", "/foo", "/", "/foo/"
	 *       ^        ^       ^       ^
	 *   "..", ".", "/..", "foo."
	 *      ^    ^      ^   ^
	 *   ".foo", "foo", ""
	 *    ^       ^      ^
	 * MS-DOS:
	 *   "A:", "A:foo", "A:/", "A:/foo"
	 *      ^     ^         ^      ^

	 * Extension: The first "." after the last "/".
	 *   Assumes a real filename.
	 *   Notes:
	 *     foo.c --> ".c"
	 *     foo --> ""
	 *     .foo.bar --> ".bar" (UNIX hidden files)
	 *     .foo --> ".foo"  (csh does this)
	 *     y.tab.c --> ".c"
	 *     "" --> ""
	 *     foo. --> ""
	 */
#if 0
static Instance *File_splitFileName(Instance *self,pArglist arglist,pVM vm)
{
   #ifdef _MSC_VER
      #define ISSLASH(c) ((c) == '/' || (c) == '\\')
   #else
      #define ISSLASH(c) ((c) == '/')
   #endif
   char	       *filename = arglistGetOnlyString(arglist,0,"File.splitFileName",vm);
   char	       *ptr;
   Instance    *list     = tupleCreate(4,I_OWNED,vm);
   Instance    *x;
   Fence	fence;

   vmSetFence(vm,&fence,0,list);
      ///////////////// Drive, Windows only
      x = emptyString;
      #ifdef _MSC_VER
	 if (filename[1] == ':')
	 {
	    x = stringCreate2(filename,2,vm);
	    filename += 2;
	 }
      #endif
      tupleAppend(list,x); x = emptyString;

      /////////////// Path
      ptr = filename + strlen(filename);  // back up until /
      while (ptr != filename)
      {
	 ptr--;
	 if (ISSLASH(*ptr))
	 {
	    if (ptr == filename) ptr++;			// /, /foo
	    break;
	 }
      }
      if (*ptr == '.')		// might have . or ..
      {
	 if (ptr[1] == '\0') ptr++;				// "."
	 else if (ptr[1] == '.' && ptr[2] == '\0') ptr += 2;	// ".."
      }	
      if (ISSLASH(*ptr)) ptr++;
      // !!!take care of foo///bar
      x = stringCreate2(filename,ptr - filename,vm);
      tupleAppend(list,x);
      filename = ptr;

      ////////////// Extension
      ptr = filename + strlen(filename);		// end of file name
      for ( ; (ptr != filename) && (*ptr != '.'); ptr--) ;
      if (ptr == filename) ptr = "";	// no extension

      ////////////// File name
      if (*ptr == '.') x = stringCreate2(filename,ptr - filename,vm);
      else	       x = stringCreate(filename,I_OWNED,vm);
      tupleAppend(list,x);

      // stash the extension
      x = emptyString;
      if (0 == strcmp(ptr,".")) ptr = "";	// no extension
      if (*ptr == '.') x = stringCreate(ptr,I_OWNED,vm);
      tupleAppend(list,x);
   vmRemoveFence(&fence,0);
   return list;
}
#else

static Instance *File_splitFileName(Instance *self,pArglist arglist,pVM vm)
{
#ifdef _MSC_VER
   #if 0  // _splitpath_s sucks
      char     *filename = arglistGetOnlyString(arglist,0,"File.splitFileName",vm);
      char      drive[50], path[_MAX_PATH], nm[_MAX_PATH], ext[50];
      int       s = _splitpath_s(filename, drive,50, path,_MAX_PATH, nm,_MAX_PATH, ext,50);

      if(s) _ioError("splitFileName",filename,"Can't parse",vm);
      tupleCreate(4,I_OWNED,vm);
      return tupleCreateX(vm,
	 stringCreate(drive,I_OWNED,vm), stringCreate(path,I_OWNED,vm),
	 stringCreate(nm,I_OWNED,vm), stringCreate(ext,I_OWNED,vm),   ZNIL);
   #else
      return fcnRunFromClass(Utils_Helpers,"splitFileNameWindows",arglist,vm);
   #endif
#else	// Unix
   return fcnRunFromClass(Utils_Helpers,"splitFileNameUnix",arglist,vm);
#endif
}
#endif

typedef struct{ Instance *i; pVM vm; } FX;

    // For lots of matches, a "better" way to do this might be to append to
    // a Data and, when done, convert to list.
static int _stashFileName(char *fileName, void *fxp)
{
   FX *fx = (FX *)fxp;
//   if(fx->dfa && !regExpExec(fx->dfa,(UChar *)fileName,1,0,0,fx->vm)) return 0;
   listAppend(fx->i, stringCreate(fileName,I_OWNED,fx->vm), fx->vm);
   return 0;
}
    // File.glob(globPattern,flags=0) --> Tuple
    // File.glob(Data,flags=0): Data is list of patterns to match
    // while(glob) { dir = glob.pop() } could be useful
static Instance *File_glob(Instance *self,pArglist arglist,pVM vm)
{
//   extern int   regExpID;			// miscObj.c
//   extern Byte *regExpExtractDFA(Instance *);	// miscObj.c

   char	    *pat;
   Instance *pati = arglistGet(arglist,0,"File.glob",vm);
   Instance *fnms = listCreate(0,0x2,I_OWNED,vm);
   FX	     fx   = { fnms,vm };
   Fence     fence;
   int	     flags = 0;
   int64_t   n;

   if (arglistTryToGetInt(arglist,1,&n,0,vm)) flags = ((int)n & FXP_USER_MASK);

   if(TYPEO(pati)==StringType) pat=stringText(pati);
   else	// match list of patterns in Data
   { 
      pati = arglistGetBT(arglist,0,DataType,"File.glob",vm);
      if(dataGetMode(pati)!=DATA_TREAT_AS_STRINGS)
         vmThrow(vm,E_TYPE_ERROR,"File.glob(Data(0,String...");
      pat=(char *)dataText(pati,0); flags |= FXP_GLOB_LIST; 
   }

   vmSetFence(vm,&fence,0,fnms);
      fxpand(pat,flags,_stashFileName,&fx);
   vmRemoveFence(&fence,0);

   return fnms;
}

/* ******************************************************************** */
/* ************************** Misc Utilities ************************** */
/* ******************************************************************** */

    // File.exists(fileName) --> Bool
static Instance *File_exists(Instance *self,pArglist arglist, pVM vm)
{
   char *fileName = arglistGetOnlyString(arglist,0,"File.exists",vm);
   struct stat stats;
   if (stat(fileName,&stats)) return BoolFalse;
   return BoolTrue;
}

    // File.searchFor(fileName, path=System.classPath, globIt=False)
static Instance *File_searchFor(Instance *self,pArglist arglist, pVM vm)
   { return fcnRunFromClass(Utils_Helpers,"findFileOnPath",arglist,vm); }

    // File.globular(startingDir, filePattern,
    //		recurse=True, flags=FILE.GLOB.NO_DIRS, out=Data(0,1))
    // Like glob only more so
static Instance *File_globular(Instance *self,pArglist arglist,pVM vm)
   { return fcnRunFromClass(Utils_Helpers,"globular",arglist,vm); }

    // .isDir(name) --> Bool
static Instance *File_isDir(Instance *self,pArglist arglist, pVM vm)
{
   char *name = arglistGetOnlyString(arglist,0,"File.isDir",vm);
   return boolCreate(is_dir(name,0));
}

    // .mkdir(name [,permissions]) --> update file system
static Instance *File_mkdir(Instance *self,pArglist arglist, pVM vm)
{
   char *path = arglistGetOnlyString(arglist,0,"File.mkdir",vm);
   #if defined(_MSC_VER)
      if (_mkdir(path))
   #else	// Linux, POSIX
      if (mkdir(path,0777))  //??? mkdir("..") works
   #endif
   {
      if (errno == EEXIST) return BoolFalse;  // exists but might not be a directory
      _ioError("mkdir",path,0,vm);
   }
   return BoolTrue;
}
    // .mkdir_p(name,verbose) --> update file system
static Instance *File_mkdir_p(Instance *self,pArglist arglist, pVM vm)
   { return fcnRunFromClass(Utils_Helpers,"mkdir_p",arglist,vm); }

    // File.len([file name])
static Instance *File_len(Instance *self,pArglist arglist, pVM vm)
{
   char	       *fileName;
   size_t	fileSize;
   struct stat	stats;

   if ((fileName = arglistTryToGetString(arglist,0,"File.len",vm)))
   {
      if (0 == stat(fileName,&stats)) fileSize = stats.st_size;
      else
      {
	 char buf[200];
	 sprintf(buf,"File.len(%.100s): Could not open",fileName);
	 vmThrow(vm,E_NAME_ERROR,buf);
      }
   }
   else
   {
      FILE *f = getFile(self,vm);
      fstat(fileno(f),&stats);
      fileSize = stats.st_size;
   }

   return intCreate(fileSize,vm);
}

    // File.info([file name])
    // --> T(size,creation time,last mod time,isDir,mode)
static Instance *File_info(Instance *self,pArglist arglist, pVM vm)
{
   char	       *fileName;
   Instance    *tuple;
   struct stat	stats;

   if ((fileName = arglistTryToGetString(arglist,0,"File.info",vm)))
   {
      if (stat(fileName,&stats))
      {
	 char buf[200];
	 sprintf(buf,"File.info(%.100s): Could not open",fileName);
	 vmThrow(vm,E_NAME_ERROR,buf);
      }
   }
   else
   {
      FILE *f = getFile(self,vm);
      fstat(fileno(f),&stats);
   }

   tuple = tupleCreateX(vm,
       intCreate(stats.st_size, vm),	// size
       intCreate(stats.st_ctime,vm),	// time of last status change
       intCreate(stats.st_mtime,vm),	// last modification time
       #ifdef _MSC_VER
	  boolCreate(is_dir(0,&stats)),
       #else	// Unix
          boolCreate(S_ISDIR(stats.st_mode)),
       #endif
       intCreate(stats.st_mode,vm),
       ZNIL);

   return tuple;
}

    // File.setModTime(file name,time_t) --> Bool
static Instance *File_setModTime(Instance *self,pArglist arglist, pVM vm)
{
   char	       *fileName = arglistGetOnlyString(arglist,0,0,vm);
   time_t	mtime = (time_t)arglistGetInt(arglist,1,0,vm);
   struct utimbuf times;

   times.actime = times.modtime = mtime;
   if (-1 == utime(fileName,&times)) _ioError("setModTime",fileName,0,vm);
   return BoolTrue;
}

static Instance *File_mkTmp(Instance *self,pArglist arglist, pVM vm)
{
#ifdef _MSC_VER		// racey, icky
   char fname[50] = "zklTmpFileXXXXXX";
   if(0 != _mktemp_s(fname,sizeof(fname)-2))
      vmThrow(vm,E_IO_ERROR,"mktmp can't");
   return fileCreate(stringCreate(fname,I_OWNED,vm),"w+b",vm);
#else	// Linux, POSIX, BSD, XOPEN
   char fname[50] = "zklTmpFileXXXXXX";
   FILE *f;
   int   fd = mkstemp(fname);
   if(fd==-1) _ioError("mktmp","",0,vm);

   f = fdopen(fd,"w+b");
   if(!f) _ioError("mktmp",fname,0,vm);

   return createFile(f,stringCreate(fname,I_OWNED,vm),0,vm);
#endif
}

    // File.rename(oldPath,newPath)
    // -->Bool
static Instance *File_rename(Instance *self,pArglist arglist,pVM vm)
{
   char *oldPath = arglistGetOnlyString(arglist,0,"File.rename",vm);
   char *newPath = arglistGetOnlyString(arglist,1,"File.rename",vm);

   if (rename(oldPath,newPath))
   {
      char buf[400];
      sprintf(buf,"rename(%.100s,%.100s): %s",oldPath,newPath,strerror(errno));
      vmThrow(vm,E_IO_ERROR,buf);
   }
   return BoolTrue;
}

    // File.delete(oldPath,newPath)
    // -->Bool
static Instance *File_delete(Instance *self,pArglist arglist,pVM vm)
{
   #ifdef _MSC_VER
      #define unlink	 _unlink
   #endif

   char *path = arglistGetOnlyString(arglist,0,"File.delete",vm);

   if (unlink(path) == -1) _ioError("delete",path,0,vm);
   return BoolTrue;
}


static const MethodTable fileMethods[] =
{
   "toString",		(pMethod)File_toString,
   "create",		(pMethod)File_open,
   "open",		(pMethod)File_open,
   "close",		(pMethod)File_close,
   "toBool",		(pMethod)File_toBool,
   "toData",		(pMethod)File_toData,
   "flush",		(pMethod)File_flush,
   "write",		(pMethod)File_write,
   "print",		(pMethod)File_print,
   "writeln",		(pMethod)File_writeln,
   "println",		(pMethod)File_writeln,
   "read",		(pMethod)File_read,
   "read1",		(pMethod)File_read1,
   "readln",		(pMethod)File_readln,
   "ask",		(pMethod)File_ask,  // Overlap with Console for stdin
//   "seek",		(pMethod)File_seek,
   "glob",		(pMethod)File_glob,
   "exists",		(pMethod)File_exists,
   "isDir",		(pMethod)File_isDir,
   "mkdir",		(pMethod)File_mkdir,
   "mkdir_p",		(pMethod)File_mkdir_p,
   "searchFor",		(pMethod)File_searchFor,
   "globular",		(pMethod)File_globular,
   "len",		(pMethod)File_len,
   "info",		(pMethod)File_info,
   "setModTime",	(pMethod)File_setModTime,
   "splitFileName",	(pMethod)File_splitFileName,

   "pump",		File_pump,
   "filter",		File_filter,
   "reduce",		File_reduce,
   "walker",		(pMethod)File_Walker,
   "howza",		(pMethod)File_howza,
   "rename",		(pMethod)File_rename,
   "delete",		(pMethod)File_delete,
   "mktmp",		(pMethod)File_mkTmp,
   0,			0
};

/* ******************************************************************** */
/* **************************** Properties **************************** */
/* ******************************************************************** */

    // .eof
static Instance *File_EoF(Instance *self, pVM vm)
{
   FILE *f = getFile(self,vm);
   return boolCreate(feof(f));
}

Instance *stdIn, *stdOut, *stdErr;

	// .stdin, .stdout, .stderr
static Instance *File_stdin( Instance *self, pVM vm) { return stdIn;  }
static Instance *File_stdout(Instance *self, pVM vm) { return stdOut; }
static Instance *File_stderr(Instance *self, pVM vm) { return stdErr; }

static Instance *File_fileName(Instance *self, pVM vm)
   { return ((File *)self)->fileName; }

static Instance *File_isClosed(Instance *self, pVM vm)
   { return FFF(self) ? BoolFalse : BoolTrue; }

static const PropertyTable properties[] =
{
   "eof",		(pProperty)File_EoF,
   "stdin",		(pProperty)File_stdin,
   "stdout",		(pProperty)File_stdout,
   "stderr",		(pProperty)File_stderr,
   "fileName",		(pProperty)File_fileName,
   "isClosed",		(pProperty)File_isClosed,
   0,			0
};

/* ******************************************************************** */
/* ******************************************************************** */

    // Create stdin, stdout, stderr as immortals
    // HANDLE hStdout;
    // hStdout = GetStdHandle(STD_OUTPUT_HANDLE); 
static Instance *createStdio(char *stdName, FILE *f)
{
   File *file=(File *)ibucketAllocate(flBuckets,&FileObject,I_IMMORTAL,1,NoVM);
   file->fileName  = 0;			// in case stringCreate() GCs
   file->fileName  = stringCreate(stdName,I_IMMORTAL,NoVM);
   file->f	   = f;
   file->howza	   = 1;
   file->isSpecial = 1;
   file->isStdio   = 1;
   file->instance.iflag = 1;
   return containerIsCooked(flBuckets,(Instance *)file,I_IMMORTAL); // no need to cook
}

static int fileMightBeDead(Instance *self)
{
   fileClose((File *)self);
   // let fileName free itself; it is usually gone by now
   return 1;
}



    /* Create a place holder in the TheVault so File can be found.
     * Don't need a GC marker because TheVault will mark it
     */
//static pMethod in_file_methods(Instance *ignore, register char *str);

static Instance *aFile;

void fileConstruct(void)
{
   constructObject(&FileObject,FileType, fileMethods,properties,0,NoVM);
   FileObject.freeMe       = fileMightBeDead;
   FileObject.magicMarker  = fileMarker;
//   FileObject.methodSearch = in_file_methods;
   FileObject.isize	   = sizeof(File);
   FileObject.isBInstance  = 1;
   FileObject.createReturnsSelf = 1;

   	// shares on Windows, not on Linux
   flBuckets = ibucketHitchHike(&FileObject,0,25,&bucketHeader,NoVM);

   aFile = createFile(0,emptyString,0,NoVM);
   vaultAdd("",aFile,NoVM);

   stdIn  = createStdio("stdin", stdin);
   stdOut = createStdio("stdout",stdout);
   stdErr = createStdio("stderr",stderr);
}





///////////////////////////////////////////////////////////////
// zkl extractTable.zkl < file.c | gperf | zkl gperf.zkl -i file


