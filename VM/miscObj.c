/* miscObj.c : Some misc objects
 * 
 * Copyright (c) 2006,2007,2008-13,2014 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008
#define _GNU_SOURCE		// pthread _np functions

#include <stdio.h>
#include <stdlib.h>		// system()
#include <string.h>
#include <time.h>
#ifdef _MSC_VER
   #include <process.h>		// system()
   #include <direct.h>		// _getcwd()
   #include <conio.h>
   #define getch	_getch
   #define getpid	_getpid
   #define tzset	_tzset
#endif
#ifdef __unix__
   extern char **environ;
   #include <unistd.h>		// getcwd
   #include <ncurses.h>		// getch
   #include <dlfcn.h>		// dlclose
   #include <sys/time.h>	// gettimeofday
   #include <pthread.h>		// to get cpus
   #include <errno.h>
   #include <sys/stat.h>
#endif
#define __NOT_A_DLL

#include "zklObject.h"
#include "zklData.h"
#include "zklDictionary.h"	// for System_getenv
#include "zklFcn.h"
#include "zklList.h"
#include "MD5/global.h"
#include "MD5/md5.h"
#include "zklMethod.h"
#include "zklNumber.h"
#include "zklString.h"
#include "zklUtil.h"

/* ******************************************************************** */
/* *************************** Console ******************************** */
/* ******************************************************************** */

static ZKL_Object ConsoleObject;
static Instance	  Console;

     /* This uses a lock to avoid multiple threads from blasting each others
      * console output.  The MS VC++ threaded libc locks as does Linux.
      */
#if 0
static SpinLock lock;
void _printToConsole(char *text, int newline)
{
   spinLockAcquire(&lock);
      fputs(text,stdout); if (newline) puts("");
   spinLockRelease(&lock);
}
#endif

    // Console.print(text), .write(text) --> stdout
    // Foo.print(text) --> Console.print(Foo,text)
Instance *Console_print(Instance *self,pArglist arglist,pVM vm)
{
   Instance *s = arglistConcat(0,arglist,vm);

   if (self != &Console)
   {
      MLIST(mlist,2);
      s = arglistConcat(0,mlistBuild(mlist,self,s,ZNIL),vm);
   }

   fputs(stringText(s),stdout); fflush(stdout);
//   _printToConsole(stringText(s),0);
   return s;
}

    // Console.println(text), writeln(text) --> text\n
    // Foo.println(text) --> Console.println(Foo,text)
Instance *Console_println(Instance *self,pArglist arglist,pVM vm)
{
   Instance *s = arglistConcat(0,arglist,vm);

   if (self != &Console)
   {
      MLIST(mlist,2);
      s = arglistConcat(0,mlistBuild(mlist,self,s,ZNIL),vm);
   }

   puts(stringText(s));
//   _printToConsole(stringText(s),1);
   return s;
}

    // Console.ask([prompt]) --> String
    // Console.ask(n [,prompt]) --> String
    //   Look for nth arg first, then ask
static Instance *Console_ask(Instance *self,pArglist arglist,pVM vm)
{
   char      buf[501], *ptr;
   Instance *pi = arglistTryToGet(arglist,0);
   int	     z=0;

   *buf = '\0';
   if (pi && TYPEO(pi) == IntType)
   {
extern Instance *vmMaybeGetArg(unsigned,pVM);
      int n = (int)arglistGetInt(arglist,0,0,vm);
      if (n >= 0)
      {
	 pi = vmMaybeGetArg(n,vm);
	 if (pi) return iToString(pi,vm);
	 z = 1;
      }
   }

//   Console_print(self,arglist,vm);
   pi = arglistConcat(z,arglist,vm);
   fputs(stringText(pi),stdout); fflush(stdout);   

#if _MSC_VER	// gets_s() (C11) isn't everywhere yet
   vmMightStall(vm,1);
      ptr = gets_s(buf,sizeof(buf));
   vmMightStall(vm,0);
   return stringCreate(buf,I_OWNED,vm);
#else
   if(feof(stdin)) vmThrowTheEnd(vm);  // ^D
   vmMightStall(vm,1);
      ptr = fgets(buf,sizeof(buf)-1,stdin);	// Windows: ^Z is EoF
   vmMightStall(vm,0);
   if (*buf) buf[strlen(buf)-1] = '\0';	// nuke trailing \n
   return stringCreate(buf,I_OWNED,vm);
#endif
}

    // Console.readKey() --> String
static Instance *Console_readKey(Instance *self,pArglist arglist,pVM vm)
{
   char buf[2] = { 0,0 };
//   int  s;
   vmMightStall(vm,1);
      #if 0 && __unix__	// doesn't work
        s = getch();
	if (s == ERR) s = 0;
	*buf = s;
      #else
         *buf = getch();
      #endif
   vmMightStall(vm,0);
   return stringCreate(buf,I_OWNED,vm);
}

//!!!! Console.wait() --> 

static const MethodTable consoleMethods[] =
{
   "toBool",		(pMethod)Bool_soTrue,
   "print",		(pMethod)Console_print,
   "write",		(pMethod)Console_print,
   "println",		(pMethod)Console_println,
   "writeln",		(pMethod)Console_println,
   "ask",		(pMethod)Console_ask,
   "readKey",		(pMethod)Console_readKey,
   "read",		(pMethod)Console_ask,	// Stream
   "readln",		(pMethod)Console_ask,	// Stream
   "close",		(pMethod)Object_noop,	// Stream
   "flush",		(pMethod)Object_noop,	// Stream
   0,			0
};

	//////////////////////// Console Properties /////////////////////

    // Console.keyPressed -->Bool
static Instance *Console_keyPressed(Instance *self,pVM vm)
{
   #ifdef _MSC_VER
      return boolCreate(_kbhit());	// ncurses?
   #elif __unix__   // both these fail
    #if 0
      fd_set rfds;
      int    s;
      struct timeval tv = { 1,0 };

      FD_ZERO(&rfds); FD_SET(0,&rfds);
      s = select(1,&rfds,0,0,&tv);
      if (s > 0) return BoolTrue;
    #else	// curses
      int s = getch();
      if (s != ERR) { ungetch(s); return BoolTrue; }
    #endif
   #endif
   return BoolFalse;
}

static const PropertyTable consoleProperties[] =
{
   "keyPressed",	(pProperty)Console_keyPressed,
   0,			0
};


void consoleConstruct(void)
{
   constructObject(&ConsoleObject,NativeType,
			consoleMethods,consoleProperties,0, NoVM);
   ConsoleObject.name   = "Console";
   ConsoleObject.isize  = sizeof(Console);

   instanceInit((Instance *)&Console,&ConsoleObject,I_UNTOUCHABLE);
   vaultAdd(0,(Instance *)&Console,NoVM);

   #ifdef __unix__
//      setlocale(LC_ALL,"");
//      initscr(); cbreak();
//      noecho(); nonl(); intrflush(stdscr,FALSE); 
//      keypad(stdscr,TRUE);

   #endif
}

#if 0
void consoleDestruct(void)
{
   #ifdef __unix__
      endwin();
   #endif
}
#endif

/* ******************************************************************** */
/* ************************* Clock ************************************ */
/* ******************************************************************** */

static Instance *_tmToList(struct tm *,pVM);

    // Clock.tickToTock(time_t,localtime=True) --> T(...)
    // Not thread safe
static Instance *Clock_tickToTock(Instance *self,pArglist arglist,pVM vm)
{
   time_t t = (time_t)arglistGetInt(arglist,0,0,vm);
   if (arglistTryToGetBool(arglist,1,1,0,vm))	// True --> use localtime
      return _tmToList(localtime(&t),vm);
   return _tmToList(gmtime(&t),vm);
}

    // Clock.mktime(y,m,d,h,m,s) --> time_t
static Instance *Clock_tockToTick(Instance *self,pArglist arglist,pVM vm)
{
   struct tm tm;
   time_t    t;

   tm.tm_year = (int)arglistGetInt(arglist,0,0,vm) - 1900;  // Y
   tm.tm_mon  = (int)arglistGetInt(arglist,1,0,vm) - 1;	    // M
   tm.tm_mday = (int)arglistTryToGetIntD(arglist,2,1,0,vm); // D=1
   tm.tm_hour = (int)arglistTryToGetIntD(arglist,3,0,0,vm); // h=0
   tm.tm_min  = (int)arglistTryToGetIntD(arglist,4,0,0,vm); // m=0
   tm.tm_sec  = (int)arglistTryToGetIntD(arglist,5,0,0,vm); // s=0
   tm.tm_wday = tm.tm_yday = tm.tm_isdst = 0;	// standard time
//!!! modifies tzname on Linux

   if (-1 == (t = mktime(&tm)))  //!!! not thread safe in glibc?
      vmThrow(vm,E_VALUE_ERROR,"mktime: Can't convert");
   return intCreate(t,vm);
}

static const MethodTable clockMethods[] =
{
   "toBool",		(pMethod)Bool_soTrue,
   "tickToTock",	(pMethod)Clock_tickToTock,
   "mktime",		(pMethod)Clock_tockToTick,
   0,			0
};

	//////////////////////// Clock Properties /////////////////////

    // Time.Clock.time --> time_t
static Instance *Clock_time(Instance *self,pVM vm)
   { return intCreate(time(0),vm); }

    // Time.Clock.timef --> time + fractional
static Instance *Clock_timef(Instance *self,pVM vm)
{
   double t;
   #ifdef __unix__
      struct timespec now;
      clock_gettime(CLOCK_REALTIME,&now);
      t = now.tv_sec + ((double)now.tv_nsec)/1e9;
   #elif defined(_MSC_VER)
      SYSTEMTIME systemTime;
      t = (double)time(0);
      GetSystemTime(&systemTime);
      t += ((double)systemTime.wMilliseconds)/1000;
   #else
     t = (double)time(0);
   #endif
   return floatCreate(t,vm); 
}

static Instance *_tmToList(struct tm *tm,pVM vm)
{
   return tupleCreateX(vm,
  	INT_CREATE(tm->tm_year + 1900,vm), 	// year, eg 2006
	INT_CREATE(tm->tm_mon + 1,vm),		// month   [1 .. 12]
	INT_CREATE(tm->tm_mday,vm),	   // day in month [1 .. 31]
	INT_CREATE(tm->tm_hour,vm),		// hour    [0 .. 23]
	INT_CREATE(tm->tm_min,vm),		// minute  [0 .. 59]
	INT_CREATE(tm->tm_sec,vm),		// seconds [0 .. 59]
	ZNIL);
}

    // Time.Clock.UTC --> T(...)
static Instance *Clock_gmTime(Instance *self,pVM vm)
{
   time_t     t  = time(0);
   struct tm *tm = gmtime(&t);
   return _tmToList(tm,vm);
}

    // Time.Clock.localTime --> T(...)
static Instance *Clock_localTime(Instance *self,pVM vm)
{
   time_t t;

   tzset();		// force TZ values in case TZ changed
   t = time(0);
   return _tmToList(localtime(&t),vm);
}

    // Time.Clock.timeZone --> 
    //   T(offset from UTC (seconds), Name of time zone,
    //     isDST (Bool), name of DST zone )
static Instance *Clock_timeZone(Instance *self,pVM vm)
{
   #ifdef _MSC_VER
      #define timezone	_timezone
      #define daylight	_daylight
   #endif

   Instance  *list = tupleCreate(4,I_OWNED,vm);
   Fence      fence;
   time_t     t;
   struct tm *tm;

   tzset();		// force TZ values
   t  = time(0);
   tm = localtime(&t);

   vmSetFence(vm,&fence,0,list);
   #if defined(__FreeBSD__) || defined(__minix__)
      tupleAppend(list,intCreate(-tm->tm_gmtoff,vm));	    // offset in seconds
      tupleAppend(list,stringCreate(tzname[0],I_OWNED,vm)); // time zone name
      tupleAppend(list,boolCreate(tm->tm_isdst));
      tupleAppend(list,stringCreate(tzname[1],I_OWNED,vm)); // DST name
   #elif defined(__unix__)
      tupleAppend(list,intCreate(timezone,vm));	 	   // offset in seconds
      tupleAppend(list,stringCreate(tzname[0],I_OWNED,vm)); // time zone name
      tupleAppend(list,boolCreate(tm->tm_isdst));
      tupleAppend(list,stringCreate(tzname[1],I_OWNED,vm)); // DST name
   #elif defined (_MSC_VER)
   {
#define TZNAME_MAX 100   //!!!where is it??? limits.h???
      char   buf[TZNAME_MAX];
      size_t n;
      tupleAppend(list,intCreate(timezone,vm));	 	   // offset in seconds
      _get_tzname(&n,buf,TZNAME_MAX,0);
      tupleAppend(list,stringCreate2(buf,n,vm)); // time zone name
      tupleAppend(list,boolCreate(tm->tm_isdst));
      _get_tzname(&n,buf,TZNAME_MAX,1);
      tupleAppend(list,stringCreate2(buf,n,vm)); // DST name
   }
   #endif
   vmRemoveFence(&fence,0);
   return list;
}

    // Time.Clock.runTime
static Instance *Clock_clock(Instance *self,pVM vm)
{
   #ifdef __unix__
      extern time_t time0;	// in atomic.c

      double now;
      struct timeval tv;

      gettimeofday(&tv,0);
      now = (double)(tv.tv_sec - time0) + (double)tv.tv_usec/1000000.0;
      return floatCreate(now,vm);
   #else
      return floatCreate(((double)clock())/CLOCKS_PER_SEC,vm);
   #endif
}

#if 0
    // .processTimes : L(user time, kernal time)
static Instance *_filetimeToSeconds(LPFILETIME ft,pVM vm)
{
   #define TEN_MILLION 10000000
   int64_t     t;
   t = ((int64_t)(ft->dwHighDateTime)) << 32 | (ft->dwLowDateTime);
   return(floatCreate(((double)t)/TEN_MILLION,vm));
}
static Instance *Clock_processTimes(Instance *self,pVM vm)
{
   FILETIME  creationTime,exitTime,kernelTime,userTime;

   GetProcessTimes(GetCurrentProcess(),
	&creationTime,&exitTime,&kernelTime,&userTime);

   return tupleCreateX(vm,
     _filetimeToSeconds(&userTime,  vm),
     _filetimeToSeconds(&kernelTime,vm),
     ZNIL);
}
#endif

static const PropertyTable clockProperties[] =
{
   "time",		(pProperty)Clock_time,
   "timef",		(pProperty)Clock_timef,
   "localTime",		(pProperty)Clock_localTime,
   "UTC",		(pProperty)Clock_gmTime,
   "timeZone",		(pProperty)Clock_timeZone,
   "runTime",		(pProperty)Clock_clock,
//   "processTimes",	(pProperty)Clock_processTimes,
   0,			0
};


static ZKL_Object ClockObject;
static Instance   Clock;

void clockConstruct(void)
{
   constructObject(&ClockObject,NativeType, clockMethods,clockProperties,0,NoVM);
   ClockObject.name	  = "Clock";
   ClockObject.vaultPath  = "Time";
   ClockObject.isize	  = sizeof(Clock);
   ClockObject.threadSafe = 1;

   instanceInit((Instance *)&Clock,&ClockObject,I_UNTOUCHABLE);
   vaultAdd(0,(Instance *)&Clock,NoVM);
}

/* ******************************************************************** */
/* ***************************** System ******************************* */
/* ******************************************************************** */

	//////////////////////// Sytem Methods /////////////////////

    // System.exit([n | msg])
static Instance *System_exit(Instance *self,pArglist arglist,pVM vm)
{
   int	     n = 0;
   Instance *pi = arglistTryToGet(arglist,0);

//!!! throw something so I can clean up

   if (pi)
   {
      if (TYPEO(pi) == StringType) vmHalt(stringText(pi));
      n = (int)convertToInt(pi,vm);
   }
   if (n == 0) // a nice clean exit so clean up
   {
      //!!!??? kill threads?
      collectGarbage(1,vm); collectGarbage(1,vm);
   }
   exit(n);
}

    // System.system(cmd) -->exit code
static Instance *System_cmd(Instance *self,pArglist arglist,pVM vm)
{
   int s;  // system() returns -1 on error
   s = system(arglistGetOnlyString(arglist,0,"System.cmd",vm));
//!!! if s == -1, look at errno
   return intCreate(s,vm);
}

    // System.getenv([name,[default]])
    // Read only as setenv() isn't thread safe
static Instance *System_getenv(Instance *self,pArglist arglist,pVM vm)
{
   char *name = arglistTryToGetString(arglist,0,"getenv",vm);

   if (name)	// .getenv(name [,default])
   {
      char     *val = getenv(name);
      Instance *dv  = arglistTryToGet(arglist,1);
      if (val) return stringCreate(val,I_OWNED,vm);
      if (dv)  return dv;
      return Void;
   }
   // getenv()
   {
      #ifdef _MSC_VER
         char **ptr = _environ;		// stdlib.h
      #elif defined(__unix__)
         char **ptr = environ;
      #endif
      Instance *env = dictionaryCreate(50,I_OWNED,vm);
      Fence     fence;

      vmSetFence(vm,&fence,0,env);
	 for (; *ptr; ptr++)
	 {
	    char *p  = strchr(*ptr,'=');
	    fence.i1 = stringCreate2(*ptr,p - *ptr,vm);
	    fence.i2 = stringCreate(p+1,I_OWNED,vm);
	    dictionaryAdd(env,fence.i1,fence.i2,vm);
	 }
      vmRemoveFence(&fence,0);
      return env;
   }
}

    // System.loadFile(file path of .zsc file [, arglist = L()])
    // Unix: a \ in name triggers case folding
static Instance *System_loadFile(Instance *self,pArglist arglist,pVM vm)
{
   char	    *name = arglistGetOnlyString(arglist,0,"System.loadFile",vm);
   Instance *ri   = arglistTryToGet(arglist,1);
   pArglist  runConstructor = NoArglist;

   if (ri)
      switch(TYPEO(ri))
      {
      	 case BoolType: if (!BOOLV(ri)) runConstructor = 0;  break;
	 case ListType: case TupleType: runConstructor = ri; break;
	 default:
	    vmThrow(vm,E_TYPE_ERROR,"System.loadFile: Arg 1 is Bool or List");
      }

   return readRootClass(name,0,runConstructor,vm);
}

    /* System.loadFile2(file path of .zsc file
     *  (1) name to search Vault for or Bool/Void		= False,
     *  (2) Bool or path to store loaded class in Vault or Void = False,
     *  (3) runConstructor					= L()
     *  Unix: a \ in name triggers case folding
     */
static Instance *System_loadFile2(Instance *self,pArglist arglist,pVM vm)
{
   char	    *name = arglistGetOnlyString(arglist,0,"System.loadFile2",vm);
   char	    *path = 0;
   pArglist  runConstructor = NoArglist;
   Instance *vi = arglistTryToGet(arglist,1);
   Instance *pi = arglistTryToGet(arglist,2);
   Instance *ri = arglistTryToGet(arglist,3);

   if (ri)
      switch(TYPEO(ri))
      {
      	 case BoolType: if (!BOOLV(ri))  runConstructor = 0;  break;
	 case ListType: case TupleType:  runConstructor = ri; break;
	 default:
	    vmThrow(vm,E_TYPE_ERROR,"System.loadFile2: Arg 3 is Bool or List");
      }

   if (vi && (TYPEO(vi)==StringType) && 
       (vi = vaultFind(stringText(vi),0,vm)))
   {
      // If it is in the vault, the constructor has already been run (I hope)
      // except in the case of scripts, which could bite me in the ass since
      // (eg) fcn var init is not static
      return(vi);
   }

   if (pi)
   {
      if (TYPEO(pi) == StringType)  path = stringText(pi);
      else if (resultToBool(pi,vm)) path = (char *)1;
   }

   return readRootClass(name,path,runConstructor,vm);
}

    // System.loadLibrary(name)
    // Want a single entry point: void *construct(void *vm)
    // http://en.wikipedia.org/wiki/Microsoft_Visual_C++_Name_Mangling
static Instance *System_loadlibrary(Instance *self,pArglist arglist,pVM vm)
{
   typedef void *(*Constructor)(void *);  // actually void *(*Constructor)(pVM)

   // searchForLib() takes care of the case DLLs are not supported
#ifdef _MSC_VER
   HMODULE	shlib;
   Constructor	constructor;
   char	       *name, buf[200];
   unsigned	protocol;
   Instance    *lib;

   name	       = arglistGetOnlyString(arglist,0,"System.loadlibrary",vm);
   shlib       = searchForLib(name,vm);		// throws
   constructor = (Constructor)GetProcAddress(shlib,"construct");
   if (!constructor)      	// try the C++ mangled name
      constructor = (Constructor)GetProcAddress(shlib,
			"?construct@@YAPAXPAX@Z");
   if (!constructor)
   {
      FreeLibrary(shlib);
      sprintf(buf,"System.loadLibrary(%.100s): "
		"Could not find entry point \"construct\"",name);
      vmThrow(vm,E_LOADER_ERROR,buf);
   }
   protocol = (unsigned)constructor(0);	// handshake
   if (protocol != ZKLX_PROTOCOL)
   {
      FreeLibrary(shlib);
      sprintf(buf,"System.loadLibrary(%.120s): "
	   "Wrong protocol (wanted %d, got %u), recompile library!",
	   name,ZKLX_PROTOCOL,protocol);
      vmThrow(vm,E_LOADER_ERROR,buf);
   }
   lib = (Instance *)constructor(vm);
   if (!lib) return Void;
   return lib;

#elif defined(__unix__)

   void	       *shlib;
   Constructor	constructor;
   char	       *name, buf[300];
   unsigned	protocol;
   Instance    *lib;

   name	       = arglistGetOnlyString(arglist,0,"System.loadlibrary",vm);
   shlib       = searchForLib(name,vm);		// throws
   constructor = dlsym(shlib, "construct");
   if (!constructor)      	// try the C++ mangled name
      constructor = dlsym(shlib,"_Z9constructPv");
   if (!constructor)
   {
      sprintf(buf,"System.loadLibrary(%.100s)(%.150s): "
		"Could not find entry point \"construct\"",name,dlerror());
      dlclose(shlib);
      vmThrow(vm,E_LOADER_ERROR,buf);
   }
   protocol = (unsigned)constructor(0);	// handshake
   if (protocol != ZKLX_PROTOCOL)
   {
      dlclose(shlib);
      sprintf(buf,"System.loadLibrary(%.150s): "
	   "Wrong protocol (wanted %d, got %u), recompile library!",
	    name,ZKLX_PROTOCOL,protocol);
      vmThrow(vm,E_LOADER_ERROR,buf);
   }
   lib = (Instance *)constructor(vm);
   if (!lib) return Void;
   return lib;
#else
   char *name = arglistGetOnlyString(arglist,0,"loadLibrary",vm);
   printf("System.loadLibrary(%s)\n",name);
   return notImplementedError(self,"loadLibrary",vm);
#endif
}

static Instance *System_cwd(Instance *,pVM);

    // System.chdir, change working directory
    // -->previous directory
static Instance *System_chdir(Instance *self,pArglist arglist,pVM vm)
{
   #if defined(_MSC_VER)
      #define chdir	_chdir
   #endif

   char     *dir = arglistGetOnlyString(arglist,0,"System.chdir",vm);
   Instance *prevDir = System_cwd(self,vm);
   if (chdir(dir))
   {
      char buf[200];
      sprintf(buf,"chdir(%.100s): %s",dir,strerror(errno));
      vmThrow(vm,E_IO_ERROR,buf);
   }
   return prevDir;
}

    // System.popen(cmd,tybe): open a pipe for read or write, 
    //   for a shell and run cmd.
    // -->File
static Instance *System_popen(Instance *self,pArglist arglist,pVM vm)
{
   Instance *doPopen(Instance *cmd, char *type, pVM vm); // file.c
   Instance *cmd  = arglistGetBT(arglist,0,StringType,"System.popen",vm);
   char     *type = arglistGetOnlyString(arglist,1,"System.popen",vm);
   return doPopen(cmd,type,vm);
}

static const MethodTable systemMethods[] =
{
   "toBool",		(pMethod)Bool_soTrue,
   "exit",		(pMethod)System_exit,
   "getenv",		(pMethod)System_getenv,
   "cmd",		(pMethod)System_cmd,
   "popen",		(pMethod)System_popen,
   "loadFile",		(pMethod)System_loadFile,
   "loadFile2",		(pMethod)System_loadFile2,
   "loadLibrary",	(pMethod)System_loadlibrary,
   "chdir",		(pMethod)System_chdir,
   0,			0
};

	//////////////////////// Sytem Properties /////////////////////

    // System.argv
static Instance *System_argv(Instance *self, pVM vm)
   { return argV; }		// in vm.c

    // System.OS
static Instance *System_OS(Instance *self, pVM vm)
{
//_osver, _winmajor, _winminor, _winver, GetVersion, Version Helper functions
//"Windows","Windows_NT"
//"Unix","Linux" | "BSD" | "HPUX" | Minix...  __unix__ (gcc/clang)

   #ifdef _WIN32	// and Windows 64, defines the APP size, not OS
      return kStringCreate("Windows",0,I_OWNED,vm);
   #elif defined(__unix__)	// gcc/clang
      #ifdef __linux__
          return kStringCreate("Unix.Linux",0,I_OWNED,vm);
      #elif defined(__FreeBSD__)
          return kStringCreate("Unix.BSD",0,I_OWNED,vm);
      #elif defined(__minix__)
          return kStringCreate("Minix.Unix.BSD",0,I_OWNED,vm);
      #endif
      return kStringCreate("Unix",0,I_OWNED,vm);
   #endif
   return emptyString;
}

    // System.isWindows
static Instance *System_isWindows(Instance *self, pVM vm)
{
   #ifdef _WIN32	// and Windows 64
      return BoolTrue;
   #endif
   return BoolFalse;
}

    // System.isUnix
static Instance *System_isUnix(Instance *self, pVM vm)
{
   #ifdef __unix__	// gcc/clang
      return BoolTrue;
   #endif
   return BoolFalse;
}

    /* System.classPath
     * If you need to set this, it is just a list so hack away. Of course,
     * that could be a problem if there are other threads involved ...
     */
static Instance *System_classPath(Instance *self,pVM vm) { return classPath; }

    // System.libPath
static Instance *System_libPath(Instance *self,pVM vm) { return libPath; }

    // System.includePath
static Instance *System_includePath(Instance *self,pVM vm)
   { return includePath; }

    // System.pid : system process id
static Instance *System_pid(Instance *self,pVM vm)
   { return intCreate(getpid(),vm); }

    // System.cwd, current working directory
    // -->String, ""
static Instance *System_cwd(Instance *self,pVM vm)
{
#ifdef _MSC_VER
   char buf[MAX_PATH];
   int  n;

   if (!_getcwd(buf,MAX_PATH))
   {
      sprintf(buf,"cwd(%.100s)",strerror(errno));
      vmThrow(vm,E_IO_ERROR,buf);
   }
   n = GetLongPathName(buf,buf,MAX_PATH);
   if (n == 0 || n > MAX_PATH)
   {
      sprintf(buf,"cwd(%d)",GetLastError());
      vmThrow(vm,E_IO_ERROR,buf);
   }
   return stringCreate(buf,I_OWNED,vm);
#elif defined(__unix__)
   #ifndef PATH_MAX	// not on Linux anyway
      #define PATH_MAX 1024
   #endif
   char buf[PATH_MAX];
   if (!getcwd(buf,PATH_MAX))
   {
      sprintf(buf,"cwd(%.100s)",strerror(errno));
      vmThrow(vm,E_IO_ERROR,buf);
   }
   return stringCreate(buf,I_OWNED,vm);	// cache!
#endif
}

static int numCPUs = 0;
static Instance *System_cpuCount(Instance *self,pVM vm)
   { return intCreate(numCPUs,vm); }

#if 0
// HKEY_LOCAL_MACHINE\HARDWARE\DESCRIPTION\System\CentralProcessor
// >>/proc/cpuinfo<<, /sys/devices/system/cpu
// libxpl/xplcpu
static Instance *System_cpuCount(Instance *self,pVM vm)
{
   char *ptr;
   int   chips = 1, cores = 1, logicalCores = 1;

   #if _MSC_VER
      //LPFN_GLPI pi;
      //pi = GetProcAddress(GetModuleHandle(TEXT("kernel32")),"GetLogicalProcessorInformation");
     if (ptr = getenv("NUMBER_OF_PROCESSORS")) cores = atoi(ptr);
   #endif
   return tupleCreateX(vm,intCreate(cores,vm),intCreate(logicalCores,vm),ZNIL);
}
#endif

Instance *Network_hostname(Instance *,pVM);

static const PropertyTable systemProperties[] =
{
   "argv",		(pProperty)System_argv,
   "OS",		(pProperty)System_OS,
   "isWindows",		(pProperty)System_isWindows,
   "isUnix",		(pProperty)System_isUnix,
   "classPath",		(pProperty)System_classPath,
   "libPath",		(pProperty)System_libPath,
   "includePath",	(pProperty)System_includePath,
   "pid",		(pProperty)System_pid,
   "cwd",		(pProperty)System_cwd,
   "cpuCount",		(pProperty)System_cpuCount,
   "hostname",		Network_hostname,
   0,			0
};


static ZKL_Object SystemObject;
static Instance	  System;

void systemConstruct(void)
{
   constructObject(&SystemObject,NativeType, systemMethods,systemProperties,0,NoVM);
   SystemObject.name  = "System";
   SystemObject.isize = sizeof(System);

   instanceInit((Instance *)&System,&SystemObject,I_UNTOUCHABLE);
   vaultAdd(0,(Instance *)&System,NoVM);

#if defined(__unix__) && !defined(__FreeBSD__) && !defined(__minix__)
   {
      cpu_set_t cpuset;	// cpuset_t on FreeBSD
      int s = pthread_getaffinity_np(pthread_self(),sizeof(cpu_set_t),&cpuset);
      if (!s) numCPUs = CPU_COUNT(&cpuset);
   }   
#endif
}

/* ******************************************************************** */
/* *********************** Regular Expressions ************************ */
/* ******************************************************************** */

#include "regex.h"	// prototypes for this code


//#define RE_MAXDFA	768		// amount of space for compiled RE
#define RE_MAXDFA      2768		// amount of space for compiled RE
#define RE_SLOP		 50		// dfa overflow protection
#define DFA_SZ	(RE_MAXDFA + RE_SLOP)
typedef struct
{
   Instance  instance;		// Inherit from Instance
   int	     compiled;
   Instance *matches;			// T(T(n,n),Strings)
   unsigned  n, ns[RE_MAXTAG*2];	// offset,len pairs for matches
   Byte      dfa[1];		// automaton, sized on malloc
} RegExp;	// 920/32, 936/64

static ZKL_Object  RegExpObject;

int regExpID=0;

static void regExpMarker(Instance *self) 
   { instanceMark(((RegExp *)self)->matches); }

	//////////////////////// RegExp Methods /////////////////////

static Instance *regExpCreate(char *pattern, pVM vm)
{
   RegExp *regExp;
   char   *error;
   size_t  sz=DFA_SZ;
   Byte   dfa[DFA_SZ];	// automaton

   error = regExpCompile((UChar *)pattern,dfa,&sz);
   if (error) vmThrow(vm,E_VALUE_ERROR,error);
   regExp = (RegExp *)instanceAllocate(sizeof(RegExp) + sz, &RegExpObject,1,vm);
   regExp->matches  = emptyList;
   regExp->compiled = 1;
   memcpy(regExp->dfa,dfa,sz);
   return addToCollectables((Instance *)regExp,I_OWNED,vm);
}

    // RegExp.create(pattern) -->RegExp
static Instance *RegExp_create(Instance *self,pArglist arglist,pVM vm)
{
   char *pattern = arglistGetOnlyString(arglist,0,"RegExp",vm);
   return regExpCreate(pattern,vm);
}

#if 0
Byte *regExpExtractDFA(Instance *rei)
{
   if(rei && I_OBJ_ID(rei)==regExpID)
   {
      RegExp *re = (RegExp *)rei;
      re->matches = emptyList; re->n = 0;
      return re->dfa;
   }
   return 0;
}
#endif

#if 0
static Instance *_reSearchList(RegExp *self,Instance *list,pVM vm)
{
   int	     i;
   Fence     fence;
   Instance *x, *result = listCreate(0,0x2,I_OWNED,vm);

   vmSetFence(vm,&fence,0,result);
      for (i = 0; (x = listGet(list,i)); i++)
      {
	 if (TYPEO(x) != StringType) continue;
	 if (regExpExec(self->dfa,stringText(x),1,0,0,vm)) listAppend(result,x,vm);
      }
   vmRemoveFence(&fence,0);
   return result;
}
#endif

    // support routine for regex.c : regExpExec()
void regExpFail(char *msg, UChar op, void *vm)
   { vmThrow(vm,E_VALUE_ERROR,msg); }

    // RegExp.search(string | data ,move=False,startAt=0)
    // -->Bool, sets self.matched
    // Not thread safe
static Instance *RegExp_exec(RegExp *self,pArglist arglist,pVM vm)
{
   UChar    *text;
   Instance *searching = arglistGet(arglist,0,"RegExp.search",vm);
   int	     move      = arglistTryToGetBool(arglist,1,0,"RegExp.search",vm);
   int64_t   startAt   = arglistTryToGetIntD(arglist,2,0,"RegExp.search",vm);
   UChar    *tags[2 * RE_MAXTAG];
   size_t     sz;

   	// Note: if RE didn't compile, RegExp wasn't created

   self->matches = emptyList; self->n = 0;
   switch (TYPEO(searching))
   {
      case DataType:	// NOT thread safe (DataType)
	 text = (UChar *)dataText(searching,&sz);
	 if(0<=startAt && startAt<sz) text += startAt; 
	 else 	       { startAt = 0; text = (unsigned char *)""; }
	 break;
      case StringType:
	 text = (UChar *)stringText(searching);
	 if(startAt)
	 {
	    sz=strlen((char *)text);
	    if(0<=startAt && startAt<sz) text += startAt; 
	    else 	  { startAt = 0; text = (unsigned char *)""; }
	 }
	 break;
      default:
      {
	 char buf[100];
	 sprintf(buf,"RegExp.search: Can't handle %s",iname(searching));
	 vmThrow(vm,E_TYPE_ERROR,buf);
      }
   }

//   if(startAt) startOfLine = 0;		//!!!?????

   if(regExpExec(self->dfa,text,1,move,tags,vm))	// throws
   {
      unsigned  i,n,x,y, *ns = self->ns;
      UChar    *a = tags[0], *b = tags[RE_MAXTAG];
      Instance *matches;
      Fence     fence;

      for (n=0; n < RE_MAXTAG && tags[n]; n++) ; 
      self->n = n;  // n > 0
      matches = tupleCreate(n,I_OWNED,vm);
      vmSetFence(vm,&fence,0,matches);
         *ns++ = x = (a - text); *ns++ = y = (b - a);
	 tupleAppend(matches,	// T(T(offset,num) : total match
	     tupleCreateX(vm, intCreate(x + startAt,vm),intCreate(y,vm), ZNIL));
	 for (i = 1; --n; i++)
	 {
	    UChar *a = tags[i], *b = tags[i + RE_MAXTAG];
	    *ns++ = (a - text); *ns++ = y = (b - a);
	    tupleAppend(matches,stringCreate2((char *)a, y, vm));
	 }
      vmRemoveFence(&fence,0);
      self->matches = matches;
      return BoolTrue;
   }
   return BoolFalse;
}

#if 0
    // RegExp.glob(src,callBack,startAt=0,badReturn=Void.Skip)
static Instance *RegExp_glob(RegExp *self,pArglist arglist,pVM vm)
{
   UChar    *text;
   Instance *searching = arglistGet(arglist,0,"RegExp.search",vm);
   Instance *f         = arglistGet(arglist,1,"RegExp.search",vm);
   Instance *r = VoidSkip;
   int64_t   startAt   = arglistTryToGetIntD(arglist,2,0,"RegExp.search",vm);
   UChar    *tags[2 * RE_MAXTAG];
   size_t     sz;
   int	      startOfLine = 1, buffer = 0;
   MLIST(osz,2);

   	// Note: if RE didn't compile, RegExp wasn't created

   self->matches = emptyList; self->n = 0;
   switch (TYPEO(searching))
   {
      case DataType:	// NOT thread safe (DataType)
	 text = (UChar *)dataText(searching,&sz);
	 if(0<=startAt && startAt<sz) text += startAt; 
	 else 	       { startAt = 0; text = (unsigned char *)""; }
	 break;
      case StringType:
	 text = (UChar *)stringText(searching);
	 if(startAt)
	 {
	    sz=strlen((char *)text);
	    if(0<=startAt && startAt<sz) text += startAt; 
	    else 	  { startAt = 0; text = (unsigned char *)""; }
	 }
	 break;
      default:
      {
	 char buf[100];
	 sprintf(buf,"RegExp.search: Can't handle %s",iname(searching));
	 vmThrow(vm,E_TYPE_ERROR,buf);
      }
   }

//   if(startAt) startOfLine = 0;	!!!?????

   if(TYPEO(f)==ListType){ listClear(f,vm); buffer = 1; r = f; }
   while(1)
   {
      if(regExpExec(self->dfa,text,startOfLine,1,tags,vm))	// throws
      {
	 UChar    *a = tags[0], *b = tags[RE_MAXTAG];
	 unsigned  sz;

	 startOfLine = 0;

	 if (tags[1]) { a = tags[1]; b = tags[1 + RE_MAXTAG]; }
	 sz = (b - a);

	 if(buffer) listAppend(f,stringCreate2((char *)a,sz,vm),vm);
	 else
	 {
	    mlistBuild(osz,
	       stringCreate2((char *)a,sz,vm), ZNIL);
	    r = objectRun(f,(Instance *)osz,0,vm);
	}
	text = tags[RE_MAXTAG];
      }
      else break;
   }
   return r;
}
#endif

static Instance *_regetter(Instance *self,size_t n,void *pptxt,size_t _,pVM vm)
{
   Instance  *r = 0;
   UChar     *text=*(UChar **)pptxt, *tags[2 * RE_MAXTAG];

   if(regExpExec(((RegExp *)self)->dfa,text,(n==0),1,tags,vm)) // throws
   {
      UChar *a = tags[0], *b = tags[RE_MAXTAG];
      if (tags[1]) { a = tags[1]; b = tags[1 + RE_MAXTAG]; }
      r = stringCreate2((char *)a,b-a,vm);
      *(UChar **)pptxt = tags[RE_MAXTAG];
   }
   return r;
}

    // regExp.pump(src,sink,...)
static Instance *RegExp_pump(Instance *self,pArglist arglist,pVM vm)
{
   UChar    *text;
   Instance *searching = arglistGet(arglist,0,"RegExp.pump",vm);

   ((RegExp *)self)->matches = emptyList; ((RegExp *)self)->n = 0;
   switch (TYPEO(searching))
   {
      case DataType:	// NOT thread safe (DataType)
	 text = (UChar *)dataText(searching,0);
	 break;
      case StringType:
	 text = (UChar *)stringText(searching);
	 break;
      default:
      {
	 char buf[100];
	 sprintf(buf,"RegExp.pump: Can't handle %s",iname(searching));
	 vmThrow(vm,E_TYPE_ERROR,buf);
      }
   }
   return pump((Instance *)self,PMP_OK2CNT,_regetter,(void *)&text,arglist,1,vm);
}

    // RegExp.matches(string,move=False), a yes/no RegExp.search(string)
    // -->Bool
    // Not thread safe
static Instance *RegExp_matches(RegExp *self,pArglist arglist,pVM vm)
{
   char  *text = arglistGetOnlyString(arglist,0,"RegExp.matches",vm);
   int	  s, move = arglistTryToGetBool(arglist,1,0,"RegExp.matches",vm);

   	// Note: if RE didn't compile, RegExp wasn't created

   self->matches = emptyList; self->n = 0;
   s = regExpExec(self->dfa,(UChar *)text,1,move,0,vm);	// throws
   return boolCreate(s);
}

#if 0
    // RegExp.replace(pattern, srcText, fcn(matchedText,matched)]-->text)
static Instance *RegExp_replace(Instance *self,pArglist arglist,pVM vm)
{
   MLIST(mlist,5);
   mlistBuild(mlist,((RegExp *)self)->matches,ZNIL);
   mlistExtendN(mlist,arglist,0,2);
   return fcnRunFromClass(Utils_Helpers,"reSub",(Instance *)mlist,vm);
#endif

    // RegExp.replace(text, fcn(matchedText,matched)]-->text  [, count])
    // Replace all ocurrences of \1 with replaceWith(matchedText)
    // RegExp(".*(foo|bar)").replace("That is a foo bird", fcn(m){ m.toUpper() })
    //   --> "That is a FOO bird"
    // re:=RegExp(pat); 
    // if(re.search(text)) text.replace(fcn(re.matched[1]), count)
    // else text
static Instance *RegExp_replace(Instance *self,pArglist arglist,pVM vm)
{
   MLIST(mlist,5);
   mlistBuild(mlist,self,ZNIL);
   mlistExtendN(mlist,arglist,0,4);
   return fcnRunFromClass(Utils_Helpers,"reReplace",(Instance *)mlist,vm);
}

static const MethodTable regExpMethods[] =
{
   "toBool",	Bool_soTrue,
   "create",	RegExp_create,
   "search",	(pMethod)RegExp_exec,
   "matches",	(pMethod)RegExp_matches,
   "replace",	RegExp_replace,
   "pump",	RegExp_pump,
   0,		0
};


	//////////////////////// RegExp Properties /////////////////////
    // RegExp.matched -->T(T(offset,len), Strings)
    // Not thread safe
static Instance *RegExp_matched(RegExp *self, pVM vm)
   { return self->matches; }

    // RegExp.matchedNs-->T(T(offset,len), T(O,L)...)
    // Not thread safe
static Instance *RegExp_matchedNs(RegExp *self, pVM vm)
{
   unsigned  n = self->n, *ns = self->ns;
   Instance *matches;
   Fence     fence;

   if (!n) return emptyList;
   matches = tupleCreate(n,I_OWNED,vm);
   vmSetFence(vm,&fence,0,matches);
      for(; n--; ns += 2)	// --> Tuple of Tuples of int pairs
	 tupleAppend(matches,
	    tupleCreateX(vm, intCreate(*ns,vm),intCreate(ns[1],vm), ZNIL));
   vmRemoveFence(&fence,0);
   return matches;
}

static Instance *RegExp_dfa(RegExp *self, pVM vm)
{
   dfaDump(self->dfa,1);
   return Void;
}

static const PropertyTable regExpProperties[] =
{
   "matched",	(pProperty)RegExp_matched,
   "matchedNs",	(pProperty)RegExp_matchedNs,
   "dfa",	(pProperty)RegExp_dfa,
   0,		0
};


//static pMethod in_regExp_methods(Instance *ignore, register char *str);

void regexpConstruct(void)
{
   constructObject(&RegExpObject,NativeType, regExpMethods,regExpProperties,0,NoVM);
   RegExpObject.name	     = "RegExp";
   RegExpObject.magicMarker  = regExpMarker;
   RegExpObject.isize	     = sizeof(RegExp);
//   RegExpObject.methodSearch = in_regExp_methods;
   RegExpObject.createReturnsSelf = 1;

   regExpID = RegExpObject.id;

#if 0
   {
      UChar dfa[DFA_SZ];	// automaton
      regExpCompile(0,dfa);	// initialize some static stuff
      // that pattern is an error but it gets the initialization done
   }
#endif

   vaultAddData("RegExp",methodCreate(Void,0,RegExp_create,NoVM),NoVM);
}

/* ******************************************************************** */
/* *************************** Utils.MD5 ****************************** */
/* ******************************************************************** */

    // MD5(text) --> String
static Instance *MD5_calc(Instance *self,pArglist arglist,pVM vm)
{
   MD5_CTX	context;
   Byte		digest[16], hash[40];
   Instance    *text = arglistGet(arglist,0,"MD5.calc",vm);
   char        *message;
   int		i,n;
   size_t	len;

   MD5Init(&context);
   switch (TYPEO(text))
   {
      case DataType:
	 message = (char *)dataText(text,&len);
	 break;
      case StringType:
	 message = stringText(text); len = strlen(message);
	 break;
      default:
      {
	 char	buf[100];
	 sprintf(buf,"MD5: Can't handle %s",iname(text));
	 vmThrow(vm,E_TYPE_ERROR,buf);
      }
   }
   MD5Update(&context,message,len);
   MD5Final(digest,&context);

   for (i = n = 0; i < 16; i++, n += 2)
      sprintf((char *)&hash[n],"%02x", digest[i]);
   return stringCreate((char *)hash,I_OWNED,vm);
}

void md5Construct(void)
   { vaultAddData("Utils.MD5",methodCreate(Void,0,MD5_calc,NoVM),NoVM); }


/* ******************************************************************** */
/* ***************************** Language ***************************** */
/* ******************************************************************** */

////////////////////// Misc Properties //////////////////////////

    // Language.name
static Instance *Language_name(Instance *self, pVM vm)
   { return kStringCreate(LANGUAGE_NAME,0,I_OWNED,vm); }

   // Language.authors
   // NOTE: You may add authors but you CAN NOT remove an author unless
   // their contributions are removed in their entirety.
static Instance *Language_authors(Instance *self, pVM vm)
{
   return createTupleOfStrings(vm,"Craig Durland","http://www.zenkinetic.com/",
          (char *)0); 
}

	// Language.email
static Instance *Language_email(Instance *self, pVM vm)
   { return kStringCreate("craigd@zenkinetic.com",0,I_OWNED,vm); }

	// Language.version
static Instance *Language_version(Instance *self, pVM vm)
{
   Instance *list = tupleCreate(4,I_OWNED,vm);
   Fence fence;

   vmSetFence(vm,&fence,0,list);
      tupleAppend(list,intCreate(VERSION_MAJOR,  vm));
      tupleAppend(list,intCreate(VERSION_MINOR,  vm));
      tupleAppend(list,intCreate(VERSION_MYSTERY,vm));
      tupleAppend(list,kStringCreate(RELEASE_DATE,0,I_OWNED,vm));
   vmRemoveFence(&fence,0);
   return list;
}

	// Language.versionString
static Instance *Language_versionString(Instance *self, pVM vm)
{
   char buf[200];
   sprintf(buf,"%s %d.%d.%d, released %s", LANGUAGE_NAME,
   	VERSION_MAJOR,VERSION_MINOR,VERSION_MYSTERY, RELEASE_DATE);
   return stringCreate(buf,I_OWNED,vm);
}

	// Language.website
static Instance *Language_website(Instance *self, pVM vm)
   { return kStringCreate("http://www.zenkinetic.com/",0,I_OWNED,vm); }

	// Language.license
static Instance *Language_license(Instance *self, pVM vm)
{
   return kStringCreate(
//   *result = kStringCreate(
"The license for the " LANGUAGE_NAME " Programming Language executable.\n\n"
"This is basically the zlib license and covers only this program.\n"
"Not covered:\n"
"  - Source code.\n"
"  - Programs produced by this program (they are yours).\n"
"  - [Shared] libraries (such as extensions) used by this program.\n"
"\n"
"Copyright (c) 2007,2008-17,2018 Craig Durland\n"
"\n"
"This program is provided 'as-is', without any express or implied\n"
"warranty.  In no event will the author(s) be held liable for any damages\n"
"arising from the use of this program.\n"
"\n"
"Permission is granted to anyone to use this program for any purpose,\n"
"including commercial applications and redistribute it freely, subject to\n"
"the following restrictions:\n"
"\n"
"1) The origin of this program must not be misrepresented; you must not\n"
"   claim that you wrote the original program.  If you use this program\n"
"   in a product, an acknowledgment in the product documentation would be\n"
"   appreciated but is not required.\n"
"2) You may not distribute an altered version of this program.  You may,\n"
"   however, build a new program from the source code and distribute\n"
"   that, subject to the source code license(s).\n"
"3) This notice may not be removed or altered.\n"
"\n"
"Please send me bug reports, fixes and enhancements so I can continue to\n"
"improve " LANGUAGE_NAME ".\n"
"Thank you.\n"
"\n"
"Craig Durland	craigd@zenkinetic.com\n"
   ,0,I_OWNED,vm);
}

static const PropertyTable languageProperties[] =
{
   "name",		(pProperty)Language_name,
   "authors",		(pProperty)Language_authors,
   "version",		(pProperty)Language_version,
   "versionString",	(pProperty)Language_versionString,
   "website",		(pProperty)Language_website,
   "email",		(pProperty)Language_email,
   "license",		(pProperty)Language_license,
   0,			0
};


static ZKL_Object LanguageObject;
static Instance	  Language;

void languageConstruct(void)
{
   constructObject(&LanguageObject,NativeType, 0,languageProperties,0,NoVM);
   LanguageObject.name	     = "Language";
   LanguageObject.isize	     = sizeof(Language);
   LanguageObject.threadSafe = 1;

   instanceInit((Instance *)&Language,&LanguageObject,I_UNTOUCHABLE);
   vaultAdd(0,(Instance *)&Language,NoVM);
}


/* ******************************************************************** */
/* ******************************** Ref ******************************* */
/* ******************************************************************** */

//!!!???? A full proxy + set/ref? Use Deferred Ops

// changes are thread safe

typedef struct
{
   BInstance  instance;	// iflag == 1 means ref is a C data blob
   Instance  *ref;	// to object of interest
} Ref;	// 8 bytes/32, 16/64

static ZKL_Object     RefObject;
static IBucketHeader *refBuckets = 0;

static void refMarker(Instance *_self)
{
   Ref *self = (Ref *)_self;
   if (!self->instance.iflag)
      instanceMark(self->ref); 	// !!!DIP? what if value changes right now?
}

    // Ref.set(i) -->self
Instance *refSet(Instance *self, Instance *value, pVM vm)
{
   	// might as well be thread safe
   if (!self->iflag)
   {
      CAP_SET(&((Ref *)self)->ref,value);
      instanceIsOrphan(value);		// DIP1
   }
   return self;
}
static Instance *Ref_set(Instance *self, pArglist arglist, pVM vm)
{
#if 0
   	// might as well be thread safe
   if (!self->iflag)
      CAP_SET(&((Ref *)self)->ref,arglistGet(arglist,0,".Ref",vm));
   return self;
#else
   return refSet(self,arglistGet(arglist,0,".Ref",vm),vm);
#endif
}

    // Ref.inc(x=1) --> prev value
    // We assume Ref.value is an integer (it can be a PtrInt)
static Instance *Ref_incN(Instance *self, pArglist arglist, pVM vm)
{
   Instance *v=((Ref *)self)->ref, *r, *x=arglistTryToGet(arglist,0);

   if (self->iflag) return Void;
   if (x && TYPEO(v)==IntType)  // Ref("HOHO").inc(4) gets other path
   {
   #if USE_POINTER_INTS
      ZKL_Int zi64;
      Instance *vi = decantInt(v,&zi64);  // first arg to add must be Instance
   #endif
      r = OBJECT1(One)->add(vi,x,vm);
   }
   else r = OBJECT1(One)->add(One,v,vm);	// throws
   	// might as well be thread safe
   CAP_SET(&((Ref *)self)->ref,r);
   return v;
}

    // Ref.inc(1) --> prev value
static Instance *Ref_inc(Instance *self, pArglist arglist, pVM vm)
   { return Ref_incN(self,NoArglist,vm); }

    // Ref.dec() --> prev value
static Instance *Ref_dec(Instance *self, pArglist arglist, pVM vm)
{
   Instance *v = ((Ref *)self)->ref;
   Instance *r;

   if (self->iflag) return Void;
   r = OBJECT1(MinusOne)->add(MinusOne,v,vm);	// throws
   	// might as well be thread safe
   CAP_SET(&((Ref *)self)->ref,r);
   return v;
}

    // Ref.apply(f) --> set(f(value))
    // --> self
static Instance *Ref_apply(Instance *self, pArglist arglist, pVM vm)
{
   Instance *f = arglistGet(arglist,0,"Ref.apply",vm);
   Instance *v = ((Ref *)self)->ref;
   Instance *r;
   MLIST(mlist,12);

   if (self->iflag) return Void;

   if (listLen(arglist,vm) > 11)
      vmThrow(vm,E_ASSERTION_ERROR,"Ref.apply: No more than 10 parameters");

   mlistBuild(mlist,v,ZNIL); mlistExtend(mlist,arglist,1,12);
   r = objectRun(f,(Instance *)mlist,0,vm);

//   r = objectRun(f,(Instance *)mlistBuild(mlist,v,ZNIL),0,vm);
   	// might as well be thread safe
   CAP_SET(&((Ref *)self)->ref,r);
   return self;
}

    // Ref.toBool() --> ref.value.toBool()
static Instance *Ref_toBool(Instance *self, pArglist arglist, pVM vm)
{
   Instance *v = ((Ref *)self)->ref;   // let Ref(Ref(123)) recurse
   if (self->iflag) return BoolFalse;
   return resultToBool(v,vm) ? BoolTrue : BoolFalse;
}

static const MethodTable refMethods[] =
{
   "inc",	Ref_inc,
   "dec",	Ref_dec,
   "set",	Ref_set,
   "incN",	Ref_incN,
   "apply",     Ref_apply,
   "toBool",	Ref_toBool,
   0,		0
};

    // Ref.ref -->i
static Instance *Ref_ref(Instance *self, pVM vm)
{
   if (self->iflag) return Void;
   return ((Ref *)self)->ref;
}

void *crefRef(Instance *self) { return ((Ref *)self)->ref; }

static const PropertyTable refProperties[] =
{
   "ref",	Ref_ref,
   "value",	Ref_ref,
   0,		0
};

int refID = 666;	// not valid until RefObject created

//static pMethod in_ref_methods(Instance *ignore, register char *str);

    // Ref.create(i) -->Ref, Strong Refs
static Instance *Ref_create(Instance *self,pArglist arglist,pVM vm)
{
   Instance *i = arglistTryToGet(arglist,0);
   Ref      *ref;

#if 0
   if (!refBuckets)	// Ref not constructed yet, !!!NOT thread safe
   {
      constructObject(&RefObject,NativeType, refMethods,refProperties,0,NoVM);
      RefObject.name         = "Ref";
      RefObject.vaultPath    = "";
      RefObject.isize        = sizeof(Ref);
      RefObject.magicMarker  = refMarker;
      RefObject.isBInstance  = 1;
      RefObject.threadSafe   = 1;
      RefObject.createReturnsSelf = 1;

      refID = RefObject.id;

      refBuckets = ibucketShare(&RefObject,0);	// Lock or KString
   }
#endif

   ref = (Ref *)ibucketAllocate(refBuckets,&RefObject,I_OWNED,1,vm);
   ref->ref = i ? i : Void;
      // in case GC is happening, make sure ref survives this cycle
   instanceIsOrphan(i);	// don't gc ref val before ref is markable
   return containerIsCooked(refBuckets,(Instance *)ref,I_OWNED);
}

   // Note! You have to protect data (I won't)
   // used by pump & Void.Recurse
Instance *refCreate(void *data,int dataIsBlob,pVM vm)
{
   Instance *ref;
   ref = Ref_create(Void,NoArglist,vm);
   if(dataIsBlob) ref->iflag = 1;    // not supposed mess with Instance header
   ((Ref *)ref)->ref = (Instance *)data; // but OK because in method, no GC now
   return ref;
}

static void RefConstruct(void)
{
#if 1
   constructObject(&RefObject,NativeType, refMethods,refProperties,0,NoVM);
   RefObject.name         = "Ref";
   RefObject.vaultPath    = "";
   RefObject.isize        = sizeof(Ref);
   RefObject.magicMarker  = refMarker;
   RefObject.isBInstance  = 1;
   RefObject.threadSafe   = 1;
   RefObject.createReturnsSelf = 1;

   refID = RefObject.id;

   refBuckets = ibucketShare(&RefObject,0);	// Lock or KString
#endif

   vaultAddData("Ref",methodCreate(Void,0,Ref_create,NoVM),NoVM);
}

/* ******************************************************************** */
/* *********************************  ********************************* */
/* ******************************************************************** */

void miscConstruct(void)
{
   consoleConstruct();
   clockConstruct();
   systemConstruct();
   regexpConstruct();
   md5Construct();
   languageConstruct();
   RefConstruct();
}

////////////////////////////////////////////// RegExp
// zkl extractTable -n regExpMethods < miscObj.c | gperf | zkl gperf -i regExp



////////////////////////////////////////////// Ref
// zkl extractTable -n refMethods < miscObj.c | gperf | zkl gperf -i ref

