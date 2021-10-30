/* util.c : A collection of useful stuff
 * 
 * Copyright (c) 2006,2007,2008-12,2013 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#define _GNU_SOURCE	// for strcasestr on Linux
#include <stdio.h>
#include <string.h>
#ifdef __unix__
   #include <dlfcn.h>		// dlopen
   #include <sys/stat.h>	// stat
   #include <errno.h>
#endif

#define __NOT_A_DLL
#define __LIST_INTERNALS
#define __FCN_INTERNALS

#include "zklObject.h"
#include "zklFcn.h"
#include "zklClass.h"
#include "zklData.h"	// ZAggie
#include "zklList.h"
#include "zklMethod.h"
#include "zklString.h"
#include "zklUtil.h"

    // Open foo.zkl, foo.zsc, etc, searching System.classPath as needed
    // Used by the loader in readRootClass()
    // Hmmm, only used by loader.c:readRootClass()
#ifdef _MSC_VER	// Windoz

FILE *searchForFile(char *fileName, pVM vm)
{
   FILE	    *f = fopen(fileName,"rb");
   int	     n;
   Instance *pathList = classPath;
   Instance *path;

   if (f) return f;

   	// didn't find file, poke around file system
   if (*fileName != '/' && *fileName != '\\')	// "/foo" --> no change-o
   {
      for (n = 0; (path = listGet(pathList,n)); n++)
      {
	 Instance *name = stringCat(vm,stringText(path),"/",fileName,(char *)0);
	 f = fopen(stringText(name),"rb");
	 if (f) return f;
      }
   }
   return 0;
}

#else	// Unix like things

static int _foundFile(char *fileName, void *fxp)
{
   FILE **f = (FILE **)fxp;
   *f = fopen(fileName,"rb");
   return 2;		// stop searching
}
static FILE *_globIt(char *fileName)
{
   FILE *f = 0;
   if(0 != strchr(fileName,'\\'))  // sleeze ball flag: import
      fxpand(fileName, FXP_NO_DIRS | FXP_ONLY_ONE | FXP_IGNORE_CASE,
		       _foundFile,&f);
   else return fopen(fileName,"rb");
   return f;
}

FILE *searchForFile(char *fileName, pVM vm)
{
   FILE	    *f;
   int	     n;
   Instance *pathList = classPath;
   Instance *path;

   f = _globIt(fileName);
   if (f) return f;

   	// didn't find file, poke around file system
   if (*fileName != '/')	// "/foo" --> no change-o
   {			// "Compiler/Compiler\.zsc" needs to match compiler.zsc
      for (n = 0; (path = listGet(pathList,n)); n++)
      {
	 Instance *name =  // too lazy to do math for strcat
	     stringCat(vm,stringText(path),"/",fileName,(char *)0);
	 char     *fname = stringText(name);
	 f = _globIt(fname);
	 if (f) return f;
      }
   }
   return 0;
}
#endif

/* open foo.[dll | lib], searching $zklLibPath/System.libPath as needed
     * You don't need to include the extension, Windows will do that.
     */
#ifdef _MSC_VER
//SetDllDirectory
HMODULE searchForLib(char *libName, pVM vm)
{
   HMODULE   shlib;
   int	     i, errCode;
   Instance *path;
   char      buf[400];

   shlib = LoadLibrary(libName);
   if (shlib) return shlib;

   if (*libName != '/' && *libName != '\\')	// "/foo" --> no change-o
      for (i = 0; path = listGet(libPath,i); i++)
      {
	 Instance *name = stringCat(vm,stringText(path),"/",libName,(char *)0);
	 shlib = LoadLibrary(stringText(name));
	 if (shlib) return shlib;
	 errCode = GetLastError();
	 if (errCode != 126)	// ERROR_MOD_NOT_FOUND
	 {
	    sprintf(buf,"loadLibrary: %.200s (%d)",libName,errCode);
	    vmThrow(vm,E_LOADER_ERROR,buf);
	 }
      }

   sprintf(buf,"Could not find/load library %.200s (%d)",libName,GetLastError());
   vmThrow(vm,E_NOT_FOUND,buf);

   return 0;	// doesn't get here
}

#elif defined(__unix__)

    // might alloc/gc, 
    // sets dlerror eg file could not be found, was not readable,
    //      had the wrong format, or caused errors during loading
static void *tryToOpenLib(char *libName, pVM vm)
{
   void *shlib;
   struct stat	stats;

   shlib = dlopen(libName,RTLD_LAZY); // RTLD_NOW, RTLD_LAZY
   if (shlib) return shlib;	      // errno not set, dlerror() is

#if 0
   errstr = dlerror();	// clears on reading
#ifdef(__linux__)
   if (!strcasestr(errstr,"cannot open"))	// like a linker error
#elif defined(__freeBSD__)
   if (!strstr(errstr,"not found"))	// like a linker error
#endif
#endif
	// if lib exists but can't open, throw error
	// dlopen() doesn't set errno, bastarts
   if (stat(libName,&stats) && errno!=ENOENT)
   {
      char buf[400];
      sprintf(buf,"loadLibrary: %.150s (%.100s)",libName,dlerror());
      vmThrow(vm,E_LOADER_ERROR,buf);
   }
   return 0;	// doesn't get here
}
static void *_tryThisOne(char *libName, pVM vm)
{
   void	  *shlib;
   size_t  n;

	// Linix doesn't search $LD_LIBRARY_PATH
   shlib = tryToOpenLib(libName,vm);
   if (shlib) return shlib;	      // errno not set, dlerror() is

	// look for name.so, if not already named name.so
	// !!! so.1, so.2,... fxpand?
   n = strlen(libName);
   if ((n>3) && strcmp(libName + n - 3,".so"))
   {
      Instance *path = stringCat(vm,libName,".so",(char *)0);
      shlib = tryToOpenLib(stringText(path),vm);
      if (shlib) return shlib;
   }
   return 0;
}
void *searchForLib(char *libName, pVM vm)
{
   void	    *shlib;
   int	     i;
   Instance *path;
   char      buf[400];

   #ifndef ZSHLIB
      sprintf(buf,"Shared libraries not supported in stand alone zkl, "
	"\"make lib\" (%.200s)",libName);
      vmThrow(vm,E_NOT_IMPLEMENTED,buf);
   #endif

   if ((shlib = _tryThisOne(libName,vm))) return shlib;

	// "/foo", "\foo", "./foo" --> no change-o
   if (*libName!='/' && *libName!='\\' && *libName!='.')
   {
      Fence  fence;
      
      for (i = 0; (path = listGet(libPath,i)); i++)
      {
	 Instance *name = stringCat(vm,stringText(path),"/",libName,(char *)0);
	 vmSetFence(vm,&fence,0,name);	// protect name from _tryThisOne()
	    shlib = _tryThisOne(stringText(name),vm);
	 vmRemoveFence(&fence,0);

	 if (shlib) return shlib;
      }
   }

   sprintf(buf,"Could not find library %.200s",libName);
   vmThrow(vm,E_NOT_FOUND,buf);

   return 0;	// doesn't get here
}
#endif

unsigned pumpId, zipWithOId;	// set in object.c

   // X.zipWith(f,obj...) --> Utils.Helpers.zipWithO(f,self,obj...)
   // f is arglist[0]
Instance *zipWithObjs(Instance *self,pArglist arglist,pVM vm)
{
   MLIST(mlist,30);
   mlistCopy(mlist,arglist,0,1);         // copy f
   if(self) mlistAppendI(mlist,self,30); // copy self
   mlistExtend(mlist,arglist,1,30);      // copy the rest of arglist
   return fcnRunFromClassId(Utils_Helpers,zipWithOId,(Instance *)mlist,vm);
}
    
   // X.zip(obj...) --> Utils.Helpers.zipWithO(f,self,obj...)
   // f might be T (-->T.create), String (-->String.create), etc 
Instance *zipObjs(Instance *self,Instance *f,pArglist arglist,pVM vm)
{
   MLIST(mlist,30);
   mlistBuild(mlist,f,ZNIL); 
   if(self) mlistAppendI(mlist,self,3);
   mlistExtend(mlist,arglist,0,30);
   return fcnRunFromClassId(Utils_Helpers,zipWithOId,(Instance *)mlist,vm);
}

#if 0  /// NOT worth it!
Instance *zipObjs(Instance *self,pMethod f,pArglist arglist,pVM vm)
{
   #define MAX_Is 30
   int       n,sz,a;
   size_t    az;
   Instance  i, **args =listTable(arglist,&az);
   ZGetter  *getter, *getters[MAX_Is];
   void	    *Xs[MAX_Is];
   ZAggie    sink;
   Fence     fence;
   MLIST(mlist,MAX_Is);

   mlistBuild(mlist,f,self,ZNIL); 
   for (n=0; n<az && n<MAX_Is; n++)
   {
      i = args[n];
      getter = OBJECTI(i)->getGetter(i,&X,vm);
      if(!getter) return emptyTuple;  // or ignore or error?
      getters[n] = getter; Xs[n] = X;
   }
   vmSetFence(vm,&fence,0,aggieInit(&sink,ZA_LIST,vm));
      sz = n;
      while(1)
      {
	 initializeMList(&mlist);
	 for(n=0; n<sz; n++)
	 {
	    i = getters[n](args[n],n,??X,aggie->sz,vm);
	    if(!i) goto done;
	    mlistAppendI(&mlist,i,MAX_Is);
	 }
	 aggieWrite(&sink,f(??,(Instance *)mlist,vm),vm);
      }
   done: ;
   vmRemoveFence(&fence,0);
   return aggieClose(&sink,0,vm);
}
#endif

/* ******************************************************************** */
/* ******************* Integer Based Random Numbers ******************* */
/* ******************************************************************** */

   /* Here's a typical (portable) linear-congruential pseudo-random number
    * generator, taken from the draft ANSI C standard; rand() returns an
    * integer uniformly distributed from 0 through 32767 (inclusive), and
    * srand() is used to initialize a sequence to a particular seed, or to
    * avoid getting a predictable sequence (by setting a seed based on some
    * system variable such as process ID or time-of-day).
    */
//!!!replace this with prng_xor128 in number.c
// although I don't think this one cares about race conditions
static unsigned long int next = 1;
int irand(void)
{
   next = next * 1103515245 + 12345;
   return (unsigned int)(next/65536) % 32768;
}
void isrand(unsigned int seed) { next = seed; }

#if 0
/* ******************************************************************** */
/* **************************** Trip Wires **************************** */
/* ******************************************************************** */
// Tripwire are single use events for a group of threads

void tripWireRootInit(TripWireRoot *root)
{
   root->wire = 0;
   acIntInit(&root->wires.tripWire1,0);	// event is "off"
   acIntInit(&root->wires.tripWire2,0);	// event is "off"
   spinLockInit(&root->lock);
//   root->next = 0;
}

    // Prepend a new TripWire and wait
    // This [thread] sits here until another thread hits the tripwire
void setTripWire(TripWireRoot *root)
{
#if __unix__	// Pthreads
   ACInt *activeTripWire;

   spinLockAcquire(&root->lock);
      activeTripWire = &root->wires.wires[root->wire];
   spinLockRelease(&root->lock);

activeTripWire->n++; printf("1: #%d  %d\n",root->wire,activeTripWire->n);
   acIntWaitFor(activeTripWire,666,1,0);	// wait for event to happen

#else
   TripWire tripWire;

   CAI_INIT(&tripWire.tripWire);
   spinLockAcquire(&root->lock);
      tripWire.next = root->next;
      root->next    = &tripWire;
   spinLockRelease(&root->lock);

   intWaitFor(&tripWire.tripWire,1,1,0,NoVM);	// don't interrupt
#endif
}

    // Signal event has happened for this group of threads
    // Event stays signaled and should not be reused
    // Root/tripwire should be reused.
void tripTheLightFantastic(TripWireRoot *root)
{
#if __unix__	// Pthreads
   ACInt *activeTripWire, *nextWire;
   int    wire,n;
   spinLockAcquire(&root->lock);
      wire	     = root->wire;
      activeTripWire = &root->wires.wires[wire];
      root->wire     = (wire + 1)%2;		// switch trip wires
      nextWire	     = &root->wires.wires[root->wire];
      CAI_ZERO(&nextWire->n);	// don't send signal
   spinLockRelease(&root->lock);
//printf("2: #%d   %d   %d\n",wire,activeTripWire->n,activeTripWire->cond.__data.__nwaiters);
//   AC_INT_ONE(activeTripWire);	// wake the waiter(s) in this group
if (AC_INT_VALUE(activeTripWire))
   acIntSet(activeTripWire,666);	// wake the waiter(s) in this group

#else
   TripWire *ptr, *next;
   spinLockAcquire(&root->lock);
      ptr = root->next;
      root->next = 0;		// nuke list
   spinLockRelease(&root->lock);
   for (; ptr; ptr = next)
   {
      next = ptr->next;		// ptr is history when setTripWire() returns
      CAI_ONE(&ptr->tripWire);	// wake the waiter
   }
#endif
}

#endif	// TripWire

/* ******************************************************************** */
/* ************************** String Tables *************************** */
/* ******************************************************************** */

__inline char *stNextString(char *ptr)
{
	// I know there is a trailing '\0'
   return strchr(ptr,'\0') + 1;
}

    // how many bytes are in that string table?
void stCalcSize(StringTable *st)
{
   char  *ptr = st->strings;
   int    n    = st->n;

   while(n--) ptr = stNextString(ptr);
   st->size = ptr - st->strings;
}

    // StringTable --> T(string,string...)
//KCString
    // Result is owned Tuple
Instance *stToList(StringTable *st, pVM vm)
{
   Instance *list;
   char	    *ptr;
   int	     n = st->n;
   Fence     fence;

   if (n == 0) return emptyTuple;

   list = tupleCreate(n,I_OWNED,vm);
   vmSetFence(vm,&fence,0,list);
      for (ptr = st->strings; n--; ptr = stNextString(ptr))
	 tupleAppend(list,stringCreate(ptr,I_OWNED,vm));
   vmRemoveFence(&fence,0);

   return list;
}

/* ******************************************************************** */
/* *************** apply, filter, pump, reduce ************************ */
/* ******************************************************************** */

   // f == Void --> id fcn
   // --> actionType (for passing to _zrun)
   // id:     global name index			 (11 bits max, guess)
   // tftype: Tuple + ftype | isPF		 (9 bits, ie the next two lines)
   // isPF:   1/0: Partial Fcn (fcn application) (1 bit)
   // ftype:  Object Type, eg FcnType, VoidType  (8 bits) zklOpcodes.h

   // Helper codes added to ftype:
#define ZRT_CREATE_String	100
#define ZRT_CREATE_Tuple	101
#define ZRT_ID			102
   
#define MAKE_ACTION_TYPE(ftype,isPF,tftype) \
   ((tftype << 9) | (isPF << 8) | ftype);	// 18 bits

#define DECODE_ACTION_ftype(at)  (at & 0xFF)
#define DECODE_ACTION_isPF(at)   ((at >> 8) & 1)
#define DECODE_ACTION_tftype(at) (at >> 9)

int eyeballCallN(Instance **ip,unsigned id,pMethod *_method,void *pfcn,int bitch,pVM vm);

unsigned _ftyper(Instance *f, int zero, pVM vm)
{
   unsigned ftype = TYPEO(f), n, isPF = 0, tftype = 0; 
   switch(ftype)	// 8 bits
   {
      case ClassType: case MethodType: case OpType: case PropertyType:
      case VoidType:  case FcnType:
         break;
      case DeferredType:
	 if ((isPF = isPartialFcn(f))) ftype = FcnType;
	 break;
      case StringType:
      {
	 char *ptr = stringText(f);
	 if(0==strlen(ptr)) ftype = ZRT_CREATE_String;
	 else if(getGlobalId(ptr,&n,vm))  // n is < 15 bits (probably < 10 bits)
	    { ftype = ZRT_ID; tftype = n; }
         else vmThrow(vm,E_NAME_ERROR,"*.pump(sink,name,...): name not known");
	 break;
      }
      case ListType: // ROlist# (Tuple partitions) only, I don't want changes
	 if(f->objID != roListID)	// really rare for this case
	    vmThrow(vm,E_ASSERTION_ERROR,".filter/pump(T(args): ROList only");
      case TupleType:
	 n = listLen(f,vm);
	 if (!n){ ftype = ZRT_CREATE_Tuple; break; }	// T() == T(args)
	 if (zero) // recursing
	    vmThrow(vm,E_TYPE_ERROR,
	     ".apply/filter/pump/reduce(T(f,args)) doesn't recurse");
	 if (n > 11)	// (f,args)
	    vmThrow(vm,E_ASSERTION_ERROR,
	     ".filter/pump(T(args): No more than 10 static parameters");
	 f      = arglistGet(f,0,".apply/filter/pump/reduce(T(f,static args)",vm);
	 tftype = _ftyper(f,1,vm);	// 9 bits
	 ftype  = TupleType;
	 if (tftype==VoidType && f->iflag) isPF=1;  // T(Void.Read...)
	 break;
      default:
      {
      #if 0
	 char buf[100];
	 sprintf(buf,".apply/filter/pump/reduce(%.80s not a valid action type",
	    iname(f));
	 vmThrow(vm,E_TYPE_ERROR,buf);
      #endif
	 break;
      }
   }
   n = MAKE_ACTION_TYPE(ftype,isPF,tftype);
   return n;
}

    /* Look actionList[a0] and determine what type of action it is
     * f is an action, list of actions or zero (eg actions are in arglist)
     * f == Void --> id fcn
     * f == T --> Void
     * Zero: arglist is f
     * Action: arglist is static args and is returned. _onef is set to 1.
     * List: it is f and is scanned for actions. A list in the list is
     *    (action,args).
     *    Rest of arglist is ignored
     *    If you don't like lists, use _ftyper() instead, with zero set to 1.
     * List: (Void.Fliter,f)-->if(f(x)) x else Void.Skip
     */
int _scanAction(Instance *f,
   pArglist arglist,int a0, int *_oneF,unsigned actionType[], pVM vm)
{
   int oneF, ftype=TupleType, numActions, N=arglistLen(arglist,vm);

   if (f) ftype = TYPEO(f);
   if (ISA_LIST(ftype)) // f==0 or f==T(f,T(g,args) ...) or (action,action...)
   {
      int a;
//if(!_oneF) vmThrow(vm,E_ASSERTION_ERROR,".apply/filter/reduce(no list of actions)");

      if (!f) a = a0;  // default
      else { arglist = f; a = 0; N = listLen(arglist,vm); } // f really is list

      if(ftype!=TupleType) vmThrow(vm,E_ASSERTION_ERROR,
	     ".apply/filter/pump/reduce(action is ROList (ie T), not List");

      if (!N){ f = Void; goto uno; }  // T --> id fcn
      if (N >= MAX_ACTIONS) vmThrow(vm,E_ASSERTION_ERROR,
		".apply/filter/reduce: Too many actions");
      oneF = 0;
 //!!!listTable
      for (numActions = 0; (f = listGet(arglist,a)); a++, numActions++)
      {
	 actionType[a] = _ftyper(f,0,vm);  // track f's
//!!! check arg len
      }
   }
   else		// not a List
   {
      if ((N - a0) > 10)
	 vmThrow(vm,E_ASSERTION_ERROR,
		".apply/filter/reduce: No more than 10 static parameters");
   uno: ;
      oneF = numActions = 1;
      actionType[a0]    = _ftyper(f,0,vm);      
   }
   if(_oneF) *_oneF = oneF;
   return numActions;
}

    // action==Void --> idFcn
    // ki==0 --> self/arglist[0] or 0 (turns off Void.Filter, "" methods)
    /* _stop --> 0 (ZR_OK), ZR_SKIP (Void.Skip), ZR_STOP (Void,Stop), others
     *    ZR_OK: r is result.
     *    ZR_STOP, ZR_SKIP: (Void.Stop,z), (Void.Skip,z), r-->z
     *        else r==VoidVoid
     *    Otherwise: r is suspect, it may be a Tuple (eg T(Void.Skip,666))
     *       or Void.* or ? depending on _stop.
     */
Instance *_zrun(Instance *ki,  // ki is arglist[0]
   Instance *f,unsigned ft,int *_stop, pArglist arglist, pVM vm)
{
   Instance *r;
   int       stop = ZR_OK;
   unsigned  isPF = DECODE_ACTION_isPF(ft);
   MLIST(argz,20);

   switch(DECODE_ACTION_ftype(ft))
   {
      default:	// Ops, Methods, other PFAs, Class  ...
	 r = objectRun(f,arglist,0,vm);
	 goto voidiods;		// eg T.fp(Void.Write)
	 break;
      case VoidType:	// -->idFcn, usually
	 if (f->iflag)
	 {
	    r = VoidVoid;
	    if      (f==VoidWrite) stop = ZR_WRITE;
	    else if (f==VoidRead)  stop = ZR_READ;
	    else if (f==VoidDrop)  stop = ZR_DROP;
	    else if (f==VoidFilter) // Void.Filter --> ki|Void.Skip
	    {
	       r = VoidVoid;
	       if(ki) stop = resultToBool(ki,vm) ? ZR_I : ZR_SKIP;
	    }
	    else { stop = ZR_SKIP; r = VoidVoid; }

	    break;
	 }
	 r = ki ? ki : VoidVoid;	// idFcn
	 break;
      case IntType: case FloatType: r = f; break;
      case FcnType:
	 r = isPF ? partialCall(f,arglist,vm) : 
		    vmCallFcnFromMethod(f,arglist,0,vm);
      voidiods:  // also (Void.Stop,z)
         if (TYPEO(r) == VoidType && r->iflag)
	 {
	    if (r == VoidSkip)	// return(Void.Skip) --> skip
	    {
	       r = VoidVoid;
	    skip:
	       stop = ZR_SKIP;
	       break;
	    }
	    if (r == VoidStop)	// return(Void.Stop)
	    {
	       r = VoidVoid;
	    stop:
	       stop = ZR_STOP;
	       break;
	    }
	    if (r == VoidWrite) { stop = ZR_WRITE; break; }
	    r = VoidVoid;	// ignore result
	 } // voidiods
	 else if (TYPEO(r) == TupleType)
	 {  // return(Void.Skip,v) is nice for T(1).pump(False,T.fp(Void.Skip))
	    // r is used (by pump) so don't slam it to VoidVoid
	    Instance *i, **is;
	    unsigned  z = TUPLE_LEN(r);
	    if (!z) break;
	    is = TUPLE_TABLE(r); i = is[0];
	    if (TYPEO(i)==VoidType && i->iflag)
	    {
	       if (i == VoidWrite) { stop = ZR_WRITE; break; }
	       if (i == VoidRead)  { stop = ZR_READ;  break; }
	       if (z >= 2)
	       {
		  if (i == VoidDrop)   { stop = ZR_DROP;    break; }
if (i == VoidVoid)   { stop = ZR_RECURSE;  break; }  //!!!! remove
		  if (i == VoidRecurse){ stop = ZR_RECURSE; break; }
		  if (i == VoidAgain)  { stop = ZR_AGAIN;   break; }

		  // .Skip, .Stop
		  r = is[1];	// (Void.Stop,z) is handy
		  if (TYPEO(r) == VoidType) r = Void; // CYA
		  if (i == VoidSkip) goto skip;
		  goto stop;
	       }
	       r = VoidVoid; stop = ZR_SKIP;
	    }
	 }
	 break;
      case ZRT_CREATE_Tuple:
	 r = Tuple_create(emptyTuple,arglist,vm);
	 break;
      case TupleType:	// L|T(f,args), _scanAction() limited depth to 1
      {
	 Instance *g = listGet(f,0);	// f & arglist are fenced

	 ft = DECODE_ACTION_tftype(ft);

	 if (ft==VoidType)
	 {
	    if (isPF) { r=f; goto voidiods; } // T(Void.Read [,n])
	    if ((r = listGet(f,1))) break;    // T(Void,K)
	    //-->pass through as Void(x)-->Void
	 }
	 mlistCopy(argz,arglist,0,20); mlistExtend(argz,f,1,20);
	 r = _zrun(ki,g,ft,&stop,(Instance *)argz,vm);
	 break;
      }
      case ZRT_CREATE_String:
	 r = String_create(emptyString,arglist,vm);
	 break;
      case ZRT_ID:
      {
	 unsigned id = DECODE_ACTION_tftype(ft);
	 pMethod  method = 0;
	 ZKL_Fcn  fcn;
	 int      s;

	 r = ki;
	 if(!r){ r = VoidVoid; stop = ZR_SKIP; break; }
	 if(!(s = eyeballCallN(&r,id,&method,&fcn,0,vm)))	// use resolve
	 {
	    r = VoidVoid; stop = ZR_SKIP; 
	    break;
	 }
	 if(s==PropertyType) break;	// Property
	 if(method == (pMethod)VM_yield)
	    { stop = ZR_SKIP; r = VoidVoid; break; }

	 mlistCopy(argz,arglist,1,20); // (self,args) --> (args)

	 #if USE_POINTER_INTS
	    if (IS_PtrInt(ki))
	    {  // Int have Methods & Properties. If id is good and not
	       // Property, must be Method.
	       r = ptrIntDoMethod(ki,0,method,(Instance *)argz,vm);
	       if (!r) { r = VoidVoid; stop = ZR_SKIP; }
	       break;
	    }
	 #endif  // USE_POINTER_INTS
	 if(method) r = method(r,(Instance *)argz,vm); // Data."pop"
	 else if(s==FcnType)
	    r = vmCallFcnFromMethod((Instance *)&fcn,(Instance *)argz,0,vm);
	 break;
      }
   } // switch
   *_stop = stop;
   return r;
}

	 //////////////////////////////////////////////////////////////////
	////////////////////// Sinks /////////////////////////////////////
/* !!!??? SinkCentral: registry for pointers to sink creaters
 * construct: register sinker (fixed table ala zklObjectTable)
 * Then get fcn & data or 0.
 * Also for ZGetters
 */

#define SANDBOX_SIZE 2500
typedef struct
{
   unsigned int how:8;		// ZA_* type selector, you fill this in
   unsigned int rdepth:16;	// recursion count, for internal use
   unsigned int closed:1;	// for internal use
	// mystery flags: flags I might want to use at some point
   unsigned int mf1:1,mf2:1,mf3:1,mf4:1;
   union			// you fill this in
   {
      MSink msink;	// ZA_OBJ, eg Pipe.write
      CSink csink;	// C code sink
   }dst;
   size_t    sz;	// objects written, for internal use
   Instance *i;	   // aka self, will be fenced, you fill this in, optional
   unsigned char sandbox[SANDBOX_SIZE];  // your own struct, this is aligned
}ZAggie;	// ZAgg with a full sandbox

typedef struct
{
   Instance  instance;
   unsigned  closed:1;
   unsigned  size:32;
   ZAgg      aggie;
}Sink;	// variable: Linux/64: 72(lots), 936(String), 2539(List);

static ZKL_Object SinkObject; // all zeros

Instance *Sink_create(Instance *,pArglist,pVM);
static int sinkToSink(Instance *,void *,size_t, size_t,size_t *, pVM);

    // --> something fence-able
Instance *aggieInit(ZAggie *aggie,int how,pVM vm)
{
   size_t _;
   int    s = ZA_2SINK_OK;

   aggie->i	 = Void;	// CYA
   aggie->how	 = how;
   aggie->sz	 = 0;
   aggie->rdepth = 0;
   aggie->closed = 0;
   aggie->mf1 = aggie->mf2 = aggie->mf3  = aggie->mf4 = 0;
   switch(how)
   {
      case ZA_LIST:
	 s=OBJECTI(emptyTuple)->toSink(emptyTuple,aggie,0,SANDBOX_SIZE,&_, vm);
	 break;
      case ZA_STRING: 
	 s=OBJECTI(emptyString)->toSink(emptyString,aggie,0,SANDBOX_SIZE,&_, vm);
	 break;
      case ZA_DATA_D: case ZA_DATA_S: 
	 s=OBJECTI(VData)->toSink((Instance *)(size_t)how,
				 aggie,0,SANDBOX_SIZE,&_, vm);
	 break;
      case ZA_NADA: break;
      default: aggie->how = ZA_NADA; break;
   }
   if(s!=ZA_2SINK_OK) vmThrow(vm,E_VM_ERROR,"aggieInit() sink failure");
   return aggie->i;	// fence this
}

    // Convert passed in sink obj to ZAggie
    // Can GC
static void _aggieInitI(ZAggie *aggie, Instance *useThis, size_t sbSz, pVM vm)
{
   size_t sbUsed = 0;	// not used

   aggieInit(aggie,ZA_NADA,vm);	// initialize
//   aggie->closeAfterPump = 0;	// turn off

//!!! len hint???
   switch(OBJECTI(useThis)->toSink(useThis,aggie,0,sbSz,&sbUsed,vm))
   {
      case ZA_2SINK_OK: return;
      case ZA_2SINK_FAIL:
      case ZA_2SINK_NO_CAN_DO: vmThrow(vm,E_ASSERTION_ERROR,".toSink failed");
      case ZA_CLOSED: vmThrow(vm,E_ACCESS_ERROR,"Sink closed");
      case ZA_SANDBOX_TOO_SMALL: // aggieInitI()
      {
	 MLIST(mlist,2);
	 mlistBuild(mlist,useThis,ZNIL); 
	 aggie->i=useThis=Sink_create(useThis,(Instance *)mlist,vm); // recurse
	 sinkToSink(useThis,aggie,0,0,0,vm);
//	 ((Sink *)useThis)->aggie.closeAfterPump = 1;
	 return;
      }
   }

   switch(TYPEO(useThis))
   {
      case VoidType: aggie->how = ZA_NADA; break;
      case MethodType:  // same as FcnType but I wanna know when closing
	 aggie->how = ZA_METHOD; aggie->i = useThis;
	 break;
      case FcnType:
      run:	// Deferred
	 aggie->how = ZA_RUN; aggie->i = useThis;
	 break;
      case IntType: vmThrow(vm,E_TYPE_ERROR,"pump(Int,...: Int not a sink");
      default:
      {
	 pMethod m;
	 if (useThis->objID == deferredID) goto run;	//!!! also Op
	 m = searchForMethod(useThis,"write",0,vm);
	 if (!m) vmThrow(vm,E_TYPE_ERROR,"pump(sink...: sink has no write method");
	 aggie->how = ZA_2M; aggie->i = useThis;
	 aggie->dst.msink.write = m;
	 aggie->dst.msink.close = 0;	// look up close method at close
	 break;
      }
   }
}
static Instance *aggieInitI(ZAggie *aggie, Instance *useThis,pVM vm)
{
   _aggieInitI(aggie,useThis,SANDBOX_SIZE,vm);
   return aggie->i;	// fence this
}

    // still fenced
Instance *aggieClose(ZAggie *aggie,int hard,pVM vm)
{
   Instance *i = aggie->i;

   if(aggie->closed) return i;	// for Sink (mostly)
   if(hard) aggie->closed = 1;

   switch(aggie->how)
   {
      case ZA_NADA: return i;
      case ZA_OBJ:  goto closeI;
      case ZA_RUN:
	 if (i->objID == deferredID)  // Deferred method?
	 {
	    i = Deferred_f(i,vm);  // Fcn|Method|Void
	    if(TYPEO(i)==MethodType) goto closeMethod;
	 }
	 break;
      case ZA_METHOD:	// pump(Dicionary().incV)-->Dicionary
      {
	 pMethod m;
	 i = aggie->i;		// the Method
      closeMethod:
	 i = Method_instance(i,vm);	// the object Method is bound to
      closeI:
	 if (hard)
	 {
	    m = searchForMethod(i,"close",0,vm);
	    if(m) m(aggie->i,NoArglist,vm);
	 }
         return i;
      }
      case ZA_C: 
      {
	 CSink *cs = &aggie->dst.csink;
      	 return aggie->i = cs->close(cs->X,vm);
      }
      case ZA_2M:
      {
	 pMethod close = aggie->dst.msink.close;
	 if(!close) goto closeI;	// eg File, Pipe
	 if(hard) close(i,NoArglist,vm);
	 return i;
      }
   }
   return Void;
}

    // Caller needs to have fenced i
    // Can GC
void aggieWrite(ZAggie *aggie, Instance *i, pVM vm)
{
   MLIST(mlist,2);

   aggie->sz++;
   switch(aggie->how)
   {
      case ZA_NADA:   aggie->i = i; break;
      case ZA_METHOD: Method_call(aggie->i,mlistBuild(mlist,i,ZNIL),vm); break;
      case ZA_RUN: 
         objectRun(aggie->i,mlistBuild(mlist,i,ZNIL),0,vm); break;
      case ZA_C: 
      {
	 CSink *cs = &aggie->dst.csink;
      	 cs->write(cs->X,i,vm);
	 break;
      }
      case ZA_2M:
      {
	 MSink *cs = &aggie->dst.msink;
	 cs->write(aggie->i,mlistBuild(mlist,i,ZNIL),vm);
	 break;
      }
   }
}

////////////////////////////////////////////// Sink

static void sinkMarker(Instance *sink)
{
   ZAgg *aggie = &((Sink *)sink)->aggie;
   instanceMark(aggie->i);
}

    // Sink.write(i)
Instance *Sink_write(Instance *self, pArglist arglist, pVM vm)
{
   unsigned   n;
   Instance **is = listTable(arglist,&n);

   if (((Sink *)self)->closed) 
      vmThrow(vm,E_ACCESS_ERROR,"The sink is closed, can't write");
   while(n--) aggieWrite((ZAggie *)&((Sink *)self)->aggie,*is++,vm);
   return self;
}   
int sinkWrite(void *sink, Instance *i, pVM vm)
{
#if 0
   if (((Sink *)sink)->closed) 
      vmThrow(vm,E_ACCESS_ERROR,"The sink is closed, can't write");
#endif
   aggieWrite((ZAggie *)&((Sink *)sink)->aggie,i,vm);
   return 1;
}   

    // Sink.close()
Instance *Sink_close(Sink *self, pArglist arglist, pVM vm)
{
   Instance *r;
   if (self->closed) return ((Sink *)self)->aggie.i;
   self->closed = 1;
   r = aggieClose((ZAggie *)&((Sink *)self)->aggie,1,vm);
   ((Sink *)self)->aggie.i = r;
   return r;
}
    // eg .pump(Sink,...)
    // Aggie->closed indicates soft or hard close
    // Always return Sink
    // This is part of a two stage close: Sink(Sink).close()-->Sink.close()
Instance *sinkClose(void *sink, pVM vm)
{
   ZAgg     *aggie = &((Sink *)sink)->aggie;
   ((Sink *)sink)->closed = aggie->closed;
   if(aggie->closed) aggieClose((ZAggie *)aggie,1,vm);	// hard close
   return (Instance *)sink;
}

    // Sink.create(obj=Void)
int sinkID = 0;

    // Sink(obj): Query obj on how to use it as a Sink
    // ???Sink(obj,args)
Instance *Sink_create(Instance *self, pArglist arglist, pVM vm)
{
   Instance *obj = arglistGet(arglist,0,"Sink",vm);
   Sink     *sink;
   Fence     fence;
   size_t    sbUsed=0, sbSz;

   if(!obj) obj = Void;
   if(sinkID==OBJECTI(obj)->id) return obj;   // Sink(Sinker)-->Sinker

   // query obj for sandbox size, this always works
   OBJECTI(obj)->toSink(obj,0,0,0,&sbUsed,vm);
   sbSz       = sizeof(Sink) + sbUsed;
   sink       = (Sink *)instanceAllocate(sbSz,&SinkObject,1,vm);
   sink->size = sbSz;
   // fence on the off chance obj allocates since obj can't fence sink
   vmSetFence(vm,&fence,0,addToCollectables((Instance *)sink,I_OWNED,vm));
      // if aggieInitI() throws, gc sink
      _aggieInitI((ZAggie *)&sink->aggie,obj,sbUsed,vm); // obj fills in Zagg
   vmRemoveFence(&fence,0);
   return (Instance *)sink;
}

    /* How to use:  Call with the size sandbox needed (zero is fine) and a
     *   pointer to Zagg (I'll set it in after I allocate it).
     *   On return, fill in Zagg (it has been initialized to Sink(Void)) and
     *   return the returned Sink.
     * If you [post call] allocate (something GC'able) remember to fence Sink.
     * If you want pump to close the sink: Zagg->closeAfterPump = 1
     *   Otherwise: .pump(Sink(obj)).close() or .pump(Sink(obj)).write(z)
     * eg LibSrc/MsgHash/zklMsgHash.c
     */
Instance *sinkCreate(size_t sbSz, ZAgg **zaggie, pVM vm)
{
   Sink   *sink=(Sink *)instanceAllocate(sizeof(Sink) + sbSz,&SinkObject,1,vm);
   ZAggie *aggie = (ZAggie *)&sink->aggie;

   sink->size = sizeof(Sink) + sbSz;
   *zaggie = (ZAgg *)aggie;
   aggieInit(aggie,ZA_NADA,vm); // initialize to ZA_NADA
   return addToCollectables((Instance *)sink,I_OWNED,vm);
}

#if 0
    // Create a Sink wrapper for C code
    /* Input:
     *    cs: a filled out CSink
     *    saveMe: something that needs marking else 0
     *        Only marked until sink is closed.
     */
    // Returns: Sink
Instance *csinkCreate(CSink *cs,Instance *saveMe,pVM vm)
{
   Instance *csink=Sink_create(Void,NoArglist,vm);
   ((Sink *)csink)->aggie.dst.csink = *cs;
   ((Sink *)csink)->aggie.how       = ZA_C;
   if(saveMe) ((Sink *)csink)->aggie.i = saveMe;
   return csink;
}
#endif

static const MethodTable sinkMethods[] =
{
   "write",	(pMethod)Sink_write,
   "close",	(pMethod)Sink_close,
   "toBool",	(pMethod)Bool_soTrue,
   0,		0
};

///////////////////////////////////////////////////////////////

    // Sink.isClosed -->Bool
static Instance *Sink_isClosed(Sink *self,pVM vm)
   { return self->closed ? BoolTrue : BoolFalse; }

    // Sink.size -->Int
static Instance *Sink_size(Sink *self,pVM vm)
   { return intCreate(self->size,vm); }

static const PropertyTable sinkProperties[] =
{
   "isClosed",	(pProperty)Sink_isClosed,
   "size",	(pProperty)Sink_size,
   0,		0
};


static int sinkToSink(
   Instance *self,void *_aggie,size_t lenHint, size_t sbsz,size_t *_, pVM vm)
{
   ZAgg *aggie = (ZAgg *)_aggie;
   Sink *sink  = (Sink *)self;

   if (sink->closed) return(ZA_CLOSED);

   if(!aggie) return ZA_2SINK_OK; 	// sbUsed query

   aggie->how = ZA_C;
   aggie->dst.csink.X     = self;
   aggie->dst.csink.write = sinkWrite;
   aggie->dst.csink.close = sinkClose;

   return ZA_2SINK_OK;
}

void sinkConstruct(void)
{
   constructObject(&SinkObject,NativeType, sinkMethods,sinkProperties,0,NoVM);
   SinkObject.name	     = "Sink";
   SinkObject.magicMarker    = sinkMarker;
   SinkObject.isize	     = sizeof(Sink);
   SinkObject.toSink	     = sinkToSink;
   SinkObject.createReturnsSelf = 1;

   sinkID = SinkObject.id;

   vaultAddData("Sink",methodCreate(Void,0,Sink_create,NoVM),NoVM); 
}


/////////////////////////////////////////////////////////////////////////

#define CHECK \
   if (0 == (idx % 0x800)) vmMightStall(vm,666); // need to gc?

//   if (*interrupt) vmProcessInterrupt(vm);	// longjmp()s


    // .pump([n|*,]sink,action [,action ...])
    // .pump(sink,action [,action ...])
//!!!pass in hint at collection/result size
    // aggregate is Bool/0/1 or Object (which is bit redundant when you can
    //    use, eg, Data.write
    // basically a riff on List.apply
    // pump(1,...) ~= pump(..., ,L().append)
    // src.pump(1) --> T(src), src.pump(0) --> src[-1,1], T.pump(0) --> Void
    // Like a Unix shell pipe
    // src.pump(True,x,y,z) == src.apply(x).apply(y).apply(z)
    //			    == src.apply(T(x,y,z))
    // If an action needs args, use .fp or T:  src.pump(1,a.fp(4),T(b,5))
    /* This is some nasty code.  It can will recurse once if it sees
     *   T(f,args) and set self to f, canAggregate to ftype & isPF, getter
     *   to 0 and X to args.  If there are Fcns in actions or nested
     *   actions, only one VM is allocated to run them.
     */

     // To stop the pump, writing n items:
     //   fcn pstopper(n){ 
     //       fcn(r,rn){ rn.dec()<=1 and T(Void.Stop,r) or r }.fp1(Ref(n)) }

     // flags: 
     //  PMP_ZERO  : if no result, return 0
     //	 PMP_OK2CNT: .pump([cnt],sink,action,..), enable cnt
     //
     // X is NOT GC safe.
Instance *pump(Instance *self, unsigned flags,
   ZGetter getter, void *X, pArglist arglist, int a0, pVM vm)
{
   extern int refID;
   void     *crefRef(Instance *);
   Instance *refCreate(void *,int, pVM);	// miscObj.c

   Fence     fence;
   Instance *r, *sink;
   unsigned  a, a1=a0, atEnd=0, cnt,counting=0;;
   int	     stop;
   unsigned  actionTypes[MAX_ACTIONS];
   size_t    idx;
   ZAggie    _aggie, *aggie = &_aggie;

   if(flags)
   {
      if(flags & PMP_DFAULT_CNT)  // get this many if not over ridden
      {
	 cnt      = (flags>>16) & 0xefff;	// 15 bits
	 counting = 1;
      }
      if(flags & PMP_OK2CNT)  // .pump([cnt]|*,...)
      {
	 Instance *pn = arglistTryToGet(arglist,a0);
	 if(pn)
	 {
	    if(pn==Star){ counting=0; a0++; }
	    else if(TYPEO(pn) == IntType)
	    {
	       int n = (int)arglistGetInt(arglist,a0,0,vm);
	       if(n<=0) atEnd = 1;
	       cnt = (unsigned)n;
	       counting = 1;
	       a0++;
	    }
	 }
      }
   }

   sink = arglistGet(arglist,a0,".pump(sink",vm);
       // check for recursive pump
   if (sink && AS_I(sink)->objID == refID && sink->iflag) aggie = crefRef(sink);
   else sink = aggieInitI(aggie,sink,vm);

   a1   = a0 + 1;
   _scanAction(0,arglist,a1,0,actionTypes,vm);

   vmSetFence(vm,&fence,0,sink);  // i(sink), i1(args), i2(i), i3(zrun)
   {
      #define MAX_READ 200

      unsigned  numActions;
      Instance *args, *i, *f, **pargs, **actions;
      MLIST(mlist,MAX_READ); MLIST(mlist2,MAX_READ);

      args     = mlistBuild(mlist,Void,ZNIL);	// no static args
      fence.i1 = args;
      pargs    = TUPLE_TABLE(args);
      r        = Void;	// in case nothing to do
      actions  = listTable(arglist,&numActions);

	// count written, good for filters
      idx = 0; while(1)
      {
	 if(atEnd || (counting && aggie->sz>=cnt))    break;
	 if(!(i = getter(self,idx++,X,aggie->sz,vm))) break;
	 r = fence.i2 = i;

	 CHECK;

	 stop = ZR_OK;	// in case no actions
	 for (a = a1; a < numActions; a++)
	 {
	    *pargs = r;		// args[0] = r
	 redo:
	    f = actions[a];
	    if(f==VoidXplode)  // list.xplode() if (..-->T,Void.Xplode,..)
	    {
	       if(a<numActions-1 && (TYPEO(r)==TupleType || TYPEO(r)==ListType))
	       {
		  // if next action is Void.Xplode, _zrun --> ZR_SKIP
		  r = fence.i3 = mlistCopy(mlist2,r,0,MAX_READ);
		  a++;
		  r = fence.i3 = _zrun(r,actions[a],actionTypes[a],&stop,r,vm);
//!!!??? what about var args & pargs?
	       }
	       else continue; // else skip Void.Xplode
	    }
	    else r = fence.i3 = _zrun(r,f,actionTypes[a],&stop,args,vm);

	    if (stop) // lots of breaks here & I want to avoid gotos in a switch
	    {
	       int rIsList = ISA_LIST(TYPEO(r));
	       if (stop == ZR_WRITE) // T(Void.Write,...])-->r[1,*].pump(sink)
	       { //  T(Void.Write)-->write arglist (as list)
		  if (!rIsList)  // Void.Write --> write/extend args
		    { r = args; goto writeWrite; }
		  else   // r is Tuple
		  {
		     unsigned   z;
		     Instance **is = listTable(r,&z);
		     if (z>1 && is[1]==VoidWrite) // WriteWrite T --> extend T
		     {
			int t;
			if (z==2) // T(Void.Write,Void.Write)
			{
			   r = *pargs; 
			writeWrite:
			   t = TYPEO(r);
			   if (ISA_LIST(t))
			   {
			      is = listTable(r,&z);
			      while(z--) aggieWrite(aggie,*is++,vm);
			   }
			   else aggieWrite(aggie,r,vm);
			}
			else  // T(Void.Write,Void.Write,stuff)
			{
			   t = TYPEO(is[2]);
			   if (ISA_LIST(t))
			   {
			      is = listTable(is[2],&z);
			      while(z--) aggieWrite(aggie,*is++,vm);
			   }
			   else aggieWrite(aggie,is[2],vm);
			}
		     }
		     else // T(Void.Write,x,y,z), T(Void.Write)
		     {
			if(z==1)  //  T(Void.Write)-->write arglist as list
			{
			writeArglist:
			   i = fence.i2 = listToTuple(args,0,TUPLE_LEN(args),vm);
			   aggieWrite(aggie,i,vm);
			}
			else
			   while(--z)
			   {
			      r = *++is;
			      if (r==VoidDrop) aggieWrite(aggie,pargs[0],vm);
			      else 	    aggieWrite(aggie,r,vm);
			   }
		     }
		  }
		  r = VoidVoid;
		  TUPLE_LEN(args) = 1;	// in case prev action was Read
		  break;	// treat like Skip
	       }
	       if (stop == ZR_READ)  // T(Void.Read,n) --> arglist.len()==n
	       {		     // or Void.Read --> r=Void.Read
		  int z;
		  if (!rIsList) z = 1;
		  else // r contains i
		  {
		     z = (int)arglistGetInt(r,1,"pump(Void.Read",vm);
		     if (z < 0 || (z + TUPLE_LEN(mlist)) >= MAX_READ)
			{ r = VoidVoid; break; }	// skip
		  }
		  while(z--)
		  {
		     if (!(i = getter(self,idx++,X,aggie->sz,vm)))
		     {
			atEnd = 1;
			if (BoolFalse==listGet(r,2)) break;
			vmThrowTheEnd(vm);
		     }
		     mlistAppendI(mlist,i,MAX_READ);
		  }
		  if (rIsList && BoolTrue==listGet(r,3)) goto redo;
		  if(a==numActions-1) goto writeArglist;   // last action
		  r = pargs[0]; // spares me a goto ...
		  continue;	// next action
	       }
	       if (stop == ZR_AGAIN)  // T(Void.Again,r) --> retry with r
	       {
		  r = TUPLE_TABLE(r)[1];  // _zrun assures me it's there
		  if (!(i = getter(self,idx++,X,aggie->sz,vm)))
		  {
		     atEnd = 1;
		     vmThrowTheEnd(vm);
		  }
		  mlistBuild(mlist,i,r,ZNIL);
		  goto redo;
	       }
	       if(stop==ZR_RECURSE) // T(Void.Recurse,i...])-->i.pump(sink,...)
	       {  	
		  //!!!!??? I would like to nuke this, only gerber uses
		  // also used in manual example & Rosetta Code (flatten)
		  unsigned   ft, z;
		  Fence      fence2;
		  Instance **is = listTable(r,&z);  // z >= 2 (_zrun)

		  if (z > 20) { r = VoidVoid; break; }
		  vmSetFence(vm,&fence2,0,0);
		  {
		     Instance *ref;
		     ref  = fence2.i1 = refCreate((void *)aggie,1,vm);
		     i    = is[1];  // r (->is) is fenced
		     mlistBuild(mlist,Void,ref,ZNIL);
		     mlistExtend(mlist,r,2,MAX_READ);
		     if (aggie->rdepth++ > 50)  // ~160 stack frames
			vmThrow(vm,E_ASSERTION_ERROR,
			 "C stack too big (run away pump recursion?)");
		     ft = MAKE_ACTION_TYPE(ZRT_ID,0,pumpId);
		     _zrun(i,Void,ft,&stop,args,vm);   // no recursion!
		     aggie->rdepth--;
		  }
		  vmRemoveFence(&fence2,0);
		  r = VoidVoid; stop = ZR_SKIP; TUPLE_LEN(args) = 1;
		  break;	// treat like Skip
	       }
	       if (stop == ZR_DROP) // T(Void.Drop,i)-->write,ignore f here on
	       {  // Void.Drop == Write/Drop, T(Void.Drop,Void.Void): no write
		  unsigned z;
		  if (!rIsList) r=pargs[0];  // pass through
		  else	         r=listTable(r,&z)[1];   // z>=2 (_zrun)
		  actionTypes[a]=MAKE_ACTION_TYPE(VoidType,0,0); // passthrough
		  actions[a]    =Void; // actions in arglist, this is opSetArg
		  break; // Skip
	       }
	       if (stop == ZR_I) { r = fence.i2; continue; }
	       TUPLE_LEN(args) = 1; // in case prev action was Read/Recurse & this action-->Void.*
	       break; // Skip rest of actions
	    }// if(stop)

	    TUPLE_LEN(args) = 1;   // in case prev action was Read/Recurse
//args = (Instance *)mlist;
	 } // foreach action
	 if (r == VoidVoid) r = Void;	// don't let VoidVoid escape
	 else aggieWrite(aggie,r,vm);
	 if (stop == ZR_STOP) break;
      } // for getter
   } // fence
   #undef MAX_READ

   // if recursing, don't close sink
   r = (aggie->rdepth) ? VoidVoid : aggieClose(aggie,0,vm);
   vmRemoveFence(&fence,0);
   if ((flags & PMP_ZERO) && !aggie->sz) return 0;
   return r;
}

#define FILTER_SAVE_RESULTS	1
#define FILTER_ONE		2
#define FILTER_INDEX		4

    // i.filter(f) --> f(i)
    // i.filter(f,x,y,z) --> f(i,x,y,z)
    // i.filter(T(x,y,z)) --> x(i) && y(i) && z(i)
    // i.filter(T) --> i.filter(Void)
    // if howza == 0, ZA_LIST is used
static Instance *_filter(Instance *self, int what, int howza,
   Instance **pResult2,
   ZGetter getter, void *X, pArglist arglist,int a0, pVM vm)
{
   Instance *f, **actions = { 0 };  // VC14
   Instance *result = Void;
   int	     ftype, oneF, saveResults = (what & FILTER_SAVE_RESULTS);
   unsigned  actionType[MAX_ACTIONS];
   unsigned  numActions;
   Fence     fence;

   f = arglistTryToGet(arglist,a0);
   if (!f) f = Void;
   _scanAction(f,arglist,a0,&oneF,actionType,vm);
   if (oneF) ftype = actionType[a0];
   else      actions = listTable(f,&numActions);

   vmSetFence(vm,&fence,0,0);	// args, getter(),r1,r2
   {
      unsigned	a;
      size_t	idx,sz;
      Instance *args, *i;
      MLIST(mlist,10);
      ZAggie zar1, zar2;   	// buffers for results:

      args    = mlistBuild(mlist,Void,ZNIL);
      fence.i = args;
      mlistExtend(mlist,arglist,a0+1,10);

      if (saveResults)	// remember that the getter can allocate
      {
	 if (!howza) howza = ZA_LIST;
	 fence.i2 = aggieInit(&zar1,howza,vm);
	 if (pResult2) fence.i3 = aggieInit(&zar2,howza,vm);
      }
      else result = BoolFalse;

      for (idx = sz = 0; (i = getter(self,idx,X,sz,vm)); idx++)
      {
	 int       stop;
	 Instance *r;
	 
	 TUPLE_TABLE(args)[0] = fence.i1 = i;

	 CHECK;

	 stop = ZR_OK;
	 if (oneF) r = _zrun(i,f,ftype,&stop,args,vm);
	 else	// Tuple: pass i to each filter, not common
	 {
	    r = BoolTrue;	// T --> Void (_scanAction already did this)
	    for (a = 0; a < numActions; a++)
	    {
	       f = actions[a];
	       r = _zrun(i,f,actionType[a],&stop,args,vm);
	       if (r == BoolFalse || stop || !resultToBool(r,vm))
	          { r = BoolFalse; break; }
	       if (stop) break;
	    } // for
	 }

	 if ((stop==ZR_OK || stop==ZR_STOP) && 
	     (r == BoolTrue || resultToBool(r,vm)))
	 {
	    if (what & FILTER_INDEX) i = intCreate(idx-a0,vm);
	    if (saveResults)	// WON'T be i64
	       aggieWrite(&zar1,i,vm);

	    if (what & FILTER_ONE) { result = i; break; }
	    sz++;
	 }
	 else if (pResult2) { aggieWrite(&zar2,i,vm); sz++; }
	 if (stop && stop!=ZR_SKIP) break;	// ZR_OK==0
      } // for getter

      if (pResult2)  *pResult2 = aggieClose(&zar2,0,vm);
      if (saveResults) result  = aggieClose(&zar1,0,vm);
   } // fence
   vmRemoveFence(&fence,0);
   return result;
}

    // .filter. xs.filter(f,args) == [!(x); xs,f; {x}]]
    // .filter(Void.Write,sink,f,args)
//!!! change howza to pass in ZAggie or zero
Instance *zfilter(Instance *self, int howza,
   ZGetter getter, void *X, pArglist arglist, int a0, pVM vm)
{
   return _filter(self,FILTER_SAVE_RESULTS,howza,0,getter,X,arglist,a0,vm); 
}

Instance *zfilter1(Instance *self, int _,
   ZGetter getter,void *X, pArglist arglist, int a0, pVM vm)
   { return  _filter(self,FILTER_ONE,0x0,0,getter,X,arglist,a0,vm); }

    // .filter1n --> idx|False
Instance *zfilter1n(Instance *self, int _,
   ZGetter getter,void *X, pArglist arglist, int a0, pVM vm)
   { return _filter(self,(FILTER_INDEX | FILTER_ONE),0x0,0,getter,X,arglist,a0,vm); }

    // .filterNs --> indexes
Instance *zfilterNs(Instance *self, int _,
   ZGetter getter,void *X, pArglist arglist, int a0, pVM vm)
   { return _filter(self,(FILTER_INDEX | FILTER_SAVE_RESULTS),0x0,0,getter,X,arglist,a0,vm); }

    // .filter22 -->T(L(pass),L(fail))
Instance *zfilter22(Instance *self, int howza,
   ZGetter getter,void *X, pArglist arglist, int a0, pVM vm)
{
   Instance *r, *r2;
   Fence     fence;

   vmSetFence(vm,&fence,0,0);
      r = _filter(self,FILTER_SAVE_RESULTS,howza,&r2,getter,X,arglist,a0,vm);
      fence.i1 = r; fence.i2 = r2;
      r = tupleCreateX(vm,r,r2,ZNIL);
   vmRemoveFence(&fence,0);
   return r;
}

   // apply(f) == [[(x); xs; f]]
   //    T(1,2,3).apply('+(1)) == [[(x);T(1,2,3);'+(1)]]
   //    fcn map(f,xs){ if(not xs)T else T(f(xs[0]).extend(map[f,xs[1,*])) }
   // if !aggregator, Y is 0 or ZA_?
//!!! change to pass in ZAggie | how
Instance *zapply(Instance *self, 
   ZGetter getter,void *X, CSinkWrite aggregator,void *Y,
   pArglist arglist, int a0, pVM vm)
{
   Instance *f = arglistGet(arglist,a0,".apply(f)",vm);
   Fence     fence;
   int	     aggit = 0;
   unsigned  action = _ftyper(f,1,vm);
   ZAggie    aggie = { 0 };

   vmSetFence(vm,&fence,0,0);
   {
      size_t	idx;
      Instance *args, *i;
      MLIST(mlist,10);

      if (!aggregator && Y)
	 { fence.i2 = aggieInit(&aggie,(int)Y,vm); aggit = 1; }

      args    = mlistBuild(mlist,Void,ZNIL);
      fence.i = args;
      mlistExtend(mlist,arglist,a0+1,10);

      for (idx = 0; (i = getter(self,idx++,X,aggie.sz,vm)); )
      {
	 int       stop;
	 Instance *r;
	 
	 TUPLE_TABLE(args)[0] = i;
	 CHECK;
	 r = fence.i1 = _zrun(i,f,action,&stop,args,vm);
	 if ((stop==ZR_OK || stop==ZR_STOP) && r!=VoidVoid)	// ignore Void*
	 {
	    if (aggregator) aggregator(Y,r,vm);
	    else if (aggit) aggieWrite(&aggie,r,vm);
	 }
	 if (stop && stop!=ZR_SKIP) break;	// ZR_OK==0
      } // for getter
      if (aggit) f = aggieClose(&aggie,0,vm);
   } // fence
   vmRemoveFence(&fence,0);
   if (aggit) return f;
   return Void;
}

    // .reduce(f=idFcn [,init [,static args]]) --> f(prev,item,static args)
    // .reduce2(sink,f [,init [,static args]]) --> f(prev,item,static args)
    // --> prev = init; foreach x in (self) { prev = f(prev,x,static args) }
    // No init, init = table[0]
    // L().reduce(f) --> Void, L().reduce(f,x) --> x
    // L(x).reduce(f) --> x, L(x).reduce(f,y) --> f(y,x)
    // NOT thread safe
Instance *zreduce(Instance *self,
  ZGetter getter, void *X, pArglist arglist, int a0, pVM vm)
{
   Fence      fence;
   Instance  *f = arglistGet(arglist,a0,0,vm);
   Instance  *pinit, *prev, *sunk=0;
   unsigned   action;
   size_t     idx;
   ZAggie     sink;

   if(f==VoidWrite)
   {
      sunk = arglistGet(arglist,++a0,0,vm);
      f = arglistGet(arglist,++a0,0,vm);
   }
   pinit = arglistTryToGet(arglist,a0+1);
   idx   = 0;
   if (pinit) prev = pinit;	// reduce(f,init), reduce(T(f,args),init)
   else		// reduce(f)
   {
      prev = getter(self,idx++,X,0,vm);
      if (!prev) return Void;	// T.reduce(f)
   }

   if (!f) f = Void;
   action = _ftyper(f,0,vm);

   if(sunk) sunk = aggieInitI(&sink,sunk,vm);  // after getter() called
   vmSetFence(vm,&fence,0,sunk);
   {
      Instance *args, *i;
      MLIST(mlist,15);

      if (pinit) // .reduce(f,init), .reduce(f,init,static args)
	 	 // copy arglist; f,init,static args are in correct places
	 args = mlistCopy(mlist,arglist,a0,15);
      else	 // .reduce(f) --> f(prev,next item)
	 args = mlistBuild(mlist,prev,Void,ZNIL);   // fence getter(0)
      fence.i1 = args;

      for( ; (i = getter(self,idx,X,idx,vm)); idx++)
      {
	 int       stop;
	 Instance *r;
	 
	 TUPLE_TABLE(args)[0] = prev; TUPLE_TABLE(args)[1] = i;

	 CHECK;

	 r = _zrun(prev,f,action,&stop,args,vm);
	 if ((stop==ZR_OK || stop==ZR_STOP || stop==ZR_SKIP) && r!=VoidVoid)
	 {
	    prev = r;
	    if(sunk) aggieWrite(&sink,r,vm);
	 }
	 if (stop && stop!=ZR_SKIP) break;	// ZR_OK==0
      } // while getter
      fence.i2 = prev;
   } // fence
   if(sunk) sunk = aggieClose(&sink,1,vm);
   vmRemoveFence(&fence,0);

   if (sunk) return sunk;
   return prev;
}
