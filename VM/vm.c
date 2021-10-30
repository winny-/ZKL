/* vm.c : The zkl Virtual Machine
 * 
 * Copyright (c) 2006,2007,2008-13,2014 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

/* Windows registry:
 * HKEY_CURRENT_USER\Software\Classes\Applications\zkl.exe\shell\open\command
 * HKEY_CURRENT_USER\Software\Classes\zkl.File\shell\open\command
 * 		app_path
 * REG_SZ : "C:\ZKL\VM\Release\zkl.exe" "%1" %*
 * And stdin is fucked (FILE._file == -1)
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#define MARKII	1		// force PTHREADS style marker

#if defined(__unix__)
   #define PTHREADS
   #include <pthread.h>		// pthread_*
#endif
#include <sys/types.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <stdarg.h>

#define __NOT_A_DLL
#define __CLASS_INTERNALS
#define __FCN_INTERNALS
#define __GC_INTERNALS
#define __LIST_INTERNALS
#define __STRING_INTERNALS

#include "zklObject.h"
#include "zklAtomic.h"
#include "zklFcn.h"
#include "zklClass.h"
#include "zklData.h"
#include "zklDictionary.h"
#include "zklList.h"
#include "zklMethod.h"
#include "zklNumber.h"
#include "zklString.h"
#include "zklUtil.h"

void vmConstruct(void), garbageManConstruct(void), threadConstruct(void);
void asyncConstruct(void);
void fileConstruct(void), tcpSocketConstruct(void), miscConstruct(void);
void pipeConstruct(void), sinkConstruct(void), walkerConstruct(void);
void markThread(void *thread);			// thread.c

void cacheSomeGlobalNames(void);	// object.c

static Instance *_vm2Instance(pVM,int lock);

#if _MSC_VER
   typedef int pthread_t;
#endif
int  threadPID(void *thread, pthread_t *pid);	// thread.c

Instance *argV = 0, *classPath = 0, *libPath = 0, *includePath = 0;
Instance *ExceptionClass = 0;

Instance *ExceptionContainer = 0, *ExTheEnd = 0, *Utils_Helpers = 0;

static SpinLock _vmLock;

typedef struct
{
   BInstance instance;		// Inherit from BInstance
   volatile pVM pVM;		// or 0
} VMWR;		// VM Weak Ref, 8 bytes

static ZKL_Object VMWRObject;


    // Each VM stack is a List or array. It isn't used much so it is small.
    // A Block has a reference to the size, and it is SMALL so check
#define FIXED_STACK	200	// size, use zero for list based stack

typedef struct VM	// 156 bytes (without fixed stack)
{
   struct VM   *prevVM, *nextVM;
   struct VM   *parentVM, *childVM, *rootVM;	// for VM "stacks"
   BlockStack  *blockStack;		// per VM
   ZKL_PC	pc;			// per VM
   Instance    *result, *regX;		// per VM
   Instance    *stack[FIXED_STACK];	// per VM
   unsigned int	stki:8;	     // stack index
   unsigned int	recursionCount:8;
   unsigned int	isFiber:8;	// could be chopped to :2
   unsigned int	isStrand:1;	// 1 if both a fiber and strand
   unsigned int argOffset:16, numArgs:16;	// arglist window
   int		mrc:16;		// method recursion count
   int		icount:16;	// instruction count
   unsigned	id;
   Instance    *arglist2;	// a list of the arglists, flattened
   Class       *self;
   CAtomicInt	mightStall;	// A Method/Property sets this, per stack
   CAtomicInt
      marked,		// for gc & fibers
      marking,		// Set in one thread, read by another, per stack
      running,		// protect against recursion, per VM
      barfing;		// in the middle of a throw, per stack
   void	       *isThread;	// Pointer to the actual Thread (or flag)
   pthread_t	pid;		// all root VMs have a pid, per stack
   int		interruptEvent;	  // so other threads can throw things at me
   Instance    *pendingException; // 0 or an Exception, queue of size 1
   Instance    *xxception;	  // onExit Exceptions, tmp
   jmp_buf     *env;		// The dark magic behind exceptions
   Fence	fence;		// where all the fences for this VM are stored
   SpinLock	fenceLock;
   VMWR	       *ref;		// 0 or ref, the ref that points to me
   #if defined(PTHREADS) || MARKII
      SpinLock	gcLock;
   #endif
} VM;		// 344 bytes


#define pVM2VM(pVM)		((VM *)pVM)

#define VM_NEXT(vm)		( (vm)->nextVM )
#define VM_PREV(vm)		( (vm)->prevVM )

#define INTERRUPT_EVENT(vm)	( ((VM *)vm)->interruptEvent )

#define VM_CLASS(vm)		( ((VM *)vm)->self )
#define VM_SELF(vm)		( (Instance *)VM_CLASS(vm) )

#define CHILD_VM(vm)		( ((VM *)vm)->childVM )
#define PARENT_VM(vm)		( ((VM *)vm)->parentVM )
#define ROOT_VM(vm)		( ((VM *)vm)->rootVM )


    // Running is per VM
#define isRunning(vm)  CAI_VALUE(&(vm)->running)
#define setRunning(vm) CAI_ONE(&(vm)->running)
#define notRunning(vm) CAI_ZERO(&(vm)->running)

    // Barfing is per VM stack (ie set on the rootVM)
#define isBarfing(vm)  CAI_VALUE(&vm->rootVM->barfing)
#define setBarfing(vm) CAI_ONE(&vm->rootVM->barfing)
#define notBarfing(vm) CAI_ZERO(&vm->rootVM->barfing)


void zklConstruct(int argc, char* argv[])
{
   char	       *ptr;
   int		i;

#if 0
   sigemptyset(&signalMask);
   sigaddset(&signalMask, SIGUSR2);
   s = pthread_sigmask(SIG_UNBLOCK,&signalMask,0);

   sigemptyset(&signalMask);
   sigaddset(&signalMask,SIGUSR1);
   s = pthread_sigmask(SIG_BLOCK,&signalMask,0);
#endif

//   atexit(shuttingDown);	// ^C not caught, exit() is

   gcConstruct();		// uses: nada. Collection is NOT started
   allocateConstruct();		// uses: nada. --> Buckets
   objectConstruct();		// uses: nada
   stringConstructPartI();	// uses: Buckets. --> emptyString, Star
   dictionaryConstructPartI();	// uses: Buckets
   vaultConstruct();		// uses: TheVault, Dictionary, String
   threadConstruct();		// uses: Buckets

   voidConstruct();		// uses: String
   dataConstruct();		// uses: TheVault
   listConstruct();  // uses: String,Buckets,TheVault --> emptyList,List,ROList
   stringConstructPartII();	// uses: TheVault
   kdataConstruct();		// uses: Thread buckets
   arglistConstruct();		// uses: emptyList. -->NoArglist
   boolConstruct();		// uses: String, TheVault
   methodConstruct();		// uses: Buckets
   blockConstruct();		// uses: Method
   classConstruct();		// uses: Method
   fcnConstruct();		// uses: NullClass
   garbageManConstruct();	// uses: TheVault
   numberConstruct();	// uses: TheVault, Buckkets, --> -1,0,1, 0.0,1.0, pi,e
   pipeConstruct();		// uses: TheVault, buckets
   miscConstruct();		// uses: TheVault
   atomicConstruct();		// uses: TheVault, Method/K*String buckets
   dictionaryConstructPartII();	// uses: TheVault
   fileConstruct();		// uses: TheVault, Dictionary/Int/Float buckets
   tcpSocketConstruct();	// uses: TheVault, Dictionary/etc buckets
   vmConstruct();		// uses: nada
   asyncConstruct();		// uses: nada
   walkerConstruct();		// uses: TheVault
   sinkConstruct();		// uses: TheVault

   classPath	= listCreate(3,0x1,I_IMMORTAL,NoVM);	// List, not Tuple
   libPath	= listCreate(3,0x1,I_IMMORTAL,NoVM);
   includePath	= listCreate(3,0x1,I_IMMORTAL,NoVM);

   argV = tupleCreate(argc,I_IMMORTAL,NoVM);
   #ifdef _MSC_VER
//      _get_pgmptr(&ptr);	// when I get a newer compiler
      ptr = _pgmptr;
      tupleAppend(argV,kStringCreate(ptr,0,I_OWNED,NoVM));  // argv[0]
      for (i = 1; i < argc; i++)
	 tupleAppend(argV,kStringCreate(argv[i],0,I_OWNED,NoVM));
   #elif defined(__unix__)
      for (i = 0; i < argc; i++)
	 tupleAppend(argV,kStringCreate(argv[i],0,I_OWNED,NoVM));
   #endif

   cacheSomeGlobalNames();

   // start GC last so I can play fast and loose and 
   // BEFORE loadWad runs construtors
   startGarbageMan();

//run boot fcn
   ptr = getenv("zklIgnoreWad");
   if ((ptr && *ptr == '1') || !loadWad(0,0,NoArglist,NoVM))
   {
      printf("Ignoring wad\n");

      ptr = getenv("zklRoot");
      if (ptr)
      {
	 char buf[200];
	 strcpy(buf,ptr); strcat(buf,"/Built");
      	 listAppend(classPath,kStringCreate(buf,0,I_OWNED,NoVM),NoVM);
      }
      else	// time to take a SWAG
      {
	 #ifdef _MSC_VER
	    listAppend(classPath,kStringCreate("C:/ZKL/Built",0,I_OWNED,NoVM),NoVM);
	 #else
	    ptr = getenv("HOME");
	    if (ptr) listAppend(classPath,stringCat(NoVM,ptr,"/ZKL/Built",(char *)0),NoVM);
	 #endif
      }

      readRootClass("exception.zsc",	     "",	NoArglist,NoVM);
      if (!ExceptionContainer || !ExceptionClass || !ExTheEnd)
         vmHalt("Exceptions not defined");
      readRootClass("minImport.zsc",	     "",	NoArglist,NoVM);
      readRootClass("Utils/helpers.zsc",  (char *)1,	NoArglist,NoVM);
      readRootClass("thread.zsc",	     "",	NoArglist,NoVM);
   }

//   startGarbageMan();		// now clean up the mess created above
}

#if 0
    // A method to finish booting the vm so I can fence
static Instance *VM_boot(Instance *self,pArglist arglist,pVM vm)
{
   static int booted = 0;
   char *ptr = getenv("zklIgnoreWad");

   if (booted) return Void;
   booted = 1;
   if ((ptr && *ptr == '1') || !loadWad(0,0,NoArglist,vm))
   {
      printf("Ignoring wad\n");

      ptr = getenv("zklRoot");
      if (ptr)
      {
	 char buf[200];
	 strcpy(buf,ptr); strcat(buf,"/Built");
      	 listAppend(classPath,kStringCreate(buf,0,I_OWNED,vm),vm);
      }
      else	// time to take a SWAG
      {
	 #ifdef _MSC_VER
	    listAppend(classPath,kStringCreate("C:/ZKL/Built",0,I_OWNED,vm),vm);
	 #else
	    ptr = getenv("HOME");
	    if (ptr) listAppend(classPath,stringCat(vm,ptr,"/ZKL/Built",(char *)0),vm);
	 #endif
      }

      readRootClass("exception.zsc",	     "",	NoArglist,vm);
      if (!ExceptionContainer || !ExceptionClass || !ExTheEnd)
         vmHalt("Exceptions not defined");
      readRootClass("minImport.zsc",	     "",	NoArglist,vm);
      readRootClass("Utils/helpers.zsc",  (char *)1,	NoArglist,vm);
      readRootClass("thread.zsc",	     "",	NoArglist,vm);
   }
   return BoolTrue;
}

static Instance *bootFcn;

    // fcn boot{ vm.boot() } where vm.boot is a method
    // This gives the boot time code a VM it can use to Fence
static void constructBootFcn(void)
{
   static StringTable bootNames =
   {
      1,		// n
      "__bootFcn\0",	// Strings: name, vaultPath
      10,		// size
   };

   static Byte bits[4] = { opVM, opCallIMethodNZ,0, opDone };

   static ZKL_Code bootCode =
   {
      bits,
      "",		// no strings
      sizeof(bits),0,	// sizes
      0,		// no kstrings
      (Byte *)"",	// no map
   };
   bootFcn = fcnEmbryoWithStaticCode(&bootNames,0,0,&bootCode,0,I_IMMORTAL,NoVM);
   bootFcn->iflag = 1;		// make runnable
   FCN_IS_STATIC(bootFcn) = 1;	// just cuz it is
}
#endif

    /* Cache some classes that are used a lot.  Grab the first matches,
     *   assume that these are from loading the wad. Ignore any changes made
     *   to the Vault after that.
     * Exceptions are thrown before zklConstruct() has finished.
     */
void cacheExceptions(void)
{
   if (!ExceptionContainer)	// only do this once, early
   {
      ExceptionContainer = vaultFind("Exception",0,NoVM);
      if (ExceptionContainer)
      {
	 ExceptionClass = classFindClass(ExceptionContainer,"Exception",0,NoVM);
	 ExTheEnd	= classFindClass(ExceptionContainer,"TheEnd",0,NoVM);
      }
   }
   if (!Utils_Helpers) Utils_Helpers=vaultFind("Utils.Helpers",0,NoVM);
}

    // marking is per stack

    // mightStall is per stack
#define mightStall(vm) CAI_VALUE(&vm->rootVM->mightStall)

    /* There is a race here: ref can point to a dying VM but the VM hasn't
     * nuked the ref so this can write into a dead VM.
     * I don't know how this happens but the debugger shows that it does
     * (fix in place).
     * 
     * Weak refs keep fibers alive because that is the only way a stalled
     * fiber can be resumed.
     */
static void vmwrMarker(Instance *ref)
{
   VM *vm = pVM2VM(((VMWR *)ref)->pVM);	   // zero if vm has died
   if (vm && isRunning(vm) && vm->isFiber) // both running & stalled fibers
      CAI_ONE(&vm->marked);
}

void vmProcessInterrupt(pVM self);

static void   vmCall(VM *,Instance *,pMethod,int tailCall,int Z);
static void   _checkArglist(VM *,int n,char *opName);
static void   barf(VM *, Instance *eClass);
static void   vmClear(VM *);
static void   _mark(VM *);
static void   clearFences(VM *);
static void   markFences(Fence *, SpinLock *);
static void   tailCall(int Z,VM *);
static Byte  *tailRecurse(VM *);
static void   squirt(VM *self, Instance *, int offset, int n);

static Instance *vmGetArg(VM *, int n);

static Fence	globalFence;		// Mark only fences, zero'ed
static SpinLock globalFenceLock;

static IBucketHeader *vmwrBuckets;


    // GC "marking" flags, 0 is initial state
#define VM_MARK_YOURSELF	1
#define VM_WAIT_UNTIL_MARKED	2
#define VM_MARKED		9

static void vmInit(VM *self,pVM parent)	// self is doo-doo
{
   self->arglist2 = listCreate(210,0x0,I_SPECIAL,parent);
   vmClear(self);
}

    // _vmLock needs to be held if self is visible to GC
    // Some resources are just reset because they will be used again and
    //   reuse is faster than reallocation.
static void vmClear(VM *self)
{
   self->parentVM = self->childVM = self->rootVM = 0;
   self->prevVM = self->nextVM = 0;
   // blockStack handled elsewhere
   pcClear(&self->pc);
   self->result = self->regX = Void;
   self->stki = self->argOffset = self->numArgs = 0;
   self->recursionCount = self->isFiber = self->isStrand = 0;
   // don't change id
   listClear(self->arglist2,0);
   self->self = 0;

   CAI_INIT(&self->marked);
   CAI_INIT(&self->marking);
   CAI_INIT(&self->mightStall);
   CAI_INIT(&self->running);
   CAI_INIT(&self->barfing);

   self->isThread	  = 0;
   self->pid		  = 0;
   self->interruptEvent   = 0;
   self->pendingException = self->xxception = 0;
   self->env		  = 0;
   clearFences(self);
   spinLockInit(&self->fenceLock);
   self->ref		  = 0;
   self->mrc		  = 0;
   self->icount		  = 0;

   #if defined(PTHREADS) || MARKII
      spinLockInit(&self->gcLock);
   #endif
}

   // You do NOT call this
Instance *vmArglist2(pVM vm) { return ((VM *)vm)->arglist2; }

int vmIsRunning(pVM vm) { return isRunning((VM *)vm); }
#if 0
   int vmIsBarfing(pVM self) { return isBarfing(I2VM(self)); }
#endif

    /* Definitions:
     *   Leaf VM:  A leaf is a VM that isn't the calling VM of any other
     *     VM, ie not a parent. Can be a root.
     *   Root VM:  A VM that has no calling VM.  Threads are root VMs.  A
     *     root can be a parent or a leaf.
     *   Parent VM:  A VM that is the calling VM for another VM. Might be a
     *      root but can't be a leaf.
     */
//static IBucketHeader vmBuckets;
static VM	  *vmLiveList = 0;
static VM	  *vmFreeList = 0;
static CAtomicInt  _gcCount;	// a semaphore
static int	   vmHW    = 0;	// 9 to compile parser, 180 when running tests
static int	   numVMs  = 0;	// number of live VMs
static int64_t	   vmCount = 0;	// number of VMs ever allocated

#if 0
void vmShutDown(void)
{
   shutDownException = classFindClass(ExceptionContainer,"ShutDown",0,NoVM);
   if (shutDownException)
   {
      printf("Found exception\n");
   send ShutDown to all roots
   wait for no vm
   gc several times
   }
}
#endif

    /* VM layout:
     * vmLiveList --> root <--> root <--> root ...
     *                 |	 ^	   ^
     *                 V	 |	   |
     *                 0	 V	   V
     *                       child/leaf	 child
     *                           |	   ^
     *                           V	   |
     *                           0	   V
     *                           	  ...
     * Root VM:  The mother thread, Thread, stalled fiber.
     *    A root is used to control GC (marking, mightStall) for the stack.
     * Child VM: VM running a fcn, etc. No siblings (ie nextVM & prevVM are zero)
     * Leaf VM:  The last child VM. No siblings.
     *    Exceptions are thrown against leafs.
     * A stalled fiber is both a root and leaf.
     * 
     * You need to be holding _vmLock
     */
static void prependRootVM(VM *vm)
{
   if (vmLiveList) VM_PREV(vmLiveList) = vm;
   vm->prevVM   = 0;
   vm->nextVM   = vmLiveList; vmLiveList = vm;
   vm->parentVM = 0;	// Roots have no parent
   vm->rootVM   = vm;	// the root of a root is the root
}

static void unlinkRootVM(VM *vm)
{
   if (vmLiveList == vm)	// first in list
   {
      vmLiveList = VM_NEXT(vm);
	 // yes, numVMs can be zero
      if (vmLiveList) VM_PREV(vmLiveList) = 0;
   }
   else
   {
      VM *next = VM_NEXT(vm);
      vm->prevVM->nextVM = next;
      if (next) VM_PREV(next) = vm->prevVM;	// not last
   }
   VM_NEXT(vm) = VM_PREV(vm) = 0;
}

    // You need to be holding _vmLock
static size_t walkVMs(size_t (*fcn)(VM *, void *X), void *X)
{
   size_t  s;
   VM	  *c, *vm;

   for (vm = vmLiveList; vm; vm = vm->nextVM)
   {
      if ((s = fcn(vm,X))) return s;
      for (c = vm->childVM; c; c = c->childVM)
	 if ((s = fcn(c,X))) return s;
   }
   return 0;
}

#if 0
    // Will return zero when called before there is a mother thread
pVM callingTheMotherShip(void)
{
   VM *vm;
   SPIN_LOCK_ACQUIRE(&_vmLock);
   for (vm = vmLiveList; vm; vm = vm->nextVM)
   {
      if (vm->isThread || vm->isFiber) continue;
      SPIN_LOCK_RELEASE(&_vmLock);
      return (pVM)vm;
   }
   SPIN_LOCK_RELEASE(&_vmLock);
   return 0;
}
#endif

#if 0
static void isThatYou(VM *vm)
{
   int s = 0;
   VM *v, *root=vm->rootVM;

   if(!vm){ printf("zero vm\n"); return; }
//   SPIN_LOCK_ACQUIRE(&_vmLock);
      for (v = vmLiveList; v; v = v->nextVM)
	 if (v==root){ s = 1; break; }
//   SPIN_LOCK_RELEASE(&_vmLock);
   if(!s)
      printf("Iz dead\n");  // debugger target
   else printf("Iz alive\n");
   fflush(stdout);
}
#endif

pVM vmRoot(pVM vm) { return (pVM)ROOT_VM(vm); }

pVM vmLeaf(pVM vm)	// --> lowest VM on this stack
{
   VM *self = (VM *)vm;
   while(self->childVM) self = self->childVM;
   return (pVM)self;
}

pVM vmParent(pVM self) { return (pVM)PARENT_VM(self); }

void vmSetX(pVM vm,Instance *x) { pVM2VM(vm)->regX = x; }

    /* Recursion checking:  I don't have to worry about the VM stack
     *   (checked), but I do have to worry about the C stack.  Since (I
     *   hope) the only way to recurse in C is via vmRun(), that usually
     *   involves creating a new VM, so I add up the cascading VMs, and if
     *   there are too many, assume there is some infinite resursion going
     *   on.  This is a pretty long chain recursion so the C stack gets
     *   pretty deep pretty fast.  On MS VC++ 7, the C stack blows at around
     *   recursionCount == 345 (debug), 1900 (release).
     * class { fcn toString { println(self) }} == infinite recursion
     * I wish there was a way to catch statck overflow via the C runtime.
     * 
     * VM recycling usually isn't a problem but, when it is, it can be a real
     * pain (eg when you have to use vm.kick, you can easily throw at a
     * recycled VM, instead of the one you intended).  Thread control is
     * sorely lacking.  So, as cheap workaround and to give VMs some
     * uniqueness, give each "new" VM a new id that doesn't repeat very
     * often.
     * 
     * GC:
     *   If the gc thread is marking, it holds _vmLock.  Which means all VM
     *     threads that try to create a VM will park here until marking has
     *     been completed.
     *   The other possibility is somebody other than GCT holds _vmLock (ie
     *     normal usage).
     * 
     * parentVM: vm or zero if this is a root VM.
     * 
     * Can't use IBUCKETS because freed VMs are still alive and a instance
     * in a free bucket isn't a valid instance anymore.
     * 
     * threadToBe:
     *    0 : New VM is not going to be a Thread
     *    1 : New VM is going to be a Thread
     *    3 : New VM will be running in a rogue thread, sandboxed VM
     * 
     * NOTE!!!  Only fiber VMs are GC'd.  Otherwise: you create, you free.
     *    barf() handles exceptions.
     */
#define MAX_C_STACK 100  // nested apply/filter/pump/reduce, compiling parser ~20
pVM vmCreate(pVM parentVM, ZKL_Block **block, int flags, pVM vm)
{
   VM  *newVM;
   int rc = 0, different = (parentVM != vm);
   int willBeThread = (flags & VM_THREAD2BE);
   int motherless   = 	// create rootVM?
      (flags & VM_MOTHERLESS_CHILD) || willBeThread;

      	// Loading initial wad, no VMs exist
	// Threads are roots and might not have vm (splashdown)
        // Otherwise, no other roots (confuses GC, unknown PID)
   if (!motherless && vmLiveList)
   {
      if (!vm)       vmHalt("vmCreate: No VM");
      if (!parentVM) vmHalt("vmCreate: No parent VM");
   }

   if (vm && ROOT_VM(vm)->marking == VM_MARK_YOURSELF) _mark((VM *)vm);

   if (parentVM)
   {
      #if GC_SANITY_CHECK
      	 // I'd like to check for a dead parent but don't know how
	 if (CHILD_VM(parentVM)) 
vmHalt("vmCreate: parentVM is not a leaf");
	 if (motherless) vmHalt("Leafs can't be threads");
      #endif

// run away fibers?
rc = 0; newVM = (VM*)parentVM; while((newVM=newVM->parentVM)) rc++;
if (rc>15) goto ohshit;

      rc = pVM2VM(parentVM)->recursionCount + 1;
      if (rc > MAX_C_STACK)
      {
      ohshit:
	 // reset recursionCount so I can create an exception
	 pVM2VM(parentVM)->recursionCount = MAX_C_STACK - 5;
      	 vmThrow(parentVM,E_VM_ERROR,"C stack too big (possible run away Method recursion)");
      }

      if (ROOT_VM(parentVM)->marking == VM_MARK_YOURSELF) _mark((VM *)parentVM);
   }
//   else if (threadToBe == 0) threadToBe = 3;	// no parent --> rogue
   // else no parent --> new root VM

   	// if gc is happening, this will stall until marking is finished
        // vm can be zero (loader during construction) as can parentVM.
   if (different) vmMightStall(parentVM,1);
#if 1
   spinLockAcquire2(&_vmLock,0,vm);	// stallable
      if (vmFreeList)
      {
	 newVM = vmFreeList; vmFreeList = vmFreeList->nextVM;
	 vmClear(newVM);
      }
      else
      {
//!!!! argh! how to catch no mem? thread is a problem
	 newVM = (VM *)ZMALLOC(sizeof(VM));	// never freed
	 newVM->blockStack = blockStackAlloc((pVM)newVM);
	 vmInit(newVM,parentVM);
	 newVM->id = ++vmHW;
      }
#else
   newVM = (VM *)ibucketAllocate(&vmBuckets,0,1,vm); // throws
   spinLockAcquire2(&_vmLock,0,vm);	// stallable, lock needs to be here
      if (newVM->id) vmClear(newVM);		// "used" VM
      else		// brand spanking new VM
      {
	 newVM->blockStack = blockStackAlloc((pVM)newVM);
	 vmInit(newVM,parentVM);
	 newVM->id = ++vmHW;
      }
#endif

      numVMs++; vmCount++;

      newVM->recursionCount = rc;

      if (block) *block = blockCreate(&newVM->blockStack,(pVM)newVM);

      if (parentVM && !motherless)	// leaf VM
      {
	 if (CHILD_VM(parentVM))
	 {
	    SPIN_LOCK_RELEASE(&_vmLock);
	    vmThrow((pVM)vm,E_ASSERTION_ERROR,"vmCreate: parentVM has children!");
	 }
	 newVM->parentVM    = (VM *)parentVM;
	 CHILD_VM(parentVM) = newVM;
	 newVM->rootVM	    = ROOT_VM(parentVM);
	 VM_NEXT(newVM)     = VM_PREV(newVM) = 0;	// no siblings
      }
      else prependRootVM(newVM);	// root VM

      CAI_INIT(&newVM->marking);
      if (willBeThread) newVM->isThread = (void *)1; //filled in @ thread start
      	// The entire VM stack has the same pid's for ease of programming
	// Note: if a new thread, this is actually the calling VMs PID
      #ifdef _MSC_VER
         newVM->pid = GetCurrentThreadId();	// in case this is a rogue
      #elif defined(PTHREADS)
	 newVM->pid = pthread_self();
      #endif
   SPIN_LOCK_RELEASE(&_vmLock);
   if (different) vmMightStall(parentVM,0);  //!!!??? doesn't seem to be needed

   return (pVM)newVM;
}

    // _vmLock is held by caller
    // no GC
static void _vmFree(VM *vm)
{
   VMWR *ref = ((VM *)vm)->ref;

if (((VM *)vm)->mrc) vmHalt("MRC is outta wack");
   if (CHILD_VM(vm))
      vmHalt("_vmFree: Trying to free VM in middle of stack");
   notRunning(vm);
   SPIN_LOCK_ACQUIRE(&vm->fenceLock);
      clearFences(vm);
   SPIN_LOCK_RELEASE(&vm->fenceLock);
   if (ref)	// nuke it again for fibers (they don't call vmFree())
   {
      CAP_SET(&ref->pVM,0);
      asyncWriteHappened((Instance *)ref);	// it's dead Jim
   }
   vm->ref = 0;		// gc the ref
   vm->blockStack = blockStackReset(vm->blockStack);	// might be big

	 // Move VM from the live list to free list
   if (vm->parentVM) vm->parentVM->childVM = 0;	// a leaf
   else unlinkRootVM(vm);			// root VM

#if 1
     // Then prepend to the free list
   vm->nextVM = vmFreeList; vmFreeList = vm;
#else
   bucketFree(&vmBuckets,(Instance *)vm);
#endif

   numVMs--;
}

    /* A VM being freed might be tagged for marking.  Or, its parent might
     * be instead.  That can happen if the parent is tagged for marking, the
     * parent creates a child VM and that child is freed.  So, check to see
     * if the parent needs marking.  If so, mark the entire tree.
     * EG fcn calls call finalizeArglist2() and that flips VMs fast.
     * I don't think this happens but the code is cheap.
     */
     // No GC but might be marked, might be stalled by others getting gc'd
void vmFree(pVM vm)
{
   VMWR *ref = ((VM *)vm)->ref;

if (((VM *)vm)->mrc) vmHalt("MRC is outta wack.");  // Generator with 'wrap

   	// Duplicate these next lines for GC and VM_throw()
   notRunning(pVM2VM(vm));
   if (ref)	// nuke the ref before can be stalled by acquire
   {
      CAP_SET(&ref->pVM,0);
      asyncWriteHappened((Instance *)ref);	// it's dead Jim
      ((VM *)vm)->ref = 0;		// gc the ref
   }

   if (ROOT_VM(vm)->marking == VM_MARK_YOURSELF) _mark((VM *)vm);
   spinLockAcquire2(&_vmLock,0,vm);	// can stall
      _vmFree((VM *)vm);
   SPIN_LOCK_RELEASE(&_vmLock);
}

///////////////////// Garbage Collection /////////////////////////////

//////////////////// Running in GC thread ////////////////////////////

    /* Called from GC thread OR VM thread, not both.  Single threaded PER VM
     *   stack, but can be marking multiple stacks at once.
     * You hold _vmLock.
     * If a VM is running in C code, it is NOT told to mark itself UNLESS
     * the C code has set mightStall, in which case, it is marked.  Thus,
     * you don't have to worry about protecting your container contents (eg
     * as your are creating it) unless you can stall during the process. The
     * exception to this is if you call something that forces a GC (like out
     * of memory). Fence that possibility.
     * You still need thread safe containers because your container might be
     * marked from another VM thread (as it is being marked here by your
     * thread).
     */
static void _mark(VM *root)
{
   VM *vm;

   root = root->rootVM;  // for sure for sure

   for(vm = root; vm; vm = vm->childVM)
   {
//    vm->marking = VM_MARKED;  // mostly for debugging, only root needs this

      markFences(&vm->fence,0);

	// stack, arglist2 have been created immortal
	// parentVM is in the vmList, so it will be marked elsewhere
	// Asm.Code gets marked by someone else
#if 0
      if (!inBlockStack(vm->result,vm->blockStack)) // opBlockUp
	 instanceMark(vm->result);
else
printf("HOHO\n");
#else
      instanceMark(vm->result);
#endif
//instanceVerify(vm->regX,1,(pVM)vm);
      instanceMark(vm->regX);
      instanceMark(VM_SELF(vm));	// Class
      instanceMark(vm->pendingException); instanceMark(vm->xxception);
      instanceMark(vm->arglist2);
      if (vm->ref) instanceMark((Instance *)vm->ref);
      blockMarker(vm->blockStack);

      {
	 int        n     = vm->stki;
	 Instance **stack = vm->stack;
	 while (n--) instanceMark(*stack++);  // much faster than for(). why?
      }

      // Fences are marked after all the VMs are marked  !!!why???
   }

   if (root->isThread) markThread(root->isThread);
for (vm=root; vm; vm=vm->childVM) CAI_SET(&vm->marking,VM_MARKED);

   if (0 == CAI_DEC(&_gcCount)) asyncWroteN((Instance *)&_gcCount,0);
}

#if defined(_MSC_VER) && !MARKII

static void vmMark(VM *root,int pid)
{
   HANDLE  handle;
   int	   s;

   if (pid == 0)	// Thread embryos
   {
      CAI_INC(&_gcCount);
      CAI_SET(&root->marking,VM_MARK_YOURSELF);
      return;
   }

   handle = OpenThread(THREAD_ALL_ACCESS,0,pid);
   if (handle == NULL)	// OpenThread(0) -> 87
   {
      printf(">>>>>>>>vmMark: OpenThread(%d) failed: %d\n",pid,GetLastError());
      return;	// assume dead thread
   }
	// (DWORD)-1 on error, otherwise, previous suspend count
   s = SuspendThread(handle);
   if (s == (DWORD)-1)
      printf(">>>>>>>>vmMark: SuspendThread(%d) failed: %d\n",pid,GetLastError());
   else if (s)	// already suspended
   {
      printf(">>>>>>>>vmMark: %d is already suspended: %d %d\n",pid,s,isRunning(root));
      ResumeThread(handle);
      s = 1;	// don't complain twice
   }
   else		// s == 0, thread suspended
   {
      if (CAI_VALUE(&root->mightStall))
      {					// VM might be stalled in C code
	 CAI_INC(&_gcCount);
	 _mark(root);			// mark VM while it is halted
	 s = ResumeThread(handle);	// VM: carry on
      }
      else				// let the VM mark itself
      {
	 CAI_SET(&root->marking,VM_MARK_YOURSELF);
	 CAI_INC(&_gcCount);
	 s = ResumeThread(handle);	    // let VM call _mark()
	    	// wait for the VM to finish marking later
	    	// if vm dies (eg throws), vmFree() will mark
      }
   }
   // Thread is running
   if (s != 1)	// something isn't right
      printf("???????????? ResumeThread isn't right: %d %d\n",s,pid);
   CloseHandle(handle);
   // VM [stack] has now been marked or has mark pending
}

   /* Mark the contents of all the running VMs.
     * The hairball here is VMs can be stacked:  fibers, fcns running
     * default args, methods that create walkers, etc.  The leafs (the VM
     * that is actually running) safe point is the safe point for the entire
     * stack and VMs in stack will likely be out of sync with the leaf.  So,
     * the vmLiveList is quasi sorted, with leaf VMs towards the front of
     * the list, in front of any parents.  The sorting occurs "naturally",
     * just by adding new VMs to the front of the list.  Except for fibers,
     * they have to relocate.
     * 
     * I also want to free any unreferenced stalled fibers. These are fibers
     * that have yielded and nobody holds a reference to them. They usually
     * have a reference to themselves so mark everything but stalled fibers,
     * then look at the stalled fibers. If it is marked, it lives,
     * otherwise, free it.
     * 
     * Marked VMs are skipped (they were marked as part of a stack mark),
     * HOWEVER, if the stack mark is pending, I could be looking at the
     * middle of a stack.
     * 
     * _mark is setting gc_flags in another thread. So don't use them.
     * 
     * The leafs are the set of VMs that are not parents
     */
void markVMs(void)	// called from GC thread
{
   VM  *i;
   int  stalledFibers = 0;

   instanceMark(ExceptionContainer);   // in case it is nuked from the Vault

   spinLockAcquire2(&_vmLock,0,NoVM);	// Don't create or free VMs
      CAI_ZERO(&_gcCount);
   	// Marks have been cleared
   	// Mark all leafs by marking all roots
      for (i = vmLiveList; i; i = i->nextVM)	// the VM stacks (roots)
      {
	 VM *root = i;
	 if (root->isFiber)	// stalled fiber, ignore for now
	    { stalledFibers = 1; continue; }

	       // find PID for this stack
	 switch((size_t)root->isThread)
	 {
//	    case 2: case 3:	// GC (running asILayDying) or rogue thread
	    case 0:	// Not a Thread, mother thread or rogue (in thread?)
	       vmMark(root,root->pid);
	       break;
	    default:		// Thread & Thread embryos 
	    {
	       int pid;
	       switch(threadPID(root->isThread,&pid))
	       {
		  default: printf("Bogus thread\n");
		     break;
		  case 0: break;	// Dead Thread
		  case 1:		// Thread embryo 
		     while(1)     	// wait for Thread to start running
		     {
			switch(threadPID(root->isThread,&pid))
			{
			   case 0:		     goto embryoMarked;
			   case 2: vmMark(root,pid); goto embryoMarked;
			}
			//SwitchToThread();	// snooze
			SPIN_STALL;
		     }
		  embryoMarked:
		     break;
		  case 2: vmMark(root,pid); break;	// Running Thread
	       } // switch
	       break;
	    }
	 } // switch
      } // for

      // Some thread VMs may be marking themselves at this very moment
      // Wait for self marking VMs to finish marking
      if (CAI_VALUE(&_gcCount)) caiWait4(&_gcCount,0,1,0);

//      markAllFences();	// got _vmLock
// BAD! if marking fence while somebody is modifing object in it
markFences(&globalFence,&globalFenceLock);

      	// Look for live stalled fibers: those marked by other VMs
        // Stalled fibers are roots with no children.
	// VMWRs marks these
      if (stalledFibers)
      {
	 VM *next;
	 for (i = vmLiveList; i; i = next)
	 {
	    next = i->nextVM;		// i might get hammered
	    if (!i->isFiber) continue;  // not a stalled fiber
	    if (i->marked) _mark(i);    // somebody owns this fiber
	    else _vmFree((VM *)i);	// nobody marked this stalled fiber
	 } // for
      }
   SPIN_LOCK_RELEASE(&_vmLock);
}

   // This is single threaded, not thread safe
   // see MARKII version for more info
int vmMightStall(pVM self,int mightStall) // --> previous value
{
   CAtomicInt *stallFlag;
   int	       amStalled;
   VM	      *root;

	// No VM, no stalling. EG GarbageMan does this
   if (!self) return 0;

   root      = ROOT_VM(self);
   stallFlag = &root->mightStall;
   amStalled = CAI_VALUE(stallFlag);
   if (mightStall)
   {
      if (mightStall == 666)	// just check
      {
      check:
	 if (root->marking == VM_MARK_YOURSELF) _mark(root);
	 return amStalled;
      }
      else if (mightStall == -1)	// soft 0/unstall
      {
	 if (amStalled) goto unstall;   // stalled, unstall
	 goto check;
      }

      if (amStalled)
	 vmThrow(self,E_VM_ERROR,"vmMightStall(1): Already stalled");

      CAI_INC(stallFlag);
      if (root->marking == VM_MARK_YOURSELF) _mark(root);
      return amStalled;
   }

unstall:
   if (CAI_VALUE(stallFlag) == 0)
      vmThrow(self,E_VM_ERROR,"vmMightStall(0): Not stalled");

   CAI_DEC(stallFlag);
   return amStalled;
}

#elif defined(PTHREADS) || MARKII

    // linux pThreads doesn't support pthread_suspend
    // So I let the thread mark itself if it is running or I mark it if it
    // is stalled.

    /* ARGH! It looks like context switching is so slow that if I see a
     * VM_WAIT_UNTIL_MARKED, I can miss an entire GC cycle, ie GC finishes,
     * _gcCount goes low but GC restarts and increments _gcCount before I
     * notice. Doesn't make sense to me either but that is what I saw. I do
     * see marking set to VM_MARKED but that isn't atomic so ...
     */
    // markVMs() might be holding root->gcLock, which means I wait if 
    // GCT is setting bits. Or vice versa if I'm marking when GC restarts
    // NOT THREAD SAFE! ONLY self should call this
    /* Stalls can NOT be stacked, MARKII GC can't deal with this case:
     *    FLTK stalls in its main loop
     *    FLTK calls zkl fcn
     *    GC sees FLTK VM as stalled, marks it
     *    FLTK callback is running in FLTK VM while it is being marked.
     *    FUBAR
     *    FLTK has to unstall/stall around running callbacks
     */
     /* Input:
      *   self: NoVM: this is a noop
      *   mightStall:
      *     1: Inform GC that a method is entering C code and may stay there
      *	       for a while.
      *     0: The method has returned from that C code and will re-enter
      *	       the zkl main loop.
      *    -1: The method might be stalled but wants to run a zkl fcn.
      *        If stalled,     this is the same as vmMightStall(0).
      *        If not stalled, this is the same as vmMightStall(666)
      *        The caller needs to look at the return values and call
      *        vmMightStall(1) if 1 is returned.
      *   666: Just check to see self needs to mark itself.
      */
int vmMightStall(pVM self,int mightStall)	// --> previous value
{
   CAtomicInt *stallFlag;
   int	       amStalled;
   VM	      *root;

	// No VM, no stalling. EG GarbageMan does this
   if (!self) return 0;

   root      = ROOT_VM(self);
   stallFlag = &root->mightStall;
   SPIN_LOCK_ACQUIRE2(&root->gcLock);
      amStalled = CAI_VALUE(stallFlag);
      if (mightStall)	// headed into slow C code
      {
	 if (mightStall == 666)	// just check, not stalling
	 {
	 check:
	    switch(root->marking)
	    {
	       case VM_MARK_YOURSELF: _mark(root); break;
	       case VM_WAIT_UNTIL_MARKED:
		  vmHalt("vmMightStall: uhhh, I don't think so");
	    }
	 }
	 else if (mightStall == -1)	// soft 0/unstall
	 {
	    if (amStalled) goto unstall;   // if stalled, unstall
	    goto check;
	 }
	 else
	 {
	    if (CAI_VALUE(stallFlag))  // case: self->running==0
	       vmThrow(self,E_ASSERTION_ERROR,
		       "vmMightStall(1): Already stalled");
	    CAI_INC(stallFlag);
	    if (root->marking == VM_MARK_YOURSELF) _mark(root);
	    // handle VM_wait_until when unstalling
	 }
	 SPIN_LOCK_RELEASE(&root->gcLock);
	 return amStalled;
      }

   unstall:
      if (CAI_VALUE(stallFlag) == 0)
         vmThrow(self,E_ASSERTION_ERROR,"vmMightStall(0): Not stalled");

      // returning from slow C code, is the GC thread marking me?
      if (root->marking == VM_WAIT_UNTIL_MARKED)
	 while(1)
	 {
	    if (!CAI_VALUE(&_gcCount)) break;	// everybody is marked
	    if (!isRunning(root)) break;		      // I died
	    if (root->marking != VM_WAIT_UNTIL_MARKED) break; // I been marked
//	    pthread_yield(pthread_self());
	    SPIN_STALL;
	 }
      CAI_DEC(stallFlag);
   SPIN_LOCK_RELEASE(&root->gcLock);
   return amStalled;
}

    // root->gcLock is held, which means vmMightStall() will stall
static void vmMark(VM *root)	// don't actually do anything, just set flags
{
   VM *vm;

   CAI_INC(&_gcCount);		// one more VM stack needs marking

   if (CAI_VALUE(&root->mightStall)) // VM might be stalled in C code
        CAI_SET(&root->marking,VM_WAIT_UNTIL_MARKED);
   else CAI_SET(&root->marking,VM_MARK_YOURSELF);  // let the VM mark itself

	 // zero children, for debugging
   for(vm = root->childVM; vm; vm = vm->childVM) CAI_SET(&vm->marking,0);
}

void markVMs(void)	// called from GC thread, ie this IS the GC pthread
{
   VM  *root;
   int  stalledFibers = 0;

   instanceMark(ExceptionContainer);	// in case it is nuked from the Vault

   spinLockAcquire2(&_vmLock,0,NoVM);	// Don't create or free VMs
      CAI_ZERO(&_gcCount);	// count of VMs being marked

      // GC marks have been cleared
      for (root = vmLiveList; root; root = root->nextVM)  // the VM roots
      {
	 CAI_SET(&root->marking,0);
	 if (root->isFiber)	// stalled fiber, ignore for now
	    { stalledFibers = 1; continue; }

	 spinLockAcquire(&root->gcLock);	// fight with vmMightStall()
	    vmMark(root);	// tell stack what they need to do
         SPIN_LOCK_RELEASE(&root->gcLock);
      } // for
      // gcLocks have been released, may have been grabbed by vmMightStall()

      for (root = vmLiveList; root; root = root->nextVM)  // mark stalled VMs
      {
	 if (root->marking == VM_WAIT_UNTIL_MARKED)	// VM is stalled
	    _mark(root);
	 // else the VM is marking itself
      }

      // Some thread VMs are marking themselves at this very moment
      // Wait for self marking VMs to finish marking
      if (CAI_VALUE(&_gcCount)) caiWait4(&_gcCount,0,1,0);

//      markAllFences();	// got _vmLock
// BAD! if marking fence while somebody is modifing object in it
markFences(&globalFence,&globalFenceLock);

      	// Look for live stalled fibers: those marked by other VMs
        // Stalled fibers are roots with no children.
	// VMWRs mark thse
      if (stalledFibers)
      {
	 VM *next, *i;
	 for (i = vmLiveList; i; i = next)
	 {
	    next = i->nextVM;		// i might get hammered
	    if (!i->isFiber) continue;	// not a stalled fiber
	    if (i->marked) _mark(i);	// somebody owns this fiber
	    else _vmFree(i);		// nobody marked this stalled fiber
	 } // for
      }
   SPIN_LOCK_RELEASE(&_vmLock);
}

#endif	// mark VMs

    /* Reset marks for pending GC sweep. vmCreate() will clear the marks for
     * any VMs created between now and when vmMark() is called.
     * Don't set vm->marking, it is zero or VM_MARKED, not atomic, let the
     *   thread set it.
     */
static size_t _resetMarks(VM *vm,void *X)
{
   CAI_ZERO(&vm->marked);
   GC_CLEAR_MARK(vm->arglist2);
   return 0;
}
void vmResetMarks(void)
{
   spinLockAcquire(&_vmLock);
      walkVMs(_resetMarks,0);
   SPIN_LOCK_RELEASE(&_vmLock);
}

/* ******************************************************************** */
/* ****************************** Fences ****************************** */
/* ******************************************************************** */

    /* A fence allows C code to hook into exceptions (in a limited way).
     * A Fence struct forms a "catch" with a payload.  When a C function
     * registers a fence and a exception is thrown (via vmThrow), the
     * function is called with the fence & thrown exception and the C code
     * can clean up.
     * Important notes:
     *   - The fence lives in the C stack, and you MUST remove it before you
     *     return from your code.
     *   - Fences works for C calls deeper than the caller's fence
     *   - Fences are for a single thread but the GC thread might call
     *     markFences.
     *   - You can extend the Fence struct to add more payload:
     *      struct BiggerFence {
     *         Fence fence;
     *         Instance *arg, *foo;
     *      }
     * GC marking: A Fence can be used to as a marker during GC:
     *   fence.i, .i1..3 hold arbitrary instances that will be marked.
     * Locks are needed because Fences are also used to protected objects
     *    from GC.  Be careful, however, if you mark the same instance in
     *    more than one thread; it is possible for GC to deadlock if the
     *    instance holds a lock (it's kinda complicated).
     */

    // YOU hold fenceLock
static void clearFences(VM *self)	// !!NOT thread safe!!
{
   memset(&self->fence,0,sizeof(Fence));
//   self->fence.prev = &self->fence;
}

    /* Prepend the new fence
     * If fcn == 0, this is just a "marking" fence and i needs marking.
     * If fcn, fcn is "clean up" fcn.  If an exception is thrown,
     *   fcn(fence,exception) is called.
     * vm == 0/NoVM --> use the global marking fence.  The global fence is
     *     NOT hooked into the exception chain so it can only be used for
     *     marking.  Also, do not use it if an exception can cause
     *     vmRemoveFence() to be skipped (otherwise, your looking at a C seg
     *     fault).
     *   ALSO: don't use it to fence things that will be modified as GC will
     *     be marking fenced items.
//!!!
     * !!WARNING!! be careful about changing a fence after you set it. Eg if
     *   you change a list that is in the fence, GC will be looking at it at
     *   the same time you are changing it.

     * Fences are marked at safe points so you don't have to worry about
     *   changing the fence or what the fence points to.
     */
void vmSetFence(pVM vm, Fence *fence, 
		void (*fcn)(Fence *,Instance *), Instance *i)
{
   fence->i   = i;	 
   fence->fcn = fcn;
   fence->vm  = vm;
   fence->mrc = 0;
   fence->i1  = fence->i2 = fence->i3 = 0;

   if (vm)
   {
      Fence *vmFence = &pVM2VM(vm)->fence;
      fence->mrc  = ((VM *)vm)->mrc;
//if(pVM2VM(vm)->fenceLock)printf("FENCE LOCKED!\n"); // yep, ?markAllFences
      SPIN_LOCK_ACQUIRE(&pVM2VM(vm)->fenceLock);	// protect against GC
	 fence->next = vmFence->next;	// prepend
	 fence->prev = vmFence;
	 if (vmFence->next) vmFence->next->prev = fence;
	 vmFence->next = fence;
      SPIN_LOCK_RELEASE(&pVM2VM(vm)->fenceLock);
   }
   else		// vm == 0 --> use global fence
   {
      SPIN_LOCK_ACQUIRE(&globalFenceLock);
	 fence->next =  globalFence.next;
	 fence->prev = &globalFence;
	 if (globalFence.next) globalFence.next->prev = fence;
	 globalFence.next = fence;
      SPIN_LOCK_RELEASE(&globalFenceLock);
   }
}

void vmRemoveFence(Fence *fence, int runit)
{
   VM	    *vm;
   SpinLock *lock;

   if (!fence) return;

   vm   = pVM2VM(fence->vm);
   lock = vm ? &vm->fenceLock : &globalFenceLock;

   if (runit && fence->fcn) fence->fcn(fence,0);
   SPIN_LOCK_ACQUIRE(lock);
      fence->prev->next = fence->next;	// prev is always !0, next might be 0
      if (fence->next) fence->next->prev = fence->prev;
      // else last fence and next == 0 so prev->next now == 0
   SPIN_LOCK_RELEASE(lock);
}

#if 0
static void fenceCheck(VM *vm)
{
   Fence *ptr;
   int    n, mrc=vm->mrc;
   for(; vm; vm=vm->parentVM)
      for (ptr = vm->fence.next, n = 0; ptr; ptr = ptr->next, n++)
      {
	 if(ptr->vm!=(pVM)vm)
	    printf("Wacked fence %d VM#%d\n",n,vm->id);  // debugger target
	 if(ptr->mrc > mrc)
	    printf("Wacked fence MRC %d VM#%d\n",n,vm->id);  // debugger target
      }
}
#endif

static void walkTheFences(VM *vm,int clearThemFences, Instance *e)
{
   SPIN_LOCK_ACQUIRE(&vm->fenceLock);
   {
      Fence *ptr = vm->fence.next;
if (!ptr) { SPIN_LOCK_RELEASE(&vm->fenceLock); return; }
      if (clearThemFences)
      {
	 int n = vm->mrc;
	 if (n==0)
	 {
	    for ( ; ptr; ptr = ptr->next)
	       if (ptr->fcn) ptr->fcn(ptr,e);
	    clearFences(vm);		// nuke list
	 }
	 else
	 {
	    Fence *next;
	    for ( ; ptr; ptr = next)
	    {
	       next = ptr->next;
	       if (ptr->mrc >= n)
	       {
		  if (ptr->fcn) ptr->fcn(ptr,e);
		  ptr->prev->next = next;
		  if (next) next->prev = ptr->prev;
	       }
	    }
	 }
      }
      else	// clearThemFences == 0
      {
	 int n = vm->mrc;
	 for ( ; ptr; ptr = ptr->next)
	 {
	    if (ptr->fcn && ptr->mrc >= n)
	    {
	       ptr->fcn(ptr,e);
	       ptr->fcn = 0;
	    }
	 }
      }
   }
   SPIN_LOCK_RELEASE(&vm->fenceLock);
}

	// An uncommon fence
//static void release_vmLock(Fence *fence) { SPIN_LOCK_RELEASE(&_vmLock); }

    /* You will probably not be surprised to know that if a C function, with
     * a fence, returns while I'm marking that fence, is a bad thing.
     * Ditto a longjmp, so clear those fences before jumping!
     */
static void markFences(Fence *fence, SpinLock *lock)
{
   Fence *ptr;

   if (lock) SPIN_LOCK_ACQUIRE(lock);
      for (ptr = fence; ptr; ptr = ptr->next)
      {
	 if (ptr->i)  instanceMark(ptr->i);
	 if (ptr->i1) instanceMark(ptr->i1);
	 if (ptr->i2) instanceMark(ptr->i2);
	 if (ptr->i3) instanceMark(ptr->i3);
      }
   if (lock) SPIN_LOCK_RELEASE(lock);
}

#if 0
static size_t _markFences(VM *vm,void *_)
{
   markFences(&vm->fence,&vm->fenceLock);
   return 0;
}
static void markAllFences(void)	// caller holds _vmLock
{
   walkVMs(_markFences,0);
   markFences(&globalFence,&globalFenceLock);
}
#endif

///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////

unsigned int vmID(pVM self) { return ((VM *)self)->id; }

ZKL_PC *vmPC(pVM self)   { return &pVM2VM(self)->pc; }

int voidPlusOK(pVM self)
{
   Byte nextOp = *pVM2VM(self)->pc.addr; 
   return (opAddToArgs == nextOp);
//   return (opAddToArgs == nextOp || opDone == nextOp);
}

BlockStack *vmBlockStack(pVM self) { return pVM2VM(self)->blockStack; }
    // ONLY call this from a method or properties! Otherwise, GC may get upset
ZKL_Block *vmBlockCreate(pVM self)
   { return blockCreate(&pVM2VM(self)->blockStack,self); }

//CAtomicBool *vmInterruptEvent(pVM self)
int *vmInterruptEvent(pVM self)
{
   if (self) return &INTERRUPT_EVENT(self);
   return 0;
}

    // called after thread has initialized itself, pior to liftoff()
void vmIsAThread(pVM self, void *thread) 
   { CAP_SET(&pVM2VM(self)->isThread,thread); }

int vmPendingException(pVM self)
   { return (0 != pVM2VM(self)->pendingException); }

__inline int resultToBool(Instance *result, pVM vm)
{
   if (result == BoolTrue || IS_ONE(result)) return 1;
   if (result == Void || result == BoolFalse ||
       result == emptyString || result == emptyList ||
       IS_ZERO(result)) return 0;
   #if USE_POINTER_INTS
      if (IS_PtrInt(result)) return (result != PTR_INT_ZERO);
   #endif
   return BOOLV(M2_BOOL(result,vm));
}

static Instance *exitFcn;

    // fcn __exitFcn(f) { f() }
    // fcn sandbox(f) { try { f() } catch(0,*) {} }  exitFcns.apply2(sandbox)
static void constructExitFcn(void)
{
   static StringTable exitNames =
   {
      1,		// n
      "__exitFcn\0",	// Strings: name, vaultPath
      10,		// size
   };

   static Byte bits[4] = { opMarkArglist, opArg0, opTailCall, opDone };

   static ZKL_Code exitCode =
   {
      bits,
      "",		// no strings
      sizeof(bits),0,	// codeSize, stringSize
      1,		// empty map: (0)
      0,		// no kstrings
      (Byte *)"",	// map is one byte of zero
   };
   exitFcn = fcnEmbryoWithStaticCode(&exitNames,0,0,&exitCode,0,I_IMMORTAL,NoVM);
   exitFcn->iflag = 1;		// make runnable
   FCN_IS_STATIC(exitFcn) = 1;	// just cuz it is
}

void runExitFcn(ZKL_Block *block,int r,Instance *e,pVM vm)
{
   Instance *ef = block->registers[r], *f, *xxception = pVM2VM(vm)->xxception;
   int       n;
   MLIST(mlist,1);
   Instance *args;

   block->exitReg = 0;
	// if walking up the exception stack, can cross VMs, eg error in apply
   vm = vmLeaf(vm);

   pVM2VM(vm)->xxception = e;
   if (TYPEO(ef) == TupleType)
      for (n = 0; (f = listGet(ef,n)); n++)	// must run every exitFcn
      {
	 if (TYPEO(f) == FcnType) runFcnInSandBox(f,NoArglist,vm); // 1 VM
	 else	// A something: Eval with __exitFcn(f())
	 {     // eg onExit(foo.println) --> Method
	    args = mlistBuild(mlist,f,ZNIL);
	    runFcnInSandBox(exitFcn,args,vm);	   // 1 VM: Partial_call()
	 }
      }
   pVM2VM(vm)->xxception = xxception;
   block->registers[r]   = emptyTuple;	// reset for onExitBlock
}

Instance *deferredMaybeEval(Instance *_self,unsigned id,pVM vm);
int eyeballCallN(Instance **ip,unsigned id,pMethod *_method,void *pfcn,int bitch,pVM vm);

    /* The passed in VM has to be ready to go, especially the block stack.
     * One thing to be really careful of is that the block stack can move,
     *   which means having a pointer into the middle of it needs to be
     *   really careful.
     * While this doesn't recurse, it does get called repeatedly with the
     *   same VM as exceptions are caught.
     */
static Instance *_run(VM *vm, jmp_buf *env, int regXGood)
{
   #define CHECK_COUNT	1300
   #define RESULT	result
   #define REG_X	regX
   #define SAVE_REG	vm->result = result; vm->regX = regX;
   #define RESTORE_REG	result = vm->result; regX = Void;

   Instance  *result = Void, *regX = Void;

   ZKL_Block *block = currentBlock(vm->blockStack);
   ZKL_PC    *pc    = &vm->pc;
   Byte	     *addr  = pc->addr, opcode;

   char	     *text;
   Instance  *tmp = 0;	// VC14
   int	      n,z,xchecker;
   pMethod    method;
   ZKL_Fcn    fcn;

//vm->mightStall = 0;  //who the hell resets this?
   if (regXGood) regX = vm->regX;

   vm->self = (Class *)block->klass;
   vm->env  = env;

   setRunning(vm);

   if (vm->isStrand) { RESTORE_REG; } // Strand being resumed, sync registers

      // common: loop methods reuse a VM for short snippets
   if (ROOT_VM(vm)->marking == VM_MARK_YOURSELF) { SAVE_REG; _mark(vm); }

   xchecker = vm->icount;
   while (1)
   {
      if ((++xchecker & 0x1FF) == 0) // 512/0x200, 1k/400, 1536/600, 2k/800 ...
      {
	 if (ROOT_VM(vm)->marking == VM_MARK_YOURSELF) { SAVE_REG; _mark(vm); }
	 if (vm->isStrand)
	 {
//!!!??? what if strand is fcn running via method? eg .apply/.reduce
	    if (xchecker == 0xf00 && !vm->mrc)
	    {
	       SAVE_REG; PC_SYNC(pc,addr); vm->icount = 0;
	       VM_yield(0,NoArglist,(pVM)vm);	// long jmp
	    }
	 }
	 xchecker = 0;
	 opcode = INTERRUPT_EVENT(vm) ? opInterrupt : ADDR_NEXT(addr);
      }
      else opcode = ADDR_NEXT(addr);	// code[addr++]

      switch(opcode)
      {
	 case opMVR:  // opDone with benefits
	 case opDone:
	    /* Back out until I see a block with a valid pc in it.
	     * This is so I properly back out of CreateBlock's if the code
	     * does a return in the middle of a such a block
	     */

	    if ((n = block->exitReg)) runExitFcn(block,n-1,0,(pVM)vm);
	    while ((block = blockPop(vm->blockStack)))
	    {
	       if (block->returnTo || block->stop) break;
	       if ((n = block->exitReg))  // block is still on top of stack
		  runExitFcn(block,n-1,0,(pVM)vm);
	    }

	        // all done or stop block?, exit this VM
	    if (!block || block->stop == HARD_STOP)
	    {
	       if (opcode == opMVR)	// multi value return (phat done)
	       {
		  argWindow(vm->arglist2,&n,&z,(pVM)vm);
			// not a list assignment, convert MVR to Tuple
		  RESULT = listToTuple(vm->arglist2,n,z,(pVM)vm);
	       }
	       SAVE_REG; PC_SYNC(pc,addr); vm->icount = xchecker;
	       return RESULT;		// marked until this VM is freed
	    }

	    	// restore pointers to caller
	    vm->self = (Class *)block->klass;
	    vm->pc   = block->returnAddr;
	    ADDR_SYNC(pc,addr);

	    if (opcode == opMVR)	// multi value return (phat done)
	    {
	       int z;
	       argWindow(vm->arglist2,&n,&z,(pVM)vm);	// caller
//	       if (ADDR_PEEKB(addr) == opSquirt) // MVR/squirt(n)
if (addr && ADDR_PEEKB(addr) == opSquirt && !block->stop) // MVR/squirt(n)
	       {
		  (void)ADDR_GETB(addr);	// suck up squirt
		  z = ADDR_GETB(addr);
		  squirt(vm,vm->arglist2,n,z);
	       }
	       else	// not a list assignment, convert MVR to Tuple
		  RESULT = listToTuple(vm->arglist2,n,z,(pVM)vm);
	       // now, clean up arglist2
	    }

	    listTruncateTo(vm->arglist2,block->lenArglist,(pVM)vm);
	    vm->argOffset = block->argOffset;
	    vm->numArgs   = block->numArgs;

	    if (block->stop)   // soft stop
	       { SAVE_REG; PC_SYNC(pc,addr); vm->icount = xchecker; return RESULT; }

		// don't use this as a return block, unless refurbished
	    block->returnTo = 0;

	    break;

	 case opNoop: break;
#if 0
	 case opBreak1:
	    break;
	 case opBreak2:
	    break;
	 case opBreak3:
	    break;
	 case opBreak:
	    break;
#endif

	 case opInterrupt:  // instruction generated in this loop
	    SAVE_REG;
	    vmProcessInterrupt((pVM)vm);  // throws by calling barf()
	    break;			  // never gets here

	 case opSelf: RESULT = VM_SELF(vm);	        break;
	 case opVM:   RESULT = _vm2Instance((pVM)vm,0); break;

	 case opThrow:	// can exit/longjmp
	    SAVE_REG; PC_SYNC(pc,addr);	// for stop blocks
	    vm->icount = xchecker;
	    barf(vm,RESULT);	// result contains a class
	    break;		// doesn't get here
	 case opCatch22:
	    n			 = ADDR_GETB(addr);  // number of exceptions
	    block->numExceptions = n;
	    if (n)		// catch22(0) == catch block, not try block
	    {
	       block->exceptionTableOffset = PC_OFFSET_AT(pc,addr);
	       block->lenArglistEx	   = (int)listLen(vm->arglist2,(pVM)vm);
	       block->argOffset		   = vm->argOffset;
	       block->numArgs		   = vm->numArgs;
	       block->stackSizeEx	   = vm->stki;
	       ADDR_INC(addr,n*4);	// skip over exception table
	    }
	    break;

	 case opTrue:  RESULT = BoolTrue;  break;
	 case opFalse: RESULT = BoolFalse; break;
	 case opVoid:  RESULT = Void;	   break;
	 case opString:		// stringTable(2 byte index)
	    RESULT = 
		kStringCreate((char *)ADDR_GET_STRING(pc,addr,n),0,I_OWNED,(pVM)vm);
	    break;
	 case opGetKString:
	    // kstringTable[1 byte index], compact version of opString
	    RESULT = (Instance *)
	        &((ZKL_KCString *)pc->code->kstrings)[ADDR_GETB(addr)];
	    break;
	 case opInt:
	    RESULT = stringToInt((char *)ADDR_GET_STRING(pc,addr,n),(pVM)vm);
	    break;
	 case opIntB: RESULT = INT_CREATE(ADDR_GETB(addr),(pVM)vm);   break;
	 case opIntW: RESULT = INT_CREATE(ADDR_GETW(addr,n),(pVM)vm); break;
	 case opFloat:
	    RESULT = stringToFloat((char *)ADDR_GET_STRING(pc,addr,n),(pVM)vm);
	    break;
	 case opIEEE754:  // 8 bytes to double
	    RESULT = floatCreate(fromIEEE754(addr),(pVM)vm);
	    addr+=8;
	    break;
	 case opZero:   RESULT = Zero;		break;
	 case opOne:    RESULT = One;		break;
	 case opNoText: RESULT = emptyString;	break;
	 case opStar:   RESULT = Star;		break;

	 case opCreateBlock:	// Note: self doesn't change
	    PC_SYNC(pc,addr);
	    block = blockCreate(&vm->blockStack,(pVM)vm);
	    block->returnAddr = vm->pc;		// save for exception handling
	    block->klass      = (Instance *)vm->self;	// ditto
	    break;
	 case opPopBlock:
	    block    = blockPop(vm->blockStack);
	    vm->self = (Class *)block->klass;
	    break;
	 case opAllocRegisters:
 	    n = ADDR_GETB(addr);
	    blockAllocRegisters(&vm->blockStack,n,(pVM)vm);  // stack might move
	    block = currentBlock(vm->blockStack);
	    break;

//!!!! you know, these can stall if result is a class
//!!!???? iWantInt, not convertToInt??? 1 + "2"?
	 case opAdd:
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(RESULT)) RESULT = 
		   intCreate(PtrInt_TO_N(RESULT) + convertToInt(REG_X,(pVM)vm),(pVM)vm);
	       else
	    #endif
	    {
	       PC_SYNC(pc,addr); // going out to object land, can throw
	       RESULT = OBJECT1(RESULT)->add(RESULT,REG_X,(pVM)vm);
	    }
	    break;
	 case opSub:
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(RESULT)) RESULT = 
		   intCreate(PtrInt_TO_N(RESULT) - convertToInt(REG_X,(pVM)vm),(pVM)vm);
	       else
	    #endif
	    {
	       PC_SYNC(pc,addr); // going out to object land, can throw
	       RESULT = OBJECT1(RESULT)->sub(RESULT,REG_X,(pVM)vm);
	    }
	    break;
	 case opMul:
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(RESULT)) RESULT = 
		   intCreate(PtrInt_TO_N(RESULT) * convertToInt(REG_X,(pVM)vm),(pVM)vm);
	       else
	    #endif
	    {
	       PC_SYNC(pc,addr); // going out to object land, can throw
	       RESULT = OBJECT1(RESULT)->mul(RESULT,REG_X,(pVM)vm);
	    }
	    break;
	 case opDiv:
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(RESULT)) RESULT = PtrInt_div(RESULT,REG_X,(pVM)vm);
	       else
	    #endif
	    {
	       PC_SYNC(pc,addr); // going out to object land, can throw
	       RESULT = OBJECT1(RESULT)->div(RESULT,REG_X,(pVM)vm);
	    }
	    break;
	 case opMod:
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(RESULT)) RESULT = PtrInt_mod(RESULT,REG_X,(pVM)vm);
	       else
	    #endif
	    {
	       PC_SYNC(pc,addr); // going out to object land, can throw
	       RESULT = OBJECT1(RESULT)->mod(RESULT,REG_X,(pVM)vm);
	    }
	    break;
	 case opNegate:
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(RESULT)) 
		  RESULT = intCreate(-PtrInt_TO_N(RESULT),(pVM)vm);
	       else
	    #endif
	    RESULT = OBJECT1(RESULT)->negate(RESULT,Void,(pVM)vm);
	    break;
	 case opNot:
	    RESULT = resultToBool(RESULT,(pVM)vm) ? BoolFalse : BoolTrue;
	    break;

	 case opGetX: RESULT = REG_X;  break;
	 case opSetX: REG_X  = RESULT; break;
	 case opSwap:
	    tmp = RESULT; RESULT = REG_X; REG_X = tmp;
	    break;
	 case opPush:
	    if (vm->stki == FIXED_STACK)
	       vmThrow((pVM)vm,E_VM_ERROR,"Stack overflow");
	    vm->stack[vm->stki++] = RESULT;
instanceIsOrphan(RESULT);
	    break;
	 case opSetXPop:
	    REG_X = RESULT;
	    // Fall though
	 case opPop:
	    if (vm->stki == 0) vmThrow((pVM)vm,E_VM_ERROR,"Stack underflow");
	    RESULT = vm->stack[--vm->stki];
	    break;
	 case opPop1: // get item above ToS, non destructive
	    if (vm->stki == 0) vmThrow((pVM)vm,E_VM_ERROR,"Stack underflow");
	    RESULT = vm->stack[vm->stki-2];
	    break;
      #if 0
	 case 106: //opPopNth: // popNth(n), 0==ToS,  non destructive
 	    n = ADDR_GETB(addr);
	    if (vm->stki <= n) vmThrow((pVM)vm,E_VM_ERROR,"Stack underflow");
	    RESULT = vm->stack[vm->stki-n-1];
	    break;
      #endif
	 case opSquirt: 
	    PC_SYNC(pc,addr); // in case obj doesn't implement []
	    squirt(vm,RESULT,0,ADDR_GETB(addr)); 
	    break;
      #if 0
	 case 106: //opStkRvs: // reverse(n)
 	    n = ADDR_GETB(addr);
	    if(n>1)
	    {
	       if (vm->stki < n) vmThrow((pVM)vm,E_VM_ERROR,"Stack underflow");
	       for (z=vm->stki, n+=z; z < n; z++,n--)
	       {
		  tmp	       = vm->stack[z];
		  vm->stack[z] = vm->stack[n];
		  vm->stack[n] = tmp;
	       }
	    }
	    break;
      #endif
//!!!! you know, these can stall if result is a class
	 case opEQ:
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(RESULT)) RESULT = PtrInt_eq(RESULT,REG_X,(pVM)vm);
	       else
	    #endif
	    {
	       PC_SYNC(pc,addr); // going out to object land, can throw
	       RESULT = OBJECT1(RESULT)->eq( RESULT,REG_X,(pVM)vm);
	    }
	    break;
	 case opNEQ:
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(RESULT)) RESULT = PtrInt_neq(RESULT,REG_X,(pVM)vm);
	       else
	    #endif
	    {
	       PC_SYNC(pc,addr); // going out to object land, can throw
	       RESULT = OBJECT1(RESULT)->neq(RESULT,REG_X,(pVM)vm);
	    }
	    break;
	 case opLT:
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(RESULT))
	       {
		  n = PtrInt_TO_N(RESULT) < convertToInt(REG_X,(pVM)vm);
	       	  RESULT = n ? BoolTrue : BoolFalse;
		}
	       else
	    #endif
	    {
	       PC_SYNC(pc,addr); // going out to object land, can throw
	       RESULT = OBJECT1(RESULT)->lt(RESULT,REG_X,(pVM)vm);
	    }
	    break;
	 case opLTE:
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(RESULT))
	       {
		  n = PtrInt_TO_N(RESULT) <= convertToInt(REG_X,(pVM)vm);
	       	  RESULT = n ? BoolTrue : BoolFalse;
		}
	       else
	    #endif
	    {
	       PC_SYNC(pc,addr); // going out to object land, can throw
	       RESULT = OBJECT1(RESULT)->lte(RESULT,REG_X,(pVM)vm);
	    }
	    break;
	 case opGT:
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(RESULT))
	       {
		  n = PtrInt_TO_N(RESULT) > convertToInt(REG_X,(pVM)vm);
	       	  RESULT = n ? BoolTrue : BoolFalse;
		}
	       else
	    #endif
	    {
	       PC_SYNC(pc,addr); // going out to object land, can throw
	       RESULT = OBJECT1(RESULT)->gt(RESULT,REG_X,(pVM)vm);
	    }
	    break;
	 case opGTE:
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(RESULT))
	       {
		  n = PtrInt_TO_N(RESULT) >= convertToInt(REG_X,(pVM)vm);
	       	  RESULT = n ? BoolTrue : BoolFalse;
		}
	       else
	    #endif
	    {
	       PC_SYNC(pc,addr); // going out to object land, can throw
	       RESULT = OBJECT1(RESULT)->gte(RESULT,REG_X,(pVM)vm);
	    }
	    break;

#if 0
	 case opResolve:	// Find name in R: result.foo
	    text = (char *)ADDR_GET_STRING(pc,addr,n);
	    PC_SYNC(pc,addr);	// name might not exist, sync for stack trace
	    SAVE_REG;		// might be a VM property (eg vm.regX)
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(RESULT)) objectResolve(&RESULT,text,0,0,1,(pVM)vm);
	       else
	    #endif
	    OBJECT1(RESULT)->resolve(&result,text,0,0,1,(pVM)vm);
	    break;
#endif
	 case opResolveNB:
	    n = ADDR_GETB(addr);
	    goto resolveN;
	 case opResolveNW:
	    ADDR_GETW(addr,n);
	 resolveN:
	    PC_SYNC(pc,addr);	// name might not exist, sync for stack trace
	    SAVE_REG;		// might be a VM property (eg vm.regX)
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(RESULT)) objectResolveN(&RESULT,n,0,0,1,(pVM)vm);
	       else
	    #endif
	    OBJECT1(RESULT)->resolveN(&result,n,0,0,1,(pVM)vm);
	    break;

#if 0
	 case opReCallZ:	// ReCall with no args
	    z = 1;
	    goto reCall;	// basically fall through
	 case opReCall:	// resolve, then call result
	    z = 0;
	 reCall:
	    text   = (char *)ADDR_GET_STRING(pc,addr,n);
	    method = 0;
	    PC_SYNC(pc,addr);  // name might not exist, sync for stack trace
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(RESULT))
	       {
		  // Properites need the RESULT: (5).isOdd()
		  ZKL_Int i64;
		  PtrInt_TO_Int64(RESULT,&i64);
		  tmp = (Instance *)&i64;
#if 0
		  objectResolve(&tmp,text,&method,0,1,(pVM)vm);
		  /* tmp is a method (eg (5).toList()) or a Property value.
		   * If method, result needs to remain the same (it is
		   * self); if Property, result needs to change.
		   */
		  if (!method) RESULT = tmp;
#else
		  if(objectResolve(&tmp,text,&method,0,1,(pVM)vm)
		     /* tmp is a method (eg (5).toList()) or a Property value.
		      * If method, result needs to remain the same (it is
		      * self); if Property, result needs to change.
		      */
		    ==PropertyType) RESULT = tmp;
#endif
	       }
	       else
	    #endif
	    {	// because of else (above in USE_POINTER_INTS)
	       tmp = RESULT;
//instanceVerify(tmp,0,vm);
	       if (TYPEO1(tmp) == ClassType)	// class.fcn()?
		    RESULT = classRecall(tmp,text,&method,(Instance *)&fcn,(pVM)vm);
//	       else RESULT = IRESOLVE(tmp)(tmp,text,&method,1,(pVM)vm);
	       else
	          { IRESOLVE(tmp)(&tmp,text,&method,0,1,(pVM)vm); RESULT=tmp; }
	    }
	    goto callResult;	// basically fall through
#endif

	 case opCallResultZ:	// callResult with no args
	    method = 0; z = 1;
	    goto callResult;	// basically fall through
	 case opCallResult:	// Call the fcn that is in result: result()
	    method = 0; z = 0;
	 callResult:
	    SAVE_REG; PC_SYNC(pc,addr); vm->icount = xchecker;
	    vmCall(vm,RESULT,method,0,z);	     // can stall, GC or throw
	    block = currentBlock(vm->blockStack);    // stack may have moved
	    RESTORE_REG;  // result of call is in vm->result
	    ADDR_SYNC(pc,addr); xchecker = vm->icount;
//instanceVerify(vm->self,0,vm);
	    break;

	 case opTailRecurse:		// tail recursion
	    addr = tailRecurse(vm);
	    break;
	 case opTailCallZ:	// tailCall with no args
	    z = 1;
	    goto tailCall; // basically fallthrough
	 case opTailCall:		// like callResult. calls vmCall()
	    z = 0;
	 tailCall:
	    SAVE_REG; PC_SYNC(pc,addr);
	    tailCall(z,vm);	// reuse stack frame
	    block = currentBlock(vm->blockStack);	// blocks were popped
	    RESTORE_REG; ADDR_SYNC(pc,addr);
	    break;
	 case opChexit:		// chexit(blkCnt) IntW(reg)
	    tmp = (Instance *)blockUp(vm->blockStack,ADDR_GETB(addr));
	    	// get reg from next instruction
	    (void)ADDR_GETB(addr); (void)ADDR_GETW(addr,n);
	    if (n < 0x100) ((ZKL_Block *)tmp)->exitReg = n + 1;
	    else	// run exit fcn(s) now
	    {
	       runExitFcn(((ZKL_Block *)tmp),n & 0xff,0,(pVM)vm);
	       block = currentBlock(vm->blockStack);	// stack may have moved
	    }
	    break;
#if 0
	 case opMagic:
	    break;
#endif
	 case opCallIMethodNZ:
	    z = 1;
	    goto callIMethodN;		// basically fall through
	 case opCallIMethodN:		// RESULT.methods[n]()
	    z = 0;
	 callIMethodN:	// said method can call a fcn
	    n	   = ADDR_GETB(addr);
	    method = METHOD_TABLE(RESULT)[n].method;
	    goto callResult;
	    break;
	 case opCallOMethodNZ:
	    z = 1;
	    goto callOMethodN;		// basically fall through
	 case opCallOMethodN:		// RESULT.Object.methods[n]()
	    z = 0;
	 callOMethodN:
	    n	   = ADDR_GETB(addr);
	    method = objectMethods[n].method;
	    goto callResult;
	    break;
	 case opIPropertyN:
	    n      = ADDR_GETB(addr);
	    RESULT = PROPERTY_TABLE(RESULT)[n].property(RESULT,(pVM)vm);
	    break;

	 case opCallNBZ:	// call(id), id is unsigned byte
	    z = 1;
	    goto callNB;
	 case opCallNB: 	// call(id), id is unsigned byte
	    z = 0;
	 callNB:
	    n = ADDR_GETB(addr);
	    goto callN;
	 case opCallNWZ:	// call(id), id is 2 unsigned bytes
	    z = 1;
	    goto callNW;
	 case opCallNW:		// call(id), id is 2 unsigned bytes
	    z = 0;
	 callNW:
	    ADDR_GETW(addr,n);
	 callN:
	    SAVE_REG;		// might be a VM property (eg vm.regX)
	    method = 0;
	    eyeballCallN(&result,n,&method,(void *)&fcn,1,(pVM)vm);
	    goto callResult; // result & z is set, callResult knows PtrInts
	    break;

	 case opJmpFwd:  ADDR_JMP_FORWARD(addr); break;
	 case opJmpBack: ADDR_JMP_BACK(addr);	 break;
	 case opJmpTrue:
	    n = resultToBool(RESULT,(pVM)vm);
	    if (n) ADDR_JMP_FORWARD(addr);
	    else   ADDR_INC2(addr);
	    break;
	 case opJmpFalse:
	    n = resultToBool(RESULT,(pVM)vm);
	    if (!n) ADDR_JMP_FORWARD(addr);
	    else    ADDR_INC2(addr);
	    break;
	 case opHopFwd:  ADDR_HOP_FORWARD(addr); break;
	 case opHopBack: ADDR_HOP_BACK(addr);	 break;
	 case opHopTrue:	// very minor tweek to opJmpTrue
	    n = resultToBool(RESULT,(pVM)vm);
	    if (n) ADDR_HOP_FORWARD(addr);
	    else   ADDR_INC1(addr);
	    break;
	 case opHopFalse:	// very minor tweek to opJmpFalse
	    n = resultToBool(RESULT,(pVM)vm);
	    if (!n) ADDR_HOP_FORWARD(addr);
	    else    ADDR_INC1(addr);
	    break;

#if 0
	 case opPrepLoop:  // addr: push Result (which is Int), jmp if <= 0
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(RESULT)) z = ((n = PtrInt_TO_N(RESULT)) <= 0);
	       else
	    #endif
	    {	// ZKL_Int, eg One
	       n = (int)((ZKL_Int *)result)->value;
		  //!!! or > 2^31
	       z = (n <= 0) || (((ZKL_Int *)RESULT)->value <= 0);
	    }
	    if (z) RESULT = BoolFalse;	// no loop
	    else
	    {
	       if (vm->lstki == FIXED_STACK)
		  vmThrow((pVM)vm,E_VM_ERROR,"Loop stack overflow");
	       vm->lstack[vm->lstki++] = n;
	       RESULT = BoolTrue;
	    }
	    // top: opJmpFalse done;
	    break;
	 case opLoop:	// addr
	    n = vm->lstki - 1;
	    z = vm->lstack[n] - 1;
	    if (z == 0) { vm->lstki--; RESULT = BoolFalse; }
	    else
	    {
	       vm->lstack[n] = (unsigned)z;
	       RESULT = BoolTrue;
	    }
	    // opJmpBack top;
	    break;
#endif

#if 0
	 case opStompI:		// result.name = regX
	    text = (char *)ADDR_GET_STRING(pc,addr,n);
//!!! might want to redirect this though Object
	    if (TYPEO(RESULT) != ClassType)
	    {
	       char buf[100];
	       sprintf(buf,"setVar(%s): \"%s\" is NOT a class!",
			text,iname(RESULT));
	       vmThrow((pVM)vm,E_TYPE_ERROR,buf);
	    }
	    if (!classFindVar(RESULT,text,REG_X,(pVM)vm))	// thread safe
	    {
	       char buf[100];
	       sprintf(buf,"setVar: This class(%s) doesn't contain var \"%.40s\"",
		       className(RESULT),text);
	       vmThrow((pVM)vm,E_NAME_ERROR,buf);
	    }
	    // class marked, vm not, can miss RX
	    // vm marked, class not, can miss R
	    instanceIsOrphan2(RESULT,REG_X);
	    break;
#endif
	 case opStompIId:		// result.name = regX
	    ADDR_GETW(addr,n);
//!!! might want to redirect this though Object
	    if (TYPEO(RESULT) != ClassType)
	    {
	       char buf[100];
	       text = getGlobalName(n,(pVM)vm);
	       sprintf(buf,"setVar(%s): \"%s\" is NOT a class!",
			text,iname(RESULT));
	       vmThrow((pVM)vm,E_TYPE_ERROR,buf);
	    }
	    if (!classFindVarById(RESULT,n,REG_X,(pVM)vm))	// thread safe
	    {
	       char buf[100];
	       text = getGlobalName(n,(pVM)vm);
	       sprintf(buf,"setVar: This class(%s) doesn't contain var \"%.40s\"",
		       className(RESULT),text);
	       vmThrow((pVM)vm,E_NAME_ERROR,buf);
	    }
	    // class marked, vm not, can miss RX
	    // vm marked, class not, can miss R
	    instanceIsOrphan2(RESULT,REG_X);
	    break;

	 case opArg0: RESULT = vmGetArg(vm,0);		     break;
	 case opArg1: RESULT = vmGetArg(vm,1);		     break;
	 case opArg2: RESULT = vmGetArg(vm,2);		     break;
	 case opArgN: RESULT = vmGetArg(vm,ADDR_GETB(addr)); break;
	 case opSetArgN:   // This is thread safe even though arglist2 isn't
	    // arglist2 is thread local and fcns see chunks of it
	    n = ADDR_GETB(addr);
	    _checkArglist(vm,n,"setArgN");
	    listReplace(vm->arglist2,vm->argOffset + n,RESULT,(pVM)vm);
instanceIsOrphan(RESULT);	// see opStompI
	    break;
	 case opAddToArgs:
	    if (RESULT != VoidPlus)
	    {
//instanceVerify(vm->self,0,vm);
	       listAppend(vm->arglist2,RESULT,(pVM)vm);
	       instanceIsOrphan(RESULT);	// I said no, testing said yes
	    }
	    break;
	 case opMarkArglist:	// Mark start of next arg window
	    listAppend(vm->arglist2,0,(pVM)vm);
	    break;

	 case opGetReg0:  RESULT = block->registers[0];		       break;
	 case opGetReg1:  RESULT = block->registers[1];		       break;
	 case opGetReg2:  RESULT = block->registers[2];		       break;
	 case opGetReg3:  RESULT = block->registers[3];		       break;
	 case opGetReg4:  RESULT = block->registers[4];		       break;
	 case opGetReg5:  RESULT = block->registers[5];		       break;
	 case opGetReg:   RESULT = block->registers[ADDR_GETB(addr)];  break;
	 case opGetRegI0: RESULT = ((ZKL_Block *)tmp)->registers[0];   break;
	 case opGetRegI1: RESULT = ((ZKL_Block *)tmp)->registers[1];   break;
	 case opGetRegI: // blockUp(n); getRegI(n);
	    RESULT = ((ZKL_Block *)tmp)->registers[ADDR_GETB(addr)];
	    break;

	 case opGetVar:  RESULT = VARS(VM_CLASS(vm))[ADDR_GETB(addr)]; break;
	 case opGetVarI: RESULT = VARS(RESULT)[ADDR_GETB(addr)];       break;

	 #define SET_REG(n) block->registers[n]=RESULT; instanceIsOrphan(RESULT)
	 case opSetReg0: SET_REG(0);		   break;
	 case opSetReg1: SET_REG(1);		   break;
	 case opSetReg2: SET_REG(2);		   break;
	 case opSetReg3: SET_REG(3);		   break;
	 case opSetReg4: SET_REG(4);		   break;
	 case opSetReg5: SET_REG(5);		   break;
	 case opSetReg:  SET_REG(ADDR_GETB(addr)); break;
	 #undef SET_REG
	 case opSetRegI: // blockUp(n); setRegI(n);
	    ((ZKL_Block *)tmp)->registers[ADDR_GETB(addr)] = REG_X;
	    instanceIsOrphan(REG_X);
	    break;

	 case opIncReg:		// R = ++reg[n]
	    RESULT = block->registers[n = ADDR_GETB(addr)];
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(RESULT)) 
		  RESULT = intCreate(PtrInt_TO_N(RESULT) + 1,(pVM)vm);
	       else  // might not be a number
	    #endif
	    {
	       PC_SYNC(pc,addr); // going out to object land, can throw
	       RESULT = OBJECT1(RESULT)->add(RESULT,One,(pVM)vm);
	    }
	    block->registers[n] = RESULT;
	    break;
	 case opDecReg:		// R = --reg[n]
	    RESULT = block->registers[n = ADDR_GETB(addr)];
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(RESULT)) 
		  RESULT = intCreate(PtrInt_TO_N(RESULT) - 1,(pVM)vm);
	       else  // might not be a number
	    #endif
	    {
	       PC_SYNC(pc,addr); // going out to object land, can throw
	       RESULT = OBJECT1(RESULT)->sub(RESULT,One,(pVM)vm);
	    }
	    block->registers[n] = RESULT;
	    break;
	 case opRegPlus:	// R = (reg[n] += R)
	    REG_X = block->registers[n = ADDR_GETB(addr)];
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(REG_X)) RESULT = 
		   intCreate(PtrInt_TO_N(REG_X) + convertToInt(RESULT,(pVM)vm),(pVM)vm);
	       else  // might not be a number
	    #endif
	    RESULT = OBJECT1(REG_X)->add(REG_X,RESULT,(pVM)vm);
	    block->registers[n] = RESULT;
	    break;

	 case opSetVar: VARS(VM_CLASS(vm))[ADDR_GETB(addr)] = RESULT;
	    instanceIsOrphan(RESULT);	// tests show this makes a difference
	    break;
	 case opSetVarI: VARS(RESULT)[ADDR_GETB(addr)] = REG_X;
	    instanceIsOrphan(REG_X);
	    break;

	 case opParentN:
	    RESULT = PARENTS(VM_CLASS(vm))[ADDR_GETB(addr)];
	    break;
	 case opParentNI:
	    RESULT = PARENTS(RESULT)[ADDR_GETB(addr)];
	    break;
	 case opClassN:		// get a child class
	    RESULT = CLASSES(VM_CLASS(vm))[ADDR_GETB(addr)];
	    break;
	 case opClassNI:
	    RESULT = CLASSES(RESULT)[ADDR_GETB(addr)];
	    break;
	 case opFcnN:	// get nth fcn out of self, ALL classes share fcns
	      // Note that in C & C(), one set of Fcns has wrong container ptr
	    tmp = (Instance *)&VM_CLASS(vm)->fcns[ADDR_GETB(addr)];
	    n   = ADDR_PEEKB(addr);
	    if (n==opCallResult || n==opCallResultZ ||  // fcnN(n)/callRresult
		n==opTailCall   || n==opTailCallZ)
	    {	//!!! I HOPE a pgm can't actually save this
	       fcn = *(ZKL_Fcn *)tmp;	// long lived temp, not marked, cls is
	       FCN_CONTAINER(&fcn) = (Instance *)VM_CLASS(vm);
	       RESULT = (Instance *)&fcn;
	       xchecker--;	// don't let Strand yield with tmp in play
	    }
	    else RESULT = fcnCopy(tmp,(Instance *)VM_CLASS(vm),(pVM)vm);
	    break;
	 case opFcnNI:		// get nth fcn out of class stored in R
	    tmp = (Instance *)&((Class *)RESULT)->fcns[ADDR_GETB(addr)];
	    n   = ADDR_PEEKB(addr);  // C.f(): FcnNI(n)/CallRresult?
	    if (n==opCallResult || n==opCallResultZ ||
		n==opTailCall   || n==opTailCallZ)
	    {
	       fcn = *(ZKL_Fcn *)tmp;	// struct copy
	       FCN_CONTAINER(&fcn) = RESULT;
	       RESULT = (Instance *)&fcn;
	       xchecker--;	// don't let Strand yield with tmp in play
	    }
	    else RESULT = fcnCopy(tmp,RESULT,(pVM)vm);
	    break;

	 case opClassUp:
	    RESULT = classUp((Instance *)VM_CLASS(vm),ADDR_GETB(addr));
	    break;

	 case opBlockUp:
	    tmp = (Instance *)blockUp(vm->blockStack,ADDR_GETB(addr));
	    xchecker--;	// don't yield when tmp is in play, next op will use tmp
	    break;
	 case opBlockUp1:
	    tmp = (Instance *)blockUp(vm->blockStack,1);
	    xchecker--;	// don't yield when tmp is in play, next op will use tmp
	    break;
	 case opBlockUp2:
	    tmp = (Instance *)blockUp(vm->blockStack,2);
	    xchecker--;	// don't yield when tmp is in play, next op will use tmp
	    break;
	 case opTheVault:
	    text   = (char *)ADDR_GET_STRING(pc,addr,n);
	    RESULT = vaultFind(text,1,(pVM)vm);
	    break;
	 case opVCache:
	    RESULT = vcGet(ADDR_GETB(addr),(pVM)vm);
	    break;
	 case opSrcLineW:		// source line # 2 bytes
	    ADDR_GETW(addr,n);
	    block->lineNum = n;
	    break;
	 case opSrcLineB:		// source line # 1 byte
	    block->lineNum = ADDR_GETB(addr);
	    break;
      #if 0
	 case opTable:	// 2 bytes len, bytes --> no-op
	    ADDR_GETW(addr,n); // len
	    ADDR_INC(addr,n);	   // skip over bytes
	    break;
      #endif
	 case opData:	// opData 2 bytes len --> KData pointing into code
	    ADDR_GETW(addr,n); // len
//!!!deadlock if GC
//!!!???container  == self??
	    RESULT = kdataCreate(addr,n,0,I_OWNED,(pVM)vm);
	    ADDR_INC(addr,n);	   // skip over bytes
	    break;
	 default:
	 {
	    char buf[100];
	    sprintf(buf,"%d: Unknown op code",(int)opcode);
	    vmThrow((pVM)vm,E_VM_ERROR,buf);
	 }
      }	// switch
   }	// while
   // never gets here

   #undef CHECK_COUNT
   #undef RESULT
   #undef REG_X
   #undef SAVE_REG
   #undef RESTORE_REG
}

    /* Returns: 0 (not found, use resolve/recall), or
     *    Fcntype|MethodType|PropertyType|UnknownType
     *    PropertyType means a id was a property, ie any type.
     *    UnknownType means just that but not one of the above.
     * Input: 
     *    ip: Instance to resolve against
     *    id: Index into global name table
     *    pfcn: 0 | &ZKL_Fcn
     *    method should be initialized to zero
     * Sets (if return !0):
     *    ip     : result of resolve
     *    _method: method if id is MethodType
     *    pfcn   : Filled in if return FcnType
     */
int eyeballCallN(
Instance **ip,unsigned id,pMethod *_method,void *pfcn,int bitch,pVM vm)
{
   Instance   *i = *ip;
   int         t = TYPEO(i);

   if(t==DeferredType)
   {
      // getGlobalName range checks n
      i = *ip = deferredMaybeEval(i,id,vm);
   }
   return OBJECTI(i)->resolveN(ip,id,_method,pfcn,bitch,vm);
}


    /* Call a fcn, class or method
     * For class or fcn, setup a new calling block and makes things ready to
     *   start running, don't acutally do the call; just set the PC and let
     *   the VM step into the fcn.
     * If calling a method, call it and set VM->result.
     * If actually calling a method (such as ask), this can stall.
     * Dealing with safe points makes this a pain in the ass:
     *   C code (methods): Can stall
     *   zkl code: Can't get into C code unless it doubles back through the
     *     VM, thus the safe point is "will stall at your command". Except for
     *     processing default args.
     * MList for methods is a huge win (~30% speed up), BFD for fcns
     * Fence arglist so it doesn't need to be orphanized.
     * The result of a method call won't be an orphan, it is stored in vm
     */
static void vmCall(VM *vm,Instance *fcn,pMethod method,int tailCall, int Z)
{
   #define MLIST_SZ	40

   MLIST(mlist,MLIST_SZ);	// if I can, stash arglist on C stack
   Fence     fence;
   Instance *result, *arglist;

   vmSetFence((pVM)vm,&fence,0,fcn);

   if (ROOT_VM(vm)->marking == VM_MARK_YOURSELF) _mark(vm);

   if (method)	// in this case, fcn is a object
   {
      if (Z) arglist = NoArglist;
//!!!could use a slice of the arglist here, rather than copy
// don't think so
      else arglist = arglistPop(vm->arglist2,mlist,MLIST_SZ,&fence.i1,(pVM)vm);
      #if USE_POINTER_INTS
         if (IS_PtrInt(fcn)) 
	    result = ptrIntDoMethod(fcn,0,method,arglist,(pVM)vm);
	 else
      #endif
      {
	 result = method(fcn,arglist,(pVM)vm);	// can stall or throw
	 if (INTERRUPT_EVENT(vm))		// cheap[er] here
	     vmProcessInterrupt((pVM)vm);
      }
   }
   else		// fcn or class? or method?
   {
      ZKL_Fcn init;		// for class(), very temporary
      if (TYPEO(fcn) == ClassType)	// create new class and call init
      {
	   // create new class instance (maybe) (a copy), get init, can GC
	 fcn = classPrepForRun(fcn,&fence.i2,&init,(pVM)vm);
	    // fcn is now FcnType
	    // Now run the constructor or init fcn in the [new] instance
	    // fall though
      }
      if (TYPEO(fcn) == FcnType)
      {
	 ZKL_Block *block;
	 int        offset, numArgs;

	 if (Z) { offset = (int)LIST_LEN(vm->arglist2); numArgs = 0; }
	 else argWindow(vm->arglist2,&offset,&numArgs,(pVM)vm);

	 if (FCN_NUM_DEFAULTS(fcn))  // finalizeArglist2 can stall, GC or throw
	    numArgs = finalizeArglist2(fcn,vm->arglist2,numArgs,(pVM)vm);

	 block = currentBlock(vm->blockStack);
	 if (!tailCall)		// return here after call
	 {
	    block->argOffset  = vm->argOffset;
	    block->numArgs    = vm->numArgs;
	    block->klass      = (Instance *)vm->self;
	    block->returnAddr = vm->pc;		// save the return addr
	    block->returnTo   = 1;
	    if (Z) block->lenArglist = offset;
	    else   block->lenArglist = offset ? offset-1 : 0; // ignore marker
	 	// create a new block for fcn, block stack can move
	    block = blockCreate(&vm->blockStack,(pVM)vm);
		// no fcn in stackTrace until call fcn
	 }
	 // else reuse current block for tail call

	     // shove fcn code into block, after this, don't need fcn anymore
	     // block->klass marks fcnBase, which holds the actual code
//!!!??? static fcn with NullClass for container?
	 fcnPrepForRun(fcn,block,(pVM)vm);  // block now contains class+code
	 vm->self = (Class *)block->klass;  // new class is fcn.container

	 vm->numArgs   = block->numArgs   = numArgs;
	 vm->argOffset = block->argOffset = offset;

	 result = Void;
      }
      else	// method or 1.create
      {
	 if (Z) arglist = NoArglist;
	 else   arglist = arglistPop(vm->arglist2,mlist,MLIST_SZ,&fence.i1,(pVM)vm);
	 	// Methods with Ints don't hold PtrInts (see above)
	 if (TYPEO(fcn) == MethodType)
	    result = Method_call(fcn,arglist,(pVM)vm);
	 else	   // create an Object: File() --> File.create(), etc
	 {
	    #if USE_POINTER_INTS
	       if (IS_PtrInt(fcn)) result = Int_create(fcn,arglist,(pVM)vm);
	       else
	    #endif
	    result = I_METHOD(fcn,M_CREATE)(fcn,arglist,(pVM)vm);
	 }
      }
   }

   vm->result = result;
   vmRemoveFence(&fence,0);
}

   // opTailCall : a tail call version of opCallResult
   //              fcn to be called is in vm->result
   // Call target but don't return here just to do a opDone; 
   //   just jmp to target and let it return
   // This is pretty vile: unwind the stack so that vmCall can wind it back up
   // This is a wash for method calls
   // Don't do tail calls in try blocks; the block isn't returnable, which 
   //   would disable the try.
   /* Normally, there is a returnTo block (ie got here from a call). But
    * there is a "special" case if the tailCall backs up all the way to
    * vmRunFcn() (which isn't returnable) [eg from classRunConstructor]:
    *    class C {
    *       fcn toString { return(text()); }	// tailCall, jmp to fcn text
    *       fcn text     { return("BUG");  }
    *    }
    *    println(C());		// BOOM on Import
    */
static void tailCall(int Z,VM *vm)
{
   ZKL_Block  *block = currentBlock(vm->blockStack);
   int	       n,er, offset = vm->argOffset;
   unsigned    bo;

	// Find block just after the returnTo (called from) block.
   	// And clobber it (in vmCall(). OK, "reuse" it)
        // If fcn is a Method, this is the arglist block (opMarkArglist)
   for (n = 0; block; n++)
   {
      if (block->returnTo) break;
      bo = block->stackOffset;
      if ((er = block->exitReg)) 
	 runExitFcn(block,er-1,0,(pVM)vm);  // can cause block stack to move
      block = blockAtOffset(vm->blockStack,bo);
   }
	// n USUALLY > 0, often 1, 4 not so often
   while(--n) blockPop(vm->blockStack);

   if (block)	// reset arglist, this is the returnTo block (ToS-1)
   {
      n = block->lenArglist + 1;
      if (n != offset) listDelete(vm->arglist2,n,offset - n,(pVM)vm);
   }
   // else assume arglist is correct  !!! probably wrong ?fiber?
//!!! the arglist can grow here for certain types of recusion
// although I may have fixed that indirectly

   vmCall(vm,vm->result,0,1,Z);	// This should be a tail call itself!
}

    // opTailRecurse: A recursive tail call
    // Turn "return(self.fcn())" into "goto self.fcn"
    // Unwind the stack to the start of the fcn and restart fcn
static Byte *tailRecurse(VM *vm)	// returns addr of new opCode
{
   Byte	*addr;
   int   offset, numArgs;

   BlockStack *blockStack = vm->blockStack;
   ZKL_Block  *block = currentBlock(blockStack);
   int	       n,er;

	// find block just after returnTo (called from) block. Restart there
   for (n = 0; block; n++)
   {
      if (block->returnTo) break;
      if ((er = block->exitReg)) runExitFcn(block,er-1,0,(pVM)vm);
      block = prevBlock(blockStack,block);
   }
   while(--n) blockPop(blockStack);  // don't nuke return block or entry block

     // Don't do default args
     // But do remove previous arg frame from stack. There can only be one.
     // Remember marker, ie nuke at least one item.
     /* Code (in fcn) is markArglist [push args] tailRecurse (no
      *   tailRecurseZ) so there is always a trailing marker.  If funRun()
      *   (eg L(f).run()) is running the fcn, there is no leading marker as
      *   block is the entry block. There might not be a leading marker.
      */
   if (!block)	// block == 0 --> fcnRun (new VM)
      listDelete(vm->arglist2,vm->argOffset,vm->numArgs+1,(pVM)vm);
   else listDelete(vm->arglist2,vm->argOffset - 1,vm->numArgs + 1,(pVM)vm);
//   listDelete(vm->arglist2,vm->argOffset,vm->numArgs + 1,(pVM)vm);
     // recalc the arg frame in case the number of args changed
   argWindow(vm->arglist2,&offset,&numArgs,(pVM)vm);
   vm->numArgs = numArgs;

   addr = vm->pc.code->code;	// fcn entry point

     // don't re-allocate already allocated registers
   if (*addr == opAllocRegisters) ADDR_INC2(addr);
   return addr;
}

unsigned sGetId, textId, catchableId;	// set in object.c

    // Push items from object onto the stack for list assignment
    // in FILO order (ie n items, stack.pop() --> i[n-1])
struct SFence { Fence fence; int idx; };
static void _spurt(Fence *fence, Instance *e) // restore stack if squirt() blows
{
   VM *vm = pVM2VM(fence->vm);
   vm->stki = ((struct SFence *)fence)->idx;
}
static void squirt(VM *self,Instance *R, int offset, int n)
{
   Instance *f, **stack = &self->stack[self->stki];
   int	     j, t = TYPEO(R);
   pMethod   get = 0;
   struct SFence fence;

   if (FIXED_STACK <= self->stki + n)
      vmThrow((pVM)self,E_VM_ERROR,"Stack overflow");

   if (t == ListType || t == TupleType)  // the expected case
   {
      listSplat(R,offset,n,stack,(pVM)self);
      self->stki += n;
      return;
   }

   #if USE_POINTER_INTS
      if (IS_PtrInt(R)) R = Zero;  // fuck 'em, not implemented
   #endif
//if(t==WalkerType) get = Walker_next;
//else
//!!! fail over __sGet, get, read
   f = R;
   OBJECT1(R)->resolveN(&f,sGetId,&get,0,1,(pVM)self);
   vmSetFence((pVM)self,(Fence *)&fence,_spurt,0);
      if (!get) fence.fence.i1 = f;	// fcn may have been allocated
      fence.idx = self->stki;
      for(j = 0; j < n; j++)
      {
	 MLIST(mlist,1);
	 Instance *i;
	 fence.fence.i2 = mlistBuild(mlist,INT_CREATE(j,(pVM)self),ZNIL);

	 if (get) i = get(f,(Instance *)mlist,(pVM)self);
	 else     i = vmCallFcnFromMethod(f,(Instance *)mlist,0,(pVM)self);
	 stack[j] = i; 
	 self->stki++;		// 2 reasons: gc marking, so f can use stack
      }
   vmRemoveFence((Fence *)&fence,0);
}

static __inline void _checkArglist(VM *self,int n,char *opName)
{
char *fcnProtoName(Instance *fcn,unsigned n);	// fcn.c

   int len = self->numArgs;
   if (n >= len)
   {
      Instance *fcn = classFcnMatchup(VM_SELF(self),self->pc.code,0);
      char      buf[100], *ptr=fcnProtoName(fcn,n);
      int	z = 0;

      if (len) len--;
//      if (fcn) z = sprintf(buf,"%s.%s:",className(VM_SELF(self)),fcnName(fcn));
      if (fcn)
      {
	 if(ptr && *ptr)
	 {
	    sprintf(buf,"%s.%s: arg \"%s\" (#%d) not there",
	       className(VM_SELF(self)),fcnName(fcn),ptr,n+1);
            vmThrow((pVM)self,E_MISSING_ARG,buf);
	 }
      	 z = sprintf(buf,"%s.%s:",className(VM_SELF(self)),fcnName(fcn));
      }
      sprintf(buf+z,"opcode %s: %d is out of range 0 .. %d",opName,n,len);
      vmThrow((pVM)self,E_MISSING_ARG,buf);
   }
}

static Instance *vmGetArg(VM *self, int n)
{
   _checkArglist(self,n,"argN");
   return LIST_TABLE(self->arglist2)[self->argOffset + n];
}

Instance *vmMaybeGetArg(unsigned n,pVM pvm)
{
   VM *vm = (VM *)pvm;
   if (n >= vm->numArgs) return 0;
   return LIST_TABLE(vm->arglist2)[vm->argOffset + n];
}

/* ******************************************************************** */
/* ************************ Exceptions, Death ************************* */
/* ******************************************************************** */

void vmHalt(char *msg)
{
      // Don't call malloc
   if (msg) { fputs(msg,stderr); fputs("\n",stderr); }
   CAI_ONE(&_vmLock);		// freeze VMs: don't be nice about this
   #if defined(__unix__)
   {
      char *ptr = getenv("zklDumpCore");
      if (ptr && *ptr == '1') abort();	// ulimit -c unlimited
   }
   #endif
   exit(3);
}

void vmPanic(pVM self, char *msg)
{
   if (!self || !isRunning(pVM2VM(self))) vmHalt(msg);

   fprintf(stderr,"%s\n",msg);
   stackTrace(self,1,0);
   walkTheFences((VM *)self,1,0);
   longjmp(*pVM2VM(self)->env,VM_ERROR);
}

    /* Only called from _run() or a method that detects an interrupt
     * VM_throw won't throw at me at this time (which is what caused this to
     * be called). self->interruptEvent is 1.
     * Single threaded
     */
void vmProcessInterrupt(pVM pSelf)
{
   char	     buf[100];
   Instance *exception;
   VM	    *self = pVM2VM(pSelf);
//   VM	    *self = (VM *)vmLeaf(pSelf);

   if (!isRunning(self))
   {
      sprintf(buf,"VM#%d isn't running and thus cannot be interrupted.",
         self->id);
      vmThrow((pVM)self,E_VM_ERROR,buf);
   }
   exception = self->pendingException;
   self->pendingException = 0;
   self->interruptEvent	  = 0;		// OK, VM_throw is good to go
   if (exception) barf(self,exception);
   sprintf(buf,
      "VM#%d has no pending exception, and thus cannot be interrupted.",
      self->id);
   vmThrow(pSelf,E_VM_ERROR,buf);
}

static void _verifyException(VM *self,Instance **peClass,
	char **name, char **text, int *catchable)
{
   Instance *eClass = *peClass;
//   Instance **eFcn;

   if (classIsChildOf(eClass,ExceptionClass))
   {
      Instance *string, *bool;

//!!!??? it might be a good idea to stash globalID in exception: vm.findGlobalName
      *name  = className(eClass);	// I know this always works
      string = classFindVarById(eClass,textId,0,(pVM)self);
      bool   = classFindVarById(eClass,catchableId,0,(pVM)self);

      if (!string || !bool || TYPEO(string) != StringType || 
	  TYPEO(bool) != BoolType)
      {
	 char buf[100];
	 sprintf(buf,"Thrown exception \"%.70s\" is corrupt.",*name);
         vmPanic((pVM)self,buf);
      }
      *text	 = ZKL_STRING(string);
      *catchable = BOOLV(bool);

#if 0
      	/* Exception.function: Set it so a catch has a bit more knowledge
	 * about what happened. However, throwing a static exception (eg
	 * throw(Exception.TheEnd) or vmThrowTheEnd()) is not so good: GC
	 * will keep fcn around for probably way too long and exception
	 * thrown from method might not reflect reality.
	 * ??? only set on initial throw? Then chained throws know the origin
	 * ??? save Traceback?
	 */
      eFcn = classFindVarSlot(eClass,"function_",1,NoVM);
      if (eFcn && (*eFcn == BoolTrue) && self->pc.code)
	 *eFcn = self->pc.code->fcn;	// eClass.function_ = self.fcn
#endif
      return;
   }

   	// sorta fake up an error. I *think* this is thread safe
   eClass     = vaultChase("Exception.BadException",0); // halts on error
   *peClass   = eClass;
   *name      = className(eClass);
   *catchable = 1;
   *text      = "Can only throw a child of Exception";
}

    /* Perform the mechanics of throwing an exception at a VM.
     * If a catch block is found:
     *   - The exception is stashed in reg0 so __exception can get it
     * If a catch block is not found in this VM tree, half the VM and let
     *   the halt propagate up the VM tree (via longjmps).
     *   - Stash the exception in regX so thread can give it to splashdown.
     * If I have to walk up though VMs to find the catch block, VMs are
     *   freed. If the thown exception happens to be in one those freed VMs,
     *   it can be reclaimed. I have to protect it. GC will not happen
     *   during this function but marking might.
     * It is OK if eClass is not protected.
     * Won't GC in here but might back in _vmRun().
     */
static void barf(VM *self, Instance *eClass)
{
   char *ename, *text;
   int	 catchable;
   VM	*startingVM, *vm;

if (mightStall(self))  // however, could be throwing "already stalled"
vmPanic((pVM)self,"mightStall oops");

	// blockstack can change, self will change

   setBarfing(self);

//!!! I should delay looking up text
   _verifyException(self,&eClass,&ename,&text,&catchable);

   if (!isRunning(self) && !self->parentVM)		// dead root?
   {
      char	buf[100];
      sprintf(buf,"Can only throw when running! (VM#%d : %.70s)",
		  self->id,ename);
      vmPanic((pVM)self,buf);
   }

   startingVM = (VM *)vmLeaf((pVM)self);	// possible, eg list.filter
   while (1)	// walk up the block chain looking for a catch block
   {
      ZKL_Block *block;
      int	 stop = 0;

      block = findCatchBlock(catchable ? eClass : 0, (pVM)self);
      if (block)	// found a catcher
      {
	 if (block->stop == HARD_STOP) // stop block
	 {
	    fprintf(stderr,"VM#%d is ignoring this unhandled exception:\n",self->id);
	    fprintf(stderr,"   %s : %s\n",ename,text);
// ASSUMES a fcn was run from runFcnInSandBox() & fcnRun() did vmCreate()
if (self->childVM)
	    self = self->childVM;
	    stop = 1;
	    block = 	// get block before stop block
	       currentBlock(self->blockStack);
	    if (block->stop)
	       vmHalt("stacked stop blocks: FIX THIS");
	 }
	 else
	    block->registers[0] = eClass;	// __exception in catch block

	    // reset pending arglist, stack size
	 self->self      = (Class *)block->klass;
	 listTruncateTo(self->arglist2,block->lenArglistEx,NoVM);
	 self->argOffset = block->argOffset;
	 self->numArgs   = block->numArgs;
	 self->stki	 = block->stackSizeEx;
	 notBarfing(self);	//!!??? count?

	 	// clean things up if more than one VM is involved
	 while (startingVM != self)
	 {
//!!! is eClass protected in this case?
	    vm = startingVM->parentVM;
	    walkTheFences(startingVM,1,eClass);
	    vmFree((pVM)startingVM);		// will mark VM stack
	    startingVM = vm;
	 }
	     /* I suspect it is possible to clean up too much here (ie
	      * somebody has a fence around the try/catch) but I can't
	      * figure how to create that scenario.
	      */
	 walkTheFences(self,1,eClass);		// clears markers

	 if (stop) longjmp(*self->env,VM_YIELD);
	 longjmp(*self->env,VM_FOUND_CATCH);
      }

	// not found in this VM, check the calling VM
      self = self->parentVM;
      if (!self) break;		// no calling VM
   } // while

   /* No catcher found, this entire VM stack is kaput.  Back out to _vmRun()
    * and let it halt the calling VM.  I probably should just free up the
    * intervening VMs and longjmp to the topmost VM but I'll let _vmRun do
    * the grunt work for me.
    */

   self = startingVM;	// start backing out from the leaf

	// This VM can't handle the excpetion, kick it upstairs
//   if name not in ("BreakPoint","SingleStep"):
   {
      fprintf(stderr,"VM#%d caught this unhandled exception:\n",self->id);
      fprintf(stderr,"   %s : %s\n",ename,text);
   }
   self->regX = eClass;		// save exception for splashdown
#if 0
   walkTheFences(self,1,eClass);
#else
   // clear all fences for the entire stack before longjmp nukes C stack
   for(vm = startingVM; vm; vm = vm->parentVM) walkTheFences(vm,1,eClass);
#endif
   if (self->env) longjmp(*self->env,VM_HALT);

   // self is a top VM. Eg thown out of vmRunFcn()/fcnPrepForRun()
   vmHalt("barf(): I don't know what to do in this case");
}

void vmThrowE(pVM self, Instance *eClass)
{
   if (!self)
   {
      Instance *text = classFindVarById(eClass,textId,0,NoVM);
      if (text && TYPEO(text) == StringType) vmPanic(self,stringText(text));
      vmPanic(self,"Boom");
   }

   if (isBarfing(pVM2VM(self)))	// !!!????
   {
      char buf[200];
      sprintf(buf,"VM%d is trying to throw %s from within a throw.\n",
              ((VM *)self)->id,className(eClass));
      vmPanic(self,buf);
   }

   barf((VM *)self,eClass);
}

void vmThrowTheEnd(pVM self) { vmThrowE(self,ExTheEnd); }

    /* eg throw(Exception.NameError("boom"));
     * Text is copied to a new string so when longjmp() resets the stack
     *   string is OK in cases like 
     *   char buf[]; sprintf(buf...); vmThrow(...,buf)
     * GC : Might be creating new exception, as orphans, they can
     * survive two GC cycles, time enough for them to get to barf()
     */
void vmThrow(pVM self, char *exceptionName, char *text)
{
   Instance *exception;
   Fence     fence;

   if (!self || self==(pVM)1)
   {
      char buf[150];
      sprintf(buf,"VM HALTing\n%s: %.100s",exceptionName,text ? text : "");
      vmHalt(buf);
   }
   if (isBarfing(pVM2VM(self)))
   {				// because of classRun below, eg out of mem
      char buf[200];
      sprintf(buf,
         "VM#%d is trying to throw from within a throw.\n"
	 "   Trying to throw %s : %.100s",
	 ((VM *)self)->id,exceptionName,text ? text : "");
      vmPanic(self,buf);
   }

	// get the default Exception class (eg IndexError)
   exception = classFindClass(ExceptionContainer,exceptionName,0,self);
   if (!exception) vmPanic(self,text);		// bogus name

   setBarfing(pVM2VM(self));	// in case classRun() throws

   if (text)	// create new Exception Instance
   {
      MLIST(mlist,1);
      Instance *pt;
      Instance *arglist;

	  /* OK, this is complicated:  If an object has locked itself and
	   * has a fence with an unlocker (eg List locked for writing, fence
	   * has unlocker) AND the object throws (eg index out of range) AND
	   * GC is happening, then dead lock can happen IF mightStall causes
	   * GCT to mark the object (eg classRun does vmCreate/vmFree).
	   * So, since we are going to be doing a longjmp no matter what,
	   * deal with the fences now, rather than in barf().
	   */
//      walkTheFences((VM *)self,0,0);	// but keep marking
walkTheFences((VM *)self,0,exception);	// but keep marking

      pt      = stringCreate(text,I_OWNED,NoVM);
      arglist = mlistBuild(mlist,pt,ZNIL);
      vmSetFence((pVM)self,&fence,0,pt);
         exception = classRun(exception,arglist,self);  // e(...)
	 fence.i1  = exception;
     // fence will be removed by barf(), exception will be stashed in a VM
   }
   barf((VM *)self,exception);	// barf sets registers[0] to exception
}

/* ******************************************************************** */
/* ********************** External Access to VM *********************** */
/* ******************************************************************** */

    /* Run a VM.
     * Returns:
     *    0:  Didn't successfully complete, status tells you why VM is freed.
     *    VM result: Ran to completion, status is VM_DONE
     * Result is still in VM so it is protected
     * This pretty much IS the entry point to a VM, you should NOT call
     *   _run().  Going through here is essential to keeping the C stack and
     *   the VM stack in sync as barf() assumes there is a 1:1 between a
     *   longjmp() and throw.  Otherwise, it is possible for throw to
     *   longjmp(VM_FOUND_CATCH) to a C call waaay above the catch block (it
     *   is really twisted). barf unwinds the VM stack, this unwinds the C
     *   stack as throw punts out of VM space (to C space) and this punts
     *   back into VM space.
     *   It is necessary to do it this way so a method (C code) can, for
     *   example, recurse, call vmThrow and the right things happen.
     * On error or halt:  The root VM isn't freed.  If VMs were spawned by
     *   this VM, they are cleaned up.  This essentially walks up the VM
     *   stack, clearing the fences, until the root VM is reached.  I do
     *   this with longjmp() so that the C stack is correct for marking
     *   fences.  I don't think that is necessary because fences in
     *   parentVMs are above me (in the C stack) so that memory is fine.
     *   Witness barf() not having any problems doing just that.
     */
static Instance *_vmRun(VM *self, int *status, int tidyUp)
{
   jmp_buf env;
   int	   n;

//if(self->mightStall)
//printf("HUH?\n");

   while (1) // loop if exception caught, longjmp() on HALT
   {
      n = setjmp(env);	// initializing env or returning from longjmp()
      if (status) *status = n;
      if (n == 0)	// longjmp() not called or fcn ran OK (VM_DONE == 0)
      {
	 jmp_buf *envSav = self->env;
	 _run(self,&env,0);	// self->result
	 notRunning(self);	// finished running
	 self->env = envSav;	// usually zero

	 // do some clean up in case this VM will be reused
	 if (tidyUp)
	 {
	    listClear(self->arglist2,(pVM)self);
	    self->argOffset = self->numArgs = 0;
	 }
	 return self->result;
      }
      else	// longjmp() was called, opDone doesn't longjmp()
      {
	 self->env = 0;		// CYA
	 switch (n)
	 {
	    case VM_FOUND_CATCH:
	       // Successfully found a catch block, now dive back into the VM
	       continue;
	    case VM_ERROR:		// Died a painful death
	    case VM_HALT:		// Return control to parent VM
	       // !!Mark self's fence before longjmp()! C stack is blotto here
	       // fences are typically on the C stack so can't access
	       stackTrace((pVM)self,1,0);
	       if (self->parentVM)	// don't free the "top" VM
	       {
		  VM *parentVM = self->parentVM;
printf("Halting VM#%d\n",parentVM->id);
		  parentVM->regX = self->regX;  // propagate exception, thread
if(self->fence.next) printf("ARGH! Fences were not cleared!\n");
		  vmFree((pVM)self);  // caller never gets a chance to free VM
		  if (parentVM->env)  // HALT parent VM
		     longjmp(*parentVM->env,VM_HALT);
		  // else, _run() never called for this VM, very bad
		  vmHalt("Zero jmp_buf");
	       }
if(self->fence.next) printf("ARGH2222! Fences were not cleared!\n");
	       return 0;    // return to the outside world
	    case VM_YIELD:
	       return self->result;
	    default:
	       vmPanic((pVM)self,"Should not get here");
	 } // switch
      }
   } // while
   return 0;		// doesn't get here
}

#if 0
    // Set up a fcn or class for a tail call from a Method.
    // Arglist in a MList is OK
    // Method stashing results --> Method --> tailcall bad
    //   eg T(1,"hoho").apply("walker")
    //   caller must look at result and wagTheTail if needbe
    // -->VoidTail
__inline Instance *
vmTailCall(Instance *fcnOrClass,pArglist arglist,pVM vm)
{
   // if the VM isn't running, no stack to set up for a tail call
   // so just run the fcnOrClass
   if (!isRunning(pVM2VM(vm)))	// or vm->env == 0
   {
      if (TYPEO1(fcnOrClass) == FcnType)
	 return vmRunFcn(vm,fcnOrClass,arglist,0);  // don't alloc another VM
      return objectRun(fcnOrClass,arglist,0,vm);  // eg Walker(). classRun
   }

   listAppend(pVM2VM(vm)->arglist2,0,vm);	// opMarkArglist
   if (arglist != emptyList && listLen(arglist,vm))
   {
      MLIST(mlist,1);
      List_extend(pVM2VM(vm)->arglist2,mlistBuild(mlist,arglist,ZNIL),vm);
   }
	// Note: not a tailcall, Method calls don't have fcn stackframe
   vmCall(pVM2VM(vm),fcnOrClass,0,0,0);
        // Code has been extracted and fcn is no longer needed
   	// arglist has now been pushed onto the stack and is no
	// longer needed
   return VoidTail;	// get method call off C stack
   // fcnOrClass will now run and return "real" result of method call
}

Instance *    // -->VoidTail
fcnVTailCall(char *vaultClassPath,char *fcnName,pArglist arglist,pVM vm)
{
   ZKL_Fcn   f;		// very temporary
   Instance *fcn = classFindFcn(vaultFind(vaultClassPath,1,vm),fcnName,0,&f,vm);
   if (!fcn)
   {
      char buf[200];
      sprintf(buf,"fcnVTailCall(): Can't find %.80s.%.80s",vaultClassPath,fcnName);
      vmThrow(vm,E_NOT_FOUND,buf);
   }
   return vmTailCall(fcn,arglist,vm);	// get method call off C stack
   // f not needed anymore
   // f will now run and return "real" result of method call
}
#endif

    /* This is only called from outside the VM
     * The passed in VM must be valid
     * The top block on the block stack must be valid.
     * parentVM has been set (most likely by vmCreate())
     * It is up to you to protect arglist
     * Result, etc is protected (in VM), until you vmFree()
     * If you call this with parentVM set to 0, throw can't bypass this VM.
     */
Instance *vmRun(pVM self, int *status)
{
   	// do some CYA
//   if (PARENT_VM(self) && CAB_IS_SET(&PARENT_VM(self)->barfing))
//      CAB_SET(&pVM2VM(self)->barfing);

   if (isRunning(pVM2VM(self)))
   {
      char buf[100];
      sprintf(buf,"VM#%d is trying to recurse!",((VM *)self)->id);
      stackTrace(self,1,0); vmHalt(buf);
   }

   return _vmRun((VM *)self,status,1);  // result marked until VM is freed
}

    // Don't cross thread(s)!
    // Run a function in a brand new VM (which you supply)
Instance *vmRunFcn(pVM pSelf, Instance *fcn, pArglist arglist, int *status)
{
   MLIST(mlist,1);	// so I can build L(arglist) & arglist2.extend(mlist)
   ZKL_Block *block;
   Instance  *result;
   int	      s,z=0;
   VM	     *self = pVM2VM(pSelf);

   switch(TYPEO(arglist))
   {
      default:
        vmThrow(pSelf,E_ASSERTION_ERROR,"vmRunFcn: arglist isn't a list!");
      case ListType:	// List, TSList & ROList
      case TupleType:	// and MList
	 break;
   }

   block = blockCreate(&self->blockStack,pSelf);
   fcnPrepForRun(fcn,block,pSelf);
   if (arglist != emptyList && listLen(arglist,(pVM)self))
   {
      List_extend(self->arglist2,mlistBuild(mlist,arglist,ZNIL),pSelf);
      z = (int)listLen(arglist,(pVM)self);
   }
      /* Minor strangeness: finalizeArglist2() allocates a VM (to run
       * default arg fcns in) and, if that fcn throws, _vmRun() will throw
       * at it's parent (ie self). If self is a thread, I need to catch that
       * throw or bye bye.
       */
   if (FCN_NUM_DEFAULTS(fcn))   // avoid work if no default args
   {
      jmp_buf env, *envSav = self->env;
      int     n = !PARENT_VM(self) ? setjmp(env) : 0;  // for threads only
      if (n == 0)		// longjmp() not called
      {
	 self->env = &env;	// in case thread default arg throws
	 z = finalizeArglist2(fcn,self->arglist2,z,pSelf);
	 self->env = envSav;
      }
      else		// oh joy, thread liftoff() default arg throwed
      {
	 if (status) *status = VM_HALT;	// threads want status
	 return Void;
      }
   }
   self->numArgs = z;
   result = vmRun(pSelf,&s);      	// vmRun()/_run() sets self->env
   if (status) *status = s;
   return result;	// marked until VM is freed
}

    // Run a function in a sandboxed VM so exceptions can't escape
    // Has to be a function so that a setjmp() is done, which forms the sandbox.
    // Attached to a VM to stack traces will include the entire chain
void runFcnInSandBox(Instance *f,pArglist arglist,pVM vm)
{
#if 1
   ZKL_Block *block;

   if (TYPEO(f) != FcnType) return;	// do I trust the compiler?
   block       = blockCreate(&pVM2VM(vm)->blockStack,vm);
   block->stop = HARD_STOP;
   fcnRun(f,arglist,0,vm);	// create sandbox VM, run f, free VM

//block=currentBlock(pVM2VM(vm)->blockStack);
//if (block->stop != HARD_STOP) printf("poooooooooooooooooh  %d\n",block->stop);
   blockPop(pVM2VM(vm)->blockStack);	// get rid of stop block

#else
   Fence fence;
   VM   *vv = (VM *)vm;
printf("BURP %p\n",vm);
   vmSetFence(vm,&fence,0,vv->result); fence.i1 = vv->regX;
// have to save R,X & stack so vm state looks identical
      vmCallFcnFromMethod(f,arglist,1,vm);
      vv->result = fence.i; vv->regX = fence.i1;
   vmRemoveFence(&fence,0);
#endif
}

static Instance *_doDaCall(unsigned cbo,VM *vm)
{
   ZKL_Block  *block;
   BlockStack *bstack;
   jmp_buf     env, *envSav;
   int	       n;
   Instance   *result=0;

//Fence fence;
//vmSetFence((pVM)vm,&fence,0,vm->result); fence.i1 = vm->regX;

   envSav = vm->env;
if (!envSav) //!!!***
{stackTrace((pVM)vm,0,0);vmHalt("This doesn't work\n");}
   vm->mrc++;
   while(1)	// loop on try/catch unless catch is above me
   {
      n = setjmp(env);
      if (n == 0)		// longjmp() not called
      {
	 int running = isRunning(vm);	// in case running in a fresh VM
	 result = _run(vm,&env,1);	// sets running
	 if(!running) notRunning(vm);
	 break;
      }
      else		// poop, exception
      {
//printf("-BOOM->VM#%d %d %d\n",vm->id,n,vm->mrc);
	 switch (n)	// mrc > 0
	 {
	    case VM_FOUND_CATCH: //5 can, and do, longjmp right back to here
	       bstack = vm->blockStack;	// might have moved
//printf("-BS->%d %d\n",bstack->cbo,cbo);
	       if (bstack->cbo <= cbo)	// catch block is above me
	       {
		  Instance *e = currentBlock(bstack)->registers[0];
		  vm->mrc--;
		  walkTheFences(vm,1,e);
		  vm->env = envSav;
		  longjmp(*envSav,VM_FOUND_CATCH);
	       }
	       continue;
	    case VM_YIELD:  // fiber has been detached
//!!!???? HARD_STOP?
vmHalt("BAD BAD BAD\n");  // can hit this on AsmError unknown opcode
	       break;
	    default:
	       vm->mrc--;
	       walkTheFences(vm,1,0);
	       vm->env = envSav;
	       longjmp(*envSav,n);
	 }
      }
   } // while
//vmRemoveFence(&fence,0);
   vm->mrc--;
   vm->env = envSav;

if (!result || n != VM_DONE)
vmHalt("NOT GOOD\n");

   bstack = vm->blockStack;	// might have moved

//block = currentBlock(bstack);
//if (!block->stop) printf("-->stop: %d %p\n",block->stop,block);

   block = blockPop(bstack);		// get rid of stop block

//if (bstack->cbo != cbo) 
//printf("CBO mismatch %d %d\n",bstack->cbo,cbo);

   return result;	// which is still in vm->result
}

    // Call a function from a running VM that is stalled in a 
    //   Method call [maybe] needing a result
    // Doesn't allocate a VM
    // Exceptions are a PITA
    // hardStop is usually 0
Instance *vmCallFcnFromMethod(Instance *f,pArglist arglist,int hardStop,pVM vm)
{
   VM	      *self = (VM *)vm;
   BlockStack *bstack = self->blockStack;
   unsigned int cbo = bstack->cbo;
   ZKL_Block  *block = blockCreate(&self->blockStack,vm);  // can move bstack
ZKL_Block  *b2;
   int	       stopper=SOFT_STOP;
   MLIST(mlist,1);	// so I can build L(arglist) & arglist2.extend(mlist)

   if (hardStop) stopper = HARD_STOP;

if (!isRunning(self) || self->childVM) printf("ACK!\n");	//!!!***

if (!f || TYPEO(f) != FcnType) vmHalt("vmCallFcnFromMethod(): NOT A FCN!\n");

//if (stack->cbo == 0)printf("Opps\n");

   if (self->mrc > MAX_C_STACK)  // 75 --> about 550-700 C stack frames
      vmThrow(vm,E_ASSERTION_ERROR,
		      "C stack too big (run away Method recursion?)");

   listAppend(self->arglist2,0,vm);	// opMarkArglist
   List_extend(self->arglist2,mlistBuild(mlist,arglist,ZNIL),vm);
   block->stop = stopper;
   vmCall(self,f,0,0,0);     // will mark if needed, blockStack can/does move
#if 1
b2 = currentBlock(self->blockStack);
if (b2->stop) printf("HMMMMMMM?????????  %d\n",b2->stop);
b2 = prevBlock(self->blockStack,b2);
//if (b2 != block) printf("UHHHHHHHHHHHHHHHH\n");
if (b2->stop != stopper) printf("HMMMMMMMMMMMMMMMMMMMMMMMMM  %d\n",b2->stop);
#endif

   return _doDaCall(cbo,self);
}

#if 0
Instance *vmContinue(Instance *f,pArglist arglist,pVM vm)
{
   VM	      *self = (VM *)vm;
   BlockStack *bstack = self->blockStack;
   unsigned int cbo = bstack->cbo;
   ZKL_Block  *block = blockCreate(&self->blockStack,vm);  // can move bstack
ZKL_Block  *b2;
   MLIST(mlist,1);	// so I can build L(arglist) & arglist2.extend(mlist)

if (TYPEO(f) != FcnType)printf("NOT A FCN!\n");

   listAppend(self->arglist2,0,vm);	// opMarkArglist
   List_extend(self->arglist2,mlistBuild(mlist,arglist,ZNIL),vm);
   vmCall(self,f,0,0,0);     // will mark if needed, blockStack can/does move

#if 1
b2 = currentBlock(self->blockStack);
if (b2->stop) printf("HMMMMMMM?????????  %d\n",b2->stop);
b2 = prevBlock(self->blockStack,b2);
#endif

   return Void;
}
#endif

Instance *vmCallCodeFromMethod(ZKL_Code *code,
			       Instance *klass,pArglist arglist,pVM vm)
{
   VM	      *self = (VM *)vm;
   BlockStack *bstack = self->blockStack;
   unsigned    cbo = bstack->cbo;
   ZKL_Block  *block = blockCreate(&self->blockStack,vm);  // can move stack
   ZKL_Fcn     f = *(ZKL_Fcn *)nullFcn;
   MLIST(mlist,1);	// so I can build L(arglist) & arglist2.extend(mlist)

if (self->childVM || !self->env) printf("ACK!\n");

if (bstack->cbo == 0)printf("Opps\n");
   f.container      = klass;
   f.instance.iflag = 1;

   listAppend(self->arglist2,0,vm);	// opMarkArglist
   List_extend(self->arglist2,mlistBuild(mlist,arglist,ZNIL),vm);
   block->stop = SOFT_STOP;
   vmCall(self,(Instance *)&f,0,0,0);
   pcInit(vm,code,0);
   return _doDaCall(cbo,self);
}

/* ******************************************************************** */
/* ***************************** Methods ****************************** */
/* ******************************************************************** */

    // Don't call this for a dead or dying VM!
static Instance *_vm2Instance(pVM pvm,int lock)
{
   VM   *vm = (VM *)pvm;
   VMWR *ref;

   if (vm->ref) return (Instance *)vm->ref;

   if (lock) spinLockAcquire2(&_vmLock,0,pvm);	// calls vmMightStall(1)
      ref = (VMWR *)ibucketAllocate(vmwrBuckets,&VMWRObject,I_OWNED,1,pvm);
      ref->pVM = pvm;
      CAP_SET(&vm->ref,ref);  // remove prossible race with the return above
   if (lock) SPIN_LOCK_RELEASE(&_vmLock);

   return containerIsCooked(vmwrBuckets,(Instance *)ref,I_OWNED);
}
Instance *vm2Instance(pVM vm) { return _vm2Instance(vm,1); }

static Instance *VM_toString(VMWR *ref,pArglist arglist, pVM vm)
{
   VM *self = (VM *)ref->pVM;
   if (self)
   {
      char buf[30];
      sprintf(buf,"VM#%d",self->id);
      return stringCreate(buf,I_OWNED,vm);
   }
   return kStringCreate("DeadVM",0,I_OWNED,vm);
}

    // .kick(targetVM): Throw HeyYou at another VM
    // .kick(targetVM,exception=HeyYou): Throw HeyYou child at another VM
    // --> True if kicked
static Instance *VM_kick(VMWR *_,pArglist arglist, VM *thrower)
{
   Instance *target    = arglistGet(arglist,0,0,(pVM)thrower);
   Instance *exception = arglistTryToGet(arglist,1);
   Instance *result;
   VM	    *throwee;
   char	     buf[100];
   Instance *HeyYou = classFindClass(ExceptionContainer,"HeyYou",0,(pVM)thrower);

   if (TYPEO(target) != VMType)
      vmThrow((pVM)thrower,E_TYPE_ERROR,"vm.kick: Target ain't a VM");

   if (!HeyYou) 
      vmThrow((pVM)thrower,E_VM_ERROR,"vm.kick: Can't find HeyYou!");

   if (!exception) exception = HeyYou;
   else if (!classIsChildOf(exception,HeyYou))
   {
      sprintf(buf,"vm.kick(%s): Not an instance of HeyYou!",iname(exception));
      vmThrow((pVM)thrower,E_TYPE_ERROR,buf);
   }

   spinLockAcquire2(&_vmLock,0,(pVM)thrower);	// freeze VM stacks
      throwee = (VM *)((VMWR *)target)->pVM;
      if (!throwee || !isRunning(throwee))
      {
	 SPIN_LOCK_RELEASE(&_vmLock);
	 vmThrow((pVM)thrower,E_NOT_FOUND,"vm.kick: Target not live");
	 // doesn't get here
      }

      if (throwee == thrower)	// throwing at myself: just don't do it
      {
	 SPIN_LOCK_RELEASE(&_vmLock);
	 sprintf(buf,"Use throw (not VM.kick) to throw at self");
	 vmThrow((pVM)thrower,E_VALUE_ERROR,buf);
	 // doesn't get here
      }

   	/* Throwing an exception at another thread
	 * Can't vmThrow here because we are running in the wrong thread.
	 *   So set it and let themm deal with it.  When throwing an
	 *   exception at another thread, the exception is queued in that
	 *   VM, which will act on it when it feels like it.  Which means
	 *   that this is possible:  VM1 throws e1 at VM2.  VM2 throws e2 at
	 *   itself.  E1 might be processed after e2.
	 * Need to freeze the VM stacks as they are churning as I speak and
	 *   I want to avoid race conditions.  This also serializes throws,
	 *   which avoids race conditions (mulitple threads throwing at the
	 *   same thread).
	 * VM->interruptEvent isn't atomic but I don't care because:
	 *   - Setting pending interrupt is serialized here.
	 *   - It is read only for almost everybody else.
	 *   - It is not time (cache) critical. It is reset while the VM is
	 *     dead.
	 *   - vmProcessInterrupt resets pending while interruptEvent is set.
	 *   
	 * Cases:
	 *   - Thread embryo: VM gets exception when it starts running.
	 *   - Dead root VM: Return False. Race condition.
	 *   - Not a leaf:  Redirect to leaf.  EG throw at VM that is doing
	 *     something like List.filter or Atomic.waitFor.  Otherwise,
	 *     exception could be ignored for a while or forever.
	 *     If leaf isn't running, throw at parent.
	 *     If stack is dying, no-op.
	 *   - VM already has a pending exception: no-op.
	 *   - VM is processing a self thrown exception: no-op.
	 *   - Stalled fibers are marked as running, exception will happen
	 *     on resume.
	 */
      throwee = (VM *)vmLeaf((pVM)throwee);  // don't throw at middle of stack
      if (!isRunning(throwee))
	 if (!(throwee = throwee->parentVM))	// dead root
	 {
	    SPIN_LOCK_RELEASE(&_vmLock);
	    return BoolFalse;
	 }

      result = BoolTrue;
//      if (CAB_IS_SET(INTERRUPT_EVENT(throwee)) ||
//	  CAB_IS_SET(&throwee->barfing)) result = BoolFalse;

      if (INTERRUPT_EVENT(throwee) || isBarfing(throwee)) result = BoolFalse;
      else
      {		// race conditions happen here!
	 CAP_SET(&throwee->pendingException,exception);
	 CAI_ONE(&INTERRUPT_EVENT(throwee));
	 // vmProcessInterrupt takes it from here
      }
   SPIN_LOCK_RELEASE(&_vmLock);
   // If this throwee is waiting, tell it it has been kicked
   asyncEventHappened((Instance *)throwee->rootVM,0x3,0); // throwee is leaf

   instanceIsOrphan(exception);
   return result;
}

    // -->0 (signaled), 1 (can't get lock, retry), 2 (pooh)
int signalTheMotherShip(void)
{
   int  n;
   VM  *vm;
   Instance *HeyYou = classFindClass(ExceptionContainer,"HeyYou",0,NoVM);

   n = 1000;
   while (--n) if (SPIN_LOCK_TRY_TO_ACQUIRE(&_vmLock)) break;
   if (!n) return 1;  // Probably me who has vmLock
   for (vm = vmLiveList; vm; vm = vm->nextVM)
   {
      if (vm->isThread || vm->isFiber) continue;

      vm = (VM *)vmLeaf((pVM)vm);  // don't throw at middle of stack
      if (!isRunning(vm))
	 if (!(vm = vm->parentVM)) continue;	// dead root

      if (INTERRUPT_EVENT(vm) || isBarfing(vm)) ;
      else
      {		// race conditions happen here!
	 CAP_SET(&vm->pendingException,HeyYou);
	 CAI_ONE(&INTERRUPT_EVENT(vm));

	 SPIN_LOCK_RELEASE(&_vmLock);

	 // If this throwee is waiting, tell it it has been kicked
	 asyncEventHappened((Instance *)vm->rootVM,0x3,0);

	 // vmProcessInterrupt takes it from here
	 return 0;
      }
   } //for
   SPIN_LOCK_RELEASE(&_vmLock);
   return 2;
}


    // .stackTrace() -->String
static Instance *VM_stackTrace(VMWR *ref,pArglist arglist, pVM vm)
{
   if (ref->pVM != vm)
      vmThrow(vm,E_ASSERTION_ERROR,"VM.stackTrace: VM isn't self");
   return stackTrace(vm,-1,1);
}

    /////////////////////////////// Fibers
    /* Be VERY careful about more than one thread talking to fibers, I can't
     *    really guard against two threads calling yield or resume at the
     *    same time.
     * References to fibers won't stop them from dying: 
     *    fiber = vm.createFiber(fcn { vm.yield(vm) }); fiber.resume()
     * fiber is a dead VM. Ditto on threads.
     */

    // VM.isFiber flags
//#define NOT_A_FIBER		0
#define FIBER_STALLED		1
#define FIBER_RUNNING		2

    // .createFiber(fcn, args)
static Instance *VM_createFiber(VMWR *ref,Instance *arglist,pVM callingVM)
{
   MLIST(mlist,11);
   int	     status;
   Instance *result, *args, *fcn;
   VM	    *fiber, *self = (VM *)callingVM;

   if (ref->pVM != callingVM)  // pretty sure it is OK, but don't want to test
      vmThrow(callingVM,E_ASSERTION_ERROR,"VM.createFiber: VM isn't self");
   if (listLen(arglist,callingVM) > 10)
      vmThrow(callingVM,E_ASSERTION_ERROR,
	"VM.createFiber: No more than 10 parameters");

   fcn   = arglistGetBT(arglist,0,FcnType,"vm.createFiber",callingVM);
   fiber = (VM *)vmCreate(callingVM,NoBlock,0x0,callingVM);   // A leaf VM
   args  = mlistCopy(mlist,arglist,1,11);

   fiber->isFiber = FIBER_RUNNING;
   result = vmRunFcn((pVM)fiber,fcn,args,&status);	// call _vmRun()
   if (status == VM_DONE)
   {
      instanceIsOrphan(result);	// see .resume
      self->isFiber = 0;	// no longer a fiber (in case vmFree gc's)
      vmFree((pVM)fiber);	// can GC
   }
   return result;	// returns here on first yield or if fcn returns
}

    /* fiber.yield(result,...)
     * NOT OK to tell another VM to yield
     *   (I don't want any depth to the C stack)
     * Only a leaf VM can yield.
     * Fiber is stalled and becomes a singleton.
     * DO NOT call this from C code! It blottos something but good.
     *   For example:
     *     L(1).apply(vm.yield)		// can't create Method(vm.yield)
     *     L(vm).apply("yield")		// can't detect
     *     L(1).apply(fcn{vm.yield(1)})	// fiber isn't leaf (fcn vm is)
     * ref is 0 if vm is swapping out a Strand (ie itself)
     */
//!!!??? if not a fiber, can I turn this into a return(result)?
Instance *VM_yield(Instance *ref,pArglist arglist,pVM callingVM)
{
   Instance *result;
   VM	    *self = (VM *)callingVM;

   if ( (ref && (((VMWR *)ref)->pVM != callingVM)) || 
       self->childVM || !self->parentVM)
      vmThrow(callingVM,E_ASSERTION_ERROR,"Can't yield here");
   if (self->isFiber != FIBER_RUNNING)
      vmThrow(callingVM,E_ASSERTION_ERROR,
	"VM is not a running fiber, can't yield.");

   if (self->mrc)
   {
      self->mrc--;
      vmThrow(callingVM,E_ASSERTION_ERROR,"Can't yield here "
          "(Generator('wrap or .fp)?");
   }

   if (ref)	// Strands don't get no steeking result, need to preserve R!
   {
      switch(listLen(arglist,callingVM))
      {
	 case 0:  result = Void;			      break;
	 case 1:  result = arglistGet(arglist,0,0,callingVM); break;
	 default: result = arglistDup(arglist,0,callingVM);   break;
      }
      self->result = result;	// but still part of fiber (ie no DIP)
   }
   walkTheFences(self,1,0);	// C stack is gonna disappear
   spinLockAcquire2(&_vmLock,0,(pVM)self);
      self->isFiber = FIBER_STALLED;
   	// move from leaf to root VM
      self->parentVM->childVM = 0;	// detach from stack
      prependRootVM(self);
   SPIN_LOCK_RELEASE(&_vmLock);

   longjmp(*self->env,VM_YIELD);   // stalling, no fences to clear

   return Void;		// shut up the compiler
}

    // .resume(), called from another VM stack
    // var f = vm.createFiber(fcn{vm}); f.resume()
    //    f points to a dead or recycled vm (which could be a fiber)
static Instance *VM_resume(VMWR *ref,pArglist arglist, pVM callingVM)
{
   Instance *result;
   int	     status;
   VM	    *self = (VM *)ref->pVM;

      // Checking 123 ... resuming a dead VM really messes up 
      // spinLockAcquire2()
   if (!self || self->isFiber != FIBER_STALLED)
   {
   diediedie: ;
      vmThrow(callingVM,E_ASSERTION_ERROR,
	      "VM is not stalled fiber, can't resume");
   }

	// Move fiber from a root VM to a leaf
        // Let GCT mark parent while I'm waiting on vmLock
   vmMightStall(callingVM,1);
   spinLockAcquire2(&_vmLock,0,(pVM)self);  // self will stall here during GC
      vmMightStall(callingVM,0);
      	// Checking 456 ... make this thread safe
      if (self->isFiber != FIBER_STALLED)   // T1: f.resume(); T2: f.resume()
      {
	 SPIN_LOCK_RELEASE(&_vmLock);
	 goto diediedie;
      }
      self->isFiber  = FIBER_RUNNING;	// real soon now
      unlinkRootVM(self);
      self->parentVM = (VM *)callingVM;
      self->rootVM   = ((VM *)callingVM)->rootVM;
      self->parentVM->childVM = self;
      self->recursionCount = 0;
      self->pid = 0;
   SPIN_LOCK_RELEASE(&_vmLock);

   result = _vmRun(self,&status,1);   // recurse to resume this continuation

	// returns here after yield or fiber is done
	// self has been detached from VM stack but is probably 
	// still owned (marked) by stack, ie result isn't orphan
   if (status == VM_DONE)
   {
      self->isFiber = 0;	// no longer a fiber (in case vmFree gc's)
      vmFree((pVM)self);	// fiber doesn't protect result, DIP2.2
      	// OK, now result is an orphan, even if the owning VM tries to mark
	// this dead fiber (as the VM has been cleared). If the root VM has
	// marking pending, then free will mark, otherwise, nope.
      instanceIsOrphan(result);
   }
   return result;
}

    /////////////////////////// Strands: pre-emptive co-op fibers/threadettes

#if 0
void strandNext(VM *vm)
{
   VM  *vnext;

   spinLockAcquire2(&_vmLock,0,(pVM)vm);
      vnext = vm->root;
      for(vnext = vm->nextVM; vnext != vm; vnext = vm->nextVM)
      {
	 if (!vnext) vnext = vmLiveList;
	 if (vnext->isStrand && 
	     vnext->isFiber = FIBER_STALLED) break;
      }
   SPIN_LOCK_RELEASE(&_vmLock);
   if (vnext == vm) return;	// all fibers are running
   VM_yield(0,vm->ref,(pVM)vm);	// --> yield(next strand to run)
need strand id so can do pool matching
}

#endif


    // VM.iIsStrand: mark fiber as a Strand
static Instance *VM_brandStrand(VMWR *ref,Instance *arglist,pVM vm)
{
   VM *fiber = (VM *)ref->pVM;

   if (ref->pVM != vm)
      vmThrow(vm,E_ASSERTION_ERROR,"VM.iIsStrand: VM isn't self");
   if (!fiber->isFiber)
      vmThrow(vm,E_ASSERTION_ERROR,"VM.iIsStrand: VM isn't a fiber");

   fiber->isStrand = 1;
   return Void;
}

    //////////////////////////////////////// args

    /* .pasteArgs([offset=0,n=*]) : Copy all (or parts of) arglist to
     *  the top of the arg stack.
     */
static Instance *VM_pasteArgs(VMWR *ref,Instance *arglist,pVM vm)
{
   VM     *self = (VM *)vm;
   size_t  numArgs, offset = self->argOffset;
   int64_t n;
   int	   pi = arglistTryToGetInt(arglist,0,&n,0,vm);

   if (ref->pVM != vm)
      vmThrow(vm,E_ASSERTION_ERROR,"VM.pasteArgs: VM isn't self");

      	// n = vm.pasteArgs() is bogus
   if (!voidPlusOK(vm)) return Void;

   numArgs = self->numArgs;
   if (pi)		// .pasteArgs(n)
   {
#if 0
      if (n >= numArgs) numArgs = 0;
      else if (n >= 0)
      {
	 offset  += (size_t)n;
	 numArgs -= (size_t)n;
      }
#endif
      if (n >= 0)	// gcc: (int64_t)-1 > (size_t)3 WTF?
      {
	 if (n >= numArgs) numArgs = 0;
	 else
	 {
	    offset  += (size_t)n;
	    numArgs -= (size_t)n;
	 }
      }
   }
   listAppendN(self->arglist2,offset,numArgs,(pVM)self);
   return VoidPlus;		// out of band signal to opAddToArgs
}

    // .nthArg(n)
static Instance *VM_nthArg(VMWR *ref,pArglist arglist,pVM vm)
{
   int n = (int)arglistGetInt(arglist,0,0,vm);
   if (ref->pVM != vm)
      vmThrow(vm,E_ASSERTION_ERROR,"VM.nthArg: VM isn't self");
   return vmGetArg(((VM *)vm),n);
}

    /* fcn { vm.argsMatch(objs) }(arglist) --> Bool
     * * --> match any
     * "" --> match any string, "name" --> obj.name == name
     * NullClass --> any class, otherwise .isInstanceOf
     * nullFcn --> any fcn, otherwise .isInstanceOf
     * 0 --> match Int, 1 --> Int/Float
     * () --> numArgs == 0
     * default --> .isType
     * Arglist length mismatch:
     *    objs.len() <= arglist.len() --> OK
     *    objs.len() >  arglist.len() --> False
???L(x,y,z) --> match any of
     */
static Instance *VM_argsMatch(VMWR *ref,pArglist arglist,pVM vm)
{
   int	     n,at;
   Instance *i, *a;	// i is type to match, a is vm.arglist
   VM 	    *self = (VM *)vm;

   if (ref->pVM != vm)
      vmThrow(vm,E_ASSERTION_ERROR,"VM.argsMatch: VM isn't self");

   for (n = 0; (i = listGet(arglist,n)); n++)
   {
      a = listGet(self->arglist2,n + self->argOffset);
      if (!a) return BoolFalse;		// objs.len() > arglist.len()

      if (i == Star) continue;		// * --> match any

      at = TYPEO(a);
      switch(TYPEO(i))
      {
	 default: if (TYPEO(i) != at) return BoolFalse; break;
	 case ClassType:	// NullClass --> match any class
	    if (i == NullClass && at == ClassType) break;
	    if (!classIsInstanceOf(a,i)) return BoolFalse;
	    break;
	 case FcnType:		// NullFcn --> match any fcn
	    if (i == nullFcn && at == FcnType) break;
	    if (!fcnIsInstanceOf(a,i)) return BoolFalse;
	    break;
	 case StringType:	// "" --> match any String
	    if (i != emptyString)	// "Foo" == obj.name
	    {
	       if (strcmp(stringText(i),iname(a))) return BoolFalse;
	       break;
	    }
	    if (at != StringType) return BoolFalse;
	    break;
	 case IntType:		// 0 --> match Int, 1 --> match Int or Float
	    if (IS_ONE(i) && (at == IntType || at == FloatType)) break;
	    if (at != IntType) return BoolFalse;
	    break;
	 case ListType: case TupleType:		// List/ROList/MList/Tuple
	    if (at != ListType && at != TupleType) return BoolFalse;
	    break;
      }
   }
   if (n == 0) return boolCreate(0 == self->numArgs);
   return BoolTrue;
}

    /* fcn { vm.argsAre(objs) }(arglist) --> Bool
     * * --> match any
     * "" --> match any string, "text" --> obj == text
     * NullClass  --> any class, otherwise .isInstanceOf
     * nullFcn    --> any fcn, otherwise .isInstanceOf
     * Int, float --> ==
     * () --> numArgs == 0
     * default --> .isType
     * Arglist length mismatch:
     *    objs.len() <= arglist.len() --> OK
     *    objs.len() >  arglist.len() --> False
     */
static Instance *VM_argsAre(VMWR *ref,pArglist arglist,pVM vm)
{
   int	     n,at;
   Instance *i, *a;	// i is type to match, a is vm.arglist
   VM	    *self = (VM *)vm;

   if (ref->pVM != vm)
      vmThrow(vm,E_ASSERTION_ERROR,"VM.argsMatch: VM isn't self");

   for (n = 0; (i = listGet(arglist,n)); n++)
   {
      a = listGet(self->arglist2,n + self->argOffset);
      if (!a) return BoolFalse;		// objs.len() > arglist.len()

      if (i == Star) continue;		// * --> match any

      at = TYPEO(a);
      switch(TYPEO(i))
      {
	 default: if (TYPEO(i) != at) return BoolFalse; break;
	 case ClassType:	// NullClass --> match any class
	    if (i == NullClass && at == ClassType) break;
	    if (!classIsInstanceOf(a,i)) return BoolFalse;
	    break;
	 case FcnType:		// NullFcn --> match any fcn
	    if (i == nullFcn && at == FcnType) break;
	    if (!fcnIsInstanceOf(a,i)) return BoolFalse;
	    break;
	 case StringType:	// "" --> match any String
	    if (at != StringType) return BoolFalse;
	    if (i != emptyString && strcmp(stringText(i),stringText(a)))
	       return BoolFalse;
	    break;
	 case IntType:
	 {
	    #if USE_POINTER_INTS
	       ZKL_Int i64;
	       i = decantInt(i,&i64);
	    #endif
	    if (Int_eq(i,a,vm) == BoolFalse) return BoolFalse;
	    break;
	 }
	 case FloatType:
	    if (Float_eq(i,a,vm) == BoolFalse) return BoolFalse;
	    break;
	 case ListType: case TupleType:		// List/ROList/MList/Tuple
	    if (at != ListType && at != TupleType) return BoolFalse;
	    break;
      }
   }
   if (n == 0) return boolCreate(0 == self->numArgs);
   return BoolTrue;
}

Instance *Object_findGlobalName(Instance *,pArglist,pVM);
Instance *Object_idToGlobalName(Instance *,pArglist,pVM);
Instance *Object_globalNameStats(Instance *,pVM);

static const MethodTable methods[] =
{
   "toString",		(pMethod)VM_toString,
   "kick",		(pMethod)VM_kick,
//   "setDebugLevel",	(pMethod)VM_setDebugLevel,
   "stackTrace",	(pMethod)VM_stackTrace,
   "createFiber",	(pMethod)VM_createFiber,
   "yield",		(pMethod)VM_yield,
   "resume",		(pMethod)VM_resume,
   "pasteArgs",		(pMethod)VM_pasteArgs,
   "nthArg",		(pMethod)VM_nthArg,
   "argsMatch",		(pMethod)VM_argsMatch,
   "argsAre",		(pMethod)VM_argsAre,
   "iIsStrand",		(pMethod)VM_brandStrand,

   "findGlobalName",	Object_findGlobalName,
   "idToGlobalName",	Object_idToGlobalName,
   0,			0
};

/* ******************************************************************** */
/* **************************** Properties **************************** */
/* ******************************************************************** */

    // .name
static Instance *VM_name(VMWR *ref, pVM vm)
   { return VM_toString(ref,NoArglist,vm); }

    // .registers
static Instance *VM_registers(VMWR *ref,VM *vm)
{
   ZKL_Block   *block;
   int		numRegisters;
   Instance    *list;
   int		i;

   if (ref->pVM == (pVM)vm || vm->isFiber == FIBER_STALLED)
   {
      block	   = currentBlock(vm->blockStack);
      numRegisters = block->numRegisters;
      list	   = tupleCreate(2 + numRegisters,I_OWNED,(pVM)vm);

      tupleAppend(list,vm->result);
      tupleAppend(list,vm->regX);
      for (i = 0; i < numRegisters; i++) 
         tupleAppend(list,block->registers[i]);
      return list;
   }

   vmThrow((pVM)vm,E_ASSERTION_ERROR,"VM.registers: VM won't speak");
   return Void;		// shut up the compiler
}

    // .regX
static Instance *VM_regX(Instance *ref,pVM vm)
{
   if (((VMWR *)ref)->pVM != vm)
      vmThrow(vm,E_ASSERTION_ERROR,"VM.regX: VM isn't self");
   return ((VM *)vm)->regX;
}
Instance *vmRegX(pVM vm) { return ((VM *)vm)->regX; }

    // .function: Dig into self to find a fcn that shares the running code
    // ??!!! how is this different from self.fcn?
    // VM not running --> no class
static Instance *VM_fcn(VMWR *ref, pVM vm)

{
   if (ref->pVM != vm)
      vmThrow(vm,E_ASSERTION_ERROR,"VM.function: VM isn't self");
   return classFcnMatchup(VM_SELF(vm),((VM *)vm)->pc.code,vm);
}

    // .arglist
    // This isn't self.fcn.arglist because I want only the running
    // function to call it.
static Instance *VM_arglist(VMWR *ref,pVM vm)
{
   if (ref->pVM != vm)
      vmThrow(vm,E_ASSERTION_ERROR,"VM.arglist: VM isn't self");
   return listToTuple(((VM *)vm)->arglist2,((VM *)vm)->argOffset,
		      ((VM *)vm)->numArgs,vm);
}

    // .numArgs
static Instance *VM_numArgs(VMWR *ref, pVM vm)
{
   if (ref->pVM != vm)
      vmThrow(vm,E_ASSERTION_ERROR,"VM.numArgs: VM isn't self");
   return INT_CREATE(((VM *)vm)->numArgs,vm);
}

static void vmLockUnlocker(Fence *f, Instance *i)
   { SPIN_LOCK_RELEASE(&_vmLock); }

    // .vms --> List, not Tuple. (vm.vms - vm) is useful
static size_t _addVMToList(VM *vm,void *list)
   { listAppend((Instance *)list,_vm2Instance((pVM)vm,0),(pVM)vm); return 0; }
static Instance *_listVMs(pVM vm)
{
   Instance *list;
   int	     n;
   Fence     fence;

   spinLockAcquire2(&_vmLock,0,vm);
   vmSetFence(vm,&fence,vmLockUnlocker,0);
      n    = numVMs;
      list = fence.i1 = listCreate(n,0x1,I_OWNED,vm);
      walkVMs(_addVMToList,list);
   vmRemoveFence(&fence,1);

   return list;
}
static Instance *VM_listVMs(Instance *_,pVM vm) { return _listVMs(vm); }

    // .numVMs
static Instance *VM_numVMs(Instance *_,pVM vm)
   { return intCreate(numVMs,vm); }

    // .vmCounts --> L(liveVMs, high water, total ever)
static Instance *VM_vmCounts(Instance *_,pVM vm)
{
   return tupleCreateX(vm,
	intCreate(numVMs,vm),intCreate(vmHW,vm),intCreate(vmCount,vm),ZNIL);
}

    // .numThreads
static Instance *VM_numThreads(Instance *_,pVM vm)
   { return intCreate(numThreads(0),vm); }

    // .threadCounts --> L(live threads, high water)
static Instance *VM_threadCounts(Instance *_,pVM vm)
{
   int max, n = numThreads(&max);
   return tupleCreateX(vm, intCreate(n,vm), intCreate(max,vm), ZNIL);
}

    // .size --> L(arglist size, stack size, block stack size, object size)
static Instance *VM_size(Instance *_,pVM vm)
{
   return tupleCreateX(vm,
	intCreate(listSize(((VM *)vm)->arglist2,vm),vm),
	intCreate(((VM *)vm)->stki,vm),
	intCreate(((VM *)vm)->blockStack->size,vm),
	intCreate(sizeof(VM),vm), intCreate(sizeof(VMWR),vm), 
	ZNIL);
}

    // .libraries
static Instance *VM_libraries(Instance *_, pVM vm)
{
   return createTupleOfStrings(vm,
	"Dynamic hash tables: Esmond Pit","",
	"MD5 Message-Digest Algorithm: RSA Data Security, Inc.",
	   "http://www.ietf.org/rfc/rfc1321.txt",
	"Regular expressions: Ozan S. Yigit","http://www.cse.yorku.ca/~oz/",
	"Wild card matching (wildmat): Rich $alz","",
	#ifdef __DLMALLOC
	   "dlmalloc: Doug Lea","",
	#endif
	#ifdef __NEDMALLOC
	   "nedalloc: Niall Douglas",
	   "http://www.nedprod.com/programs/portable/nedmalloc/",
	   "dlmalloc: Doug Lea","",
	#endif
	(char *)0);
}

    // .xxception --> exception that was thrown to cause exit fcns to run
static Instance *VM_xxception(VMWR *ref,VM *vm)
{
   while(vm)
   {
      if (vm->xxception) return vm->xxception;
      vm = vm->parentVM;
   }
   return Void;
}

    // .isRunning
static Instance *VM_isRunning(VMWR *ref,VM *vm)
{
   VM *theVM = (VM *)ref->pVM;
   if (theVM && isRunning(theVM)) return BoolTrue;
   return BoolFalse;
}

    // .isDead --> Bool, has the ref'd VM been GC'd?
static Instance *VM_isDead(VMWR *ref, pVM vm)
   { return ref->pVM ?  BoolFalse : BoolTrue; }

    // .isFiber -->0|1|2 (no,stalled,running)
int vmIsFiber(pVM vm)
{
   VM *theVM = (VM *)vm;
   return theVM->isFiber;
}
static Instance *VM_isFiber(VMWR *ref,VM *vm)
{
   VM *theVM = (VM *)ref->pVM;
   if (theVM && isRunning(theVM)) return intCreate(theVM->isFiber,(pVM)vm);
   return Zero;
}

    // .isStrand -->Bool
static Instance *VM_isStrand(VMWR *ref,VM *vm)
{
   VM *theVM = (VM *)ref->pVM;
   if (theVM && isRunning(theVM) && theVM->isStrand) return BoolTrue;
   return BoolFalse;
}

    // .isThread
int vmIsThread(pVM vm)
{
   VM *theVM = (VM *)vm;
   if (theVM && isRunning(theVM) && ((size_t)theVM->rootVM->isThread > 10))
      return 1;
   return 0;
}
static Instance *VM_isThread(VMWR *ref,VM *vm)
   { return vmIsThread(ref->pVM) ? BoolTrue : BoolFalse; }


static const PropertyTable properties[] =
{
   "name",		(pProperty)VM_name,
   "registers",		(pProperty)VM_registers,
   "regX",		VM_regX,
   "vms",		VM_listVMs,
   "numVMs",		VM_numVMs,
   "vmCounts",		VM_vmCounts,
   "threadCounts",	VM_threadCounts,
   "numThreads",	VM_numThreads,
   "size",		VM_size,
   "libraries",		VM_libraries,
   "function",		(pProperty)VM_fcn,
   "arglist",		(pProperty)VM_arglist,
   "numArgs",		(pProperty)VM_numArgs,
   "xxception",		(pProperty)VM_xxception,
   "isRunning",		(pProperty)VM_isRunning,
   "isDead",		(pProperty)VM_isDead,
   "isFiber",		(pProperty)VM_isFiber,
   "isStrand",		(pProperty)VM_isStrand,
   "isThread",		(pProperty)VM_isThread,
   "globalNameStats",	Object_globalNameStats,
   0,			0
};


/* ******************************************************************** */
/* ***************************** Op Codes ***************************** */
/* ******************************************************************** */

static Instance *VM_eq(VMWR *ref,VMWR *X,pVM vm)
{
   pVM rv = ref->pVM;
   if (rv && TYPEO(X) == VMType && rv == X->pVM) return BoolTrue;
   return BoolFalse;
}

static Instance *VM_neq(VMWR *ref,VMWR *X,pVM vm)
   { return (VM_eq(ref,X,vm) == BoolFalse) ? BoolTrue : BoolFalse; }

static const OpcodeTable ops[] = 
{
   OP_EQ,	(pOp)VM_eq,
   OP_NEQ,	(pOp)VM_neq,
   0,		0
};


//////////////////////////////////////////////////////


//static pMethod   in_vm_methods(   Instance *ignore, register char *str);
//static pProperty in_vm_properties(Instance *ignore, register char *str);

void vmConstruct(void)
{
   static IBucketHeader _vmwrBuckets;	// might not be used

   constructObject(&VMWRObject,VMType,methods,properties,ops, NoVM);
   VMWRObject.isize	     = sizeof(VMWR);
   VMWRObject.isBInstance    = 1;
   VMWRObject.threadSafe     = 1;
   VMWRObject.magicMarker    = vmwrMarker;

//   bucketReserve(sizeof(VM),15,&vmBuckets,1,NoVM);

   vmwrBuckets = ibucketHitchHike(&VMWRObject,4,10,&_vmwrBuckets,NoVM);

   spinLockInit(&_vmLock);
   spinLockInit(&globalFenceLock); globalFence.prev = &globalFence;
   CAI_INIT(&_gcCount);

   constructExitFcn();
}


///////////////////////////////////////////////////////////////
// zkl extractTable.zkl    < vm.c | gperf | zkl gperf.zkl -i vm
// zkl extractTable.zkl -p < vm.c | gperf | zkl gperf.zkl -i vm




////////////////////////////////////////////////////////////////
// zkl extractTable.zkl -p < vm.c | gperf | zkl gperf.zkl -i vm


