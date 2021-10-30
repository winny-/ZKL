/* pcBlock.c : PC and Block objects
 * 
 * Copyright (c) 2006,7,8,9,10,2011 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#include <stdio.h>
#include <string.h>

#define __NOT_A_DLL
#define __FCN_INTERNALS
#define __PC_INTERNALS
#define __LIST_INTERNALS
#define __GC_INTERNALS

#include "zklObject.h"
#include "zklFcn.h"
#include "zklClass.h"
//#include "zklMethod.h"	// BlockHead
#include "zklPCBlock.h"
#include "zklString.h"	// ZBText

#include "zklList.h"
#include "zklMemory.h"

/* ******************************************************************** */
/* ********************** Program Counter ***************************** */
/* ******************************************************************** */

static ZKL_PC *_setPC(ZKL_PC *pc,ZKL_Code *code,size_t offset)
{
   pc->code = code;		// struct copy
   pc->addr = &code->code[offset];
   return pc;
}

ZKL_PC *pcInit(pVM vm,ZKL_Code *code,size_t offset)
   { return _setPC(vmPC(vm),code,offset); }

void pcClear(ZKL_PC *self) { memset(self,0,sizeof(ZKL_PC)); }

#if 0
static void pcSetAddr(ZKL_PC *self, size_t offset)
   { self->addr = &self->code->code[offset]; }
#endif

      //////////////////////////////////// Code
      /////////////////////////////////////////
    /* This doesn't need to be called because Code is embedded in a FcnBase,
     * which lives in vm->self, which will be marked (and mark this Code) if
     * this code is running.
     * If that is not the case (eg fcn embryo that hasn't been added to it's
     * class), FcnBase can be freed which will result in PC pointing at
     * garbage.
     */
#if 0
void pcMarker(ZKL_PC *pc)
{
   ZKL_Code *code = pc->code;

   if (!code) return;
   if (code->asmDotCode) instanceMark(code->asmDotCode);
}
#endif

/* ******************************************************************** */
/* ****************************** Blocks ****************************** */
/* ******************************************************************** */

/*              |<-------------------- size ------------------->|
 * Block Stack: [Stack Data][BLock]...[Block][Block]
 *                          ^ZERO            ^cbo   ^offset
 *                                    ^stackOffset
 * Blocks are variable size (depends on the number of registers)
 * 
 * Block stacks are dynamic - they can grow while a program is running so
 * the VM has to be careful and not lose track.
 * 
 * GC marking can be happening while the stack is moving or being modified,
 * even though the VM is single threaded.  It is the responsibility of the
 * VM to lock marking, ie this code can ignore GC.
 */

#define INITIAL_STACK_SIZE	 3000
#define STACK_STEP_SIZE		 1000
#define BIG_STACK_SIZE	       200000
#define DIE_STACK_SIZE	       400000

#define STACK_ADD(stack,n)	((Byte *)(stack) + n)
#define CURRENT_BLOCK(stack)	((ZKL_Block *)STACK_ADD(stack,stack->cbo))
#define PREV_BLOCK(stack,block)	\
			((ZKL_Block *)STACK_ADD(stack,block->stackOffset))

    // offset of stack->blocks so I can use offset==0 as empty
#define ZERO	(sizeof(BlockStack))  // 12 (Visual C 32 bit), 16 (Linux/64)

ZKL_Block *currentBlock(BlockStack *stack) { return CURRENT_BLOCK(stack); }
ZKL_Block *blockAtOffset(BlockStack *stack,unsigned offset)
{
   if (offset == 0) return 0;
   return (ZKL_Block *)STACK_ADD(stack,offset);
}

ZKL_Block *prevBlock(BlockStack *stack, ZKL_Block *block)
{
   if (block->stackOffset == 0) return 0;	// empty stack
   return PREV_BLOCK(stack,block);
}

#if 0
BlockStack *blockStackClear(BlockStack *stack)
{
   stack->offset = ZERO;
   stack->cbo	 = 0;	// not a valid stack address
}
#endif

BlockStack *blockStackReset(BlockStack *stack)
{
///!!!! if stack is mondo, free it.
   stack->offset = ZERO;
   stack->cbo	 = 0;	// not a valid stack address
   return stack;
}

int inBlockStack(Instance *i, BlockStack *stack)
{
   int n;
   n = ((Byte *)i > (Byte *)stack) && 
       ((Byte *)i < STACK_ADD(stack,stack->size));
   return n;
}

    /* Be careful not to call this if it is possible for the stack to
     * change (such as popping). If it does, this will mark garbage.
     */
void blockMarker(BlockStack *stack)
{
   ZKL_Block *block;
   #if GC_SANITY_CHECK
      size_t cbo = stack->cbo, size = stack->size;
   #endif

   if (stack->cbo == 0) return;
   block = CURRENT_BLOCK(stack);
   while (block)
   {
      int n = block->numRegisters;
      Instance **registers = block->registers;

      #if GC_SANITY_CHECK
         if (n > 255) vmHalt("blockMarker: Too many registers, corrupt block");
      #endif

      while(n--) instanceMark(*registers++);
      instanceMark(block->klass);
      block = prevBlock(stack,block);
   }
   #if GC_SANITY_CHECK
      if (cbo != stack->cbo || size != stack->size)
	 vmHalt("blockMarker: block stack changed");
   #endif
}

    // called only by the VM, early in VM creatation
BlockStack *blockStackAlloc(pVM vm)
{
   BlockStack *stack	= ZMALLOC(INITIAL_STACK_SIZE);
   if (!stack) vmPanic(vm,"Out of memory in a bad way: Can't allocate stack");
   stack->size		= INITIAL_STACK_SIZE;
   stack->offset	= ZERO;
   stack->cbo		= 0;

   return stack;
}

#if 0
void blockStackFree(BlockStack *stack)
{
   ZFREE(stack);
}
#endif

    // Move the stack to a bigger place
    // Doesn't GC but should
static BlockStack *growStack(BlockStack **_stack, pVM vm)
{
   BlockStack  *newStack, *stack = *_stack;
   int 		tooBig=0;

   if (stack->size > BIG_STACK_SIZE)
   {
      if (stack->size > DIE_STACK_SIZE)
	 vmPanic(vm,"Told you once, twice, now you die (pork stack)");
      tooBig = 1;
   }
   newStack = ZREALLOC(stack,stack->size + STACK_STEP_SIZE);
   if (!newStack)
      vmPanic(vm,"Out of memory in a bad way: Can't grow stack");
   newStack->size += STACK_STEP_SIZE;
   *_stack = newStack;
   if (tooBig) vmThrow(vm,E_ASSERTION_ERROR,
			"That is one big stack, infinite recursion?");
   return newStack;
}

    // The block stack can move
    // NOT thread safe (from GC in this case)
ZKL_Block *blockCreate(BlockStack **theStack, pVM vm)
{
   unsigned	bytes = sizeof(ZKL_Block);
   ZKL_Block   *block;
   BlockStack  *stack = *theStack;

   if (stack->offset + bytes >= stack->size) stack = growStack(theStack,vm);

   block = (ZKL_Block *)STACK_ADD(stack,stack->offset);

   memset(block,0,sizeof(ZKL_Block));	// make block GC'able

   block->stackOffset = stack->cbo;	// offset of previous block, 0 for 1st

   stack->cbo = stack->offset;
   stack->offset += bytes;

   return block;
}

    // The block stack can move
    // NOT thread safe (from GC in this case)
void blockAllocRegisters(BlockStack **theStack, int n, pVM vm)
{
   BlockStack   *stack = *theStack;
   ZKL_Block    *block;
   Instance    **registers;

   block = CURRENT_BLOCK(stack);
   if (n)
   {
      uint32_t bytes = n * sizeof(Instance *);
      if (stack->offset + bytes >= stack->size) stack = growStack(theStack,vm);
      block = CURRENT_BLOCK(stack);
      stack->offset += bytes;
   }
   block->numRegisters = n;

      // Do this so I can GC immediately, !!!this line kills GCC -O*, -g no O fine
   registers = block->registers; while(n--) *registers++ = Void;
}

ZKL_Block *blockUp(BlockStack *stack, int n)	// n >= 0
{
   ZKL_Block *block;

   block = CURRENT_BLOCK(stack);
   while (n--) block = PREV_BLOCK(stack,block);
   return block;
}

    // NOT thread safe (from GC in this case)
ZKL_Block *blockPop(BlockStack *stack)
{
   ZKL_Block *block;

   stack->offset = stack->cbo;
   stack->cbo	 = CURRENT_BLOCK(stack)->stackOffset;

   block = (stack->cbo == 0) ? 0 : CURRENT_BLOCK(stack);
   return block;
}

#if 0
    // restore the block just popped
void blockRestore(ZKL_Block *block, BlockStack *stack)
{
   stack->cbo	 = stack->offset; //(uint32_t)((Byte *)block - (Byte *)stack);
   stack->offset += sizeof(ZKL_Block) + block->numRegisters*sizeof(Instance *);
}
#endif

    // Get rid of all blocks after (and including) self
    // NOT thread safe (from GC in this case)
void blockPopTo(ZKL_Block *self, BlockStack *stack)
{
//if (!self) { blockStackClear(stack); return; }
   stack->offset = 
       (uint32_t)((char *)self - (char *)stack);  // == start of Block
   stack->cbo	 = self->stackOffset;		  // previous block
}

    /* Print a stack trace.
     * Doesn't find code running from C code/methods.
     * Can print blocks that are irrelevant, eg nested blocks.
     * A stacked block.class can be out of sync with the PC
     */
Instance *stackTrace(pVM vm, int toStderr,int toString)
{
   extern int vmPendingException(pVM);
   extern pVM vmParent(pVM);

   char   buf[300], prevLine[300];
   FILE  *out = toStderr ? stderr : stdout;
   Fence  fence;
   ZBText Text, *text = 0;

   vmSetFence(vm,&fence,0,0);

   if (toString) fence.i = zbtextInit((text = &Text),vm);
   if (toStderr == -1) out = 0;

   vm = vmLeaf(vm);
   do
   {
      BlockStack *stack = vmBlockStack(vm);
      ZKL_Block  *block = (stack->cbo == 0) ? 0 : CURRENT_BLOCK(stack);
      ZKL_PC     *pc    = vmPC(vm);
      char        flags[80];
      int	  repeats;

      *flags = '\0';
      if (vmIsThread(vm)) strcat(flags,"T");
      if (vmIsFiber(vm))  strcat(flags,"F");
      sprintf(buf,"Stack trace for VM#%d (%s):\n",vmID(vm),flags);
      if (out) fputs(buf,out); if (text) zbtextAppend(text,buf,vm);

      *prevLine = '\0'; repeats = 0;
      while (block)
      {
	 Instance *klass     = block->klass;	// can be 0, --> ""
	 char     *klassName = className(klass);
	 unsigned  line      = block->lineNum;

	 sprintf(flags,"args(%d) reg(%d) ",block->numArgs,block->numRegisters);
	 if (vmPendingException(vm)) strcat(flags,"P");
	 if (block->exitReg)	     strcat(flags,"x");
	 if (block->numExceptions)   strcat(flags,"E");
	 if (block->stop)	     strcat(flags,"S");
	 if (block->returnTo)	     strcat(flags,"R");

	 if (pc)
	 {
	    if (pc->code)
	    {
	       Instance *fcn    = classFcnMatchup(klass,pc->code,0);
	       unsigned  offset = PC_OFFSET(pc);	// 32 bits
	       char     *fname, *mname=0;

	       if (fcn == Void)	// self/code out of sync, default arg?
		  fname = "?";
	       else
	       {
		  klassName = className(FCN_CONTAINER(fcn));
		  fname     = fcnName(fcn);

		  if (offset > 3)	// take a guess at called name
		  {
		     Byte    *a  = (pc->addr - 3);
		     unsigned op = *a;
		     if(op==opReCall || op==opReCallZ)
		     {
			unsigned n = ( *(a+1) << 8 | *(a+2) );
			if (n < pc->code->stringSize)
			   mname = pc->code->strings + n;
		     }
		     else if(op==opCallNW || op==opCallNWZ)
		     {
			unsigned n = ( *(a+1) << 8 | *(a+2) );
			mname = getGlobalName(n,NoVM);
		     }
		  }
	       }
	       if(mname) sprintf(buf,"   %s.%s@%s addr:%u  %s",
				  klassName,fname,mname,offset,flags);
	       else sprintf(buf,"   %s.%s addr:%u  %s",
				  klassName,fname,offset,flags);
	    }
	    else sprintf(buf,"   %s.<fcn entry block>  %s",klassName,flags);
	 }
	 else sprintf(buf,"   %s.?? addr:??  %s",klassName,flags);

	 if(line) sprintf(buf+strlen(buf),"   Line:~%d",line);

#if 0
	 if (out) fputs(buf,out); if (text) zbtextAppend(text,buf,vm);
	 if (out) fprintf(out,"\n"); if (text) zbtextAppend(text,"\n",vm);
#else
	 if (0 == strcmp(prevLine,buf)) repeats++;
	 else
	 {
void poot(char *, FILE *, ZBText *,pVM vm);
	    strcpy(prevLine,buf);
	    if (repeats) 
	    {
	       sprintf(prevLine,"   <repeats %d times>",repeats);
	       poot(prevLine,out,text,vm);
	    }	       
	    poot(buf,out,text,vm);
	    strcpy(prevLine,buf); repeats = 0;
	 }
#endif
	 block = prevBlock(stack,block);
	 if (block) pc = &block->returnAddr;
      } // while block
      vm = vmParent(vm);
   } while(vm);

   if (text) zbtextClose(text,vm);
   vmRemoveFence(&fence,0);
   if (text) return text->string;

   return 0;
}

void poot(char *buf, FILE *out, ZBText *text,pVM vm)
{
   if (out)  { fputs(buf,out); fprintf(out,"\n"); }
   if (text) { zbtextAppend(text,buf,vm); zbtextAppend(text,"\n",vm); }
}

   /* Reset some block bits for a fcn call or just to create some space for
    * a new set of registers.
    * blockCreate() has zero'd the block.
    */
void blockReset(ZKL_Block *self,Instance *fcn, pVM vm)
{
   self->returnTo      = 0;
   self->numExceptions = 0;
   self->numRegisters  = 0;
   self->argOffset     = 0;
   self->numArgs       = 0;
   self->exitReg       = 0;
   self->stop	       = 0;
   self->klass	       = FCN_CONTAINER(fcn);
}

void runExitFcn(ZKL_Block *,int,Instance *e,pVM);	// vm.c

unsigned starId,plusTraceId,minusTraceId,bangId,dotId,zeroId; //set in object.c

    /* If the exception is uncatchable, name == 0
     * Catch table:
     *   address of catch block, offset of exception name, offset, ...
     * Specials:
     *   !name: Don't catch that exception
     *   :name: Match if a parent of thrown exception is name
     *   "*": Catch any catchable
     *   "0": Catch the uncatchable, in addition to anything else
     *   "1": Stack trace, toggles
     *   "2": Don't want a stack trace, toggles
     * Notes:
     *    If catch block is found, the block chain is modified to unlink all
     *      extrainous blocks and those blocks are freed.
     *    If not found, the block chain is unchanged.
     * NOT thread safe (from GC in this case)
     * Returns:
     *   The catch block or stop block.
     *   0 if the not caught.
     */
ZKL_Block *findCatchBlock(Instance *eClass,pVM vm)
{
   int		numExceptions, n, wantStackTrace;
   size_t	offset;
   ZKL_PC	pc;
   BlockStack  *stack = vmBlockStack(vm);
   ZKL_Block   *block = CURRENT_BLOCK(stack), *b=0;
   unsigned	eid, catcher, getEid=1;

   if (stack->cbo == 0) return 0;   	// dead VMs don't catch exceptions

   while (block)
   {
      numExceptions = block->numExceptions;
      if (block->stop == HARD_STOP) return block;	// stop block

      if ((n = block->exitReg))
      {
	     // Use True for uncatchable exceptions
	 runExitFcn(block,n-1,eClass ? eClass : BoolTrue,vm);
	     // block stack has changed, resync
	 if (stack != vmBlockStack(vm))
	 {
	    vmHalt("Ack! exitReg, handle this case!");
	 }
      }

      if (numExceptions)
      {
	 wantStackTrace = 0;
	 _setPC(&pc,block->returnAddr.code,block->exceptionTableOffset);
	 for (n = numExceptions; n--; )
	 {
    		// pull the offset and name from the exception table
	    PC_GETW(&pc,offset);	// get offset of catch code
	    PC_GETW(&pc,catcher);
	    if(!eClass)		// not normally catchable
	    {
	       if(catcher==zeroId) goto caught;  // catch(0)
	       continue;
	    }
	    if(catcher==starId)		// catch(*) aka catch
	    {
	    caught:
	       if(wantStackTrace) stackTrace(vm,1,0);
	       pcInit(vm,pc.code,offset);
	       if(b) blockPopTo(b,stack);

	       return block;
	    }
	    if(catcher==plusTraceId) { wantStackTrace = 1; continue; }
	    if(catcher==minusTraceId){ wantStackTrace = 0; continue; }

	    if(getEid)	// doesn't get here if eClass==0
	       { getGlobalId(className(eClass),&eid,vm); getEid = 0; }

	    if(eid==catcher) goto caught; // throw(E) catch(E)
	    if(catcher==bangId)		// throw(BadDay) catch(!BadDay)
	    {				// don't catch this exception
	       PC_GETW(&pc,catcher);
	       PC_GETW(&pc,catcher);
	       if(catcher==eid) goto next;
	    }
	    else if(catcher==dotId)	// exception and children
	    {	    			// catch(BadDay.)
	       PC_GETW(&pc,catcher);
	       PC_GETW(&pc,catcher);
	       if(catcher==eid) goto caught;
	       if(classFindParentById(eClass,catcher,1,vm)) goto caught;
	    }
	 } // for
      }
    next: ;
      b	    = block;
      block = prevBlock(stack,block);
   } // while
   return 0;
}

#if 0
/* ******************************************************************** */
/* **************************** BlockHead ***************************** */
/* ******************************************************************** */

typedef struct
{
   BInstance  instance;
   ZKL_Block *blk;
   int	      live;
} BlockHead;	// 24 (Linux/64)

#define REG(bh)     ( ((ZKL_Block *)(((BlockHead *)bh)->blk))->registers )
#define NUM_REG(bh) ( ((ZKL_Block *)(((BlockHead *)bh)->blk))->numRegisters )

static ZKL_Object     BlockHeadObject;
static IBucketHeader *bhBuckets = 0;

    // BlockHead.get(n) --> block.registers[n]
static Instance *BH_get(Instance *self, pArglist arglist, pVM vm)
{
   int n = (int)arglistGetInt(arglist,0,0,vm);
   if (((BlockHead *)self)->live && n < NUM_REG(self)) return REG(self)[n];
   
   return Void;
}

    // BlockHead.getUp(up,n) -->i


    // BlockHead.set(n,v) -->v|Void
static Instance *BH_set(Instance *self, pArglist arglist, pVM vm)
{
   int	     n = (int)arglistGetInt(arglist,0,0,vm);
   Instance *v = arglistGet(arglist,1,0,vm);

   if (((BlockHead *)self)->live && n < NUM_REG(self))
   {
      REG(self)[n] = v;
      return v;
   }
   return Void;
}

    // BlockHead.setr(T(ns),T(vs)) -->Void
static Instance *BH_setr(Instance *self, pArglist arglist, pVM vm)
{
   Instance *ns = arglistGet(arglist,0,0,vm);
   Instance *vs = arglistGet(arglist,1,0,vm), *v;
   int       n, z;

   if (!((BlockHead *)self)->live) return Void;

   for (z = 0; (v = listGet(vs,z)); z++)
   {
      n = (int)arglistGetInt(ns,z,0,vm);
      REG(self)[n] = v;
   }
   return Void;
}

    // BlockHead.setUp(up,n,v) -->?

    // BlockHead.kill() -->?
static Instance *BH_kill(Instance *self, pArglist arglist, pVM vm)
{
   ((BlockHead *)self)->live = 0;
   return Void;
}

static const MethodTable bhMethods[] =
{
   "get",	BH_get,
   "set",	BH_set,
//   "setr",	BH_setr,
   "kill",	BH_kill,
   0,		0
};

    // BH.create() -->
static Instance *BH_create(Instance *self,pArglist arglist,pVM vm)
{
   BlockHead  *bh;
   BlockStack *stack = vmBlockStack(vm);
   ZKL_Block  *block = (stack->cbo == 0) ? 0 : CURRENT_BLOCK(stack);

   if (!bhBuckets)	// BlockHead not constructed yet, NOT thread safe
   {
      constructObject(&BlockHeadObject,NativeType, bhMethods,0,0);
      BlockHeadObject.name        = "BlockHead";
      BlockHeadObject.vaultPath   = "";
      BlockHeadObject.isize       = sizeof(BlockHead);
      BlockHeadObject.isBInstance = 1;
      BlockHeadObject.createReturnsSelf = 1;

      bhBuckets = ibucketShare(&BlockHeadObject,0);
   }

   bh = (BlockHead *)ibucketAllocate(bhBuckets,&BlockHeadObject,I_OWNED,1,vm);
//   bh->blk  = prevBlock(stack,block);
   bh->blk  = block;
   bh->live = 1;
printf("num registers: %d\n",bh->blk->numRegisters);
   return (Instance *)bh;
}

static void BHConstruct(void)
{
   vaultAddData("BlockHead",methodCreate(Void,0,BH_create,NoVM),NoVM);
}
#endif

void blockConstruct(void)
{
//   BHConstruct();
}
