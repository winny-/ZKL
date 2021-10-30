/* zklPCBlock.h : Manipulate PCs and Blocks
 * 
 * Copyright (c) 2006,7,8,9,10,2011 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */
#ifndef __ZKL_PCBLOCK_H
#define __ZKL_PCBLOCK_H

////////////////////////////// Code /////////////////////////////

typedef struct		// embedded in a FcnBase
{
   Byte     *code;	 // usually points into a KData (Asm.Code.code)
   char     *strings;	 // Null terminated strings (in Asm.Code.strings)
   uint16_t  codeSize;   // ZSCs support 24 bits
   uint16_t  stringSize; // ZSCs support 24 bits
   uint16_t  mapSize;
   Instance *kstrings;	 // actually KString[], points into FcnBase
   Byte     *map;	 // Map between opGetKString & strings (Asm.Code.map)
	 // Used once to create kstrings, max 255 entries
	 // map[0] == # entries, map[n] is index of nth string (in strings)
	 // Because of the way StringTables are created (see asm.zkl)
   	 // also hold info to link to global names table (fcnLink.c)
}ZKL_Code;	// 20 bytes (32 bit), 40 (64 bit)

//////////////////////////////// PCs /////////////////////////////

typedef struct
{
   ZKL_Code *code;
   Byte	    *addr;	// points into code->code
} ZKL_PC;		// 8 bytes

    /* Offsets and such should be unsigned but since they are two bytes max
     * it doesn't matter on a 32 bit int machine.
     * All of these increment (or set) the pc (except OFFSET)
     */

#define PC_OFFSET(pc)   ( (uint32_t)((pc)->addr - (pc)->code->code) )
#define PC_OFFSET_AT(pc,addr) ( (uint32_t)(addr - (pc)->code->code) )

#define PC_INC2(pc)   ( (pc)->addr += 2 )
#define PC_INC(pc,n)  ( (pc)->addr += n )

#define PC_GETB(pc)   ( *(pc)->addr++ )
#define PC_GETW(pc,w) ( w = PC_GETB(pc) << 8, w |= PC_GETB(pc) )

//#define PC_PEEKB(pc)   ( *(pc)->addr )
//#define PC_PEEKW(pc)   ( *(pc)->addr << 8 | *((pc)->addr+1) )

#define PC_SYNC(pc,addr)   ( (pc)->addr = addr )
#define ADDR_SYNC(pc,addr) ( addr = (pc)->addr )
#define ADDR_INC(addr,n)   ( addr += n )
#define ADDR_INC1(addr)    ( addr++ )
#define ADDR_INC2(addr)    ADDR_INC(addr,2)
#define ADDR_DEC(addr,n)   ( addr -= n )
#define ADDR_PEEKB(addr)   ( *addr )
#define ADDR_GETB(addr)    ( *addr++ )
#define ADDR_NEXT(addr)    ADDR_GETB(addr)
#define ADDR_PEEKW(addr)   ( (*addr << 8) | *(addr + 1) )
#define ADDR_GETW(addr,w)  ( w = *addr++ << 8, w |= *addr++ )


    /* 00: jmpFwd b1 b2	# pc == 1
     * 03: xxx
     * pc = pc + offset (== b1:b2)
     * 
     * 00: hopFwd b	# pc == 1
     * 02: xxx
     * pc = pc + offset (== b)
     */
//#define PC_JMP_FORWARD(pc) ( (pc)->addr += PC_PEEKW(pc) )
//#define PC_HOP_FORWARD(pc) ( (pc)->addr += PC_PEEKB(pc) )
#define ADDR_JMP_FORWARD(addr) ( addr += ADDR_PEEKW(addr) )
#define ADDR_HOP_FORWARD(addr) ( addr += ADDR_PEEKB(addr) )


    /* 00: jmpBack b1 b2  # pc == 1
     * 03: xxx
     * pc = pc - offset (== b1:b2)
     * 
     * 00: hopBack b	  # pc == 1
     * 02: xxx
     * pc = pc - offset (== b)
     */
//#define PC_JMP_BACK(pc) ( (pc)->addr -= PC_PEEKW(pc) )
//#define PC_HOP_BACK(pc) ( (pc)->addr -= PC_PEEKB(pc) )
#define ADDR_JMP_BACK(pc) ( addr -= ADDR_PEEKW(addr) )
#define ADDR_HOP_BACK(pc) ( addr -= ADDR_PEEKB(addr) )

    /* 00: getString b1 b2
     * Offset (b1:b2) into code->string, increment the pc
     */
#define PC_GET_STRING(pc,tmpOffset) \
	( (pc)->code->strings + PC_GETW(pc,tmpOffset) )

#define ADDR_GET_STRING(pc,addr,tmpOffset) \
	( ADDR_GETW(addr,tmpOffset), (pc)->code->strings + tmpOffset )

#ifdef __NOT_A_DLL

ZKL_PC *pcInit(pVM,ZKL_Code *,size_t offset);
void	pcClear(ZKL_PC *);
void    pcMarker(ZKL_PC *);

#endif	// __NOT_A_DLL

//////////////////////// Blocks //////////////////////////////

#define NoBlock		0

typedef struct Block	// AKA a stack frame AKA the stuff in { }s
{
   ZKL_PC	returnAddr;	 // set by caller when calling fcn
   Instance    *klass;		 // self

   unsigned int	stackOffset:32;	 // into BlockStack

      // from start of code, ZSC is 24 bits but codeSize is 16 bits
   unsigned int	exceptionTableOffset:16;

   unsigned int	lenArglist:16, lenArglistEx:16;
   unsigned int	argOffset:16,  numArgs:16;

   unsigned int	exitReg:8;	 // 0 if none, exitReg-1 for the actual reg
   unsigned int	numExceptions:8; // that this block will catch, <= 64
   unsigned int	stackSizeEx:8;	 // for exception recovery
   unsigned int	numRegisters:8;	 // < 256

   unsigned int stop:2;		 // 1: hard stop, 2: soft stop
   unsigned int returnTo:1;	 // 1: returnAddr is valid

   unsigned int lineNum:16;	 // into src

   Instance    *registers[0];	 // grows to fit
} ZKL_Block;	// 48 bytes Linux/64, packed for size but still has padding


typedef struct
{
   unsigned int size:32;
   unsigned int offset:32;   // where next block will start
   unsigned int cbo:32;	     // current block offset
// VC bitches on next line, why is same in ZKL_Block OK?!?
// padding isn't correct?
//   ZKL_Block    blocks[0];   // blocks start here, stack grows as needed
} BlockStack;	// 12 should be 16?

#ifdef __NOT_A_DLL

#define HARD_STOP	1
#define SOFT_STOP	2

void	    blockAllocRegisters(BlockStack **,int n, pVM);
ZKL_Block  *blockCreate(BlockStack **,pVM);
BlockStack *blockStackAlloc(pVM);
void	    blockMarker(BlockStack *);
BlockStack *blockStackReset(BlockStack *);
ZKL_Block  *blockPop(BlockStack *);
void	    blockRestore(ZKL_Block *, BlockStack *);
ZKL_Block  *blockUp(BlockStack *, int n);
ZKL_Block  *currentBlock(BlockStack *);
ZKL_Block  *prevBlock(BlockStack *, ZKL_Block *);
ZKL_Block *blockAtOffset(BlockStack *,unsigned offset);
void	    blockConstruct(void);
void	    blockReset(ZKL_Block *,Instance *fcn,pVM);
ZKL_Block  *findCatchBlock(Instance *eClass,pVM);
Instance   *stackTrace(pVM,int toStderr,int toString);
int	    inBlockStack(Instance *, BlockStack *);

#endif	// __NOT_A_DLL

#endif // __ZKL_PCBLOCK_H
