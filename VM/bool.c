/* bool.c : The Bool Object
 * 
 * Copyright (c) 2006,7,8,9,10,2011 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#include <string.h>

#define __NOT_A_DLL
#define __STRING_INTERNALS

#include "zklObject.h"
#include "zklNumber.h"
#include "zklString.h"

static ZKL_Object BoolObject;

static Instance theOnlyTrueObject, theOnlyFalseObject;
Instance *BoolTrue = &theOnlyTrueObject, *BoolFalse = &theOnlyFalseObject;
	 
static Instance *STrue, *SFalse;	// Strings

Instance *boolCreate(int x) { return x ? BoolTrue : BoolFalse; }

/* ******************************************************************** */
/* *************************** Methods ******************************** */
/* ******************************************************************** */

Instance *	// convience method for other objects
Bool_soTrue(Instance *self,pArglist arglist,pVM vm) { return BoolTrue; }

Instance *	// convience method for other objects
Bool_nope(Instance *self,pArglist arglist,pVM vm) { return BoolFalse; }

    // .toString
static Instance *
Bool_toString(Instance *self,pArglist arglist,pVM vm)
{
   if ( BOOLV(self) ) return STrue;
   return SFalse;
}

    // .toInt
static Instance *
Bool_toInt(Instance *self,pArglist arglist,pVM vm)
{
   if ( BOOLV(self) ) return One;
   return Zero;
}

    // .toFloat
static Instance *
Bool_toFloat(Instance *self,pArglist arglist,pVM vm)
{
   if ( BOOLV(self) ) return FOne;
   return FZero;
}

static const MethodTable methodTable[] = 
{
   "create",	Object_noop,
   "toBool",	Object_noop,
   "toFloat",	Bool_toFloat,
   "toInt",	Bool_toInt,
   "toString",	Bool_toString,
   0,		0
};

/* ******************************************************************** */
/* ***************************** Op Codes ***************************** */
/* ******************************************************************** */

    // if it ain't a Bool, they ain't equal
static Instance *Bool_eq(Instance *self,Instance *X,pVM vm)
{
   if (TYPEO(X) == BoolType && BOOLV(self) == BOOLV(X)) return BoolTrue;
   return BoolFalse;
}

static Instance *Bool_neq(Instance *self,Instance *X,pVM vm)
{
   if (TYPEO(X) != BoolType || BOOLV(self) != BOOLV(X)) return BoolTrue;
   return BoolFalse;
}


static Instance *Bool_add(Instance *self,Instance *X,pVM vm)	// logical OR
{
   Instance *x = convertTo(X,TO_BOOL,vm);
   return boolCreate(BOOLV(self) + BOOLV(x));
}

static Instance *Bool_sub(Instance *self,Instance *X,pVM vm)	// logical XOR
{
   Instance *x = convertTo(X,TO_BOOL,vm);
   return boolCreate(BOOLV(self) - BOOLV(x));
}

static Instance *Bool_mul(Instance *self,Instance *X,pVM vm)	// logical AND
{
   Instance *x = convertTo(X,TO_BOOL,vm);
   return boolCreate(BOOLV(self) * BOOLV(x));
}

Instance *boolNot(Instance *self) { return boolCreate( ! BOOLV(self) ); }

static Instance *Bool_negate(Instance *self,Instance *X,pVM vm)
   { return boolCreate( ! BOOLV(self) ); }

static const OpcodeTable opcodeTable[] = 
{
   OP_EQ,	(pOp)Bool_eq,
   OP_NEQ,	(pOp)Bool_neq,
   OP_ADD,	(pOp)Bool_add,
   OP_SUB,	(pOp)Bool_sub,
   OP_MUL,	(pOp)Bool_mul,
   OP_NEGATE,	(pOp)Bool_negate,

   0,		0
};


/* ******************************************************************** */
/* ******************************************************************** */

static ZKL_KString tText, fText;

void boolConstruct(void)
{
   constructObject(&BoolObject,BoolType,
	methodTable,(PropertyTable *)0,opcodeTable,NoVM);
   BoolObject.threadSafe    = 1;
   BoolObject.isize	    = sizeof(Instance);

   instanceInit(BoolTrue, &BoolObject,I_UNTOUCHABLE);
   instanceInit(BoolFalse,&BoolObject,I_UNTOUCHABLE);

   STrue  = kStringInit(&tText,"True");	// keep these off the heap
   SFalse = kStringInit(&fText,"False");
}
