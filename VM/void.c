/* void.c : the Void Object
 * 
 * Copyright (c) 2006,7,8,9,10,2011 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#define __NOT_A_DLL
#define __STRING_INTERNALS

#include "zklObject.h"
#include "zklNumber.h"
#include "zklString.h"

static ZKL_Object  VoidObject;
static Instance   *voidText;
static Instance    theOnlyVoidObject, _voidPlus, _voidRecurse;
static Instance    _voidVoid, _voidStop, _voidSkip, _voidWrite, _voidRead;
static Instance    _voidDrop, _voidAgain, _voidFilter, _voidXplode;

//int	  voidID;

Instance *Void       = &theOnlyVoidObject;
Instance *VoidPlus   = &_voidPlus;

Instance *VoidVoid    = &_voidVoid;	// user space thing
Instance *VoidSkip    = &_voidSkip;	// user space thing
Instance *VoidStop    = &_voidStop;	// user space thing
Instance *VoidWrite   = &_voidWrite;	// user space thing
Instance *VoidRead    = &_voidRead;	// user space thing
Instance *VoidDrop    = &_voidDrop;	// user space thing
Instance *VoidAgain   = &_voidAgain;	// user space thing
Instance *VoidFilter  = &_voidFilter;	// user space thing
Instance *VoidXplode  = &_voidXplode;	// user space thing
Instance *VoidRecurse = &_voidRecurse;	// user space thing

Instance *voidCreate(void) { return Void; }	// for DLLs

   // Important! Void doesn't create any methods or properties that
   // Object doesn't have. This lets Void be a proxy for Object
   // Which I break in the case of Properties

/* ******************************************************************** */
/* *************************** Methods ******************************** */
/* ******************************************************************** */

    // Void*(Void) --> VoidVoid, Void*() --> self
static Instance *Void_create(Instance *self,pArglist arglist,pVM vm)
{
   if (Void == arglistTryToGet(arglist,0)) return VoidVoid;
   return self;
}

static Instance *Void_toString(Instance *self,pArglist arglist,pVM vm)
{
#if 1
   typedef struct{ Instance *v; char *nm; } VTable;
   VTable table[] = {
      VoidPlus,  "Void.Plus",  VoidVoid, "Void.Void",  VoidSkip,  "Void.Skip",
      VoidStop,  "Void.Stop",  VoidRead, "Void.Read",  VoidWrite, "Void.Write",
      VoidDrop,  "Void.Drop",  VoidAgain,"Void.Again", VoidFilter,"Void.Filter",
      VoidXplode,"Void.Xplode", VoidRecurse,"Void.Recurse",
      0,0
   };
   if (self->iflag)
   {
      VTable *ptr = table;
      for(ptr = table; ptr->v; ptr++)
         if(self==ptr->v) return kStringCreate(ptr->nm,0,I_OWNED,vm);
      return kStringCreate("Void.?",0,I_OWNED,vm);
   }
   return voidText;
#else
   if (self->iflag)
   {
      if (VoidPlus   == self) return kStringCreate("Void.Plus",   0,I_OWNED,vm);
      if (VoidVoid   == self) return kStringCreate("Void.Void",   0,I_OWNED,vm);
      if (VoidSkip   == self) return kStringCreate("Void.Skip",   0,I_OWNED,vm);
      if (VoidStop   == self) return kStringCreate("Void.Stop",   0,I_OWNED,vm);
      if (VoidRead   == self) return kStringCreate("Void.Read",   0,I_OWNED,vm);
      if (VoidWrite  == self) return kStringCreate("Void.Write",  0,I_OWNED,vm);
      if (VoidDrop   == self) return kStringCreate("Void.Drop",   0,I_OWNED,vm);
      if (VoidAgain  == self) return kStringCreate("Void.Again",  0,I_OWNED,vm);
      if (VoidFilter == self) return kStringCreate("Void.Filter", 0,I_OWNED,vm);
      if (VoidXplode == self) return kStringCreate("Void.Xplode", 0,I_OWNED,vm);
      if (VoidRecurse== self) return kStringCreate("Void.Recurse",0,I_OWNED,vm);
      return kStringCreate("Void.?", 0,I_OWNED,vm);
   }
   return voidText;
#endif
}

#if 0
   // Void.Stop.is(Void.Void)
static Instance *Void_is(Instance *self,pArglist arglist,pVM vm)
{
   return (self==arglistGet(arglist,0,"Void.is",vm)) ? BoolTrue : BoolFalse;
}
#endif

static const MethodTable methods[] = 
{
   "create",	Void_create,
   "toBool",	Bool_nope,
   "toString",	Void_toString,
   0,		0
};

/* ******************************************************************** */
/* *************************** Properties ***************************** */
/* ******************************************************************** */

static Instance *Void_Skip(   Instance *_, pVM vm) { return VoidSkip;    }
static Instance *Void_Stop(   Instance *_, pVM vm) { return VoidStop;    }
static Instance *Void_Void(   Instance *_, pVM vm) { return VoidVoid;    }
static Instance *Void_Read(   Instance *_, pVM vm) { return VoidRead;    }
static Instance *Void_Write(  Instance *_, pVM vm) { return VoidWrite;   }
static Instance *Void_Drop(   Instance *_, pVM vm) { return VoidDrop;    }
static Instance *Void_Again(  Instance *_, pVM vm) { return VoidAgain;   }
static Instance *Void_Filter( Instance *_, pVM vm) { return VoidFilter;  }
static Instance *Void_Xplode( Instance *_, pVM vm) { return VoidXplode;  }
static Instance *Void_Recurse(Instance *_, pVM vm) { return VoidRecurse; }

static const PropertyTable properties[] = 
{
   "Skip",	Void_Skip,
   "Stop",	Void_Stop,
   "Void",	Void_Void,
   "Read",	Void_Read,
   "Write",	Void_Write,
   "Drop",	Void_Drop,
   "Again",	Void_Again,
   "Filter",	Void_Filter,
   "Xplode",	Void_Xplode,
   "Recurse",	Void_Recurse,
   0,		0
};

/* ******************************************************************** */
/* ***************************** Op Codes ***************************** */
/* ******************************************************************** */

Instance *Void_eq(Instance *self,Instance *X,pVM vm)
   { return (TYPEO(X) == VoidType) ? BoolTrue : BoolFalse; }

Instance *Void_neq(Instance *self,Instance *X,pVM vm)
   { return (TYPEO(X) == VoidType) ? BoolFalse : BoolTrue; }

static const OpcodeTable opcodes[] = 
{
   OP_EQ,	(pOp)Void_eq,
   OP_NEQ,	(pOp)Void_neq,
   0,		0
};

//static pProperty in_void_properties(Instance *ignore, register char *str);

static ZKL_KString vText;

static void voider(Instance *v,...)
{
   va_list  ap;		// argument list pointer
   unsigned n;
   va_start(ap,v);
      for (n = 0; v; v = va_arg(ap, Instance *),n++)
      {
	 instanceInit(v,&VoidObject,I_UNTOUCHABLE);
	 v->iflag = 1;
      }
   va_end(ap);
}

void voidConstruct(void)
{
   constructObject(&VoidObject,VoidType,methods,properties,opcodes,NoVM);
   VoidObject.threadSafe     = 1;
//   VoidObject.propertySearch = in_void_properties;

//   voidID = VoidObject.id;

	// Void is not a allocated instance, and thus is immortal
   instanceInit(Void,      &VoidObject,I_UNTOUCHABLE);
   voider(VoidPlus,VoidVoid, VoidSkip,  VoidStop,  VoidRead,   VoidWrite,
	  VoidDrop,VoidAgain,VoidFilter,VoidXplode,VoidRecurse,
	  (Instance *)0);

   voidText = kStringInit(&vText,"Void");	// keep this out of the heap
}


///////////////////////////////////////////////////////////
// Property table
// zkl extractTable -p < void.c | gperf | zkl gperf -i void

