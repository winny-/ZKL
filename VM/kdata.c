/* kdata.c : the ConstData object : A container of constant bytes
 * Read only, would be thread safe if the cursor was.
 * 
 * Copyright (c) 2006,7,8,9,10,2011 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#include <string.h>

#define __DATA_INTERNALS
#define __NOT_A_DLL

#include "zklObject.h"
#include "zklData.h"
#include "zklList.h"
#include "zklNumber.h"
#include "zklString.h"
#include "zklUtil.h"

    // Thread safety: No problem. Well, except for the cursor

static ZKL_Object KDataObject;

#define D_BITS(d)	( ((KData *)d)->bits )
#define D_SIZE(d)	( ((KData *)d)->size )
#define D_CURSOR(d)	( ((KData *)d)->cursor )
#define D_TREAT_AS(d)	( ((KData *)d)->treatAs )

static KData  _emptyKData;
static Byte   emptybits[1] = { 0 };
Instance     *emptyKData = (Instance *)&_emptyKData;

Instance *getEmptyKData(void) { return emptyKData; }

static void kdataMarker(Instance *_self)
{
   KData *self = (KData *)_self;
   if (self->container) instanceMark(self->container); 
}

static IBucketHeader *kdBuckets;

static int kdataFree(Instance *_self)
{
   KData *self = (KData *)_self;
   if (self->flags == 1) free(self->bits);
   self->bits = 0;	// just in case
   return 1;
}

    // Asm.AsmWalker uses a ton of these for String.walker(1)
    //   since the compiler uses strings to hold asm code.
    // If container == 1, this is an external something that was malloc()ed,
    //   and wants to hook into gc.
Instance *
kdataCreate(Byte *bits,size_t size,Instance *container,int itype,pVM vm)
{
   KData *kd;
   int    flags = 0;

   if (!bits || !size) return emptyKData;

   if (container == (Instance *)1) { flags = 1; container = 0; }

   kd = (KData *)ibucketAllocate(kdBuckets,&KDataObject,itype,1,vm);
   kd->instance.iflag = 1;
   kd->size	= (DSize_t)size;
   kd->cursor	= 0;
   kd->treatAs	= DATA_TREAT_AS_DATA;
   kd->bits	= bits;
   kd->container= container;
   kd->flags	= flags;
   kd->z	= !bits[size-1];	// I has zero?
   kd->howza	= 0;

   if (container && TYPEO1(container) == StringType)
   {
      kd->treatAs = DATA_TREAT_AS_STRINGS;
      kd->z	 = 1;
   }

   return containerIsCooked(kdBuckets,(Instance *)kd,itype);
}

/* ******************************************************************** */
/* *************************** Methods ******************************** */
/* ******************************************************************** */

	// KData.copy()
static Instance *KData_copy(KData *self,pArglist arglist,pVM vm)
   { return dataCreate2(self->bits,self->size,vm); }

static const MethodTable methods[] = 
{
   "copy",		(pMethod)KData_copy,
   "toData",		(pMethod)Object_noop,

   "toBigEndian",	(pMethod)Data_toBigEndian,
   "toLittleEndian",	(pMethod)Data_toLittleEndian,
   "toBool",		(pMethod)Data_toBool,
   "toList",		(pMethod)Data_toList,
   "toString",		(pMethod)Data_toString,

   "len",		(pMethod)Data_len,
   "close",		(pMethod)Object_noop,	// Stream
   "flush",		(pMethod)Object_noop,	// Stream

   "read",		(pMethod)Data_read,
   "readln",		(pMethod)Data_readln,
   "readString",	(pMethod)Data_readString,
   "readNthLine",	(pMethod)Data_readNthLine,
   "__sGet",		(pMethod)Data_sGet,
   "get",		(pMethod)Data_sGet,
   "charAt",		(pMethod)Data_charAt,

   "bytes",		(pMethod)Data_bytes,

   "seek",		(pMethod)Data_seek,
   "inlineCursor",	(pMethod)Data_inlineCursor,

   "mode",		(pMethod)Data_mode,
   "howza",		(pMethod)Data_howza,

   "find",		(pMethod)Data_find,
   "findString",	(pMethod)Data_findString,
   "index",		(pMethod)Data_index,

   "walker",		Data_walker,
   "reduce",		Data_reduce,
   "pump",		Data_pump,
   "filter",		Data_filter,
   0,			0
};

/* ******************************************************************** */
/* *************************** Properties ***************************** */
/* ******************************************************************** */

    // .text --> String
static Instance *KData_text(KData *self, pVM vm)
{
   Byte	    *bits      = D_BITS(self);
   Instance *container = self->container;
   size_t    size      = D_SIZE(self);

   if (!size) return emptyString;
   if (container && TYPEO1(container) == StringType) return container;
   if (bits[size-1])	// no terminating '\0'
      return stringCreate2((char *)bits,size,vm);
   return kStringCreate((char *)bits,(Instance *)self,I_OWNED,vm);
}

    // .bits -->?address
static Instance *KData_bits(KData *self, pVM vm)
   { return intCreate((size_t)D_BITS(self),vm); }

static const PropertyTable properties[] = 
{
   "text",		(pProperty)KData_text,
   "cursor",		(pProperty)Data_cursor,
   "bits",		(pProperty)KData_bits,
   "isReadOnly",	(pProperty)Bool_soTrue,
   0,			0
};


/* ******************************************************************** */
/* ***************************** Op Codes ***************************** */
/* ******************************************************************** */

static const OpcodeTable opcodes[] = 
{
   OP_EQ,	(pOp)Data_eq,
   OP_NEQ,	(pOp)Data_neq,
   0,		0
};

//static pMethod in_kdata_methods(Instance *ignore, register char *str);

void kdataConstruct(void)
{
   static IBucketHeader _kdBuckets;	// might not be used

   constructObject(&KDataObject,DataType, methods, properties, opcodes, NoVM);
   KDataObject.name	    = "ConstData";
   KDataObject.magicMarker  = kdataMarker;
//   KDataObject.methodSearch = in_kdata_methods;
   KDataObject.freeMe       = kdataFree;
   KDataObject.isize	    = sizeof(KData);
   KDataObject.threadSafe   = 0;	// cursor isn't
   KDataObject.isBInstance  = 1;

	// share w/CuckooTuple
//   kdBuckets = ibucketHitchHike(&KDataObject,8,1030,&_kdBuckets,NoVM);
   kdBuckets = ibucketReserve(&KDataObject,1533,&_kdBuckets,0,NoVM);

   memset(&_emptyKData,0,sizeof(KData));
   instanceInit((Instance *)&_emptyKData,&KDataObject,I_UNTOUCHABLE);
   _emptyKData.bits  = emptybits;	// one byte of zero == ""
   _emptyKData.z     = 1;
   emptyKData->iflag = 1;
   dataMode(emptyKData,DATA_TREAT_AS_STRINGS,NoVM);  // "".walker(1)
}



///////////////////////////////////////////////////////////////////
// zkl extractTable.zkl < kdata.c | gperf | zkl gperf.zkl -i kdata


