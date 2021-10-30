/* zklData.h : Header file for Data and KData
 * 
 * Copyright (c) 2006,7,8,9,10,2011 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#ifndef __ZKL_DATA_H
#define __ZKL_DATA_H

// #include "zklObject.h" implied here

typedef Instance *pData;   // Since Data & KData are supposed to be opaque

#define DATA_TREAT_AS_DATA	0
#define DATA_TREAT_AS_STRINGS	1

#ifdef __DATA_INTERNALS

    // 32 bits max on data size. 16 bits also works (uint16_t)
typedef uint32_t DSize_t;    // 32 bits for Data

#define DNUM_MARKS	10
typedef struct	// not thread safe
{
   BInstance instance;
   	//////////////// Overlap with KData
   unsigned int treatAs:1, z:1;
   unsigned int howza:5, howzaTmp:5, startAtCursor:1;
   DSize_t   len;		// bytes of allocatedSpace used
   DSize_t   cursor;
   Byte	    *bits;
   	/////////////////////////
   DSize_t   marks[DNUM_MARKS];	// settable offsets into bits
   DSize_t   allocatedSpace, gapStart, gapSize;
}Data;	// Linux/64: 80, Win10/32: ??

typedef struct	// not thread safe
{
   BInstance  instance;		// iflag is set
   	//////////////// Overlap with Data
   unsigned int treatAs:1, z:1, howza:5;
   DSize_t   size;		// number of bits
   DSize_t   cursor;		// for read operations
   Byte	    *bits;
   	////////////////////////
   unsigned int flags:8;
   Instance *container;		// where the bits live
}KData;	// Linux/64: 40, Win10/32: ??

#endif	// __DATA_INTERNALS

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __NOT_A_DLL
extern DllExport Instance *emptyKData, *VData;

void	  dataConstruct(void), kdataConstruct(void);

Instance *Data_bytes(Instance *,pArglist,pVM);
Instance *Data_cursor(Instance *self,pVM);
Instance *Data_len(Instance *,pArglist,pVM);
Instance *Data_sGet(Instance *,pArglist,pVM);
Instance *Data_charAt(Instance *,pArglist,pVM);
Instance *Data_toBigEndian(Instance *,pArglist,pVM);
Instance *Data_toLittleEndian(Instance *,pArglist,pVM);
Instance *Data_toBool(Instance *,pArglist,pVM);
Instance *Data_toList(Instance *,pArglist,pVM);
Instance *Data_mode(Instance *,pArglist,pVM);
Instance *Data_howza(Instance *,pArglist,pVM);
Instance *Data_seek(Instance *,pArglist,pVM);
Instance *Data_inlineCursor(Instance *,pArglist,pVM);
Instance *Data_toString(Instance *,pArglist,pVM);
Instance *Data_readString(Instance *,pArglist,pVM);
Instance *Data_readln(Instance *,pArglist,pVM);
Instance *Data_read(Instance *,pArglist,pVM);
Instance *Data_readNthLine(Instance *,pArglist,pVM);
Instance *Data_find(Instance *,pArglist,pVM);
Instance *Data_index(Instance *,pArglist,pVM);
Instance *Data_findString(Instance *,pArglist,pVM);
Instance *Data_walker(Instance *,pArglist, pVM);
Instance *Data_reduce(Instance *,pArglist, pVM);
Instance *Data_pump(Instance *,pArglist, pVM);
Instance *Data_filter(Instance *,pArglist, pVM);

Instance *Data_eq(Instance *,Instance *X,pVM);
Instance *Data_neq(Instance *,Instance *X,pVM);
#endif	// __NOT_A_DLL


	///////////////////////////// Data
DllExport Instance *Data_create(Instance *,pArglist,pVM);
DllExport Instance *dataCreate(size_t size, int itype, pVM);
DllExport Instance *dataCreate2(Byte *bytes, size_t size, pVM);
DllExport Byte	   *dataText(Instance *,size_t *len);
DllExport void	   *dataWillFill(Instance *);
DllExport void	    dataSetSize(Instance *, size_t size);
DllExport void	    dataMode(Instance *,int mode, pVM);
DllExport char	   *dataNextString(Instance *, size_t *lineLen);
DllExport char	   *dataNextLine(  Instance *, int strip, size_t *lineLen);
DllExport void	    dataSeek(Instance *,Offset_t, pVM);
DllExport size_t    dataInsertBytes(Instance *, Offset_t n,
				Byte *, size_t size, int append, pVM);
DllExport void	    dataAppend(Instance *self, Instance *x, pVM);
DllExport size_t    dataLen(Instance *);
DllExport int	    dataGetMode(Instance *);
DllExport Instance *Data_isType(Instance *,pArglist,pVM);

DllExport Byte	   *dataChunkAt(Instance *,size_t offset,size_t size);

DllExport size_t    dataCursorPos(Instance *);
DllExport size_t    dataCursorSet(Instance *, size_t);
DllExport size_t    dataSize(Instance *);
DllExport size_t    dataSpace(Instance *);

DllExport Instance *Data_clear(Instance *,pArglist,pVM);


	///////////////////////////// KData
DllExport Instance *
	kdataCreate(Byte *,size_t,Instance *container,int itype,pVM);
DllExport Instance *getEmptyKData(void);

#ifdef __cplusplus
};		// end of extern "C"
#endif

#endif // __ZKL_DATA_H
