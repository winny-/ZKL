/* sfmt.c : kinda really like sprintf
 * %[+-0,# ][W][.P]][?bcdfsx%]
 *   - : Left justify, with blanks filling right side of the field
 *   0 : Pad width with leading zeros (numbers). Minus is ignored for width
 *       calculation.
 *   + : Always print a "+" or "-" for numbers
 *   , : Add commas to %d, "|" for binary or hex
 *   " " : If number is non negative, prepend a space
 *   W : Minimum field width. Doesn't chop arg.
 *   P : Precision.  May be "*".
 *       If used with numbers, leading zeros are added as needed to fill the
 *         width.
 *       If used with strings, it is the max # of characters.
 *       If Int, the mininum of digits, padded with leading zeros
 * Format characters:
 *   ?  : toString
 *   b  : bool
 *   B  : base N, where N is the precision
 *   c  : character/byte
 *   d  : integer
 *   f  : float. A precision suppresses the decimal point
 *   F  : same as f but with different width semantics
 *   e,E:
 *   g,G:
 *   s  : string
 *   x  : hex int
 *   I  : ignore this parameter
 *   %% : %
 * 
 * There is no limit on the size of the resulting string (unlike printf).
 * Space is allocated as needed.
 * 
 * C Durland 3/06 rewritten from spoof	Public Domain
 * C Durland 8/87 modified  from spewf	Public Domain
 * craigd@zenkinetic.com
 */

#define _CRT_SECURE_NO_WARNINGS		// VC++ 2008

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
//#include <stdarg.h>

#define __NOT_A_DLL
#define __LIST_INTERNALS
#define __STRING_INTERNALS

#include "zklObject.h"
#include "zklList.h"
#include "zklNumber.h"
#include "zklString.h"

    /* I had thought that since malloc is so very slow, avoiding it at all
     * costs would be a very good thing so I tried an experiment with double
     * buffering:  moving text though a fixed buffer and, if that fills up,
     * into malloc()d space.  But testing with the test suite shows no
     * difference in speed or space, even if the buffer is really small.  Go
     * figure.
     * The test suite almost never overflows the initial buffer so the only
     * malloc is usually in textClose().
     * Input:
     *   Pointer to your ZBText
     * Output: Result is a collectable String or an exception
     * GC will take care of things if your code throws an exception
     */
Instance *zbtextInit(ZBText *self, pVM vm)
{
   self->bufSize = 0;
   self->string  = emptyString;
   mlistBuild(self->cya,Void,Void,ZNIL);  // fence this, hold allocated string
   return (Instance *)self->cya;
}

    // initialize buffer with a String
Instance *zbtextInit2(ZBText *self, Instance *text, pVM vm)
{
   Instance *i  = zbtextInit(self,vm);
   self->string = TUPLE_TABLE(self->cya)[0] = text;
   return i;
}

    // Append size characters of text
    // If text is a ptr into some object, it may need a fence
void zbtextAppendN(ZBText *self, char *text, size_t size, pVM vm)
{
   size_t    availableSpace;
   char	    *buffer = self->buffer;
   size_t    n      = self->bufSize;
   Instance *string = self->string;

   if (!size) return;

   do	// Assume size is small, 32k long lines are going to suck
   {
      availableSpace = ZBTEXT_BUF_SIZE - n;
      if (size < availableSpace)	// text fits in buffer
      {
	 memcpy(buffer + n,text,size);
	 self->bufSize += size;
	 break;
      }
      	// chunk text through buffer into a String
      memcpy(buffer + n,text,availableSpace);
      buffer[ZBTEXT_BUF_SIZE] = '\0';
      size -= availableSpace; text += availableSpace;
      self->bufSize = n = 0;

      string = stringCat(vm,ZKL_STRING(string),buffer,(char *)0); // can GC
      TUPLE_TABLE(self->cya)[0] = string;
   } while(size);
   self->string = string;
}

void zbtextAppend(ZBText *self, char *text, pVM vm)
   { zbtextAppendN(self,text,strlen(text),vm); }

#if 1
void zbtextAppendX(ZBText *self,pVM vm, char *text,...)
{
   va_list ap;		// argument list pointer

   zbtextAppend(self,text,vm);
   va_start(ap,text);
      while((text = va_arg(ap,char *))) zbtextAppend(self,text,vm);
   va_end(ap);
}
#endif

void zbtextAppendI(ZBText *self, Instance *text, pVM vm)
{
   char *ptr;
   text = TUPLE_TABLE(self->cya)[1] = iToString(text,vm);	// might alloc
   ptr  = ZKL_STRING(text);
   zbtextAppendN(self,ptr,strlen(ptr),vm);
   TUPLE_TABLE(self->cya)[1] = Void;
}

Instance *zbtextFlush(ZBText *self, pVM vm)
{
   char     *buffer = self->buffer;
   size_t    n      = self->bufSize;
   Instance *i;

   if (n)
   {
      buffer[n] = '\0';
      i = stringCat(vm,ZKL_STRING(self->string),buffer,(char *)0);  // GCs
      self->string = TUPLE_TABLE(self->cya)[0] = i;
   }
   self->bufSize = 0;
   // leave result in self->string
   return self->string;
}

Instance *zbtextClose(ZBText *self, pVM vm)	// soft close
{
   Instance *i = zbtextFlush(self,vm);
   return i;
}


#define BLANKS "                                                              "
#define ZEROS  "000000000000000000000000000000000000"

#define LEADING_ZEROS	0x01
#define PREPEND_SPACE	0x02
#define PREPEND_SIGN	0x04
#define TRUNCATE	0x08
#define LEFT_JUSTIFY	0x10

static void pad(ZBText *buf, char *filler, size_t len, pVM vm)
{
   size_t	fillerLen = strlen(filler);
   while (fillerLen < len)
   {
      zbtextAppendN(buf,filler,fillerLen,vm);
      len -= fillerLen;
   }
   zbtextAppendN(buf,filler,len,vm);
}

static char firstChar(ZBText *buf,char **ptr,int *len, int flags, pVM vm)
{
   char *string = *ptr;
   char	 out    = '\0';

   if (*string == '-' || *string == '+')
   {
       out = *string;
      *ptr = string + 1; (*len)--;
   }
   else if (flags & PREPEND_SIGN)  out = '+';
   else if (flags & PREPEND_SPACE) out = ' ';

   return out;
}

static int adjustWidthBy(char *string, int flags)
{
   if (*string == '-' || *string == '+') return 0;
   if (flags & (PREPEND_SIGN | PREPEND_SPACE)) return 1;
   return 0;
}

     // A nonZero width == fixed field size
static void append2Text(ZBText *buf, char *string, int width, int flags, pVM vm)
{
   extern size_t utf8Len(UChar *utf,pVM vm);	// string.c

   char	c;
   int  len = (int)strlen(string);
//   int  len8 = (int)utf8Len((UChar *)string,vm);  // nukes compiler on bad UTF8

   if (width == 0 || len == width)
   {
      c = firstChar(buf,&string,&len,flags,vm);
      if (c) zbtextAppendN(buf,&c,1,vm);
      zbtextAppendN(buf,string,len,vm);
   }
   else		// pad fill a fixed size field
   {
      width -= adjustWidthBy(string,flags);

      if (width < len)	// string too long to fit in field
      {
	 c = firstChar(buf,&string,&len,flags,vm);
	 if (c) zbtextAppendN(buf,&c,1,vm);
	 if (flags & TRUNCATE) zbtextAppendN(buf,string,width,vm);
	 else		       zbtextAppendN(buf,string,len,vm);
      }
      else
      {
	 int padLen = width - len;
	 c = firstChar(buf,&string,&len,flags,vm);

		// right justify with blanks?
	 if (0 == (flags & (LEFT_JUSTIFY | LEADING_ZEROS)))
	 {
	    pad(buf,BLANKS,padLen,vm);
	    if (c) zbtextAppendN(buf,&c,1,vm);
	    zbtextAppendN(buf,string,len,vm);
	    return;
	 }

	 if (c) zbtextAppendN(buf,&c,1,vm);

	 if (flags & LEFT_JUSTIFY)
	 {
	    zbtextAppendN(buf,string,len,vm);
	    pad(buf,BLANKS,padLen,vm);
	 }
	 else
	 {
		// 12 --> [+]00012, -12.3 --> -00012.3, 12 --> +12
//	    if (flags & LEADING_ZEROS) pad(buf,ZEROS,padLen,vm);
	    pad(buf,ZEROS,padLen,vm);
	    zbtextAppendN(buf,string,len,vm);
	 }
      }
   }
}

//char *decimalMark  =".";	// or ','  ? I use libc?
//char *thousandsMark=",";	// or "_" or "." or UTF-8
// code in number.c to look at locale
// so BigNum can access

    // "1234" --> "1,234"
    // In place: Shift text right and insert commas
    // !!!??? 1000 to 9999 don't add commas?
    // https://en.wikipedia.org/wiki/Decimal_mark
char *fmtCommaize(char *buf,int spacing,char thousandsMark)
{
   char *ptr, *ptr2;
   int	 n,z,len = (int)strlen(buf);

   z = (len - 1 - (*buf == '-')) / spacing;   // the number of commas needed
   if (z <= 0) return buf;

   ptr = buf + len; ptr2 = ptr + z; n = spacing + 1;
   while(z--)
   {
      while(n--) *ptr2-- = *ptr--;
      *ptr2-- = thousandsMark;
      n = spacing;
   }
   return buf;
}

static char *percentDBX(ZBText *zb,     // %d, %B, %x
   Instance *d,int base,int commas,int cat,char thousandsMark, char *buf,pVM vm)
{
   if (
     #if USE_POINTER_INTS
     IS_PtrInt(d) || 
     #endif
     TYPEO1(d) != NativeType)
   {				// no GC
      int64_t i64;
   treatAsInt:
      i64 = convertToInt(d,vm);
      (base == 10) ? intToA(i64,buf) : i64ToBaseB(i64,buf,base,vm);
      if (commas) fmtCommaize(buf,cat,thousandsMark);
      return buf;
   }
   else	// Something I don't know about, like BigNum, might GC or throw
   {
      Instance *i;
      pMethod   method = 0;   // VC14
      MLIST(mlist,3);	// d.toIntString(base [,commasAt,c])

#if 0
      objectResolve(d,"toIntString",&method,1,vm);
      if (!method) goto treatAsInt;
#else
      i = d;
      if(MethodType!=objectResolve(&i,"toIntString",&method,0,1,vm)) 
	 goto treatAsInt;
#endif
      mlistBuild(mlist,intCreate(base,vm),ZNIL);
      if (commas)
      {
	 char cbuf[3];
	 *cbuf = thousandsMark; cbuf[1] = '\0';
	 mlistAppendI(mlist,intCreate(cat,vm),3);
	 mlistAppendI(mlist,kStringCreate(cbuf,0,I_OWNED,vm),3);
      }

      i = method(d,(Instance *)mlist,vm);   // d.toIntString(base,commasAt,c)
//      zb->aFence.i2 = i;
      return ZKL_STRING(i);
   }
}

    // sfmt: My version of printf
    // Returns a collectable String or throws
Instance *sfmt(char *format, pArglist args, pVM vm)
{
   char	     aChar[2], c;
   char	     tmp[700];  // big enough to hold 64 bits + commas & sign base2 |
   char	    *ptr;
   Instance *i;
   int	     n, width, precision, defaultPrecision, commas;
   int	     flags;
   Fence     fence;
   ZBText    buf;

   vmSetFence(vm,&fence,0,zbtextInit(&buf,vm));
   aChar[1] = '\0';
   n = 0;
   while (*format)
   {
      if (*format != '%') zbtextAppendN(&buf,format,1,vm);
      else	//  %
      {
	 width = commas = 0;
	 defaultPrecision = 1; precision = 6;
	 flags = 0;
      more:		// process some more of '%'
	 switch (c = *++format)
	 {
	    case '%':	// %%
	       zbtextAppendN(&buf,format,1,vm);
	       break;
	    case '-': flags |= LEFT_JUSTIFY;  goto more;
	    case '+': flags |= PREPEND_SIGN;  goto more;
	    case ' ': flags |= PREPEND_SPACE; goto more;
	    case ',': commas = 1;	      goto more;
	    case '_': commas = 2;	      goto more;
	    case 'c':
	    {
	       Instance *i = arglistGet(args,n++,".fmt",vm);

	       flags &= ~(PREPEND_SPACE | PREPEND_SIGN);
	       switch (TYPEO(i))
	       {
		  case StringType:
		     ptr = ZKL_STRING(i);
		     if (strlen(ptr)) aChar[0] = *ptr;
		     else
		     {
			sprintf(tmp,"fmt(%%c): string is empty");
			vmThrow(vm,E_VALUE_ERROR,tmp);
		     }
		     break;
		  case IntType:
		     aChar[0] = (char)convertToInt(i,vm);
		     break;
		  default:
		     sprintf(tmp,"String.fmt(%%c): Invalid type: %s",iname(i));
		     vmThrow(vm,E_TYPE_ERROR,tmp);
	       }
	       append2Text(&buf,aChar,width,flags,vm);
	       break;
	    }
	    case 'b':	// Bool
	       i   = arglistGetT(args,n++,TO_BOOL,".fmt",vm);
	       ptr = ZKL_STRING(M2_STRING(i,vm));   // no mallocs
	       append2Text(&buf,ptr,width,flags,vm);
	       break;
	    case 'B':	// base
	       if (precision != 2) commas = 0;	// binary only
	       ptr = percentDBX(&buf,arglistGet(args,n++,".fmt",vm),precision,
				commas,4,'|',tmp,vm);
	       append2Text(&buf,ptr,width,flags,vm);
	       break;
	    case 'd': 	// decimal aka Int
	       ptr = percentDBX(&buf,arglistGet(args,n++,".fmt",vm),10,
//				commas,3,',',tmp,vm);
				commas,3, (commas==1) ? ',' : '_', tmp,vm);
	       append2Text(&buf,ptr,width,flags,vm);
	       break;
	    case 'x':	// hex
	       ptr = percentDBX(&buf,arglistGet(args,n++,".fmt",vm),16,
				commas,2,'|',tmp,vm);
	       append2Text(&buf,ptr,width,flags,vm);
	       break;
	    case 'I':	// Ignore
	       arglistGet(args,n++,".fmt",vm); //!!!??? verify parameter exists
	       break;
	    case 'e': case 'E':
	       commas = 0;	// %,e never gets commas.
	    case 'f':  // let Float do the "right" thing
	    case 'g': case 'G':
	       i = arglistGetT(args,n++,TO_FLOAT,".fmt",vm);
	       floatToString(i,tmp,precision,c,(commas==1) ? ',' : 0);
	       append2Text(&buf,tmp,width,flags,vm);
	       break;
	    case 'F':  // toFloat: W.P --> W digits "." P digits
	       i = arglistGetT(args,n++,TO_FLOAT,".fmt",vm);
	       floatToString(i,tmp,precision,'f',(commas==1) ? ',' : 0);
	       append2Text(&buf,tmp,width+precision+1,flags,vm);
	       break;
	    case 's':	// toString
	       flags &= ~PREPEND_SIGN;
	       i   = arglistGetString(args,n++,".fmt",vm);
	       ptr = ZKL_STRING(i);
	       if(!*ptr) flags&=~PREPEND_SPACE;		// ""
	       if (defaultPrecision) append2Text(&buf,ptr,width,flags,vm);
	       else	// [W].P
	       {
		  char *p  = ptr;
		  int  len = (int)strlen(ptr);
		  c = *ptr;
		  if (precision < len)
		  {
		      p += precision;
		      c = *p;		// save char
		     *p = '\0';		// blast char
		  }
	       	  append2Text(&buf,ptr,width,flags,vm);
		  *p = c;		// restore char
	       }
	       break;
#if 0
	    case 'L':	// List   ???????? what to use as seperator?
	       listConcat(&buf,arglistGetBT(args,n++,ListType,".fmt",vm),vm);
	       break;
#endif
	    case '0': case '1': case '2': case '3': case '4': case '5':
	    case '6': case '7': case '8': case '9':
	    {
	       c = *format;

	       if (c == '0') { flags |= LEADING_ZEROS; format++; }

	       for (; *format; format++)
	       {
		  c = *format;
		  if ('0' <= c && c <= '9') width = width*10 + c - '0';
		  else break;
	       }
	       if (c == '.')	// precision
	       {
	       getPrecision:
		  defaultPrecision = precision = 0;
		  for (format++; *format; format++)
		  {
		     c = *format;
		     if ('0' <= c && c <= '9')
			precision = precision*10 + c - '0';
		     else break;
		  }
	       }
	       if (*format) { format--; goto more; }
	       break;
	    }
	    case '.':		// eg "%.4s"
	       goto getPrecision;
	    case '\0':		// "%" --> %
	       format--;
	       zbtextAppendN(&buf,format,1,vm);
	       break;
	    default:		 // copy %<char not matched>
	    {
	       zbtextAppendN(&buf,format - 1,2,vm);
	       break;
	    }
	 } // switch
      }	// else

      format++;
   }	// while

   i = zbtextClose(&buf,vm);
   vmRemoveFence(&fence,0);
   return i;
}
