/* regex.h : Regular Expressions, regex.c
 * 
 * Copyright (c) 2006,2007,2008-12,2013 Craig Durland craigd@zenkinetic.com
 * License: The zlib/libpng License,
 * 	http://www.opensource.org/licenses/zlib-license.php
 */

#ifndef __REGEX_H
#define __REGEX_H

#include <stdint.h>

#ifndef Byte
typedef unsigned char	UChar;
typedef uint8_t		Byte;	// or UChar if you don't have <stdint.h>
#endif


#define RE_MAXTAG	 20		// max number of ( tags: 10 is normal

char *regExpCompile(UChar *pattern, UChar dfa[], size_t *dfaSz);
int   regExpExec(UChar *dfa, UChar *textToSearch,
              int SoL, int move, UChar *tags[], void *vm);
void  regExpFail(char *msg, UChar op, void *vm);
void  dfaDump(Byte *dfa, int showSz);

#endif // __REGEX_H
