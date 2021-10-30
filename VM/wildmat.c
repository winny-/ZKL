/*
**  Do shell-style pattern matching for ?, \, [], and * characters.
**  Might not be robust in face of malformed patterns; e.g., "foo[a-"
**  could cause a segmentation violation.  It is 8bit clean.
**
**  Written by Rich $alz, mirror!rs, Wed Nov 26 19:03:17 EST 1986.
**  Rich $alz is now <rsalz@bbn.com>.
**  Special thanks to Lars Mathiesen <thorinn@diku.dk> for the ABORT code.
**  This can greatly speed up failing wildcard patterns.  For example:
**	pattern: -*-*-*-*-*-*-12-*-*-*-m-*-*-*
**	text 1:	 -adobe-courier-bold-o-normal--12-120-75-75-m-70-iso8859-1
**	text 2:	 -adobe-courier-bold-o-normal--12-120-75-75-X-70-iso8859-1
**  Text 1 matches with 51 calls, while text 2 fails with 54 calls.  Without
**  the ABORT, then it takes 22310 calls to fail.  Ugh.
**
**  C Durland added:
**    Implemented "[-]", "[]]",  "[]...-]"
**    Put man page at the end of this file.
*/

#include <string.h>
#include <ctype.h>

#define TRUE		 1
#define FALSE		 0
#define ABORT		-1

#define NEGATE_CLASS	'^'

/* Forward declaration. */
static int DoMatch(char *text, char *p, int flags);

/*
**  See if the text matches the p, which has an implied leading asterisk.
*/
static int Star(char *text, char *p, int flags)
{
   int	ret;

   do ret = DoMatch(text++,p,flags); while (ret == FALSE);
   return ret;
}


/*
**  Match text and p, return TRUE, FALSE, or ABORT.
**  Flag bits: 1 (ignore case), 2 (restricted quote)
*/
static int DoMatch(char *text, char *p, int flags)
{
   int ignoreCase = (flags & 1), restrictedQuote = (flags & 2);
   for (; *p; text++, p++)
   {
      if (*text == '\0' && *p != '*') return ABORT;
      switch (*p)
      {
	 case '\\': p++;	/* Literal match with following character. */
		// uggg, Windows, restrict "\" to only "?*[]"
	    if (restrictedQuote && !strchr("?*[]",*p)) p--;
	    if (*text != *p) return FALSE;
	    break;
	 default:
	    if (ignoreCase)
		{ if (tolower(*text) != tolower(*p)) return FALSE; }
	    else if (*text != *p) return FALSE;
	    continue;
	 case '?': continue;	/* Match anything. */
	 case '*':		/* Trailing star matches everything. */
	    return *++p ? Star(text,p,flags) : TRUE;
	 case '[':	// special: [-], [...-], []], []...], [-, [^
	 {
	    int  last, matched, reverse, c1,c2;

	    if ((reverse = (p[1] == NEGATE_CLASS))) // Inverted character class?
	       p++;	// yes
	    #if 1	// make [-] match "-", []] match "]"
	       matched = FALSE;
	       c2 = p[1];
	       if (c2 == ']' || c2 == '-')	// []..., [-...
	       {
		  p++;
		  matched = (*text == c2);
	       }
	       for (last = 0x100; *++p && *p != ']'; last = *p)
	       {
		  c1 = *p; c2 = p[1];			// c2 might == '\0'
		  if ((c1 == '-' && c2 != ']') ?	// -] matches '-' only
		         p++, *text <= c2 && *text >= last : *text == c1)
		     matched = TRUE;
	       }
	    #else
	       for (last = 0x100, matched = FALSE; *++p && *p != ']'; last = *p)
			/* This next line requires a good C compiler. */
	          if (*p == '-' ? *text <= *++p && *text >= last : *text == *p)
		     matched = TRUE;
	    #endif
	    if (matched == reverse) return FALSE;
	    continue;
	 } // [
      } // swtich
   } // for

   return (*text == '\0');
}


/*
**  User-level routine.  Returns TRUE or FALSE.
**  See DoMatch() for flags.
*/
int wildmat(char *text, char *pattern, int flags)
{
    return DoMatch(text,pattern,flags) == TRUE;
}



#ifdef	TEST
#include <stdio.h>

/* Yes, we use gets not fgets.  Sue me. */
extern char	*gets_s();


main()
{
    char	 pattern[80];
    char	 text[80];

    printf("Wildmat tester.  Enter pattern, then strings to test.\n");
    printf("A blank line gets prompts for a new pattern; a blank pattern\n");
    printf("exits the program.\n\n");

    for ( ; ; ) {
	printf("Enter pattern:  ");
	(void)fflush(stdout);
	if (gets_s(pattern,80) == NULL || pattern[0] == '\n')
	    break;
	for ( ; ; ) {
	    printf("Enter text:  ");
	    (void)fflush(stdout);
	    if (gets_s(text,80) == NULL)
		exit(0);
	    if (text[0] == '\0')
		/* Blank line; go back and get a new pattern. */
		break;
	    printf("      %s\n", wildmat(text, pattern) ? "YES" : "NO");
	}
    }

    exit(0);
    /* NOTREACHED */
}
#endif	/* TEST */



#if 0		/* the man page */

 WILDMAT(3)                                                       WILDMAT(3)


 NAME
      wildmat - perform shell-style wildcard matching

 SYNOPSIS
      int
      wildmat(text, pattern)
          char       *text;
          char       *pattern;

 DESCRIPTION
      Wildmat compares the text against the pattern and returns non-zero if
      the pattern matches the text.  The pattern is interpreted similar to
      shell filename wildcards, and not as a full regular expression such as
      those handled by the grep(1) family of programs or the regex(3) or
      regexp(3) set of routines.

      The pattern is interpreted according to the following rules:

      \x   Turns off the special meaning of x and matches it directly; this
           is used mostly before a question mark or asterisk, and is not
           valid inside square brackets.

      ?    Matches any single character.

      *    Matches any sequence of zero or more characters.

      [x...y]
           Matches any single character specified by the set x...y, where
           any character other than minus sign or close bracket may appear
           in the set.  A minus sign may be used to indicate a range of
           characters.  That is, [0-5abc] is a shorthand for [012345abc].
           More than one range may appear inside a character set; [0-9a-zA-
           Z._] matches almost all of the legal characters for a host name.
	   [-] and [x...y-] match "-"
	   [-y] doesn`t match anything

      [^x...y]
           This matches any character not in the set x...y, which is
           interpreted as described above.

 HISTORY
      Written by Rich $alz <rsalz@bbn.com> in 1986, and posted to Usenet
      several times since then, most notably in comp.sources.misc in March,
      1991.
      Lars Mathiesen <thorinn@diku.dk> enhanced the multi-asterisk failure
      mode in early 1991.

 SEE ALSO
      grep(1), regex(3), regexp(3).


                                    - 1 -       Formatted:  January 17, 1992

#endif
