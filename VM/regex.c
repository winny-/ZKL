/*
 * regex - Regular expression pattern matching and replacement
 *
 * By:  Ozan S. Yigit (oz), Dept. of Computer Science, York University
 * Mods: C Durland craigd@zenkinetic.com
 *
 * These routines are the PUBLIC DOMAIN equivalents of regex routines as
 * found in 4.nBSD UN*X, with minor extensions.
 *
 * These routines are derived from various implementations found in software
 * tools books, and Conroy's grep.  They are NOT derived from
 * licensed/restricted software.  For more interesting/academic/complicated
 * implementations, see Henry Spencer's regexp routines, or GNU Emacs
 * pattern matching module.
 *
 * dfa = deterministic finite automata
 * Routines:
 *  re_comp: compile a regular expression into a DFA.
 *	char *re_comp(UChar *pattern, UChar *dfa)
 *	returns: NULL if OK, else error string
 *  re_exec: execute the DFA to match a pattern.
 *	int re_exec(UChar *dfa, UChar *textToSearch ???)
 *  re_subs: substitute the matched portions in a new string.
 *	int re_subs(char *src,char *dst)
 *  regExpFail: failure routine for re_exec.
 *	void regExpFail(char *msg, char op, void *X)
 *  
 * Regular Expressions:
 *
 *      [1]     char    matches itself, unless it is a special
 *                      character (metachar): . \ [ ] ( ) * + ^ $
 *
 *      [2]     .       matches any character.
 *
 *      [3]     \       matches the character following it, except
 *			when followed by one of: ()123456789<> adnwW
 *			(see [7], [8] and [9])
 *			It is used as an escape character for all other
 *			meta-characters, and itself.  When used in a set
 *			([4]), it is treated as an ordinary character.
 *
 *      [4]     [set]   matches one of the characters in the set.
 *                      If the first character in the set is "^",
 *                      it matches a character NOT in the set. A
 *                      shorthand S-E is used to specify a set of
 *                      characters S upto E, inclusive. The special
 *                      characters "]" and "-" have no special
 *                      meaning if they appear as the first chars
 *                      in the set.
 *                      Example:   Matches:
 *			--------   -------
 *			[a-z]	   Any lowercase alpha.
 *
 *			[^]-]      Any char except ] and -.
 *
 *			[^A-Z]     Any char except uppercase alpha.
 *
 *			[a-zA-Z]   Any alpha.
 *
 *                      [a-b-c] == [a-bb-c] == [a-c]
 *                      [a-a] == [a]
 *			[-abc] == [abc-]  Match -, a, b, c.
 *
 *			[]] == ]   Match "]"
 *			[]-]	   Match only "-]".  This is a set ([-])
 *				   and a character (]).
 *			
 *			[z-a]      Nothing and is an error.
 *
 *      [5]     *       any regular expression form [1] to [4], followed by
 *                      closure char (*) matches zero or more matches of
 *                      that form.
 *
 *      [6]     +       same as [5], except it matches one or more.
 *
 *      [7]    \?	One or none, like +
 *
 *      [8]     (       a regular expression in the form [1] to [11], enclosed
 *                      as (form) matches what form matches. The enclosure
 *                      creates a set of tags, used for [8] and for
 *                      pattern substution. The tagged forms are numbered
 *			starting from 1.
 *
 *      [9]     \1      a \ followed by a digit 1 to 9 matches [verbatum] 
 *			whatever a previously tagged regular expression ([8]) 
 *			matched.
 *
 *	[10]	\<	a regular expression starting with a \< construct
 *		\>	and/or ending with a \> construct, restricts the
 *			pattern matching to the beginning of a word, and/or
 *			the end of a word. A word is defined to be a character
 *			string beginning and/or ending with the characters
 *			A-Z a-z 0-9 and _. It must also be preceded and/or
 *			followed by any character outside those mentioned.
 *
 *      [11]   A|B     	Matches subexpression A, or failing that, matches B.
 *
 *      [12]            a composite regular expression xy where x and y
 *                      are in the form [1] to [11] matches the longest
 *                      match of x followed by a match for y.
 *
 *      [13]	^	a regular expression starting with a ^ character
 *		$	and/or ending with a $ character, restricts the
 *                      pattern matching to the beginning of the line,
 *                      or the end of line. [anchors] Elsewhere in the
 *			pattern, ^ and $ are treated as ordinary characters.
 *
 * Acknowledgements:
 *   HCR's Hugh Redelmeier has been most helpful in various stages of
 *   development.  He convinced me to include BOW and EOW constructs,
 *   originally invented by Rob Pike at the University of Toronto.
 * References:
 *   Software tools		Kernighan & Plauger
 *   Software tools in Pascal	Kernighan & Plauger
 *   Grep [rsx-11 C dist]	David Conroy
 *   ed - text editor		Un*x Programmer's Manual
 *   Advanced editing on Un*x	B. W. Kernighan
 *   RegExp routines		Henry Spencer
 * Notes:
 *  This implementation uses a bit-set representation for character sets for
 *    speed and compactness.  Each character is represented by one bit in a
 *    N-bit block.  Thus, SET or NSET always takes a constant M bytes in the
 *    internal dfa, and re_exec does a single bit comparison to locate the
 *    character in the set.  N is 128 for 7 bits ASCII and 256 for 8 bit
 *    data.  Thus M is 16 or 32 bytes.
 *  Put CLO in front of what gets closed for ease of interpreting.
 *  Put END at end of what gets closed to limit recursion.
 * Examples:
 *	pattern:	foo*.*
 *	compile:	CHR f CHR o CLO CHR o END CLO ANY END END
 *	matches:	fo foo fooo foobar fobar foxx ...
 *
 *	pattern:	fo[ob]a[rz]	
 *	compile:	CHR f CHR o SET bitset CHR a SET bitset END
 *	matches:	fobar fooar fobaz fooaz
 *
 *	pattern:	foo\\+
 *	compile:	CHR f CHR o CHR o CHR \ CLO CHR \ END END
 *	matches:	foo\ foo\\ foo\\\  ...
 *
 *	pattern:	(foo)[1-3]\1	(same as foo[1-3]foo)
 *	compile:	BOT 1 CHR f CHR o CHR o EOT 1 SET bitset REF 1 END
 *	matches:	foo1foo foo2foo foo3foo
 *
 *	pattern:	(fo.*)-\1
 *	compile:	BOT 1 CHR f CHR o CLO ANY END EOT 1 CHR - REF 1 END
 *	matches:	foo-foo fo-fo fob-fob foobar-foobar ...
 */

//#define EXTEND

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "regex.h"	// prototypes for this code

//#define RE_MAXDFA     768   // amount of space for compiled RE. EG, passed in
//#define RE_MAXTAG      10     // max number of ( tags
#define RE_MAXOR      500     // max number of |'s

#define CHR	 1	// character		:: CHR<character>
#define ANY	 2	// .			:: ANY
#define SET	 3	// set: [...]		:: SET bitset
#define NSET	 4	// not set: [^...]	:: SET bitset
#define BOL	 5	// beginning of line: ^ :: BOL
#define EOL	 6	// end of line: $	:: EOL
#define BOT	 7	// beginning of tag: (
#define EOT	 8	// end of tag: )
#define BOW	 9	// beginning of word: \<
#define EOW	10	// end of word: \>
#define REF	11	// tag reference: \1,...,\9 
#define CLO	12	// closure: +, *	:: CLO dfa END
#define DIGIT	13	// \d 	match isdigit()
#define N_DIGIT	14	// \D	match !isdigit()
#define SPACE	15	// \s   match isspace()
#define N_SPACE	16	// \S   match !isspace()
#define ALPHA	17	// \w	match isalnum() & "_" (Alphanumeric)
#define N_ALPHA	18	// \W	match isalnum() (Alphanumeric)
#define POR	19	// |    A|B, (A|B)
#define ONE	20	// \? one or more :: ONE dfa END

#define END	 0

#define IS_ALPHA(c)	(isalnum(c) || c == '_')

/* ******************************************************************** */
/* **************************** Bit Tables **************************** */
/* ******************************************************************** */

/* 
 * Bit table:  a string of bits stored in an array of char
 *     .-----------------------------------.
 *     |01234567|89012345|67890123|45678901|
 *     `-----------------------------------'
 *    bits[0]      [1]      [2]      [3]
 * To find what bucket the nth bit is in (8 bits per bucket):
 *       bit_bucket(n) = bits[n/8]
 *   It might be a good idea to restrict n so it doesn't index outside its
 *     range (only works if number of bits is a power of 2):
 *       n = n & ((max_n - 1) & ~7)  where max_n is a power of 2
 *     The ~7 is just to get rid of the lower bits that won't do anything
 *	 anyway.
 * The nth bit in the bucket is n mod 8 (ie the lower 3 bits of n (0-7) are
 *   the bit position):
 *       bit_flag(n) = 1 << (n & 7)
 * To find the state of the nth bit (0 == off and !0 == on):
 *       bit_bucket(n) & bit_flag(n)
 * To set the nth bit:
 *       bits[bit_bucket(n)] |= bit_flag(n)
 * Notes:
 *   The bits are stored in a character array so that the array can be
 *     easily copied without worrying about alignment (ie can use a loop as
 *     well as memcpy()).
 *   This is based on two's complement math.
 */

/* The following defines are for character set size.  128 for straight
 *   ASCII, 256 for Euro ASCII (8 bit characters).
 */
#define MAXCHR	256		/*  128 or  256 */
#define BLKIND	0xf8		/* 0x78 or 0xf8 */

/*
 * The following defines are not meant to be changeable.
 * They are for readability only.
 */
#define CHRBIT	8
#define BITBLK	MAXCHR/CHRBIT		// 16 or 32 bytes
#define BITIND	0x7

    /* Add or check to see if a character is in the bit table (character
     *   set).
     * Note:
     *   When calling these routines, make sure c is an unsigned char (or
     *     int) so if it has the high bit set, casting it to an int won't
     *     make it a large negative number.
     */
#define ISINSET(bittab,c) ((bittab)[((c) & BLKIND)>>3] & (1<<((c) & BITIND)))
#define   CHSET(bittab,c)  (bittab)[((c) & BLKIND)>>3] |= 1<<((c) & BITIND)

static void   chset(Byte *bitTable, Byte c) { CHSET(bitTable,c); }

static Byte wordTable[BITBLK]; 	// bit table for word definition
static int  wordTableDefined = 0;

#define IS_WORD(c)	ISINSET(wordTable,c)

/* ******************************************************************** */

//#define RE_MAXDFA	768		// amount of space for compiled RE
#define RE_SLOP	50		// dfa overflow protection

// declare a dfa like so:  Byte dfa[RE_MAXDFA + RE_SLOP];	// automaton

#define BADPAT(dfa,msg)	return (*dfa = END, (char *)msg)
#define STORE(x)	(*mp++ = x)  // RE_SLOP guards against overflow
 
    /* Compile RE to internal format & store in dfa[]
     * Input:
     *   pat : pointer to regular expression string to compile.
     * Returns:
     *   NULL:  RE compiled OK.
     *   Pointer to error message. DON'T use DFA!
     */
char *regExpCompile(UChar *pat, Byte dfa[], size_t *dfaSz)
{
   int	tagstk[RE_MAXTAG] = { 0 }; // subpat tag stack, [0] not [really] used
   				   // [0] for ortstk: a|(b)
   int	ortstk[RE_MAXOR];   // | tag stack, [0] not used, PITA
   UChar
     *p,		// pattern pointer
     *mp = dfa,		// dfa pointer
     *lp,		// saved pointer
     *sp = dfa,		// another one
     *endDFA = dfa + *dfaSz - RE_SLOP;	// overflow checking
   UChar
      bittab[BITBLK];	// bit table for SET
   int
      tagi = 0,		// tag stack index
      tagc = 1,		// actual tag count
      orti = 0,		// or/tag stack index
      n;

   for (n = 0; n < BITBLK; bittab[n++] = 0) ;	// clear SET bit table

	// Build a bit table definition of a word. Done once.
        // Not thread safe so I call during VM construction
   if (!wordTableDefined)
   {
      wordTableDefined = 1;
      for (n = 0; n <= 0xff; n++)
         if (IS_ALPHA(n)) CHSET(wordTable,n);
   }

   if (pat == 0 || *pat == '\0') BADPAT(dfa,"Bad regular expression");

   *dfa = END;			// init dfa for lp and sp
   mp++;			// some startup state

   for (p = pat; *p; p++)
   {
      lp = mp;			// start of next dfa state
      switch(*p)
      {
	 case '.': STORE(ANY); break;		// match any character
	 case '^':				// match beginning of line
	    if (p==pat || (!tagi && *p=='^')) STORE(BOL);
	    else { STORE(CHR); STORE(*p); }	// match ^
	    break;
	 case '$':				// match end of line
	    if (*(p+1) == '\0' || (!tagi && p[1]=='|')) STORE(EOL);
	    else { STORE(CHR); STORE(*p); }	// match $
	    break;
	 case '[':				// match a set of characters
	    if (*++p == '^') { STORE(NSET); p++; } else STORE(SET);
	    if (*p == ']') chset(bittab,*p++);	// real bracket, match ]
	    if (*p == '-') chset(bittab,*p++);	// real dash, match -
	    while (*p && *p != ']')
	    {
	       if (*p == '-' && *(p+1) != '\0' && *(p+1) != ']')  // [a-z]
	       {
		  int c1, c2;
		  p++;
		  c1 = *(p-2);		// 'a'
		  c2 = *p++;		// 'z'
		  if (c1 > c2)		// something like [z-a]
		     BADPAT(dfa,"Regexp: Empty set");
			  // remember that 'a' has already been put into bittab
		  while (++c1 <= c2) chset(bittab,c1);	// build bit table
	       }
#ifdef EXTEND
//	       else if (*p == '\\' && *(p+1)) { p++; chset(bittab,*p++); }
	       else if (*p == '\\' && *(p+1))
	       {
		  char c = *++p;
		  switch(c)
		  {
		     case 'b': c = '\b'; break;
		     case 'n': c = '\n'; break;
		     case 'f': c = '\f'; break;
		     case 'r': c = '\r'; break;
		     case 't': c = '\t'; break;
		  }
		  chset(bittab,c); p++;
	       }
#endif
	       else chset(bittab,*p++);
	    } // while
	    if (*p == '\0') BADPAT(dfa,"Regexp: Missing ]");
		// store table and clear for next use
	    for (n = 0; n < BITBLK; bittab[n++] = '\0') STORE(bittab[n]);
	    break;
	 case '*':		// match 0 or more of preceding RE
	 case '+':		// match 1 or more.  Note: x+ == xx*
	 one:
	    if (p == pat) BADPAT(dfa,"Regexp: Empty closure");
	    lp = sp;			// previous opcode
	    if (*lp == CLO) break;  // equivalence: x** == x*, CLO CLO --> CLO
	 #ifdef ONE
	    if (*lp == ONE) break;	// equivalence: x\?\? == x
	 #endif
	    switch(*lp)
	    {
	       case BOL: case BOT: case EOT: case BOW: case EOW: case REF:
	       case POR:
		  BADPAT(dfa,"Regexp: Illegal closure");
	    }
	    if (*p == '+') for (sp = mp; lp < sp; lp++) STORE(*lp);
	    STORE(END); STORE(END); sp = mp;
	 #ifdef ONE
	    while (--mp > lp) *mp = mp[-1];   // open hole for CLO
	    STORE((*p=='?') ? ONE : CLO);
	 #else
	    while (--mp > lp) *mp = mp[-1]; STORE(CLO);  // open hole for CLO
	 #endif
	    mp = sp;
	    break;
	 case '(':	//!!! (?: don't actually store result
	    if(orti) tagc = ortstk[orti];
	    if (tagc < RE_MAXTAG)
	       { tagstk[++tagi] = tagc; STORE(BOT); STORE(tagc++); }
	    else BADPAT(dfa,"Regexp: Too many () pairs");
	    break;
	 case ')':
	    if (*sp == BOT) BADPAT(dfa,"Regexp: Null pattern inside ()");
	    if (tagi > 0) { STORE(EOT); STORE(tagstk[tagi--]); }
	    else BADPAT(dfa,"Regexp: Unmatched )");
	    if(orti) orti--;
	    break;
	 case '|':
	    if (p==pat || !p[1]) BADPAT(dfa,"Regexp: Empty |");
	    switch(*sp)			// previous opcode
	    {
	       case BOL: case BOT: case BOW: case EOW: case REF:
	       case POR:
		  BADPAT(dfa,"Regexp: Illegal |");
	    }
	    STORE(POR);
	    if (orti <= RE_MAXOR) ortstk[++orti] = tagstk[tagi] + 1;
	    else BADPAT(dfa,"Regexp: Too many |'s");
	    *dfa = ANY;		// startup state so I don't have to scan dfa
	    break;
	 case '\\':  		            // tags, backrefs
	    switch(*++p)
	    {
	       case '\0': BADPAT(dfa,"Regexp: Bad quote");
	       case '<': STORE(BOW); break;
	       case '>':
		  if (*sp == BOW)
		     BADPAT(dfa,"Regexp: Null pattern inside \\<\\>");
		  STORE(EOW);
		  break;
	       case '1': case '2': case '3': case '4': case '5': case '6': 
	       case '7': case '8': case '9':
		  n = *p - '0';
		  if (tagi > 0 && tagstk[tagi] == n)
		     BADPAT(dfa,"Regexp: Cyclical reference");
		  if (tagc > n) { STORE(REF); STORE(n); }
		  else BADPAT(dfa,"Regexp: Undetermined reference");
		  break;
	       case 's': STORE(SPACE);	 break;
	       case 'S': STORE(N_SPACE); break;
	       case 'w': STORE(ALPHA);	 break;
	       case 'W': STORE(N_ALPHA); break;
	       case 'd': STORE(DIGIT);	 break;
	       case 'D': STORE(N_DIGIT); break;
#ifdef EXTEND
	       case 'b': STORE(CHR); STORE('\b'); break;
	       case 'n': STORE(CHR); STORE('\n'); break;
	       case 'f': STORE(CHR); STORE('\f'); break;
	       case 'r': STORE(CHR); STORE('\r'); break;
	       case 't': STORE(CHR); STORE('\t'); break;
#endif
#ifdef ONE
	       case '?':		// match 1 or none.
		  goto one;
#endif
	       default: STORE(CHR); STORE(*p);
	    } // switch
	    break;
	    default : STORE(CHR); STORE(*p); break;   // an ordinary character
      }
      sp = lp;		// start of previous state

//      if (mp > dfa + RE_MAXDFA)
      if (mp > endDFA)
	 BADPAT(dfa,"Regexp: Pattern too long (braindead re_comp())");
   }

   if (tagi > 0) BADPAT(dfa,"Regexp: Unmatched (");

   STORE(END);

   *dfaSz = (mp - dfa);		// in case you want to malloc() the dfa
   return 0;
}

typedef struct{ UChar *lp; Byte *dfa; } Stator;	// state for |

static UChar *dfaScanForward(Byte *dfa,int a);
static UChar *pmatch(
   UChar *lp, Byte *dfa, UChar *bol,
   UChar *bopat[], UChar *eopat[], Stator *, void *X);

#define ceq(a,b) 	((a) == (b))		// character eq

/* re_exec:  execute dfa to find a match.
 *
 * special cases: (dfa[0])	
 *  BOL
 *	Match only once, starting from the beginning.
 *  CHR
 *	First locate the character without calling pmatch(), and if found,
 *	call pmatch() for the remaining string.
 *  END
 *	re_comp() failed, poor luser did not check for it. Fail fast.
 *
 * If a match is found, bopat[0] and eopat[0] are set to the beginning and
 *   the end of the matched fragment, respectively.
 *
 * Input:
 *   lp: string to search
 *   SoL:  1 if lp starts line
 *   move: 1 if search the entire string for match
 *   tags:  char *tags[2 * RE_MAXTAG] or 0, these are the \( ptrs into lp
 *      tags[0]-->start of match, tags[RE_MAXTAG]-->end of match
 *      If tags==0, they are ignored.
 * Returns:
 *   0: Fail
 *   1: Match
 * Notes:
 *   If SoL is 0, lp[-1] MUST be at valid!  A couple of REs will look
 *     there if they can.
 */
int regExpExec(Byte *dfa, UChar *lp, int SoL, int move, UChar *tags[], void *X)
{
   #define REX_FAIL	0
   #define REX_MATCHED	1

   UChar  *ap;
   UChar  *ep = 0;
   UChar  *bol;
   UChar **bopat, **eopat, *fakeTags[2 * RE_MAXTAG];
   UChar  *startMatch = lp;
   int	   orlando, canScan = 1;	// opto
   Stator  stator = { 0,0 };		// | state for moving around the DFA

   orlando = (*dfa++!=END);	  // is there an | in the DFA?
   canScan = (!orlando && move);  // no | in dfa

   if(!tags) tags = fakeTags;
   bopat = tags; eopat = &tags[RE_MAXTAG];

tiptop:
   ap = dfa;
   bol = SoL ? lp : 0;

   memset(bopat,0,RE_MAXTAG*sizeof(char *));
   memset(eopat,0,RE_MAXTAG*sizeof(char *));

top:
   switch(*ap)
   {
      case END: return 0;		// munged automaton. fail always
      case BOL:				// anchored: match from BOL only
	 if (!SoL) return 0;		// BoL can only be at front of dfa
	 ep = pmatch(lp,++ap,bol,bopat,eopat,&stator,X);
	 break;
      case CHR:				// ordinary char: locate it fast
	 if (canScan)
	 {
	 #if 0
	    c = *(ap+1);
	    while (*lp && !ceq(*lp,c)) lp++;
	    if (!*lp) return REX_FAIL;	// if EoS, fail. else fall thru
	 #else
	    lp = (UChar *)strchr((char *)lp,*(ap+1));
	    if (!lp) return REX_FAIL;	// if EoS, fail. else fall thru
	 #endif
	    startMatch = lp;
	 }
	 // FALLTHROUGH
      default:				// regular matching all the way.
	 ep = pmatch(lp,ap,bol,bopat,eopat,&stator,X);
	 break; 
   }
   if (!ep)	// no match, may have hit |
   {
      UChar *ptr, *plp;  // plp !0 if in tag
      int    n;

      if(orlando)    // no |'s, don't look
      {
	 // Intuit if in something like (a|b)c vs (a)c
	 // Treat tags as stack, get most recent open one and proceed from there
	 // stator.lp points to src where | stopped match (if it in fact did)
	 for(plp=0, n=RE_MAXTAG; --n; )	// bopot[0] not a tag
	    if(bopat[n] && !eopat[n])	// last open tag: ((a|b)c) match a
	    {
	       plp = bopat[n];
	       break;
	    }

	 if(stator.lp) // match: hit start of next | clause, move to ) or done
	 {
	    // if in open tab, move past last dangle to end tag
	    ptr = plp ? dfaScanForward(stator.dfa,EOT) : 0;
	    if(!ptr)
	    {
	       if(!*startMatch) return REX_FAIL;
	       ep = stator.lp;
	       goto matched; // but done anyway
	    }
	    ap	      = (UChar *)ptr;
	    lp	      = stator.lp;
	    stator.lp = 0;
	    goto top; 
	 }

	 // no match: look for next | clause
	 if((ptr = dfaScanForward(ap,POR))) // NOT stator.dfa
	 {  // failed in middle of | with another clause to check
	    if(plp) lp = plp;	// reset src if open tag
	    ap         = (UChar *)ptr + 1; 
	    stator.lp  = 0;
	    goto top; 
	 }
      }// orlando

      // match failed, move to next character and try again
      if (move && *lp && *++lp)	
      {
	 startMatch = lp;
	 SoL        = 0;
	 stator.lp  = 0;	// CYA !!! not sure I need this
	 goto tiptop;
      }

      return REX_FAIL;
   }

matched:
   if(orlando)	// take care of "(a|b)|c" search("c") leaving dangling tag
   {
      int n;
      for(n=RE_MAXTAG; --n; )	// bopot[0] not a tag
         if(bopat[n] && !eopat[n]) eopat[n] = bopat[n];	// ""
   }

   bopat[0] = startMatch; eopat[0] = ep;
      
   return REX_MATCHED;
}

/* pmatch: internal routine for the hard part
 *
 * This code is mostly snarfed from an early grep written by David Conroy.
 *   The backref and tag stuff, and various other mods are by oZ.
 *
 * special cases: (dfa[n], dfa[n+1])
 *  CLO ANY
 *    We KNOW ".*" will match ANYTHING upto the end of line.  Thus, go to
 *    the end of line straight, without calling pmatch() recursively.  As in
 *    the other closure cases, the remaining pattern must be matched by
 *    moving backwards on the string recursively, to find a match for xy (x
 *    is ".*" and y is the remaining pattern) where the match satisfies the
 *    LONGEST match for x followed by a match for y.
 *  CLO CHR
 *    Scan forward matching the single char without recursion, and at the
 *    point of failure, we execute the remaining dfa recursively, as
 *    described above.
 *
 * At the end of a successful match, bopat[n] and eopat[n] are set to the
 *   beginning and end of subpatterns matched by tagged expressions (n = 1
 *   to 9).
 * 
 * Input:
 * Returns:
 *   0: No match, regExpFail() may have been called.
 *   else: pointer to end of match.
 */

	/* skip values for CLO XXX to skip past the closure */
#define ANYSKIP	2 		/* CLO ANY END ...	   */
#define CHRSKIP	3		/* CLO CHR chr END ...	   */
#define SETSKIP (2 +BITBLK)	/* CLO SET 16bytes END ... */

static UChar *pmatch(
   UChar *lp, Byte *dfa, UChar *bol,
   UChar *bopat[], UChar *eopat[], Stator *stator, void *X)
{
  register UChar
    *e,			/* extra pointer for CLO */
    *bp, *ep;		/* beginning and ending of subpat */
  UChar *are;		/* to save the line ptr */
  int op, c, n;
//  int ignoreCase = 0;

  while((op = *dfa++) != END)
    switch(op)
    {
    #if 1
      case CHR:	if (!ceq(*lp++,*dfa++)) return 0; break;
    #else
      case CHR:
      {
	 char c1=*lp++, c2=*dfa++; 
	 if(ignoreCase){ if(tolower(c1)!=tolower(c2)) return 0; }
	 else            if(!ceq(c1,c2)) return 0;
	 break; 
      }
    #endif
      case ANY: if (*lp++ == '\0') return 0; break;
      case SET:
        c = *lp++;
	if (!ISINSET(dfa,c)) 	   return 0;	/* ISINSET(dfa,0) is 0 */
	dfa += BITBLK;
	break;
      case NSET:
	if ((c = *lp++) == '\0' || ISINSET(dfa,c)) return 0;
	dfa += BITBLK;
	break;
      case BOT: bopat[*dfa++] = lp; break;
      case EOT:	eopat[*dfa++] = lp; break;
      case EOL://	    if (*lp != '\0')		 return 0; break;
	 if(*lp != '\0') return 0;
	 if(*dfa==POR)   return lp;	// dfa: $|	valid in Python
	 break;
      case DIGIT:   if (!*lp || !isdigit(*lp++)) return 0;	 break;
      case N_DIGIT: if (!*lp ||  isdigit(*lp++)) return 0;	 break;
      case SPACE:   if (!*lp || !isspace(*lp++)) return 0;	 break;
      case N_SPACE: if (!*lp ||  isspace(*lp++)) return 0;	 break;
      case ALPHA:   if (!*lp || !IS_WORD(*lp))   return 0; lp++; break;
      case N_ALPHA: if (!*lp ||  IS_WORD(*lp))   return 0; lp++; break;
      case BOW:
        if (!(lp != bol && IS_WORD(lp[-1])) && IS_WORD(*lp)) break;
	return 0;
      case EOW:		// 'w\0' is OK here
        if ((lp != bol && IS_WORD(lp[-1])) && !IS_WORD(*lp)) break;
	return 0;
      case POR:		// | matched, now skip | I'm looking at
	stator->lp  = lp;  // signal match
	stator->dfa = dfa; // save ptr to next dfa state
	return 0;	   // fake fail
      case REF:
        n = *dfa++; bp = bopat[n]; ep = eopat[n];
	while (bp < ep) if (*bp++ != *lp++) return 0;
	break;
      case CLO:
        are = lp; n = ANYSKIP;
	switch(*dfa)
	{
	  case ANY:     while (*lp)		     lp++; break;
	  case DIGIT:   while ( isdigit(*lp))	     lp++; break;
	  case N_DIGIT: while (!isdigit(*lp) && *lp) lp++; break;
	  case SPACE:   while ( isspace(*lp))	     lp++; break;
	  case N_SPACE: while (!isspace(*lp) && *lp) lp++; break;
	  case ALPHA:   while ( IS_WORD(*lp))	     lp++; break;
	  case N_ALPHA: while (!IS_WORD(*lp) && *lp) lp++; break;
	  case CHR:
	    c = *(dfa+1);		// we know c != '\0'
	    while (ceq(*lp,c)) lp++;
	    n = CHRSKIP;
	    break;
	  case SET: case NSET:
	    while (*lp && (e = pmatch(lp,dfa,bol,bopat,eopat,stator,X))) lp = e;
	    n = SETSKIP;
	    break;
	  default:
	     regExpFail("re_exec: closure: bad dfa.",*dfa,X);
	     return 0;
	}
	dfa += n;
	while (lp >= are)	/* backup up till match next pattern */
	{
	  if ((e = pmatch(lp,dfa,bol,bopat,eopat,stator,X))) return e;
	  if(stator->lp) return 0;	// hit |: ".*(foo|bar)"
	  --lp;
	}
	return 0;
    #ifdef ONE
      case ONE:
        are = lp; n = ANYSKIP;
	switch(*dfa)
	{
	  case ANY:     if(*lp)		         lp++; break;
	  case DIGIT:   if( isdigit(*lp))	 lp++; break;
	  case N_DIGIT: if(!isdigit(*lp) && *lp) lp++; break;
	  case SPACE:   if( isspace(*lp))	 lp++; break;
	  case N_SPACE: if(!isspace(*lp) && *lp) lp++; break;
	  case ALPHA:   if( IS_WORD(*lp))	 lp++; break;
	  case N_ALPHA: if(!IS_WORD(*lp) && *lp) lp++; break;
	  case CHR:
	    c = *(dfa+1);		// we know c != '\0'
	    if(ceq(*lp,c)) lp++;
	    n = CHRSKIP;
	    break;
	  case SET: case NSET:
	    if(lp && (e = pmatch(lp,dfa,bol,bopat,eopat,stator,X))) lp = e;
	    n = SETSKIP;
	    break;
	  default:
	     regExpFail("re_exec: one or none closure: bad dfa.",*dfa,X);
	     return 0;
	}
	dfa += n;
	break;
    #endif
      default:
         regExpFail("re_exec: bad dfa.",op,X);
	 return 0;
    }// switch, while
  return lp;
}

static UChar *dfaScanForward(Byte *dfa,int a)
{
   int n;

   while(*dfa != END)
   {
      if (*dfa == a) return dfa;
      switch(*dfa++)
      {
      #ifdef ONE
	 case ONE:
      #endif
	 case CLO:
	    n = 1;  // CYA
	    switch(*dfa)
	    {
	       case SPACE: case N_SPACE:
	       case DIGIT: case N_DIGIT:
	       case ALPHA: case N_ALPHA:
	       case CHR:            n = CHRSKIP; break;
	       case ANY:            n = ANYSKIP; break;
	       case SET: case NSET: n = SETSKIP; break;
	    }
	    dfa += n;
	    break;
         case CHR:
	 case BOT: case EOT:  // !!! (|) don't scan out of group!
	 case REF: 
	    dfa++; break;
         case SET: case NSET: dfa += BITBLK; break;
      }
   }
   return 0;
}

#if 0
/* re_subs: substitute the matched portions of the src in dst.
 *	&	substitute the entire matched pattern.
 *	\digit	substitute a subpattern, with the given
 *		tag number. Tags are numbered from 1 to
 *		9. If the particular tagged subpattern
 *		does not exist, null is substituted.
 * 	!!!Note: if the line that was used re_exec() has gone byebye
 *	  then \digit will blow cookies since the tags point into the line.
 * Input:
 *   src:
 *   dst:
 * Returns:
 *   1:  Everything went as expected
 *   0:  Bad src or no match.
 */
int re_subs(UChar *src, UChar *dst, UChar *tags[])
{
   UChar   c, *bp, *ep;
   int	   pin;
   UChar **bopat = tags, **eopat = &tags[RE_MAXTAG];

   if (!tags[0]) return 0;

   while((c = *src++))
   {
      switch(c)
      {
	 case '&': pin = 0; break;
	 case '\\': 
	    c = *src++;
	    if (c >= '0' && c <= '9') { pin = c - '0'; break; }
	 default: *dst++ = c; continue;
      }
      if ((bp = bopat[pin]) && (ep = eopat[pin]))
      {
	 while (*bp && bp < ep) *dst++ = *bp++;
	 if (bp < ep) return 0;
      }
   }
   *dst = '\0';
   return 1;
}
#endif

/* ******************************************************************** */
/* ************************* DEBUG ************************************ */
/* ******************************************************************** */

#if 1
void dfaDump(Byte *dfa, int showSz)
{
  int    n;
  UChar *start=dfa;

  dfa++;	// skip over state
  while (*dfa != END)
    switch(*dfa++)
    {
    #ifdef ONE
      case ONE: printf("ONE");
    #endif
      case CLO:
        printf("CLOSURE\n");
	dfaDump(dfa - 1, 0);
	switch(*dfa)
	{
	   case SPACE: case N_SPACE:
	   case DIGIT: case N_DIGIT:
	   case ALPHA: case N_ALPHA:
	   case CHR: 	        n = CHRSKIP; break;
	   case ANY:		n = ANYSKIP; break;
	   case SET: case NSET: n = SETSKIP; break;
	   case CLO: n = 0; break;
	 default:
	   printf("bad dfa. opcode %o\n", dfa[-1]);
	   exit(1);
	}
	dfa += n;
	break;
      case CHR: printf("\tCHR %c\n",*dfa++); break;
      case ANY: printf("\tANY .\n"); break;
      case BOL: printf("\tBOL -\n"); break;
      case EOL: printf("\tEOL -\n"); break;

      case SPACE:   printf("\tSPACE\n");  break;
      case N_SPACE: printf("\t!SPACE\n"); break;
      case DIGIT:   printf("\tDIGIT\n");  break;
      case N_DIGIT: printf("\t!DIGIT\n"); break;
      case ALPHA:   printf("\tALPHA\n");  break;
      case N_ALPHA: printf("\t!ALPHA\n"); break;

      case BOT: printf("BOT: %d\n",*dfa++); break;
      case EOT: printf("EOT: %d\n",*dfa++); break;
      case BOW: printf("BOW\n"); break;
      case EOW: printf("EOW\n"); break;
      case POR: printf("OR\n");  break;
      case REF: printf("REF: %d\n",*dfa++); break;
      case SET:
        printf("\tSET [");
	for (n = 0; n < MAXCHR; n++)
	  if (ISINSET(dfa,n)) printf("%c",n);
	printf("]\n");
	dfa += BITBLK;
	break;
      case NSET:
        printf("\tNSET [");
	for (n = 0; n < MAXCHR; n++)
	  if (ISINSET(dfa,n)) printf("%c",n);
	printf("]\n"); dfa += BITBLK;
	break;
      default:
        printf("bad dfa. opcode %o\n", dfa[-1]);
	exit(1);
	break;
    }
    printf("END\n");
    if(showSz) printf("%ld bytes\n",dfa - start + 1);
}
#endif
