/* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   if !defined (TRUE)
#      define TRUE (1==1)
#   endif

#   if !defined (FALSE)
#      define FALSE (1==0)
#   endif

#include <stddef.h>
#include <string.h>
#include <limits.h>
#   include "GStorage.h"
#define _mcError_H
#define _mcError_C

#   include "GASCII.h"
#   include "GDynamicStrings.h"
#   include "GFIO.h"
#   include "GStrLib.h"
#   include "GFormatStrings.h"
#   include "GStorage.h"
#   include "GM2RTS.h"
#   include "GSYSTEM.h"
#   include "GStdIO.h"
#   include "GnameKey.h"
#   include "GmcLexBuf.h"
#   include "GmcPrintf.h"

#   define Debugging TRUE
#   define DebugTrace FALSE
#   define Xcode TRUE
typedef struct _T1_r _T1;

typedef _T1 *mcError_error;

struct _T1_r {
               mcError_error parent;
               mcError_error child;
               mcError_error next;
               unsigned int fatal;
               DynamicStrings_String s;
               unsigned int token;
             };

static mcError_error head;
static unsigned int inInternal;

/*
   internalError - displays an internal error message together with the compiler source
                   file and line number.
                   This function is not buffered and is used when the compiler is about
                   to give up.
*/

void mcError_internalError (char *a_, unsigned int _a_high, char *file_, unsigned int _file_high, unsigned int line);

/*
   writeFormat0 - displays the source module and line together
                  with the encapsulated format string.
                  Used for simple error messages tied to the current token.
*/

void mcError_writeFormat0 (char *a_, unsigned int _a_high);

/*
   writeFormat1 - displays the source module and line together
                  with the encapsulated format string.
                  Used for simple error messages tied to the current token.
*/

void mcError_writeFormat1 (char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);

/*
   writeFormat2 - displays the module and line together with the encapsulated
                  format strings.
                  Used for simple error messages tied to the current token.
*/

void mcError_writeFormat2 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);

/*
   writeFormat3 - displays the module and line together with the encapsulated
                  format strings.
                  Used for simple error messages tied to the current token.
*/

void mcError_writeFormat3 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);

/*
   newError - creates and returns a new error handle.
*/

mcError_error mcError_newError (unsigned int atTokenNo);

/*
   newWarning - creates and returns a new error handle suitable for a warning.
                A warning will not stop compilation.
*/

mcError_error mcError_newWarning (unsigned int atTokenNo);

/*
   chainError - creates and returns a new error handle, this new error
                is associated with, e, and is chained onto the end of, e.
                If, e, is NIL then the result to NewError is returned.
*/

mcError_error mcError_chainError (unsigned int atTokenNo, mcError_error e);
void mcError_errorFormat0 (mcError_error e, char *a_, unsigned int _a_high);
void mcError_errorFormat1 (mcError_error e, char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);
void mcError_errorFormat2 (mcError_error e, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);
void mcError_errorFormat3 (mcError_error e, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);
void mcError_errorString (mcError_error e, DynamicStrings_String str);

/*
   errorStringAt - given an error string, s, it places this
                   string at token position, tok.
                   The string is consumed.
*/

void mcError_errorStringAt (DynamicStrings_String s, unsigned int tok);

/*
   errorStringAt2 - given an error string, s, it places this
                    string at token positions, tok1 and tok2, respectively.
                    The string is consumed.
*/

void mcError_errorStringAt2 (DynamicStrings_String s, unsigned int tok1, unsigned int tok2);

/*
   errorStringsAt2 - given error strings, s1, and, s2, it places these
                     strings at token positions, tok1 and tok2, respectively.
                     Both strings are consumed.
*/

void mcError_errorStringsAt2 (DynamicStrings_String s1, DynamicStrings_String s2, unsigned int tok1, unsigned int tok2);

/*
   warnStringAt - given an error string, s, it places this
                  string at token position, tok.
                  The string is consumed.
*/

void mcError_warnStringAt (DynamicStrings_String s, unsigned int tok);

/*
   warnStringAt2 - given an warning string, s, it places this
                   string at token positions, tok1 and tok2, respectively.
                   The string is consumed.
*/

void mcError_warnStringAt2 (DynamicStrings_String s, unsigned int tok1, unsigned int tok2);

/*
   warnStringsAt2 - given warning strings, s1, and, s2, it places these
                    strings at token positions, tok1 and tok2, respectively.
                    Both strings are consumed.
*/

void mcError_warnStringsAt2 (DynamicStrings_String s1, DynamicStrings_String s2, unsigned int tok1, unsigned int tok2);
void mcError_warnFormat0 (char *a_, unsigned int _a_high);

/*
   warnFormat1 - displays the source module and line together
                 with the encapsulated format string.
                 Used for simple warning messages tied to the current token.
*/

void mcError_warnFormat1 (char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);

/*
   flushErrors - switches the output channel to the error channel
                 and then writes out all errors.
*/

void mcError_flushErrors (void);

/*
   flushWarnings - switches the output channel to the error channel
                   and then writes out all warnings.
                   If an error is present the compilation is terminated,
                   if warnings only were emitted then compilation will
                   continue.
*/

void mcError_flushWarnings (void);

/*
   errorAbort0 - aborts compiling, it flushes all warnings and errors before aborting.
*/

void mcError_errorAbort0 (char *a_, unsigned int _a_high);

/*
   cast - casts a := b
*/

static void cast (unsigned char *a, unsigned int _a_high, unsigned char *b_, unsigned int _b_high);
static unsigned int translateNameToCharStar (char *a, unsigned int _a_high, unsigned int n);

/*
   outString - writes the contents of String to stdout.
               The string, s, is destroyed.
*/

static void outString (DynamicStrings_String file, unsigned int line, unsigned int col, DynamicStrings_String s);
static DynamicStrings_String doFormat1 (char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);

/*
   doFormat2 -
*/

static DynamicStrings_String doFormat2 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);

/*
   writeFormat2 - displays the module and line together with the encapsulated
                  format strings.
                  Used for simple error messages tied to the current token.
*/

static DynamicStrings_String doFormat3 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);

/*
   init - initializes the error list.
*/

static void init (void);

/*
   checkIncludes - generates a sequence of error messages which determine the relevant
                   included file and line number.
                   For example:

                   gcc a.c
                   In file included from b.h:1,
                                    from a.c:1:
                   c.h:1: parse error before `and'

                   where a.c is: #include "b.h"
                         b.h is: #include "c.h"
                         c.h is: and this and that

                   we attempt to follow the error messages that gcc issues.
*/

static void checkIncludes (unsigned int token, unsigned int depth);

/*
   flushAll - flushes all errors in list, e.
*/

static unsigned int flushAll (mcError_error e, unsigned int FatalStatus);


/*
   cast - casts a := b
*/

static void cast (unsigned char *a, unsigned int _a_high, unsigned char *b_, unsigned int _b_high)
{
  unsigned int i;
  unsigned char b[_b_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (b, b_, _b_high+1);

  if ((_a_high) == (_b_high))
    for (i=0; i<=_a_high; i++)
      a[i] = b[i];
}

static unsigned int translateNameToCharStar (char *a, unsigned int _a_high, unsigned int n)
{
  unsigned int argno;
  unsigned int i;
  unsigned int h;

  /* 
   translateNameToString - takes a format specification string, a, and
                           if they consist of of %a then this is translated
                           into a String and %a is replaced by %s.
  */
  argno = 1;
  i = 0;
  h = StrLib_StrLen ((char *) a, _a_high);
  while (i < h)
    {
      if ((a[i] == '%') && ((i+1) < h))
        {
          if ((a[i+1] == 'a') && (argno == n))
            {
              a[i+1] = 's';
              return TRUE;
            }
          argno += 1;
          if (argno > n)
            /* all done  */
            return FALSE;
        }
      i += 1;
    }
  return FALSE;
}


/*
   outString - writes the contents of String to stdout.
               The string, s, is destroyed.
*/

static void outString (DynamicStrings_String file, unsigned int line, unsigned int col, DynamicStrings_String s)
{
  DynamicStrings_String leader;
  unsigned int space;
  unsigned int newline;
  char * p;
  char * q;

  col += 1;
  if (Xcode)
    leader = FormatStrings_Sprintf2 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) "%s:%d:", 6)), (unsigned char *) &file, (sizeof (file)-1), (unsigned char *) &line, (sizeof (line)-1));
  else
    leader = FormatStrings_Sprintf3 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) "%s:%d:%d:", 9)), (unsigned char *) &file, (sizeof (file)-1), (unsigned char *) &line, (sizeof (line)-1), (unsigned char *) &col, (sizeof (col)-1));
  p = DynamicStrings_string (s);
  newline = TRUE;
  space = FALSE;
  while ((p != NULL) && ((*p) != ASCII_nul))
    {
      if (newline)
        {
          q = DynamicStrings_string (leader);
          while ((q != NULL) && ((*q) != ASCII_nul))
            {
              StdIO_Write ((*q));
              q += 1;
            }
        }
      newline = (*p) == ASCII_nl;
      space = (*p) == ' ';
      if (newline && Xcode)
        mcPrintf_printf1 ((char *) "(pos: %d)", 9, (unsigned char *) &col, (sizeof (col)-1));
      StdIO_Write ((*p));
      p += 1;
    }
  if (! newline)
    {
      if (Xcode)
        {
          if (! space)
            StdIO_Write (' ');
          mcPrintf_printf1 ((char *) "(pos: %d)", 9, (unsigned char *) &col, (sizeof (col)-1));
        }
      StdIO_Write (ASCII_nl);
    }
  FIO_FlushBuffer (FIO_StdOut);
  if (! Debugging)
    {
      s = DynamicStrings_KillString (s);
      leader = DynamicStrings_KillString (leader);
    }
}

static DynamicStrings_String doFormat1 (char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high)
{
  DynamicStrings_String s;
  nameKey_Name n;
  char a[_a_high+1];
  unsigned char w[_w_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (w, w_, _w_high+1);

  /* 
   DoFormat1 -
  */
  if (translateNameToCharStar ((char *) a, _a_high, 1))
    {
      cast ((unsigned char *) &n, (sizeof (n)-1), (unsigned char *) w, _w_high);
      s = DynamicStrings_Mark (DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)));
      s = FormatStrings_Sprintf1 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s, (sizeof (s)-1));
    }
  else
    s = FormatStrings_Sprintf1 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w, _w_high);
  return s;
}


/*
   doFormat2 -
*/

static DynamicStrings_String doFormat2 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high)
{
  nameKey_Name n;
  DynamicStrings_String s;
  DynamicStrings_String s1;
  DynamicStrings_String s2;
  unsigned int b;
  char a[_a_high+1];
  unsigned char w1[_w1_high+1];
  unsigned char w2[_w2_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (w1, w1_, _w1_high+1);
  memcpy (w2, w2_, _w2_high+1);

  b = (unsigned int) 0;
  if (translateNameToCharStar ((char *) a, _a_high, 1))
    {
      cast ((unsigned char *) &n, (sizeof (n)-1), (unsigned char *) w1, _w1_high);
      s1 = DynamicStrings_Mark (DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)));
      b |= (1 << (1 ));
    }
  if (translateNameToCharStar ((char *) a, _a_high, 2))
    {
      cast ((unsigned char *) &n, (sizeof (n)-1), (unsigned char *) w2, _w2_high);
      s2 = DynamicStrings_Mark (DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)));
      b |= (1 << (2 ));
    }
  switch (b)
    {
      case (unsigned int) 0:
        s = FormatStrings_Sprintf2 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w1, _w1_high, (unsigned char *) w2, _w2_high);
        break;

      case (unsigned int) ((1 << (1))):
        s = FormatStrings_Sprintf2 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s1, (sizeof (s1)-1), (unsigned char *) w2, _w2_high);
        break;

      case (unsigned int) ((1 << (2))):
        s = FormatStrings_Sprintf2 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w1, _w1_high, (unsigned char *) &s2, (sizeof (s2)-1));
        break;

      case (unsigned int) ((1 << (1)) | (1 << (2))):
        s = FormatStrings_Sprintf2 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s1, (sizeof (s1)-1), (unsigned char *) &s2, (sizeof (s2)-1));
        break;


      default:
        M2RTS_HALT (-1);
        break;
    }
  return s;
}


/*
   writeFormat2 - displays the module and line together with the encapsulated
                  format strings.
                  Used for simple error messages tied to the current token.
*/

static DynamicStrings_String doFormat3 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high)
{
  nameKey_Name n;
  DynamicStrings_String s;
  DynamicStrings_String s1;
  DynamicStrings_String s2;
  DynamicStrings_String s3;
  unsigned int b;
  char a[_a_high+1];
  unsigned char w1[_w1_high+1];
  unsigned char w2[_w2_high+1];
  unsigned char w3[_w3_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (w1, w1_, _w1_high+1);
  memcpy (w2, w2_, _w2_high+1);
  memcpy (w3, w3_, _w3_high+1);

  b = (unsigned int) 0;
  if (translateNameToCharStar ((char *) a, _a_high, 1))
    {
      cast ((unsigned char *) &n, (sizeof (n)-1), (unsigned char *) w1, _w1_high);
      s1 = DynamicStrings_Mark (DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)));
      b |= (1 << (1 ));
    }
  if (translateNameToCharStar ((char *) a, _a_high, 2))
    {
      cast ((unsigned char *) &n, (sizeof (n)-1), (unsigned char *) w2, _w2_high);
      s2 = DynamicStrings_Mark (DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)));
      b |= (1 << (2 ));
    }
  if (translateNameToCharStar ((char *) a, _a_high, 3))
    {
      cast ((unsigned char *) &n, (sizeof (n)-1), (unsigned char *) w3, _w3_high);
      s3 = DynamicStrings_Mark (DynamicStrings_InitStringCharStar (nameKey_keyToCharStar (n)));
      b |= (1 << (3 ));
    }
  switch (b)
    {
      case (unsigned int) 0:
        s = FormatStrings_Sprintf3 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w1, _w1_high, (unsigned char *) w2, _w2_high, (unsigned char *) w3, _w3_high);
        break;

      case (unsigned int) ((1 << (1))):
        s = FormatStrings_Sprintf3 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s1, (sizeof (s1)-1), (unsigned char *) w2, _w2_high, (unsigned char *) w3, _w3_high);
        break;

      case (unsigned int) ((1 << (2))):
        s = FormatStrings_Sprintf3 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w1, _w1_high, (unsigned char *) &s2, (sizeof (s2)-1), (unsigned char *) w3, _w3_high);
        break;

      case (unsigned int) ((1 << (1)) | (1 << (2))):
        s = FormatStrings_Sprintf3 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s1, (sizeof (s1)-1), (unsigned char *) &s2, (sizeof (s2)-1), (unsigned char *) w3, _w3_high);
        break;

      case (unsigned int) ((1 << (3))):
        s = FormatStrings_Sprintf3 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w1, _w1_high, (unsigned char *) w2, _w2_high, (unsigned char *) &s3, (sizeof (s3)-1));
        break;

      case (unsigned int) ((1 << (1)) | (1 << (3))):
        s = FormatStrings_Sprintf3 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s1, (sizeof (s1)-1), (unsigned char *) w2, _w2_high, (unsigned char *) &s3, (sizeof (s3)-1));
        break;

      case (unsigned int) ((1 << (2)) | (1 << (3))):
        s = FormatStrings_Sprintf3 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) w1, _w1_high, (unsigned char *) &s2, (sizeof (s2)-1), (unsigned char *) &s3, (sizeof (s3)-1));
        break;

      case (unsigned int) ((1 << (1)) | (1 << (2)) | (1 << (3))):
        s = FormatStrings_Sprintf3 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)), (unsigned char *) &s1, (sizeof (s1)-1), (unsigned char *) &s2, (sizeof (s2)-1), (unsigned char *) &s3, (sizeof (s3)-1));
        break;


      default:
        M2RTS_HALT (-1);
        break;
    }
  return s;
}


/*
   init - initializes the error list.
*/

static void init (void)
{
  head = NULL;
  inInternal = FALSE;
}


/*
   checkIncludes - generates a sequence of error messages which determine the relevant
                   included file and line number.
                   For example:

                   gcc a.c
                   In file included from b.h:1,
                                    from a.c:1:
                   c.h:1: parse error before `and'

                   where a.c is: #include "b.h"
                         b.h is: #include "c.h"
                         c.h is: and this and that

                   we attempt to follow the error messages that gcc issues.
*/

static void checkIncludes (unsigned int token, unsigned int depth)
{
  DynamicStrings_String included;
  unsigned int lineno;

  included = mcLexBuf_findFileNameFromToken (token, depth+1);
  if (included != NULL)
    {
      lineno = mcLexBuf_tokenToLineNo (token, depth+1);
      if (depth == 0)
        mcPrintf_printf2 ((char *) "In file included from %s:%d", 27, (unsigned char *) &included, (sizeof (included)-1), (unsigned char *) &lineno, (sizeof (lineno)-1));
      else
        mcPrintf_printf2 ((char *) "                 from %s:%d", 27, (unsigned char *) &included, (sizeof (included)-1), (unsigned char *) &lineno, (sizeof (lineno)-1));
      if ((mcLexBuf_findFileNameFromToken (token, depth+2)) == NULL)
        mcPrintf_printf0 ((char *) ":\\n", 3);
      else
        mcPrintf_printf0 ((char *) ",\\n", 3);
      checkIncludes (token, depth+1);
    }
}


/*
   flushAll - flushes all errors in list, e.
*/

static unsigned int flushAll (mcError_error e, unsigned int FatalStatus)
{
  mcError_error f;
  unsigned int written;

  written = FALSE;
  if (e != NULL)
    do {
      if ((FatalStatus == e->fatal) && (e->s != NULL))
        {
          checkIncludes (e->token, 0);
          if (e->fatal)
            e->s = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) " error: ", 8), DynamicStrings_Mark (e->s));
          else
            e->s = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) " warning: ", 10), DynamicStrings_Mark (e->s));
          outString (mcLexBuf_findFileNameFromToken (e->token, 0), mcLexBuf_tokenToLineNo (e->token, 0), mcLexBuf_tokenToColumnNo (e->token, 0), e->s);
          if ((e->child != NULL) && (flushAll (e->child, FatalStatus)))
            {}  /* empty.  */
          e->s = NULL;
          written = TRUE;
        }
      f = e;
      e = e->next;
      if (! Debugging)
        {
          f->s = DynamicStrings_KillString (f->s);
          Storage_DEALLOCATE ((void **) &f, sizeof (_T1));
        }
    } while (! (e == NULL));
  return written;
}


/*
   internalError - displays an internal error message together with the compiler source
                   file and line number.
                   This function is not buffered and is used when the compiler is about
                   to give up.
*/

void mcError_internalError (char *a_, unsigned int _a_high, char *file_, unsigned int _file_high, unsigned int line)
{
  char a[_a_high+1];
  char file[_file_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (file, file_, _file_high+1);

  M2RTS_ExitOnHalt (1);
  if (! inInternal)
    {
      inInternal = TRUE;
      mcError_flushErrors ();
      outString (mcLexBuf_findFileNameFromToken (mcLexBuf_getTokenNo (), 0), mcLexBuf_tokenToLineNo (mcLexBuf_getTokenNo (), 0), mcLexBuf_tokenToColumnNo (mcLexBuf_getTokenNo (), 0), DynamicStrings_Mark (DynamicStrings_InitString ((char *) "*** fatal error ***", 19)));
    }
  outString (DynamicStrings_Mark (DynamicStrings_InitString ((char *) file, _file_high)), line, 0, DynamicStrings_ConCat (DynamicStrings_Mark (DynamicStrings_InitString ((char *) "*** internal error *** ", 23)), DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high))));
  M2RTS_HALT (-1);
}


/*
   writeFormat0 - displays the source module and line together
                  with the encapsulated format string.
                  Used for simple error messages tied to the current token.
*/

void mcError_writeFormat0 (char *a_, unsigned int _a_high)
{
  mcError_error e;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  e = mcError_newError (mcLexBuf_getTokenNo ());
  e->s = FormatStrings_Sprintf0 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)));
}


/*
   writeFormat1 - displays the source module and line together
                  with the encapsulated format string.
                  Used for simple error messages tied to the current token.
*/

void mcError_writeFormat1 (char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high)
{
  mcError_error e;
  char a[_a_high+1];
  unsigned char w[_w_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (w, w_, _w_high+1);

  e = mcError_newError (mcLexBuf_getTokenNo ());
  e->s = doFormat1 ((char *) a, _a_high, (unsigned char *) w, _w_high);
}


/*
   writeFormat2 - displays the module and line together with the encapsulated
                  format strings.
                  Used for simple error messages tied to the current token.
*/

void mcError_writeFormat2 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high)
{
  mcError_error e;
  char a[_a_high+1];
  unsigned char w1[_w1_high+1];
  unsigned char w2[_w2_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (w1, w1_, _w1_high+1);
  memcpy (w2, w2_, _w2_high+1);

  e = mcError_newError (mcLexBuf_getTokenNo ());
  e->s = doFormat2 ((char *) a, _a_high, (unsigned char *) w1, _w1_high, (unsigned char *) w2, _w2_high);
}


/*
   writeFormat3 - displays the module and line together with the encapsulated
                  format strings.
                  Used for simple error messages tied to the current token.
*/

void mcError_writeFormat3 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high)
{
  mcError_error e;
  char a[_a_high+1];
  unsigned char w1[_w1_high+1];
  unsigned char w2[_w2_high+1];
  unsigned char w3[_w3_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (w1, w1_, _w1_high+1);
  memcpy (w2, w2_, _w2_high+1);
  memcpy (w3, w3_, _w3_high+1);

  e = mcError_newError (mcLexBuf_getTokenNo ());
  e->s = doFormat3 ((char *) a, _a_high, (unsigned char *) w1, _w1_high, (unsigned char *) w2, _w2_high, (unsigned char *) w3, _w3_high);
}


/*
   newError - creates and returns a new error handle.
*/

mcError_error mcError_newError (unsigned int atTokenNo)
{
  mcError_error e;
  mcError_error f;

  Storage_ALLOCATE ((void **) &e, sizeof (_T1));
  e->s = NULL;
  e->token = atTokenNo;
  e->next = NULL;
  e->parent = NULL;
  e->child = NULL;
  e->fatal = TRUE;
  if ((head == NULL) || (head->token > atTokenNo))
    {
      e->next = head;
      head = e;
    }
  else
    {
      f = head;
      while ((f->next != NULL) && (f->next->token < atTokenNo))
        f = f->next;
      e->next = f->next;
      f->next = e;
    }
  return e;
}


/*
   newWarning - creates and returns a new error handle suitable for a warning.
                A warning will not stop compilation.
*/

mcError_error mcError_newWarning (unsigned int atTokenNo)
{
  mcError_error e;

  e = mcError_newError (atTokenNo);
  e->fatal = FALSE;
  return e;
}


/*
   chainError - creates and returns a new error handle, this new error
                is associated with, e, and is chained onto the end of, e.
                If, e, is NIL then the result to NewError is returned.
*/

mcError_error mcError_chainError (unsigned int atTokenNo, mcError_error e)
{
  mcError_error f;

  if (e == NULL)
    return mcError_newError (atTokenNo);
  else
    {
      Storage_ALLOCATE ((void **) &f, sizeof (_T1));
      f->s = NULL;
      f->token = atTokenNo;
      f->next = e->child;
      f->parent = e;
      f->child = NULL;
      f->fatal = e->fatal;
      e->child = f;
    }
  return f;
}

void mcError_errorFormat0 (mcError_error e, char *a_, unsigned int _a_high)
{
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  /* 
   errorFormat routines provide a printf capability for the error handle.
  */
  if (e->s == NULL)
    e->s = FormatStrings_Sprintf0 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)));
  else
    e->s = DynamicStrings_ConCat (e->s, DynamicStrings_Mark (FormatStrings_Sprintf0 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)))));
}

void mcError_errorFormat1 (mcError_error e, char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high)
{
  DynamicStrings_String s1;
  char a[_a_high+1];
  unsigned char w[_w_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (w, w_, _w_high+1);

  s1 = doFormat1 ((char *) a, _a_high, (unsigned char *) w, _w_high);
  if (e->s == NULL)
    e->s = s1;
  else
    e->s = DynamicStrings_ConCat (e->s, DynamicStrings_Mark (s1));
}

void mcError_errorFormat2 (mcError_error e, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high)
{
  DynamicStrings_String s1;
  char a[_a_high+1];
  unsigned char w1[_w1_high+1];
  unsigned char w2[_w2_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (w1, w1_, _w1_high+1);
  memcpy (w2, w2_, _w2_high+1);

  s1 = doFormat2 ((char *) a, _a_high, (unsigned char *) w1, _w1_high, (unsigned char *) w2, _w2_high);
  if (e->s == NULL)
    e->s = s1;
  else
    e->s = DynamicStrings_ConCat (e->s, DynamicStrings_Mark (s1));
}

void mcError_errorFormat3 (mcError_error e, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high)
{
  DynamicStrings_String s1;
  char a[_a_high+1];
  unsigned char w1[_w1_high+1];
  unsigned char w2[_w2_high+1];
  unsigned char w3[_w3_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (w1, w1_, _w1_high+1);
  memcpy (w2, w2_, _w2_high+1);
  memcpy (w3, w3_, _w3_high+1);

  s1 = doFormat3 ((char *) a, _a_high, (unsigned char *) w1, _w1_high, (unsigned char *) w2, _w2_high, (unsigned char *) w3, _w3_high);
  if (e->s == NULL)
    e->s = s1;
  else
    e->s = DynamicStrings_ConCat (e->s, DynamicStrings_Mark (s1));
}

void mcError_errorString (mcError_error e, DynamicStrings_String str)
{
  e->s = str;
}


/*
   errorStringAt - given an error string, s, it places this
                   string at token position, tok.
                   The string is consumed.
*/

void mcError_errorStringAt (DynamicStrings_String s, unsigned int tok)
{
  mcError_error e;

  e = mcError_newError (tok);
  mcError_errorString (e, s);
}


/*
   errorStringAt2 - given an error string, s, it places this
                    string at token positions, tok1 and tok2, respectively.
                    The string is consumed.
*/

void mcError_errorStringAt2 (DynamicStrings_String s, unsigned int tok1, unsigned int tok2)
{
  mcError_errorStringsAt2 (s, s, tok1, tok2);
}


/*
   errorStringsAt2 - given error strings, s1, and, s2, it places these
                     strings at token positions, tok1 and tok2, respectively.
                     Both strings are consumed.
*/

void mcError_errorStringsAt2 (DynamicStrings_String s1, DynamicStrings_String s2, unsigned int tok1, unsigned int tok2)
{
  mcError_error e;

  if (s1 == s2)
    s2 = DynamicStrings_Dup (s1);
  e = mcError_newError (tok1);
  mcError_errorString (e, s1);
  mcError_errorString (mcError_chainError (tok2, e), s2);
}


/*
   warnStringAt - given an error string, s, it places this
                  string at token position, tok.
                  The string is consumed.
*/

void mcError_warnStringAt (DynamicStrings_String s, unsigned int tok)
{
  mcError_error e;

  e = mcError_newWarning (tok);
  mcError_errorString (e, s);
}


/*
   warnStringAt2 - given an warning string, s, it places this
                   string at token positions, tok1 and tok2, respectively.
                   The string is consumed.
*/

void mcError_warnStringAt2 (DynamicStrings_String s, unsigned int tok1, unsigned int tok2)
{
  mcError_warnStringsAt2 (s, s, tok1, tok2);
}


/*
   warnStringsAt2 - given warning strings, s1, and, s2, it places these
                    strings at token positions, tok1 and tok2, respectively.
                    Both strings are consumed.
*/

void mcError_warnStringsAt2 (DynamicStrings_String s1, DynamicStrings_String s2, unsigned int tok1, unsigned int tok2)
{
  mcError_error e;

  if (s1 == s2)
    s2 = DynamicStrings_Dup (s1);
  e = mcError_newWarning (tok1);
  mcError_errorString (e, s1);
  mcError_errorString (mcError_chainError (tok2, e), s2);
}

void mcError_warnFormat0 (char *a_, unsigned int _a_high)
{
  mcError_error e;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  /* 
   WarnFormat0 - displays the source module and line together
                 with the encapsulated format string.
                 Used for simple warning messages tied to the current token.
  */
  e = mcError_newWarning (mcLexBuf_getTokenNo ());
  e->s = FormatStrings_Sprintf0 (DynamicStrings_Mark (DynamicStrings_InitString ((char *) a, _a_high)));
}


/*
   warnFormat1 - displays the source module and line together
                 with the encapsulated format string.
                 Used for simple warning messages tied to the current token.
*/

void mcError_warnFormat1 (char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high)
{
  mcError_error e;
  char a[_a_high+1];
  unsigned char w[_w_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (w, w_, _w_high+1);

  e = mcError_newWarning (mcLexBuf_getTokenNo ());
  e->s = doFormat1 ((char *) a, _a_high, (unsigned char *) w, _w_high);
}


/*
   flushErrors - switches the output channel to the error channel
                 and then writes out all errors.
*/

void mcError_flushErrors (void)
{
  if (DebugTrace)
    {
      mcPrintf_printf0 ((char *) "\\nFlushing all errors\\n", 23);
      mcPrintf_printf0 ((char *) "===================\\n", 21);
    }
  if (flushAll (head, TRUE))
    {
      M2RTS_ExitOnHalt (1);
      M2RTS_HALT (-1);
    }
}


/*
   flushWarnings - switches the output channel to the error channel
                   and then writes out all warnings.
                   If an error is present the compilation is terminated,
                   if warnings only were emitted then compilation will
                   continue.
*/

void mcError_flushWarnings (void)
{
  if (flushAll (head, FALSE))
    {}  /* empty.  */
}


/*
   errorAbort0 - aborts compiling, it flushes all warnings and errors before aborting.
*/

void mcError_errorAbort0 (char *a_, unsigned int _a_high)
{
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  mcError_flushWarnings ();
  if (! (StrLib_StrEqual ((char *) a, _a_high, (char *) "", 0)))
    mcError_writeFormat0 ((char *) a, _a_high);
  if (! (flushAll (head, TRUE)))
    {
      mcError_writeFormat0 ((char *) "unidentified error", 18);
      if (flushAll (head, TRUE))
        {}  /* empty.  */
    }
  M2RTS_ExitOnHalt (1);
  M2RTS_HALT (-1);
}

void _M2_mcError_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
  init ();
}

void _M2_mcError_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
