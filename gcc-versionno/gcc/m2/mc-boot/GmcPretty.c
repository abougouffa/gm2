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
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

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
#define _mcPretty_H
#define _mcPretty_C

#   include "GDynamicStrings.h"
#   include "GStorage.h"

typedef struct mcPretty_writeProc_p mcPretty_writeProc;

typedef struct mcPretty_writeLnProc_p mcPretty_writeLnProc;

typedef struct _T1_r _T1;

typedef _T1 *mcPretty_pretty;

typedef void (*mcPretty_writeProc_t) (char);
struct mcPretty_writeProc_p { mcPretty_writeProc_t proc; };

typedef void (*mcPretty_writeLnProc_t) (void);
struct mcPretty_writeLnProc_p { mcPretty_writeLnProc_t proc; };

struct _T1_r {
               mcPretty_writeProc write_;
               mcPretty_writeLnProc writeln;
               unsigned int needsSpace;
               unsigned int needsIndent;
               unsigned int seekPos;
               unsigned int curLine;
               unsigned int curPos;
               unsigned int indent;
               mcPretty_pretty stacked;
             };


/*
   initPretty - initialise a pretty print data structure.
*/

mcPretty_pretty mcPretty_initPretty (mcPretty_writeProc w, mcPretty_writeLnProc l);

/*
   dupPretty - duplicate a pretty print data structure.
*/

mcPretty_pretty mcPretty_dupPretty (mcPretty_pretty p);

/*
   killPretty - destroy a pretty print data structure.
                Post condition:  p is assigned to NIL.
*/

void mcPretty_killPretty (mcPretty_pretty *p);

/*
   pushPretty - duplicate, p.  Push, p, and return the duplicate.
*/

mcPretty_pretty mcPretty_pushPretty (mcPretty_pretty p);

/*
   popPretty - pops the pretty object from the stack.
*/

mcPretty_pretty mcPretty_popPretty (mcPretty_pretty p);

/*
   getindent - returns the current indent value.
*/

unsigned int mcPretty_getindent (mcPretty_pretty p);

/*
   setindent - sets the current indent to, n.
*/

void mcPretty_setindent (mcPretty_pretty p, unsigned int n);

/*
   getcurpos - returns the current cursor position.
*/

unsigned int mcPretty_getcurpos (mcPretty_pretty s);

/*
   getseekpos - returns the seek position.
*/

unsigned int mcPretty_getseekpos (mcPretty_pretty s);

/*
   getcurline - returns the current line number.
*/

unsigned int mcPretty_getcurline (mcPretty_pretty s);
void mcPretty_setNeedSpace (mcPretty_pretty s);

/*
   noSpace - unset needsSpace.
*/

void mcPretty_noSpace (mcPretty_pretty s);

/*
   print - print a string using, p.
*/

void mcPretty_print (mcPretty_pretty p, char *a_, unsigned int _a_high);

/*
   prints - print a string using, p.
*/

void mcPretty_prints (mcPretty_pretty p, DynamicStrings_String s);

/*
   raw - print out string, s, without any translation of
         escape sequences.
*/

void mcPretty_raw (mcPretty_pretty p, DynamicStrings_String s);

/*
   flushSpace -
*/

static void flushSpace (mcPretty_pretty p);

/*
   flushIndent -
*/

static void flushIndent (mcPretty_pretty p);


/*
   flushSpace -
*/

static void flushSpace (mcPretty_pretty p)
{
  if (p->needsSpace)
    {
      (*p->write_.proc) (' ');
      p->needsSpace = FALSE;
      p->curPos += 1;
      p->seekPos += 1;
    }
}


/*
   flushIndent -
*/

static void flushIndent (mcPretty_pretty p)
{
  unsigned int i;

  flushSpace (p);
  if (p->needsIndent)
    {
      while (p->curPos < p->indent)
        {
          (*p->write_.proc) (' ');
          p->curPos += 1;
          p->seekPos += 1;
        }
      p->needsIndent = FALSE;
    }
}


/*
   initPretty - initialise a pretty print data structure.
*/

mcPretty_pretty mcPretty_initPretty (mcPretty_writeProc w, mcPretty_writeLnProc l)
{
  mcPretty_pretty p;

  Storage_ALLOCATE ((void **) &p, sizeof (_T1));
  p->write_ = w;
  p->writeln = l;
  p->needsSpace = FALSE;
  p->needsIndent = FALSE;
  p->curPos = 0;
  p->curLine = 0;
  p->seekPos = 0;
  p->indent = 0;
  p->stacked = NULL;
  return p;
}


/*
   dupPretty - duplicate a pretty print data structure.
*/

mcPretty_pretty mcPretty_dupPretty (mcPretty_pretty p)
{
  mcPretty_pretty q;

  Storage_ALLOCATE ((void **) &q, sizeof (_T1));
  (*q) = (*p);
  return q;
}


/*
   killPretty - destroy a pretty print data structure.
                Post condition:  p is assigned to NIL.
*/

void mcPretty_killPretty (mcPretty_pretty *p)
{
  Storage_DEALLOCATE ((void **) &(*p), sizeof (_T1));
  (*p) = NULL;
}


/*
   pushPretty - duplicate, p.  Push, p, and return the duplicate.
*/

mcPretty_pretty mcPretty_pushPretty (mcPretty_pretty p)
{
  mcPretty_pretty q;

  q = mcPretty_dupPretty (p);
  q->stacked = p;
  return q;
}


/*
   popPretty - pops the pretty object from the stack.
*/

mcPretty_pretty mcPretty_popPretty (mcPretty_pretty p)
{
  mcPretty_pretty q;

  q = p->stacked;
  q->needsIndent = p->needsIndent;
  q->needsSpace = p->needsSpace;
  q->curPos = p->curPos;
  q->seekPos = p->seekPos;
  q->curLine = p->curLine;
  mcPretty_killPretty (&p);
  return q;
}


/*
   getindent - returns the current indent value.
*/

unsigned int mcPretty_getindent (mcPretty_pretty p)
{
  return p->indent;
}


/*
   setindent - sets the current indent to, n.
*/

void mcPretty_setindent (mcPretty_pretty p, unsigned int n)
{
  p->indent = n;
}


/*
   getcurpos - returns the current cursor position.
*/

unsigned int mcPretty_getcurpos (mcPretty_pretty s)
{
  if (s->needsSpace)
    {
      return s->curPos+1;
    }
  else
    {
      return s->curPos;
    }
}


/*
   getseekpos - returns the seek position.
*/

unsigned int mcPretty_getseekpos (mcPretty_pretty s)
{
  return s->seekPos;
}


/*
   getcurline - returns the current line number.
*/

unsigned int mcPretty_getcurline (mcPretty_pretty s)
{
  return s->curLine;
}

void mcPretty_setNeedSpace (mcPretty_pretty s)
{
  /* 
   setneedSpace - sets needSpace flag to TRUE.
  */
  s->needsSpace = TRUE;
}


/*
   noSpace - unset needsSpace.
*/

void mcPretty_noSpace (mcPretty_pretty s)
{
  s->needsSpace = FALSE;
}


/*
   print - print a string using, p.
*/

void mcPretty_print (mcPretty_pretty p, char *a_, unsigned int _a_high)
{
  DynamicStrings_String s;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  s = DynamicStrings_InitString ((char *) a, _a_high);
  mcPretty_prints (p, s);
  s = DynamicStrings_KillString (s);
}


/*
   prints - print a string using, p.
*/

void mcPretty_prints (mcPretty_pretty p, DynamicStrings_String s)
{
  unsigned int l;
  unsigned int i;

  l = DynamicStrings_Length (s);
  i = 0;
  flushSpace (p);
  while (i < l)
    {
      if ((((i+2) <= l) && ((DynamicStrings_char (s, (int) i)) == '\\')) && ((DynamicStrings_char (s, (int) i+1)) == 'n'))
        {
          p->needsIndent = TRUE;
          p->needsSpace = FALSE;
          p->curPos = 0;
          (*p->writeln.proc) ();
          p->seekPos += 1;
          p->curLine += 1;
          i += 1;
        }
      else
        {
          flushIndent (p);
          (*p->write_.proc) (DynamicStrings_char (s, (int) i));
          p->curPos += 1;
          p->seekPos += 1;
        }
      i += 1;
    }
}


/*
   raw - print out string, s, without any translation of
         escape sequences.
*/

void mcPretty_raw (mcPretty_pretty p, DynamicStrings_String s)
{
  unsigned int l;
  unsigned int i;

  l = DynamicStrings_Length (s);
  i = 0;
  flushSpace (p);
  flushIndent (p);
  while (i < l)
    {
      (*p->write_.proc) (DynamicStrings_char (s, (int) i));
      p->curPos += 1;
      p->seekPos += 1;
      i += 1;
    }
}

void _M2_mcPretty_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_mcPretty_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
