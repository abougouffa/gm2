/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcPretty.mod.  */

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

mcPretty_pretty mcPretty_initPretty (mcPretty_writeProc w, mcPretty_writeLnProc l);
mcPretty_pretty mcPretty_dupPretty (mcPretty_pretty p);
void mcPretty_killPretty (mcPretty_pretty *p);
mcPretty_pretty mcPretty_pushPretty (mcPretty_pretty p);
mcPretty_pretty mcPretty_popPretty (mcPretty_pretty p);
unsigned int mcPretty_getindent (mcPretty_pretty p);
void mcPretty_setindent (mcPretty_pretty p, unsigned int n);
unsigned int mcPretty_getcurpos (mcPretty_pretty s);
unsigned int mcPretty_getseekpos (mcPretty_pretty s);
unsigned int mcPretty_getcurline (mcPretty_pretty s);
void mcPretty_setNeedSpace (mcPretty_pretty s);
void mcPretty_noSpace (mcPretty_pretty s);
void mcPretty_print (mcPretty_pretty p, char *a_, unsigned int _a_high);
void mcPretty_prints (mcPretty_pretty p, DynamicStrings_String s);
void mcPretty_raw (mcPretty_pretty p, DynamicStrings_String s);
static void flushSpace (mcPretty_pretty p);
static void flushIndent (mcPretty_pretty p);

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

mcPretty_pretty mcPretty_dupPretty (mcPretty_pretty p)
{
  mcPretty_pretty q;

  Storage_ALLOCATE ((void **) &q, sizeof (_T1));
  (*q) = (*p);
  return q;
}

void mcPretty_killPretty (mcPretty_pretty *p)
{
  Storage_DEALLOCATE ((void **) &(*p), sizeof (_T1));
  (*p) = NULL;
}

mcPretty_pretty mcPretty_pushPretty (mcPretty_pretty p)
{
  mcPretty_pretty q;

  q = mcPretty_dupPretty (p);
  q->stacked = p;
  return q;
}

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

unsigned int mcPretty_getindent (mcPretty_pretty p)
{
  return p->indent;
}

void mcPretty_setindent (mcPretty_pretty p, unsigned int n)
{
  p->indent = n;
}

unsigned int mcPretty_getcurpos (mcPretty_pretty s)
{
  if (s->needsSpace)
    return s->curPos+1;
  else
    return s->curPos;
}

unsigned int mcPretty_getseekpos (mcPretty_pretty s)
{
  return s->seekPos;
}

unsigned int mcPretty_getcurline (mcPretty_pretty s)
{
  return s->curLine;
}

void mcPretty_setNeedSpace (mcPretty_pretty s)
{
  s->needsSpace = TRUE;
}

void mcPretty_noSpace (mcPretty_pretty s)
{
  s->needsSpace = FALSE;
}

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

void mcPretty_prints (mcPretty_pretty p, DynamicStrings_String s)
{
  unsigned int l;
  unsigned int i;

  l = DynamicStrings_Length (s);
  i = 0;
  flushSpace (p);
  while (i < l)
    {
      if ((((i+2) <= l) && ((DynamicStrings_char (s, (int ) i)) == '\\')) && ((DynamicStrings_char (s, (int ) i+1)) == 'n'))
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
          (*p->write_.proc) (DynamicStrings_char (s, (int ) i));
          p->curPos += 1;
          p->seekPos += 1;
        }
      i += 1;
    }
}

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
      (*p->write_.proc) (DynamicStrings_char (s, (int ) i));
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
