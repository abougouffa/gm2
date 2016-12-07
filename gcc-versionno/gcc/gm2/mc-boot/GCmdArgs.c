/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/CmdArgs.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#include <string.h>
#include <limits.h>
#define _CmdArgs_H
#define _CmdArgs_C

#   include "GASCII.h"
#   include "GStrLib.h"

#   define esc '\\'
#   define space ' '
#   define squote '\''
#   define dquote '"'
#   define tab ' '
unsigned int CmdArgs_GetArg (char *CmdLine_, unsigned int _CmdLine_high, unsigned int n, char *Argi, unsigned int _Argi_high);
unsigned int CmdArgs_Narg (char *CmdLine_, unsigned int _CmdLine_high);
static unsigned int GetNextArg (char *CmdLine_, unsigned int _CmdLine_high, unsigned int *CmdIndex, char *Arg, unsigned int _Arg_high);
static void CopyUntilSpace (char *From_, unsigned int _From_high, unsigned int *FromIndex, unsigned int FromHigh, char *To, unsigned int _To_high, unsigned int *ToIndex, unsigned int ToHigh);
static void CopyUntil (char *From_, unsigned int _From_high, unsigned int *FromIndex, unsigned int FromHigh, char *To, unsigned int _To_high, unsigned int *ToIndex, unsigned int ToHigh, char UntilChar);
static void CopyChar (char *From_, unsigned int _From_high, unsigned int *FromIndex, unsigned int FromHigh, char *To, unsigned int _To_high, unsigned int *ToIndex, unsigned int ToHigh);
static unsigned int Escape (char ch);
static unsigned int Space (char ch);
static unsigned int DoubleQuote (char ch);
static unsigned int SingleQuote (char ch);

static unsigned int GetNextArg (char *CmdLine_, unsigned int _CmdLine_high, unsigned int *CmdIndex, char *Arg, unsigned int _Arg_high)
{
  unsigned int ArgIndex;
  unsigned int HighA;
  unsigned int HighC;
  char CmdLine[_CmdLine_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (CmdLine, CmdLine_, _CmdLine_high+1);

  HighA = _Arg_high;
  HighC = StrLib_StrLen ((char *) CmdLine, _CmdLine_high);
  ArgIndex = 0;
  while (((*CmdIndex) < HighC) && (Space (CmdLine[(*CmdIndex)])))
    (*CmdIndex) += 1;
  if ((*CmdIndex) < HighC)
    {
      /* avoid gcc warning by using compound statement even if not strictly necessary.  */
      if (SingleQuote (CmdLine[(*CmdIndex)]))
        {
          (*CmdIndex) += 1;
          CopyUntil ((char *) CmdLine, _CmdLine_high, CmdIndex, HighC, (char *) Arg, _Arg_high, &ArgIndex, HighA, squote);
          (*CmdIndex) += 1;
        }
      else if (DoubleQuote (CmdLine[(*CmdIndex)]))
        {
          (*CmdIndex) += 1;
          CopyUntil ((char *) CmdLine, _CmdLine_high, CmdIndex, HighC, (char *) Arg, _Arg_high, &ArgIndex, HighA, dquote);
          (*CmdIndex) += 1;
        }
      else
        CopyUntilSpace ((char *) CmdLine, _CmdLine_high, CmdIndex, HighC, (char *) Arg, _Arg_high, &ArgIndex, HighA);
    }
  while (((*CmdIndex) < HighC) && (Space (CmdLine[(*CmdIndex)])))
    (*CmdIndex) += 1;
  if (ArgIndex < HighA)
    Arg[ArgIndex] = ASCII_nul;
  return (*CmdIndex) < HighC;
}

static void CopyUntilSpace (char *From_, unsigned int _From_high, unsigned int *FromIndex, unsigned int FromHigh, char *To, unsigned int _To_high, unsigned int *ToIndex, unsigned int ToHigh)
{
  char From[_From_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (From, From_, _From_high+1);

  while ((((*FromIndex) < FromHigh) && ((*ToIndex) < ToHigh)) && (! (Space (From[(*FromIndex)]))))
    CopyChar ((char *) From, _From_high, FromIndex, FromHigh, (char *) To, _To_high, ToIndex, ToHigh);
}

static void CopyUntil (char *From_, unsigned int _From_high, unsigned int *FromIndex, unsigned int FromHigh, char *To, unsigned int _To_high, unsigned int *ToIndex, unsigned int ToHigh, char UntilChar)
{
  char From[_From_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (From, From_, _From_high+1);

  while ((((*FromIndex) < FromHigh) && ((*ToIndex) < ToHigh)) && (From[(*FromIndex)] != UntilChar))
    CopyChar ((char *) From, _From_high, FromIndex, FromHigh, (char *) To, _To_high, ToIndex, ToHigh);
}

static void CopyChar (char *From_, unsigned int _From_high, unsigned int *FromIndex, unsigned int FromHigh, char *To, unsigned int _To_high, unsigned int *ToIndex, unsigned int ToHigh)
{
  char From[_From_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (From, From_, _From_high+1);

  if (((*FromIndex) < FromHigh) && ((*ToIndex) < ToHigh))
    {
      if (Escape (From[(*FromIndex)]))
        (*FromIndex) += 1;
      if ((*FromIndex) < FromHigh)
        {
          To[(*ToIndex)] = From[(*FromIndex)];
          (*ToIndex) += 1;
          (*FromIndex) += 1;
        }
    }
}

static unsigned int Escape (char ch)
{
  return ch == esc;
}

static unsigned int Space (char ch)
{
  return (ch == space) || (ch == tab);
}

static unsigned int DoubleQuote (char ch)
{
  return ch == dquote;
}

static unsigned int SingleQuote (char ch)
{
  return ch == squote;
}

unsigned int CmdArgs_GetArg (char *CmdLine_, unsigned int _CmdLine_high, unsigned int n, char *Argi, unsigned int _Argi_high)
{
  unsigned int Index;
  unsigned int i;
  unsigned int Another;
  char CmdLine[_CmdLine_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (CmdLine, CmdLine_, _CmdLine_high+1);

  Index = 0;
  i = 0;
  do {
    Another = GetNextArg ((char *) CmdLine, _CmdLine_high, &Index, (char *) Argi, _Argi_high);
    i += 1;
  } while (! ((i > n) || ! Another));
  return i > n;
}

unsigned int CmdArgs_Narg (char *CmdLine_, unsigned int _CmdLine_high)
{
  typedef struct _T1_a _T1;

  struct _T1_a { char array[1000+1]; };
  _T1 a;
  unsigned int ArgNo;
  char CmdLine[_CmdLine_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (CmdLine, CmdLine_, _CmdLine_high+1);

  ArgNo = 0;
  while (CmdArgs_GetArg ((char *) CmdLine, _CmdLine_high, ArgNo, (char *) &a.array[0], 1000))
    ArgNo += 1;
  return ArgNo;
}

void _M2_CmdArgs_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_CmdArgs_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
