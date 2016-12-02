/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/StrLib.mod.  */

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

#include <string.h>
#include <limits.h>
#include "Gmcrts.h"
#define _StrLib_H
#define _StrLib_C

#   include "GASCII.h"

void StrLib_StrConCat (char *a_, unsigned int _a_high, char *b_, unsigned int _b_high, char *c, unsigned int _c_high);
unsigned int StrLib_StrLess (char *a_, unsigned int _a_high, char *b_, unsigned int _b_high);
unsigned int StrLib_StrEqual (char *a_, unsigned int _a_high, char *b_, unsigned int _b_high);
unsigned int StrLib_StrLen (char *a_, unsigned int _a_high);
void StrLib_StrCopy (char *a_, unsigned int _a_high, char *b, unsigned int _b_high);
unsigned int StrLib_IsSubString (char *a_, unsigned int _a_high, char *b_, unsigned int _b_high);
void StrLib_StrRemoveWhitePrefix (char *a_, unsigned int _a_high, char *b, unsigned int _b_high);
static unsigned int IsWhite (char ch);

static unsigned int IsWhite (char ch)
{
  return (ch == ' ') || (ch == ASCII_tab);
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/StrLib.def", 20, 0);
}

void StrLib_StrConCat (char *a_, unsigned int _a_high, char *b_, unsigned int _b_high, char *c, unsigned int _c_high)
{
  unsigned int Highb;
  unsigned int Highc;
  unsigned int i;
  unsigned int j;
  char a[_a_high+1];
  char b[_b_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (b, b_, _b_high+1);

  Highb = StrLib_StrLen ((char *) b, _b_high);
  Highc = _c_high;
  StrLib_StrCopy ((char *) a, _a_high, (char *) c, _c_high);
  i = StrLib_StrLen ((char *) c, _c_high);
  j = 0;
  while ((j < Highb) && (i <= Highc))
    {
      c[i] = b[j];
      i += 1;
      j += 1;
    }
  if (i <= Highc)
    c[i] = ASCII_nul;
}

unsigned int StrLib_StrLess (char *a_, unsigned int _a_high, char *b_, unsigned int _b_high)
{
  unsigned int Higha;
  unsigned int Highb;
  unsigned int i;
  char a[_a_high+1];
  char b[_b_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (b, b_, _b_high+1);

  Higha = StrLib_StrLen ((char *) a, _a_high);
  Highb = StrLib_StrLen ((char *) b, _b_high);
  i = 0;
  while ((i < Higha) && (i < Highb))
    {
      if (a[i] < b[i])
        return TRUE;
      else if (a[i] > b[i])
        return FALSE;
      i += 1;
    }
  return Higha < Highb;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/StrLib.def", 20, 0);
}

unsigned int StrLib_StrEqual (char *a_, unsigned int _a_high, char *b_, unsigned int _b_high)
{
  unsigned int i;
  unsigned int higha;
  unsigned int highb;
  char a[_a_high+1];
  char b[_b_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (b, b_, _b_high+1);

  higha = _a_high;
  highb = _b_high;
  i = 0;
  while ((((i <= higha) && (i <= highb)) && (a[i] != ASCII_nul)) && (b[i] != ASCII_nul))
    {
      if (a[i] != b[i])
        return FALSE;
      i += 1;
    }
  return ! (((i <= higha) && (a[i] != ASCII_nul)) || ((i <= highb) && (b[i] != ASCII_nul)));
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/StrLib.def", 20, 0);
}

unsigned int StrLib_StrLen (char *a_, unsigned int _a_high)
{
  unsigned int High;
  unsigned int Len;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  Len = 0;
  High = _a_high;
  while ((Len <= High) && (a[Len] != ASCII_nul))
    Len += 1;
  return Len;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/StrLib.def", 20, 0);
}

void StrLib_StrCopy (char *a_, unsigned int _a_high, char *b, unsigned int _b_high)
{
  unsigned int Higha;
  unsigned int Highb;
  unsigned int n;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  n = 0;
  Higha = StrLib_StrLen ((char *) a, _a_high);
  Highb = _b_high;
  while ((n < Higha) && (n <= Highb))
    {
      b[n] = a[n];
      n += 1;
    }
  if (n <= Highb)
    b[n] = ASCII_nul;
}

unsigned int StrLib_IsSubString (char *a_, unsigned int _a_high, char *b_, unsigned int _b_high)
{
  unsigned int i;
  unsigned int j;
  unsigned int LengthA;
  unsigned int LengthB;
  char a[_a_high+1];
  char b[_b_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);
  memcpy (b, b_, _b_high+1);

  LengthA = StrLib_StrLen ((char *) a, _a_high);
  LengthB = StrLib_StrLen ((char *) b, _b_high);
  i = 0;
  if (LengthA > LengthB)
    while (i <= (LengthA-LengthB))
      {
        j = 0;
        while ((j < LengthB) && (a[i+j] == b[j]))
          j += 1;
        if (j == LengthB)
          return TRUE;
        else
          i += 1;
      }
  return FALSE;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/StrLib.def", 20, 0);
}

void StrLib_StrRemoveWhitePrefix (char *a_, unsigned int _a_high, char *b, unsigned int _b_high)
{
  unsigned int i;
  unsigned int j;
  unsigned int higha;
  unsigned int highb;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  i = 0;
  j = 0;
  higha = StrLib_StrLen ((char *) a, _a_high);
  highb = _b_high;
  while ((i < higha) && (IsWhite (a[i])))
    i += 1;
  while ((i < higha) && (j <= highb))
    {
      b[j] = a[i];
      i += 1;
      j += 1;
    }
  if (j <= highb)
    b[j] = ASCII_nul;
}

void _M2_StrLib_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_StrLib_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
