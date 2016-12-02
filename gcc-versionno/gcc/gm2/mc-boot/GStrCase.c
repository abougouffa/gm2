/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/StrCase.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#include <string.h>
#include <limits.h>
#include "Gmcrts.h"
#define _StrCase_H
#define _StrCase_C

#   include "GASCII.h"
#   include "GStrLib.h"

void StrCase_StrToUpperCase (char *a_, unsigned int _a_high, char *b, unsigned int _b_high);
void StrCase_StrToLowerCase (char *a_, unsigned int _a_high, char *b, unsigned int _b_high);
char StrCase_Cap (char ch);
char StrCase_Lower (char ch);

void StrCase_StrToUpperCase (char *a_, unsigned int _a_high, char *b, unsigned int _b_high)
{
  unsigned int higha;
  unsigned int highb;
  unsigned int i;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  higha = StrLib_StrLen ((char *) a, _a_high);
  highb = _b_high;
  i = 0;
  while (((i < higha) && (a[i] != ASCII_nul)) && (i < highb))
    {
      b[i] = StrCase_Cap (a[i]);
      i += 1;
    }
  if (i < highb)
    b[i] = ASCII_nul;
}

void StrCase_StrToLowerCase (char *a_, unsigned int _a_high, char *b, unsigned int _b_high)
{
  unsigned int higha;
  unsigned int highb;
  unsigned int i;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  higha = StrLib_StrLen ((char *) a, _a_high);
  highb = _b_high;
  i = 0;
  while (((i < higha) && (a[i] != ASCII_nul)) && (i < highb))
    {
      b[i] = StrCase_Lower (a[i]);
      i += 1;
    }
  if (i < highb)
    b[i] = ASCII_nul;
}

char StrCase_Cap (char ch)
{
  if ((ch >= 'a') && (ch <= 'z'))
    ch = (char) ((((unsigned int) (ch))-((unsigned int) ('a')))+((unsigned int) ('A')));
  return ch;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/StrCase.def", 20, 0);
}

char StrCase_Lower (char ch)
{
  if ((ch >= 'A') && (ch <= 'Z'))
    ch = (char) ((((unsigned int) (ch))-((unsigned int) ('A')))+((unsigned int) ('a')));
  return ch;
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/StrCase.def", 20, 0);
}

void _M2_StrCase_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_StrCase_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
