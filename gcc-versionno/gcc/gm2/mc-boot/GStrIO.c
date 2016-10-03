/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/StrIO.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   if !defined (FALSE)
#      define FALSE (1==0)
#   endif

#include <string.h>
#include <limits.h>
#define _StrIO_H
#define _StrIO_C

#   include "GASCII.h"
#   include "GStdIO.h"
#   include "Glibc.h"

static unsigned int IsATTY;
void StrIO_WriteLn (void);
void StrIO_ReadString (char *a, unsigned int _a_high);
void StrIO_WriteString (char *a_, unsigned int _a_high);
static void Erase (void);
static void Echo (char ch);
static unsigned int AlphaNum (char ch);

static void Erase (void)
{
  Echo (ASCII_bs);
  Echo (' ');
  Echo (ASCII_bs);
}

static void Echo (char ch)
{
  if (IsATTY)
    StdIO_Write (ch);
}

static unsigned int AlphaNum (char ch)
{
  return (((ch >= 'a') && (ch <= 'z')) || ((ch >= 'A') && (ch <= 'Z'))) || ((ch >= '0') && (ch <= '9'));
}

void StrIO_WriteLn (void)
{
  Echo (ASCII_cr);
  StdIO_Write (ASCII_lf);
}

void StrIO_ReadString (char *a, unsigned int _a_high)
{
  unsigned int n;
  unsigned int high;
  char ch;

  high = _a_high;
  n = 0;
  do {
    StdIO_Read (&ch);
    if ((ch == ASCII_del) || (ch == ASCII_bs))
      if (n == 0)
        StdIO_Write (ASCII_bel);
      else
        {
          Erase ();
          n -= 1;
        }
    else if (ch == ASCII_nak)
      while (n > 0)
        {
          Erase ();
          n -= 1;
        }
    else if (ch == ASCII_etb)
      if (n == 0)
        Echo (ASCII_bel);
      else if (AlphaNum (a[n-1]))
        do {
          Erase ();
          n -= 1;
        } while (! ((n == 0) || (! (AlphaNum (a[n-1])))));
      else
        {
          Erase ();
          n -= 1;
        }
    else if (n <= high)
      if ((ch == ASCII_cr) || (ch == ASCII_lf))
        {
          a[n] = ASCII_nul;
          n += 1;
        }
      else if (ch == ASCII_ff)
        {
          a[0] = ch;
          if (high > 0)
            a[1] = ASCII_nul;
          ch = ASCII_cr;
        }
      else if (ch >= ' ')
        {
          Echo (ch);
          a[n] = ch;
          n += 1;
        }
      else if (ch == ASCII_eof)
        {
          a[n] = ch;
          n += 1;
          ch = ASCII_cr;
          if (n <= high)
            a[n] = ASCII_nul;
        }
    else if (ch != ASCII_cr)
      Echo (ASCII_bel);
  } while (! ((ch == ASCII_cr) || (ch == ASCII_lf)));
}

void StrIO_WriteString (char *a_, unsigned int _a_high)
{
  unsigned int n;
  unsigned int high;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  high = _a_high;
  n = 0;
  while ((n <= high) && (a[n] != ASCII_nul))
    {
      StdIO_Write (a[n]);
      n += 1;
    }
}

void _M2_StrIO_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
  IsATTY = FALSE;
}

void _M2_StrIO_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
