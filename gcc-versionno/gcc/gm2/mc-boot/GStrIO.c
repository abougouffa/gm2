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
  } while (! ((ch == ASCII_cr) || (ch == ASCII_lf)));
}

void StrIO_WriteString (char *a_, unsigned int _a_high)
{
  unsigned int n;
  unsigned int high;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high);

  high = _a_high;
  n = 0;
  while ((n <= high) && (a[n] != ASCII_nul))
    {
      StdIO_Write (a[n]);
      n += 1;
    }
}

void _M2_StrIO_init (int argc, char *argv[])
{
  IsATTY = FALSE;
}
