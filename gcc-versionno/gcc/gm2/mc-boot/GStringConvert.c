/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/StringConvert.mod.  */

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
#include <stdlib.h>
#define _StringConvert_H
#define _StringConvert_C

#   include "GSYSTEM.h"
#   include "Glibc.h"
#   include "Glibm.h"
#   include "GM2RTS.h"
#   include "GDynamicStrings.h"
#   include "Gldtoa.h"
#   include "Gdtoa.h"

DynamicStrings_String StringConvert_IntegerToString (int i, unsigned int width, char padding, unsigned int sign, unsigned int base, unsigned int lower);
DynamicStrings_String StringConvert_CardinalToString (unsigned int c, unsigned int width, char padding, unsigned int base, unsigned int lower);
int StringConvert_StringToInteger (DynamicStrings_String s, unsigned int base, unsigned int *found);
unsigned int StringConvert_StringToCardinal (DynamicStrings_String s, unsigned int base, unsigned int *found);
DynamicStrings_String StringConvert_LongIntegerToString (long int i, unsigned int width, char padding, unsigned int sign, unsigned int base, unsigned int lower);
long int StringConvert_StringToLongInteger (DynamicStrings_String s, unsigned int base, unsigned int *found);
DynamicStrings_String StringConvert_LongCardinalToString (long unsigned int c, unsigned int width, char padding, unsigned int base, unsigned int lower);
long unsigned int StringConvert_StringToLongCardinal (DynamicStrings_String s, unsigned int base, unsigned int *found);
DynamicStrings_String StringConvert_ShortCardinalToString (short unsigned int c, unsigned int width, char padding, unsigned int base, unsigned int lower);
short unsigned int StringConvert_StringToShortCardinal (DynamicStrings_String s, unsigned int base, unsigned int *found);
int StringConvert_stoi (DynamicStrings_String s);
DynamicStrings_String StringConvert_itos (int i, unsigned int width, char padding, unsigned int sign);
DynamicStrings_String StringConvert_ctos (unsigned int c, unsigned int width, char padding);
unsigned int StringConvert_stoc (DynamicStrings_String s);
int StringConvert_hstoi (DynamicStrings_String s);
int StringConvert_ostoi (DynamicStrings_String s);
int StringConvert_bstoi (DynamicStrings_String s);
unsigned int StringConvert_hstoc (DynamicStrings_String s);
unsigned int StringConvert_ostoc (DynamicStrings_String s);
unsigned int StringConvert_bstoc (DynamicStrings_String s);
long double StringConvert_StringToLongreal (DynamicStrings_String s, unsigned int *found);
DynamicStrings_String StringConvert_LongrealToString (long double x, unsigned int TotalWidth, unsigned int FractionWidth);
double StringConvert_stor (DynamicStrings_String s);
long double StringConvert_stolr (DynamicStrings_String s);
DynamicStrings_String StringConvert_ToSigFig (DynamicStrings_String s, unsigned int n);
DynamicStrings_String StringConvert_ToDecimalPlaces (DynamicStrings_String s, unsigned int n);
static void Assert (unsigned int b, char *file_, unsigned int _file_high, unsigned int line, char *func_, unsigned int _func_high);
static unsigned int Max (unsigned int a, unsigned int b);
static unsigned int Min (unsigned int a, unsigned int b);
static long unsigned int LongMin (long unsigned int a, long unsigned int b);
static unsigned int IsDigit (char ch);
static unsigned int IsDecimalDigitValid (char ch, unsigned int base, unsigned int *c);
static unsigned int IsHexidecimalDigitValid (char ch, unsigned int base, unsigned int *c);
static unsigned int IsDecimalDigitValidLong (char ch, unsigned int base, long unsigned int *c);
static unsigned int IsHexidecimalDigitValidLong (char ch, unsigned int base, long unsigned int *c);
static unsigned int IsDecimalDigitValidShort (char ch, unsigned int base, short unsigned int *c);
static unsigned int IsHexidecimalDigitValidShort (char ch, unsigned int base, short unsigned int *c);
static long double ToThePower10 (long double v, int power);
static unsigned int DetermineSafeTruncation (void);
static DynamicStrings_String rtos (double r, unsigned int TotalWidth, unsigned int FractionWidth);
static DynamicStrings_String lrtos (long double r, unsigned int TotalWidth, unsigned int FractionWidth);
static DynamicStrings_String doDecimalPlaces (DynamicStrings_String s, unsigned int n);
static DynamicStrings_String doSigFig (DynamicStrings_String s, unsigned int n);
static DynamicStrings_String carryOne (DynamicStrings_String s, unsigned int i);

static void Assert (unsigned int b, char *file_, unsigned int _file_high, unsigned int line, char *func_, unsigned int _func_high)
{
  char file[_file_high+1];
  char func[_func_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (file, file_, _file_high+1);
  memcpy (func, func_, _func_high+1);

  if (! b)
    M2RTS_ErrorMessage ((char *) "assert failed", 13, (char *) file, _file_high, line, (char *) func, _func_high);
}

static unsigned int Max (unsigned int a, unsigned int b)
{
  if (a > b)
    return a;
  else
    return b;
}

static unsigned int Min (unsigned int a, unsigned int b)
{
  if (a < b)
    return a;
  else
    return b;
}

static long unsigned int LongMin (long unsigned int a, long unsigned int b)
{
  if (a < b)
    return a;
  else
    return b;
}

static unsigned int IsDigit (char ch)
{
  return (ch >= '0') && (ch <= '9');
}

static unsigned int IsDecimalDigitValid (char ch, unsigned int base, unsigned int *c)
{
  if ((IsDigit (ch)) && ((((unsigned int) (ch))-((unsigned int) ('0'))) < base))
    {
      (*c) = ((*c)*base)+(((unsigned int) (ch))-((unsigned int) ('0')));
      return TRUE;
    }
  else
    return FALSE;
}

static unsigned int IsHexidecimalDigitValid (char ch, unsigned int base, unsigned int *c)
{
  if (((ch >= 'a') && (ch <= 'f')) && (((((unsigned int) (ch))-((unsigned int) ('a')))+10) < base))
    {
      (*c) = ((*c)*base)+((((unsigned int) (ch))-((unsigned int) ('a')))+10);
      return TRUE;
    }
  else if (((ch >= 'A') && (ch <= 'F')) && (((((unsigned int) (ch))-((unsigned int) ('F')))+10) < base))
    {
      (*c) = ((*c)*base)+((((unsigned int) (ch))-((unsigned int) ('A')))+10);
      return TRUE;
    }
  else
    return FALSE;
}

static unsigned int IsDecimalDigitValidLong (char ch, unsigned int base, long unsigned int *c)
{
  if ((IsDigit (ch)) && ((((unsigned int) (ch))-((unsigned int) ('0'))) < base))
    {
      (*c) = (*c)*((long unsigned int ) (base+(((unsigned int) (ch))-((unsigned int) ('0')))));
      return TRUE;
    }
  else
    return FALSE;
}

static unsigned int IsHexidecimalDigitValidLong (char ch, unsigned int base, long unsigned int *c)
{
  if (((ch >= 'a') && (ch <= 'f')) && (((((unsigned int) (ch))-((unsigned int) ('a')))+10) < base))
    {
      (*c) = (*c)*((long unsigned int ) (base+((((unsigned int) (ch))-((unsigned int) ('a')))+10)));
      return TRUE;
    }
  else if (((ch >= 'A') && (ch <= 'F')) && (((((unsigned int) (ch))-((unsigned int) ('F')))+10) < base))
    {
      (*c) = (*c)*((long unsigned int ) (base+((((unsigned int) (ch))-((unsigned int) ('A')))+10)));
      return TRUE;
    }
  else
    return FALSE;
}

static unsigned int IsDecimalDigitValidShort (char ch, unsigned int base, short unsigned int *c)
{
  if ((IsDigit (ch)) && ((((unsigned int) (ch))-((unsigned int) ('0'))) < base))
    {
      (*c) = (*c)*((short unsigned int ) (base+(((unsigned int) (ch))-((unsigned int) ('0')))));
      return TRUE;
    }
  else
    return FALSE;
}

static unsigned int IsHexidecimalDigitValidShort (char ch, unsigned int base, short unsigned int *c)
{
  if (((ch >= 'a') && (ch <= 'f')) && (((((unsigned int) (ch))-((unsigned int) ('a')))+10) < base))
    {
      (*c) = (*c)*((short unsigned int ) (base+((((unsigned int) (ch))-((unsigned int) ('a')))+10)));
      return TRUE;
    }
  else if (((ch >= 'A') && (ch <= 'F')) && (((((unsigned int) (ch))-((unsigned int) ('F')))+10) < base))
    {
      (*c) = (*c)*((short unsigned int ) (base+((((unsigned int) (ch))-((unsigned int) ('A')))+10)));
      return TRUE;
    }
  else
    return FALSE;
}

static long double ToThePower10 (long double v, int power)
{
  int i;

  i = 0;
  if (power > 0)
    while (i < power)
      {
        v = v*10.0;
        i += 1;
      }
  else
    while (i > power)
      {
        v = v/10.0;
        i -= 1;
      }
  return v;
}

static unsigned int DetermineSafeTruncation (void)
{
  double MaxPowerOfTen;
  unsigned int LogPower;

  MaxPowerOfTen = 1.0;
  LogPower = 0;
  while ((MaxPowerOfTen*10.0) < ((double) ((INT_MAX) / 10)))
    {
      MaxPowerOfTen = MaxPowerOfTen*10.0;
      LogPower += 1;
    }
  return LogPower;
}

static DynamicStrings_String rtos (double r, unsigned int TotalWidth, unsigned int FractionWidth)
{
  M2RTS_HALT (0);
  return NULL;
}

static DynamicStrings_String lrtos (long double r, unsigned int TotalWidth, unsigned int FractionWidth)
{
  M2RTS_HALT (0);
  return NULL;
}

static DynamicStrings_String doDecimalPlaces (DynamicStrings_String s, unsigned int n)
{
  int i;
  int l;
  int point;
  DynamicStrings_String t;
  DynamicStrings_String whole;
  DynamicStrings_String fraction;
  DynamicStrings_String tenths;
  DynamicStrings_String hundreths;

  l = DynamicStrings_Length (s);
  i = 0;
  point = DynamicStrings_Index (s, '.', 0);
  if (point == 0)
    s = DynamicStrings_Slice (DynamicStrings_Mark (s), 1, 0);
  else if (point < l)
    s = DynamicStrings_ConCat (DynamicStrings_Slice (DynamicStrings_Mark (s), 0, point), DynamicStrings_Mark (DynamicStrings_Slice (DynamicStrings_Mark (s), point+1, 0)));
  else
    s = DynamicStrings_Slice (DynamicStrings_Mark (s), 0, point);
  l = DynamicStrings_Length (s);
  i = 0;
  if (l > 0)
    {
      while ((i < l) && ((DynamicStrings_char (s, i)) == '0'))
        i += 1;
      if ((i == l) && ((DynamicStrings_char (s, i-1)) == '0'))
        {
          s = DynamicStrings_KillString (s);
          s = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "0.", 2), DynamicStrings_Mark (DynamicStrings_Mult (DynamicStrings_Mark (DynamicStrings_InitStringChar ('0')), n)));
          return s;
        }
    }
  s = DynamicStrings_ConCat (DynamicStrings_InitStringChar ('0'), DynamicStrings_Mark (s));
  point += 1;
  l = DynamicStrings_Length (s);
  i = point;
  while ((n > 1) && (i < l))
    {
      n -= 1;
      i += 1;
    }
  if ((i+3) <= l)
    {
      t = DynamicStrings_Dup (s);
      hundreths = DynamicStrings_Slice (DynamicStrings_Mark (s), i+1, i+3);
      s = t;
      if ((StringConvert_stoc (hundreths)) >= 50)
        s = carryOne (DynamicStrings_Mark (s), (unsigned int ) i);
      hundreths = DynamicStrings_KillString (hundreths);
    }
  else if ((i+2) <= l)
    {
      t = DynamicStrings_Dup (s);
      tenths = DynamicStrings_Slice (DynamicStrings_Mark (s), i+1, i+2);
      s = t;
      if ((StringConvert_stoc (tenths)) >= 5)
        s = carryOne (DynamicStrings_Mark (s), (unsigned int ) i);
      tenths = DynamicStrings_KillString (tenths);
    }
  if ((DynamicStrings_char (s, 0)) == '0')
    {
      s = DynamicStrings_Slice (DynamicStrings_Mark (s), 1, 0);
      l -= 1;
      point -= 1;
    }
  if (i < l)
    {
      s = DynamicStrings_Slice (DynamicStrings_Mark (s), 0, i);
      l = DynamicStrings_Length (s);
      if (l < point)
        s = DynamicStrings_ConCat (s, DynamicStrings_Mult (DynamicStrings_Mark (DynamicStrings_InitStringChar ('0')), (unsigned int ) point-l));
    }
  if (point >= 0)
    {
      /* avoid gcc warning by using compound statement even if not strictly necessary.  */
      if (point == 0)
        s = DynamicStrings_ConCat (DynamicStrings_InitStringChar ('.'), DynamicStrings_Mark (s));
      else
        s = DynamicStrings_ConCat (DynamicStrings_ConCatChar (DynamicStrings_Slice (DynamicStrings_Mark (s), 0, point), '.'), DynamicStrings_Mark (DynamicStrings_Slice (DynamicStrings_Mark (s), point, 0)));
    }
  return s;
}

static DynamicStrings_String doSigFig (DynamicStrings_String s, unsigned int n)
{
  int i;
  int l;
  int z;
  int point;
  DynamicStrings_String t;
  DynamicStrings_String tenths;
  DynamicStrings_String hundreths;

  l = DynamicStrings_Length (s);
  i = 0;
  point = DynamicStrings_Index (s, '.', 0);
  if (point >= 0)
    if (point == 0)
      s = DynamicStrings_Slice (DynamicStrings_Mark (s), 1, 0);
    else if (point < l)
      s = DynamicStrings_ConCat (DynamicStrings_Slice (DynamicStrings_Mark (s), 0, point), DynamicStrings_Mark (DynamicStrings_Slice (DynamicStrings_Mark (s), point+1, 0)));
    else
      s = DynamicStrings_Slice (DynamicStrings_Mark (s), 0, point);
  else
    s = DynamicStrings_Dup (DynamicStrings_Mark (s));
  l = DynamicStrings_Length (s);
  i = 0;
  if (l > 0)
    {
      while ((i < l) && ((DynamicStrings_char (s, i)) == '0'))
        i += 1;
      if ((i == l) && ((DynamicStrings_char (s, i-1)) == '0'))
        {
          s = DynamicStrings_Slice (DynamicStrings_Mark (s), 0, (int ) n);
          i = n;
        }
    }
  z = i;
  if (z == 0)
    s = DynamicStrings_ConCat (DynamicStrings_InitStringChar ('0'), DynamicStrings_Mark (s));
  else
    s = DynamicStrings_ConCat (DynamicStrings_ConCatChar (DynamicStrings_Slice (DynamicStrings_Mark (s), 0, i), '0'), DynamicStrings_Mark (DynamicStrings_Slice (DynamicStrings_Mark (s), i, 0)));
  n += 1;
  l = DynamicStrings_Length (s);
  while ((n > 1) && (i < l))
    {
      n -= 1;
      i += 1;
    }
  if ((i+3) <= l)
    {
      t = DynamicStrings_Dup (s);
      hundreths = DynamicStrings_Slice (DynamicStrings_Mark (s), i+1, i+3);
      s = t;
      if ((StringConvert_stoc (hundreths)) >= 50)
        s = carryOne (DynamicStrings_Mark (s), (unsigned int ) i);
      hundreths = DynamicStrings_KillString (hundreths);
    }
  else if ((i+2) <= l)
    {
      t = DynamicStrings_Dup (s);
      tenths = DynamicStrings_Slice (DynamicStrings_Mark (s), i+1, i+2);
      s = t;
      if ((StringConvert_stoc (tenths)) >= 5)
        s = carryOne (DynamicStrings_Mark (s), (unsigned int ) i);
      tenths = DynamicStrings_KillString (tenths);
    }
  if ((DynamicStrings_char (s, z)) == '0')
    {
      if (z == 0)
        s = DynamicStrings_Slice (DynamicStrings_Mark (s), z+1, 0);
      else
        s = DynamicStrings_ConCat (DynamicStrings_Slice (DynamicStrings_Mark (s), 0, z), DynamicStrings_Mark (DynamicStrings_Slice (DynamicStrings_Mark (s), z+1, 0)));
      l = DynamicStrings_Length (s);
    }
  else
    point += 1;
  if (i < l)
    {
      s = DynamicStrings_Slice (DynamicStrings_Mark (s), 0, i);
      l = DynamicStrings_Length (s);
      if (l < point)
        s = DynamicStrings_ConCat (s, DynamicStrings_Mult (DynamicStrings_Mark (DynamicStrings_InitStringChar ('0')), (unsigned int ) point-l));
    }
  if (point >= 0)
    {
      /* avoid gcc warning by using compound statement even if not strictly necessary.  */
      if (point == 0)
        s = DynamicStrings_ConCat (DynamicStrings_InitStringChar ('.'), DynamicStrings_Mark (s));
      else
        s = DynamicStrings_ConCat (DynamicStrings_ConCatChar (DynamicStrings_Slice (DynamicStrings_Mark (s), 0, point), '.'), DynamicStrings_Mark (DynamicStrings_Slice (DynamicStrings_Mark (s), point, 0)));
    }
  return s;
}

static DynamicStrings_String carryOne (DynamicStrings_String s, unsigned int i)
{
  if (i >= 0)
    if (IsDigit (DynamicStrings_char (s, (int ) i)))
      {
        /* avoid gcc warning by using compound statement even if not strictly necessary.  */
        if ((DynamicStrings_char (s, (int ) i)) == '9')
          if (i == 0)
            {
              s = DynamicStrings_ConCat (DynamicStrings_InitStringChar ('1'), DynamicStrings_Mark (s));
              return s;
            }
          else
            {
              s = DynamicStrings_ConCat (DynamicStrings_ConCatChar (DynamicStrings_Slice (DynamicStrings_Mark (s), 0, (int ) i), '0'), DynamicStrings_Mark (DynamicStrings_Slice (DynamicStrings_Mark (s), (int ) i+1, 0)));
              return carryOne (s, i-1);
            }
        else
          if (i == 0)
            s = DynamicStrings_ConCat (DynamicStrings_InitStringChar ((char) (((unsigned int) (DynamicStrings_char (s, (int ) i)))+1)), DynamicStrings_Mark (DynamicStrings_Slice (DynamicStrings_Mark (s), (int ) i+1, 0)));
          else
            s = DynamicStrings_ConCat (DynamicStrings_ConCatChar (DynamicStrings_Slice (DynamicStrings_Mark (s), 0, (int ) i), (char) (((unsigned int) (DynamicStrings_char (s, (int ) i)))+1)), DynamicStrings_Mark (DynamicStrings_Slice (DynamicStrings_Mark (s), (int ) i+1, 0)));
      }
  return s;
}

DynamicStrings_String StringConvert_IntegerToString (int i, unsigned int width, char padding, unsigned int sign, unsigned int base, unsigned int lower)
{
  DynamicStrings_String s;
  unsigned int c;

  if (i < 0)
    {
      if (i == (INT_MIN))
        {
          c = ((unsigned int ) (abs (i+1)))+1;
          if (width > 0)
            return DynamicStrings_ConCat (StringConvert_IntegerToString (-((int ) (c / base)), width-1, padding, sign, base, lower), DynamicStrings_Mark (StringConvert_IntegerToString ((int ) c % base, 0, ' ', FALSE, base, lower)));
          else
            return DynamicStrings_ConCat (StringConvert_IntegerToString (-((int ) (c / base)), 0, padding, sign, base, lower), DynamicStrings_Mark (StringConvert_IntegerToString ((int ) c % base, 0, ' ', FALSE, base, lower)));
        }
      else
        s = DynamicStrings_InitString ((char *) "-", 1);
      i = -i;
    }
  else
    if (sign)
      s = DynamicStrings_InitString ((char *) "+", 1);
    else
      s = DynamicStrings_InitString ((char *) "", 0);
  if (i > (((int ) (base))-1))
    s = DynamicStrings_ConCat (DynamicStrings_ConCat (s, DynamicStrings_Mark (StringConvert_IntegerToString ((int ) ((unsigned int ) (i)) / base, 0, ' ', FALSE, base, lower))), DynamicStrings_Mark (StringConvert_IntegerToString ((int ) ((unsigned int ) (i)) % base, 0, ' ', FALSE, base, lower)));
  else
    if (i <= 9)
      s = DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_InitStringChar ((char) (((unsigned int ) (i))+((unsigned int) ('0'))))));
    else
      if (lower)
        s = DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_InitStringChar ((char) ((((unsigned int ) (i))+((unsigned int) ('a')))-10))));
      else
        s = DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_InitStringChar ((char) ((((unsigned int ) (i))+((unsigned int) ('A')))-10))));
  if (width > (DynamicStrings_Length (s)))
    return DynamicStrings_ConCat (DynamicStrings_Mult (DynamicStrings_Mark (DynamicStrings_InitStringChar (padding)), width-(DynamicStrings_Length (s))), DynamicStrings_Mark (s));
  return s;
}

DynamicStrings_String StringConvert_CardinalToString (unsigned int c, unsigned int width, char padding, unsigned int base, unsigned int lower)
{
  DynamicStrings_String s;

  s = DynamicStrings_InitString ((char *) "", 0);
  if (c > (base-1))
    s = DynamicStrings_ConCat (DynamicStrings_ConCat (s, DynamicStrings_Mark (StringConvert_CardinalToString (c / base, 0, ' ', base, lower))), DynamicStrings_Mark (StringConvert_CardinalToString (c % base, 0, ' ', base, lower)));
  else
    if (c <= 9)
      s = DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_InitStringChar ((char) (c+((unsigned int) ('0'))))));
    else
      if (lower)
        s = DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_InitStringChar ((char) ((c+((unsigned int) ('a')))-10))));
      else
        s = DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_InitStringChar ((char) ((c+((unsigned int) ('A')))-10))));
  if (width > (DynamicStrings_Length (s)))
    return DynamicStrings_ConCat (DynamicStrings_Mult (DynamicStrings_Mark (DynamicStrings_InitStringChar (padding)), width-(DynamicStrings_Length (s))), s);
  return s;
}

int StringConvert_StringToInteger (DynamicStrings_String s, unsigned int base, unsigned int *found)
{
  unsigned int n;
  unsigned int l;
  unsigned int c;
  unsigned int negative;

  s = DynamicStrings_RemoveWhitePrefix (s);
  l = DynamicStrings_Length (s);
  c = 0;
  n = 0;
  negative = FALSE;
  if (n < l)
    {
      while (((DynamicStrings_char (s, (int ) n)) == '-') || ((DynamicStrings_char (s, (int ) n)) == '+'))
        {
          if ((DynamicStrings_char (s, (int ) n)) == '-')
            negative = ! negative;
          n += 1;
        }
      while ((n < l) && ((IsDecimalDigitValid (DynamicStrings_char (s, (int ) n), base, &c)) || (IsHexidecimalDigitValid (DynamicStrings_char (s, (int ) n), base, &c))))
        {
          (*found) = TRUE;
          n += 1;
        }
    }
  s = DynamicStrings_KillString (s);
  if (negative)
    return -((int ) (Min (((unsigned int ) (INT_MAX))+1, c)));
  else
    return (int ) (Min ((unsigned int ) INT_MAX, c));
}

unsigned int StringConvert_StringToCardinal (DynamicStrings_String s, unsigned int base, unsigned int *found)
{
  unsigned int n;
  unsigned int l;
  unsigned int c;

  s = DynamicStrings_RemoveWhitePrefix (s);
  l = DynamicStrings_Length (s);
  c = 0;
  n = 0;
  if (n < l)
    {
      while ((DynamicStrings_char (s, (int ) n)) == '+')
        n += 1;
      while ((n < l) && ((IsDecimalDigitValid (DynamicStrings_char (s, (int ) n), base, &c)) || (IsHexidecimalDigitValid (DynamicStrings_char (s, (int ) n), base, &c))))
        {
          (*found) = TRUE;
          n += 1;
        }
    }
  s = DynamicStrings_KillString (s);
  return c;
}

DynamicStrings_String StringConvert_LongIntegerToString (long int i, unsigned int width, char padding, unsigned int sign, unsigned int base, unsigned int lower)
{
  DynamicStrings_String s;
  long unsigned int c;

  if (i < 0)
    {
      if (i == (LONG_MIN))
        {
          c = ((long unsigned int ) (labs (i+1)))+1;
          if (width > 0)
            return DynamicStrings_ConCat (StringConvert_LongIntegerToString (-((long int ) (c / ((long unsigned int ) (base)))), width-1, padding, sign, base, lower), DynamicStrings_Mark (StringConvert_LongIntegerToString ((long int ) c % ((long unsigned int ) (base)), 0, ' ', FALSE, base, lower)));
          else
            return DynamicStrings_ConCat (StringConvert_LongIntegerToString (-((long int ) (c / ((long unsigned int ) (base)))), 0, padding, sign, base, lower), DynamicStrings_Mark (StringConvert_LongIntegerToString ((long int ) c % ((long unsigned int ) (base)), 0, ' ', FALSE, base, lower)));
        }
      else
        s = DynamicStrings_InitString ((char *) "-", 1);
      i = -i;
    }
  else
    if (sign)
      s = DynamicStrings_InitString ((char *) "+", 1);
    else
      s = DynamicStrings_InitString ((char *) "", 0);
  if (i > ((long int ) (base-1)))
    s = DynamicStrings_ConCat (DynamicStrings_ConCat (s, DynamicStrings_Mark (StringConvert_LongIntegerToString (i / ((long int ) (base)), 0, ' ', FALSE, base, lower))), DynamicStrings_Mark (StringConvert_LongIntegerToString (i % ((long int ) (base)), 0, ' ', FALSE, base, lower)));
  else
    if (i <= 9)
      s = DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_InitStringChar ((char) (((unsigned int ) (i))+((unsigned int) ('0'))))));
    else
      if (lower)
        s = DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_InitStringChar ((char) ((((unsigned int ) (i))+((unsigned int) ('a')))-10))));
      else
        s = DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_InitStringChar ((char) ((((unsigned int ) (i))+((unsigned int) ('A')))-10))));
  if (width > (DynamicStrings_Length (s)))
    return DynamicStrings_ConCat (DynamicStrings_Mult (DynamicStrings_Mark (DynamicStrings_InitStringChar (padding)), width-(DynamicStrings_Length (s))), s);
  return s;
}

long int StringConvert_StringToLongInteger (DynamicStrings_String s, unsigned int base, unsigned int *found)
{
  unsigned int n;
  unsigned int l;
  long unsigned int c;
  unsigned int negative;

  s = DynamicStrings_RemoveWhitePrefix (s);
  l = DynamicStrings_Length (s);
  c = 0;
  n = 0;
  negative = FALSE;
  if (n < l)
    {
      while (((DynamicStrings_char (s, (int ) n)) == '-') || ((DynamicStrings_char (s, (int ) n)) == '+'))
        {
          if ((DynamicStrings_char (s, (int ) n)) == '-')
            negative = ! negative;
          n += 1;
        }
      while ((n < l) && ((IsDecimalDigitValidLong (DynamicStrings_char (s, (int ) n), base, &c)) || (IsHexidecimalDigitValidLong (DynamicStrings_char (s, (int ) n), base, &c))))
        {
          (*found) = TRUE;
          n += 1;
        }
    }
  s = DynamicStrings_KillString (s);
  if (negative)
    return -((long int ) (LongMin (((long unsigned int ) (LONG_MAX))+1, c)));
  else
    return (long int ) (LongMin ((long unsigned int ) LONG_MAX, c));
}

DynamicStrings_String StringConvert_LongCardinalToString (long unsigned int c, unsigned int width, char padding, unsigned int base, unsigned int lower)
{
  DynamicStrings_String s;

  s = DynamicStrings_InitString ((char *) "", 0);
  if (c > ((long unsigned int ) (base-1)))
    s = DynamicStrings_ConCat (DynamicStrings_ConCat (s, StringConvert_LongCardinalToString (c / ((long unsigned int ) (base)), 0, ' ', base, lower)), StringConvert_LongCardinalToString (c % ((long unsigned int ) (base)), 0, ' ', base, lower));
  else
    if (c <= 9)
      s = DynamicStrings_ConCat (s, DynamicStrings_InitStringChar ((char) (((unsigned int ) (c))+((unsigned int) ('0')))));
    else
      if (lower)
        s = DynamicStrings_ConCat (s, DynamicStrings_InitStringChar ((char) ((((unsigned int ) (c))+((unsigned int) ('a')))-10)));
      else
        s = DynamicStrings_ConCat (s, DynamicStrings_InitStringChar ((char) ((((unsigned int ) (c))+((unsigned int) ('A')))-10)));
  if (width > (DynamicStrings_Length (s)))
    return DynamicStrings_ConCat (DynamicStrings_Mult (DynamicStrings_Mark (DynamicStrings_InitStringChar (padding)), width-(DynamicStrings_Length (s))), s);
  return s;
}

long unsigned int StringConvert_StringToLongCardinal (DynamicStrings_String s, unsigned int base, unsigned int *found)
{
  unsigned int n;
  unsigned int l;
  long unsigned int c;

  s = DynamicStrings_RemoveWhitePrefix (s);
  l = DynamicStrings_Length (s);
  c = 0;
  n = 0;
  if (n < l)
    {
      while ((DynamicStrings_char (s, (int ) n)) == '+')
        n += 1;
      while ((n < l) && ((IsDecimalDigitValidLong (DynamicStrings_char (s, (int ) n), base, &c)) || (IsHexidecimalDigitValidLong (DynamicStrings_char (s, (int ) n), base, &c))))
        {
          (*found) = TRUE;
          n += 1;
        }
    }
  s = DynamicStrings_KillString (s);
  return c;
}

DynamicStrings_String StringConvert_ShortCardinalToString (short unsigned int c, unsigned int width, char padding, unsigned int base, unsigned int lower)
{
  DynamicStrings_String s;

  s = DynamicStrings_InitString ((char *) "", 0);
  if (((unsigned int ) (c)) > (base-1))
    s = DynamicStrings_ConCat (DynamicStrings_ConCat (s, StringConvert_ShortCardinalToString (c / ((short unsigned int ) (base)), 0, ' ', base, lower)), StringConvert_ShortCardinalToString (c % ((short unsigned int ) (base)), 0, ' ', base, lower));
  else
    if (c <= 9)
      s = DynamicStrings_ConCat (s, DynamicStrings_InitStringChar ((char) (((unsigned int ) (c))+((unsigned int) ('0')))));
    else
      if (lower)
        s = DynamicStrings_ConCat (s, DynamicStrings_InitStringChar ((char) ((((unsigned int ) (c))+((unsigned int) ('a')))-10)));
      else
        s = DynamicStrings_ConCat (s, DynamicStrings_InitStringChar ((char) ((((unsigned int ) (c))+((unsigned int) ('A')))-10)));
  if (width > (DynamicStrings_Length (s)))
    return DynamicStrings_ConCat (DynamicStrings_Mult (DynamicStrings_Mark (DynamicStrings_InitStringChar (padding)), width-(DynamicStrings_Length (s))), s);
  return s;
}

short unsigned int StringConvert_StringToShortCardinal (DynamicStrings_String s, unsigned int base, unsigned int *found)
{
  unsigned int n;
  unsigned int l;
  short unsigned int c;

  s = DynamicStrings_RemoveWhitePrefix (s);
  l = DynamicStrings_Length (s);
  c = 0;
  n = 0;
  if (n < l)
    {
      while ((DynamicStrings_char (s, (int ) n)) == '+')
        n += 1;
      while ((n < l) && ((IsDecimalDigitValidShort (DynamicStrings_char (s, (int ) n), base, &c)) || (IsHexidecimalDigitValidShort (DynamicStrings_char (s, (int ) n), base, &c))))
        {
          (*found) = TRUE;
          n += 1;
        }
    }
  s = DynamicStrings_KillString (s);
  return c;
}

int StringConvert_stoi (DynamicStrings_String s)
{
  unsigned int found;

  return StringConvert_StringToInteger (s, 10, &found);
}

DynamicStrings_String StringConvert_itos (int i, unsigned int width, char padding, unsigned int sign)
{
  return StringConvert_IntegerToString (i, width, padding, sign, 10, FALSE);
}

DynamicStrings_String StringConvert_ctos (unsigned int c, unsigned int width, char padding)
{
  return StringConvert_CardinalToString (c, width, padding, 10, FALSE);
}

unsigned int StringConvert_stoc (DynamicStrings_String s)
{
  unsigned int found;

  return StringConvert_StringToCardinal (s, 10, &found);
}

int StringConvert_hstoi (DynamicStrings_String s)
{
  unsigned int found;

  return StringConvert_StringToInteger (s, 16, &found);
}

int StringConvert_ostoi (DynamicStrings_String s)
{
  unsigned int found;

  return StringConvert_StringToInteger (s, 8, &found);
}

int StringConvert_bstoi (DynamicStrings_String s)
{
  unsigned int found;

  return StringConvert_StringToInteger (s, 2, &found);
}

unsigned int StringConvert_hstoc (DynamicStrings_String s)
{
  unsigned int found;

  return StringConvert_StringToCardinal (s, 16, &found);
}

unsigned int StringConvert_ostoc (DynamicStrings_String s)
{
  unsigned int found;

  return StringConvert_StringToCardinal (s, 8, &found);
}

unsigned int StringConvert_bstoc (DynamicStrings_String s)
{
  unsigned int found;

  return StringConvert_StringToCardinal (s, 2, &found);
}

long double StringConvert_StringToLongreal (DynamicStrings_String s, unsigned int *found)
{
  unsigned int error;
  long double value;

  s = DynamicStrings_RemoveWhitePrefix (s);
  value = ldtoa_strtold (DynamicStrings_string (s), &error);
  s = DynamicStrings_KillString (s);
  (*found) = ! error;
  return value;
}

DynamicStrings_String StringConvert_LongrealToString (long double x, unsigned int TotalWidth, unsigned int FractionWidth)
{
  unsigned int maxprecision;
  DynamicStrings_String s;
  void * r;
  int point;
  unsigned int sign;
  int l;

  if (TotalWidth == 0)
    {
      maxprecision = TRUE;
      r = ldtoa_ldtoa (x, (ldtoa_Mode) ldtoa_decimaldigits, 100, &point, &sign);
    }
  else
    r = ldtoa_ldtoa (x, (ldtoa_Mode) ldtoa_decimaldigits, 100, &point, &sign);
  s = DynamicStrings_InitStringCharStar (r);
  libc_free (r);
  l = DynamicStrings_Length (s);
  if (point > l)
    {
      s = DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_Mult (DynamicStrings_Mark (DynamicStrings_InitStringChar ('0')), (unsigned int ) point-l)));
      s = DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_InitString ((char *) ".0", 2)));
      if (! maxprecision && (FractionWidth > 0))
        {
          FractionWidth -= 1;
          if (((int ) (FractionWidth)) > (point-l))
            s = DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_Mult (DynamicStrings_Mark (DynamicStrings_InitString ((char *) "0", 1)), FractionWidth)));
        }
    }
  else if (point < 0)
    {
      s = DynamicStrings_ConCat (DynamicStrings_Mult (DynamicStrings_Mark (DynamicStrings_InitStringChar ('0')), (unsigned int ) -point), DynamicStrings_Mark (s));
      l = DynamicStrings_Length (s);
      s = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "0.", 2), DynamicStrings_Mark (s));
      if (! maxprecision && (l < ((int ) (FractionWidth))))
        s = DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_Mult (DynamicStrings_Mark (DynamicStrings_InitString ((char *) "0", 1)), (unsigned int ) ((int ) (FractionWidth))-l)));
    }
  else
    {
      if (point == 0)
        s = DynamicStrings_ConCat (DynamicStrings_InitString ((char *) "0.", 2), DynamicStrings_Mark (DynamicStrings_Slice (DynamicStrings_Mark (s), point, 0)));
      else
        s = DynamicStrings_ConCat (DynamicStrings_ConCatChar (DynamicStrings_Slice (DynamicStrings_Mark (s), 0, point), '.'), DynamicStrings_Mark (DynamicStrings_Slice (DynamicStrings_Mark (s), point, 0)));
      if (! maxprecision && ((l-point) < ((int ) (FractionWidth))))
        s = DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_Mult (DynamicStrings_Mark (DynamicStrings_InitString ((char *) "0", 1)), (unsigned int ) ((int ) (FractionWidth))-(l-point))));
    }
  if ((DynamicStrings_Length (s)) > TotalWidth)
    {
      /* avoid gcc warning by using compound statement even if not strictly necessary.  */
      if (TotalWidth > 0)
        if (sign)
          {
            s = DynamicStrings_Slice (DynamicStrings_Mark (StringConvert_ToDecimalPlaces (s, FractionWidth)), 0, (int ) TotalWidth-1);
            s = DynamicStrings_ConCat (DynamicStrings_InitStringChar ('-'), DynamicStrings_Mark (s));
            sign = FALSE;
          }
        else
          s = DynamicStrings_Slice (DynamicStrings_Mark (StringConvert_ToDecimalPlaces (s, FractionWidth)), 0, (int ) TotalWidth);
      else
        if (sign)
          {
            s = StringConvert_ToDecimalPlaces (s, FractionWidth);
            s = DynamicStrings_ConCat (DynamicStrings_InitStringChar ('-'), DynamicStrings_Mark (s));
            sign = FALSE;
          }
        else
          s = StringConvert_ToDecimalPlaces (s, FractionWidth);
    }
  if ((DynamicStrings_Length (s)) < TotalWidth)
    s = DynamicStrings_ConCat (DynamicStrings_Mult (DynamicStrings_Mark (DynamicStrings_InitStringChar (' ')), TotalWidth-(DynamicStrings_Length (s))), DynamicStrings_Mark (s));
  return s;
}

double StringConvert_stor (DynamicStrings_String s)
{
  unsigned int found;

  return (double ) (StringConvert_StringToLongreal (s, &found));
}

long double StringConvert_stolr (DynamicStrings_String s)
{
  unsigned int found;

  return StringConvert_StringToLongreal (s, &found);
}

DynamicStrings_String StringConvert_ToSigFig (DynamicStrings_String s, unsigned int n)
{
  int point;
  unsigned int poTen;

  Assert ((IsDigit (DynamicStrings_char (s, 0))) || ((DynamicStrings_char (s, 0)) == '.'), (char *) "../../gcc-5.2.0/gcc/gm2/gm2-libs/StringConvert.mod", 50, 1220, (char *) "ToSigFig", 8);
  point = DynamicStrings_Index (s, '.', 0);
  if (point < 0)
    poTen = DynamicStrings_Length (s);
  else
    poTen = point;
  s = doSigFig (s, n);
  if (((DynamicStrings_Length (s)) > 0) && ((DynamicStrings_char (s, -1)) == '.'))
    return DynamicStrings_Slice (DynamicStrings_Mark (s), 0, -1);
  else
    {
      if (poTen > (DynamicStrings_Length (s)))
        s = DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_Mult (DynamicStrings_Mark (DynamicStrings_InitStringChar ('0')), poTen-(DynamicStrings_Length (s)))));
      return s;
    }
}

DynamicStrings_String StringConvert_ToDecimalPlaces (DynamicStrings_String s, unsigned int n)
{
  int point;

  Assert ((IsDigit (DynamicStrings_char (s, 0))) || ((DynamicStrings_char (s, 0)) == '.'), (char *) "../../gcc-5.2.0/gcc/gm2/gm2-libs/StringConvert.mod", 50, 1067, (char *) "ToDecimalPlaces", 15);
  point = DynamicStrings_Index (s, '.', 0);
  if (point < 0)
    {
      /* avoid gcc warning by using compound statement even if not strictly necessary.  */
      if (n > 0)
        return DynamicStrings_ConCat (DynamicStrings_ConCat (s, DynamicStrings_Mark (DynamicStrings_InitStringChar ('.'))), DynamicStrings_Mult (DynamicStrings_Mark (DynamicStrings_InitStringChar ('0')), n));
      else
        return s;
    }
  s = doDecimalPlaces (s, n);
  if (((DynamicStrings_Length (s)) > 0) && ((DynamicStrings_char (s, -1)) == '.'))
    return DynamicStrings_Slice (DynamicStrings_Mark (s), 0, -1);
  else
    return s;
}

void _M2_StringConvert_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_StringConvert_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
