/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/FpuIO.mod.  */

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
#define _FpuIO_H
#define _FpuIO_C

#   include "GStrIO.h"
#   include "GStrLib.h"
#   include "GASCII.h"
#   include "GDynamicStrings.h"
#   include "GStringConvert.h"

#   define MaxLineLength 100
void FpuIO_ReadReal (double *x);
void FpuIO_WriteReal (double x, unsigned int TotalWidth, unsigned int FractionWidth);
void FpuIO_StrToReal (char *a_, unsigned int _a_high, double *x);
void FpuIO_RealToStr (double x, unsigned int TotalWidth, unsigned int FractionWidth, char *a, unsigned int _a_high);
void FpuIO_ReadLongReal (long double *x);
void FpuIO_WriteLongReal (long double x, unsigned int TotalWidth, unsigned int FractionWidth);
void FpuIO_StrToLongReal (char *a_, unsigned int _a_high, long double *x);
void FpuIO_LongRealToStr (long double x, unsigned int TotalWidth, unsigned int FractionWidth, char *a, unsigned int _a_high);
void FpuIO_ReadLongInt (long int *x);
void FpuIO_WriteLongInt (long int x, unsigned int n);
void FpuIO_StrToLongInt (char *a_, unsigned int _a_high, long int *x);
void FpuIO_LongIntToStr (long int x, unsigned int n, char *a, unsigned int _a_high);

void FpuIO_ReadReal (double *x)
{
  typedef struct _T1_a _T1;

  struct _T1_a { char array[MaxLineLength+1]; };
  _T1 a;

  StrIO_ReadString ((char *) &a.array[0], MaxLineLength);
  FpuIO_StrToReal ((char *) &a.array[0], MaxLineLength, x);
}

void FpuIO_WriteReal (double x, unsigned int TotalWidth, unsigned int FractionWidth)
{
  typedef struct _T2_a _T2;

  struct _T2_a { char array[MaxLineLength+1]; };
  _T2 a;

  FpuIO_RealToStr (x, TotalWidth, FractionWidth, (char *) &a.array[0], MaxLineLength);
  StrIO_WriteString ((char *) &a.array[0], MaxLineLength);
}

void FpuIO_StrToReal (char *a_, unsigned int _a_high, double *x)
{
  long double lr;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  FpuIO_StrToLongReal ((char *) a, _a_high, &lr);
  (*x) = (double ) (lr);
}

void FpuIO_RealToStr (double x, unsigned int TotalWidth, unsigned int FractionWidth, char *a, unsigned int _a_high)
{
  long double lr;

  lr = (long double ) (x);
  FpuIO_LongRealToStr (lr, TotalWidth, FractionWidth, (char *) a, _a_high);
}

void FpuIO_ReadLongReal (long double *x)
{
  typedef struct _T3_a _T3;

  struct _T3_a { char array[MaxLineLength+1]; };
  _T3 a;

  StrIO_ReadString ((char *) &a.array[0], MaxLineLength);
  FpuIO_StrToLongReal ((char *) &a.array[0], MaxLineLength, x);
}

void FpuIO_WriteLongReal (long double x, unsigned int TotalWidth, unsigned int FractionWidth)
{
  typedef struct _T4_a _T4;

  struct _T4_a { char array[MaxLineLength+1]; };
  _T4 a;

  FpuIO_LongRealToStr (x, TotalWidth, FractionWidth, (char *) &a.array[0], MaxLineLength);
  StrIO_WriteString ((char *) &a.array[0], MaxLineLength);
}

void FpuIO_StrToLongReal (char *a_, unsigned int _a_high, long double *x)
{
  unsigned int found;
  DynamicStrings_String s;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  s = DynamicStrings_InitString ((char *) a, _a_high);
  (*x) = StringConvert_StringToLongreal (s, &found);
  s = DynamicStrings_KillString (s);
}

void FpuIO_LongRealToStr (long double x, unsigned int TotalWidth, unsigned int FractionWidth, char *a, unsigned int _a_high)
{
  DynamicStrings_String s;

  s = StringConvert_LongrealToString (x, TotalWidth, FractionWidth);
  DynamicStrings_CopyOut ((char *) a, _a_high, s);
  s = DynamicStrings_KillString (s);
}

void FpuIO_ReadLongInt (long int *x)
{
  typedef struct _T5_a _T5;

  struct _T5_a { char array[MaxLineLength+1]; };
  _T5 a;

  StrIO_ReadString ((char *) &a.array[0], MaxLineLength);
  FpuIO_StrToLongInt ((char *) &a.array[0], MaxLineLength, x);
}

void FpuIO_WriteLongInt (long int x, unsigned int n)
{
  typedef struct _T6_a _T6;

  struct _T6_a { char array[MaxLineLength+1]; };
  _T6 a;

  FpuIO_LongIntToStr (x, n, (char *) &a.array[0], MaxLineLength);
  StrIO_WriteString ((char *) &a.array[0], MaxLineLength);
}

void FpuIO_StrToLongInt (char *a_, unsigned int _a_high, long int *x)
{
  DynamicStrings_String s;
  unsigned int found;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  s = DynamicStrings_InitString ((char *) a, _a_high);
  (*x) = StringConvert_StringToLongInteger (s, 10, &found);
  s = DynamicStrings_KillString (s);
}

void FpuIO_LongIntToStr (long int x, unsigned int n, char *a, unsigned int _a_high)
{
  DynamicStrings_String s;

  s = StringConvert_LongIntegerToString (x, n, ' ', FALSE, 10, TRUE);
  DynamicStrings_CopyOut ((char *) a, _a_high, s);
  s = DynamicStrings_KillString (s);
}

void _M2_FpuIO_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_FpuIO_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
