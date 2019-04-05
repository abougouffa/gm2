/* Copyright (C) 2019 Free Software Foundation, Inc.

This file is part of GNU Modula-2.

GNU Modula-2 is software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING.  If not,
see <https://www.gnu.org/licenses/>.  */

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

/*
   WriteReal - converts a REAL number, x, which has a, TotalWidth, and
               FractionWidth into, string, a.
*/

void FpuIO_WriteReal (double x, unsigned int TotalWidth, unsigned int FractionWidth);

/*
   WriteReal - converts a REAL number, x, which has a, TotalWidth, and
               FractionWidth into, string, a.
*/

void FpuIO_StrToReal (char *a_, unsigned int _a_high, double *x);

/*
   RealToStr - converts a LONGREAL number, Real, which has, TotalWidth, and
               FractionWidth into a string.
*/

void FpuIO_RealToStr (double x, unsigned int TotalWidth, unsigned int FractionWidth, char *a, unsigned int _a_high);
void FpuIO_ReadLongReal (long double *x);

/*
   WriteLongReal - converts a LONGREAL number, x, which has a, TotalWidth, and
                   FractionWidth into a string.
*/

void FpuIO_WriteLongReal (long double x, unsigned int TotalWidth, unsigned int FractionWidth);

/*
   WriteLongReal - converts a LONGREAL number, x, which has a, TotalWidth, and
                   FractionWidth into a string.
*/

void FpuIO_StrToLongReal (char *a_, unsigned int _a_high, long double *x);

/*
   LongRealToStr - converts a LONGREAL number, Real, which has, TotalWidth, and
                   FractionWidth into a string.
*/

void FpuIO_LongRealToStr (long double x, unsigned int TotalWidth, unsigned int FractionWidth, char *a, unsigned int _a_high);

/*
   LongRealToStr - converts a LONGREAL number, Real, which has, TotalWidth, and
                   FractionWidth into a string.
*/

void FpuIO_ReadLongInt (long int *x);

/*
   LongRealToStr - converts a LONGREAL number, Real, which has, TotalWidth, and
                   FractionWidth into a string.
*/

void FpuIO_WriteLongInt (long int x, unsigned int n);

/*
   LongRealToStr - converts a LONGREAL number, Real, which has, TotalWidth, and
                   FractionWidth into a string.
*/

void FpuIO_StrToLongInt (char *a_, unsigned int _a_high, long int *x);

/*
   LongRealToStr - converts a LONGREAL number, Real, which has, TotalWidth, and
                   FractionWidth into a string.
*/

void FpuIO_LongIntToStr (long int x, unsigned int n, char *a, unsigned int _a_high);

void FpuIO_ReadReal (double *x)
{
  typedef struct _T1_a _T1;

  struct _T1_a { char array[MaxLineLength+1]; };
  _T1 a;

  /* 
#undef GM2_DEBUG_FPUIO
if defined(GM2_DEBUG_FPUIO)
#  define InitString(X) InitStringDB(X, __FILE__, __LINE__)
#  define InitStringCharStar(X) InitStringCharStarDB(X, __FILE__, __LINE__)
#  define InitStringChar(X) InitStringCharDB(X, __FILE__, __LINE__)
#  define Mult(X,Y) MultDB(X, Y, __FILE__, __LINE__)
#  define Dup(X) DupDB(X, __FILE__, __LINE__)
#  define Slice(X,Y,Z) SliceDB(X, Y, Z, __FILE__, __LINE__)
#endif
  */
  StrIO_ReadString ((char *) &a.array[0], MaxLineLength);
  FpuIO_StrToReal ((char *) &a.array[0], MaxLineLength, x);
}


/*
   WriteReal - converts a REAL number, x, which has a, TotalWidth, and
               FractionWidth into, string, a.
*/

void FpuIO_WriteReal (double x, unsigned int TotalWidth, unsigned int FractionWidth)
{
  typedef struct _T2_a _T2;

  struct _T2_a { char array[MaxLineLength+1]; };
  _T2 a;

  FpuIO_RealToStr (x, TotalWidth, FractionWidth, (char *) &a.array[0], MaxLineLength);
  StrIO_WriteString ((char *) &a.array[0], MaxLineLength);
}


/*
   WriteReal - converts a REAL number, x, which has a, TotalWidth, and
               FractionWidth into, string, a.
*/

void FpuIO_StrToReal (char *a_, unsigned int _a_high, double *x)
{
  long double lr;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  FpuIO_StrToLongReal ((char *) a, _a_high, &lr);  /* let StrToLongReal do the work and we convert the result back to REAL  */
  (*x) = (double ) (lr);  /* let StrToLongReal do the work and we convert the result back to REAL  */
}


/*
   RealToStr - converts a LONGREAL number, Real, which has, TotalWidth, and
               FractionWidth into a string.
*/

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


/*
   WriteLongReal - converts a LONGREAL number, x, which has a, TotalWidth, and
                   FractionWidth into a string.
*/

void FpuIO_WriteLongReal (long double x, unsigned int TotalWidth, unsigned int FractionWidth)
{
  typedef struct _T4_a _T4;

  struct _T4_a { char array[MaxLineLength+1]; };
  _T4 a;

  FpuIO_LongRealToStr (x, TotalWidth, FractionWidth, (char *) &a.array[0], MaxLineLength);
  StrIO_WriteString ((char *) &a.array[0], MaxLineLength);
}


/*
   WriteLongReal - converts a LONGREAL number, x, which has a, TotalWidth, and
                   FractionWidth into a string.
*/

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


/*
   LongRealToStr - converts a LONGREAL number, Real, which has, TotalWidth, and
                   FractionWidth into a string.
*/

void FpuIO_LongRealToStr (long double x, unsigned int TotalWidth, unsigned int FractionWidth, char *a, unsigned int _a_high)
{
  DynamicStrings_String s;

  s = StringConvert_LongrealToString (x, TotalWidth, FractionWidth);
  DynamicStrings_CopyOut ((char *) a, _a_high, s);
  s = DynamicStrings_KillString (s);
}


/*
   LongRealToStr - converts a LONGREAL number, Real, which has, TotalWidth, and
                   FractionWidth into a string.
*/

void FpuIO_ReadLongInt (long int *x)
{
  typedef struct _T5_a _T5;

  struct _T5_a { char array[MaxLineLength+1]; };
  _T5 a;

  StrIO_ReadString ((char *) &a.array[0], MaxLineLength);
  FpuIO_StrToLongInt ((char *) &a.array[0], MaxLineLength, x);
}


/*
   LongRealToStr - converts a LONGREAL number, Real, which has, TotalWidth, and
                   FractionWidth into a string.
*/

void FpuIO_WriteLongInt (long int x, unsigned int n)
{
  typedef struct _T6_a _T6;

  struct _T6_a { char array[MaxLineLength+1]; };
  _T6 a;

  FpuIO_LongIntToStr (x, n, (char *) &a.array[0], MaxLineLength);
  StrIO_WriteString ((char *) &a.array[0], MaxLineLength);
}


/*
   LongRealToStr - converts a LONGREAL number, Real, which has, TotalWidth, and
                   FractionWidth into a string.
*/

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


/*
   LongRealToStr - converts a LONGREAL number, Real, which has, TotalWidth, and
                   FractionWidth into a string.
*/

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
