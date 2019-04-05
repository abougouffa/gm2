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


#if !defined (_FpuIO_H)
#   define _FpuIO_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_FpuIO_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

EXTERN void FpuIO_ReadReal (double *x);
EXTERN void FpuIO_WriteReal (double x, unsigned int TotalWidth, unsigned int FractionWidth);
EXTERN void FpuIO_StrToReal (char *a_, unsigned int _a_high, double *x);
EXTERN void FpuIO_RealToStr (double x, unsigned int TotalWidth, unsigned int FractionWidth, char *a, unsigned int _a_high);
EXTERN void FpuIO_ReadLongReal (long double *x);
EXTERN void FpuIO_WriteLongReal (long double x, unsigned int TotalWidth, unsigned int FractionWidth);
EXTERN void FpuIO_StrToLongReal (char *a_, unsigned int _a_high, long double *x);
EXTERN void FpuIO_LongRealToStr (long double x, unsigned int TotalWidth, unsigned int FractionWidth, char *a, unsigned int _a_high);
EXTERN void FpuIO_ReadLongInt (long int *x);
EXTERN void FpuIO_WriteLongInt (long int x, unsigned int n);
EXTERN void FpuIO_StrToLongInt (char *a_, unsigned int _a_high, long int *x);
EXTERN void FpuIO_LongIntToStr (long int x, unsigned int n, char *a, unsigned int _a_high);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
