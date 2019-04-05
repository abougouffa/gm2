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


#if !defined (_mcPrintf_H)
#   define _mcPrintf_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"
#   include "GFIO.h"

#   if defined (_mcPrintf_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   printf0 - writes out an array to, StdOut, after the escape sequences have been
             translated.
*/

EXTERN void mcPrintf_printf0 (char *a_, unsigned int _a_high);

/*
   printf0 - writes out an array to, StdOut, after the escape sequences have been
             translated.
*/

EXTERN void mcPrintf_printf1 (char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);

/*
   printf0 - writes out an array to, StdOut, after the escape sequences have been
             translated.
*/

EXTERN void mcPrintf_printf2 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);

/*
   printf0 - writes out an array to, StdOut, after the escape sequences have been
             translated.
*/

EXTERN void mcPrintf_printf3 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);

/*
   printf0 - writes out an array to, StdOut, after the escape sequences have been
             translated.
*/

EXTERN void mcPrintf_printf4 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high, unsigned char *w4_, unsigned int _w4_high);

/*
   fprintf0 - writes out an array to, file, after the escape sequences have been
              translated.
*/

EXTERN void mcPrintf_fprintf0 (FIO_File file, char *a_, unsigned int _a_high);

/*
   fprintf0 - writes out an array to, file, after the escape sequences have been
              translated.
*/

EXTERN void mcPrintf_fprintf1 (FIO_File file, char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);

/*
   fprintf0 - writes out an array to, file, after the escape sequences have been
              translated.
*/

EXTERN void mcPrintf_fprintf2 (FIO_File file, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);

/*
   fprintf0 - writes out an array to, file, after the escape sequences have been
              translated.
*/

EXTERN void mcPrintf_fprintf3 (FIO_File file, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);

/*
   fprintf0 - writes out an array to, file, after the escape sequences have been
              translated.
*/

EXTERN void mcPrintf_fprintf4 (FIO_File file, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high, unsigned char *w4_, unsigned int _w4_high);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
