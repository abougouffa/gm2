/* Copyright (C) 2009 Free Software Foundation, Inc. */
/* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA */

/*
 *   dtoa.c - 
 */

#define GM2

#include <p2c/p2c.h>

#include "gm2-libs-host.h"

#if defined(HAVE_STRINGS)
#  include <strings.h>
#endif

#if defined(HAVE_STRING)
#  include <string.h>
#endif


#if !defined(TRUE)
#  define TRUE (1==1)
#endif
#if !defined(FALSE)
#  define FALSE (1==0)
#endif

#if defined(HAVE_STDLIB_H)
#  if !defined(_ISOC99_SOURCE)
#     define _ISOC99_SOURCE
#  endif
#  include <stdlib.h>
#endif

#define MAX_FP_DIGITS 500

typedef enum Mode { maxsignicant, decimaldigits } Mode;

/*
 *  maxsignicant:  return a string containing max(1,ndigits) significant
 *                 digits.  The return string contains the string produced
 *                 by ecvt.
 *  decimaldigits: return a string produced by fcvt.  The string will
 *                 contain ndigits past the decimal point
 *                 (ndigits may be negative).
 */



double dtoa_strtod (const char *s, int *error)
{
  char *endp;
  double d;

#if defined(HAVE_STRTOLD)
  errno = 0;
  d = strtod (s, &endp);
#else
# error "you need to build on a system which can support strtod"
#endif
  if (endp != NULL && (*endp == '\0'))
    *error = (errno != 0);
  else
    *error = TRUE;
  return d;
}

char *dtoa_dtoa (double d, int mode, int ndigits, int *decpt, int *sign)
{
  char *p;
  int r;
  switch (mode) {

  case maxsignicant:
    p = malloc (ndigits+2);
    r = ecvt_r (d, ndigits, decpt, sign, p, ndigits+2);
    if (r != 0)
      perror("ecvt_r");
    return p;
  case decimaldigits:
    p = malloc (MAX_FP_DIGITS);
    r = fcvt_r (d, ndigits, decpt, sign, p, MAX_FP_DIGITS);
    if (r != 0)
      perror("fcvt_r");
    return p;
  default:
    abort();
  }
}

#if defined(GM2)
/*
 *  GNU Modula-2 hooks
 */

void _M2_dtoa_init (void) {}
void _M2_dtoa_finish (void) {}
#endif
