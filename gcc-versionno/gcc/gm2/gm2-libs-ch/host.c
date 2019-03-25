/* host.c supply missing math routines.

Copyright (C) 2010-2019 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
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

#include "gm2-libs-host.h"

#if defined(HAVE_MATH_H)
#include <math.h>
#endif

#if !defined(HAVE_EXP10)
double
exp10 (double x)
{
  return exp (x * M_LN10);
}
#endif

#if !defined(HAVE_EXP10F)
float
exp10f (float x)
{
  return expf (x * M_LN10);
}
#endif

#if !defined(HAVE_EXP10L)
long double
exp10l (long double x)
{
  return expl (x * M_LN10);
}
#endif
