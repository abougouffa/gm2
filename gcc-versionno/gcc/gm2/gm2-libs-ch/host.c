/* Copyright (C) 2010, 2011, 2012, 2013, 2014, 2015, 2016
 *               2017
 *               Free Software Foundation, Inc. */
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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
MA 02110-1301, USA */

#include "gm2-libs-host.h"

#if defined(HAVE_MATH_H)
#  include <math.h>
#endif

#if !defined(HAVE_EXP10)
double exp10 (double x)
{
  return exp (x*M_LN10);
}
#endif

#if !defined(HAVE_EXP10F)
float exp10f (float x)
{
  return expf (x*M_LN10);
}
#endif

#if !defined(HAVE_EXP10L)
long double exp10l (long double x)
{
  return expl (x*M_LN10);
}
#endif
