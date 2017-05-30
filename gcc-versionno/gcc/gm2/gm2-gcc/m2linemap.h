/* Copyright (C) 2013
 * Free Software Foundation, Inc.
 *
 *  Gaius Mulley <gaius@glam.ac.uk> constructed this file.
 */

/*
This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING.  If not, write to the
Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.
*/

#if !defined(m2linemap_h)

#include "input.h"

#   define m2linemap_h
#   if defined(m2linemap_c)
#      if (__cplusplus)
#         define EXTERN extern "C"
#      else
#         define EXTERN
#      endif
#   else
#      if (__cplusplus)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN void m2linemap_StartFile (void *filename, unsigned int linebegin);
EXTERN void m2linemap_EndFile (void);
EXTERN void m2linemap_StartLine (unsigned int linenumber, unsigned int linesize);
EXTERN location_t m2linemap_GetLocationColumn (unsigned int column);

EXTERN location_t m2linemap_UnknownLocation (void);
EXTERN location_t m2linemap_BuiltinsLocation (void);

#undef EXTERN
#endif
