/* Copyright (C) 2012
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

#if !defined(m2convert_h)
#  define m2convert_h
#  if defined(m2convert_c)
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN 
#      endif
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#  endif

EXTERN tree m2convert_BuildConvert (tree type, tree value, int checkOverflow);
EXTERN tree m2convert_ConvertToPtr (tree p);
EXTERN tree m2convert_ConvertString (tree type, tree expr);
EXTERN tree m2convert_ConvertConstantAndCheck (location_t location, tree type, tree expr);
EXTERN tree m2convert_convertToPtr (tree type);
EXTERN tree m2convert_ToCardinal (tree expr);
EXTERN tree m2convert_ToInteger (tree expr);
EXTERN tree m2convert_ToWord (tree expr);
EXTERN tree m2convert_ToBitset (tree expr);

#  undef EXTERN
#endif
