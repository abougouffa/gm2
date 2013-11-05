/* Copyright (C) 2012, 2013
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

#if !defined(dynamicstrings_h)

#   define dynamicstrings_h
#   if defined(dynamicstrings_c)
#       define EXTERN 
#   else
#       define EXTERN extern
#   endif

typedef void *dynamicstrings_string;

EXTERN dynamicstrings_string DynamicStrings_Mark (dynamicstrings_string s);
EXTERN dynamicstrings_string DynamicStrings_InitStringCharStar (dynamicstrings_string s);

#undef EXTERN
#endif
