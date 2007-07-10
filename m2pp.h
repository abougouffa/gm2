/* Copyright (C) 2007
 * Free Software Foundation, Inc.
 *
 *  Gaius Mulley <gaius@glam.ac.uk> constructed this file.
 *  It was built by borrowing code from the gcc-.../gcc/c-*.c files
 *  and its function is to aid debugging of the GNU Modula-2 front
 *  end.  It prints the GCC trees in Modula-2 format.
 */

/*
This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
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

#if !defined(M2PP_H)
#   define M2PP_H

#   if defined(M2PP_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

EXTERN void pf (tree t);
EXTERN void pe (tree t);
EXTERN void pt (tree t);
EXTERN void ptl (tree t);
EXTERN void pv (tree t);


#   undef EXTERN
#endif
