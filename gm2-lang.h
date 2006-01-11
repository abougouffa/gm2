/* Language-dependent hooks for GNU Modula-2.
   Copyright (C) 2003, 2004, 2005, 2006 Free Software Foundation, Inc.
   Contributed by Gaius Mulley <gaius@glam.ac.uk>

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#if !defined(GM2_LANG_H)
#   define GM2_LANG_H

/* Language-dependent contents of an identifier.  */

/* The limbo_value is used for block level extern declarations, which need
   to be type checked against subsequent extern declarations.  They can't
   be referenced after they fall out of scope, so they can't be global.

   The rid_code field is used for keywords.  It is in all
   lang_identifier nodes, because some keywords are only special in a
   particular context.  */

struct lang_identifier GTY(())
{
  struct c_common_identifier common_id;
  tree global_value;
  tree local_value;
  tree label_value;
  tree implicit_decl;
  tree error_locus;
  tree limbo_value;
};


EXTERN void  objc_check_decl PARAMS((tree decl));
EXTERN int   objc_comptypes  PARAMS((tree lhs, tree rhs, int reflexive));

#endif

