/* Language-dependent hooks for GNU Modula-2.
   Copyright 2003 Free Software Foundation, Inc.
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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#if !defined(GM2_COMMON_H)
#   define GM2_COMMON_H

EXTERN const char *gm2_init		PARAMS ((const char *));
EXTERN int  gm2_decode_option		PARAMS ((int, char **));
EXTERN int  gm2_parse_file              PARAMS ((void));
EXTERN void gm2_print_decl		PARAMS ((FILE *, tree, int));
EXTERN void gm2_print_type		PARAMS ((FILE *, tree, int));
EXTERN void gm2_init_decl_processing	PARAMS ((void));
EXTERN bool gm2_mark_addressable        PARAMS ((tree));
EXTERN tree gm2_truthvalue_conversion   PARAMS ((tree expr));
EXTERN tree gm2_type_for_size           PARAMS ((unsigned precision, int unsignedp));
EXTERN tree gm2_type_for_mode           PARAMS ((enum machine_mode mode, int unsignedp));
EXTERN tree gm2_unsigned_type           PARAMS ((tree type_node));
EXTERN tree gm2_signed_type             PARAMS ((tree type_node));
EXTERN tree gm2_signed_or_unsigned_type PARAMS ((int unsignedp, tree type));
EXTERN tree pushdecl                    PARAMS ((tree x));
EXTERN tree poplevel                    PARAMS ((int keep, int reverse, int functionbody));
EXTERN void set_block                   PARAMS ((tree block));
EXTERN tree getdecls                    PARAMS ((void));
EXTERN rtx  gm2_expand_expr             PARAMS ((tree exp, rtx target, enum machine_mode, int modifier));
EXTERN void pushlevel                   PARAMS ((int tag_transparent));
EXTERN int  global_bindings_p           PARAMS ((void));
EXTERN void insert_block                PARAMS ((tree block));

#endif
