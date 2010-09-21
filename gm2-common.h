/* Language-dependent hooks for GNU Modula-2.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
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

#if !defined(GM2_COMMON_H)
#   define GM2_COMMON_H

EXTERN bool gm2_init                      (void);
EXTERN unsigned int  gm2_init_options     (unsigned int argc, const char **argv);
EXTERN int  gm2_handle_option             (size_t scode, const char *arg, int value);
EXTERN void gm2_parse_file                (int debug_flag);
EXTERN void gm2_print_decl		  (FILE *, tree, int);
EXTERN void gm2_print_type		  (FILE *, tree, int);
EXTERN void gm2_init_decl_processing	  (void);
EXTERN bool gm2_mark_addressable          (tree);
EXTERN tree gm2_truthvalue_conversion     (tree expr);
EXTERN tree gm2_type_for_size             (unsigned precision, int unsignedp);
EXTERN tree gm2_type_for_mode             (enum machine_mode mode, int unsignedp);
EXTERN tree gm2_unsigned_type             (tree type_node);
EXTERN tree gm2_signed_type               (tree type_node);
EXTERN tree gm2_signed_or_unsigned_type   (int unsignedp, tree type);
EXTERN tree pushdecl                      (tree x);
EXTERN tree poplevel                      (int keep, int reverse, int functionbody);
EXTERN void set_block                     (tree block);
EXTERN tree getdecls                      (void);
EXTERN rtx  gm2_expand_expr               (tree exp, rtx target,
					   enum machine_mode tmode ATTRIBUTE_UNUSED,
					   int modifier ATTRIBUTE_UNUSED,
					   rtx *alt_rtl ATTRIBUTE_UNUSED);
EXTERN void pushlevel                     (int tag_transparent);
EXTERN int  global_bindings_p             (void);
EXTERN void insert_block                  (tree block);
EXTERN tree builtin_function              (const char *name, tree type, int function_code,
					   enum built_in_class class,
					   const char *library_name,
					   tree attrs ATTRIBUTE_UNUSED);
EXTERN void gm2_expand_function           (tree fndecl);
EXTERN void gm2_enter_nested              (struct function *f);
EXTERN void gm2_leave_nested              (struct function *f);
EXTERN void gm2_write_global_declarations (void);
EXTERN tree gm2_tree_inlining_walk_subtrees (tree *tp ATTRIBUTE_UNUSED,
					     int *subtrees ATTRIBUTE_UNUSED,
					     walk_tree_fn func ATTRIBUTE_UNUSED,
					     void *data ATTRIBUTE_UNUSED,
					     struct pointer_set_t *pset ATTRIBUTE_UNUSED);

#endif
