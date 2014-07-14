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

#if !defined(m2block_h)
#   define m2block_h
#   if defined(m2block_c)
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
#   endif

EXTERN tree m2block_getLabel (location_t location, char *name);
EXTERN void m2block_pushFunctionScope (tree fndecl);
EXTERN tree m2block_popFunctionScope (void);
EXTERN void m2block_pushGlobalScope (void);
EXTERN void m2block_popGlobalScope (void);
EXTERN tree m2block_pushDecl (tree decl);
EXTERN void m2block_addDeclExpr (tree t);

EXTERN tree m2block_end_statement_list (tree t);
EXTERN tree m2block_begin_statement_list (void);
EXTERN tree m2block_push_statement_list (tree t);
EXTERN tree m2block_pop_statement_list (void);

EXTERN void m2block_finishFunctionDecl (tree fndecl);
EXTERN void m2block_finishFunctionCode (tree fndecl);

EXTERN tree m2block_RememberType (tree t);
EXTERN tree m2block_RememberConstant (tree t);
EXTERN tree m2block_DumpGlobalConstants (void);
EXTERN tree m2block_RememberInitModuleFunction (tree t);
EXTERN tree m2block_global_constant (tree t);
EXTERN int m2block_toplevel (void);
EXTERN tree m2block_GetErrorNode (void);

EXTERN tree m2block_cur_stmt_list (void);
EXTERN tree *m2block_cur_stmt_list_addr (void);
EXTERN int m2block_is_building_stmt_list (void);
EXTERN tree m2block_GetGlobals (void);
EXTERN tree m2block_GetGlobalContext (void);
EXTERN tree m2block_finishGlobals (void);
EXTERN tree m2block_includeDecl (tree);

EXTERN void m2block_init (void);

#undef EXTERN
#endif
