/* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
 *               2010
 *               Free Software Foundation, Inc.

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
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#ifndef GCC_GM2_TREE_H
#define GCC_GM2_TREE_H

/* Modula-2 language-specific tree codes.  */
#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) SYM,
enum m2_tree_code {
  M2_DUMMY_TREE_CODE = LAST_C_TREE_CODE,
#include "gm2-tree.def"
  LAST_M2_TREE_CODE
};
#undef DEFTREECODE

#endif

/* These macros provide convenient access to the various _STMT nodes
   created when parsing template declarations.  */
#define TRY_STMTS(NODE)		TREE_OPERAND (TRY_BLOCK_CHECK (NODE), 0)
#define TRY_HANDLERS(NODE)	TREE_OPERAND (TRY_BLOCK_CHECK (NODE), 1)

/* Nonzero if this try block is a function try block.  */
#define FN_TRY_BLOCK_P(NODE)	TREE_LANG_FLAG_3 (TRY_BLOCK_CHECK (NODE))
#define HANDLER_PARMS(NODE)	TREE_OPERAND (HANDLER_CHECK (NODE), 0)
#define HANDLER_BODY(NODE)	TREE_OPERAND (HANDLER_CHECK (NODE), 1)
#define HANDLER_TYPE(NODE)	TREE_TYPE (HANDLER_CHECK (NODE))

/* STMT_EXPR accessor.  */
#define STMT_EXPR_STMT(NODE)	TREE_OPERAND (STMT_EXPR_CHECK (NODE), 0)

/* EXPR_STMT accessor. This gives the expression associated with an
   expression statement.  */
#define EXPR_STMT_EXPR(NODE)	TREE_OPERAND (EXPR_STMT_CHECK (NODE), 0)
