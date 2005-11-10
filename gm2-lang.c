/* Language-dependent hooks for GNU Modula-2.
   Copyright 2002 Free Software Foundation, Inc.
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
Boston, MA 02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "toplev.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "ggc.h"
#include "c-common.h"

#define EXTERN extern
#include "gm2-common.h"
#undef EXTERN
#define EXTERN
#include "gm2-lang.h"

static HOST_WIDE_INT gm2_get_alias_set PARAMS ((tree));


/* Structure giving our language-specific hooks.  */

struct language_function GTY(())
{
  int unused;
};

#undef  LANG_HOOKS_NAME
#define LANG_HOOKS_NAME			   "GNU Modula-2"
#undef  LANG_HOOKS_INIT
#define LANG_HOOKS_INIT			   gm2_init
#undef  LANG_HOOKS_DECODE_OPTION
#define LANG_HOOKS_DECODE_OPTION	   gm2_decode_option
#undef LANG_HOOKS_HONOR_READONLY
#define LANG_HOOKS_HONOR_READONLY	   1
#undef LANG_HOOKS_GET_ALIAS_SET
#define LANG_HOOKS_GET_ALIAS_SET	   gm2_get_alias_set

#undef LANG_HOOKS_MARK_ADDRESSABLE
#define LANG_HOOKS_MARK_ADDRESSABLE        gm2_mark_addressable
#undef LANG_HOOKS_TRUTHVALUE_CONVERSION
#define LANG_HOOKS_TRUTHVALUE_CONVERSION   gm2_truthvalue_conversion
#undef LANG_HOOKS_TYPE_FOR_MODE
#define LANG_HOOKS_TYPE_FOR_MODE           gm2_type_for_mode
#undef LANG_HOOKS_TYPE_FOR_SIZE
#define LANG_HOOKS_TYPE_FOR_SIZE           gm2_type_for_size
#undef LANG_HOOKS_SIGNED_TYPE
#define LANG_HOOKS_SIGNED_TYPE             gm2_signed_type
#undef LANG_HOOKS_UNSIGNED_TYPE
#define LANG_HOOKS_UNSIGNED_TYPE           gm2_unsigned_type
#undef LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE
#define LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE gm2_signed_or_unsigned_type
#undef LANG_HOOKS_EXPAND_EXPR
#define LANG_HOOKS_EXPAND_EXPR             gm2_expand_expr
#undef LANG_HOOKS_PARSE_FILE
#define LANG_HOOKS_PARSE_FILE              gm2_parse_file

const struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

/* Tree code classes.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

const char tree_code_type[] = {
#include "tree.def"
  'x',
#include "c-common.def"
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

const unsigned char tree_code_length[] = {
#include "tree.def"
  0,
#include "c-common.def"
};
#undef DEFTREECODE

/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

const char *const tree_code_name[] = {
#include "tree.def"
  "@@dummy",
#include "c-common.def"
};
#undef DEFTREECODE

/* Return the typed-based alias set for T, which may be an expression
   or a type.  Return -1 if we don't do anything special.  */

static HOST_WIDE_INT
gm2_get_alias_set (t)
     tree t ATTRIBUTE_UNUSED;
{
  return -1;
}

void
objc_check_decl (decl)
     tree decl ATTRIBUTE_UNUSED;
{
}

int
objc_comptypes (lhs, rhs, reflexive)
     tree lhs ATTRIBUTE_UNUSED;
     tree rhs ATTRIBUTE_UNUSED;
     int reflexive ATTRIBUTE_UNUSED;
{
  return -1;
}

