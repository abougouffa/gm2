/* Copyright (C) 2008, 2009, 2010, 2011, 2012
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


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "toplev.h"
#include "tm_p.h"
#include "flags.h"
#include <stdio.h>


/*
 *  utilize some of the C build routines
 */

#include "c-tree.h"
#include "rtl.h"
#include "function.h"
#include "expr.h"
#include "output.h"
#include "ggc.h"
#include "intl.h"
#include "convert.h"
#include "target.h"
#include "debug.h"
#include "diagnostic.h"
#include "except.h"
#include "libfuncs.h"
#include "input.h"
#include "../gm2-tree.h"


#define m2tree_c
#include "m2tree.h"


int
m2tree_is_var (tree var)
{
  return TREE_CODE(var) == VAR_DECL;
}

int
m2tree_is_array (tree array)
{
  return TREE_CODE(array) == ARRAY_TYPE;
}

int
m2tree_is_type (tree type)
{
  switch (TREE_CODE(type)) {

  case TYPE_DECL:
  case ARRAY_TYPE:
  case RECORD_TYPE:
  case SET_TYPE:
  case ENUMERAL_TYPE:
  case POINTER_TYPE:
  case INTEGER_TYPE:
  case REAL_TYPE:
  case UNION_TYPE:
  case BOOLEAN_TYPE:
  case COMPLEX_TYPE:
    return TRUE;
  default:
    return FALSE;
  }
}


tree
m2tree_skip_type_decl (tree type)
{
  if (type == error_mark_node)
    return error_mark_node;

  if (type == NULL_TREE)
    return NULL_TREE;

  if (TREE_CODE (type) == TYPE_DECL)
    return TREE_TYPE (type);
  return type;
}


tree
m2tree_skip_const_decl (tree exp)
{
  if (exp == error_mark_node)
    return error_mark_node;

  if (exp == NULL_TREE)
    return NULL_TREE;

  if (TREE_CODE (exp) == CONST_DECL)
    return DECL_INITIAL (exp);
  return exp;
}

/*
 *  m2tree_IsOrdinal - return TRUE if code is an INTEGER, BOOLEAN or ENUMERAL type.
 */

int
m2tree_IsOrdinal (tree type)
{
  enum tree_code code = TREE_CODE (type);

  return (code == INTEGER_TYPE
	  || (code) == BOOLEAN_TYPE
	  || (code) == ENUMERAL_TYPE);
}

/*
 *  is_a_constant - returns TRUE if tree, t, is a constant.
 */

int
m2tree_IsAConstant (tree t)
{
  return (TREE_CODE (t) == INTEGER_CST) ||
         (TREE_CODE (t) == REAL_CST) ||
         (TREE_CODE (t) == REAL_CST) ||
         (TREE_CODE (t) == COMPLEX_CST) ||
         (TREE_CODE (t) == STRING_CST);
}
