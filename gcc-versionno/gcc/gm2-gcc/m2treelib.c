/* Copyright (C) 2012.
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
#include "tree-iterator.h"
#include "tree-dump.h"
#include "gimple.h"
#include "cgraph.h"
#include "../gm2-tree.h"
#include "../gm2-lang.h"


/*
 *
 * prototypes
 *
 */

#define m2treelib_c
#include "m2expr.h"
#include "m2statement.h"
#include "m2decl.h"
#include "m2type.h"
#include "m2convert.h"
#include "m2assert.h"
#include "m2tree.h"
#include "m2treelib.h"
#include "m2block.h"
#include "m2treelib.h"



/*
 *  do_jump_if_bit - tests bit in word against integer zero using operator, code.
 *                   If the result is true then jump to label.
 */

void
m2treelib_do_jump_if_bit (location_t location, enum tree_code code, tree word, tree bit, char *label)
{
  word = m2convert_ToWord (word);
  bit = m2convert_ToWord (bit);
  m2statement_DoJump (location,
		      m2expr_build_binary_op (location, code,
					      m2expr_build_binary_op (location, BIT_AND_EXPR,
								      word,
								      m2expr_BuildLSL (location, m2expr_GetWordOne(), bit, FALSE),
								      FALSE),
					      m2expr_GetWordZero (), FALSE),
		      NULL, label);
}


/*
 *  build_modify_expr - taken from c-typeck.c and heavily pruned.
 *
 *  Build an assignment expression of lvalue LHS from value RHS.
 *  If LHS_ORIGTYPE is not NULL, it is the original type of LHS, which
 *  may differ from TREE_TYPE (LHS) for an enum bitfield.
 *  MODIFYCODE is the code for a binary operator that we use
 *  to combine the old value of LHS with RHS to get the new value.
 *  Or else MODIFYCODE is NOP_EXPR meaning do a simple assignment.
 *  If RHS_ORIGTYPE is not NULL_TREE, it is the original type of RHS,
 *  which may differ from TREE_TYPE (RHS) for an enum value.

 *  LOCATION is the location of the MODIFYCODE operator.
 *  RHS_LOC is the location of the RHS.
 */

static
tree
build_modify_expr (location_t location, tree lhs, enum tree_code modifycode, tree rhs)
{
  tree result;
  tree newrhs;
  tree rhs_semantic_type = NULL_TREE;
  tree lhstype = TREE_TYPE (lhs);
  tree olhstype = lhstype;
#if 0
  bool npc;
#endif


  ASSERT_CONDITION (modifycode == NOP_EXPR);

  if (TREE_CODE (rhs) == EXCESS_PRECISION_EXPR)
    {
      rhs_semantic_type = TREE_TYPE (rhs);
      rhs = TREE_OPERAND (rhs, 0);
    }

  newrhs = rhs;

#if 0
  if (TREE_CODE (lhs) == C_MAYBE_CONST_EXPR)
    {
      tree inner = build_modify_expr (location, C_MAYBE_CONST_EXPR_EXPR (lhs),
				      lhs_origtype, modifycode, rhs_loc, rhs,
				      rhs_origtype);
      if (inner == error_mark_node)
	return error_mark_node;
      result = build2 (C_MAYBE_CONST_EXPR, TREE_TYPE (inner),
		       C_MAYBE_CONST_EXPR_PRE (lhs), inner);
      gcc_assert (!C_MAYBE_CONST_EXPR_INT_OPERANDS (lhs));
      C_MAYBE_CONST_EXPR_NON_CONST (result) = 1;
      protected_set_expr_location (result, location);
      return result;
    }
#endif

  /* If storing into a structure or union member,
     it has probably been given type `int'.
     Compute the type that would go with
     the actual amount of storage the member occupies.  */

  if (TREE_CODE (lhs) == COMPONENT_REF
      && (TREE_CODE (lhstype) == INTEGER_TYPE
	  || TREE_CODE (lhstype) == BOOLEAN_TYPE
	  || TREE_CODE (lhstype) == REAL_TYPE
	  || TREE_CODE (lhstype) == ENUMERAL_TYPE))
    lhstype = TREE_TYPE (get_unwidened (lhs, 0));

  /* If storing in a field that is in actuality a short or narrower than one,
     we must store in the field in its actual type.  */

  if (lhstype != TREE_TYPE (lhs))
    {
      lhs = copy_node (lhs);
      TREE_TYPE (lhs) = lhstype;
    }

#if 0
  /* Convert new value to destination type.  Fold it first, then
     restore any excess precision information, for the sake of
     conversion warnings.  */

  npc = null_pointer_constant_p (newrhs);
  newrhs = c_fully_fold (newrhs, false, NULL);
#endif
  newrhs = fold (newrhs);

  if (rhs_semantic_type)
    newrhs = build1 (EXCESS_PRECISION_EXPR, rhs_semantic_type, newrhs);
#if 0
  newrhs = convert_for_assignment (location, lhstype, newrhs, rhs_origtype,
				   ic_assign, npc, NULL_TREE, NULL_TREE, 0);
#endif

  /* Scan operands.  */

  result = build2 (MODIFY_EXPR, lhstype, lhs, newrhs);
  TREE_SIDE_EFFECTS (result) = 1;
  protected_set_expr_location (result, location);

  /* If we got the LHS in a different type for storing in,
     convert the result back to the nominal type of LHS
     so that the value we return always has the same type
     as the LHS argument.  */

  ASSERT_CONDITION (olhstype == TREE_TYPE (result));
  /* in Modula-2 I'm assuming this will be true
     this maybe wrong, but at least I'll know about it soon.  If true then we do not need to implement
     convert_for_assignment - which is huge.
  */

  if (olhstype == TREE_TYPE (result))
    return result;

#if 0
  result = convert_for_assignment (location, olhstype, result, rhs_origtype,
				   ic_assign, false, NULL_TREE, NULL_TREE, 0);

  protected_set_expr_location (result, location);
#endif
  return result;
}


/*
 *  m2treelib_build_modify_expr - wrapper function for build_modify_expr.
 */

tree
m2treelib_build_modify_expr (location_t location,
			     tree des, enum tree_code modifycode, tree copy)
{
  return build_modify_expr (location, des, modifycode, copy);
}


/*
 *  nCount - return the number of trees chained on, t.
 */

static int nCount (tree t)
{
  int i = 0;

  while (t != NULL) {
    i++;
    t = TREE_CHAIN (t);
  }
  return i;
}


/*
 *  DoCall - build a call tree arranging the parameter list as a vector.
 */

tree m2treelib_DoCall (location_t location, tree rettype, tree funcptr, tree param_list)
{
  int n = nCount (param_list);
  tree *argarray = XALLOCAVEC (tree, n);
  tree l = param_list;
  int i;

  for (i = 0; i < n; i++) {
    argarray[i] = TREE_VALUE (l);
    l = TREE_CHAIN (l);
  }
  return build_call_array_loc (location, rettype, funcptr, n, argarray);
}


/*
 *  DoCall0 - build a call tree with no parameters.
 */

tree m2treelib_DoCall0 (location_t location, tree rettype, tree funcptr)
{
  tree *argarray = XALLOCAVEC (tree, 1);

  argarray[0] = NULL_TREE;

  return build_call_array_loc (location, rettype, funcptr, 0, argarray);
}


/*
 *  DoCall1 - build a call tree with 1 parameter.
 */

tree m2treelib_DoCall1 (location_t location, tree rettype, tree funcptr,
			tree arg0)
{
  tree *argarray = XALLOCAVEC (tree, 1);

  argarray[0] = arg0;

  return build_call_array_loc (location, rettype, funcptr, 1, argarray);
}


/*
 *  DoCall2 - build a call tree with 2 parameters.
 */

tree m2treelib_DoCall2 (location_t location, tree rettype, tree funcptr,
			tree arg0, tree arg1)
{
  tree *argarray = XALLOCAVEC (tree, 2);

  argarray[0] = arg0;
  argarray[1] = arg1;

  return build_call_array_loc (location, rettype, funcptr, 2, argarray);
}


/*
 *  DoCall3 - build a call tree with 3 parameters.
 */

tree m2treelib_DoCall3 (location_t location, tree rettype, tree funcptr,
			tree arg0, tree arg1, tree arg2)
{
  tree *argarray = XALLOCAVEC (tree, 3);

  argarray[0] = arg0;
  argarray[1] = arg1;
  argarray[2] = arg2;

  return build_call_array_loc (location, rettype, funcptr, 3, argarray);
}


/*
 *  get_rvalue - returns the rvalue of t. The, type, is the object type to be
 *               copied upon indirection.
 */

tree
m2treelib_get_rvalue (location_t location, tree t, tree type, int is_lvalue)
{
  if (is_lvalue)
    return m2expr_BuildIndirect (location, t, type);
  else
    return t;
}


/*
 *  get_field_no - returns the field no for, op.  The, op, is either
 *                 a constructor or a variable of type record.
 *                 If, op, is a constructor (a set constant in GNU Modula-2)
 *                 then this function is essentially a no-op and it returns op.
 *                 Else we iterate over the field list and return the
 *                 appropriate field number.
 */

tree
m2treelib_get_field_no (tree type, tree op, int is_const, unsigned int fieldNo)
{
  ASSERT_BOOL (is_const);
  if (is_const)
    return op;
  else {
    tree list = TYPE_FIELDS (type);
    while (fieldNo > 0 && list != NULL_TREE) {
      list = TREE_CHAIN (list);
      fieldNo--;
    }
    return list;
  }
}


/*
 *  get_set_value - returns the value indicated by, field, in the set.
 *                  Either p->field or the constant(op.fieldNo) is returned.
 */

tree
m2treelib_get_set_value (location_t location, tree p, tree field, int is_const, tree op, unsigned int fieldNo)
{
  tree value;

  ASSERT_BOOL (is_const);
  if (is_const) {
    gcc_assert( !VEC_empty (constructor_elt, CONSTRUCTOR_ELTS (op)));
    unsigned int size = VEC_length (constructor_elt, CONSTRUCTOR_ELTS (op));
    if (size < fieldNo)
      internal_error ("field number exceeds definition of set");
    value = VEC_index (constructor_elt, CONSTRUCTOR_ELTS (op), fieldNo)->value;
  }
  else
    {
      ASSERT_CONDITION (TREE_CODE (TREE_TYPE (op)) == RECORD_TYPE);
      value = m2expr_BuildComponentRef (op, field);
    }
  value = m2convert_ToBitset (value);
  return value;
}


/*
 *  get_set_address - returns the address of op1.
 */

tree
m2treelib_get_set_address (location_t location, tree op1, int is_lvalue)
{
  if (is_lvalue)
    return op1;
  else
    return m2expr_BuildAddr (location, op1, FALSE);
}


/*
 *  get_set_field_lhs - returns the address of p->field.
 */

tree
m2treelib_get_set_field_lhs (location_t location, tree p, tree field)
{
  return m2expr_BuildAddr (location, m2convert_ToBitset (m2expr_BuildComponentRef (p, field)), FALSE);
}


/*
 *  get_set_field_rhs - returns the value of p->field.
 */

tree
m2treelib_get_set_field_rhs (location_t location, tree p, tree field)
{
  return m2convert_ToBitset (m2expr_BuildComponentRef (p, field));
}


/*
 *  get_set_field_des - returns the p->field ready to be a (rhs) designator.
 */

tree
m2treelib_get_set_field_des (location_t location, tree p, tree field)
{
  return m2expr_BuildComponentRef (p, field);
}


/*
 *  get_set_address_if_var - returns the address of, op, providing
 *                           it is not a constant.
 *                           NULL is returned if, op, is a constant.
 */

tree
m2treelib_get_set_address_if_var (location_t location, tree op, int is_lvalue, int is_const)
{
  if (is_const)
    return NULL;
  else
    return m2treelib_get_set_address (location, op, is_lvalue);
}
