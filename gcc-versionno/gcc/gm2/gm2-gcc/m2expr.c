/* Copyright (C) 2012, 2013, 2014, 2015, 2016.
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


#include "gcc-consolidation.h"

#include "../gm2-tree.h"
#include "../gm2-lang.h"
#include "m2convert.h"


/*
 *
 * prototypes
 *
 */
#define m2expr_c
#include "m2expr.h"
#include "m2statement.h"
#include "m2decl.h"
#include "m2type.h"
#include "m2convert.h"
#include "m2assert.h"
#include "m2tree.h"
#include "m2treelib.h"


static int label_count = 0;
static GTY(()) tree set_full_complement;


/*
 *  CompareTrees - returns -1 if e1 < e2, 0 if e1 == e2, and 1 if e1 > e2.
 */

int m2expr_CompareTrees (tree e1, tree e2)
{
  return tree_int_cst_compare (m2expr_FoldAndStrip (e1), m2expr_FoldAndStrip (e2));
}


/*
 *  FoldAndStrip - return expression, t, after it has been folded (if possible).
 */

tree
m2expr_FoldAndStrip (tree t)
{
  if (t != NULL) {
    t = fold (t);
    if (TREE_CODE (t) == CONST_DECL)
      return m2expr_FoldAndStrip (DECL_INITIAL (t));
  }

  return t;
}


/*
 *  StringLength - returns an unsigned int which is the length
 *                 of, string.
 */

unsigned int
m2expr_StringLength (tree string)
{
  return TREE_STRING_LENGTH (string);
}


/*
 *
 */

static
tree
CheckAddressToCardinal (location_t location, tree op)
{
  if (m2type_IsAddress (TREE_TYPE (op)))
    return m2convert_BuildConvert (location, m2type_GetCardinalAddressType (), op, FALSE);
  return op;
}


/*
 *  BuildAdd - builds an addition tree.
 */

tree
m2expr_BuildAdd (location_t location, tree op1, tree op2, int needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op (location, PLUS_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}


/*
 *  BuildSub - builds a subtraction tree.
 */

tree
m2expr_BuildSub (location_t location, tree op1, tree op2, int needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op (location, MINUS_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}


/*
 *  BuildDivTrunc - builds a division tree.
 */

tree
m2expr_BuildDivTrunc (location_t location, tree op1, tree op2, int needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op (location, TRUNC_DIV_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}


/*
 *  BuildModTrunc - builds a modulus tree.
 */

tree
m2expr_BuildModTrunc (location_t location, tree op1, tree op2, int needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op (location, TRUNC_MOD_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}


/*
 *  BuildDivFloor - builds a division tree.
 */

tree
m2expr_BuildDivFloor (location_t location, tree op1, tree op2, int needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op (location, FLOOR_DIV_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}


/*
 *  BuildRDiv - builds a division tree (this should only be used for REAL and COMPLEX
 *              types and NEVER for integer based types).
 */

tree
m2expr_BuildRDiv (location_t location, tree op1, tree op2, int needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  t = m2expr_build_binary_op (location, RDIV_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}


/*
 *  BuildModFloor - builds a modulus tree.
 */

tree
m2expr_BuildModFloor (location_t location, tree op1, tree op2, int needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op (location, FLOOR_MOD_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}


/*
 *  BuildLSL - builds and returns tree (op1 << op2)
 */

tree
m2expr_BuildLSL (location_t location, tree op1, tree op2, int needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  t = m2expr_build_binary_op (location, LSHIFT_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}


/*
 *  BuildLSR - builds and returns tree (op1 >> op2)
 */

tree
m2expr_BuildLSR (location_t location, tree op1, tree op2, int needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  t = m2expr_build_binary_op (location, RSHIFT_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}


/*
 *  createUniqueLabel - returns a unique label which has been alloc'ed
 */

static char *
createUniqueLabel (void)
{
  int size, i;
  char *label;

  label_count++;
  i = label_count;
  size = strlen(".LSHIFT")+2;
  while (i>0) {
    i /= 10;
    size++;
  }
  label = (char *) ggc_alloc_atomic (size);
  sprintf(label, ".LSHIFT%d", label_count);
  return label;
}


/*
 *  BuildLogicalShift - builds the ISO Modula-2 SHIFT operator
 *                      for a fundamental data type.
 */

void
m2expr_BuildLogicalShift (location_t location, tree op1, tree op2, tree op3,
                          tree nBits ATTRIBUTE_UNUSED,
                          int needconvert)
{
  tree res;

  m2assert_AssertLocation (location);
  op2 = m2expr_FoldAndStrip (op2);
  op3 = m2expr_FoldAndStrip (op3);
  if (TREE_CODE (op3) == INTEGER_CST) {
    op2 = m2convert_ToWord (location, op2);
    if (tree_int_cst_sgn (op3) < 0)
      res = m2expr_BuildLSR (location, op2,
                             m2convert_ToWord (location, m2expr_BuildNegate (location, op3, needconvert)),
                             needconvert);
    else
      res = m2expr_BuildLSL (location, op2, m2convert_ToWord (location, op3), needconvert);
    res = m2convert_BuildConvert (location, m2tree_skip_type_decl (TREE_TYPE (op1)), res, FALSE);
    m2statement_BuildAssignmentTree (location, op1, res);
  }
  else {
    char *labelElseName = createUniqueLabel ();
    char *labelEndName  = createUniqueLabel ();
    tree  is_less       = m2expr_BuildLessThan (location,
						m2convert_ToInteger (location, op3),
						m2expr_GetIntegerZero (location));

    m2statement_DoJump (location, is_less, NULL, labelElseName);
    op2 = m2convert_ToWord (location, op2);
    op3 = m2convert_ToWord (location, op3);
    res = m2expr_BuildLSL (location, op2, op3, needconvert);
    res = m2convert_BuildConvert (location, m2tree_skip_type_decl (TREE_TYPE (op1)), res, FALSE);
    m2statement_BuildAssignmentTree (location, op1, res);
    m2statement_BuildGoto (location, labelEndName);
    m2statement_DeclareLabel (location, labelElseName);
    res = m2expr_BuildLSR (location, op2,
                           m2expr_BuildNegate (location, op3, needconvert),
                           needconvert);
    res = m2convert_BuildConvert (location, m2tree_skip_type_decl (TREE_TYPE (op1)), res, FALSE);
    m2statement_BuildAssignmentTree (location, op1, res);
    m2statement_DeclareLabel (location, labelEndName);
  }
}


/*
 *  BuildLRL - builds and returns tree (op1 rotate left by op2 bits)
 */

tree
m2expr_BuildLRL (location_t location, tree op1, tree op2, int needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  t = m2expr_build_binary_op (location, LROTATE_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}


/*
 *  BuildLRR - builds and returns tree (op1 rotate right by op2 bits)
 */

tree
m2expr_BuildLRR (location_t location, tree op1, tree op2, int needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  t = m2expr_build_binary_op (location, RROTATE_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}


/*
 *  m2expr_BuildMask - returns a tree for the mask of a set
 *                     of nBits. It assumes nBits is <= TSIZE(WORD)
 */

tree
m2expr_BuildMask (location_t location, tree nBits, int needconvert)
{
  tree mask = m2expr_BuildLSL (location, m2expr_GetIntegerOne (location), nBits, needconvert);
  m2assert_AssertLocation (location);
  return m2expr_BuildSub (location, mask, m2expr_GetIntegerOne (location), needconvert);
}


/*
 *  m2expr_BuildLRotate - returns a tree in which op1 has been left rotated by
 *                        nBits.
 *                        It assumes nBits is <= TSIZE(WORD)
 */

tree
m2expr_BuildLRotate (location_t location, tree op1, tree nBits, int needconvert)
{
  tree t;

  op1 = m2expr_FoldAndStrip (op1);
  nBits = m2expr_FoldAndStrip (nBits);
  t = m2expr_build_binary_op (location, LROTATE_EXPR, op1, nBits, needconvert);
  return m2expr_FoldAndStrip (t);
}


/*
 *  m2expr_BuildRRotate - returns a tree in which op1 has been left rotated by
 *                        nBits.
 *                        It assumes nBits is <= TSIZE(WORD)
 */

tree
m2expr_BuildRRotate (location_t location, tree op1, tree nBits, int needconvert)
{
  tree t;

  op1 = m2expr_FoldAndStrip (op1);
  nBits = m2expr_FoldAndStrip (nBits);
  t = m2expr_build_binary_op (location, RROTATE_EXPR, op1, nBits, needconvert);
  return m2expr_FoldAndStrip (t);
}


/*
 *  BuildLRLn - builds and returns tree (op1 rotate left by op2 bits)
 *              it rotates a set of size, nBits.
 */

tree
m2expr_BuildLRLn (location_t location, tree op1, tree op2, tree nBits,
                  int  needconvert)
{
  tree op2min;

  m2assert_AssertLocation (location);
  /*
   *  ensure we wrap the rotate
   */
  op2min = m2expr_BuildModTrunc (location,
				 m2convert_ToCardinal (location, op2),
				 m2convert_ToCardinal (location, nBits), needconvert);
  /*
   *  optimize if we are we going to rotate a TSIZE(BITSET) set
   */
  if (m2expr_CompareTrees (m2decl_BuildIntegerConstant (m2decl_GetBitsPerBitset ()),
                           nBits) == 0)
    return m2expr_BuildLRotate (location, op1, op2min, needconvert);
  else {
    tree mask = m2expr_BuildMask (location, nBits, needconvert);
    tree left, right;

    /*
     *  make absolutely sure there are no high order bits lying around
     */
    op1 = m2expr_BuildLogicalAnd (location, op1, mask, needconvert);
    left = m2expr_BuildLSL (location, op1, op2min, needconvert);
    left = m2expr_BuildLogicalAnd (location, left, mask, needconvert);
    right = m2expr_BuildLSR (location,
			     op1, m2expr_BuildSub (location,
						   m2convert_ToCardinal (location, nBits), op2min, needconvert),
                             needconvert);
    return m2expr_BuildLogicalOr (location, left, right, needconvert);
  }
}


/*
 *  BuildLRRn - builds and returns tree (op1 rotate right by op2 bits)
 *              it rotates a set of size, nBits.
 */

tree
m2expr_BuildLRRn (location_t location, tree op1, tree op2, tree nBits, int needconvert)
{
  tree op2min;

  m2assert_AssertLocation (location);
  /*
   *  ensure we wrap the rotate
   */
  op2min = m2expr_BuildModTrunc (location,
				 m2convert_ToCardinal (location, op2),
				 m2convert_ToCardinal (location, nBits),
				 needconvert);
  /*
   *  optimize if we are we going to rotate a TSIZE(BITSET) set
   */
  if (m2expr_CompareTrees (m2decl_BuildIntegerConstant (m2decl_GetBitsPerBitset ()),
                           nBits) == 0)
    return m2expr_BuildRRotate (location, op1, op2min, needconvert);
  else {
    tree mask = m2expr_BuildMask (location, nBits, needconvert);
    tree left, right;

    /*
     *  make absolutely sure there are no high order bits lying around
     */
    op1 = m2expr_BuildLogicalAnd (location, op1, mask, needconvert);
    right = m2expr_BuildLSR (location, op1, op2min, needconvert);
    left = m2expr_BuildLSL (location,
			    op1, m2expr_BuildSub (location,
						  m2convert_ToCardinal (location, nBits), op2min, needconvert),
                            needconvert);
    left = m2expr_BuildLogicalAnd (location, left, mask, needconvert);
    return m2expr_BuildLogicalOr (location, left, right, needconvert);
  }
}


/*
 *  BuildLogicalRotate - builds the ISO Modula-2 ROTATE operator
 *                       for a fundamental data type.
 */

void
m2expr_BuildLogicalRotate (location_t location,
			   tree op1, tree op2, tree op3, tree nBits, int needconvert)
{
  tree res;

  m2assert_AssertLocation (location);
  op2 = m2expr_FoldAndStrip (op2);
  op3 = m2expr_FoldAndStrip (op3);
  if (TREE_CODE (op3) == INTEGER_CST) {
    if (tree_int_cst_sgn (op3) < 0)
      res = m2expr_BuildLRRn (location, op2,
			      m2expr_BuildNegate (location, op3, needconvert),
			      nBits,
			      needconvert);
    else
      res = m2expr_BuildLRLn (location, op2, op3, nBits, needconvert);
    m2statement_BuildAssignmentTree (location, op1, res);
  }
  else {
    char *labelElseName = createUniqueLabel ();
    char *labelEndName  = createUniqueLabel ();
    tree  is_less       = m2expr_BuildLessThan (location,
						m2convert_ToInteger (location, op3),
						m2expr_GetIntegerZero (location));

    m2statement_DoJump (location, is_less, NULL, labelElseName);
    res = m2expr_BuildLRLn (location, op2, op3, nBits, needconvert);
    m2statement_BuildAssignmentTree (location, op1, res);
    m2statement_BuildGoto (location, labelEndName);
    m2statement_DeclareLabel (location, labelElseName);
    res = m2expr_BuildLRRn (location, op2,
                            m2expr_BuildNegate (location, op3, needconvert),
                            nBits,
                            needconvert);
    m2statement_BuildAssignmentTree (location, op1, res);
    m2statement_DeclareLabel (location, labelEndName);
  }
}


/*
 *  buildUnboundedArrayOf - constructs an unbounded struct and returns
 *                          the gcc tree. The two fields of the structure
 *                          are initialized to contentsPtr and high.
 */

static
tree
buildUnboundedArrayOf (tree unbounded, tree contentsPtr, tree high)
{
  tree fields     = TYPE_FIELDS (unbounded);
  tree field_list = NULL_TREE;
  tree constructor;

  field_list = tree_cons (fields, contentsPtr, field_list);
  fields = TREE_CHAIN (fields);

  field_list = tree_cons (fields, high, field_list);

  constructor = build_constructor_from_list (unbounded, nreverse (field_list));
  TREE_CONSTANT (constructor) = 0;
  TREE_STATIC (constructor) = 0;

  return constructor;
}


/*
 *  BuildBinarySetDo - if the size of the set is <= TSIZE(WORD) then
 *                        op1 := binop(op2, op3)
 *                     else
 *                        call m2rtsprocedure(op1, op2, op3)
 */

void
m2expr_BuildBinarySetDo (location_t location, tree settype, tree op1, tree op2, tree op3,
                         void (*binop)(location_t, tree, tree, tree, tree, int),
                         int is_op1lvalue, int is_op2lvalue, int is_op3lvalue,
                         tree nBits,
                         tree unbounded,
                         tree varproc, tree leftproc, tree rightproc)
{
  tree size     = m2expr_GetSizeOf (location, settype);
  int  is_const = FALSE;
  int  is_left  = FALSE;

  m2assert_AssertLocation (location);

  ASSERT_BOOL (is_op1lvalue);
  ASSERT_BOOL (is_op2lvalue);
  ASSERT_BOOL (is_op3lvalue);

  if (m2expr_CompareTrees (size, m2decl_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    (*binop) (location,
	      m2treelib_get_rvalue (location, op1, settype, is_op1lvalue),
              m2treelib_get_rvalue (location, op2, settype, is_op2lvalue),
              m2treelib_get_rvalue (location, op3, settype, is_op3lvalue),
              nBits,
              FALSE);
  else {
    tree result;
    tree high = m2expr_BuildSub (location,
				 m2convert_ToCardinal (location, m2expr_BuildDivTrunc (location, size,
										       m2expr_GetSizeOf (location, m2type_GetBitsetType ()), FALSE)),
                                 m2expr_GetCardinalOne (location), FALSE);

    /*
     * if op3 is constant
     * then
     *    make op3 positive and remember which direction we are shifting
     * fi
     */
    op3 = m2tree_skip_const_decl (op3);
    if (TREE_CODE (op3) == INTEGER_CST) {
      is_const = TRUE;
      if (tree_int_cst_sgn (op3) < 0)
        op3 = m2expr_BuildNegate (location, op3, FALSE);
      else
        is_left = TRUE;
      op3 = m2convert_BuildConvert (location, m2type_GetM2CardinalType (), op3, FALSE);
    }

    /*
     *  these parameters must match the prototypes of the procedures:
     *  ShiftLeft, ShiftRight, ShiftVal, RotateLeft, RotateRight, RotateVal
     *  inside gm2-iso/SYSTEM.mod.
     *
     *  Remember we must build the parameters in reverse.
     */

    /* parameter 4 amount */
    m2statement_BuildParam (location,
			    m2convert_BuildConvert (location,
						    m2type_GetM2IntegerType(),
						    m2treelib_get_rvalue (location,
								op3, m2tree_skip_type_decl (TREE_TYPE (op3)),
								is_op3lvalue), FALSE));

    /* parameter 3 nBits */
    m2statement_BuildParam (location,
			    m2convert_BuildConvert (location,
						    m2type_GetM2CardinalType(), m2expr_FoldAndStrip (nBits), FALSE));

    /* parameter 2 destination set */
    m2statement_BuildParam (location,
			    buildUnboundedArrayOf (unbounded,
						   m2treelib_get_set_address (location, op1,
									      is_op1lvalue),
						   high));

    /* parameter 1 source set */
    m2statement_BuildParam (location,
			    buildUnboundedArrayOf (unbounded,
						   m2treelib_get_set_address (location, op2,
								    is_op2lvalue),
						   high));

    /* now call the appropriate procedure inside SYSTEM.mod */
    if (is_const)
      if (is_left)
        result = m2statement_BuildProcedureCallTree (location, leftproc, NULL_TREE);
      else
        result = m2statement_BuildProcedureCallTree (location, rightproc, NULL_TREE);
    else
      result = m2statement_BuildProcedureCallTree (location, varproc, NULL_TREE);
    add_stmt (location, result);
  }
}


/* Print a warning if a constant expression had overflow in folding.
   Invoke this function on every expression that the language
   requires to be a constant expression.
*/

void
m2expr_ConstantExpressionWarning (tree value)
{
  if ((TREE_CODE (value) == INTEGER_CST || TREE_CODE (value) == REAL_CST
       || TREE_CODE (value) == FIXED_CST
       || TREE_CODE (value) == VECTOR_CST
       || TREE_CODE (value) == COMPLEX_CST)
      && TREE_OVERFLOW (value))
    pedwarn (input_location, OPT_Woverflow, "overflow in constant expression");
}


/*
 *  TreeOverflow - returns TRUE if the contant expression, t, has
 *                 caused an overflow. No error message or warning
 *                 is emitted and no modification is made to, t.
 */

int
m2expr_TreeOverflow (tree t)
{
  if ((TREE_CODE (t) == INTEGER_CST
       || (TREE_CODE (t) == COMPLEX_CST
           && TREE_CODE (TREE_REALPART (t)) == INTEGER_CST))
      && TREE_OVERFLOW (t))
    return TRUE;
  else if ((TREE_CODE (t) == REAL_CST
            || (TREE_CODE (t) == COMPLEX_CST
                && TREE_CODE (TREE_REALPART (t)) == REAL_CST))
           && TREE_OVERFLOW (t))
    return TRUE;
  else
    return FALSE;
}


/*
 *  RemoveOverflow - if tree, t, is a constant expression it removes
 *                   any overflow flag and returns, t.
 */

tree
m2expr_RemoveOverflow (tree t)
{
  if (TREE_CODE (t) == INTEGER_CST
      || (TREE_CODE (t) == COMPLEX_CST
          && TREE_CODE (TREE_REALPART (t)) == INTEGER_CST))
    TREE_OVERFLOW (t) = 0;
  else if (TREE_CODE (t) == REAL_CST
           || (TREE_CODE (t) == COMPLEX_CST
               && TREE_CODE (TREE_REALPART (t)) == REAL_CST))
    TREE_OVERFLOW (t) = 0;
  return t;
}


/*
 *  BuildCoerce - returns a tree containing the expression, expr, after
 *                it has been coersed to, type.
 */

tree
m2expr_BuildCoerce (location_t location, tree des, tree type, tree expr)
{
  tree copy = copy_node (expr);
  TREE_TYPE (copy) = type;

  m2assert_AssertLocation (location);

  return m2treelib_build_modify_expr (location, des, NOP_EXPR, copy);
}


/*
 *  BuildTrunc - returns an integer expression from a REAL or LONGREAL op1.
 */

tree
m2expr_BuildTrunc (tree op1)
{
  return convert_to_integer (m2type_GetIntegerType (), m2expr_FoldAndStrip (op1));
}


/*
 *  build_unary_op -
 */

tree
m2expr_build_unary_op (location_t location,
		       enum tree_code code, tree arg, int flag ATTRIBUTE_UNUSED)
{
  tree argtype = TREE_TYPE (arg);
  tree result;

  m2assert_AssertLocation (location);

  arg = m2expr_FoldAndStrip (arg);
  result = build1 (code, argtype, arg);
  protected_set_expr_location (result, location);

  return m2expr_FoldAndStrip (result);
}


/*
 *  build_binary_op - this is a heavily pruned version of the one found in c-typeck.c.
 *                    The Modula-2 expression rules are much more restricted than C.
 */

tree
build_binary_op (location_t location,
		 enum tree_code code, tree op1, tree op2,
		 int convert ATTRIBUTE_UNUSED)
{
  tree type1 = TREE_TYPE (op1);
  tree result;

  m2assert_AssertLocation (location);

  /* Strip NON_LVALUE_EXPRs, etc., since we aren't using as an lvalue.  */
  STRIP_TYPE_NOPS (op1);
  STRIP_TYPE_NOPS (op2);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  result = build2 (code, type1, op1, op2);
  protected_set_expr_location (result, location);

  return m2expr_FoldAndStrip (result);
}


#if 0
/*
 *  default_convert_binary_operands -
 */

static void
default_convert_binary_operands (location_t location, tree *op1, tree *op2)
{
  tree type1 = m2tree_skip_type_decl (TREE_TYPE (*op1));
  tree type2 = m2tree_skip_type_decl (TREE_TYPE (*op2));

  m2assert_AssertLocation (location);

  if (type1 != type2)
    {
      if (type1 == m2type_GetM2ZType ())
	*op1 = m2convert_BuildConvert (type2, *op1, FALSE);
      else if (type2 == m2type_GetM2ZType ())
	*op2 = m2convert_BuildConvert (type1, *op2, FALSE);

      else if (type1 == m2type_GetM2RType ())
	*op1 = m2convert_BuildConvert (type2, *op1, FALSE);
      else if (type2 == m2type_GetM2RType ())
	*op2 = m2convert_BuildConvert (type1, *op2, FALSE);
    }
}


/* Return a NOP_EXPR converting EXPR to TYPE.  */

static
tree
build_nop (tree type, tree expr)
{
  if (type == error_mark_node || error_operand_p (expr))
    return expr;
  return build1 (NOP_EXPR, type, expr);
}
#endif


/*
 *  build_binary_op - a wrapper for the lower level build_binary_op above.
 */

tree
m2expr_build_binary_op (location_t location,
			enum tree_code code, tree op1, tree op2,
			int convert)
{
  tree type1, type2;

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  type1 = m2tree_skip_type_decl (TREE_TYPE (op1));
  type2 = m2tree_skip_type_decl (TREE_TYPE (op2));

  m2assert_AssertLocation (location);

  if (convert || TRUE)  // --fixme-- hack
    {
      if (code == PLUS_EXPR)
	{
	  if (POINTER_TYPE_P (type1))
	    {
	      op2 = fold_convert_loc (location, sizetype, unshare_expr (op2));
	      return fold_build2_loc (location, POINTER_PLUS_EXPR,
				      TREE_TYPE (op1), op1, op2);
	    }
	  else if (POINTER_TYPE_P (type2))
	    {
	      op1 = fold_convert_loc (location, sizetype, unshare_expr (op1));
	      return fold_build2_loc (location, POINTER_PLUS_EXPR,
				      TREE_TYPE (op2), op2, op1);
	    }
	}
      if (code == MINUS_EXPR)
	{
	  if (POINTER_TYPE_P (type1))
	    {
	      op2 = fold_convert_loc (location, sizetype, unshare_expr (op2));
	      op2 = fold_build1_loc (location, NEGATE_EXPR, sizetype, op2);
	      return fold_build2_loc (location, POINTER_PLUS_EXPR,
				      TREE_TYPE (op1), op1, op2);
	    }
	  else if (POINTER_TYPE_P (type2))
	    {
	      op2 = fold_convert_loc (location, sizetype, unshare_expr (op2));
	      op2 = fold_build1_loc (location, NEGATE_EXPR, sizetype, op2);
	      op1 = fold_convert_loc (location, sizetype, unshare_expr (op1));
	      return fold_build2_loc (location, POINTER_PLUS_EXPR,
				      TREE_TYPE (op2), op2, op1);
	    }
	}
    }

#if 0
  default_convert_binary_operands (location, &op1, &op2);
  type1 = m2tree_skip_type_decl (TREE_TYPE (op1));
  type2 = m2tree_skip_type_decl (TREE_TYPE (op2));
#endif

  if ((code != LSHIFT_EXPR) && (code != RSHIFT_EXPR)
      && (code != LROTATE_EXPR) && (code == RROTATE_EXPR))
    if (type1 != type2)
      error_at (location, "not expecting different types to binary operator");

  return build_binary_op (location, code, op1, op2, convert);
}


/*
 *  BuildAddAddress - returns an expression op1+op2 where op1 is a pointer type
 *                    and op2 is not a pointer type.
 */

tree
m2expr_BuildAddAddress (location_t location, tree op1, tree op2)
{
  tree type1, type2;

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  type1 = m2tree_skip_type_decl (TREE_TYPE (op1));
  type2 = m2tree_skip_type_decl (TREE_TYPE (op2));

  m2assert_AssertLocation (location);
  ASSERT_CONDITION (POINTER_TYPE_P (type1));
  ASSERT_CONDITION (! POINTER_TYPE_P (type2));

  op2 = fold_convert_loc (location, sizetype, unshare_expr (op2));
  return fold_build2_loc (location, POINTER_PLUS_EXPR,
			  TREE_TYPE (op1),
			  m2expr_FoldAndStrip (op1),
			  m2expr_FoldAndStrip (op2));
}

/*
 *  BuildNegate - builds a negate expression and returns the tree.
 */

tree
m2expr_BuildNegate (location_t location, tree op1, int needconvert)
{
#if 0
  tree type = TREE_TYPE (op1);
  enum tree_code code = TREE_CODE (type);

  if (code == ENUMERAL_TYPE)
    error_at (location, "not expecting to negate an enumerated value");
#endif

  m2assert_AssertLocation (location);
  return m2expr_build_unary_op (location, NEGATE_EXPR,
				m2expr_FoldAndStrip (op1), needconvert);
}


/*
 *  BuildSetNegate - builds a set negate expression and returns the tree.
 */

tree
m2expr_BuildSetNegate (location_t location, tree op1, int needconvert)
{
  m2assert_AssertLocation (location);

  return m2expr_build_binary_op (location, BIT_XOR_EXPR,
				 m2convert_BuildConvert (location,
							 m2type_GetWordType (),
							 m2expr_FoldAndStrip (op1), FALSE),
				 set_full_complement,
				 needconvert);
}


/*
 *  BuildMult - builds a multiplication tree.
 */

tree
m2expr_BuildMult (location_t location, tree op1, tree op2, int needconvert)
{
  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  m2assert_AssertLocation (location);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  return m2expr_build_binary_op (location, MULT_EXPR,
				 op1, op2, needconvert);
}


/*
 *  testLimits - returns the number of bits required to represent:  min..max
 *               if it matches the, type.  Otherwise NULL_TREE is returned.
 */

static tree
testLimits (location_t location, tree type, tree min, tree max)
{
  m2assert_AssertLocation (location);

  if ((m2expr_CompareTrees (TYPE_MAX_VALUE (type), max) == 0) &&
      (m2expr_CompareTrees (TYPE_MIN_VALUE (type), min) == 0))
    return m2expr_BuildMult (location,
			     m2expr_GetSizeOf (location, type),
			     m2decl_BuildIntegerConstant (BITS_PER_UNIT),
			     FALSE);
  return NULL_TREE;
}


/*
 *  noBitsRequired - returns the number of bits required to contain, values.
 */

static tree
noBitsRequired (tree values)
{
  int bits = tree_floor_log2 (values);

  if (integer_pow2p (values))
    return m2decl_BuildIntegerConstant (bits+1);
  else
    return m2decl_BuildIntegerConstant (bits+1);
}


/*
 *  getMax - returns the result of max(a, b).
 */

static tree
getMax (tree a, tree b)
{
  if (m2expr_CompareTrees (a, b) > 0)
    return a;
  else
    return b;
}


/*
 *  calcNbits - returns the smallest number of bits required to represent:
 *              min..max.
 */

static tree
calcNbits (location_t location, tree min, tree max)
{
  int negative = FALSE;
  tree t = testLimits (location, m2type_GetIntegerType (), min, max);

  m2assert_AssertLocation (location);

  if (t == NULL)
    t = testLimits (location, m2type_GetCardinalType (), min, max);

  if (t == NULL)
    {
      if (m2expr_CompareTrees (min, m2expr_GetIntegerZero (location)) < 0)
	{
	  min = m2expr_BuildAdd (location, min, m2expr_GetIntegerOne (location), FALSE);
	  min = fold (m2expr_BuildNegate (location, min, FALSE));
	  negative = TRUE;
	}
      if (m2expr_CompareTrees (max, m2expr_GetIntegerZero (location)) < 0)
	{
	  max = fold (m2expr_BuildNegate (location, max, FALSE));
	  negative = TRUE;
	}
      t = noBitsRequired (getMax (min, max));
      if (negative)
	t = m2expr_BuildAdd (location, t, m2expr_GetIntegerOne (location), FALSE);
    }
  return t;
}


/*
 *  BuildTBitSize - returns the minimum number of bits to represent, type.
 */

tree
m2expr_BuildTBitSize (location_t location, tree type)
{
  enum tree_code code = TREE_CODE (type);
  tree min;
  tree max;
  m2assert_AssertLocation (location);

  switch (code) {

  case TYPE_DECL:
    return m2expr_BuildTBitSize (location, TREE_TYPE (type));
  case INTEGER_TYPE:
  case ENUMERAL_TYPE:
    max = m2convert_BuildConvert (location, m2type_GetIntegerType (), TYPE_MAX_VALUE (type), FALSE);
    min = m2convert_BuildConvert (location, m2type_GetIntegerType (), TYPE_MIN_VALUE (type), FALSE);
    return calcNbits (location, min, max);
  case BOOLEAN_TYPE:
    return m2expr_GetIntegerOne (location);
  default:
    return m2expr_BuildMult (location,
			     m2expr_GetSizeOf (location, type),
			     m2decl_BuildIntegerConstant (BITS_PER_UNIT),
			     FALSE);
  }
}


/*
 *  BuildSize - builds a SIZE function expression and returns the tree.
 */

tree
m2expr_BuildSize (location_t location, tree op1, int needconvert ATTRIBUTE_UNUSED)
{
  m2assert_AssertLocation (location);
  return m2expr_GetSizeOf (location, op1);
}


/*
 *  BuildAddr - builds an expression which calculates the address of
 *              op1 and returns the tree.  If use_generic is TRUE then
 *              create a generic pointer type.
 */

tree
m2expr_BuildAddr (location_t location, tree op1, int use_generic)
{
  tree type = m2tree_skip_type_decl (TREE_TYPE (op1));
  tree ptrType = build_pointer_type (type);
  tree result;

  m2assert_AssertLocation (location);

  if (! gm2_mark_addressable (op1))
    error_at (location, "cannot take the address of this expression");

  if (use_generic)
    result = build1 (ADDR_EXPR, m2type_GetPointerType (), op1);
  else
    result = build1 (ADDR_EXPR, ptrType, op1);
  protected_set_expr_location (result, location);
  return result;
}


/*
 *  BuildOffset1 - builds an expression containing the number of bytes the field
 *                 is offset from the start of the record structure.
 *                 This function is the same as the above, except that it
 *                 derives the record from the field and then calls BuildOffset.
 *                 The expression is returned.
 */

tree
m2expr_BuildOffset1 (location_t location, tree field, int needconvert ATTRIBUTE_UNUSED)
{
  m2assert_AssertLocation (location);
  return m2expr_BuildOffset (location, DECL_CONTEXT (field), field, needconvert);
}


/*
 *  determinePenultimateField - returns the field associated with the DECL_CONTEXT (field)
 *                              within a record or varient.
 *                              record, is a record/varient but it maybe an outer nested record
 *                              to the field that we are searching. Ie:
 *
 *                              record = RECORD
 *                                          x: CARDINAL ;
 *                                          y: RECORD
 *                                                field: CARDINAL ;
 *                                             END
 *                                       END ;
 *
 *                              determinePenultimateField (record, field) returns, y.
 *
 *                              we are assurred that the chain of records leading to field
 *                              will be unique as they are built on the fly to implement varient
 *                              records.
 */

static
tree
determinePenultimateField (tree record, tree field)
{
  tree fieldlist = TYPE_FIELDS (record);
  tree x, r;

  for (x = fieldlist; x; x = TREE_CHAIN (x)) {
    if (DECL_CONTEXT (field) == TREE_TYPE (x))
      return x;
    switch (TREE_CODE (TREE_TYPE (x))) {
      case RECORD_TYPE:
      case UNION_TYPE:
        r = determinePenultimateField (TREE_TYPE (x), field);
        if (r != NULL)
          return r;
        break;
      default:
        break;
    }
  }
  return NULL_TREE;
}


/*
 *  BuildOffset - builds an expression containing the number of bytes the field
 *                is offset from the start of the record structure.
 *                The expression is returned.
 */

tree
m2expr_BuildOffset (location_t location,
		    tree record, tree field,
                    int needconvert ATTRIBUTE_UNUSED)
{
  m2assert_AssertLocation (location);

  if (DECL_CONTEXT (field) == record)
    return m2convert_BuildConvert (location,
				   m2type_GetIntegerType (),
				   m2expr_BuildAdd (location,
						    DECL_FIELD_OFFSET (field),
						    m2expr_BuildDivTrunc (location,
									  DECL_FIELD_BIT_OFFSET (field),
									  m2decl_BuildIntegerConstant (BITS_PER_UNIT),
									  FALSE),
						    FALSE),
				   FALSE);
  else {
    tree r1 = DECL_CONTEXT (field);
    tree r2 = determinePenultimateField (record, field);
    return m2convert_BuildConvert (location,
				   m2type_GetIntegerType (),
				   m2expr_BuildAdd (location,
						    m2expr_BuildOffset (location, r1, field, needconvert),
						    m2expr_BuildOffset (location, record, r2, needconvert),
						    FALSE),
				   FALSE);
  }
}


/*
 *  BuildLogicalOrAddress - build a logical or expressions and return the tree.
 */

tree
m2expr_BuildLogicalOrAddress (location_t location,
			      tree op1, tree op2,
                              int  needconvert)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (location, BIT_IOR_EXPR, op1, op2, needconvert);
}


/*
 *  BuildLogicalOr - build a logical or expressions and return the tree.
 */

tree
m2expr_BuildLogicalOr (location_t location, tree op1, tree op2,
                       int needconvert)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (location, BIT_IOR_EXPR,
				 m2convert_BuildConvert (location, m2type_GetWordType (), op1, FALSE),
				 m2convert_BuildConvert (location, m2type_GetWordType (), op2, FALSE), needconvert);
}


/*
 *  BuildLogicalAnd - build a logical and expression and return the tree.
 */

tree
m2expr_BuildLogicalAnd (location_t location, tree op1, tree op2,
                        int needconvert)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (location, BIT_AND_EXPR,
				 m2convert_BuildConvert (location, m2type_GetWordType (), op1, FALSE),
				 m2convert_BuildConvert (location, m2type_GetWordType (), op2, FALSE), needconvert);
}


/*
 *  BuildSymmetricalDifference - build a logical xor expression and return the tree.
 */

tree
m2expr_BuildSymmetricDifference (location_t location, tree op1, tree op2,
                                 int needconvert)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (location, BIT_XOR_EXPR,
				 m2convert_BuildConvert (location, m2type_GetWordType (), op1, FALSE),
				 m2convert_BuildConvert (location, m2type_GetWordType (), op2, FALSE), needconvert);
}


/*
 *  BuildLogicalDifference - build a logical difference expression and
 *                           return the tree.
 *                           (op1 and (not op2))
 */

tree
m2expr_BuildLogicalDifference (location_t location, tree op1, tree op2,
                               int needconvert)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (location, BIT_AND_EXPR,
				 m2convert_BuildConvert (location, m2type_GetWordType (), op1, FALSE),
				 m2expr_BuildSetNegate (location, op2, needconvert),
				 needconvert);
}


/* Returns the base type of an ordinal subrange, or the type
 * itself if it is not a subrange.
 */
static
tree
base_type (tree type)
{
  if (type == error_mark_node)
    return error_mark_node;

  /* Check for ordinal subranges.
   */
  if (m2tree_IsOrdinal (type)
      && TREE_TYPE (type))
    type = TREE_TYPE (type);
  return TYPE_MAIN_VARIANT (type);
}


/*
 *  boolean_enum_to_unsigned - converts BOOLEAN_TYPEs and
 *                             ENUMERAL_TYPEs to unsigned int.
 */

static tree
boolean_enum_to_unsigned (location_t location, tree t)
{
  tree type = TREE_TYPE (t);

#if 0
  if (TREE_CODE (skip_type_decl (type)) == BOOLEAN_TYPE)
    return convert (integer_type_node, t);
#endif
  if (TREE_CODE (base_type (type)) == BOOLEAN_TYPE)
    return m2convert_BuildConvert (location, unsigned_type_node, t, FALSE);
  else if (TREE_CODE (base_type (type)) == ENUMERAL_TYPE)
    return m2convert_BuildConvert (location, unsigned_type_node, t, FALSE);
  else
    return t;
}


/*
 *  check_for_comparison - checks to see if, op, is of type, badType.
 *                         If so then it returns op after it has been
 *                         cast to, goodType.  op will be an array
 *                         so we take the address and cast the contents.
 */

static tree
check_for_comparison (location_t location, tree op, tree badType, tree goodType)
{
  m2assert_AssertLocation (location);
  if (m2tree_skip_type_decl (TREE_TYPE (op)) == badType)
    /* cannot compare array contents in m2expr_build_binary_op */
    return m2expr_BuildIndirect (location, m2expr_BuildAddr (location, op, FALSE), goodType);
  return op;
}


/*
 *  convert_for_comparison - returns a tree which can be used as an argument
 *                           during a comparison.
 */

static tree
convert_for_comparison (location_t location, tree op)
{
  m2assert_AssertLocation (location);
  op = boolean_enum_to_unsigned (location, op);

  op = check_for_comparison (location, op, m2type_GetISOWordType (), m2type_GetWordType ());
  op = check_for_comparison (location, op, m2type_GetM2Word16 (), m2type_GetM2Cardinal16 ());
  op = check_for_comparison (location, op, m2type_GetM2Word32 (), m2type_GetM2Cardinal32 ());
  op = check_for_comparison (location, op, m2type_GetM2Word64 (), m2type_GetM2Cardinal64 ());

  return op;
}


/*
 *  BuildLessThan - return a tree which computes <
 */

tree
m2expr_BuildLessThan (location_t location, tree op1, tree op2)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (location, LT_EXPR,
				 boolean_enum_to_unsigned (location, op1),
				 boolean_enum_to_unsigned (location, op2), TRUE);
}


/*
 *  BuildGreaterThan - return a tree which computes >
 */

tree
m2expr_BuildGreaterThan (location_t location, tree op1, tree op2)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (location, GT_EXPR,
				 boolean_enum_to_unsigned (location, op1),
				 boolean_enum_to_unsigned (location, op2), TRUE);
}


/*
 *  BuildLessThanOrEqual - return a tree which computes <
 */

tree
m2expr_BuildLessThanOrEqual (location_t location, tree op1, tree op2)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (location, LE_EXPR,
				 boolean_enum_to_unsigned (location, op1),
				 boolean_enum_to_unsigned (location, op2), TRUE);
}


/*
 *  BuildGreaterThanOrEqual - return a tree which computes >=
 */

tree
m2expr_BuildGreaterThanOrEqual (location_t location, tree op1, tree op2)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (location, GE_EXPR,
				 boolean_enum_to_unsigned (location, op1),
				 boolean_enum_to_unsigned (location, op2), TRUE);
}


/*
 *  BuildEqualTo - return a tree which computes =
 */

tree
m2expr_BuildEqualTo (location_t location, tree op1, tree op2)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (location, EQ_EXPR,
				 convert_for_comparison (location, op1),
				 convert_for_comparison (location, op2), TRUE);
}


/*
 *  BuildEqualNotTo - return a tree which computes #
 */

tree
m2expr_BuildNotEqualTo (location_t location, tree op1, tree op2)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (location, NE_EXPR,
				 convert_for_comparison (location, op1),
				 convert_for_comparison (location, op2), TRUE);
}


/*
 *  BuildIsSuperset - return a tree which computes:  op1 & op2 == op2
 */

tree
m2expr_BuildIsSuperset (location_t location, tree op1, tree op2)
{
  m2assert_AssertLocation (location);
  return m2expr_BuildEqualTo (location, op2,
                              m2expr_BuildLogicalAnd (location, op1, op2, FALSE));
}


/*
 *  BuildIsNotSuperset - return a tree which computes: op1 & op2 != op2
 */

tree
m2expr_BuildIsNotSuperset (location_t location, tree op1, tree op2)
{
  m2assert_AssertLocation (location);
  return m2expr_BuildNotEqualTo (location, op2,
                                 m2expr_BuildLogicalAnd (location, op1, op2, FALSE));
}


/*
 *  BuildIsSubset - return a tree which computes:  op1 & op2 == op1
 */

tree
m2expr_BuildIsSubset (location_t location, tree op1, tree op2)
{
  m2assert_AssertLocation (location);
  return m2expr_BuildEqualTo (location, op1,
                              m2expr_BuildLogicalAnd (location, op1, op2, FALSE));
}


/*
 *  BuildIsNotSubset - return a tree which computes: op1 & op2 != op1
 */

tree
m2expr_BuildIsNotSubset (location_t location, tree op1, tree op2)
{
  m2assert_AssertLocation (location);
  return m2expr_BuildNotEqualTo (location, op1,
                                 m2expr_BuildLogicalAnd (location, op1, op2, FALSE));
}


/*
 *  BuildIfConstInVar - generates: if constel in varset then goto label.
 */

void
m2expr_BuildIfConstInVar (location_t location, tree type, tree varset, tree constel,
                          int is_lvalue, int fieldno,
                          char *label)
{
  tree size = m2expr_GetSizeOf (location, type);
  m2assert_AssertLocation (location);

  ASSERT_BOOL (is_lvalue);
  if (m2expr_CompareTrees (size, m2decl_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    m2treelib_do_jump_if_bit (location, NE_EXPR, m2treelib_get_rvalue (location, varset, type, is_lvalue), constel, label);
  else {
    tree fieldlist = TYPE_FIELDS (type);
    tree field;

    for (field = fieldlist; (field != NULL) && (fieldno>0); field = TREE_CHAIN (field))
      fieldno--;

    m2treelib_do_jump_if_bit (location, NE_EXPR, m2treelib_get_set_field_rhs (location, varset, field), constel, label);
  }
}


/*
 *  BuildIfConstInVar - generates: if not (constel in varset) then goto label.
 */

void
m2expr_BuildIfNotConstInVar (location_t location, tree type, tree varset, tree constel,
                             int is_lvalue, int fieldno,
                             char *label)
{
  tree size = m2expr_GetSizeOf (location, type);

  m2assert_AssertLocation (location);

  ASSERT_BOOL (is_lvalue);
  if (m2expr_CompareTrees (size, m2decl_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    m2treelib_do_jump_if_bit (location, EQ_EXPR, m2treelib_get_rvalue (location, varset, type, is_lvalue), constel, label);
  else {
    tree fieldlist = TYPE_FIELDS (type);
    tree field;

    for (field = fieldlist; (field != NULL) && (fieldno>0); field = TREE_CHAIN (field))
      fieldno--;

    m2treelib_do_jump_if_bit (location, EQ_EXPR, m2treelib_get_set_field_rhs (location, varset, field), constel, label);
  }
}


/*
 *  BuildIfVarInVar - generates: if varel in varset then goto label
 */

void
m2expr_BuildIfVarInVar (location_t location,
			tree type, tree varset, tree varel,
                        int is_lvalue,
                        tree  low,
                        tree  high ATTRIBUTE_UNUSED,
                        char *label)
{
  tree size = m2expr_GetSizeOf (location, type);
  /* calculate the index from the first bit, ie bit 0 represents low value */
  tree index = m2expr_BuildSub (location,
				m2convert_BuildConvert (location, m2type_GetIntegerType(), varel, FALSE),
                                m2convert_BuildConvert (location, m2type_GetIntegerType(), low, FALSE), FALSE);

  m2assert_AssertLocation (location);

  if (m2expr_CompareTrees (size, m2decl_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    m2treelib_do_jump_if_bit (location, NE_EXPR, m2treelib_get_rvalue (location, varset, type, is_lvalue), index, label);
  else {
    tree p1               = m2treelib_get_set_address (location, varset, is_lvalue);
    /* which word do we need to fetch? */
    tree word_index       = m2expr_FoldAndStrip (m2expr_BuildDivTrunc (location, index, m2decl_BuildIntegerConstant (SET_WORD_SIZE), FALSE));
    /* calculate the bit in this word */
    tree offset_into_word = m2expr_FoldAndStrip (m2expr_BuildModTrunc (location, index, m2decl_BuildIntegerConstant (SET_WORD_SIZE), FALSE));
    tree p2               = m2expr_FoldAndStrip (m2expr_BuildMult (location,
								   word_index, m2decl_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT),
								   FALSE));

    /* calculate the address of the word we are interested in */
    p1 = m2expr_BuildAddAddress (location, m2convert_convertToPtr (location, p1), p2);

    /* fetch the word, extract the bit and test for != 0 */
    m2treelib_do_jump_if_bit (location, NE_EXPR, m2expr_BuildIndirect (location, p1, m2type_GetBitsetType ()), offset_into_word, label);
  }
}


/*
 *  BuildIfNotVarInVar - generates: if not (varel in varset) then goto label
 */

void
m2expr_BuildIfNotVarInVar (location_t location,
			   tree type, tree varset, tree varel,
                           int is_lvalue,
                           tree  low,
                           tree  high ATTRIBUTE_UNUSED,
                           char *label)
{
  tree size = m2expr_GetSizeOf (location, type);
  /* calculate the index from the first bit, ie bit 0 represents low value */
  tree index = m2expr_BuildSub (location,
				m2convert_BuildConvert (location, m2type_GetIntegerType(), m2expr_FoldAndStrip (varel), FALSE),
                                m2convert_BuildConvert (location, m2type_GetIntegerType(), m2expr_FoldAndStrip (low), FALSE), FALSE);

  index = m2expr_FoldAndStrip (index);
  m2assert_AssertLocation (location);

  if (m2expr_CompareTrees (size, m2decl_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    m2treelib_do_jump_if_bit (location, EQ_EXPR, m2treelib_get_rvalue (location, varset, type, is_lvalue), index, label);
  else {
    tree p1               = m2treelib_get_set_address (location, varset, is_lvalue);
    /* calculate the index from the first bit */

    /* which word do we need to fetch? */
    tree word_index       = m2expr_FoldAndStrip (m2expr_BuildDivTrunc (location, index, m2decl_BuildIntegerConstant (SET_WORD_SIZE), FALSE));
    /* calculate the bit in this word */
    tree offset_into_word = m2expr_FoldAndStrip (m2expr_BuildModTrunc (location, index, m2decl_BuildIntegerConstant (SET_WORD_SIZE), FALSE));
    tree p2               = m2expr_FoldAndStrip (m2expr_BuildMult (location, word_index,
								   m2decl_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT),
								   FALSE));

    /* calculate the address of the word we are interested in */
    p1 = m2expr_BuildAddAddress (location, p1, p2);

    /* fetch the word, extract the bit and test for == 0 */
    m2treelib_do_jump_if_bit (location, EQ_EXPR, m2expr_BuildIndirect (location, p1, m2type_GetBitsetType ()), offset_into_word, label);
  }
}


/*
 *  BuildForeachWordInSetDoIfExpr - foreach word in set, type, compute the expression, expr, and if true
 *                                  goto label.
 */

void
m2expr_BuildForeachWordInSetDoIfExpr (location_t location,
				      tree type, tree op1, tree op2,
                                      int is_op1lvalue, int is_op2lvalue,
                                      int  is_op1const, int is_op2const,
                                      tree (*expr) (location_t, tree, tree),
                                      char *label)
{
  tree p1 = m2treelib_get_set_address_if_var (location, op1, is_op1lvalue, is_op1const);
  tree p2 = m2treelib_get_set_address_if_var (location, op2, is_op2lvalue, is_op2const);
  unsigned int fieldNo = 0;
  tree field1 = m2treelib_get_field_no (type, op1, is_op1const, fieldNo);
  tree field2 = m2treelib_get_field_no (type, op2, is_op2const, fieldNo);

  m2assert_AssertLocation (location);
  ASSERT_CONDITION (TREE_CODE (TREE_TYPE (op1)) == RECORD_TYPE);
  ASSERT_CONDITION (TREE_CODE (TREE_TYPE (op2)) == RECORD_TYPE);

  while (field1 != NULL && field2 != NULL) {
    m2statement_DoJump (location,
			(*expr) (location,
				 m2treelib_get_set_value (location, p1, field1, is_op1const, is_op1lvalue, op1, fieldNo),
				 m2treelib_get_set_value (location, p2, field2, is_op2const, is_op2lvalue, op2, fieldNo)),
			NULL, label);
    fieldNo++;
    field1 = m2treelib_get_field_no (type, op1, is_op1const, fieldNo);
    field2 = m2treelib_get_field_no (type, op2, is_op2const, fieldNo);
  }
}


/*
 *  BuildIfInRangeGoto - if var is in the range low..high then goto label
 */

void
m2expr_BuildIfInRangeGoto (location_t location, tree var, tree low, tree high, char *label)
{
  m2assert_AssertLocation (location);

  if (m2expr_CompareTrees (low, high) == 0)
    m2statement_DoJump (location, m2expr_BuildEqualTo (location, var, low),
			NULL, label);
  else
    m2statement_DoJump (location,
			m2expr_build_binary_op (location,
						TRUTH_ANDIF_EXPR,
						m2expr_BuildGreaterThanOrEqual (location, var, low),
						m2expr_BuildLessThanOrEqual (location, var, high), FALSE),
			NULL, label);
}


/*
 *  BuildIfNotInRangeGoto - if var is not in the range low..high then goto label
 */

void
m2expr_BuildIfNotInRangeGoto (location_t location, tree var, tree low, tree high, char *label)
{
  m2assert_AssertLocation (location);

  if (m2expr_CompareTrees (low, high) == 0)
    m2statement_DoJump (location,
			m2expr_BuildNotEqualTo (location, var, low),
			NULL, label);
  else
    m2statement_DoJump (location,
			m2expr_build_binary_op (location,
						TRUTH_ORIF_EXPR,
						m2expr_BuildLessThan (location, var, low),
						m2expr_BuildGreaterThan (location, var, high), FALSE),
			NULL, label);
}


/*
 *  BuildArray - returns a tree which accesses array[index]
 *               given, lowIndice.
 */

tree
m2expr_BuildArray (tree type, tree array, tree index, tree lowIndice)
{
  type = m2tree_skip_type_decl (type);

  return build4 (ARRAY_REF, type, array, index, lowIndice, NULL_TREE);
}


/*
 *  BuildComponentRef - build a component reference tree which accesses record.field.
 *                      If field does not belong to record it calls
 *                      BuildComponentRef on the penultimate field.
 */

tree
m2expr_BuildComponentRef (location_t location, tree record, tree field)
{
  tree recordType = m2tree_skip_reference_type (m2tree_skip_type_decl (TREE_TYPE (record)));

  if (DECL_CONTEXT (field) == recordType)
    return build3 (COMPONENT_REF, TREE_TYPE (field), record, field, NULL_TREE);
  else {
    tree f = determinePenultimateField (recordType, field);
    return m2expr_BuildComponentRef (location,
				     m2expr_BuildComponentRef (location, record, f), field);
  }
}


/*
 *  BuildIndirect - build: (*target) given that the object to be copied is of, type.
 */

tree
m2expr_BuildIndirect (location_t location ATTRIBUTE_UNUSED, tree target, tree type)
{
  /*
   *   Note that the second argument to build1 is:
   *
   *   TYPE_QUALS is a list of modifiers such as const or volatile
   *   to apply to the pointer type, represented as identifiers.
   *
   *   it also determines the type of arithmetic and size of
   *   the object to be indirectly moved.
   */

  tree t1 = m2tree_skip_type_decl (type);
  tree t2 = build_pointer_type (t1);

  m2assert_AssertLocation (location);

  return build1 (INDIRECT_REF, t1, m2convert_BuildConvert (location, t2, target, FALSE));
}


/*
 *  IsTrue - returns TRUE if, t, is known to be TRUE.
 */

int
m2expr_IsTrue (tree t)
{
  return (m2expr_FoldAndStrip (t) == m2type_GetBooleanTrue ());
}


/*
 *  IsFalse - returns FALSE if, t, is known to be FALSE.
 */

int
m2expr_IsFalse (tree t)
{
  return (m2expr_FoldAndStrip (t) == m2type_GetBooleanFalse ());
}


/*
 *  AreConstantsEqual - maps onto tree.c (tree_int_cst_equal). It returns
 *                      TRUE if the value of e1 is the same as e2.
 */

int
m2expr_AreConstantsEqual (tree e1, tree e2)
{
  return tree_int_cst_equal (e1, e2) != 0;
}


/*
 *  AreRealOrComplexConstantsEqual - returns TRUE if constants,
 *                                   e1 and e2 are equal according
 *                                   to IEEE rules.  This does not
 *                                   perform bit equivalence for
 *                                   example IEEE states that
 *                                   -0 == 0 and NaN != NaN.
 */

int
m2expr_AreRealOrComplexConstantsEqual (tree e1, tree e2)
{
  if (TREE_CODE (e1) == COMPLEX_CST)
    return (m2expr_AreRealOrComplexConstantsEqual (TREE_REALPART (e1), TREE_REALPART (e2)) &&
            m2expr_AreRealOrComplexConstantsEqual (TREE_IMAGPART (e1), TREE_IMAGPART (e2)));
  else
    return real_compare (EQ_EXPR, &TREE_REAL_CST (e1), &TREE_REAL_CST (e2));
}


/*
 *  DetermineSign - returns -1 if e<0
 *                           0 if e==0
 *                           1 if e>0
 *
 *                  an unsigned constant will never return -1
 */

int
m2expr_DetermineSign (tree e)
{
  return tree_int_cst_sgn (e);
}


/*
 * Similar to build_int_2 () but allows you to specify the type of the
 * integer constant that you are creating.
 */

static
tree
build_int_2_type (HOST_WIDE_INT low, HOST_WIDE_INT hi, tree type)
{
  tree value;
  HOST_WIDE_INT ival[3];

  ival[0] = low;
  ival[1] = hi;
  ival[2] = 0;

  widest_int wval = widest_int::from_array (ival, 3);
  value = wide_int_to_tree (type, wval);

  return value;
}


/*
 *  BuildCap - builds the Modula-2 function CAP(t) and returns
 *             the result in a gcc Tree.
 */

tree
m2expr_BuildCap (location_t location, tree t)
{
  tree tt;
  tree out_of_range, less_than, greater_than, translated;

  m2assert_AssertLocation (location);

  t = fold (t);
  if (t == error_mark_node)
    return error_mark_node;

  tt = TREE_TYPE (t);

  t = fold (convert (m2type_GetM2CharType (), t));

  if (TREE_CODE (tt) == INTEGER_TYPE) {
    less_than = fold (m2expr_build_binary_op (location, LT_EXPR, t,
					      build_int_2_type ('a', 0,
								m2type_GetM2CharType ()), 0));
    greater_than = fold (m2expr_build_binary_op (location, GT_EXPR, t,
						 build_int_2_type ('z', 0,
								   m2type_GetM2CharType ()), 0));
    out_of_range = fold (m2expr_build_binary_op (location, TRUTH_ORIF_EXPR,
						 less_than, greater_than, 0));

    translated = fold (convert (m2type_GetM2CharType (),
				m2expr_build_binary_op (location, MINUS_EXPR, t,
							build_int_2_type ('a'-'A', 0,
									  m2type_GetM2CharType ()), 0)));

    return fold_build3 (COND_EXPR, m2type_GetM2CharType (), out_of_range, t, translated);
  }

  error_at (location, "argument to CAP is not a constant or variable of type CHAR");
  return error_mark_node;
}


/*
 *  BuildAbs - builds the Modula-2 function ABS(t) and returns
 *             the result in a gcc Tree.
 */

tree
m2expr_BuildAbs (location_t location, tree t)
{
  m2assert_AssertLocation (location);

  return m2expr_build_unary_op (location, ABS_EXPR, t, 0);
}


/*
 *  BuildRe - builds an expression for the function RE.
 */

tree
m2expr_BuildRe (tree op1)
{
  op1 = m2expr_FoldAndStrip (op1);
  if (TREE_CODE (op1) == COMPLEX_CST)
    return fold_build1 (REALPART_EXPR, TREE_TYPE (TREE_TYPE (op1)), op1);
  else
    return build1 (REALPART_EXPR, TREE_TYPE (TREE_TYPE (op1)), op1);
}


/*
 *  BuildIm - builds an expression for the function IM.
 */

tree
m2expr_BuildIm (tree op1)
{
  op1 = m2expr_FoldAndStrip (op1);
  if (TREE_CODE (op1) == COMPLEX_CST)
    return fold_build1 (IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (op1)), op1);
  else
    return build1 (IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (op1)), op1);
}


/*
 *  BuildCmplx - builds an expression for the function CMPLX.
 */

tree
m2expr_BuildCmplx (location_t location, tree type, tree real, tree imag)
{
  tree scalor;
  real = m2expr_FoldAndStrip (real);
  imag = m2expr_FoldAndStrip (imag);
  type = m2tree_skip_type_decl (type);
  scalor = TREE_TYPE (type);

  if (scalor != TREE_TYPE (real))
    real = m2convert_BuildConvert (location, scalor, real, FALSE);
  if (scalor != TREE_TYPE (imag))
    imag = m2convert_BuildConvert (location, scalor, imag, FALSE);

  if ((TREE_CODE (real) == REAL_CST) && (TREE_CODE (imag) == REAL_CST))
    return build_complex (type, real, imag);
  else
    return build2 (COMPLEX_EXPR, type, real, imag);
}


/*
 *  BuildBinaryForeachWordDo - provides the large set operators. Each word
 *                             (or less) of the set can be calculated by binop.
 *                             This procedure runs along each word of the
 *                             large set invoking the binop.
 */

void
m2expr_BuildBinaryForeachWordDo (location_t location, tree type, tree op1, tree op2, tree op3,
                                 tree  (*binop)(location_t, tree, tree, int),
                                 int is_op1lvalue, int is_op2lvalue, int is_op3lvalue,
                                 int is_op1const, int is_op2const, int is_op3const)
{
  tree size = m2expr_GetSizeOf (location, type);

  m2assert_AssertLocation (location);

  ASSERT_BOOL (is_op1lvalue);
  ASSERT_BOOL (is_op2lvalue);
  ASSERT_BOOL (is_op3lvalue);
  ASSERT_BOOL (is_op1const);
  ASSERT_BOOL (is_op2const);
  ASSERT_BOOL (is_op3const);
  if (m2expr_CompareTrees (size, m2decl_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    m2statement_BuildAssignmentTree (location, m2treelib_get_rvalue (location, op1, type, is_op1lvalue),
				     (*binop) (location,
					       m2treelib_get_rvalue (location, op2, type, is_op2lvalue),
					       m2treelib_get_rvalue (location, op3, type, is_op3lvalue), FALSE));
  else {
    /* large set size > TSIZE(WORD) */

    tree p2     = m2treelib_get_set_address_if_var (location, op2, is_op2lvalue, is_op2const);
    tree p3     = m2treelib_get_set_address_if_var (location, op3, is_op3lvalue, is_op3const);
    unsigned int fieldNo = 0;
    tree field1 = m2treelib_get_field_no (type, op1, is_op1const, fieldNo);
    tree field2 = m2treelib_get_field_no (type, op2, is_op2const, fieldNo);
    tree field3 = m2treelib_get_field_no (type, op3, is_op3const, fieldNo);

    if (is_op1const)
      error_at (location, "internal error: not expecting operand1 to be a constant set");

    while (field1 != NULL && field2 != NULL && field3 != NULL) {
      m2statement_BuildAssignmentTree (location,
				       m2treelib_get_set_field_des (location, op1, field1),
				       (*binop) (location,
						 m2treelib_get_set_value (location, p2, field2,
									  is_op2const, is_op2lvalue, op2, fieldNo),
						 m2treelib_get_set_value (location, p3, field3,
									  is_op3const, is_op3lvalue, op3, fieldNo), FALSE));
      fieldNo++;
      field1 = m2treelib_get_field_no (type, op1, is_op1const, fieldNo);
      field2 = m2treelib_get_field_no (type, op2, is_op2const, fieldNo);
      field3 = m2treelib_get_field_no (type, op3, is_op3const, fieldNo);
    }
  }
}


/* Append DIGIT to NUM, a number of PRECISION bits being read in base
   BASE.  */
static int
append_digit (unsigned HOST_WIDE_INT *low, HOST_WIDE_INT *high,
              unsigned int digit, unsigned int base)
{
  unsigned int shift;
  int overflow;
  HOST_WIDE_INT add_high, res_high;
  unsigned HOST_WIDE_INT add_low, res_low;

  switch (base) {

  case 2:  shift = 1; break;
  case 8:  shift = 3; break;
  case 10: shift = 3; break;
  case 16: shift = 4; break;

  default:
    shift = 3;
    error("internal error: not expecting this base value for a constant");
  }

  /* Multiply by 2, 8 or 16.  Catching this overflow here means we don't
     need to worry about add_high overflowing.  */
  if (((*high) >> (INT_TYPE_SIZE - shift)) == 0)
    overflow = FALSE;
  else
    overflow = TRUE;

  res_high = *high << shift;
  res_low = *low << shift;
  res_high |= (*low) >> (INT_TYPE_SIZE - shift);

  if (base == 10)
    {
      add_low = (*low) << 1;
      add_high = ((*high) << 1) + ((*low) >> (INT_TYPE_SIZE - 1));
    }
  else
    add_high = add_low = 0;

  if (add_low + digit < add_low)
    add_high++;
  add_low += digit;

  if (res_low + add_low < res_low)
    add_high++;
  if (res_high + add_high < res_high)
    overflow = TRUE;

  *low = res_low + add_low;
  *high = res_high + add_high;

  return overflow;
}


/*
 *  interpret_integer - converts an integer constant into two integer
 *                      constants.  Heavily borrowed from gcc/cppexp.c.
 */

int
m2expr_interpret_integer (const char *str, unsigned int base,
			  unsigned HOST_WIDE_INT *low, HOST_WIDE_INT *high)
{
  unsigned const char *p, *end;
  int overflow = FALSE;
  int len;

  *low = 0;
  *high = 0;
  p = (unsigned const char *)str;
  len = strlen(str);
  end = p + len;

  /* Common case of a single digit.  */
  if (len == 1)
    *low = p[0] - '0';
  else
    {
      unsigned int c = 0;

      /* We can add a digit to numbers strictly less than this without
         needing the precision and slowness of double integers.  */

      unsigned HOST_WIDE_INT max = ~(unsigned HOST_WIDE_INT) 0;
      max = (max - base + 1) / base + 1;

      for (; p < end; p++)
        {
          c = *p;

          if (ISDIGIT (c) || (base == 16 && ISXDIGIT (c)))
            c = hex_value (c);
          else
            return overflow;

          /* Strict inequality for when max is set to zero.  */
          if (*low < max)
            *low = (*low) * base + c;
          else
            {
              overflow = append_digit (low, high, c, base);
              max = 0;  /* from now on we always use append_digit */
            }
        }
    }
  return overflow;
}


/* Append DIGIT to NUM, a number of PRECISION bits being read in base
   BASE.  */
static int
append_m2_digit (unsigned int *low, int *high,
                 unsigned int digit, unsigned int base)
{
  unsigned int shift;
  int overflow;
  int add_high, res_high;
  unsigned int add_low, res_low;

  switch (base) {

  case 2:  shift = 1; break;
  case 8:  shift = 3; break;
  case 10: shift = 3; break;
  case 16: shift = 4; break;

  default:
    shift = 3;
    error("internal error: not expecting this base value for a constant");
  }

  /* Multiply by 2, 8 or 16.  Catching this overflow here means we don't
     need to worry about add_high overflowing.  */
  if (((*high) >> (INT_TYPE_SIZE - shift)) == 0)
    overflow = FALSE;
  else
    overflow = TRUE;

  res_high = *high << shift;
  res_low = *low << shift;
  res_high |= (*low) >> (INT_TYPE_SIZE - shift);

  if (base == 10)
    {
      add_low = (*low) << 1;
      add_high = ((*high) << 1) + ((*low) >> (INT_TYPE_SIZE - 1));
    }
  else
    add_high = add_low = 0;

  if (add_low + digit < add_low)
    add_high++;
  add_low += digit;

  if (res_low + add_low < res_low)
    add_high++;
  if (res_high + add_high < res_high)
    overflow = TRUE;

  *low = res_low + add_low;
  *high = res_high + add_high;

  return overflow;
}


/*
 * interpret_m2_integer - converts an integer constant into two integer
 *                        constants. Heavily borrowed from gcc/cppexp.c.
 *                        Note that this is a copy of the above code
 *                        except that it uses `int' rather than
 *                        HOST_WIDE_INT to allow gm2 to determine
 *                        what Modula-2 base type to use for this
 *                        constant.
 */

int
m2expr_interpret_m2_integer (const char *str, unsigned int base,
			     unsigned int *low, int *high)
{
  const unsigned char *p, *end;
  int overflow = FALSE;
  int len;

  *low = 0;
  *high = 0;
  p = (unsigned const char *)str;
  len = strlen(str);
  end = p + len;

  /* Common case of a single digit.  */
  if (len == 1)
    *low = p[0] - '0';
  else
    {
      unsigned int c = 0;

      /* We can add a digit to numbers strictly less than this without
         needing the precision and slowness of double integers.  */

      unsigned int max = ~(unsigned int) 0;
      max = (max - base + 1) / base + 1;

      for (; p < end; p++)
        {
          c = *p;

          if (ISDIGIT (c) || (base == 16 && ISXDIGIT (c)))
            c = hex_value (c);
          else
            return overflow;

          /* Strict inequality for when max is set to zero.  */
          if (*low < max)
            *low = (*low) * base + c;
          else
            {
              overflow = append_m2_digit (low, high, c, base);
              max = 0;  /* from now on we always use append_digit */
            }
        }
    }
  return overflow;
}


/*
 *  m2expr_GetSizeOfInBits - returns the number of bits used to contain, type.
 */

tree
m2expr_GetSizeOfInBits (tree type)
{
  enum tree_code code = TREE_CODE (type);

  if (code == FUNCTION_TYPE)
    return m2expr_GetSizeOfInBits (ptr_type_node);

  if (code == VOID_TYPE) {
    error ("sizeof applied to a void type");
    return size_one_node;
  }

  if (code == VAR_DECL)
    return m2expr_GetSizeOfInBits (TREE_TYPE (type));

  if (code == PARM_DECL)
    return m2expr_GetSizeOfInBits (TREE_TYPE (type));

  if (code == TYPE_DECL)
    return m2expr_GetSizeOfInBits (TREE_TYPE (type));

  if (code == COMPONENT_REF)
    return m2expr_GetSizeOfInBits (TREE_TYPE (type));

  if (code == ERROR_MARK)
    return size_one_node;

  if (!COMPLETE_TYPE_P (type))
    {
      error ("sizeof applied to an incomplete type");
      return size_zero_node;
    }

  return m2decl_BuildIntegerConstant (TYPE_PRECISION (type));
}


/*
 *  m2expr_GetSizeOf - taken from c-typeck.c (c_sizeof).
 */

tree
m2expr_GetSizeOf (location_t location, tree type)
{
  enum tree_code code = TREE_CODE (type);
  m2assert_AssertLocation (location);

  if (code == FUNCTION_TYPE)
    return m2expr_GetSizeOf (location, m2type_GetPointerType());

  if (code == VOID_TYPE)
    return size_one_node;

  if (code == VAR_DECL)
    return m2expr_GetSizeOf (location, TREE_TYPE (type));

  if (code == PARM_DECL)
    return m2expr_GetSizeOf (location, TREE_TYPE (type));

  if (code == TYPE_DECL)
    return m2expr_GetSizeOf (location, TREE_TYPE (type));

  if (code == ERROR_MARK)
    return size_one_node;

  if (code == CONSTRUCTOR)
    return m2expr_GetSizeOf (location, TREE_TYPE (type));

  if (code == FIELD_DECL)
    return m2expr_GetSizeOf (location, TREE_TYPE (type));

  if (code == COMPONENT_REF)
    return m2expr_GetSizeOf (location, TREE_TYPE (type));

  if (!COMPLETE_TYPE_P (type))
    {
      error_at (location, "sizeof applied to an incomplete type");
      return size_zero_node;
    }

  /* Convert in case a char is more than one unit.  */
  return size_binop_loc (location,
			 CEIL_DIV_EXPR, TYPE_SIZE_UNIT (type),
			 size_int (TYPE_PRECISION (char_type_node)
				   / BITS_PER_UNIT));
}

tree
m2expr_GetIntegerZero (location_t location ATTRIBUTE_UNUSED)
{
  return integer_zero_node;
}

tree
m2expr_GetIntegerOne (location_t location ATTRIBUTE_UNUSED)
{
  return integer_one_node;
}

tree
m2expr_GetCardinalOne (location_t location)
{
  return m2convert_ToCardinal (location, integer_one_node);
}

tree
m2expr_GetCardinalZero (location_t location)
{
  return m2convert_ToCardinal (location, integer_zero_node);
}

tree
m2expr_GetWordZero (location_t location)
{
  return m2convert_ToWord (location, integer_zero_node);
}

tree
m2expr_GetWordOne (location_t location)
{
  return m2convert_ToWord (location, integer_one_node);
}

tree
m2expr_GetPointerZero (location_t location)
{
  return m2convert_convertToPtr (location, integer_zero_node);
}

tree
m2expr_GetPointerOne (location_t location)
{
  return m2convert_convertToPtr (location, integer_one_node);
}


/*
 *  build_set_full_complement - returns a word size value with all bits set to one.
 */

static tree
build_set_full_complement (location_t location)
{
  tree value = integer_zero_node;
  int i;

  m2assert_AssertLocation (location);

  for (i=0; i<SET_WORD_SIZE; i++) {
    value = m2expr_BuildLogicalOr(location, value,
                                  m2expr_BuildLSL (location,
						   m2expr_GetWordOne(location),
                                                   m2convert_BuildConvert (location, m2type_GetWordType (), m2decl_BuildIntegerConstant (i), FALSE),
                                                   FALSE),
                                  FALSE);
  }
  return value;
}


/*
 *  init - initialise this module.
 */

void
m2expr_init (location_t location)
{
  m2assert_AssertLocation (location);

  set_full_complement = build_set_full_complement (location);
}


#include "gt-gm2-m2expr.h"
