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
#include "function.h"
#include "../gm2-tree.h"
#include "../gm2-lang.h"

/*
 *
 * prototypes
 *
 */
#define m2convert_c
#include "m2expr.h"
#include "m2statement.h"
#include "m2decl.h"
#include "m2type.h"
#include "m2assert.h"
#include "m2tree.h"
#include "m2treelib.h"
#include "m2block.h"
#include "m2expr.h"
#include "m2convert.h"

static tree const_to_ISO_type (location_t location, tree expr, tree iso_type);
static tree const_to_ISO_aggregate_type (location_t location, tree expr, tree iso_type);


/*
 *  ConvertString - converts string, expr, into a string
 *                  of type, type.
 */

tree
m2convert_ConvertString (tree type, tree expr)
{
  const char *str = TREE_STRING_POINTER (expr);
  int len = TREE_STRING_LENGTH (expr);
  return m2decl_BuildStringConstantType (len, str, type);
}


/*
 *  (taken from c-common.c and trimmed for Modula-2)
 *
 *  Checks if expression EXPR of real/integer type cannot be converted 
 *  to the real/integer type TYPE. Function returns true when:
 *      * EXPR is a constant which cannot be exactly converted to TYPE 
 *      * EXPR is not a constant and size of EXPR's type > than size of TYPE, 
 *        for EXPR type and TYPE being both integers or both real.
 *      * EXPR is not a constant of real type and TYPE is an integer.  
 *      * EXPR is not a constant of integer type which cannot be 
 *        exactly converted to real type.  
 *  Function allows conversions between types of different signedness and
 *  does not return true in that case.  Function can produce signedness
 *  warnings if PRODUCE_WARNS is true.
 */

static
bool
unsafe_conversion_p (tree type, tree expr, bool produce_warns)
{
  bool give_warning = false;
  tree expr_type = TREE_TYPE (expr);
  location_t loc = EXPR_LOC_OR_HERE (expr);

  if (TREE_CODE (expr) == REAL_CST || TREE_CODE (expr) == INTEGER_CST)
    {
      /* Warn for real constant that is not an exact integer converted
	 to integer type.  */
      if (TREE_CODE (expr_type) == REAL_TYPE
	  && TREE_CODE (type) == INTEGER_TYPE)
	{
	  if (!real_isinteger (TREE_REAL_CST_PTR (expr), TYPE_MODE (expr_type)))
	    give_warning = true;
	}
      /* Warn for an integer constant that does not fit into integer type.  */
      else if (TREE_CODE (expr_type) == INTEGER_TYPE
	       && TREE_CODE (type) == INTEGER_TYPE
	       && !int_fits_type_p (expr, type))
	{
	  if (TYPE_UNSIGNED (type) && !TYPE_UNSIGNED (expr_type)
	      && tree_int_cst_sgn (expr) < 0)
	    {
	      if (produce_warns)
		warning_at (loc, OPT_Wsign_conversion, "negative INTEGER"
			    " implicitly converted to a CARDINAL type");
	    }
	  else if (!TYPE_UNSIGNED (type) && TYPE_UNSIGNED (expr_type))
	    {
	      if (produce_warns)
		warning_at (loc, OPT_Wsign_conversion, "conversion of CARDINAL"
			    " constant value to negative INTEGER");
	    }
	  else
	    give_warning = true;
	}
      else if (TREE_CODE (type) == REAL_TYPE)
	{
	  /* Warn for an integer constant that does not fit into real type.  */
	  if (TREE_CODE (expr_type) == INTEGER_TYPE)
	    {
	      REAL_VALUE_TYPE a = real_value_from_int_cst (0, expr);
	      if (!exact_real_truncate (TYPE_MODE (type), &a))
		give_warning = true;
	    }
	  /* Warn for a real constant that does not fit into a smaller
	     real type.  */
	  else if (TREE_CODE (expr_type) == REAL_TYPE
		   && TYPE_PRECISION (type) < TYPE_PRECISION (expr_type))
	    {
	      REAL_VALUE_TYPE a = TREE_REAL_CST (expr);
	      if (!exact_real_truncate (TYPE_MODE (type), &a))
		give_warning = true;
	    }
	}
    }
  else
    {
      /* Warn for real types converted to integer types.  */
      if (TREE_CODE (expr_type) == REAL_TYPE
	  && TREE_CODE (type) == INTEGER_TYPE)
	give_warning = true;
#if 0
      else if (TREE_CODE (expr_type) == INTEGER_TYPE
	       && TREE_CODE (type) == INTEGER_TYPE)
	{
	  /* Don't warn about unsigned char y = 0xff, x = (int) y;  */
	  expr = get_unwidened (expr, 0);
	  expr_type = TREE_TYPE (expr);

	  /* Don't warn for short y; short x = ((int)y & 0xff);  */
	  if (TREE_CODE (expr) == BIT_AND_EXPR
	      || TREE_CODE (expr) == BIT_IOR_EXPR
	      || TREE_CODE (expr) == BIT_XOR_EXPR)
	    {
	      /* If both args were extended from a shortest type,
		 use that type if that is safe.  */
	      expr_type = shorten_binary_op (expr_type,
					     TREE_OPERAND (expr, 0),
					     TREE_OPERAND (expr, 1),
					     /* bitwise */1);

	      if (TREE_CODE (expr) == BIT_AND_EXPR)
		{
		  tree op0 = TREE_OPERAND (expr, 0);
		  tree op1 = TREE_OPERAND (expr, 1);
		  bool unsigned0 = TYPE_UNSIGNED (TREE_TYPE (op0));
		  bool unsigned1 = TYPE_UNSIGNED (TREE_TYPE (op1));

		  /* If one of the operands is a non-negative constant
		     that fits in the target type, then the type of the
		     other operand does not matter. */
		  if ((TREE_CODE (op0) == INTEGER_CST
		       && int_fits_type_p (op0, gm2_signed_type (type))
		       && int_fits_type_p (op0, gm2_unsigned_type (type)))
		      || (TREE_CODE (op1) == INTEGER_CST
			  && int_fits_type_p (op1, gm2_signed_type (type))
			  && int_fits_type_p (op1,
					      gm2_unsigned_type (type))))
		    return false;
		  /* If constant is unsigned and fits in the target
		     type, then the result will also fit.  */
		  else if ((TREE_CODE (op0) == INTEGER_CST
			    && unsigned0
			    && int_fits_type_p (op0, type))
			   || (TREE_CODE (op1) == INTEGER_CST
			       && unsigned1
			       && int_fits_type_p (op1, type)))
		    return false;
		}
	    }
	  /* Warn for integer types converted to smaller integer types.  */
	  if (TYPE_PRECISION (type) < TYPE_PRECISION (expr_type))
	    give_warning = true;

	  /* When they are the same width but different signedness,
	     then the value may change.  */
	  else if (((TYPE_PRECISION (type) == TYPE_PRECISION (expr_type)
		    && TYPE_UNSIGNED (expr_type) != TYPE_UNSIGNED (type))
		   /* Even when converted to a bigger type, if the type is
		      unsigned but expr is signed, then negative values
		      will be changed.  */
		    || (TYPE_UNSIGNED (type) && !TYPE_UNSIGNED (expr_type)))
		   && produce_warns)
	    warning_at (loc, OPT_Wsign_conversion, "conversion to %qT from %qT "
			"may change the sign of the result",
			type, expr_type);
	}

      /* Warn for integer types converted to real types if and only if
	 all the range of values of the integer type cannot be
	 represented by the real type.  */
      else if (TREE_CODE (expr_type) == INTEGER_TYPE
	       && TREE_CODE (type) == REAL_TYPE)
	{
	  tree type_low_bound, type_high_bound;
	  REAL_VALUE_TYPE real_low_bound, real_high_bound;

	  /* Don't warn about char y = 0xff; float x = (int) y;  */
	  expr = get_unwidened (expr, 0);
	  expr_type = TREE_TYPE (expr);

	  type_low_bound = TYPE_MIN_VALUE (expr_type);
	  type_high_bound = TYPE_MAX_VALUE (expr_type);
	  real_low_bound = real_value_from_int_cst (0, type_low_bound);
	  real_high_bound = real_value_from_int_cst (0, type_high_bound);

	  if (!exact_real_truncate (TYPE_MODE (type), &real_low_bound)
	      || !exact_real_truncate (TYPE_MODE (type), &real_high_bound))
	    give_warning = true;
	}

      /* Warn for real types converted to smaller real types.  */
      else if (TREE_CODE (expr_type) == REAL_TYPE
	       && TREE_CODE (type) == REAL_TYPE
	       && TYPE_PRECISION (type) < TYPE_PRECISION (expr_type))
	give_warning = true;
#endif
    }

  return give_warning;
}


/*
 *  (taken from c-common.c and trimmed for Modula-2)
 *
 *  Warns if the conversion of EXPR to TYPE may alter a value.
 *  This is a helper function for warnings_for_convert_and_check.
 */

static void
conversion_warning (tree type, tree expr)
{
  tree expr_type = TREE_TYPE (expr);
  location_t loc = EXPR_LOC_OR_HERE (expr);

  if (!warn_conversion && !warn_sign_conversion)
    return;

  switch (TREE_CODE (expr))
    {
    case EQ_EXPR:
    case NE_EXPR:
    case LE_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case TRUTH_NOT_EXPR:
      /* Conversion from boolean to a signed:1 bit-field (which only
	 can hold the values 0 and -1) doesn't lose information - but
	 it does change the value.  */
      if (TYPE_PRECISION (type) == 1 && !TYPE_UNSIGNED (type))
	warning_at (loc, OPT_Wconversion,
		    "conversion to %qT from BOOLEAN expression", type);
      return;

    case REAL_CST:
    case INTEGER_CST:
      if (unsafe_conversion_p (type, expr, true))
	warning_at (loc, OPT_Wconversion,
		    "conversion to %qT alters %qT constant value",
		    type, expr_type);
      return;

    case COND_EXPR:
      {
	/* In case of COND_EXPR, if both operands are constants or
	   COND_EXPR, then we do not care about the type of COND_EXPR,
	   only about the conversion of each operand.  */
	tree op1 = TREE_OPERAND (expr, 1);
	tree op2 = TREE_OPERAND (expr, 2);

	if ((TREE_CODE (op1) == REAL_CST || TREE_CODE (op1) == INTEGER_CST
	     || TREE_CODE (op1) == COND_EXPR)
	    && (TREE_CODE (op2) == REAL_CST || TREE_CODE (op2) == INTEGER_CST
		|| TREE_CODE (op2) == COND_EXPR))
	  {
	    conversion_warning (type, op1);
	    conversion_warning (type, op2);
	    return;
	  }
	/* Fall through.  */
      }

    default: /* 'expr' is not a constant.  */
      if (unsafe_conversion_p (type, expr, true))
	warning_at (loc, OPT_Wconversion,
		    "conversion to %qT from %qT may alter its value",
		    type, expr_type);
    }
}


/*
 *  (taken from c-common.c and trimmed for Modula-2)
 *
 *  Produce warnings after a conversion. RESULT is the result of
 *  converting EXPR to TYPE.  This is a helper function for
 *  convert_and_check and cp_convert_and_check.
 */

static void
warnings_for_convert_and_check (tree type, tree expr, tree result)
{
  if (TREE_CODE (expr) == INTEGER_CST
      && (TREE_CODE (type) == INTEGER_TYPE
          || TREE_CODE (type) == ENUMERAL_TYPE)
      && !int_fits_type_p (expr, type))
    {
      /* Do not diagnose overflow in a constant expression merely
         because a conversion overflowed.  */
      if (TREE_OVERFLOW (result))
        TREE_OVERFLOW (result) = TREE_OVERFLOW (expr);

      if (TYPE_UNSIGNED (type))
        {
          /* This detects cases like converting -129 or 256 to
             unsigned char.  */
          if (!int_fits_type_p (expr, m2type_gm2_signed_type (type)))
            warning (OPT_Woverflow,
                     "large INTEGER based constant implicitly truncated to a fit into a smaller CARDINAL based type");
          else
            conversion_warning (type, expr);
        }
      else if (!int_fits_type_p (expr, m2type_gm2_unsigned_type (type)))
	warning (OPT_Woverflow,
		 "overflow in implicit constant conversion");
      /* No warning for converting 0x80000000 to int.  */
      else if ((TREE_CODE (TREE_TYPE (expr)) != INTEGER_TYPE
		|| TYPE_PRECISION (TREE_TYPE (expr))
		!= TYPE_PRECISION (type)))
	warning (OPT_Woverflow,
		 "overflow in implicit constant conversion");

      else
	conversion_warning (type, expr);
    }
  else if ((TREE_CODE (result) == INTEGER_CST
	    || TREE_CODE (result) == FIXED_CST) && TREE_OVERFLOW (result))
    warning (OPT_Woverflow,
             "overflow in implicit constant conversion");
  else
    conversion_warning (type, expr);
}


/*
 *  (taken from c-common.c and trimmed for Modula-2)
 *
 *  Convert EXPR to TYPE, warning about conversion problems with constants.
 *  Invoke this function on every expression that is converted implicitly,
 *  i.e. because of language rules and not because of an explicit cast.
 */

static
tree
convert_and_check (tree type, tree expr)
{
  tree result;
  tree expr_for_warning;

  /* Convert from a value with possible excess precision rather than
     via the semantic type, but do not warn about values not fitting
     exactly in the semantic type.  */
  if (TREE_CODE (expr) == EXCESS_PRECISION_EXPR)
    {
      tree orig_type = TREE_TYPE (expr);
      expr = TREE_OPERAND (expr, 0);
      expr_for_warning = convert (orig_type, expr);
      if (orig_type == type)
	return expr_for_warning;
    }
  else
    expr_for_warning = expr;

  if (TREE_TYPE (expr) == type)
    return expr;

  result = convert (type, expr);

  if (!TREE_OVERFLOW_P (expr)
      && result != error_mark_node)
    warnings_for_convert_and_check (type, expr_for_warning, result);

  return result;
}


static tree
doOrdinal (tree value)
{
  if (TREE_CODE (value) == STRING_CST
      && (m2expr_StringLength (value) <= 1))
    {
      const char *p = TREE_STRING_POINTER (value);
      int i = p[0];

      return m2decl_BuildIntegerConstant (i);
    }
  return value;
}


static int
same_size_types (location_t location, tree t1, tree t2)
{
  tree n1 = m2expr_GetSizeOf (location, t1);
  tree n2 = m2expr_GetSizeOf (location, t2);

  return m2expr_CompareTrees (n1, n2) == 0;
}


static int
converting_ISO_generic (location_t location, tree type, tree value, tree generic_type, tree *result)
{
  tree value_type = m2tree_skip_type_decl (TREE_TYPE (value));

  if (value_type == type)
    /*
     *  we let the caller deal with this
     */
    return FALSE;

  if ((TREE_CODE (value) == INTEGER_CST)
      && (type == generic_type))
    {
      *result = const_to_ISO_type (location, value, generic_type);
      return TRUE;
    }

  if (same_size_types (location, type, value_type))
    {
      if (value_type == generic_type)
	{
	  tree pt = build_pointer_type (type);
	  tree a = build1 (ADDR_EXPR, pt, value);
	  tree t = build1 (INDIRECT_REF, type, a);
	  *result = build1 (NOP_EXPR, type, t);
	  return TRUE;
	}
	else if (type == generic_type)
	  {
	    tree pt = build_pointer_type (type);
	    tree a = build1 (ADDR_EXPR, pt, value);
	    tree t = build1 (INDIRECT_REF, type, a);
	    *result = build1 (NOP_EXPR, type, t);
	    return TRUE;
	  }
    }
  return FALSE;
}


/*
 *  BuildConvert - build and return tree VAL (type, value).
 *                 checkOverflow determines whether we
 *                 should suppress overflow checking.
 */

tree
m2convert_BuildConvert (location_t location, tree type, tree value, int checkOverflow)
{
  type = m2tree_skip_type_decl (type);
  tree t;

  value = fold (value);
  STRIP_NOPS (value);
  value = m2expr_FoldAndStrip (value);

  if (TREE_CODE (value) == STRING_CST
      && (m2expr_StringLength (value) <= 1)
      && (m2tree_IsOrdinal (type)))
    value = doOrdinal (value);
  else if (TREE_CODE (value) == FUNCTION_DECL && TREE_TYPE (value) != type)
    value = m2expr_BuildAddr (0, value, FALSE);

  if (converting_ISO_generic (location, type, value, m2type_GetByteType (), &t) ||
      converting_ISO_generic (location, type, value, m2type_GetISOLocType (), &t) ||
      converting_ISO_generic (location, type, value, m2type_GetISOByteType (), &t) ||
      converting_ISO_generic (location, type, value, m2type_GetISOWordType (), &t) ||
      converting_ISO_generic (location, type, value, m2type_GetM2Word16 (), &t) ||
      converting_ISO_generic (location, type, value, m2type_GetM2Word32 (), &t) ||
      converting_ISO_generic (location, type, value, m2type_GetM2Word64 (), &t))
    return t;

  if (checkOverflow)
    return convert_and_check (type, value);
  else
    return convert (type, value);
}


/*
 *  const_to_ISO_type - perform VAL (iso_type, expr).
 */

static tree
const_to_ISO_type (location_t location, tree expr, tree iso_type)
{
  tree n = m2expr_GetSizeOf (location, iso_type);

  if ((m2expr_CompareTrees (n, m2decl_BuildIntegerConstant (1)) == 0)
      && (iso_type == m2type_GetByteType ()
	  || iso_type == m2type_GetISOLocType ()
	  || iso_type == m2type_GetISOByteType ()))
    return build1 (NOP_EXPR, iso_type, expr);
  return const_to_ISO_aggregate_type (location, expr, iso_type);
}


/*
 *  const_to_ISO_aggregate_type - perform VAL (iso_type, expr).
 *                                The iso_type will be declared by the SYSTEM module
 *                                as:
 *                                TYPE iso_type = ARRAY [0..n] OF LOC
 *
 *                                this function will store a constant into the iso_type
 *                                in the correct endian order.  It converts the expr
 *                                into a unsigned int or signed int and then
 *                                strips it a byte at a time.
 */

static tree
const_to_ISO_aggregate_type (location_t location, tree expr, tree iso_type)
{
  tree byte;
  m2type_Constructor c;
  tree i = m2decl_BuildIntegerConstant (0);
  tree n = m2expr_GetSizeOf (location, iso_type);
  tree max_uint = m2decl_BuildIntegerConstant (256);

  while (m2expr_CompareTrees (i, n) < 0)
    {
      max_uint = m2expr_BuildMult (location, max_uint, m2decl_BuildIntegerConstant (256), FALSE);
      i = m2expr_BuildAdd (location, i, m2decl_BuildIntegerConstant (1), FALSE);
    }
  max_uint = m2expr_BuildDivFloor (location, max_uint, m2decl_BuildIntegerConstant (2), FALSE);

  if (m2expr_CompareTrees (expr, m2decl_BuildIntegerConstant (0)) < 0)
    expr = m2expr_BuildAdd (location, expr, max_uint, FALSE);
    
  i = m2decl_BuildIntegerConstant (0);
  c = m2type_BuildStartArrayConstructor (iso_type);
  while (m2expr_CompareTrees (i, n) < 0)
    {
      byte = m2expr_BuildModTrunc (location, expr, m2decl_BuildIntegerConstant (256), FALSE);
      if (BYTES_BIG_ENDIAN)
	m2type_BuildArrayConstructorElement (c,
					     m2convert_ToLoc (location, byte),
					     m2expr_BuildSub (location,
							      m2expr_BuildSub (location, n, i, FALSE),
							      m2decl_BuildIntegerConstant (1),
							      FALSE));
      else
	m2type_BuildArrayConstructorElement (c, m2convert_ToLoc (location, byte), i);

      i = m2expr_BuildAdd (location, i, m2decl_BuildIntegerConstant (1), FALSE);
      expr = m2expr_BuildDivFloor (location, expr, m2decl_BuildIntegerConstant (256), FALSE);
    }

  return m2type_BuildEndArrayConstructor (c);  
}


/*
 *  ConvertConstantAndCheck - in Modula-2 sementics: RETURN( VAL(type, expr) )
 *
 *                            Only to be used for a constant expr,
 *                            overflow checking is performed. 
 */

tree
m2convert_ConvertConstantAndCheck (location_t location, tree type, tree expr)
{
  tree etype;
  expr = fold (expr);
  STRIP_NOPS (expr);
  expr = m2expr_FoldAndStrip (expr);
  etype = TREE_TYPE (expr);

  m2assert_AssertLocation (location);
  if (etype == type)
    return expr;

  if (TREE_CODE (expr) == FUNCTION_DECL)
    expr = m2expr_BuildAddr (location, expr, FALSE);

  type = m2tree_skip_type_decl (type);
  if (type == m2type_GetByteType ()
      || type == m2type_GetISOLocType ()
      || type == m2type_GetISOByteType ()
      || type == m2type_GetISOWordType ()
      || type == m2type_GetM2Word16 ()
      || type == m2type_GetM2Word32 ()
      || type == m2type_GetM2Word64 ())
    return const_to_ISO_type (location, expr, type);
  
  return convert_and_check (type, m2expr_FoldAndStrip (expr));
}


/*
 *  ToWord - converts an expression (Integer or Ordinal type) into
 *           a WORD.
 */

tree
m2convert_ToWord (location_t location, tree expr)
{
  return m2convert_BuildConvert (location, m2type_GetWordType(), expr, FALSE);
}


/*
 *  ToCardinal - convert an expression, expr, to a CARDINAL.
 */

tree
m2convert_ToCardinal (location_t location, tree expr)
{
  return m2convert_BuildConvert (location, m2type_GetCardinalType (), expr, FALSE);
}


/*
 *  convertToPtr - if the type of tree, t, is not a ptr_type_node then convert it.
 */

tree
m2convert_convertToPtr (location_t location, tree type)
{
  if (TREE_CODE (TREE_TYPE (type)) == POINTER_TYPE)
    return type;
  else
    return m2convert_BuildConvert (location, m2type_GetPointerType (), type, FALSE);
}


/*
 *  ToInteger - convert an expression, expr, to an INTEGER.
 */

tree
m2convert_ToInteger (location_t location, tree expr)
{
  return m2convert_BuildConvert (location, m2type_GetIntegerType (), expr, FALSE);
}


/*
 *  ToBitset - convert an expression, expr, to a BITSET type.
 */

tree
m2convert_ToBitset (location_t location, tree expr)
{
  return m2convert_BuildConvert (location, m2type_GetBitsetType (), expr, FALSE);
}


/*
 *  ToLoc - convert an expression, expr, to a LOC.
 */

tree
m2convert_ToLoc (location_t location, tree expr)
{
  return m2convert_BuildConvert (location, m2type_GetISOByteType (), expr, FALSE);
}


/*
 *  GenericToType - converts, expr, into, type, providing that expr is
 *                  a generic system type (byte, word etc).  Otherwise
 *                  expr is returned unaltered.
 */

tree
m2convert_GenericToType (location_t location, tree type, tree expr)
{
  tree etype = TREE_TYPE (expr);

  type = m2tree_skip_type_decl (type);
  if (type == etype)
    return expr;

  if (type == m2type_GetISOWordType ()
      || type == m2type_GetM2Word16 ()
      || type == m2type_GetM2Word32 ()
      || type == m2type_GetM2Word64 ())
    return const_to_ISO_type (location, expr, type);

  return expr;
}
