/* Copyright (C) 2012
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
#include "../gm2-tree.h"
#include "../gm2-lang.h"

#define m2decl_c
#include "m2decl.h"
#include "m2assert.h"
#include "m2expr.h"
#include "m2type.h"
#include "m2tree.h"
#include "m2block.h"


/*
 *
 * prototypes
 *
 */

extern GTY(()) tree current_function_decl;


/* Used in BuildStartFunctionType */
static GTY(()) tree param_type_list;
static GTY(()) tree param_list = NULL_TREE;   /* ready for the next time we call/define a function */


/*
 *  DeclareKnownVariable - declares a variable in scope,
 *                         funcscope. Note that the global variable,
 *                         current_function_decl, is altered if
 *                         isglobal is TRUE.
 */

tree
m2decl_DeclareKnownVariable (location_t location, char *name, tree type, int exported,
                             int imported, int istemporary,
                             int isglobal, tree scope)
{
  tree id;
  tree decl;

  m2assert_AssertLocation (location);
  ASSERT (m2tree_is_type(type), type);
  ASSERT_BOOL (isglobal);

#if 0
  if (name && (strcmp (name, "_T25") == 0))
    stop ();
#endif

  id   = get_identifier (name);
  type = m2tree_skip_type_decl (type);
  decl = build_decl (location, VAR_DECL, id, type);

  DECL_SOURCE_LOCATION (decl) = location;

  DECL_EXTERNAL (decl) = imported;
  if (isglobal && (scope == NULL_TREE)) {
    TREE_PUBLIC (decl) = exported;
    TREE_STATIC (decl) = isglobal;           /* declaration and definition */
  } 
  else if (imported) {
    TREE_STATIC (decl) = 0;
    TREE_PUBLIC (decl) = 1;
  }
  else {
    TREE_STATIC (decl) = 0;
    TREE_PUBLIC (decl) = 0;
  }

  /* The variable was not declared by GCC, but by the front end  */
  DECL_ARTIFICIAL (decl) = 0;
  DECL_IGNORED_P (decl) = istemporary;

  DECL_CONTEXT (decl) = scope;

  /* now for the id */

  if (imported)
    TREE_STATIC (id) = 0;           /* declaration and definition */
  else
    TREE_STATIC (id) = 1;           /* declaration and definition */

  TREE_PUBLIC (id) = TREE_PUBLIC (decl);

#if 0
  if (name && ((strcmp (name, "_T25") == 0))) {
    debug_tree (decl);
    stop ();
  }
#endif

  m2block_pushDecl (decl);

  if (DECL_SIZE (decl) == 0)
    error ("storage size of %q+D' hasn't been resolved", decl);

  if ((TREE_PUBLIC (decl) == 0) && DECL_EXTERNAL (decl))
    internal_error ("inconsistant because PUBLIC_DECL(decl) == 0 && DECL_EXTERNAL(decl) == 1");

  if (! isglobal)
    m2block_addDeclExpr (build_stmt (location, DECL_EXPR, decl));

  return decl;
}


/*
 *  DeclareKnownConstant - given a constant, value, of, type, create a constant in the GCC
 *                         symbol table. Note that the name of the constant is not used
 *                         as _all_ constants are declared in the global scope. The front end
 *                         deals with scoping rules - here we declare all constants with no names
 *                         in the global scope. This allows M2SubExp and constant folding routines
 *                         the liberty of operating with quadruples which all assume constants can
 *                         always be referenced.
 */

tree
m2decl_DeclareKnownConstant (location_t location, tree type, tree value)
{
  tree id = make_node (IDENTIFIER_NODE);  /* ignore the name of the constant */
  tree decl;

  m2assert_AssertLocation (location);
  m2expr_ConstantExpressionWarning (value);
  type = m2tree_skip_type_decl (type);
  layout_type (type);
  
  decl = build_decl (location, CONST_DECL, id, type);
    
  DECL_INITIAL (decl) = value;
  TREE_TYPE (decl)    = type;

  decl = m2block_global_constant (decl);

  return decl;
}


/*
 *  BuildParameterDeclaration - creates and returns one parameter from, name, and, type.
 *                              It appends this parameter to the internal param_type_list.
 *                              If name is nul then we assume we are creating a function
 *                              type declaration and we ignore names.
 */

tree
m2decl_BuildParameterDeclaration (location_t location, char *name, tree type,
                                  int isreference)
{
  tree parm_decl;

  m2assert_AssertLocation (location);
  ASSERT_BOOL (isreference);
  type = m2tree_skip_type_decl(type);
  layout_type (type);
  if (isreference)
    type = build_reference_type (type);

  if ((name != NULL) && (strcmp(name, "") != 0)) {
    /* creating a function with parameters */
    parm_decl = build_decl (location, PARM_DECL, get_identifier (name), type);
    DECL_ARG_TYPE (parm_decl) = type;
    param_list = chainon (parm_decl, param_list);
    layout_type (type);
#if 0
    printf("making parameter %s known to gcc\n", name);
#endif
    param_type_list = tree_cons (NULL_TREE, type, param_type_list);
    return parm_decl;
  } else {
    param_type_list = tree_cons (NULL_TREE, type, param_type_list);
    return type;
  }
}


/*
 *  BuildStartFunctionDeclaration - initializes global variables ready
 *                                  for building a function.
 */

void
m2decl_BuildStartFunctionDeclaration (int uses_varargs)
{
  if (uses_varargs)
    param_type_list = NULL_TREE;
  else
    param_type_list = tree_cons (NULL_TREE, void_type_node, NULL_TREE);
  param_list = NULL_TREE;   /* ready for when we define a function */
}


/*
 *  BuildEndFunctionDeclaration - build a function which will return a value of returntype.
 *                                The arguments have been created by BuildParameterDeclaration.
 */

tree
m2decl_BuildEndFunctionDeclaration (location_t location, const char *name, tree returntype,
                                    int isexternal, int isnested, int ispublic)
{
  tree fntype;
  tree fndecl;

  m2assert_AssertLocation (location);
  ASSERT_BOOL (isexternal);
  ASSERT_BOOL (isnested);
  ASSERT_BOOL (ispublic);
  returntype = m2tree_skip_type_decl (returntype);
  /*
   *  the function type depends on the return type and type of args,
   *  both of which we have created in BuildParameterDeclaration
   */
  if (returntype == NULL_TREE)
    returntype = void_type_node;
  else if (TREE_CODE (returntype) == FUNCTION_TYPE)
    returntype = ptr_type_node;

  fntype = build_function_type (returntype, param_type_list);
  fndecl = build_decl (location, FUNCTION_DECL, get_identifier (name), fntype);

  if (isexternal)
    ASSERT_CONDITION (ispublic);

  DECL_EXTERNAL (fndecl) = isexternal;
  TREE_PUBLIC (fndecl) = ispublic;
  TREE_STATIC (fndecl) = (! isexternal);
  DECL_ARGUMENTS (fndecl) = param_list;
  DECL_RESULT (fndecl) = build_decl (location, RESULT_DECL, NULL_TREE, returntype);
  DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;
  TREE_TYPE (fndecl) = fntype;

  DECL_SOURCE_LOCATION (fndecl) = location;

  /* Prevent the optimizer from removing it if it is public. */
  if (TREE_PUBLIC (fndecl))
    gm2_mark_addressable (fndecl);

  m2block_pushDecl (fndecl);

  rest_of_decl_compilation (fndecl, 1, 0);
  param_list = NULL_TREE;   /* ready for the next time we call/define a function */
  return fndecl;
}

#if 0
/* not needed? */
/*
 *  RememberVariables - 
 */

static void
m2decl_RememberVariables (tree l)
{
  tree t;

  for (t = l; t; t = TREE_CHAIN (t))
    if (TREE_CODE (t) == VAR_DECL)
      add_stmt (build_stmt (DECL_EXPR, t));
}
#endif


/*
 *  DetermineSizeOfConstant - given, str, and, base, fill in
 *                            needsLong and needsUnsigned appropriately.
 */

void
m2decl_DetermineSizeOfConstant (const char *str, unsigned int base,
                                int *needsLong, int *needsUnsigned)
{
  int low;
  int high;
  int overflow;

  overflow = m2expr_interpret_m2_integer (str, base, (unsigned int *)&low, &high);
  *needsLong = (high != 0);
  if (*needsLong)
    *needsUnsigned = (high < 0);
  else
    *needsUnsigned = (low < 0);
}


/*
 *  BuildConstLiteralNumber - returns a GCC TREE built from the string, str.
 *                            It assumes that, str, represents a legal
 *                            number in Modula-2. It always returns a
 *                            positive value.
 */

tree
m2decl_BuildConstLiteralNumber (const char *str, unsigned int base)
{
  tree value, type;
  unsigned HOST_WIDE_INT low;
  HOST_WIDE_INT high;
  int needLong, needUnsigned;
  int overflow;

  overflow = m2expr_interpret_integer (str, base,
				       &low, (HOST_WIDE_INT *) &high);
  m2decl_DetermineSizeOfConstant (str, base, &needLong, &needUnsigned);
  
  if (needUnsigned && needLong)
    type = m2type_GetM2LongCardType ();
  else
    type = m2type_GetM2LongIntType ();

  value = build_int_cst_wide (type, low, high);

  if (m2expr_TreeOverflow (value))
    error("constant too large");

  return m2block_RememberConstant (value);
}


/*
 *  BuildStringConstant - creates a string constant given a, string,
 *                        and, length.
 */

tree
m2decl_BuildStringConstant (const char *string, int length)
{
  tree elem, index, type;

  /* +1 ensures that we always nul terminate our strings */
  elem = build_type_variant (char_type_node, 1, 0);
  index = build_index_type (build_int_cst (integer_type_node, length+1));
  type = build_array_type (elem, index);
  return m2decl_BuildStringConstantType (length+1, string, type);
}


/*
 *  BuildIntegerConstant - return a tree containing the integer value.
 */

tree
m2decl_BuildIntegerConstant (int value)
{
  switch (value) {

  case 0:  return integer_zero_node;
  case 1:  return integer_one_node;

  default:
    return m2block_RememberConstant (build_int_cst (integer_type_node, value));
  }
}


/*
 *  BuildStringConstantType - builds a string constant with a type.
 */

tree
m2decl_BuildStringConstantType (int length, const char *string, tree type)
{
  tree id = build_string (length, string);

  TREE_TYPE (id) = type;
  TREE_CONSTANT (id) = TRUE;
  TREE_READONLY (id) = TRUE;
  TREE_STATIC (id) = TRUE;

  return m2block_RememberConstant (id);
}


/*
 *  GetBitsPerWord - returns the number of bits in a WORD.
 */

int
m2decl_GetBitsPerWord (void)
{
  return BITS_PER_WORD;
}


/*
 *  GetBitsPerInt - returns the number of bits in a INTEGER.
 */

int
m2decl_GetBitsPerInt (void)
{
  return INT_TYPE_SIZE;
}


/*
 *  GetBitsPerBitset - returns the number of bits in a BITSET.
 */

int
m2decl_GetBitsPerBitset (void)
{
  return SET_WORD_SIZE;
}


/*
 *  GetBitsPerUnit - returns the number of bits in a UNIT.
 */

int
m2decl_GetBitsPerUnit (void)
{
  return BITS_PER_UNIT;
}


/*
 *   m2decl_GetDeclContext - returns the DECL_CONTEXT of tree, t.
 */

tree
m2decl_GetDeclContext (tree t)
{
  return DECL_CONTEXT (t);
}


#include "gt-gm2-m2decl.h"