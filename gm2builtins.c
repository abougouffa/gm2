/* Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008
 * Free Software Foundation, Inc.  */
/* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "input.h"
#include "tm.h"
#include "tree.h"
#include "toplev.h"
#include "tm_p.h"
#include "flags.h"
#include "tree-inline.h"

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
#include "langhooks.h"

extern tree builtin_function (const char *, tree, int, enum built_in_class,
			      const char *, tree);
extern tree pushdecl (tree);


#define GM2
#define GM2_BUG_REPORT "Please report this crash to the GNU Modula-2 mailing list <gm2@glam.ac.uk>\n"

#define ASSERT(X,Y)   { if (!(X)) { debug_tree(Y); internal_error("[%s:%d]:condition `%s' failed", \
                                                                   __FILE__, __LINE__, #X); } }
#define ERROR(X)      { internal_error("[%s:%d]:%s",               __FILE__, __LINE__, X); }


typedef enum {
  BT_FN_PTR_SIZE,
  BT_FN_TRAD_PTR_PTR_CONST_PTR_SIZE,
  BT_FN_FLOAT,
  BT_FN_DOUBLE,
  BT_FN_LONG_DOUBLE,
  BT_FN_FLOAT_FLOAT,
  BT_FN_DOUBLE_DOUBLE,
  BT_FN_LONG_DOUBLE_LONG_DOUBLE,
  BT_FN_STRING_CONST_STRING_INT,
  BT_FN_INT_CONST_PTR_CONST_PTR_SIZE,
  BT_FN_TRAD_PTR_PTR_INT_SIZE,
  BT_FN_STRING_STRING_CONST_STRING,
  BT_FN_STRING_STRING_CONST_STRING_SIZE,
  BT_FN_INT_CONST_STRING_CONST_STRING,
  BT_FN_INT_CONST_STRING_CONST_STRING_SIZE,
  BT_FN_INT_CONST_STRING,
  BT_FN_STRING_CONST_STRING_CONST_STRING,
  BT_FN_SIZE_CONST_STRING_CONST_STRING,
  BT_FN_PTR_UNSIGNED,
  BT_FN_VOID_PTR_INT,
  BT_FN_INT_PTR,
  BT_FN_INT_FLOAT,
  BT_FN_INT_DOUBLE,
  BT_FN_INT_LONG_DOUBLE,
  BT_FN_FLOAT_FCOMPLEX,
  BT_FN_DOUBLE_DCOMPLEX,
  BT_FN_LONG_DOUBLE_LDCOMPLEX,

  BT_FN_FCOMPLEX_FCOMPLEX,
  BT_FN_DCOMPLEX_DCOMPLEX,
  BT_FN_LDCOMPLEX_LDCOMPLEX,
} builtin_prototype;

struct builtin_function_entry {
  const char *name;
  builtin_prototype defn;
  int function_code;
  int class;
  const char *library_name;
  tree function_node;
  tree return_node;
};

/*
 *  entries are added by examining gcc/builtins.def and copying those
 *  functions which can be applied to Modula-2
 */

static struct builtin_function_entry list_of_builtins[] = {
{ "__builtin_alloca",  BT_FN_PTR_SIZE, BUILT_IN_ALLOCA, BUILT_IN_NORMAL, "alloca", NULL, NULL},
{ "__builtin_memcpy",  BT_FN_TRAD_PTR_PTR_CONST_PTR_SIZE, BUILT_IN_MEMCPY, BUILT_IN_NORMAL, "memcpy", NULL, NULL},
{ "__builtin_sinf",    BT_FN_FLOAT_FLOAT, BUILT_IN_SINF, BUILT_IN_NORMAL, "sinf", NULL, NULL},
{ "__builtin_sin",     BT_FN_DOUBLE_DOUBLE, BUILT_IN_SIN, BUILT_IN_NORMAL, "sin", NULL, NULL},
{ "__builtin_sinl",    BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_SINL, BUILT_IN_NORMAL, "sinl", NULL, NULL},
{ "__builtin_cosf",    BT_FN_FLOAT_FLOAT, BUILT_IN_SINF, BUILT_IN_NORMAL, "cosf", NULL, NULL},
{ "__builtin_cos",     BT_FN_DOUBLE_DOUBLE, BUILT_IN_COS, BUILT_IN_NORMAL, "cos", NULL, NULL},
{ "__builtin_cosl",    BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_COSL, BUILT_IN_NORMAL, "cosl", NULL, NULL},
{ "__builtin_sqrtf",   BT_FN_FLOAT_FLOAT, BUILT_IN_SQRTF, BUILT_IN_NORMAL, "sqrtf", NULL, NULL},
{ "__builtin_sqrt",    BT_FN_DOUBLE_DOUBLE, BUILT_IN_SQRT, BUILT_IN_NORMAL, "sqrt", NULL, NULL},
{ "__builtin_sqrtl",   BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_SQRTL, BUILT_IN_NORMAL, "sqrtl", NULL, NULL},
{ "__builtin_fabsf",   BT_FN_FLOAT_FLOAT, BUILT_IN_FABSF, BUILT_IN_NORMAL, "fabsf", NULL, NULL},
{ "__builtin_fabs",    BT_FN_DOUBLE_DOUBLE, BUILT_IN_FABS, BUILT_IN_NORMAL, "fabs", NULL, NULL},
{ "__builtin_fabsl",   BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_FABSL, BUILT_IN_NORMAL, "fabsl", NULL, NULL},
{ "__builtin_logf",    BT_FN_FLOAT_FLOAT, BUILT_IN_LOGF, BUILT_IN_NORMAL, "logf", NULL, NULL},
{ "__builtin_log",     BT_FN_DOUBLE_DOUBLE, BUILT_IN_LOG, BUILT_IN_NORMAL, "log", NULL, NULL},
{ "__builtin_logl",    BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_LOGL, BUILT_IN_NORMAL, "logl", NULL, NULL},
{ "__builtin_expf",    BT_FN_FLOAT_FLOAT, BUILT_IN_EXPF, BUILT_IN_NORMAL, "expf", NULL, NULL},
{ "__builtin_exp",     BT_FN_DOUBLE_DOUBLE, BUILT_IN_EXP, BUILT_IN_NORMAL, "exp", NULL, NULL},
{ "__builtin_expl",    BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_EXPL, BUILT_IN_NORMAL, "expl", NULL, NULL},
{ "__builtin_log10f",  BT_FN_FLOAT_FLOAT, BUILT_IN_LOG10F, BUILT_IN_NORMAL, "log10f", NULL, NULL},
{ "__builtin_log10",   BT_FN_DOUBLE_DOUBLE, BUILT_IN_LOG10, BUILT_IN_NORMAL, "log10", NULL, NULL},
{ "__builtin_log10l",  BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_LOG10L, BUILT_IN_NORMAL, "log10l", NULL, NULL},
{ "__builtin_ilogbf",  BT_FN_INT_FLOAT, BUILT_IN_ILOGBF, BUILT_IN_NORMAL, "ilogbf", NULL, NULL},
{ "__builtin_ilogb",   BT_FN_INT_DOUBLE, BUILT_IN_ILOGB, BUILT_IN_NORMAL, "ilogb", NULL, NULL},
{ "__builtin_ilogbl",  BT_FN_INT_LONG_DOUBLE, BUILT_IN_ILOGBL, BUILT_IN_NORMAL, "ilogbl", NULL, NULL},

/* complex intrinsic functions */
{ "__builtin_cabs", BT_FN_DOUBLE_DCOMPLEX, BUILT_IN_CABS, BUILT_IN_NORMAL, "cabs", NULL, NULL},
{ "__builtin_cabsf", BT_FN_FLOAT_FCOMPLEX, BUILT_IN_CABSF, BUILT_IN_NORMAL, "cabsf", NULL, NULL},
{ "__builtin_cabsl", BT_FN_LONG_DOUBLE_LDCOMPLEX, BUILT_IN_CABSL, BUILT_IN_NORMAL, "cabsl", NULL, NULL},

{ "__builtin_huge_val",   BT_FN_DOUBLE, BUILT_IN_HUGE_VAL, BUILT_IN_NORMAL, "hughe_val", NULL, NULL},
{ "__builtin_huge_valf",  BT_FN_FLOAT , BUILT_IN_HUGE_VALF, BUILT_IN_NORMAL, "hughe_valf", NULL, NULL},
{ "__builtin_huge_vall",  BT_FN_LONG_DOUBLE, BUILT_IN_HUGE_VALL, BUILT_IN_NORMAL, "hughe_vall", NULL, NULL},

{ "__builtin_index",   BT_FN_STRING_CONST_STRING_INT, BUILT_IN_INDEX, BUILT_IN_NORMAL, "index", NULL, NULL},
{ "__builtin_rindex",  BT_FN_STRING_CONST_STRING_INT, BUILT_IN_RINDEX, BUILT_IN_NORMAL, "rindex", NULL, NULL},
{ "__builtin_memcmp",  BT_FN_INT_CONST_PTR_CONST_PTR_SIZE, BUILT_IN_MEMCMP, BUILT_IN_NORMAL, "memcmp", NULL, NULL},
{ "__builtin_memmove", BT_FN_TRAD_PTR_PTR_CONST_PTR_SIZE, BUILT_IN_MEMMOVE, BUILT_IN_NORMAL, "memmove", NULL, NULL},
{ "__builtin_memset",  BT_FN_TRAD_PTR_PTR_INT_SIZE, BUILT_IN_MEMSET, BUILT_IN_NORMAL, "memset", NULL, NULL},
{ "__builtin_strcat",  BT_FN_STRING_STRING_CONST_STRING, BUILT_IN_STRCAT, BUILT_IN_NORMAL, "strcat", NULL, NULL},
{ "__builtin_strncat", BT_FN_STRING_STRING_CONST_STRING_SIZE, BUILT_IN_STRNCAT, BUILT_IN_NORMAL, "strncat", NULL, NULL},
{ "__builtin_strcpy",  BT_FN_STRING_STRING_CONST_STRING, BUILT_IN_STRCPY, BUILT_IN_NORMAL, "strcpy", NULL, NULL},
{ "__builtin_strncpy", BT_FN_STRING_STRING_CONST_STRING_SIZE, BUILT_IN_STRNCPY, BUILT_IN_NORMAL, "strncpy", NULL, NULL},
{ "__builtin_strcmp",  BT_FN_INT_CONST_STRING_CONST_STRING, BUILT_IN_STRCMP, BUILT_IN_NORMAL, "strcmp", NULL, NULL},
{ "__builtin_strncmp", BT_FN_INT_CONST_STRING_CONST_STRING_SIZE, BUILT_IN_STRNCMP, BUILT_IN_NORMAL, "strncmp", NULL, NULL},
{ "__builtin_strlen",  BT_FN_INT_CONST_STRING, BUILT_IN_STRLEN, BUILT_IN_NORMAL, "strlen", NULL, NULL},
{ "__builtin_strstr",  BT_FN_STRING_CONST_STRING_CONST_STRING, BUILT_IN_STRSTR, BUILT_IN_NORMAL, "strstr", NULL, NULL},
{ "__builtin_strpbrk", BT_FN_STRING_CONST_STRING_CONST_STRING, BUILT_IN_STRPBRK, BUILT_IN_NORMAL, "strpbrk", NULL, NULL},
{ "__builtin_strspn",  BT_FN_SIZE_CONST_STRING_CONST_STRING, BUILT_IN_STRSPN, BUILT_IN_NORMAL, "strspn", NULL, NULL},
{ "__builtin_strcspn", BT_FN_SIZE_CONST_STRING_CONST_STRING, BUILT_IN_STRCSPN, BUILT_IN_NORMAL, "strcspn", NULL, NULL},
{ "__builtin_strchr",  BT_FN_STRING_CONST_STRING_INT, BUILT_IN_STRCHR, BUILT_IN_NORMAL, "strchr", NULL, NULL},
{ "__builtin_strrchr", BT_FN_STRING_CONST_STRING_INT, BUILT_IN_STRCHR, BUILT_IN_NORMAL, "strrchr", NULL, NULL},
  //{ "__builtin_constant_p", BT_FN_INT_VAR, BUILT_IN_CONSTANT_P, BUILT_IN_NORMAL, "constant_p", NULL, NULL},
{ "__builtin_frame_address", BT_FN_PTR_UNSIGNED, BUILT_IN_FRAME_ADDRESS, BUILT_IN_NORMAL, "frame_address", NULL, NULL},
{ "__builtin_return_address", BT_FN_PTR_UNSIGNED, BUILT_IN_RETURN_ADDRESS, BUILT_IN_NORMAL, "return_address", NULL, NULL},
  //{ "__builtin_aggregate_incoming_address", BT_FN_PTR_VAR, BUILT_IN_AGGREGATE_INCOMING_ADDRESS, BUILT_IN_NORMAL, "aggregate_incoming_address", NULL, NULL},
{ "__builtin_longjmp", BT_FN_VOID_PTR_INT, BUILT_IN_LONGJMP, BUILT_IN_NORMAL, "longjmp", NULL, NULL},
{ "__builtin_setjmp", BT_FN_INT_PTR, BUILT_IN_SETJMP, BUILT_IN_NORMAL, "setjmp", NULL, NULL},
{ NULL, 0, 0, 0, "", NULL, NULL} };


static GTY(()) tree sizetype_endlink;
static GTY(()) tree unsigned_endlink;
static GTY(()) tree endlink;
static GTY(()) tree math_endlink;
static GTY(()) tree int_endlink;
static GTY(()) tree ptr_endlink;
static GTY(()) tree const_ptr_endlink;
static GTY(()) tree double_ftype_void;
static GTY(()) tree float_ftype_void;
static GTY(()) tree ldouble_ftype_void;
static GTY(()) tree float_ftype_float;
static GTY(()) tree double_ftype_double;
static GTY(()) tree ldouble_ftype_ldouble;
static GTY(()) tree gm2_alloca_node;
static GTY(()) tree gm2_memcpy_node;
static GTY(()) tree gm2_huge_valf_node;
static GTY(()) tree gm2_huge_val_node;
static GTY(()) tree gm2_huge_vall_node;


/* prototypes go here */
/* imports */
extern tree                   convertToPtr   		       	 	  (tree t);
extern tree                   gccgm2_BuildIntegerConstant                 (int value);
extern void                   gccgm2_SetLastFunction                      (tree t);
extern tree                   gccgm2_GetLastFunction                      (void);
extern void                   gccgm2_SetParamList                         (tree t);
extern tree                   gccgm2_GetParamList                         (void);

/* locally defined functions */
static tree                   DoBuiltinAlloca                             (tree params);
static tree                   DoBuiltinMemCopy                            (tree params);
       tree                   gm2builtins_BuildBuiltinTree                (char *name);
       tree                   gm2builtins_GetBuiltinConst                 (char *name);
       int                    gm2builtins_GetBuiltinConstType             (char *name);
       int                    gm2builtins_BuiltinExists                   (char *name);
       tree                   gm2builtins_BuildBuiltinTree                (char *name);
       tree                   gm2builtins_BuiltInMemCopy                  (tree, tree, tree);
       tree                   gm2builtins_BuiltInAlloca                   (tree);
       tree                   gm2builtins_BuiltInHugeVal                  (void);
static void                   create_function_prototype                   (struct builtin_function_entry *fe);
       void                   gm2builtins_init                            (void);
       tree                   gm2builtins_BuiltInHugeValShort             (void);
       tree                   gm2builtins_BuiltInHugeValLong              (void);

/* prototypes finish here */


/* Given a chain CHAIN of tree nodes,
   construct and return a list of those nodes.  */

static tree
listify (tree chain)
{
  tree result = NULL_TREE;
  tree in_tail = chain;
  tree out_tail = NULL_TREE;

  while (in_tail)
    {
      tree next = tree_cons (NULL_TREE, in_tail, NULL_TREE);
      if (out_tail)
	TREE_CHAIN (out_tail) = next;
      else
	result = next;
      out_tail = next;
      in_tail = TREE_CHAIN (in_tail);
    }

  return result;
}

/* Return a definition for a builtin function named NAME and whose data type
   is TYPE.  TYPE should be a function type with argument types.
   FUNCTION_CODE tells later passes how to compile calls to this function.
   See tree.h for its possible values.

   If LIBRARY_NAME is nonzero, use that for DECL_ASSEMBLER_NAME,
   the name to be called if we can't opencode the function.  */

tree
builtin_function (const char *name, tree type, int function_code,
		  enum built_in_class class, const char *library_name,
		  tree attrs ATTRIBUTE_UNUSED)
{
  tree decl = build_decl (FUNCTION_DECL, get_identifier (name), type);

  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  if (library_name)
    SET_DECL_ASSEMBLER_NAME (decl, get_identifier (library_name));

  pushdecl (decl);
  DECL_BUILT_IN_CLASS (decl) = class;
  DECL_FUNCTION_CODE (decl) = function_code;

#if 0
  if (attrs)
    decl_attributes (&decl, attrs, ATTR_FLAG_BUILT_IN);
#endif

  return decl;
}

/*
 *  GetBuiltinConst - returns the gcc tree of a builtin constant, name.
 *                    NIL is returned if the constant is unknown.
 */

tree
gm2builtins_GetBuiltinConst (char *name)
{
  if (strcmp(name, "BITS_PER_UNIT") == 0)
    return gccgm2_BuildIntegerConstant (BITS_PER_UNIT);
  if (strcmp(name, "BITS_PER_WORD") == 0)
    return gccgm2_BuildIntegerConstant (BITS_PER_WORD);
  if (strcmp (name, "BITS_PER_CHAR") == 0)
    return gccgm2_BuildIntegerConstant (CHAR_TYPE_SIZE);
  if (strcmp (name, "UNITS_PER_WORD") == 0)
    return gccgm2_BuildIntegerConstant (UNITS_PER_WORD);
  
  return NULL_TREE;
}

/*
 *  GetBuiltinConstType - returns the type of a builtin constant, name.
 *
 *                        0 = unknown constant name
 *                        1 = integer
 *                        2 = real
 */

int
gm2builtins_GetBuiltinConstType (char *name)
{
  if (strcmp(name, "BITS_PER_UNIT") == 0)
    return 1;
  if (strcmp(name, "BITS_PER_WORD") == 0)
    return 1;
  if (strcmp (name, "BITS_PER_CHAR") == 0)
    return 1;
  if (strcmp (name, "UNITS_PER_WORD") == 0)
    return 1;
  
  return 0;
}

/*
 *  BuiltInMemCopy - copy n bytes of memory efficiently from address
 *                   src to dest.
 */

tree
gm2builtins_BuiltInMemCopy (tree dest, tree src, tree n)
{
  tree params = chainon (chainon (build_tree_list (NULL_TREE, convertToPtr (dest)),
				  build_tree_list (NULL_TREE, convertToPtr (src))),
			 build_tree_list (NULL_TREE, n));
  return DoBuiltinMemCopy (params);
}

/*
 *  BuiltInAlloca - given an expression, n, allocate, n, bytes on
 *                  the stack for the life of the current function.
 */

tree
gm2builtins_BuiltInAlloca (tree n)
{
  return DoBuiltinAlloca (listify (n));
}

/*
 *  BuiltinExists - returns TRUE if the builtin function, name, exists
 *                  for this target architecture.
 */

int
gm2builtins_BuiltinExists (char *name)
{
  struct builtin_function_entry *fe;

  for (fe=&list_of_builtins[0]; fe->name != NULL; fe++)
    if (strcmp(name, fe->name) == 0)
      return TRUE;

  return FALSE;
}

/*
 *  BuildBuiltinTree - returns a Tree containing the builtin function, name.
 */

tree
gm2builtins_BuildBuiltinTree (char *name)
{
  struct builtin_function_entry *fe;
  tree t;

  gccgm2_SetLastFunction(NULL_TREE);
  for (fe=&list_of_builtins[0]; fe->name != NULL; fe++)
    if (strcmp(name, fe->name) == 0) {
      tree functype = TREE_TYPE (fe->function_node);
      tree funcptr  = build1 (ADDR_EXPR, build_pointer_type (functype), fe->function_node);
      gccgm2_SetLastFunction(build (CALL_EXPR, fe->return_node,
				    funcptr, gccgm2_GetParamList (), NULL_TREE));

      gccgm2_SetParamList (NULL_TREE);
      t = gccgm2_GetLastFunction ();
      if (fe->return_node == void_type_node)
	gccgm2_SetLastFunction(NULL_TREE);
      return t;
    }
  
  gccgm2_SetParamList(NULL_TREE);
  return gccgm2_GetLastFunction ();
}

static tree
DoBuiltinMemCopy (tree params)
{
  tree functype = TREE_TYPE (gm2_memcpy_node);
  tree funcptr  = build1 (ADDR_EXPR, build_pointer_type (functype), gm2_memcpy_node);
  tree call     = build (CALL_EXPR, ptr_type_node, funcptr, params, NULL_TREE);

#if 0
  fprintf(stderr, "built the modula-2 call, here are the params\n"); fflush(stderr);
  debug_tree (params);
  fprintf(stderr, "built the modula-2 call, here is the tree\n"); fflush(stderr);
  debug_tree (call);
#endif
  return call;
}

static tree
DoBuiltinAlloca (tree params)
{
  tree functype = TREE_TYPE (gm2_alloca_node);
  tree funcptr  = build1 (ADDR_EXPR, build_pointer_type (functype), gm2_alloca_node);
  tree call     = build (CALL_EXPR, ptr_type_node, funcptr, params, NULL_TREE);

  return call;
}

tree
gm2builtins_BuiltInHugeVal (void)
{
  tree functype = TREE_TYPE (gm2_huge_val_node);
  tree funcptr  = build1 (ADDR_EXPR, build_pointer_type (functype), gm2_huge_val_node);
  tree call     = build (CALL_EXPR, ptr_type_node, funcptr, NULL_TREE, NULL_TREE);
  return call;
}

tree
gm2builtins_BuiltInHugeValShort (void)
{
  tree functype = TREE_TYPE (gm2_huge_valf_node);
  tree funcptr  = build1 (ADDR_EXPR, build_pointer_type (functype), gm2_huge_valf_node);
  tree call     = build (CALL_EXPR, ptr_type_node, funcptr, NULL_TREE, NULL_TREE);
  return call;
}

tree
gm2builtins_BuiltInHugeValLong (void)
{
  tree functype = TREE_TYPE (gm2_huge_vall_node);
  tree funcptr  = build1 (ADDR_EXPR, build_pointer_type (functype), gm2_huge_vall_node);
  tree call     = build (CALL_EXPR, ptr_type_node, funcptr, NULL_TREE, NULL_TREE);
  return call;
}

static void
create_function_prototype (struct builtin_function_entry *fe)
{
  tree ftype;

  switch (fe->defn) {

  case BT_FN_PTR_SIZE:
    ftype = build_function_type (ptr_type_node, sizetype_endlink);
    fe->return_node = ptr_type_node;
    break;

  case BT_FN_STRING_STRING_CONST_STRING_SIZE:
  case BT_FN_TRAD_PTR_PTR_CONST_PTR_SIZE:
    ftype = build_function_type (ptr_type_node,
				 tree_cons (NULL_TREE, ptr_type_node,
					    tree_cons (NULL_TREE, const_ptr_type_node,
						       sizetype_endlink)));
    fe->return_node = ptr_type_node;
    break;
  case BT_FN_FLOAT:
    ftype = float_ftype_void;
    fe->return_node = float_type_node;
    break;
  case BT_FN_DOUBLE:
    ftype = double_ftype_void;
    fe->return_node = double_type_node;
    break;
  case BT_FN_LONG_DOUBLE:
    ftype = ldouble_ftype_void;
    fe->return_node = long_double_type_node;
    break;
  case BT_FN_FLOAT_FLOAT:
    ftype = float_ftype_float;
    fe->return_node = float_type_node;
    break;
  case BT_FN_DOUBLE_DOUBLE:
    ftype = double_ftype_double;
    fe->return_node = double_type_node;
    break;
  case BT_FN_LONG_DOUBLE_LONG_DOUBLE:
    ftype = ldouble_ftype_ldouble;
    fe->return_node = long_double_type_node;
    break;
  case BT_FN_STRING_CONST_STRING_INT:
    ftype = build_function_type (ptr_type_node,
				 tree_cons (NULL_TREE, ptr_type_node,
					    int_endlink));
    fe->return_node = ptr_type_node;
    break;
  case BT_FN_INT_CONST_PTR_CONST_PTR_SIZE:
    ftype = build_function_type (integer_type_node,
				 tree_cons (NULL_TREE, const_ptr_type_node,
					    tree_cons (NULL_TREE, const_ptr_type_node,
						       int_endlink)));
    fe->return_node = integer_type_node;
    break;
  case BT_FN_TRAD_PTR_PTR_INT_SIZE:
    ftype = build_function_type (ptr_type_node,
				 tree_cons (NULL_TREE, ptr_type_node,
					    tree_cons (NULL_TREE, integer_type_node,
						       sizetype_endlink)));
    fe->return_node = ptr_type_node;
    break;
  case BT_FN_STRING_STRING_CONST_STRING:
    ftype = build_function_type (ptr_type_node,
				 tree_cons (NULL_TREE, ptr_type_node,
					    ptr_endlink));
    fe->return_node = ptr_type_node;
    break;
  case BT_FN_INT_CONST_STRING_CONST_STRING:
    ftype = build_function_type (integer_type_node,
				 tree_cons (NULL_TREE, const_ptr_type_node,
					    ptr_endlink));
    fe->return_node = integer_type_node;
    break;
  case BT_FN_INT_CONST_STRING_CONST_STRING_SIZE:
    ftype = build_function_type (integer_type_node,
				 tree_cons (NULL_TREE, const_ptr_type_node,
					    tree_cons (NULL_TREE, const_ptr_type_node,
						       sizetype_endlink)));
    fe->return_node = integer_type_node;
    break;
  case BT_FN_INT_CONST_STRING:
    ftype = build_function_type (integer_type_node, ptr_endlink);
    fe->return_node = integer_type_node;
    break;
  case BT_FN_STRING_CONST_STRING_CONST_STRING:
    ftype = build_function_type (ptr_type_node,
				 tree_cons (NULL_TREE, const_ptr_type_node,
					    const_ptr_endlink));
    fe->return_node = ptr_type_node;
    break;
  case BT_FN_SIZE_CONST_STRING_CONST_STRING:
    ftype = build_function_type (sizetype,
				 tree_cons (NULL_TREE, const_ptr_type_node,
					    const_ptr_endlink));
    fe->return_node = sizetype;
    break;
  case BT_FN_PTR_UNSIGNED:
    ftype = build_function_type (ptr_type_node, unsigned_endlink);
    fe->return_node = ptr_type_node;
    break;
  case BT_FN_VOID_PTR_INT:
    ftype = build_function_type (void_type_node,
				 tree_cons (NULL_TREE, ptr_type_node,
					    int_endlink));
    fe->return_node = void_type_node;
    break;
  case BT_FN_INT_PTR:
    ftype = build_function_type (integer_type_node, ptr_endlink);
    fe->return_node = integer_type_node;
    break;
  case BT_FN_INT_FLOAT:
    ftype = build_function_type (integer_type_node,
				 tree_cons (NULL_TREE, float_type_node,
					    endlink));
    fe->return_node = integer_type_node;
    break;
  case BT_FN_INT_DOUBLE:
    ftype = build_function_type (integer_type_node,
				 tree_cons (NULL_TREE, double_type_node,
					    endlink));
    fe->return_node = integer_type_node;
    break;
  case BT_FN_INT_LONG_DOUBLE:
    ftype = build_function_type (integer_type_node,
				 tree_cons (NULL_TREE, long_double_type_node,
					    endlink));
    fe->return_node = integer_type_node;
    break;
  case BT_FN_FLOAT_FCOMPLEX:
    ftype = build_function_type (float_type_node,
				 tree_cons (NULL_TREE, complex_float_type_node,
					    endlink));
    fe->return_node = float_type_node;
    break;
  case BT_FN_DOUBLE_DCOMPLEX:
    ftype = build_function_type (double_type_node,
				 tree_cons (NULL_TREE, complex_double_type_node,
					    endlink));
    fe->return_node = double_type_node;
    break;
  case BT_FN_LONG_DOUBLE_LDCOMPLEX:
    ftype = build_function_type (long_double_type_node,
				 tree_cons (NULL_TREE, complex_long_double_type_node,
					    endlink));
    fe->return_node = long_double_type_node;
    break;
  case BT_FN_FCOMPLEX_FCOMPLEX:
    ftype = build_function_type (complex_float_type_node,
				 tree_cons (NULL_TREE, complex_float_type_node,
					    endlink));
    fe->return_node = complex_float_type_node;
    break;
  case BT_FN_DCOMPLEX_DCOMPLEX:
    ftype = build_function_type (complex_double_type_node,
				 tree_cons (NULL_TREE, complex_double_type_node,
					    endlink));
    fe->return_node = complex_double_type_node;
    break;
  case BT_FN_LDCOMPLEX_LDCOMPLEX:
    ftype = build_function_type (complex_long_double_type_node,
				 tree_cons (NULL_TREE, complex_long_double_type_node,
					    endlink));
    fe->return_node = complex_long_double_type_node;
    break;
  default:
    ERROR("enum has no case");
  }
  fe->function_node = builtin_function (fe->name, ftype, fe->function_code, fe->class, fe->library_name, NULL);
}

static tree
find_builtin_tree (const char *name)
{
  struct builtin_function_entry *fe;

  for (fe=&list_of_builtins[0]; fe->name != NULL; fe++)
    if (strcmp(name, fe->name) == 0)
      return fe->function_node;

  ERROR ("cannot find builtin function");
  return NULL_TREE;
}

void
gm2builtins_init (void)
{
  int i;

  endlink = void_list_node;
  sizetype_endlink = tree_cons (NULL_TREE, sizetype, endlink);
  math_endlink = tree_cons (NULL_TREE, void_type_node, NULL_TREE);
  int_endlink =  tree_cons (NULL_TREE, integer_type_node, NULL_TREE);
  ptr_endlink =  tree_cons (NULL_TREE, ptr_type_node, NULL_TREE);
  const_ptr_endlink =  tree_cons (NULL_TREE, const_ptr_type_node, NULL_TREE);
  unsigned_endlink = tree_cons (NULL_TREE, unsigned_type_node, NULL_TREE);

  float_ftype_void = build_function_type (float_type_node, math_endlink);
  double_ftype_void = build_function_type (double_type_node, math_endlink);
  ldouble_ftype_void = build_function_type (long_double_type_node,
					    math_endlink);

  float_ftype_float
    = build_function_type (float_type_node,
 			   tree_cons (NULL_TREE, float_type_node, math_endlink));

  double_ftype_double
    = build_function_type (double_type_node,
			   tree_cons (NULL_TREE, double_type_node, math_endlink));
			   
  ldouble_ftype_ldouble
    = build_function_type (long_double_type_node,
			   tree_cons (NULL_TREE, long_double_type_node,
				      endlink));
  
  for (i=0; list_of_builtins[i].name != NULL; i++)
    create_function_prototype (&list_of_builtins[i]);

  gm2_alloca_node = find_builtin_tree ("__builtin_alloca");
  gm2_memcpy_node = find_builtin_tree ("__builtin_memcpy");
  gm2_huge_valf_node = find_builtin_tree ("__builtin_huge_valf");
  gm2_huge_val_node = find_builtin_tree ("__builtin_huge_val");
  gm2_huge_vall_node = find_builtin_tree ("__builtin_huge_vall");
}

#include "gt-gm2-gm2builtins.h"

/* END gm2builtins. */
