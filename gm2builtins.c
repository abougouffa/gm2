/* Copyright (C) 2003 Free Software Foundation, Inc. */
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
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

#include "config.h"
#include "system.h"
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

#define GM2
#define GM2_BUG_REPORT "Please report this crash to the GNU Modula-2 mailing list <gm2@glam.ac.uk>\n"

#define ASSERT(X,Y)   { if (!(X)) { debug_tree(Y); internal_error("[%s:%d]:condition `%s' failed", \
                                                                   __FILE__, __LINE__, #X); } }
#define ERROR(X)      { internal_error("[%s:%d]:%s",               __FILE__, __LINE__, X); }

extern tree last_function;  /* declared in gccgm2.c */
extern tree param_list;     /* declared in gccgm2.c */


typedef enum {
  BT_FN_PTR_SIZE,
  BT_FN_TRAD_PTR_PTR_CONST_PTR_SIZE,
  BT_FN_FLOAT_FLOAT,
  BT_FN_LONG_DOUBLE_LONG_DOUBLE,
} builtin_prototype;

struct builtin_function_entry {
  const char *name;
  builtin_prototype defn;
  int function_code;
  int class;
  const char *library_name;
  tree function_node;
};

/*
 *  entries are added by examining gcc/builtins.def and copying those functions which can be
 *  applied to Modula-2
 */

static struct builtin_function_entry list_of_builtins[] = {
{ "__builtin_alloca", BT_FN_PTR_SIZE, BUILT_IN_ALLOCA, BUILT_IN_NORMAL, "alloca", NULL},
{ "__builtin_memcpy", BT_FN_TRAD_PTR_PTR_CONST_PTR_SIZE, BUILT_IN_MEMCPY, BUILT_IN_NORMAL, "memcpy", NULL},
{ "__builtin_sinf",   BT_FN_FLOAT_FLOAT, BUILT_IN_SINF, BUILT_IN_NORMAL, "sinf", NULL},
  // { "__builtin_sinl",   BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_SINL, BUILT_IN_NORMAL, "sinl", NULL},
{ NULL, 0, 0, 0, "", NULL} };


static tree sizetype_endlink;
static tree endlink, math_endlink;
static tree float_ftype_float, double_ftype_double, ldouble_ftype_ldouble;
static tree gm2_alloca_node, gm2_memcpy_node;


/* prototypes go here */
/* imports */
extern tree                   convertToPtr   		       	 	  PARAMS ((tree t));
extern tree                   gccgm2_BuildIntegerConstant                 PARAMS ((int value));

/* locally defined functions */
       tree                   builtin_function                  	  PARAMS ((const char *name, tree type, int function_void,
									 	  enum built_in_class class,
									 	  const char *library_void));
static tree                   DoBuiltinAlloca                             PARAMS ((tree params));
static tree                   DoBuiltinMemCopy                            PARAMS ((tree params));
       tree                   gm2builtins_BuildBuiltinTree                PARAMS ((char *name));
       tree                   gm2builtins_GetBuiltinConst                 PARAMS ((char *name));
       int                    gm2builtins_GetBuiltinConstType             PARAMS ((char *name));
       int                    gm2builtins_BuiltinExists                   PARAMS ((char *name));
       tree                   gm2builtins_BuildBuiltinTree                PARAMS ((char *name));
       tree                   gm2builtins_BuiltInMemCopy                  PARAMS ((tree, tree, tree));
       tree                   gm2builtins_BuiltInAlloca                   PARAMS ((tree));
static void                   create_function_prototype                   PARAMS ((struct builtin_function_entry *fe));
       void                   gm2builtins_init                            PARAMS ((void));


/* prototypes finish here */


/* Return a definition for a builtin function named NAME and whose data type
   is TYPE.  TYPE should be a function type with argument types.
   FUNCTION_CODE tells later passes how to compile calls to this function.
   See tree.h for its possible values.

   If LIBRARY_NAME is nonzero, use that for DECL_ASSEMBLER_NAME,
   the name to be called if we can't opencode the function.  */

tree
builtin_function (name, type, function_code, class, library_name)
     const char *name;
     tree type;
     int function_code;
     enum built_in_class class;
     const char *library_name;
{
  tree decl = build_decl (FUNCTION_DECL, get_identifier (name), type);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;

  if (library_name)
    SET_DECL_ASSEMBLER_NAME (decl, get_identifier (library_name));
  make_decl_rtl (decl, NULL);
  pushdecl (decl);
  DECL_BUILT_IN_CLASS (decl) = class;
  DECL_FUNCTION_CODE (decl) = function_code;

  /* Warn if a function in the namespace for users
     is used without an occasion to consider it declared.  */
  if (name[0] != '_' || name[1] != '_')
    C_DECL_ANTICIPATED (decl) = 1;

  return decl;
}

/*
 *  GetBuiltinConst - returns the gcc tree of a builtin constant, name.
 *                    NIL is returned if the constant is unknown.
 */

tree
gm2builtins_GetBuiltinConst (name)
     char *name;
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
gm2builtins_GetBuiltinConstType (name)
     char *name;
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
 *  BuiltInMemCopy - copy n bytes of memory efficiently from address src to dest.
 */

tree
gm2builtins_BuiltInMemCopy (dest, src, n)
     tree dest, src, n;
{
  tree params   = chainon (chainon (build_tree_list (NULL_TREE, convertToPtr (dest)),
				    build_tree_list (NULL_TREE, convertToPtr (src))),
			   build_tree_list (NULL_TREE, n));
  return DoBuiltinMemCopy (params);
}

/*
 *  BuiltInAlloca - given an expression, n, allocate, n, bytes on the stack for the life
 *                  of the current function.
 */

tree
gm2builtins_BuiltInAlloca (n)
     tree n;
{
  return DoBuiltinAlloca (listify (n));
}

/*
 *  BuiltinExists - returns TRUE if the builtin function, name, exists
 *                  for this target architecture.
 */

int
gm2builtins_BuiltinExists (name)
     char *name;
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
gm2builtins_BuildBuiltinTree (name)
     char *name;
{
  struct builtin_function_entry *fe;
  last_function = NULL_TREE;

  for (fe=&list_of_builtins[0]; fe->name != NULL; fe++)
    if (strcmp(name, fe->name) == 0) {
      tree functype = TREE_TYPE (fe->function_node);
      tree funcptr  = build1 (ADDR_EXPR, build_pointer_type (functype), fe->function_node);
      last_function = build (CALL_EXPR, ptr_type_node, funcptr, param_list, NULL_TREE);

      param_list = NULL_TREE;
      return last_function;
    }
  
  param_list = NULL_TREE;
  return last_function;
}

static tree
DoBuiltinMemCopy (params)
     tree params;
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
DoBuiltinAlloca (params)
     tree params;
{
  tree functype = TREE_TYPE (gm2_alloca_node);
  tree funcptr  = build1 (ADDR_EXPR, build_pointer_type (functype), gm2_alloca_node);
  tree call     = build (CALL_EXPR, ptr_type_node, funcptr, params, NULL_TREE);

  return call;
}

static void
create_function_prototype (fe)
     struct builtin_function_entry *fe;
{
  tree ftype;

  switch (fe->defn) {

  case BT_FN_PTR_SIZE:
    ftype = build_function_type (ptr_type_node, sizetype_endlink);
    break;

  case BT_FN_TRAD_PTR_PTR_CONST_PTR_SIZE:
    ftype = build_function_type (ptr_type_node,
				 tree_cons (NULL_TREE, ptr_type_node,
					    tree_cons (NULL_TREE, const_ptr_type_node,
						       sizetype_endlink)));
    break;
  case BT_FN_FLOAT_FLOAT:
    ftype = float_ftype_float;
    break;
#if 0
  case BT_FN_DOUBLE_DOUBLE:
    /* not actually used */
    ftype = double_ftype_double;
    break;
#endif
  case BT_FN_LONG_DOUBLE_LONG_DOUBLE:
    ftype = ldouble_ftype_ldouble;
    break;
    
  default:
    ERROR("enum has no case");
  }
  fe->function_node = builtin_function (fe->name, ftype, fe->function_code, fe->class, fe->library_name);
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
gm2builtins_init ()
{
  int i;

  endlink = void_list_node;
  sizetype_endlink = tree_cons (NULL_TREE, sizetype, endlink);
  math_endlink = tree_cons (NULL_TREE, void_type_node, NULL_TREE);
#if 1
  float_ftype_float
    = build_function_type (gccgm2_GetM2RealType (),
			   tree_cons (NULL_TREE, gccgm2_GetM2RealType (), math_endlink));
#else
  float_ftype_float
    = build_function_type (float_type_node,
			   tree_cons (NULL_TREE, float_type_node, math_endlink));
#endif

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
}
