/* Copyright (C) 2010 Free Software Foundation, Inc. */
/* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston,
MA 02110-1301, USA. */
/* Copyright (C) 2008, 2009, 2010
 * Free Software Foundation, Inc.
 *
 *  Gaius Mulley <gaius@glam.ac.uk> constructed this file.
 *  It was built by borrowing code from the gcc-.../gcc/cp/except.c
 *  file and its function is to provide an interface between the
 *  Modula-2 front end quadruples and GCC's exception handling ABI.
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
#include "gm2-tree.h"

#define GM2
#define GM2_BUG_REPORT "Please report this crash to the GNU Modula-2 mailing list <gm2@glam.ac.uk>\n"

#define ASSERT(X,Y)   { if (!(X)) { debug_tree(Y); internal_error("[%s:%d]:condition `%s' failed", \
                                                                   __FILE__, __LINE__, #X); } }
#define ERROR(X)      { internal_error("[%s:%d]:%s",               __FILE__, __LINE__, X); }

/* external functions */

void gccgm2_BuildStartFunctionDeclaration (int uses_varargs);
tree gccgm2_BuildEndFunctionDeclaration (const char *name, tree returntype, int isexternal, int isnested, int ispublic);
tree gccgm2_BuildParameterDeclaration (char *name, tree type, int isreference);
tree push_stmt_list (void);
tree pop_stmt_list (tree t);

/* local prototypes */

void gm2except_InitExceptions (void);
/* tree gm2except_BuildTryCatch (tree try_block, tree catch_block); */
static tree build_exc_ptr (void);
tree gm2except_BuildThrow (tree exp);
static tree do_begin_catch (void);
static tree do_end_catch (void);
tree gm2except_BuildTryBegin (void);
void gm2except_BuildTryEnd (tree try_block);
tree gm2except_BuildCatchBegin (void);
tree gm2except_BuildCatchEnd (tree handler, tree try_block);
static tree begin_handler (void);
static void finish_handler (tree handler);
static tree finish_handler_parms (tree handler);
static void finish_handler_sequence (tree try_block);
static tree begin_try_block (void);
static void finish_handler (tree handler);
static tree finish_expr_stmt (tree expr);
static tree maybe_cleanup_point_expr_void (tree expr);
tree build_target_expr_with_type (tree init, tree type);
tree get_target_expr (tree init);
static tree build_eh_type_type (tree type);
static tree get_tinfo_decl_m2 (void);
static tree eh_type_info (tree type);
static tree build_address (tree t);


void _M2_gm2except_init (void);
void _M2_gm2except_finally (void);

/* exception handling library functions */

static GTY(()) tree fn_rethrow_tree = NULL_TREE;
static GTY(()) tree fn_begin_catch_tree = NULL_TREE;
static GTY(()) tree fn_end_catch_tree = NULL_TREE;
static GTY(()) tree fn_throw_tree = NULL_TREE;
static GTY(()) tree cleanup_type = NULL_TREE;
static GTY(()) tree fn_allocate_exception_tree = NULL_TREE;
static GTY(()) tree gm2_eh_int_type = NULL_TREE;

/* Modula-2 linker fodder */

void _M2_gm2except_init (void) {}
void _M2_gm2except_finally (void) {}

/*
 *  InitExceptions - initialize this module, it declares the
 *                   external functions and assigns them to
 *                   the appropriate global tree variables.
 */

void
gm2except_InitExceptions (void)
{
  tree t;

  flag_exceptions = 1;
  init_eh ();
  eh_personality_libfunc = init_one_libfunc (USING_SJLJ_EXCEPTIONS
					     ? "__gxx_personality_sj0"
					     : "__gxx_personality_v0");
  default_init_unwind_resume_libfunc ();

  gccgm2_BuildStartFunctionDeclaration (FALSE);
  fn_rethrow_tree = gccgm2_BuildEndFunctionDeclaration ("__cxa_rethrow",
							void_type_node,
							TRUE, FALSE, TRUE);
  TREE_NOTHROW (fn_rethrow_tree) = 0;

  gccgm2_BuildStartFunctionDeclaration (FALSE);
  gccgm2_BuildParameterDeclaration (NULL, ptr_type_node, FALSE);
  fn_begin_catch_tree = gccgm2_BuildEndFunctionDeclaration ("__cxa_begin_catch",
							    ptr_type_node,
							    TRUE, FALSE, TRUE);
  gccgm2_BuildStartFunctionDeclaration (FALSE);
  fn_end_catch_tree = gccgm2_BuildEndFunctionDeclaration ("__cxa_end_catch",
							  void_type_node,
							  TRUE, FALSE, TRUE);
  /* This can throw if the destructor for the exception throws.  */
  TREE_NOTHROW (fn_end_catch_tree) = 0;

  /* The CLEANUP_TYPE is the internal type of a destructor.  */
  t = void_list_node;
  t = tree_cons (NULL_TREE, ptr_type_node, t);
  t = build_function_type (void_type_node, t);
  cleanup_type = build_pointer_type (t);

  /* Declare void __cxa_throw (void*, void*, void (*)(void*)).  */
  gccgm2_BuildStartFunctionDeclaration (FALSE);
  gccgm2_BuildParameterDeclaration (NULL, cleanup_type, FALSE);
  gccgm2_BuildParameterDeclaration (NULL, ptr_type_node, FALSE);
  gccgm2_BuildParameterDeclaration (NULL, ptr_type_node, FALSE);
  fn_throw_tree = gccgm2_BuildEndFunctionDeclaration ("__cxa_throw",
						      void_type_node,
						      TRUE, FALSE, TRUE);

  /* Declare void *__cxa_allocate_exception(size_t).  */
  gccgm2_BuildStartFunctionDeclaration (FALSE);
  gccgm2_BuildParameterDeclaration (NULL, size_type_node, FALSE);
  fn_allocate_exception_tree = gccgm2_BuildEndFunctionDeclaration ("__cxa_allocate_exception",
								   ptr_type_node, 
								   TRUE, FALSE, TRUE);
 /*
  *  define integer type exception type which will match
  *  C++ int type in the C++ runtime library.
  */
  gm2_eh_int_type = build_eh_type_type (integer_type_node);
}

#if 0
/* unused
 *  gccgm2_BuildTryCatch - builds and returns a tree which
 *                         represents a try_block and a
 *                         catch statement list.
 */

tree
gm2except_BuildTryCatch (tree try_block, tree catch_block)
{
  return build2 (TRY_CATCH_EXPR, void_type_node,
		 try_block, catch_block);
}
#endif

/*
 *  build_exc_ptr - creates the GCC internal type, pointer to
 *                  exception control block.
 */

static
tree
build_exc_ptr (void)
{
  return build0 (EXC_PTR_EXPR, ptr_type_node);
}

static
tree
get_tinfo_decl_m2 (void)
{
  tree t = build_decl (VAR_DECL, get_identifier ("_ZTIi"),
		       ptr_type_node);
  TREE_STATIC (t) = 1;
  DECL_EXTERNAL (t) = 1;
  TREE_PUBLIC (t) = 1;
  DECL_ARTIFICIAL (t) = 1;
  DECL_IGNORED_P (t) = 1;
  pushdecl (t);
  make_decl_rtl (t);
  return t;
}

/* Return the type info for TYPE as used by EH machinery.  */
static
tree
eh_type_info (tree type)
{
  tree exp;

  if (type == NULL_TREE || type == error_mark_node)
    return type;

#if 0
  if (decl_is_java_type (type, 0))
    exp = build_java_class_ref (TREE_TYPE (type));
  else
    exp = get_tinfo_decl (type);
#else
  exp = get_tinfo_decl_m2 ();
#endif

  return exp;
}

/* Return an ADDR_EXPR giving the address of T.  This function
   attempts no optimizations or simplifications; it is a low-level
   primitive.  */

static
tree
build_address (tree t)
{
  tree addr = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (t)), t);

  return addr;
}

/* Build the address of a typeinfo decl for use in the runtime
   matching field of the exception model.  */

static tree
build_eh_type_type (tree type)
{
  tree exp = eh_type_info (type);

  if (!exp)
    return NULL;

  TREE_USED (exp) = 1;
#if 0
  mark_used (exp);
#endif

  return convert (ptr_type_node, build_address (exp));
}

/* Return a pointer to a buffer for an exception object of type TYPE.  */

static tree
do_allocate_exception (tree type)
{
  return build_function_call (fn_allocate_exception_tree,
			      tree_cons (NULL_TREE, size_in_bytes (type),
					 NULL_TREE));
}

/* Build a TARGET_EXPR, initializing the DECL with the VALUE.  */

static tree
build_target_expr (tree decl, tree value)
{
  tree t;

  t = build4 (TARGET_EXPR, TREE_TYPE (decl), decl, value,
	      NULL_TREE, NULL_TREE);
  /* We always set TREE_SIDE_EFFECTS so that expand_expr does not
     ignore the TARGET_EXPR.  If there really turn out to be no
     side-effects, then the optimizer should be able to get rid of
     whatever code is generated anyhow.  */
  TREE_SIDE_EFFECTS (t) = 1;

  return t;
}

/* Return an undeclared local temporary of type TYPE for use in building a
   TARGET_EXPR.  */

static tree
build_local_temp (tree type)
{
  tree slot = build_decl (VAR_DECL, NULL_TREE, type);
  DECL_ARTIFICIAL (slot) = 1;
  DECL_IGNORED_P (slot) = 1;
  DECL_CONTEXT (slot) = current_function_decl;
  layout_decl (slot, 0);
  return slot;
}

/* Build a TARGET_EXPR using INIT to initialize a new temporary of the
   indicated TYPE.  */

tree
build_target_expr_with_type (tree init, tree type)
{
  tree slot;

  gcc_assert (!VOID_TYPE_P (type));

  if (TREE_CODE (init) == TARGET_EXPR)
    return init;

  slot = build_local_temp (type);
  return build_target_expr (slot, init);
}

/* Like build_target_expr_with_type, but use the type of INIT.  */

tree
get_target_expr (tree init)
{
  return build_target_expr_with_type (init, TREE_TYPE (init));
}

/* Given an expression PTR for a pointer, return an expression
   for the value pointed to.
   ERRORSTRING is the name of the operator to appear in error messages.  */

tree
build_indirect_ref (tree ptr, const char *errorstring)
{
  tree pointer = default_conversion (ptr);
  tree type = TREE_TYPE (pointer);

  if (TREE_CODE (type) == POINTER_TYPE)
    {
      if (TREE_CODE (pointer) == ADDR_EXPR
	  && (TREE_TYPE (TREE_OPERAND (pointer, 0))
	      == TREE_TYPE (type)))
	return TREE_OPERAND (pointer, 0);
      else
	{
	  tree t = TREE_TYPE (type);
	  tree ref;

	  ref = build1 (INDIRECT_REF, t, pointer);

	  if (!COMPLETE_OR_VOID_TYPE_P (t) && TREE_CODE (t) != ARRAY_TYPE)
	    {
	      error ("dereferencing pointer to incomplete type");
	      return error_mark_node;
	    }
	  if (VOID_TYPE_P (t) && skip_evaluation == 0)
	    warning (0, "dereferencing %<void *%> pointer");

	  /* We *must* set TREE_READONLY when dereferencing a pointer to const,
	     so that we get the proper error message if the result is used
	     to assign to.  Also, &* is supposed to be a no-op.
	     And ANSI C seems to specify that the type of the result
	     should be the const type.  */
	  /* A de-reference of a pointer to const is not a const.  It is valid
	     to change it via some other pointer.  */
	  TREE_READONLY (ref) = TYPE_READONLY (t);
	  TREE_SIDE_EFFECTS (ref)
	    = TYPE_VOLATILE (t) || TREE_SIDE_EFFECTS (pointer);
	  TREE_THIS_VOLATILE (ref) = TYPE_VOLATILE (t);
	  return ref;
	}
    }
  else if (TREE_CODE (pointer) != ERROR_MARK)
    error ("invalid type argument of %qs", errorstring);
  return error_mark_node;
}

/*
 *  gccgm2_BuildThrow - builds a throw expression and
 *                      return the tree.
 */

tree
gm2except_BuildThrow (tree exp)
{
  if (exp == NULL_TREE)
    {
      /* rethrow the current exception */
      tree exp = build_function_call (fn_rethrow_tree, NULL_TREE);
      return build1 (THROW_EXPR, void_type_node, exp);
    }
  else
    {
      tree throw_type;
      tree cleanup;
      tree object, ptr;
      tree allocate_expr;
      tree tmp;

      /* Allocate the space for the exception.  */
      allocate_expr = do_allocate_exception (TREE_TYPE (exp));
      allocate_expr = get_target_expr (allocate_expr);
      ptr = TARGET_EXPR_SLOT (allocate_expr);
      object = build1 (NOP_EXPR, build_pointer_type (TREE_TYPE (exp)), ptr);
#if 1
      object = build_indirect_ref (object, NULL);
#else
      object = build1 (INDIRECT_REF, skip_type_decl (object),
		       build_pointer_type (skip_type_decl (object)));
#endif
      /* And initialize the exception object.  */
      exp = build2 (INIT_EXPR, TREE_TYPE (object), object, exp);

      exp = build1 (CLEANUP_POINT_EXPR, TREE_TYPE (exp), exp);
#if 0
      exp = build1 (MUST_NOT_THROW_EXPR, void_type_node, exp);
#endif

      /* Prepend the allocation.  */
      exp = build2 (COMPOUND_EXPR, TREE_TYPE (exp), allocate_expr, exp);

      throw_type = gm2_eh_int_type;

      cleanup = build_int_cst (cleanup_type, 0);

      tmp = tree_cons (NULL_TREE, cleanup, NULL_TREE);
      tmp = tree_cons (NULL_TREE, throw_type, tmp);
      tmp = tree_cons (NULL_TREE, ptr, tmp);
      /* ??? Indicate that this function call throws throw_type.  */
      tmp = build_function_call (fn_throw_tree, tmp);

      /* Tack on the initialization stuff.  */
      exp = build2 (COMPOUND_EXPR, TREE_TYPE (tmp), exp, tmp);
      exp = build1 (THROW_EXPR, void_type_node, exp);

      return exp;
    }
}

/*  Build up a call to __cxa_begin_catch, to tell the runtime that the
 *  exception has been handled.
 */

static tree
do_begin_catch (void)
{
  return build_function_call (fn_begin_catch_tree,
			      tree_cons (NULL_TREE, build_exc_ptr (),
					 NULL_TREE));
}

/* Build up a call to __cxa_end_catch, to destroy the exception object
   for the current catch block if no others are currently using it.  */

static tree
do_end_catch (void)
{
  tree cleanup = build_function_call (fn_end_catch_tree, NULL_TREE);
  TREE_NOTHROW (cleanup) = 1;

  return cleanup;
}

/*
 *  BuildTryBegin - returns a tree representing the 'try' block.
 */

tree
gm2except_BuildTryBegin (void)
{
  return begin_try_block ();
}

/*
 *  BuildTryEnd - builds the end of the Try block and prepares
 *                for the catch handlers.
 */

void
gm2except_BuildTryEnd (tree try_block)
{
  TRY_STMTS (try_block) = pop_stmt_list (TRY_STMTS (try_block));
  TRY_HANDLERS (try_block) = push_stmt_list ();
}

/*
 *  BuildCatchBegin - creates a handler tree for the C++
 *                    statement 'catch (...) {'.
 *                    It returns the handler tree.
 */

tree
gm2except_BuildCatchBegin (void)
{
  tree handler = begin_handler ();
  return finish_handler_parms (handler);
}

/*
 *  BuildCatchEnd - completes a try catch block.
 *                  It returns the, try_block, tree.
 *                  It creates the C++ statement
 *
 *                  '}' which matches the catch above.
 */

tree
gm2except_BuildCatchEnd (tree handler, tree try_block)
{
  finish_handler (handler);
  finish_handler_sequence (try_block);  /* this does a pop_stmt_list */
  return try_block;
}

/* Begin a handler.  Returns a HANDLER if appropriate.  */

static
tree
begin_handler (void)
{
  tree r;

  r = build_stmt (HANDLER, NULL_TREE, NULL_TREE);
  add_stmt (r);

#if 0
  /* Create a binding level for the eh_info and the exception object
     cleanup.  */
  HANDLER_BODY (r) = do_pushlevel (sk_catch);
#else
  HANDLER_BODY (r) = push_stmt_list ();
  /* pushlevel (0);   /* gaius */
#endif

  return r;
}

/* Finish a handler, which may be given by HANDLER.  The BLOCKs are
   the return value from the matching call to finish_handler_parms.  */

static
void
finish_handler (tree handler)
{
  /* we might need to rethrow the exception if we reach the end */
  /* use this code:  finish_expr_stmt (build_throw (NULL_TREE)); */
#if 0
  HANDLER_BODY (handler) = do_poplevel (HANDLER_BODY (handler));
#else
  /* poplevel (0, 1, 0);  /* here or elsewhere? gaius */
  tree body = pop_stmt_list (HANDLER_BODY (handler));
  HANDLER_BODY (handler) = build2 (TRY_FINALLY_EXPR, void_type_node, body, do_end_catch ());
#endif
}

/* Finish the handler-parameters for a handler, which may be given by
   HANDLER.  */

static
tree
finish_handler_parms (tree handler)
{
  /* equivalent to C++ catch (...) */
  finish_expr_stmt (do_begin_catch ());

  HANDLER_TYPE (handler) = NULL_TREE;
  return handler;
}

/* Finish a handler-sequence for a try-block, which may be given by
   TRY_BLOCK.  */

static
void
finish_handler_sequence (tree try_block)
{
  TRY_HANDLERS (try_block) = pop_stmt_list (TRY_HANDLERS (try_block));
}

/* Begin a try-block.  Returns a newly-created TRY_BLOCK if
   appropriate.  */

static
tree
begin_try_block (void)
{
  tree r = build_stmt (TRY_BLOCK, NULL_TREE, NULL_TREE);
#if 0
  add_stmt (r);
#endif
  TRY_STMTS (r) = push_stmt_list ();
  return r;
}

/* Finish an expression-statement, whose EXPRESSION is as indicated.  */

static
tree
finish_expr_stmt (tree expr)
{
  tree r = NULL_TREE;

  if (expr != NULL_TREE)
    {
      expr = build1 (CONVERT_EXPR, void_type_node, expr);

      /* Simplification of inner statement expressions, compound exprs,
	 etc can result in us already having an EXPR_STMT.  */
      if (TREE_CODE (expr) != CLEANUP_POINT_EXPR)
	{
	  if (TREE_CODE (expr) != EXPR_STMT)
	    expr = build_stmt (EXPR_STMT, expr);
	  expr = maybe_cleanup_point_expr_void (expr);
	}
      r = add_stmt (expr);
    }

  return r;
}

/* Like maybe_cleanup_point_expr except have the type of the new expression be
   void so we don't need to create a temporary variable to hold the inner
   expression.  The reason why we do this is because the original type might be
   an aggregate and we cannot create a temporary variable for that type.  */

static tree
maybe_cleanup_point_expr_void (tree expr)
{
  /* if (stmts_are_full_exprs_p ()) */
  return fold_build_cleanup_point_expr (void_type_node, expr);
}

#include "gt-gm2-gm2except.h"
