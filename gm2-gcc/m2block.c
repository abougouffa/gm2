/* Copyright (C) 2012, 2013
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
#include "tree-iterator.h"


#define m2block_c
#include "m2block.h"
#include "m2assert.h"
#include "m2tree.h"


/* For each binding contour we allocate a binding_level structure which records
   the entities defined or declared in that contour. Contours include:

        the global one
        one for each subprogram definition

   Binding contours are used to create GCC tree BLOCK nodes.  */


struct binding_level GTY(())
  {
    /* The function associated with the scope.  This is NULL_TREE for the
       global scope.
    */
    tree fndecl;

    /* A chain of _DECL nodes for all variables, constants, functions,
       and typedef types.  These are in the reverse of the order supplied.
     */
    tree names;

    /* The binding level below this one.  This field is only used when the binding level
       has been pushed by pushFunctionScope.
    */
    struct binding_level *next;

    /* All binding levels are placed onto this list.
     */
    struct binding_level *list;

    /* A varray of trees, which represent the statement stack. */
    VEC(tree,gc) *m2_stmt_stack;

    /* A list of constants (only kept in the global binding level).
       Constants need to be kept through the life of the compilation,
       as the same constants can be used in any scope.
    */
    tree constants;

    /* A list of inner module initialisation functions.
     */
    tree init_functions;

    /* A list of types created by M2GCCDeclare prior to code generation
       and those which may not be specifically declared and saved via
       a push_decl.
    */
    tree types;

    /* A list of all DECL_EXPR created within this binding level.  This will
       be prepended to the statement list once the binding level (scope is finished).
    */
    tree decl;

    /* A list of labels which have been created in this scope.
     */
    tree labels;

    /*
     * The number of times this level has been pushed.
     */
    int count;
  };

/* The binding level currently in effect.  */

static GTY(()) struct binding_level *current_binding_level;

/* A chain of binding_level structures awaiting reuse.  */

static GTY(()) struct binding_level *free_binding_level;

/* The outermost binding level, for names of file scope.
   This is created when the compiler is started and exists
   through the entire run.  */

static GTY(()) struct binding_level *global_binding_level;

/* The head of the binding level lists.
 */
static GTY(()) struct binding_level *head_binding_level;

/* Binding level structures are initialized by copying this one.  */

static struct binding_level clear_binding_level
= {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0};


/* The current statement tree.  */

typedef struct stmt_tree_s *stmt_tree_t;

#undef DEBUGGING


/*
 *  assert_global_names - asserts that the global_binding_level->names can be chained.
 */

static void assert_global_names (void)
{
  tree p = global_binding_level->names;

  while (p)
    p = TREE_CHAIN (p);
}


/*
 *  lookupLabel - return label tree in current scope, otherwise NULL_TREE.
 */

static
tree
lookupLabel (tree id)
{
  tree t;

  for (t = current_binding_level->labels; t != NULL_TREE; t = TREE_CHAIN (t))
    {
      tree l = TREE_VALUE (t);

      if (id == DECL_NAME (l))
	return l;
    }
  return NULL_TREE;
}


/*
 *  getLabel - return the label, name, or create a label, name
 *             in the current scope.
 */

tree
m2block_getLabel (location_t location, char *name)
{
  tree id = get_identifier (name);
  tree label = lookupLabel (id);

  if (label == NULL_TREE) {
    label = build_decl (location, LABEL_DECL, id, void_type_node);
    current_binding_level->labels = tree_cons (NULL_TREE, label, current_binding_level->labels);
  }
  if (DECL_CONTEXT (label) == NULL_TREE)
    DECL_CONTEXT (label) = current_function_decl;
  ASSERT ((DECL_CONTEXT (label) == current_function_decl), current_function_decl);
	  
  DECL_MODE (label) = VOIDmode;
  return label;
}


static
struct binding_level *
newLevel (void)
{
  struct binding_level *newlevel;

  if (free_binding_level == NULL)
    newlevel = (struct binding_level *) xmalloc (sizeof (struct binding_level));
  else
    {
      newlevel = free_binding_level;
      free_binding_level = free_binding_level->next;
    }
  *newlevel = clear_binding_level;
  newlevel->m2_stmt_stack = VEC_alloc(tree, gc, 1);

   /* now we push_statement_list (begin_statement_list ())) */
  VEC_safe_push (tree, gc, newlevel->m2_stmt_stack, m2block_begin_statement_list ());
  return newlevel;
}

#if 0
static
void disposeLevel (void)
{
  struct binding_level *b;

  b = current_binding_level;
  current_binding_level = current_binding_level->next;
  b->next = free_binding_level;
  free_binding_level = b;
}
#endif


tree *
m2block_cur_stmt_list_addr (void)
{
  ASSERT_CONDITION (current_binding_level != NULL);

  return (VEC_address (tree, current_binding_level->m2_stmt_stack)
	  + VEC_length (tree, current_binding_level->m2_stmt_stack) - 1);
}

tree
m2block_cur_stmt_list (void)
{
  tree *t = m2block_cur_stmt_list_addr ();

  return *t;
}


/*
 *  is_building_stmt_list - returns TRUE if we are building a statement
 *                          list.  TRUE is returned if we are in a binding level
 *                          and a statement list is under construction.
 */

int
m2block_is_building_stmt_list (void)
{
  ASSERT_CONDITION (current_binding_level != NULL);
  return !VEC_empty (tree, current_binding_level->m2_stmt_stack);
}


/*
 *  push_statement_list - pushes the statement list, t, onto the
 *                        current binding level.
 */

tree
m2block_push_statement_list (tree t)
{
  ASSERT_CONDITION (current_binding_level != NULL);
  VEC_safe_push (tree, gc, current_binding_level->m2_stmt_stack, t);
  return t;
}


/*
 *  pop_statement_list - pops and returns a statement list from the
 *                       current binding level.
 */

tree
m2block_pop_statement_list (void)
{
  ASSERT_CONDITION (current_binding_level != NULL);
  {
    tree t = VEC_pop (tree, current_binding_level->m2_stmt_stack);

    return t;
  }
}


/*
 *  begin_statement_list - starts a tree statement.  It pushes the
 *                         statement list and returns the list node.
 */

tree
m2block_begin_statement_list (void)
{
  return alloc_stmt_list ();
}


/*
 *  end_statement_list - returns the current statement tree.
 *                       The current statement tree is popped from the
 *                       statement stack and the list node is returned.
 */

tree
m2block_end_statement_list (tree t)
{
  /* should we do anything with, t?
     Specifically we may need to test for the presence of a label
     --fixme-- check this
   */
  return t;
}


#if 0
{
  tree u = NULL_TREE;

  /* Pop statement lists until we reach the target level.  The extra
     nestings will be due to outstanding cleanups.  */
  while (TRUE)
    {
      u = VEC_pop (tree, stmt_list_stack);
      if (!VEC_empty (tree, stmt_list_stack))
	{
	  tree x = VEC_last (tree, stmt_list_stack);
	  if (x != NULL_TREE)
	    STATEMENT_LIST_HAS_LABEL (x) |= STATEMENT_LIST_HAS_LABEL (u);
	}
      if (t == u)
	break;
    }

  gcc_assert (u != NULL_TREE);

  /* If the statement list is completely empty, just return it.  This is
     just as good small as build_empty_stmt, with the advantage that
     statement lists are merged when they appended to one another.  So
     using the STATEMENT_LIST avoids pathological buildup of EMPTY_STMT_P
     statements.  */
  if (TREE_SIDE_EFFECTS (t))
    {
      tree_stmt_iterator i = tsi_start (t);

      /* If the statement list contained exactly one statement, then
	 extract it immediately.  */
      if (tsi_one_before_end_p (i))
	{
	  u = tsi_stmt (i);
	  tsi_delink (&i);
	  free_stmt_list (t);
	  t = u;
	}
    }

  return t;
}
#endif


/* Build a generic statement based on the given type of node and
   arguments. Similar to `build_nt', except that we set
   EXPR_LOCATION to LOC. */
/* ??? This should be obsolete with the lineno_stmt productions
   in the grammar.  */

tree
build_stmt (location_t loc, enum tree_code code, ...)
{
  tree ret;
  int length, i;
  va_list p;
  bool side_effects;

  /* This function cannot be used to construct variably-sized nodes.  */
  gcc_assert (TREE_CODE_CLASS (code) != tcc_vl_exp);

  va_start (p, code);

  ret = make_node (code);
  TREE_TYPE (ret) = void_type_node;
  length = TREE_CODE_LENGTH (code);
  SET_EXPR_LOCATION (ret, loc);

  /* TREE_SIDE_EFFECTS will already be set for statements with
     implicit side effects.  Here we make sure it is set for other
     expressions by checking whether the parameters have side
     effects.  */

  side_effects = false;
  for (i = 0; i < length; i++)
    {
      tree t = va_arg (p, tree);
      if (t && !TYPE_P (t))
	side_effects |= TREE_SIDE_EFFECTS (t);
      TREE_OPERAND (ret, i) = t;
    }

  TREE_SIDE_EFFECTS (ret) |= side_effects;

  va_end (p);
  return ret;
}

tree
add_stmt (tree t)
{
  enum tree_code code = TREE_CODE (t);

  ASSERT_CONDITION (code != ERROR_MARK);
  if (CAN_HAVE_LOCATION_P (t) && code != LABEL_EXPR)
    {
      if (!EXPR_HAS_LOCATION (t))
	SET_EXPR_LOCATION (t, input_location);
    }

#if 0
  /* Add T to the statement-tree.  Non-side-effect statements need to be
     recorded during statement expressions.  */
  if (!building_stmt_list_p ())
    m2block_begin_statement_list ();
#endif

  if (code == LABEL_EXPR || code == CASE_LABEL_EXPR)
    STATEMENT_LIST_HAS_LABEL (m2block_cur_stmt_list()) = 1;

  append_to_statement_list_force (t, m2block_cur_stmt_list_addr ());

  return t;
}


/*
 *  findLevel - returns the binding level associated with, fndecl, one is created if
 *              there is no existing one on head_binding_level.
 */

static struct binding_level *
findLevel (tree fndecl)
{
  struct binding_level *b;

  if (fndecl == NULL_TREE)
    return global_binding_level;

  b = head_binding_level; 
  while ((b != NULL) && (b->fndecl != fndecl))
    b = b->list;

  if (b == NULL) {
    b = newLevel ();
    b->fndecl = fndecl;
    b->list = head_binding_level;
    b->next = NULL;
  }
  return b;
}


/*
 *  pushFunctionScope - push a binding level.
 */

void
m2block_pushFunctionScope (tree fndecl)
{
  struct binding_level *n;
  struct binding_level *b;

#if defined(DEBUGGING)
  if (fndecl != NULL)
    printf("pushFunctionScope\n");
#endif

  /*
   *  allow multiple consecutive pushes of the same scope
   */

  if ((current_binding_level && current_binding_level->fndecl != NULL) && (fndecl != NULL))
    if (current_binding_level->fndecl == fndecl) {
      current_binding_level->count++;
      return;
    }

  /*
   *  firstly check to see that fndecl is not already on the binding stack.
   */

  for (b = current_binding_level; b != NULL; b = b->next)
    /* only allowed one instance of the binding on the stack at a time.  */
    ASSERT_CONDITION (b->fndecl != fndecl);

  n = findLevel (fndecl);

  /* Add this level to the front of the stack. */
  n->next = current_binding_level;
  current_binding_level = n;
}


#if 0
/* Set the TYPE_CONTEXT of all of TYPE's variants to CONTEXT.  */

static void
set_type_context (tree type, tree context)
{
  for (type = TYPE_MAIN_VARIANT (type); type;
       type = TYPE_NEXT_VARIANT (type))
    TYPE_CONTEXT (type) = context;
}
#endif


#if 0
/*
 *
 */

static void
addTreeToBlock (tree fndecl, tree block, tree p)
{
  switch (TREE_CODE (p))
    {
    case LABEL_DECL:
      /* Labels go in BLOCK_VARS.  */
      DECL_CHAIN (p) = BLOCK_VARS (block);
      BLOCK_VARS (block) = p;

    case ENUMERAL_TYPE:
    case UNION_TYPE:
    case RECORD_TYPE:
      set_type_context (p, fndecl);
      break;
    default:
      break;
    }

#if 0
    case FUNCTION_DECL:
	  /* Propagate TREE_ADDRESSABLE from nested functions to their
	     containing functions.  */
	  if (!TREE_ASM_WRITTEN (p)
	      && DECL_INITIAL (p) != 0
	      && TREE_ADDRESSABLE (p)
	      && DECL_ABSTRACT_ORIGIN (p) != 0
	      && DECL_ABSTRACT_ORIGIN (p) != p)
	    TREE_ADDRESSABLE (DECL_ABSTRACT_ORIGIN (p)) = 1;
	  if (!DECL_EXTERNAL (p)
	      && !DECL_INITIAL (p)
	      && scope != file_scope
	      && scope != external_scope)
	    {
	      error ("nested function %q+D declared but never defined", p);
	      undef_nested_function = true;
	    }
	  else if (DECL_DECLARED_INLINE_P (p)
		   && TREE_PUBLIC (p)
		   && !DECL_INITIAL (p))
	    {
	      /* C99 6.7.4p6: "a function with external linkage... declared
		 with an inline function specifier ... shall also be defined
		 in the same translation unit."  */
	      if (!flag_gnu89_inline)
		pedwarn (input_location, 0,
			 "inline function %q+D declared but never defined", p);
	      DECL_EXTERNAL (p) = 1;
	    }

	  goto common_symbol;

	case VAR_DECL:
	  /* Warnings for unused variables.  */
	  if ((!TREE_USED (p) || !DECL_READ_P (p))
	      && !TREE_NO_WARNING (p)
	      && !DECL_IN_SYSTEM_HEADER (p)
	      && DECL_NAME (p)
	      && !DECL_ARTIFICIAL (p)
	      && scope != file_scope
	      && scope != external_scope)
	    {
	      if (!TREE_USED (p))
		warning (OPT_Wunused_variable, "unused variable %q+D", p);
	      else if (DECL_CONTEXT (p) == current_function_decl)
		warning_at (DECL_SOURCE_LOCATION (p),
			    OPT_Wunused_but_set_variable,
			    "variable %qD set but not used", p);
	    }

	  if (b->inner_comp)
	    {
	      error ("type of array %q+D completed incompatibly with"
		     " implicit initialization", p);
	    }

	  /* Fall through.  */
	case TYPE_DECL:
	case CONST_DECL:
	common_symbol:
	  /* All of these go in BLOCK_VARS, but only if this is the
	     binding in the home scope.  */
	  if (!b->nested)
	    {
	      DECL_CHAIN (p) = BLOCK_VARS (block);
	      BLOCK_VARS (block) = p;
	    }
	  else if (VAR_OR_FUNCTION_DECL_P (p) && scope != file_scope)
	    {
	      /* For block local externs add a special
		 DECL_EXTERNAL decl for debug info generation.  */
	      tree extp = copy_node (p);

	      DECL_EXTERNAL (extp) = 1;
	      TREE_STATIC (extp) = 0;
	      TREE_PUBLIC (extp) = 1;
	      DECL_INITIAL (extp) = NULL_TREE;
	      DECL_LANG_SPECIFIC (extp) = NULL;
	      DECL_CONTEXT (extp) = current_function_decl;
	      if (TREE_CODE (p) == FUNCTION_DECL)
		{
		  DECL_RESULT (extp) = NULL_TREE;
		  DECL_SAVED_TREE (extp) = NULL_TREE;
		  DECL_STRUCT_FUNCTION (extp) = NULL;
		}
	      if (b->locus != UNKNOWN_LOCATION)
		DECL_SOURCE_LOCATION (extp) = b->locus;
	      DECL_CHAIN (extp) = BLOCK_VARS (block);
	      BLOCK_VARS (block) = extp;
	    }
	  /* If this is the file scope set DECL_CONTEXT of each decl to
	     the TRANSLATION_UNIT_DECL.  This makes same_translation_unit_p
	     work.  */
	  if (scope == file_scope)
	    {
	      DECL_CONTEXT (p) = context;
	      if (TREE_CODE (p) == TYPE_DECL
		  && TREE_TYPE (p) != error_mark_node)
		set_type_context (TREE_TYPE (p), context);
	    }

	  /* Fall through.  */
	  /* Parameters go in DECL_ARGUMENTS, not BLOCK_VARS, and have
	     already been put there by store_parm_decls.  Unused-
	     parameter warnings are handled by function.c.
	     error_mark_node obviously does not go in BLOCK_VARS and
	     does not get unused-variable warnings.  */
	case PARM_DECL:
	case ERROR_MARK:
	  /* It is possible for a decl not to have a name.  We get
	     here with b->id NULL in this case.  */
	  if (b->id)
	    {
	      gcc_assert (I_SYMBOL_BINDING (b->id) == b);
	      I_SYMBOL_BINDING (b->id) = b->shadowed;
	      if (b->shadowed && b->shadowed->u.type)
		TREE_TYPE (b->shadowed->decl) = b->shadowed->u.type;
	    }
	  break;

	default:
	  gcc_unreachable ();
	}
    }  
#endif
}
#endif


/*
 *  popFunctionScope - pops a binding level, returning the function associated with the
 *                     binding level.
 */

tree
m2block_popFunctionScope (void)
{
  tree fndecl = current_binding_level->fndecl;

#if defined(DEBUGGING)
  if (fndecl != NULL)
    printf("popFunctionScope\n");
#endif

  if (fndecl != NULL && current_binding_level->count>0) {
    /*
     *  multiple pushes have occurred of the same function scope (and ignored),
     *  pop them likewise.
     */
    current_binding_level->count--;
    return fndecl;
  }
  ASSERT_CONDITION (current_binding_level->fndecl != NULL_TREE);   /* expecting local scope */

#if 0  
  ASSERT_CONDITION (current_binding_level->constants == NULL_TREE);   /* should not be used */
  ASSERT_CONDITION (current_binding_level->names == NULL_TREE);       /* should be cleared */
  ASSERT_CONDITION (current_binding_level->decl == NULL_TREE);        /* should be cleared */
#endif

  current_binding_level = current_binding_level->next;
  return fndecl;
}


/*
 *  pushGlobalScope - push the global scope onto the binding level stack.
 *                    There can only ever be one instance of the global binding
 *                    level on the stack.
 */

void
m2block_pushGlobalScope (void)
{
#if defined(DEBUGGING)
  printf("pushGlobalScope\n");
#endif
  m2block_pushFunctionScope (NULL_TREE);
}


/*
 *  popGlobalScope - pops the current binding level, it expects this binding level
 *                   to be the global binding level.
 */

void
m2block_popGlobalScope (void)
{
  ASSERT_CONDITION (current_binding_level->fndecl == NULL_TREE);   /* expecting global scope */
  ASSERT_CONDITION (current_binding_level == global_binding_level);
  current_binding_level = current_binding_level->next;
#if defined(DEBUGGING)
  printf("popGlobalScope\n");
#endif

  assert_global_names ();
}


/*
 *  finishFunctionDecl - removes declarations from the current binding level and places
 *                       them inside fndecl.  The current binding level is then able to
 *                       be destroyed by a call to popFunctionScope.
 *
 *                       The extra tree nodes associated with fndecl will be created
 *                       such as BIND_EXPR, BLOCK and the initial STATEMENT_LIST
 *                       containing the DECL_EXPR is also created.
 */

void
m2block_finishFunctionDecl (tree fndecl)
{
  tree block = DECL_INITIAL (fndecl);
  tree bind_expr = DECL_SAVED_TREE (fndecl);
  tree_stmt_iterator i;

  if (block == NULL_TREE)
    {
      block = make_node (BLOCK);
      DECL_INITIAL (fndecl) = block;
      TREE_USED (block) = TRUE;
      BLOCK_SUBBLOCKS (block) = NULL_TREE;
      BLOCK_SUPERCONTEXT (block) = fndecl;
    }

  BLOCK_VARS (block) = chainon (BLOCK_VARS (block), current_binding_level->names);
  TREE_USED (fndecl) = TRUE;

  if (bind_expr == NULL_TREE)
    DECL_SAVED_TREE (fndecl) = build3 (BIND_EXPR, void_type_node,
				       current_binding_level->names,
				       current_binding_level->decl,
				       block);
  else
    {
      BIND_EXPR_VARS (bind_expr) = chainon (BIND_EXPR_VARS (bind_expr),
					    current_binding_level->names);
      if (current_binding_level->names != NULL_TREE)
	for (i = tsi_start (current_binding_level->names); !tsi_end_p (i); tsi_next (&i))
	  append_to_statement_list_force (*tsi_stmt_ptr (i), &BIND_EXPR_BODY (bind_expr));
    }

  current_binding_level->names = NULL_TREE;
  current_binding_level->decl = NULL_TREE;

  if (BLOCK_SUPERCONTEXT (block) == NULL_TREE)
    {
      if (current_binding_level->fndecl == NULL_TREE)
	BLOCK_SUPERCONTEXT (block) = build_translation_unit_decl (NULL_TREE);
      else
	BLOCK_SUPERCONTEXT (block) = fndecl;
    }
}


/*
 *  finishFunctionCode - adds cur_stmt_list to fndecl.  The current binding level
 *                       is then able to be destroyed by a call to popFunctionScope.
 *                       The cur_stmt_list is appended to the STATEMENT_LIST.
 */

void
m2block_finishFunctionCode (tree fndecl)
{
  tree bind_expr;
  tree block;
  tree statements = m2block_pop_statement_list ();
  tree_stmt_iterator i;

  statements = m2block_end_statement_list (statements);
  ASSERT_CONDITION (DECL_SAVED_TREE (fndecl) != NULL_TREE);

  bind_expr = DECL_SAVED_TREE (fndecl);
  ASSERT_CONDITION (TREE_CODE (bind_expr) == BIND_EXPR);

  block = DECL_INITIAL (fndecl);
  ASSERT_CONDITION (TREE_CODE (block) == BLOCK);

  if (current_binding_level->names != NULL_TREE)
    {
      BIND_EXPR_VARS (bind_expr) = chainon (BIND_EXPR_VARS (bind_expr),
					    current_binding_level->names);
      current_binding_level->names = NULL_TREE;
    }
  if (current_binding_level->labels != NULL_TREE)
    {
      tree t;

      for (t = current_binding_level->labels; t != NULL_TREE; t = TREE_CHAIN (t))
	{
	  tree l = TREE_VALUE (t);

	  BIND_EXPR_VARS (bind_expr) = chainon (BIND_EXPR_VARS (bind_expr), l);
	}
      current_binding_level->labels = NULL_TREE;
    }

  BLOCK_VARS (block) = BIND_EXPR_VARS (bind_expr);

  if (current_binding_level->decl != NULL_TREE)
    for (i = tsi_start (current_binding_level->decl); !tsi_end_p (i); tsi_next (&i))
      append_to_statement_list_force (*tsi_stmt_ptr (i), &BIND_EXPR_BODY (bind_expr));

  for (i = tsi_start (statements); !tsi_end_p (i); tsi_next (&i))
    append_to_statement_list_force (*tsi_stmt_ptr (i), &BIND_EXPR_BODY (bind_expr));
}


static void stop (void) {}

/*
 *  pushDecl - pushes a declaration onto the current binding level.
 */

tree m2block_pushDecl (tree decl)
{
  /* External objects aren't nested, other objects may be.  */

#if 0
  if (TREE_CODE (decl) == VAR_DECL)
    pv(decl);
  else
    pf(decl);
#endif

  if ((DECL_EXTERNAL (decl)) || (decl==current_function_decl))
    DECL_CONTEXT (decl) = NULL_TREE;
  else
    DECL_CONTEXT (decl) = current_function_decl;

  /* Put the declaration on the list.  The list of declarations is in reverse
     order. The list will be reversed later if necessary.  This needs to be
     this way for compatibility with the back-end.  */

  if (current_binding_level->fndecl != NULL)
    stop ();

  TREE_CHAIN (decl) = current_binding_level->names;
  current_binding_level->names = decl;

  /* For the declaration of a type, set its name if it is not already set. */

  if (TREE_CODE (decl) == TYPE_DECL
      && TYPE_NAME (TREE_TYPE (decl)) == 0)
    TYPE_NAME (TREE_TYPE (decl)) = DECL_NAME (decl);

  assert_global_names ();

  return decl;
}


/*
 *  addDeclExpr - adds the DECL_EXPR node, t, to the statement list
 *                current_binding_level->decl.  This allows us to
 *                order all declarations at the beginning of the function.
 */

void m2block_addDeclExpr (tree t)
{
  append_to_statement_list_force (t, &current_binding_level->decl);
}


/*
 *  RememberType - remember the type, t, in the ggc marked list.
 */

tree
m2block_RememberType (tree t)
{
  global_binding_level->types = tree_cons (NULL_TREE,
                                           t, global_binding_level->types);
  return t;
}


/*
 *  global_constant - t is a constant, we keep a chain of all constants
 *                    in the global binding level.
 */

tree
m2block_global_constant (tree t)
{
  tree s;

  if (global_binding_level->constants != NULL_TREE)
    for (s = global_binding_level->constants; s != NULL_TREE; s = TREE_CHAIN (s))
      {
	tree c = TREE_VALUE (s);

	if (c == t)
	  return t;
      }
  
  global_binding_level->constants = tree_cons (NULL_TREE,
					       t, global_binding_level->constants);
  return t;
}


/*
 *  RememberConstant - adds a tree, t, onto the list of constants to be marked
 *                     whenever the ggc re-marks all used storage.  Constants
 *                     live throughout the whole compilation - and they
 *                     can be used by many different functions if necessary.
 */

tree
m2block_RememberConstant (tree t)
{
  if ((t != NULL) && (m2tree_IsAConstant (t)))
    return m2block_global_constant (t);
  return t;
}


/*
 *  DumpGlobalConstants - displays all global constants and checks none are
 *                        poisoned.
 */

tree
m2block_DumpGlobalConstants (void)
{
  tree s;

  if (global_binding_level->constants != NULL_TREE)
    for (s = global_binding_level->constants; TREE_CHAIN (s); s = TREE_CHAIN (s))
      debug_tree (s);
  return NULL_TREE;
}


/*
 *  RememberInitModuleFunction - records tree, t, in the global binding level.
 *                               So that it will not be garbage collected.
 *                               In theory the inner modules could be placed
 *                               inside the current_binding_level I suspect.
 */

tree
m2block_RememberInitModuleFunction (tree t)
{
  global_binding_level->init_functions = tree_cons (NULL_TREE,
                                                    t, global_binding_level->init_functions);
  return t;
}


/*
 *  toplevel - return TRUE if we are in the global scope.
 */

int m2block_toplevel (void)
{
  if (current_binding_level == NULL)
    return TRUE;
  if (current_binding_level->fndecl == NULL)
    return TRUE;
  return FALSE;
}


/*
 *  GetErrorNode - returns the gcc error_mark_node.
 */

tree
m2block_GetErrorNode (void)
{
  return error_mark_node;
}


/*
 *  GetGlobals - returns a list of global variables, functions, constants.
 */

tree
m2block_GetGlobals (void)
{
  assert_global_names ();
  return global_binding_level->names;
}


/*
 *  init - initialise the data structures in this module.
 */

void
m2block_init (void)
{
  free_binding_level = NULL;
  global_binding_level = newLevel ();
  current_binding_level = NULL;
}
