/* Copyright (C) 2007, 2008
 * Free Software Foundation, Inc.
 *
 *  Gaius Mulley <gaius@glam.ac.uk> constructed this file.
 *  It was built by borrowing code from the gcc-.../gcc/c-*.c files
 *  and its function is to aid debugging of the GNU Modula-2 front
 *  end.  It prints the GCC trees in Modula-2 format.
 */

/*
This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
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

#include "gcc-version.h"

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
#include "output.h"
#include "pointer-set.h"

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
#include "real.h"
#include "c-common.h"
#include "tree-iterator.h"
#include "tree-gimple.h"
#include "cgraph.h"

#if defined(GM2)
#  include "gm2-tree.h"
#endif
#if defined(CPP)
#  include "cp-tree.h"
#endif


#define M2PP_C
#include "gm2/m2pp.h"


typedef struct pretty_t {
  int needs_space;
  int needs_indent;
  int curpos;
  int indent;
  int issued_begin;
  int in_vars;
  tree block;
} pretty;

typedef struct stack_t {
  tree value;
  struct stack_t *next;
} stack;


/*
 *  Prototypes
 */

static pretty *initPretty (void);
static pretty *dupPretty (pretty *s);
static int getindent (pretty *s);
static void setindent (pretty *s, int n);
static int getcurpos (pretty *s);
static void m2pp_identifier (pretty *s, tree t);
static void m2pp_needspace (pretty *s);
static void m2pp_function (pretty *s, tree t);
static void m2pp_function_header (pretty *s, tree t);
static void m2pp_function_vars (pretty *s, tree t);
static void m2pp_statement_sequence (pretty *s, tree t);
static void m2pp_print (pretty *s, const char *p);
static void m2pp_print_char (pretty *s, char ch);
static void m2pp_parameter (pretty *s, tree t);
static void m2pp_type (pretty *s, tree t);
static void m2pp_ident_pointer (pretty *s, tree t);
static void m2pp_set_type (pretty *s, tree t);
static void m2pp_enum (pretty *s, tree t);
static void m2pp_array (pretty *s, tree t);
static void m2pp_subrange (pretty *s, tree t);
static void m2pp_gimpified (pretty *s, tree t);
static void m2pp_pointer_type (pretty *s, tree t);
static void m2pp_record_type (pretty *s, tree t);
static void m2pp_simple_type (pretty *s, tree t);
static void m2pp_expression (pretty *s, tree t);
static void m2pp_relop (pretty *s, tree t, const char *p);
static void m2pp_simple_expression (pretty *s, tree t);
static void m2pp_statement_sequence (pretty *s, tree t);
static void m2pp_unknown (pretty *s, const char *s1, const char *s2);
static void m2pp_statement (pretty *s, tree t);
static void m2pp_assignment (pretty *s, tree t);
static void m2pp_designator (pretty *s, tree t);
static void m2pp_conditional (pretty *s, tree t);
static void m2pp_label_expr (pretty *s, tree t);
static void m2pp_label_decl (pretty *s, tree t);
static void m2pp_goto (pretty *s, tree t);
static void m2pp_list (pretty *s, tree t);
static void m2pp_offset (pretty *s, tree t);
static void m2pp_indirect_ref (pretty *s, tree t);
static void m2pp_integer_cst (pretty *s, tree t);
static void m2pp_real_cst (pretty *s, tree t);
static void m2pp_string_cst (pretty *s, tree t);
static void m2pp_addr_expr (pretty *s, tree t);
static void m2pp_nop (pretty *s, tree t);
static void m2pp_convert (pretty *s, tree t);
static void m2pp_var_decl (pretty *s, tree t);
static void m2pp_binary (pretty *s, tree t, const char *p);
static void m2pp_unary (pretty *s, tree t, const char *p);
static void m2pp_call_expr (pretty *s, tree t);
static void m2pp_procedure_call (pretty *s, tree t);
static void m2pp_ssa (pretty *s, tree t);
static void m2pp_block (pretty *s, tree t);
static void m2pp_block_list (pretty *s, tree t);
static void m2pp_var_list (pretty *s, tree t);
static void m2pp_type_list (pretty *s, tree t);
static void m2pp_bind_expr (pretty *s, tree t);
static void m2pp_return_expr (pretty *s, tree t);
static void m2pp_result_decl (pretty *s, tree t);
static void m2pp_try_block (pretty *s, tree t);
static void m2pp_cleanup_point_expr (pretty *s, tree t);
static void m2pp_handler (pretty *s, tree t);
static void m2pp_component_ref (pretty *s, tree t);
static void m2pp_array_ref (pretty *s, tree t);
static void m2pp_begin (pretty *s);
static void m2pp_var (pretty *s);
static void hextree (tree t);
static void m2pp_decl_expr (pretty *s, tree t);
static void m2pp_var_type_decl (pretty *s, tree t);
static void m2pp_non_lvalue_expr (pretty *s, tree t);
static void m2pp_procedure_type (pretty *s, tree t);
static void m2pp_param_type (pretty *s, tree t);
static void m2pp_type_lowlevel (pretty *s, tree t);
static void m2pp_try_catch_expr (pretty *s, tree t);
static void m2pp_throw (pretty *s, tree t);
static void m2pp_catch_expr (pretty *s, tree t);
static void m2pp_try_finally_expr (pretty *s, tree t);
static void m2pp_if_stmt (pretty *s, tree t);
static void killPretty (pretty *s);
static void m2pp_compound_expression (pretty *s, tree t);
static void m2pp_target_expression (pretty *s, tree t);
static void m2pp_array_type (pretty *s, tree t);
static void m2pp_constructor (pretty *s, tree t);
static void push (tree t);
static void pop (void);
static int  begin_printed (tree t);
extern void stop (void);

static stack *stackPtr = NULL;


/*
 *  pf - print function.  Expected to be printed interactively from the
 *       debugger:  print pf(func), or to be called from code.
 */

void
pf (tree t)
{
  pretty *state = initPretty ();

  if (TREE_CODE (t) == FUNCTION_DECL)
    m2pp_function (state, t);
  else
    m2pp_statement_sequence (state, t);
  killPretty (state);
}

/*
 *  pe - print expression.  Expected to be printed interactively from the
 *       debugger:  print pe(expr), or to be called from code.
 */

void
pe (tree t)
{
  pretty *state = initPretty ();

  m2pp_expression (state, t);
  m2pp_needspace (state);
  m2pp_print (state, ";\n");
  killPretty (state);
}

/*
 *  pt - print type.  Expected to be printed interactively from the
 *       debugger:  print pt(expr), or to be called from code.
 */

void
pt (tree t)
{
  pretty *state = initPretty ();
  m2pp_type (state, t);
  m2pp_needspace (state);
  m2pp_print (state, ";\n");
  killPretty (state);
}

/*
 *  ptl - print type low level.  Expected to be printed interactively
 *        from the debugger:  print ptl(type), or to be called from code.
 */

void
ptl (tree t)
{
  pretty *state = initPretty ();
  m2pp_type_lowlevel (state, t);
  m2pp_needspace (state);
  m2pp_print (state, ";\n");
  killPretty (state);
}

void
pv (tree t)
{
  if (t)
    {
      enum tree_code code = TREE_CODE (t);

      if (code == PARM_DECL)
	{
	  pretty *state = initPretty ();
	  m2pp_identifier (state, t);
	  m2pp_needspace (state);
	  m2pp_print (state, "<parm_decl context = ");
	  m2pp_identifier (state, DECL_CONTEXT (t));
	  if (DECL_ABSTRACT_ORIGIN (t) == t)
	    m2pp_print (state, ">\n");
	  else
	    {
	      m2pp_print (state, ", abstract origin = ");
	      m2pp_identifier (state, DECL_ABSTRACT_ORIGIN (t));
	      m2pp_print (state, ">\n");
	      pv (DECL_ABSTRACT_ORIGIN (t));
	    }
	  killPretty (state);
	}
      if (code == VAR_DECL)
	{
	  pretty *state = initPretty ();
	  m2pp_identifier (state, t);
	  m2pp_needspace (state);
	  m2pp_print (state, "<var_decl context = ");
	  m2pp_identifier (state, DECL_CONTEXT (t));
	  if (DECL_ABSTRACT_ORIGIN (t) == t)
	    m2pp_print (state, ">\n");
	  else
	    {
	      m2pp_print (state, ", abstract origin = ");
	      m2pp_identifier (state, DECL_ABSTRACT_ORIGIN (t));
	      m2pp_print (state, ">\n");
	      pv (DECL_ABSTRACT_ORIGIN (t));
	    }
	  killPretty (state);
	}
    }
}

/*
 *  push - pushes tree, t, onto stack.
 */

static void
push (tree t)
{
  stack *s = (stack *)xmalloc (sizeof (stack));

  s->value = t;
  s->next = stackPtr;
  stackPtr = s;
}

/*
 *  pop - pops a tree, from the stack.
 */

static void
pop (void)
{
  stack *s = stackPtr;

  stackPtr = stackPtr->next;
  free (s);
}

/*
 *  being_printed - returns TRUE if, t, is held on the stack.
 */

static int
begin_printed (tree t)
{
  stack *s = stackPtr;

  while (s != NULL) {
    if (s->value == t)
      return TRUE;
    else
      s = s->next;
  }
  return FALSE;
}

/*
 *  dupPretty - duplicate and return a copy of state, s.
 */

static pretty *
dupPretty (pretty *s)
{
  pretty *p = initPretty ();
  *p = *s;
  return p;
}

/*
 *  initPretty - initialise the state of the pretty printer.
 */

static pretty *
initPretty (void)
{
  pretty *state = (pretty *)xmalloc (sizeof (pretty));
  state->needs_space = FALSE;
  state->needs_indent = FALSE;
  state->curpos = 0;
  state->indent = 0;
  state->issued_begin = FALSE;
  state->in_vars = FALSE;
  state->block = NULL_TREE;
  return state;
}

/*
 *  killPretty - cleans up the state.
 */

static void
killPretty (pretty *s)
{
  free (s);
  fflush(stdout);
}

/*
 *  getindent - returns the current indent value.
 */

static int
getindent (pretty *s)
{
  return s->indent;
}

/*
 *  setindent - sets the current indent to, n.
 */

static void
setindent (pretty *s, int n)
{
  s->indent = n;
}

/*
 *  getcurpos - returns the current cursor position.
 */

static int getcurpos (pretty *s)
{
  if (s->needs_space)
    return s->curpos+1;
  else
    return s->curpos;
}

/*
 *  m2pp_type_lowlevel - prints out the low level details of a
 *                       fundamental type.
 */

static void
m2pp_type_lowlevel (pretty *s, tree t)
{
  if (TREE_CODE (t) == INTEGER_TYPE)
    {
      m2pp_print (s, "min");
      m2pp_needspace (s);
      m2pp_integer_cst (s, TYPE_MIN_VALUE (t));
      m2pp_print (s, ", max");
      m2pp_needspace (s);
      m2pp_integer_cst (s, TYPE_MAX_VALUE (t));
      m2pp_print (s, ", type size unit");
      m2pp_needspace (s);
      m2pp_integer_cst (s, TYPE_SIZE_UNIT (t));
      m2pp_print (s, ", type size");
      m2pp_needspace (s);
      m2pp_integer_cst (s, TYPE_SIZE (t));

      printf (", precision %d, mode %d, align %d, user align %d",
	      TYPE_PRECISION (t), TYPE_MODE (t),
	      TYPE_ALIGN (t), TYPE_USER_ALIGN (t));

      m2pp_needspace (s);
      if (TYPE_UNSIGNED (t))
	m2pp_print (s, "unsigned\n");
      else
	m2pp_print (s, "signed\n");
    }
}

/*
 *  m2pp_var - emit a VAR if necessary.
 */

static void
m2pp_var (pretty *s)
{
  if (! s->in_vars)
    {
      s->in_vars = TRUE;
      m2pp_print (s, "VAR\n");
      setindent (s, getindent (s) + 3);
    }
}

/*
 *  hextree - displays the critical fields for function, block and bind_expr
 *            trees in raw hex.
 */

static void
hextree (tree t)
{
  if (t == NULL_TREE)
    return;

  if (TREE_CODE (t) == BLOCK)
    {
      printf("(* BLOCK %p *)\n", t);
      printf("BLOCK_VARS (t) =  %p\n", BLOCK_VARS (t));
      printf("BLOCK_SUPERCONTEXT (t)  =  %p\n", BLOCK_SUPERCONTEXT (t));
    }
  if (TREE_CODE (t) == BIND_EXPR)
    {
      printf("(* BIND_EXPR %p *)\n", t);
      printf("BIND_EXPR_VARS (t) =  %p\n", BIND_EXPR_VARS (t));
      printf("BIND_EXPR_BLOCK (t) =  %p\n", BIND_EXPR_BLOCK (t));
      printf("BIND_EXPR_BODY (t) =  %p\n", BIND_EXPR_BODY (t));
    }
  if (TREE_CODE (t) == FUNCTION_DECL)
    {
      printf("(* FUNCTION_DECL %p *)\n", t);
      printf("DECL_INITIAL (t) =  %p\n", DECL_INITIAL (t));
      printf("DECL_SAVED_TREE (t) = %p\n", DECL_SAVED_TREE (t));
      hextree(DECL_INITIAL (t));
      hextree(DECL_SAVED_TREE (t));
    }
  if (TREE_CODE (t) == VAR_DECL)
    {
      pretty *state = initPretty ();

      printf("(* VAR_DECL %p <", t);
      if (DECL_SEEN_IN_BIND_EXPR_P (t))
	printf("b");
      if (DECL_EXTERNAL (t))
	printf("e");
      if (TREE_STATIC (t))
	printf("s");
      printf("> context = %p*)\n", decl_function_context (t));
      m2pp_type (state, TREE_TYPE (t));
      m2pp_needspace (state);
      m2pp_print (state, ";\n");
      killPretty (state);
    }
  if (TREE_CODE (t) == PARM_DECL)
    {
      pretty *state = initPretty ();

      printf("(* PARM_DECL %p <", t);
      printf("> context = %p*)\n", decl_function_context (t));
      m2pp_type (state, TREE_TYPE (t));
      m2pp_needspace (state);
      m2pp_print (state, ";\n");
      killPretty (state);
    }
}

/*
 *  m2pp_begin - emit a BEGIN if necessary.
 */

static void
m2pp_begin (pretty *s)
{
  if (! s->issued_begin)
    {
      if (s->in_vars)
	{
	  setindent (s, getindent (s) - 3);
	  m2pp_print (s, "BEGIN\n");
	  setindent (s, getindent (s) + 3);
	}
      else
	{
	  m2pp_print (s, "BEGIN\n");
	  setindent (s, getindent (s) + 3);
	}
      s->issued_begin = TRUE;
      s->in_vars = FALSE;
    }
}

/*
 *  m2pp_function - walk over the function.
 */

static void
m2pp_function (pretty *s, tree t)
{
  m2pp_function_header (s, t);
  m2pp_function_vars (s, t);
  m2pp_statement_sequence (s, DECL_SAVED_TREE (t));
  if (TREE_CODE (t) == FUNCTION_DECL)
    {
      m2pp_begin (s);
      setindent (s, getindent (s)-3);
      m2pp_print (s, "END");
      m2pp_needspace (s);
      m2pp_identifier (s, t);
      m2pp_needspace (s);
      m2pp_print (s, ";\n");
    }
}

/*
 *  m2pp_bind_expr - displays the bind expr tree node.
 */

static void
m2pp_bind_expr (pretty *s, tree t)
{
  if (TREE_CODE (t) == BIND_EXPR)
    {
      if (BIND_EXPR_VARS (t))
	{
	  m2pp_print (s, "(* variables in bind_expr *)\n");
	  m2pp_var (s);
	  m2pp_var_list (s, BIND_EXPR_VARS (t));
	}
      if (BIND_EXPR_BLOCK (t))
	{
	  m2pp_print (s, "(* bind_expr_block *)\n");
	  m2pp_statement_sequence (s, BIND_EXPR_BLOCK (t));
	  m2pp_needspace (s);
	  m2pp_print (s, "; \n");
	}
      m2pp_statement_sequence (s, BIND_EXPR_BODY (t));
    }
}

/*
 *  m2pp_block_list - iterates over the list of blocks.
 */

static void
m2pp_block_list (pretty *s, tree t)
{
    for (; t; t = BLOCK_CHAIN (t))
      m2pp_block (s, t);
}

/*
 *  m2pp_block - prints the VARiables and the TYPEs inside a block.
 */

static void
m2pp_block (pretty *s, tree t)
{
  if ((BLOCK_VARS (t) != NULL_TREE) && (s->block != BLOCK_VARS (t)))
    {
      s->block = BLOCK_VARS (t);
      m2pp_print (s, "(* block variables *)\n");
      m2pp_var (s);
      m2pp_var_list (s, BLOCK_VARS (t));
    }
}

/*
 *  m2pp_var_type_decl - displays the variable and type declaration.
 */

static void
m2pp_var_type_decl (pretty *s, tree t)
{
  m2pp_identifier (s, t);
  m2pp_needspace (s);
  m2pp_print (s, ":");
  m2pp_needspace (s);
  m2pp_type (s, TREE_TYPE (t));
  m2pp_needspace (s);
  m2pp_print (s, ";\n");
}

/*
 *  m2pp_var_list - print a variable list.
 */

static void
m2pp_var_list (pretty *s, tree t)
{
  if (t != NULL_TREE)
    for (; t; t = TREE_CHAIN (t)) {
      if (TREE_CODE (t) == FUNCTION_DECL)
	{
	  pretty *p = dupPretty (s);
	  printf("\n");
	  p->in_vars = FALSE;
	  m2pp_function (p, t);
	  killPretty (p);
	  printf("\n");
	}
      else
	m2pp_var_type_decl (s, t);
    }
}

/*
 *  m2pp_type_list - print a variable list.
 */

static void
m2pp_type_list (pretty *s, tree t)
{
  if (t != NULL_TREE)
    for (; t; t = TREE_CHAIN (t))
      {
	m2pp_identifier (s, t);
	m2pp_needspace (s);
	m2pp_print (s, "=");
	m2pp_needspace (s);
	m2pp_type (s, TREE_TYPE (t));
	m2pp_needspace (s);
	m2pp_print (s, ";\n");
      } 
}

/*
 *  m2pp_needspace - sets appropriate flag to TRUE.
 */

static void
m2pp_needspace (pretty *s)
{
  s->needs_space = TRUE;
}

/*
 *  m2pp_identifer - prints an identifier.
 */

static void
m2pp_identifier (pretty *s, tree t)
{
  if (t)
    {
      if (DECL_NAME (t))
	m2pp_ident_pointer (s, DECL_NAME (t));
      else
	{
	  char name[100];

	  if (TREE_CODE (t) == CONST_DECL)
	    snprintf (name, 100, "C_%u", DECL_UID (t));
	  else
	    snprintf (name, 100, "D_%u", DECL_UID (t));
	  m2pp_print (s, name);
	}
    }
}

/*
 *  m2pp_ident_pointer - displays an ident pointer.
 */

static void
m2pp_ident_pointer (pretty *s, tree t)
{
  if (t)
    m2pp_print (s, IDENTIFIER_POINTER (t));
}

/*
 *  m2pp_parameter - prints out a param decl tree.
 */

static void
m2pp_parameter (pretty *s, tree t)
{
  if (TREE_CODE (t) == PARM_DECL)
    {
      if (TREE_TYPE (t) && (TREE_CODE (TREE_TYPE (t)) == REFERENCE_TYPE))
	{
	  m2pp_print (s, "VAR");
	  m2pp_needspace (s);
	  m2pp_identifier (s, t);
	  m2pp_print (s, ":");
	  m2pp_needspace (s);
	  m2pp_simple_type (s, TREE_TYPE (TREE_TYPE (t)));
	}
      else
	{
	  m2pp_identifier (s, t);
	  m2pp_print (s, ":");
	  m2pp_needspace (s);
	  m2pp_simple_type (s, TREE_TYPE (t));
	}
    }
}

/*
 *  m2pp_param_type - prints out the type of parameter.
 */

static void
m2pp_param_type (pretty *s, tree t)
{
  push (t);
  if (t && (TREE_CODE (t) == REFERENCE_TYPE))
    {
      m2pp_print (s, "VAR");
      m2pp_needspace (s);
      m2pp_simple_type (s, TREE_TYPE (t));
    }
  else
    m2pp_simple_type (s, t);
  pop ();
}

/*
 *  m2pp_procedure_type - displays a procedure type.
 */

static void
m2pp_procedure_type (pretty *s, tree t)
{
  push (t);
  if (TREE_CODE (t) == FUNCTION_TYPE)
    {
      tree i = TYPE_ARG_TYPES (t);
      tree returnType = TREE_TYPE (TREE_TYPE (t));

      m2pp_needspace (s);
      m2pp_print (s, "PROCEDURE");
      m2pp_needspace (s);
      if (i != NULL_TREE) {
	int o = getindent (s);
	int p;

	m2pp_print (s, "(");
	p = getcurpos (s);
	setindent (s, p);
	while (i != NULL_TREE) {
	  m2pp_param_type (s, TREE_VALUE (i));
	  i = TREE_CHAIN (i);
	  if (i != NULL_TREE)
	    {
	      m2pp_print (s, ",");
	      m2pp_needspace (s);
	    }
	}
	m2pp_print (s, ")");
	setindent (s, o);
      }
      else if (returnType != NULL_TREE)
	{
	  m2pp_needspace (s);
	  m2pp_print (s, "()");
	}
      if (returnType != NULL_TREE)
	{
	  m2pp_needspace (s);
	  m2pp_print (s, ": ");
	  m2pp_simple_type (s, returnType);
	}
    }
  pop ();
}

/*
 *  m2pp_function_header - displays the function header.
 */

static void
m2pp_function_header (pretty *s, tree t)
{
  push (t);
  if (TREE_CODE (t) == FUNCTION_DECL)
    {
      tree i = DECL_ARGUMENTS (t);
      tree returnType = TREE_TYPE (TREE_TYPE (t));

      m2pp_print(s, "PROCEDURE ");
      m2pp_identifier (s, t);
      m2pp_needspace (s);
      if (i != NULL_TREE) {
	int o = getindent (s);
	int p;

	m2pp_print (s, "(");
	p = getcurpos (s);
	setindent (s, p);
	while (i != NULL_TREE) {
	  m2pp_parameter (s, i);
	  i = TREE_CHAIN (i);
	  if (i != NULL_TREE)
	    m2pp_print (s, ";\n");
	}
	m2pp_print (s, ")");
	m2pp_needspace (s);
	setindent (s, o);
      }
      else if (returnType != NULL_TREE)
	{
	  m2pp_print (s, "()");
	  m2pp_needspace (s);
	}
      if (returnType != NULL_TREE)
	{
	  m2pp_print (s, ": ");
	  m2pp_simple_type (s, returnType);
	  m2pp_needspace (s);
	}
      m2pp_print (s, ";\n");
    }
  pop ();
}

/*
 *  m2pp_add_var - adds a variable into a list as defined by, data.
 */

static tree
m2pp_add_var (tree *tp, int *walk_subtrees, void *data)
{
  tree t = *tp;
  pretty *s = (pretty *)data;
  enum tree_code code = TREE_CODE (t);

  if (code == VAR_DECL)
    {
      m2pp_var (s);
      m2pp_identifier (s, t);
      m2pp_needspace (s);
      m2pp_print (s, ":");
      m2pp_needspace (s);
      m2pp_type (s, TREE_TYPE (t));
      m2pp_needspace (s);
      m2pp_print (s, ";\n");
    }
  if (code == SSA_NAME)
    {
      m2pp_var (s);
      m2pp_ssa(s, t);
      m2pp_identifier (s, SSA_NAME_VAR (t));
      m2pp_needspace (s);
      m2pp_print (s, ":");
      m2pp_needspace (s);
      m2pp_type (s, TREE_TYPE (t));
      m2pp_needspace (s);
      m2pp_print (s, ";\n");
    }
    
  *walk_subtrees = 1;
  return NULL_TREE;
}

/*
 *  m2pp_function_vars - displays variables as defined by the
 *                       function tree.
 */

static void
m2pp_function_vars (pretty *s, tree t)
{
  walk_tree_without_duplicates (&t, m2pp_add_var, s);  

  if (TREE_CODE (t) == FUNCTION_DECL
      && DECL_INITIAL (t))
    {
      m2pp_print (s, "(* variables in function_decl (decl_initial) *)\n");
      m2pp_var (s);
      m2pp_statement_sequence (s, DECL_INITIAL (t));
    }
}

/*
 *  m2pp_print - print out a string, p, interpreting '\n' and
 *               adjusting the fields within, state, s.
 */

static void
m2pp_print (pretty *s, const char *p)
{
  if (p)
    {
      int l = strlen (p);
      int i = 0;

      if (s->needs_space)
	{
	  printf(" ");
	  s->needs_space = FALSE;
	  s->curpos++;
	}

      while (i<l) {
	if (p[i] == '\n')
	  {
	    s->needs_indent = TRUE;
	    s->curpos = 0;
	    printf("\n");
	  }
	else
	  {
	    if (s->needs_indent)
	      {
		if (s->indent > 0)
		  printf("%*c", s->indent, ' ');
		s->needs_indent = FALSE;
		s->curpos += s->indent;
	      }
	    s->curpos++;
	    putchar(p[i]);
	  }
	i++;
      }
    }
}

/*
 *  m2pp_print_char - prints out a character, ch, obeying
 *                    needs_space and needs_indent.
 */

static
void
m2pp_print_char (pretty *s, char ch)
{
  if (s->needs_space)
    {
      printf(" ");
      s->needs_space = FALSE;
      s->curpos++;
    }
  if (s->needs_indent)
    {
      if (s->indent > 0)
	printf("%*c", s->indent, ' ');
      s->needs_indent = FALSE;
      s->curpos += s->indent;
    }
  if (ch == '\n')
    {
      s->curpos++;
      putchar('\\');
      putchar('n');
    }
  else
    putchar(ch);
  s->curpos++;
}

/*
 *  m2pp_type - prints a full type.
 */

void
m2pp_type (pretty *s, tree t)
{
  if (begin_printed (t)) {
    m2pp_print (s, "<...>");
    return;
  }
  m2pp_gimpified (s, t);
  switch (TREE_CODE (t))
    {
    case CHAR_TYPE:
      m2pp_print (s, "CHAR");
      break;
    case INTEGER_TYPE:
      m2pp_print (s, "INTEGER");
      break;
    case REAL_TYPE:
      m2pp_print (s, "REAL");
      break;
    case ENUMERAL_TYPE:
      m2pp_enum (s, t);
      break;
    case UNION_TYPE:
      /* m2pp_union (s, t); */
      /* break; */
    case RECORD_TYPE:
      m2pp_record_type (s, t);
      break;
    case ARRAY_TYPE:
      m2pp_array (s, t);
      break;
#if 0
    case FUNCTION_TYPE:
      m2pp_function_type (s, t);
      break;
#endif
    case TYPE_DECL:
      m2pp_identifier (s, t);
      break;
    case POINTER_TYPE:
      m2pp_pointer_type (s, t);
      break;
#if defined(GM2)
    case SET_TYPE:
      m2pp_set_type (s, t);
      break;
#endif
    case VOID_TYPE:
      m2pp_print (s, "ADDRESS");
      break;
    default:
      m2pp_unknown (s, __FUNCTION__, tree_code_name[TREE_CODE (t)]);
    }
}

/*
 *  m2pp_set_type - prints out the set type.
 */

static void
m2pp_set_type (pretty *s, tree t)
{
  push (t);
  m2pp_print (s, "SET OF");
  m2pp_needspace (s);
  m2pp_type (s, TREE_TYPE (t));
  pop ();
}

/*
 *  m2pp_enum - print out the enumeration type.
 */

static void
m2pp_enum (pretty *s, tree t)
{
  tree chain_p = TYPE_VALUES (t);

  push (t);
  m2pp_print (s, "(");
  while (chain_p)
    {
      m2pp_ident_pointer (s, TREE_PURPOSE (chain_p));
      chain_p = TREE_CHAIN (chain_p);
      if (chain_p)
	m2pp_print (s, ", ");
    }
  m2pp_print (s, ")");
  pop ();
}

/*
 *  m2pp_array - prints out the array type.
 */

static void
m2pp_array (pretty *s, tree t)
{
  push (t);
  m2pp_print (s, "ARRAY");
  m2pp_needspace (s);
  m2pp_subrange (s, TYPE_DOMAIN (t));
  m2pp_needspace (s);
  m2pp_print (s, "OF");
  m2pp_needspace (s);
  m2pp_type (s, TREE_TYPE (t));
  pop ();
}

/*
 *  m2pp_subrange - prints out the subrange, but probably
 *                  the lower bound will always be zero.
 */

static void
m2pp_subrange (pretty *s, tree t)
{
  tree min = TYPE_MIN_VALUE (t);
  tree max = TYPE_MAX_VALUE (t);

  m2pp_print (s, "[");
  m2pp_expression (s, min);
  m2pp_print (s, "..");
  m2pp_expression (s, max);
  m2pp_print (s, "]");
}

/*
 *  m2pp_gimplified - print out a gimplified comment.
 */

static void
m2pp_gimpified (pretty *s, tree t)
{
  if (! TYPE_SIZES_GIMPLIFIED (t))
    {
      m2pp_print (s, "(* <!g> *)");
      m2pp_needspace (s);
    }
}

/*
 *  m2pp_printer_type - display the pointer type.
 */

static void
m2pp_pointer_type (pretty *s, tree t)
{
  push (t);
  if (TREE_CODE (t) == POINTER_TYPE)
    {
      if (TREE_CODE (TREE_TYPE (t)) == FUNCTION_TYPE)
	m2pp_procedure_type (s, TREE_TYPE(t));
      else if (t == ptr_type_node)
	m2pp_print (s, "ADDRESS");
      else
	{
	  m2pp_print (s, "POINTER TO");
	  m2pp_needspace (s);
	  m2pp_type (s, TREE_TYPE (t));
	}
    }
  pop ();
}

/*
 *  m2pp_record_type - displays the record type.
 */

static void
m2pp_record_type (pretty *s, tree t)
{
  push (t);
  if (TREE_CODE (t) == RECORD_TYPE)
    {
      tree i;
      int o = getindent (s);
      int p = getcurpos (s);

      m2pp_print(s, "RECORD\n");
      setindent (s, p+3);
      for (i = TYPE_FIELDS (t); i != NULL_TREE; i = TREE_CHAIN (i))
	{
	  m2pp_identifier (s, i);
	  m2pp_print(s, " : ");
	  m2pp_type (s, TREE_TYPE (i));
	  m2pp_print (s, ";\n");
	}
      setindent (s, p);
      m2pp_print(s, "END");
      setindent (s, o);
    }
  pop ();
}

/*
 *  m2pp_simple_type.
 */

static void
m2pp_simple_type (pretty *s, tree t)
{
  if (begin_printed (t)) {
    m2pp_print (s, "<...>");
    return;
  }

  m2pp_gimpified (s, t);
  switch (TREE_CODE (t))
    {
    case INTEGER_TYPE:
      m2pp_print (s, "INTEGER");
      break;
    case REAL_TYPE:
      m2pp_print (s, "REAL");
      break;
    case CHAR_TYPE:
      m2pp_print (s, "CHAR");
      break;
    case BOOLEAN_TYPE:
      m2pp_print (s, "BOOLEAN");
      break;
    case VOID_TYPE:
      m2pp_print (s, "ADDRESS");
      break;
    case TYPE_DECL:
      m2pp_identifier (s, t);
      break;
    case POINTER_TYPE:
      m2pp_pointer_type (s, t);
      break;
    case RECORD_TYPE:
      m2pp_record_type (s, t);
      break;
    case ENUMERAL_TYPE:
      m2pp_enum (s, t);
      break;
    default:
      m2pp_unknown (s, __FUNCTION__, tree_code_name[TREE_CODE (t)]);
    }
}

/*
 *  m2pp_expression - 
 */

static void
m2pp_expression (pretty *s, tree t)
{
  enum tree_code code = TREE_CODE (t);

  switch (code)
    {
    case EQ_EXPR:
      m2pp_relop (s, t, "=");
      break;
    case NE_EXPR:
      m2pp_relop (s, t, "#");
      break;
    case LE_EXPR:
      m2pp_relop (s, t, "<=");
      break;
    case GE_EXPR:
      m2pp_relop (s, t, ">=");
      break;
    case LT_EXPR:
      m2pp_relop (s, t, "<");
      break;
    case GT_EXPR:
      m2pp_relop (s, t, ">");
      break;
    default:
      m2pp_simple_expression (s, t);
    }
}

/*
 *  m2pp_relop - displays the lhs relop rhs.
 */

static void
m2pp_relop (pretty *s, tree t, const char *p)
{
  m2pp_expression (s, TREE_OPERAND (t, 0));
  m2pp_needspace (s);
  m2pp_print (s, p);
  m2pp_needspace (s);
  m2pp_expression (s, TREE_OPERAND (t, 1));
}

/*
 *  m2pp_compound_expression - handle compound expression tree.
 */

static void
m2pp_compound_expression (pretty *s, tree t)
{
  m2pp_print (s, "compound expression {");
  m2pp_expression (s, TREE_OPERAND (t, 0));
  m2pp_print (s, " (* result ignored *), ");
  m2pp_expression (s, TREE_OPERAND (t, 1));
  m2pp_print (s, "}");
  m2pp_needspace (s);
}

/*
 *  m2pp_target_expression - handle target expression tree.
 */

static void
m2pp_target_expression (pretty *s, tree t)
{
  m2pp_print (s, "{");
  m2pp_needspace (s);
  if (TREE_OPERAND (t, 0) != NULL_TREE) {
    m2pp_print (s, "(* target *) ");
    m2pp_expression (s, TREE_OPERAND (t, 0));
    m2pp_print (s, ",");
    m2pp_needspace (s);
  }
  if (TREE_OPERAND (t, 1) != NULL_TREE) {
    m2pp_print (s, "(* initializer *) ");
    m2pp_expression (s, TREE_OPERAND (t, 1));
    m2pp_print (s, ",");
    m2pp_needspace (s);
  }
  if (TREE_OPERAND (t, 2) != NULL_TREE) {
    m2pp_print (s, "(* cleanup *) ");
    m2pp_expression (s, TREE_OPERAND (t, 2));
    m2pp_print (s, ",");
    m2pp_needspace (s);
  }
  if (TREE_OPERAND (t, 3) != NULL_TREE) {
    m2pp_print (s, "(* saved initializer *) ");
    m2pp_expression (s, TREE_OPERAND (t, 3));
    m2pp_print (s, ",");
    m2pp_needspace (s);
  }
  m2pp_print (s, "}");
  m2pp_needspace (s);
}

/*
 *  m2pp_constructor - print out a constructor.
 */

static void
m2pp_constructor (pretty *s, tree t)
{
  tree purpose, value;
  unsigned HOST_WIDE_INT ix;

  m2pp_print (s, "{ ");
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (t), ix, purpose, value) {
    m2pp_print (s, "(index: ");
    m2pp_simple_expression (s, purpose);
    m2pp_print (s, ") ");
    m2pp_simple_expression (s, value);
    m2pp_print (s, ", ");
  }
  m2pp_print (s, "}");
  m2pp_print (s, "(* type: ");
  setindent (s, getindent (s)+8);
  m2pp_type (s, TREE_TYPE (t));
  setindent (s, getindent (s)-8);
  m2pp_print (s, " *)\n");
}

/*
 *  m2pp_simple_expression - handle GCC expression tree.
 */

static void
m2pp_simple_expression (pretty *s, tree t)
{
  enum tree_code code = TREE_CODE (t);

  switch (code)
    {
    case ERROR_MARK:
      m2pp_print (s, "(* !!! ERROR NODE !!! *)");
      break;
    case CONSTRUCTOR:
      m2pp_constructor (s, t);
      break;
    case IDENTIFIER_NODE:
      m2pp_ident_pointer (s, t);
      break;
    case PARM_DECL:
      m2pp_identifier (s, t);
      break;
    case FIELD_DECL:
      m2pp_identifier (s, t);
      break;
    case TREE_LIST:
      m2pp_list (s, t);
      break;
    case BLOCK:
      m2pp_print (s, "(* BLOCK NODE *)");
      break;
    case OFFSET_TYPE:
      m2pp_offset (s, t);
      break;
    case INTEGER_CST:
      m2pp_integer_cst (s, t);
      break;
    case REAL_CST:
      m2pp_real_cst (s, t);
      break;
    case STRING_CST:
      m2pp_string_cst (s, t);
      break;
    case INDIRECT_REF:
      m2pp_indirect_ref (s, t);
      break;
    case ADDR_EXPR:
      m2pp_addr_expr (s, t);
      break;
    case NOP_EXPR:
      m2pp_nop (s, t);
      break;
    case CONVERT_EXPR:
      m2pp_convert (s, t);
      break;
    case VAR_DECL:
      m2pp_var_decl (s, t);
      break;
    case RESULT_DECL:
      m2pp_result_decl (s, t);
      break;
    case PLUS_EXPR:
      m2pp_binary (s, t, "+");
      break;
    case MINUS_EXPR:
      m2pp_binary (s, t, "+");
      break;
    case MULT_EXPR:
      m2pp_binary (s, t, "*");
      break;
    case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case TRUNC_DIV_EXPR:
    case ROUND_DIV_EXPR:
      m2pp_binary (s, t, "DIV");
      break;
    case FLOOR_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case TRUNC_MOD_EXPR:
    case ROUND_MOD_EXPR:
      m2pp_binary (s, t, "MOD");
      break;
    case NEGATE_EXPR:
      m2pp_unary (s, t, "-");
      break;
    case CALL_EXPR:
      m2pp_call_expr (s, t);
      break;
    case SSA_NAME:
      m2pp_ssa (s, t);
      break;
    case COMPONENT_REF:
      m2pp_component_ref (s, t);
      break;
    case RETURN_EXPR:
      m2pp_return_expr (s, t);
      break;
    case ARRAY_REF:
      m2pp_array_ref (s, t);
      break;
    case NON_LVALUE_EXPR:
      m2pp_non_lvalue_expr (s, t);
      break;
    case EXPR_STMT:
      m2pp_expression (s, EXPR_STMT_EXPR (t));
      break;
    case EXC_PTR_EXPR:
      m2pp_print (s, "GCC_EXCEPTION_OBJECT");
      break;
    case INIT_EXPR:
    case MODIFY_EXPR:
      m2pp_assignment (s, t);
      break;
    case COMPOUND_EXPR:
      m2pp_compound_expression (s, t);
      break;
    case TARGET_EXPR:
      m2pp_target_expression (s, t);
      break;
    case THROW_EXPR:
      m2pp_throw (s, t);
      break;
    case FUNCTION_DECL:
      m2pp_identifier (s, t);
      break;
    default:
      m2pp_unknown (s, __FUNCTION__, tree_code_name[code]);
    }
}

/*
 *  non_lvalue_expr - indicates that operand 0 is not an lvalue.
 */

static void
m2pp_non_lvalue_expr (pretty *s, tree t)
{
  m2pp_needspace (s);
  m2pp_print (s, "assert_non_lvalue(");
  m2pp_needspace (s);
  m2pp_expression (s, TREE_OPERAND (t, 0));
  m2pp_needspace (s);
  m2pp_print (s, ")");
}

/*
 *  m2pp_array_ref - prints out the array reference.
 */

static void
m2pp_array_ref (pretty *s, tree t)
{
  m2pp_expression (s, TREE_OPERAND (t, 0));
  m2pp_print (s, "[");
  m2pp_expression (s, TREE_OPERAND (t, 1));
  m2pp_print (s, "]");
}

/*
 *  m2pp_ssa - prints out the ssa variable name.
 */

static void
m2pp_ssa (pretty *s, tree t)
{
  m2pp_identifier (s, SSA_NAME_VAR (t));
}

/*
 *  m2pp_binary - print the binary operator, p, and lhs, rhs.
 */

static void
m2pp_binary (pretty *s, tree t, const char *p)
{
  tree left = TREE_OPERAND (t, 0);
  tree right = TREE_OPERAND (t, 1);

  m2pp_expression (s, left);
  m2pp_needspace (s);
  m2pp_print (s, p);
  m2pp_needspace (s);
  m2pp_expression (s, right);
}

/*
 *  m2pp_unary - print the unary operator, p, and expression.
 */

static void
m2pp_unary (pretty *s, tree t, const char *p)
{
  tree expr = TREE_OPERAND (t, 0);

  m2pp_needspace (s);
  m2pp_print (s, p);
  m2pp_expression (s, expr);
}

/*
 *  m2pp_integer_cst - displays the integer constant.
 */

static void
m2pp_integer_cst (pretty *s, tree t)
{
  char val[100];

  snprintf(val, 100, "%lld", TREE_INT_CST_LOW (t));
  m2pp_print (s, val);
}

/*
 *  m2pp_real_cst - displays the real constant.
 */

static void
m2pp_real_cst (pretty *s, tree t ATTRIBUTE_UNUSED)
{
  m2pp_print (s, "<unknown real>");
}

/*
 *  m2pp_string_cst - displays the real constant.
 */

static void
m2pp_string_cst (pretty *s, tree t)
{
  const char *p = TREE_STRING_POINTER (t);
  int i=0;
  
  m2pp_print (s, "\"");
  while (p[i] != '\0') {
    m2pp_print_char (s, p[i]);
    i++;
  }
  m2pp_print (s, "\"");
}

/*
 *  m2pp_statement_sequence - iterates over a statement list displaying
 *                            each statement in turn.
 */

static void
m2pp_statement_sequence (pretty *s, tree t)
{
  if (t != NULL_TREE)
    {
      if (TREE_CODE (t) == STATEMENT_LIST)
	{
	  tree_stmt_iterator i;
	  m2pp_print (s, "(* statement list *)\n");

	  for (i = tsi_start (t); !tsi_end_p (i); tsi_next (&i))
	    m2pp_statement (s, *tsi_stmt_ptr (i));
	}
      else
	m2pp_statement (s, t);
    }
}

/*
 *  m2pp_unknown - displays an error message.
 */

static void
m2pp_unknown (pretty *s, const char *s1, const char *s2)
{
  m2pp_begin (s);
  m2pp_print (s, s1);
  m2pp_needspace (s);
  m2pp_print (s, s2);
  m2pp_needspace (s);
}

/*
 *  m2pp_throw - displays a throw statement.
 */

static void
m2pp_throw (pretty *s, tree t)
{
  tree expr = TREE_OPERAND (t, 0);

  if (expr == NULL_TREE)
    m2pp_print (s, "THROW ;\n");
  else
    {
      m2pp_print (s, "THROW (");
      m2pp_expression (s, TREE_OPERAND (t, 0));
      m2pp_print (s, ")\n");
    }
}

/*
 *  m2pp_catch_expr - attempts to reconstruct a catch expr.
 */

static void
m2pp_catch_expr (pretty *s, tree t)
{
  tree types = CATCH_TYPES (t);
  tree body = CATCH_BODY (t);

  m2pp_print (s, "(* CATCH expression ");
  if (types != NULL_TREE)
    {
      m2pp_print (s, "(");
      m2pp_expression (s, types);
      m2pp_print (s, ")");
    }
  m2pp_print (s, "*)\n");
  m2pp_print (s, "(* catch body *)\n");
  m2pp_statement_sequence (s, body);
  m2pp_print (s, "(* end catch body *)\n");
}

/*
 *  m2pp_try_finally_expr - attemts to reconstruct a try finally expr.
 */

static void
m2pp_try_finally_expr (pretty *s, tree t)
{
  m2pp_begin (s);
  m2pp_print (s, "(* try_finally_expr *)\n");
  setindent (s, getindent (s)+3);
  m2pp_statement_sequence (s, TREE_OPERAND (t, 0));
  setindent (s, getindent (s)-3);
  m2pp_print (s, "(* finally (cleanup which is executed after the above) *)\n");
  setindent (s, getindent (s)+3);
  m2pp_statement_sequence (s, TREE_OPERAND (t, 1));
  setindent (s, getindent (s)-3);
  m2pp_print (s, "(* end try_finally_expr *)\n");
}

/*
 *  m2pp_if_stmt - pretty print a C++ if_stmt.
 */

static void
m2pp_if_stmt (pretty *s, tree t)
{
  m2pp_print (s, "(* only C++ uses if_stmt nodes *)\n");
  m2pp_print (s, "IF ");
  m2pp_expression (s, TREE_OPERAND (t, 0));
  m2pp_print (s, "\n");
  m2pp_print (s, "THEN\n");
  setindent (s, getindent (s)+3);
  m2pp_statement_sequence (s, TREE_OPERAND (t, 1));
  setindent (s, getindent (s)-3);
  m2pp_print (s, "ELSE\n");
  setindent (s, getindent (s)+3);
  m2pp_statement_sequence (s, TREE_OPERAND (t, 2));
  setindent (s, getindent (s)-3);
  m2pp_print (s, "END\n");
}

/*
 *  m2pp_statement - attempts to reconstruct a statement.
 */

static void
m2pp_statement (pretty *s, tree t)
{
  enum tree_code code = TREE_CODE (t);
  
  switch (code)
    {
    case COND_EXPR:
      m2pp_conditional (s, t);
      break;
    case LABEL_EXPR:
      m2pp_label_expr (s, t);
      break;
    case LABEL_DECL:
      m2pp_label_decl (s, t);
      break;
    case GOTO_EXPR:
      m2pp_goto (s, t);
      break;
    case INIT_EXPR:
    case MODIFY_EXPR:
      m2pp_assignment (s, t);
      break;
    case CALL_EXPR:
      m2pp_procedure_call (s, t);
      break;
    case BLOCK:
      m2pp_block_list (s, t);
      break;
    case BIND_EXPR:
      m2pp_bind_expr (s, t);
      break;
    case RETURN_EXPR:
      m2pp_return_expr (s, t);
      break;
    case DECL_EXPR:
      m2pp_decl_expr (s, t);
      break;
    case TRY_BLOCK:
      m2pp_try_block (s, t);
      break;
    case HANDLER:
      m2pp_handler (s, t);
      break;
    case CLEANUP_POINT_EXPR:
      m2pp_cleanup_point_expr (s, t);
      break;
    case THROW_EXPR:
      m2pp_throw (s, t);
      break;
    case TRY_CATCH_EXPR:
      m2pp_try_catch_expr (s, t);
      break;
    case TRY_FINALLY_EXPR:
      m2pp_try_finally_expr (s, t);
      break;
    case CATCH_EXPR:
      m2pp_catch_expr (s, t);
      break;
#if defined(CPP)
    case IF_STMT:
      m2pp_if_stmt (s, t);
      break;
#endif
    default:
      m2pp_unknown (s, __FUNCTION__, tree_code_name[TREE_CODE (t)]);
    }
}

/*
 *  m2pp_try_catch_expr - is used after gimplification.
 */

static void
m2pp_try_catch_expr (pretty *s, tree t)
{
  m2pp_print (s, "(* try_catch_expr (post gimplification) *)\n");
  m2pp_statement_sequence (s, TREE_OPERAND (t, 0));
  setindent (s, 0);
  m2pp_print (s, "EXCEPT\n");
  setindent (s, 3);
  m2pp_statement_sequence (s, TREE_OPERAND (t, 1));
  m2pp_print (s, "(* end of try_catch_expr (post gimplification) *)\n");
}

/*
 *  m2pp_cleanup_point_expr - emits a comment indicating
 *                            a GCC cleanup_point_expr is present.
 */

static void
m2pp_cleanup_point_expr (pretty *s, tree t)
{
  m2pp_print (s, "(* cleanup point,  expression = ");
  m2pp_expression (s, TREE_OPERAND (t, 0));
  m2pp_print (s, " *)\n");
}

/*
 *  m2pp_decl_expr - displays a local declaration.
 */

static void
m2pp_decl_expr (pretty *s, tree t)
{
  m2pp_var (s);
  m2pp_print (s, "(* variable in decl_expr *)\n");
  m2pp_var_type_decl (s, DECL_EXPR_DECL (t));
}

/*
 *  m2pp_procedure_call - print a call to a procedure.
 */

static void
m2pp_procedure_call (pretty *s, tree t)
{
  m2pp_begin (s);
  m2pp_call_expr (s, t);
  m2pp_needspace (s);
  m2pp_print (s, ";\n");
}

/*
 *  m2pp_call_expr - print a call to a procedure or function.
 */

static void
m2pp_call_expr (pretty *s, tree t)
{
  tree call = TREE_OPERAND (t, 0);
  tree args = TREE_OPERAND (t, 1);
  tree type = TREE_TYPE (t);
  int has_return_type = TRUE;
  tree proc;

  if (type && (TREE_CODE (type) == VOID_TYPE))
    has_return_type = FALSE;

  if (TREE_CODE (call) == ADDR_EXPR || TREE_CODE (call) == NON_LVALUE_EXPR)
    proc = TREE_OPERAND (call, 0);
  else
    proc = call;

  m2pp_identifier (s, proc);
  if (args || has_return_type)
    m2pp_list (s, args);
}

/*
 *  m2pp_return_expr - displays the return statement.
 */

static void
m2pp_return_expr (pretty *s, tree t)
{
  tree e = TREE_OPERAND (t, 0);

  m2pp_begin (s);
  if (e == NULL_TREE)
    {
      m2pp_print (s, "RETURN");
    }
  else if (TREE_CODE (e) == MODIFY_EXPR ||
	   (TREE_CODE (e) == INIT_EXPR))
    {
      m2pp_assignment (s, e);
      m2pp_print (s, "RETURN");
      m2pp_needspace (s);
      m2pp_expression (s, TREE_OPERAND (e, 0));
    }
  else
    {
      m2pp_print (s, "RETURN");
      m2pp_needspace (s);
      m2pp_expression (s, e);
    }
  m2pp_needspace (s);
  m2pp_print (s, ";\n");
}

/*
 *  m2pp_try_block - displays the try block.
 */

static void
m2pp_try_block (pretty *s, tree t)
{
  tree stmts = TRY_STMTS (t);
  tree handlers = TRY_HANDLERS (t);

  m2pp_begin (s);
  m2pp_print (s, "(* TRY *)\n");
  m2pp_statement_sequence (s, stmts);
  setindent (s, 0);
  m2pp_print (s, "EXCEPT\n");
  setindent (s, 3);
  m2pp_statement_sequence (s, handlers);
  m2pp_print (s, "(* END TRY *)\n");
}

/*
 *  m2pp_try_block - displays the handler block.
 */

static void
m2pp_handler (pretty *s, tree t)
{
  tree parms = HANDLER_PARMS (t);
  tree body = HANDLER_BODY (t);
  tree type = HANDLER_TYPE (t);

  m2pp_print (s, "(* handler *)\n");
  if (parms != NULL_TREE)
    {
      m2pp_print (s, "(* handler parameter has a type (should be NULL_TREE) in Modula-2 *)\n");
      m2pp_print (s, "CATCH (");
      m2pp_expression (s, parms);
      m2pp_print (s, ")\n");
    }
  if (type != NULL_TREE)
    m2pp_print (s, "(* handler type (should be NULL_TREE) in Modula-2 *)\n");
  m2pp_statement_sequence (s, body);
}

/*
 *  m2pp_assignment - prints out the assignment statement.
 */

static void
m2pp_assignment (pretty *s, tree t)
{
  int o;

  m2pp_begin (s);
  m2pp_designator (s, TREE_OPERAND (t, 0));
  m2pp_needspace (s);
  m2pp_print (s, ":=") ;
  m2pp_needspace (s);
  o = getindent (s);
  setindent (s, getcurpos (s)+1);
  m2pp_expression (s, TREE_OPERAND (t, 1));
  m2pp_needspace (s);
  m2pp_print (s, ";\n") ;
  setindent (s, o);
}

/*
 *  m2pp_designator - displays the lhs of an assignment.
 */

static void
m2pp_designator (pretty *s, tree t)
{
  m2pp_expression (s, t);
}

/*
 *  m2pp_indirect_ref - displays the indirect operator.
 */

static void
m2pp_indirect_ref (pretty *s, tree t)
{
  m2pp_print (s, "(");
  m2pp_expression (s, TREE_OPERAND (t, 0));
  m2pp_print (s, ")^");
}

/*
 *  m2pp_conditional - builds an IF THEN ELSE END.
 *                     With more work this should be moved into statement
 *                     sequence which could look for repeat and while loops.
 */

static void
m2pp_conditional (pretty *s, tree t)
{
  int o;

  m2pp_begin (s);
  m2pp_print (s, "IF");
  m2pp_needspace (s);
  m2pp_expression (s, TREE_OPERAND (t, 0));
  m2pp_print (s, "\nTHEN\n");
  o = getindent (s);
  setindent (s, o+3);
  m2pp_statement_sequence (s, TREE_OPERAND (t, 1));
  setindent (s, o);
  if (TREE_OPERAND (t, 2) != NULL_TREE)
    {
      m2pp_print (s, "ELSE\n");
      setindent (s, o+3);
      m2pp_statement_sequence (s, TREE_OPERAND (t, 2));
      setindent (s, o);
    }
  m2pp_print (s, "END ;\n");
}

/*
 *  m2pp_label_decl - displays a label.  Again should be moved into statement
 *                    sequence to determine proper loop constructs.
 */

static void
m2pp_label_decl (pretty *s, tree t)
{
  m2pp_begin (s);
  m2pp_print (s, "(* label  ");
  m2pp_identifier (s, t);
  m2pp_print (s, ": *)\n");
}

/*
 *  m2pp_label_expr - skips the LABEL_EXPR to find the LABEL_DECL.
 */

static void
m2pp_label_expr (pretty *s, tree t)
{
  m2pp_begin (s);
  m2pp_statement (s, TREE_OPERAND (t, 0));
}

/*
 *  m2pp_goto - displays a goto statement.  Again should be moved
 *              into statement sequence to determine proper loop
 *              constructs.
 */

static void
m2pp_goto (pretty *s, tree t)
{
  m2pp_begin (s);
  m2pp_print (s, "(* goto ");
  m2pp_identifier (s, TREE_OPERAND (t, 0));
  m2pp_print (s, " *)\n");
}

/*
 *  m2pp_list - prints a TREE_CHAINed list.
 */

static void
m2pp_list (pretty *s, tree t)
{
  tree u = t;

  m2pp_print (s, "(");
  m2pp_needspace (s);
  while (t != NULL_TREE)
    {
      m2pp_expression (s, TREE_VALUE (t));
      t = TREE_CHAIN (t);
      if (t == u || t == NULL_TREE)
	break;
      m2pp_print (s, ",");
      m2pp_needspace (s);
    }
  m2pp_needspace (s);
  m2pp_print (s, ")");
}

/*
 *  m2pp_offset - displays the offset operator.
 */

static void
m2pp_offset (pretty *s, tree t)
{
  tree type = TREE_TYPE (t);
  tree base = TYPE_OFFSET_BASETYPE (t);

  m2pp_print (s, "OFFSET (");
  m2pp_type (s, base);
  m2pp_print (s, ".");
  m2pp_type (s, type);
  m2pp_print (s, ")");
}

/*
 *  m2pp_addr_expr -
 */

static void
m2pp_addr_expr (pretty *s, tree t)
{
  m2pp_needspace (s);
  m2pp_print (s, "ADR (");
  m2pp_expression (s, TREE_OPERAND (t, 0));
  m2pp_print (s, ")");
}

/*
 *  m2pp_nop -
 */

static void
m2pp_nop (pretty *s, tree t)
{
  m2pp_needspace (s);
  m2pp_print (s, "CAST (");
  m2pp_simple_type (s, TREE_TYPE (t));
  m2pp_print (s, ", ");
  m2pp_expression (s, TREE_OPERAND (t, 0));
  m2pp_print (s, ")");  
}

/*
 *  m2pp_convert -
 */

static void
m2pp_convert (pretty *s, tree t)
{
  m2pp_needspace (s);
  m2pp_print (s, "CONVERT (");
  m2pp_simple_type (s, TREE_TYPE (t));
  m2pp_print (s, ", ");
  m2pp_expression (s, TREE_OPERAND (t, 0));
  m2pp_print (s, ")");  
}

/*
 *  m2pp_var_decl -
 */

static void
m2pp_var_decl (pretty *s, tree t)
{
  m2pp_identifier (s, t);
}

/*
 *  m2pp_result_decl -
 */

static void
m2pp_result_decl (pretty *s, tree t)
{
  m2pp_identifier (s, t);
}

/*
 *  m2pp_component_ref -
 */

static void
m2pp_component_ref (pretty *s, tree t)
{
  m2pp_simple_expression (s, TREE_OPERAND (t, 0));
  m2pp_print (s, ".");
  m2pp_simple_expression (s, TREE_OPERAND (t, 1));
}

