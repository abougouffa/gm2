/* rtegraph.c graph and nodes used by m2rte.

Copyright (C) 2019 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING.  If not,
see <https://www.gnu.org/licenses/>.  */

#include "gm2-gcc/gcc-consolidation.h"

#include "langhooks-def.h" /* FIXME: for lhd_set_decl_assembler_name.  */
#include "tree-pass.h"     /* FIXME: only for PROP_gimple_any.  */
#include "toplev.h"
#include "debug.h"

#include "opts.h"
#include <mpfr.h>

#define RTEGRAPH_C
#include "rtegraph.h"

#undef DEBUGGING
#define DEBUGGING


rtegraph::rtegraph ()
{
}


rtegraph::~rtegraph ()
{
}


rtegraph::rtegraph (const rtegraph &from)
{
  all_rtenodes = from.all_rtenodes;
  candidates = from.candidates;
  externs = from.externs;
  constructors = from.constructors;
}

rtegraph& rtegraph::operator= (const rtegraph &from)
{
  all_rtenodes = from.all_rtenodes;
  candidates = from.candidates;
  externs = from.externs;
  constructors = from.constructors;
  return *this;
}

rtenode *
rtegraph::lookup (gimple *g, tree fndecl, bool is_call)
{
  for (unsigned int i = 0; i < all_rtenodes.length (); i++)
    {
      if (all_rtenodes[i]->grtenode == g
	  && all_rtenodes[i]->func == fndecl
	  && all_rtenodes[i]->is_call == is_call)
	return all_rtenodes[i];
    }
  rtenode *n = new rtenode (g, fndecl, is_call);
  all_rtenodes.safe_push (n);
  return n;
}


void rtegraph::determine_reachable (void)
{
  for (unsigned int i = 0; i < constructors.length (); i++)
    constructors[i]->propagate_constructor_reachable (constructors[i]);
  for (unsigned int i = 0; i < externs.length (); i++)
    externs[i]->propagate_export_reachable (externs[i]);
}


void rtegraph::issue_messages (void)
{
  for (unsigned int i = 0; i < candidates.length (); i++)
    {
      if (candidates[i]->constructor_reachable)
	candidates[i]->error_message ();
      else if (candidates[i]->export_reachable)
	candidates[i]->warning_message ();
      else
	candidates[i]->note_message ();
    }
}


void
rtegraph::dump_vec (const char *title, vec<rtenode *> &list)
{
#if defined (DEBUGGING)
  printf ("%s (length = %d)\n", title, list.length ());
  for (unsigned int i = 0; i < list.length (); i++)
    {
      printf ("[%d]: rtenode %p ", i, list[i]);
      list[i]->dump ();
    }
  printf ("end\n");
#endif
}

void rtegraph::dump (void)
{
#if defined (DEBUGGING)
  dump_vec ("all_rtenodes", all_rtenodes);
  dump_vec ("candidates", candidates);
  dump_vec ("externs", externs);
  dump_vec ("constructors", constructors);
#endif
}


GTY(()) rtegraph *m2rte_graph = NULL;
GTY(()) rtenode *m2rte_current_function_rtenode = NULL;
GTY(()) tree m2rte_current_function = NULL;


rtenode::rtenode ()
: constructor_reachable (false),
  export_reachable (false),
  constructor_final (false),
  export_final (false),
  is_call (false),
  grtenode (NULL),
  func (NULL),
  reachable_src (NULL)
{
}

rtenode::rtenode (gimple *g, tree fndecl, bool is_func_call)
: constructor_reachable (false),
  export_reachable (false),
  constructor_final (false),
  export_final (false),
  is_call (is_func_call),
  grtenode (g),
  func (fndecl),
  reachable_src (NULL)
{
}

rtenode::rtenode (const rtenode &from)
{
  function_call = from.function_call;
  rts_calls = from.rts_calls;

  constructor_reachable = from.constructor_reachable;
  export_reachable = from.export_reachable;
  constructor_final = from.constructor_final;
  export_final = from.export_final;
  is_call = from.is_call;
  grtenode = from.grtenode;
  func = from.func;
  reachable_src = from.reachable_src;
}

rtenode& rtenode::operator= (const rtenode &from)
{
  function_call = from.function_call;
  rts_calls = from.rts_calls;

  constructor_reachable = from.constructor_reachable;
  export_reachable = from.export_reachable;
  constructor_final = from.constructor_final;
  export_final = from.export_final;
  is_call = from.is_call;
  grtenode = from.grtenode;
  func = from.func;
  reachable_src = from.reachable_src;
  return *this;
}


/* rte_error_at - wraps up an error message.  */

static void
rte_error_at (location_t location, diagnostic_t kind, const char *message, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  rich_location richloc (line_table, location);

  va_start (ap, message);
  diagnostic_set_info (&diagnostic, message, &ap, &richloc, kind);
  diagnostic_report_diagnostic (global_dc, &diagnostic);
  va_end (ap);
}

static bool
access_int (tree t, int *value)
{
  enum tree_code code = TREE_CODE (t);

  if (code == SSA_NAME)
    return access_int (SSA_NAME_VAR (t), value);
  if (code == INTEGER_CST)
    {
      *value = TREE_INT_CST_LOW (t);
      return true;
    }
  if ((code == VAR_DECL || code == PARM_DECL)
      && DECL_HAS_VALUE_EXPR_P (t))
    return access_int (DECL_VALUE_EXPR (t), value);
  return false;
}


static bool
access_string (tree t, const char **value)
{
  if (TREE_CODE (t) == ADDR_EXPR)
    {
      if (TREE_CODE (TREE_OPERAND (t, 0)) == STRING_CST)
	{
	  *value = TREE_STRING_POINTER (TREE_OPERAND (t, 0));
	  return true;
	}
    }
  return false;
}


/* generate an error using the parameters of the M2RTS exception handler to
   locate the source code.  We dont use location, as the error_at function will
   give the function context which might be misleading if this is inlined.  */

static void
generate_report (gimple *stmt, const char *report, diagnostic_t kind)
{
  if (gimple_call_num_args (stmt) == 5)
    {
      tree s0 = gimple_call_arg (stmt, 0);
      tree i1 = gimple_call_arg (stmt, 1);
      tree i2 = gimple_call_arg (stmt, 2);
      tree s1 = gimple_call_arg (stmt, 3);
      tree s2 = gimple_call_arg (stmt, 4);
      const char *file;
      int line;
      int col;
      const char *scope;
      const char *message;

      if (access_string (s0, &file)
	  && access_int (i1, &line)
	  && access_int (i2, &col)
	  && access_string (s1, &scope)
	  && access_string (s2, &message))
	{
	  /* continue to use scope as this will survive any
	     optimization transforms.  */
	  location_t location = gimple_location (stmt);
	  rte_error_at (location, kind, "%s, %s (in %s)\n",
			report, message, scope);
	}
    }
}


const char *rtenode::get_func_name (void)
{
  if (func != NULL && (DECL_NAME (func) != NULL))
    return IDENTIFIER_POINTER (DECL_NAME (func));
  return NULL;
}


const char *rtenode::create_message (const char *with_name, const char *without_name)
{
  const char *name = get_func_name ();
  if (name == NULL)
    return without_name;

  int len = strlen (with_name) + 1 + strlen (name);
  char *message = XNEWVEC (char, len);
  snprintf (message, len, with_name, name);
  return message;
}


void rtenode::error_message (void)
{
  if (grtenode != NULL)
    generate_report (grtenode, "runtime error will occur", DK_ERROR);
}


void rtenode::warning_message (void)
{
  const char *message = reachable_src->create_message
    ("runtime error will occur if an exported procedure is called from %s",
     "runtime error will occur if an exported procedure is called");
  if (grtenode != NULL)
    generate_report (grtenode, message, DK_WARNING);
}


void rtenode::note_message (void)
{
  if (grtenode != NULL)
    generate_report (grtenode, "runtime will occur if this procedure is called", DK_NOTE);
}


void
rtenode::dump_vec (const char *title, vec<rtenode *> &list)
{
#if defined (DEBUGGING)
  printf ("  %s (length = %d)\n", title, list.length ());
  for (unsigned int i = 0; i < list.length (); i++)
    printf ("   [%d]: rtenode %p\n", i, list[i]);
#endif
}

void
rtenode::dump (void)
{
#if defined (DEBUGGING)
  printf ("rtenode::dump: ");
  if (func != NULL && (DECL_NAME (func) != NULL))
    {
      const char *n = IDENTIFIER_POINTER (DECL_NAME (func));
      printf ("%s", n);
    }
  if (constructor_reachable)
    printf (", constructor_reachable");
  if (export_reachable)
    printf (", export_reachable");
  if (constructor_final)
    printf (", constructor_final");
  if (export_final)
    printf (", export_final");
  if (is_call)
    printf (", is_call");
  else
    printf (", decl");
  printf (", grtenode %p, func = %p\n", grtenode, func);
  dump_vec ("function_call", function_call);
  dump_vec ("rts_calls", rts_calls);
#endif
}

void rtenode::propagate_constructor_reachable (rtenode *src)
{
  if (constructor_final)
    return;
  constructor_final = true;
  constructor_reachable = true;
  reachable_src = src;
  for (unsigned int i = 0; i < function_call.length (); i++)
    function_call[i]->propagate_constructor_reachable (src);
  for (unsigned int i = 0; i < rts_calls.length (); i++)
    rts_calls[i]->propagate_constructor_reachable (src);
}


void rtenode::propagate_export_reachable (rtenode *src)
{
  if (export_final)
    return;
  export_final = true;
  export_reachable = true;
  reachable_src = src;
  for (unsigned int i = 0; i < function_call.length (); i++)
    function_call[i]->propagate_export_reachable (src);
  for (unsigned int i = 0; i < rts_calls.length (); i++)
    rts_calls[i]->propagate_export_reachable (src);
}

// #include "gt-gm2-rtegraph.h"
