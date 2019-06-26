/* m2rte.c a plugin to detect runtime exceptions at compiletime.

Copyright (C) 2017-2019 Free Software Foundation, Inc.
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


#include "gcc-plugin.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "alias.h"
#include "predict.h"
#include "tm.h"
#include "tree.h"
#include "stringpool.h"
#include "toplev.h"
#include "basic-block.h"
#include "hash-table.h"
#include "vec.h"
#include "ggc.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimple-pretty-print.h"
#include "tree.h"
#include "tree-pass.h"
#include "intl.h"
#include "plugin-version.h"
#include "diagnostic.h"
#include "context.h"


#define DEVELOPMENT
#undef DEVELOPMENT


int plugin_is_GPL_compatible;

void debug_tree (tree);

#if 0
class m2node
{
 private:

 public:
  bool error_reachable;
  bool warning_reachable;
  vec<m2node> edge;
  gimple *node;

  m2node (gimple *g);
  ~m2node (void);
  m2node (const dlist &from);
}


class m2graph
{
 private:
  vec<m2node> node;
  m2node *lookup_node (gimple *g);
  void walk (bool error, bool warning);

 public:
  m2graph (void);
  ~m2graph (void);
  m2graph (const dlist &from);

  void add_edge (gimple *src, gimple *dest);
  void set_reachable (gimple *g);
  void determine_reachable (void);
  void issue_messages (void);
}


m2graph::m2graph ()
{
}

m2graph::~m2graph ()
{
}

void
m2graph::add_edge (gimple *src, gimple *src)
{
  m2node *gsrc = lookup_node (src);
  m2node *gdest = lookup_node (dest);

  gsrc->edge.safe_push (gdest);
}

m2node *
m2graph::lookup_node (gimple *g)
{
  for (int i = 0; i < edge.length (); i++)
    {
      if (edge[i].node == g)
	return edge[i];
    }
  m2node *n = new m2node (g);
  node.safe_push (n);
  return n;
}

#endif


static void
pretty_function (tree fndecl)
{
  if (fndecl != NULL && (DECL_NAME (fndecl) != NULL))
    {
      const char *n = IDENTIFIER_POINTER (DECL_NAME (fndecl));
      fprintf (stderr, "%s\n", n);
    }
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


void
print_rtl (FILE *outf, const_rtx rtx_first);

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
#if 0
  if (code == PARM_DECL)
    if (DECL_INCOMING_RTL (t) != 0)
      print_rtl (stdout, DECL_INCOMING_RTL (t));
  if ((code == VAR_DECL || code == PARM_DECL || code == RESULT_DECL)
      && DECL_BY_REFERENCE (t))
    fprintf (stderr, "passed by ref\n");
  fprintf (stderr, "failed to find int\n");
#endif

  return false;
}


/*  rte_error_at - wraps up an error message, --fixme-- this needs to turn
    off the 'In function' component of the error message as it can be misleading
    after optimization.  Is this still true?  */

static void
rte_error_at (location_t location, const char *message, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  rich_location richloc (line_table, location);

  va_start (ap, message);
  diagnostic_set_info (&diagnostic, message, &ap, &richloc, DK_WARNING);
  diagnostic_report_diagnostic (global_dc, &diagnostic);
  va_end (ap);
}

/* generate an error using the parameters of the M2RTS exception handler to
   locate the source code.  We dont use location, as the error_at function will
   give the function context which might be misleading if this is inlined.  */

static void
generate_error (gimple *stmt)
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
	  rte_error_at (location, "runtime error will occur, %s (in %s)\n",
			message, scope);
	}
    }
}


static void
examine_call (gimple *stmt)
{
  tree fndecl = gimple_call_fndecl (stmt);
  if (fndecl != NULL && (DECL_NAME (fndecl) != NULL))
    {
      const char *n = IDENTIFIER_POINTER (DECL_NAME (fndecl));
      if (strcmp (n, "M2RTS_AssignmentException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_ReturnException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_IncException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_DecException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_InclException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_ExclException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_ShiftException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_RotateException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_StaticArraySubscriptException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_DynamicArraySubscriptException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_ForLoopBeginException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_ForLoopToException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_ForLoopEndException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_PointerNilException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_NoReturnException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_CaseException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_WholeNonPosDivException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_WholeNonPosModException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_WholeZeroDivException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_WholeZeroRemException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_WholeValueException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_RealValueException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_ParameterException") == 0)
	generate_error (stmt);
      else if (strcmp (n, "M2RTS_NoException") == 0)
	generate_error (stmt);
    }
}

/* Check and warn if STMT is a self-assign statement.  */

static void
runtime_exception_inevitable (gimple *stmt)
{
  if (is_gimple_call (stmt))
    examine_call (stmt);
}


namespace {

const pass_data pass_data_exception_detection =
{
  GIMPLE_PASS, /* type */
  "runtime_exception_inevitable", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_gimple_lcf , /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_warn_exception_inevitable : public gimple_opt_pass
{
public:
  pass_warn_exception_inevitable(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_exception_detection, ctxt)
  {}

  /* opt_pass methods: */
  // opt_pass * clone () { return new pass_warn_exception_inevitable (m_ctxt); }
  virtual unsigned int execute (function *);

};

unsigned int
pass_warn_exception_inevitable::execute (function *fun)
{
  gimple_stmt_iterator gsi;
  basic_block bb;

#if defined (DEVELOPMENT)
  fprintf (stderr, "function ");
  pretty_function (fun->decl);
  fprintf (stderr, "{\n");
  int count = 0;
#endif
  FOR_EACH_BB_FN (bb, fun)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
        runtime_exception_inevitable (gsi_stmt (gsi));
      /*  we only care about the first basic block in each function.  */
#if defined (DEVELOPMENT)
      if (count == 0)
	fprintf (stderr, "/* end of first bb in function.  */\n\n");
      else
	fprintf (stderr, "/* end of bb.  */\n\n");
      count++;
#endif
      return 0;
    }
#if defined (DEVELOPMENT)
  fprintf (stderr, "}\n\n");
#endif

  return 0;
}

} // anon namespace


static gimple_opt_pass *
make_pass_warn_exception_inevitable (gcc::context *ctxt)
{
  return new pass_warn_exception_inevitable (ctxt);
}


/* plugin_init, check the version and register the plugin.  */

int
plugin_init (struct plugin_name_args *plugin_info,
	     struct plugin_gcc_version *version)
{
  struct register_pass_info pass_info;
  const char *plugin_name = plugin_info->base_name;
  int argc = plugin_info->argc;
  struct plugin_argument *argv = plugin_info->argv;

  if (!plugin_default_version_check (version, &gcc_version))
    {
      fprintf (stderr, "incorrect GCC version (%s) this plugin was built for GCC version %s\n",
	       version->basever, gcc_version.basever);
      return 1;
    }

  /* runtime exception inevitable detection.  This plugin is most effective if
     it is run after after all optimizations.  This is plugged in at the end of
     gimple range of optimizations.  */
  pass_info.pass = make_pass_warn_exception_inevitable (g);
  pass_info.reference_pass_name = "*warn_function_noreturn";

  pass_info.ref_pass_instance_number = 1;
  pass_info.pos_op = PASS_POS_INSERT_AFTER;

  register_callback (plugin_name,
		     PLUGIN_PASS_MANAGER_SETUP,
		     NULL,
		     &pass_info);

#if defined (DEVELOPMENT)
  fprintf (stderr, "m2rte installed\n\n");
#endif
  return 0;
}
