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

#include "rtegraph.h"
extern bool ggc_force_collect;
extern void ggc_collect (void);

#define DEVELOPMENT
#undef DEVELOPMENT

#undef DEBUGGING
#define DEBUGGING


int plugin_is_GPL_compatible;

void debug_tree (tree);

static const char *m2_runtime_error_calls[] = {
  "M2RTS_AssignmentException",
  "M2RTS_ReturnException",
  "M2RTS_IncException",
  "M2RTS_DecException",
  "M2RTS_InclException",
  "M2RTS_ExclException",
  "M2RTS_ShiftException",
  "M2RTS_RotateException",
  "M2RTS_StaticArraySubscriptException",
  "M2RTS_DynamicArraySubscriptException",
  "M2RTS_ForLoopBeginException",
  "M2RTS_ForLoopToException",
  "M2RTS_ForLoopEndException",
  "M2RTS_PointerNilException",
  "M2RTS_NoReturnException",
  "M2RTS_CaseException",
  "M2RTS_WholeNonPosDivException",
  "M2RTS_WholeNonPosModException",
  "M2RTS_WholeZeroDivException",
  "M2RTS_WholeZeroRemException",
  "M2RTS_WholeValueException",
  "M2RTS_RealValueException",
  "M2RTS_ParameterException",
  "M2RTS_NoException",
  NULL,
};


#if defined(DEVELOPMENT)
static void
pretty_function (tree fndecl)
{
  if (fndecl != NULL && (DECL_NAME (fndecl) != NULL))
    {
      const char *n = IDENTIFIER_POINTER (DECL_NAME (fndecl));
      fprintf (stderr, "%s\n", n);
    }
}
#endif


void
print_rtl (FILE *outf, const_rtx rtx_first);

static bool
strend (const char *name, const char *ending)
{
  unsigned int len = strlen (name);
  return (len > strlen (ending)
	  && (strcmp (&name[len-strlen (ending)], ending) == 0));
}


static bool
is_constructor (tree fndecl)
{
  const char *name = IDENTIFIER_POINTER (DECL_NAME (fndecl));
  unsigned int len = strlen (name);

  return ((len > strlen ("_M2_"))
	  && (strncmp (name, "_M2_", strlen ("_M2_")) == 0)
	  && (strend (name, "_init") || strend (name, "_finish")));
}


static bool
is_external (tree function)
{
  return (! DECL_EXTERNAL (function))
    && TREE_PUBLIC (function)
    && TREE_STATIC (function);
}


static bool
is_rte (tree fndecl)
{
  const char *n = IDENTIFIER_POINTER (DECL_NAME (fndecl));

  for (int i = 0; m2_runtime_error_calls[i] != NULL; i++)
    if (strcmp (m2_runtime_error_calls[i], n) == 0)
      return true;
  return false;
}


static void
examine_call (gimple *stmt)
{
  tree fndecl = gimple_call_fndecl (stmt);
  rtenode *func = rtegraph_lookup (stmt, fndecl, true);
  // rtegraph_dump ();
  if (fndecl != NULL && (DECL_NAME (fndecl) != NULL))
    {
      /* firstly check if the function is a runtime exception.  */
      if (is_rte (fndecl))
	{
	  /* remember runtime exception call.  */
	  rtegraph_include_rtscall (func);
	  /* add the callee to the list of candidates to be queried reachable.  */
	  rtegraph_candidates_include (func);
	  return;
	}
    }
  /* add it to the list of calls.  */
  rtegraph_include_function_call (func);
}


/* examine_function_decl, check if the current function is a module
   constructor/deconstructor.  Also check if the current function is
   declared as external.  */

static void
examine_function_decl (rtenode *rt)
{
  tree fndecl = rtegraph_get_func (rt);
  if (fndecl != NULL && (DECL_NAME (fndecl) != NULL))
    {
      /* check if the function is a module constructor.  */
      if (is_constructor (fndecl))
	rtegraph_constructors_include (rt);
      /* can it be called externally?  */
      if (is_external (fndecl))
	rtegraph_externs_include (rt);
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
  /* record a function declaration.  */
  rtenode *fn = rtegraph_lookup (fun->gimple_body, fun->decl, false);

  rtegraph_set_current_function (fn);
  /* check if the current function is a module constructor/deconstructor.
     Also check if the current function is declared as external.  */
  examine_function_decl (fn);
  FOR_EACH_BB_FN (bb, fun)
    {
      int stmt_count = 0;
      bool last_statement_call = true;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  // printf ("  [%d][%d]  [basic block][statement]\n", count, stmt_count);
	  stmt_count++;
	  runtime_exception_inevitable (gsi_stmt (gsi));
	  last_statement_call = is_gimple_call (gsi_stmt (gsi));
	  // debug (gsi_stmt (gsi));
	}
      /* we only care about the first basic block in each function,
	 but we continue to search if the last statement was a function.  */
      if (! last_statement_call)
	return 0;
    }
  return 0;
}

void analyse_graph (void *gcc_data, void *user_data)
{
  return;
  rtegraph_discover ();
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

  rtegraph_init ();

  register_callback (plugin_name,
		     PLUGIN_PASS_MANAGER_SETUP,
		     NULL,
		     &pass_info);
  register_callback (plugin_name,
		     PLUGIN_FINISH, analyse_graph, NULL);
  return 0;
}
