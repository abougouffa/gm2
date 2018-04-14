/* This plugin recursively dumps the source-code location ranges of
   expressions, at the pre-gimplification tree stage.  */
/* { dg-options "-O" } */

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


static void pretty_function (tree fndecl)
{
  if (fndecl != NULL && (DECL_NAME (fndecl) != NULL))
    {
      const char *n = IDENTIFIER_POINTER (DECL_NAME (fndecl));
      fprintf (stderr, "%s\n", n);
    }
}


static const char *access_string (tree t)
{
  if (TREE_CODE (t) == ADDR_EXPR)
    {
      if (TREE_CODE (TREE_OPERAND (t, 0)) == STRING_CST)
	return TREE_STRING_POINTER (TREE_OPERAND (t, 0));
    }
  return NULL;
}

/*  my_error_at - wraps up an error message, --fixme-- this needs to turn
    off the 'In function' component of the error message as it can be misleading
    after optimization.  */

static void my_error_at (location_t location, const char *message, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  rich_location richloc (line_table, location);

  va_start (ap, message);
  diagnostic_set_info (&diagnostic, message, &ap, &richloc, DK_ERROR);
  diagnostic_report_diagnostic (global_dc, &diagnostic);
  va_end (ap);
}

/* generate an error using the parameters of the M2RTS exception handler to
   locate the source code.  We dont use location, as the error_at function will
   give the function context which might be misleading if this is inlined.  */

static void generate_error (gimple *stmt, const char *message)
{
  if (gimple_call_num_args (stmt) == 4)
    {
      tree s0 = gimple_call_arg (stmt, 0);
      tree i1 = gimple_call_arg (stmt, 1);
      tree i2 = gimple_call_arg (stmt, 2);
      const char *file = access_string (s0);
      int line = TREE_INT_CST_LOW (i1);
      int col = TREE_INT_CST_LOW (i2);
      fprintf (stderr, "%s:%d:%d:inevitable that this error will occur at runtime, %s\n", file, line, col, message);
#if 0
      location_t location = gimple_location (stmt);
      my_error_at (location, "inevitable that this error will occur at runtime, %s exceeds range\n", message);
#endif
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
	generate_error (stmt, "assignment will result in an overflow");
      else if (strcmp (n, "M2RTS_ReturnException") == 0)
	generate_error (stmt, "the expression in the RETURN statement of a procedure function exceeds the function return type range");
      else if (strcmp (n, "M2RTS_IncException") == 0)
	generate_error (stmt, "standard procedure INC overflow");
      else if (strcmp (n, "M2RTS_DecException") == 0)
	generate_error (stmt, "standard procedure DEC overflow");
      else if (strcmp (n, "M2RTS_InclException") == 0)
	generate_error (stmt, "standard procedure INCL overflow");
      else if (strcmp (n, "M2RTS_ExclException") == 0)
	generate_error (stmt, "standard procedure EXCL overflow");
      else if (strcmp (n, "M2RTS_ShiftException") == 0)
	generate_error (stmt, "standard procedure SHIFT overflow");
      else if (strcmp (n, "M2RTS_RotateException") == 0)
	generate_error (stmt, "standard procedure ROTATE overflow");
      else if (strcmp (n, "M2RTS_StaticArraySubscriptException") == 0)
	generate_error (stmt, "static array subscript out of bounds");
      else if (strcmp (n, "M2RTS_DynamicArraySubscriptException") == 0)
	generate_error (stmt, "dynamic array subscript out of bounds");
      else if (strcmp (n, "M2RTS_ForLoopBeginException") == 0)
	generate_error (stmt, "assignment at the beginning of the FOR loop will cause an overflow");
      else if (strcmp (n, "M2RTS_ForLoopToException") == 0)
	generate_error (stmt, "the TO expression of the FOR loop is out of range");
      else if (strcmp (n, "M2RTS_ForLoopEndException") == 0)
	generate_error (stmt, "the increment or decrement in the FOR loop will cause the iterator to exceed its type range");
      else if (strcmp (n, "M2RTS_PointerNilException") == 0)
	generate_error (stmt, "attempting to dereference a NIL pointer");
      else if (strcmp (n, "M2RTS_NoReturnException") == 0)
	generate_error (stmt, "procedure function will finish without executing a RETURN statement");
      else if (strcmp (n, "M2RTS_CaseException") == 0)
	generate_error (stmt, "CASE statement will not detect the selector expression");
      else if (strcmp (n, "M2RTS_WholeNonPosDivException") == 0)
	generate_error (stmt, "expression will generate an exception as the denominator to DIV is negative");
      else if (strcmp (n, "M2RTS_WholeNonPosModException") == 0)
	generate_error (stmt, "expression will generate an exception as the denominator to MOD is negative");
      else if (strcmp (n, "M2RTS_WholeZeroDivException") == 0)
	generate_error (stmt, "expression will generate an exception as the denominator to DIV is zero");
      else if (strcmp (n, "M2RTS_WholeZeroRemException") == 0)
	generate_error (stmt, "expression will generate an exception as the denominator to REM is zero");
      else if (strcmp (n, "M2RTS_WholeValueException") == 0)
	generate_error (stmt, "expression will generate an exception as a whole value will overflow the type range");
      else if (strcmp (n, "M2RTS_RealValueException") == 0)
	generate_error (stmt, "expression will generate an exception as a real value will overflow the type range");
      else if (strcmp (n, "M2RTS_NoException") == 0)
	generate_error (stmt, "an exception will occur because the program is attempting to discover the source of a non-existent exception");
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
      /*  we only care about the first basic block in each function.
       */
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


/*
 *  plugin_init - check the version and register the plugin.
 */

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
