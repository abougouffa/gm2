/* Language-dependent hooks for GNU Modula-2.
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
                 2011, 2012, 2013, 2014
                 Free Software Foundation, Inc.
   Contributed by Gaius Mulley <gaius@glam.ac.uk>

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


#include "gm2-gcc/gcc-consolidation.h"

#include "langhooks-def.h"	/* FIXME: for lhd_set_decl_assembler_name */
#include "tree-pass.h"		/* FIXME: only for PROP_gimple_any */
#include "toplev.h"
#include "debug.h"

#include "opts.h"

#include <mpfr.h>

#define GM2_LANG_C
#include "gm2-lang.h"
#include "m2block.h"
#include "dynamicstrings.h"
#include "m2options.h"
#include "gm2version.h"
#include "m2convert.h"
#include "m2linemap.h"
#include "init.h"
#include "../gm2-tree.h"
#include "convert.h"

static void write_globals (void);


static int insideCppArgs = FALSE;


#define EXPR_STMT_EXPR(NODE)  TREE_OPERAND (EXPR_STMT_CHECK (NODE), 0)

/* start of new stuff.  */

/* Language-dependent contents of a type.  */

struct GTY(()) lang_type
{
  char dummy;
};

/* Language-dependent contents of a decl.  */

struct GTY(()) lang_decl
{
  char dummy;
};

/* Language-dependent contents of an identifier.  This must include a
   tree_identifier.  */

struct GTY(()) lang_identifier
{
  struct tree_identifier common;
};

/* The resulting tree type.  */

union GTY((desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE"),
	   chain_next ("CODE_CONTAINS_STRUCT (TREE_CODE (&%h.generic), TS_COMMON) ? ((union lang_tree_node *) TREE_CHAIN (&%h.generic)) : NULL")))
lang_tree_node
{
  union tree_node GTY((tag ("0"),
		       desc ("tree_node_structure (&%h)"))) generic;
  struct lang_identifier GTY((tag ("1"))) identifier;
};

/* We don't use language_function.  */

struct GTY(()) language_function
{
  /* While we are parsing the function, this contains information
     about the statement-tree that we are building.  */
  /* struct stmt_tree_s stmt_tree; */
    tree stmt_tree;
};

/* end of new stuff.  */





/* Language hooks.  */

bool
gm2_langhook_init (void)
{
  build_common_tree_nodes (false);

  /* I don't know why this has to be done explicitly.  */
  void_list_node = build_tree_list (NULL_TREE, void_type_node);

  build_common_builtin_nodes ();

  /* The default precision for floating point numbers.  This is used
     for floating point constants with abstract type.  This may
     eventually be controllable by a command line option.  */
  mpfr_set_default_prec (256);

  /* Go uses exceptions.  */
  using_eh_for_cleanups ();

  return true;
}

/* The option mask.  */

static unsigned int
gm2_langhook_option_lang_mask (void)
{
  return CL_ModulaX2;
}

/* Initialize the options structure.  */

static void
gm2_langhook_init_options_struct (struct gcc_options *opts)
{
  /* Default to avoiding range issues for complex multiply and
     divide.  */
  opts->x_flag_complex_method = 2;

  /* The builtin math functions should not set errno.  */
  opts->x_flag_errno_math = 0;
  opts->frontend_set_flag_errno_math = true;

  /* Exceptions are used to handle recovering from panics.  */
  opts->x_flag_exceptions = 1;
  opts->x_flag_non_call_exceptions = 1;

  init_FrontEndInit ();
}

/* Infrastructure for a VEC of bool values.  */

/* This array determines whether the filename is associated with
   the C preprocessor.  */

static vec<bool> filename_cpp;


void
gm2_langhook_init_options (unsigned int decoded_options_count,
			   struct cl_decoded_option *decoded_options)
{
  unsigned int i;
  bool in_cpp_args = false;

  // filename_cpp = ggc_vec_alloc<bool> (decoded_options_count);

  for (i = 1; i < decoded_options_count; i++)
    {
      switch (decoded_options[i].opt_index)
	{
	case OPT_fcppbegin:
	  in_cpp_args = true;
	  break;
	case OPT_fcppend:
	  in_cpp_args = false;
	  break;
	case OPT_SPECIAL_input_file:
	case OPT_SPECIAL_program_name:
	  filename_cpp.safe_push (in_cpp_args);
	}
    }
  filename_cpp.safe_push (false);
}


static bool is_cpp_filename (unsigned int i)
{
  gcc_assert (i < filename_cpp.length ());
  return filename_cpp[i];
}

#if 0
/* Infrastructure for a vector of char * pointers.  */

typedef const char *gm2_char_p;

/* The list of directories to search after all the Go specific
   directories have been searched.  */

static vec<gm2_char_p> gm2_search_dirs;

/* The list of directories to search after all the gm2 specific
   directories have been searched.  */
#endif

/* Handle gm2 specific options.  Return 0 if we didn't do anything.  */

bool
gm2_langhook_handle_option (size_t scode, const char *arg,
			    int value,
			    int kind ATTRIBUTE_UNUSED,
			    location_t loc ATTRIBUTE_UNUSED,
			    const struct cl_option_handlers *handlers ATTRIBUTE_UNUSED)
{
  enum opt_code code = (enum opt_code) scode;

  /* ignore file names.  */
  if (code == N_OPTS)
    return 1;

  switch (code) {

  case OPT_I:
    if (insideCppArgs)
      {
	const struct cl_option *option = &cl_options[scode];
	const char *opt = (const char *) option->opt_text;
	M2Options_CppArg(opt, arg, TRUE);
      }
    else
      M2Options_SetSearchPath (arg);
    return 1;
  case OPT_fiso:
    M2Options_SetISO (value);
    return 1;
  case OPT_fpim:
    M2Options_SetPIM (value);
    return 1;
  case OPT_fpim2:
    M2Options_SetPIM2 (value);
    return 1;
  case OPT_fpim3:
    M2Options_SetPIM3 (value);
    return 1;
  case OPT_fpim4:
    M2Options_SetPIM4 (value);
    return 1;
  case OPT_fpositive_mod_floor_div:
    M2Options_SetPositiveModFloor (value);
    return 1;
  case OPT_flibs_:
    /* handled in the gm2 driver */
    return 1;
  case OPT_fnil:
    M2Options_SetNilCheck (value);
    return 1;
#if 0
  case OPT_fwholediv:
    M2Options_SetWholeDiv (value);
    return 1;
  case OPT_findex:
    M2Options_SetIndex (value);
    return 1;
  case OPT_frange:
    M2Options_SetRange (value);
    return 1;
#endif
  case OPT_freturn:
    M2Options_SetReturnCheck (value);
    return 1;
  case OPT_fcase:
    M2Options_SetCaseCheck (value);
    return 1;
  case OPT_fd:
    M2Options_SetCompilerDebugging (value);
    return 1;
  case OPT_fdebug_trace_quad:
    M2Options_SetDebugTraceQuad (value);
    return 1;
  case OPT_fdebug_trace_api:
    M2Options_SetDebugTraceAPI (value);
    return 1;
  case OPT_fdebug_function_line_numbers:
    M2Options_SetDebugFunctionLineNumbers (value);
    return 1;
  case OPT_fsoft_check_all:
    M2Options_SetCheckAll (value);
    return 1;
  case OPT_fexceptions:
    M2Options_SetExceptions (value);
    return 1;
  case OPT_Wstudents:
    M2Options_SetStudents (value);
    return 1;
  case OPT_Wpedantic:
    M2Options_SetPedantic (value);
    return 1;
  case OPT_Wpedantic_param_names:
    M2Options_SetPedanticParamNames (value);
    return 1;
  case OPT_Wpedantic_cast:
    M2Options_SetPedanticCast (value);
    return 1;
  case OPT_fextended_opaque:
    M2Options_SetExtendedOpaque (value);
    return 1;
  case OPT_Wverbose_unbounded:
    M2Options_SetVerboseUnbounded (value);
    return 1;
  case OPT_fxcode:
    M2Options_SetXCode (value);
    return 1;
  case OPT_fuselist:
    /* handled in the driver */
    return 1;
  case OPT_fmakelist:
    /* handled in the driver */
    return 1;
  case OPT_fmodules:
    /* handled in the driver */
    return 1;
  case OPT_fruntime_modules_:
    /* handled in the driver */
    return 1;
  case OPT_fclean:
    /* handled in the driver */
    return 1;
  case OPT_fmakeall:
    /* handled in the driver */
    return 1;
  case OPT_fmakeall0:
    /* handled in the driver */
    return 1;
  case OPT_fmake_I_:
    /* handled in the driver */
    return 1;
  case OPT_fno_pth:
    /* handled in the driver */
    return 1;
  case OPT_ftarget_ar_:
    /* handled in the driver */
    return 1;
  case OPT_ftarget_ranlib_:
    /* handled in the driver */
    return 1;
  case OPT_fcpp:
    M2Options_SetCpp (value);
    return 1;
  case OPT_fcppbegin:
    insideCppArgs = TRUE;
    return 1;
  case OPT_fcppend:
    insideCppArgs = FALSE;
    return 1;
  case OPT_fcppprog_:
    M2Options_CppProg (arg);
    return 1;
  case OPT_fq:
    M2Options_SetQuadDebugging (value);
    return 1;
  case OPT_fsources:
    M2Options_SetSources (value);
    return 1;
  case OPT_funbounded_by_reference:
    M2Options_SetUnboundedByReference (value);
    return 1;
  case OPT_fdef_:
    M2Options_setdefextension (arg);
    return 1;
  case OPT_fmod_:
    M2Options_setmodextension (arg);
    return 1;
  case OPT_fdump_system_exports:
    M2Options_SetDumpSystemExports (value);
    return 1;
  case OPT_fswig:
    M2Options_SetSwig (value);
    return 1;
  case OPT_fshared:
    /* handled by the linker */
    return 1;
  case OPT_fmakeinit:
    /* handled by the linker */
    return 1;
  case OPT_fm2_statistics:
    M2Options_SetStatistics (value);
    return 1;
  case OPT_fm2_g:
    M2Options_SetM2g (value);
    return 1;
  case OPT_fobject_path_:
    /* handled by the linker */
    return 1;
  case OPT_fonlylink:
    /* handled by the driver */
    return 1;
  case OPT_version:
    M2Options_DisplayVersion (FALSE);
    return 1;
  case OPT_fgm2_version:
    M2Options_DisplayVersion (FALSE);
    return 1;
  case OPT_O:
    M2Options_SetOptimizing (value);
    return 1;
  case OPT_quiet:
    M2Options_SetQuiet(value);
    return 1;
  case OPT_fm2_whole_program:
    M2Options_SetWholeProgram (value);
    return 1;
  case OPT_flocation_:
    if (strcmp (arg, "builtins") == 0) {
      M2Options_SetForcedLocation (BUILTINS_LOCATION);
      return 1;
    }
    else if (strcmp (arg, "unknown") == 0) {
      M2Options_SetForcedLocation (UNKNOWN_LOCATION);
      return 1;
    }
    else if ((arg != NULL) && (ISDIGIT (arg[0]))) {
      M2Options_SetForcedLocation (atoi (arg));
      return 1;
    }
    else
      return 0;
  default:
    if (insideCppArgs) {
      const struct cl_option *option = &cl_options[scode];
      const char *opt = (const char *) option->opt_text;

      M2Options_CppArg (opt, arg, TRUE);
      return 1;
    }
    return 0;
  }
  return 0;
}

/* Run after parsing options.  */

static bool
gm2_langhook_post_options (const char **pfilename ATTRIBUTE_UNUSED)
{
  flag_excess_precision_cmdline = EXCESS_PRECISION_FAST;
  M2Options_SetCC1Quiet (quiet_flag);
  M2Options_FinaliseOptions ();

  /* Returning false means that the backend should be used.  */
  return false;
}

static void
gm2_parse_input_files (const char** filenames, unsigned int filename_count)
{
  unsigned int i;
  gcc_assert (filename_count > 0);

  for (i = 0; i < filename_count; i++)
    if (! is_cpp_filename (i))
      init_PerCompilationInit (filenames[i]);
}

static void
gm2_langhook_parse_file (void)
{
  gm2_parse_input_files (in_fnames, num_in_fnames);
  write_globals ();
}

static tree
gm2_langhook_type_for_size (unsigned int bits, int unsignedp)
{
  return gm2_type_for_size (bits, unsignedp);
}

static tree
gm2_langhook_type_for_mode (enum machine_mode mode, int unsignedp)
{
  tree type;

  if (VECTOR_MODE_P (mode))
    {
      tree inner;

      inner = gm2_langhook_type_for_mode (GET_MODE_INNER (mode), unsignedp);
      if (inner != NULL_TREE)
	return build_vector_type_for_mode (inner, mode);
      return NULL_TREE;
    }

  // FIXME: This static_cast should be in machmode.h.
  enum mode_class mc = static_cast<enum mode_class>(GET_MODE_CLASS(mode));
  if (mc == MODE_INT)
    return gm2_langhook_type_for_size (GET_MODE_BITSIZE(mode), unsignedp);
  else if (mc == MODE_FLOAT)
    {
      switch (GET_MODE_BITSIZE (mode))
	{
	case 32:
	  return float_type_node;
	case 64:
	  return double_type_node;
	default:
	  // We have to check for long double in order to support
	  // i386 excess precision.
	  if (mode == TYPE_MODE (long_double_type_node))
	    return long_double_type_node;
	}
    }
  else if (mc == MODE_COMPLEX_FLOAT)
    {
      switch (GET_MODE_BITSIZE (mode))
	{
	case 64:
	  return complex_float_type_node;
	case 128:
	  return complex_double_type_node;
	default:
	  // We have to check for long double in order to support
	  // i386 excess precision.
	  if (mode == TYPE_MODE (complex_long_double_type_node))
	    return complex_long_double_type_node;
	}
    }

#if HOST_BITS_PER_WIDE_INT >= 64
  /* The middle-end and some backends rely on TImode being supported
     for 64-bit HWI.  */
  if (mode == TImode)
    {
      type = build_nonstandard_integer_type (GET_MODE_BITSIZE (TImode),
					     unsignedp);
      if (type && TYPE_MODE (type) == TImode)
	return type;
    }
#endif
  return NULL_TREE;
}

/* Record a builtin function.  We just ignore builtin functions.  */

static tree
gm2_langhook_builtin_function (tree decl)
{
  return decl;
}

/* Return true if we are in the global binding level.  */

static bool
gm2_langhook_global_bindings_p (void)
{
  return current_function_decl == NULL_TREE;
}

/* Push a declaration into the current binding level.  We can't
   usefully implement this since we don't want to convert from tree
   back to one of our internal data structures.  I think the only way
   this is used is to record a decl which is to be returned by
   getdecls, and we could implement it for that purpose if
   necessary.  */

static tree
gm2_langhook_pushdecl (tree decl ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* This hook is used to get the current list of declarations as trees.
   We don't support that; instead we use write_globals.  This
   can't simply crash because it is called by -gstabs.  */

static tree
gm2_langhook_getdecls (void)
{
  return NULL;
}

static void
m2_write_global_declarations (tree globals)
{
  tree decl = globals;
  int n = 0;

  while (decl != NULL)
    {
      n++;
      decl = TREE_CHAIN (decl);
    }

  if (n > 0)
    {
      int i = 0;
      tree vec[n];
      decl = globals;
      while (decl != NULL)
	{
	  vec[i] = decl;
	  decl = TREE_CHAIN (decl);
	  i++;
	}
      wrapup_global_declarations (vec, n);
    }
}

#if 0
  for (decl = globals; decl ; decl = DECL_CHAIN (decl))
    {
      rest_of_decl_compilation (decl, 1, 1);
      debug_hooks->late_global_decl (decl);
    }
#endif

/* Write out globals.  */

static void
write_globals (void)
{
  tree t;
  unsigned i;

  m2block_finishGlobals ();

#if 0
  /* Process all file scopes in this compilation, and the external_scope,
     through wrapup_global_declarations and check_global_declarations.  */
  FOR_EACH_VEC_ELT (all_translation_units, i, t)
    m2_write_global_declarations (BLOCK_VARS (DECL_INITIAL (t)));
  /* in the future it is likely that GCC will call this automatically.
     Until then we must do this.
  */
  /* We're done parsing; proceed to optimize and emit assembly. */
  cgraph_finalize_compilation_unit ();
#endif

 /* Process all file scopes in this compilation, and the external_scope,
     through wrapup_global_declarations and check_global_declarations.  */
  FOR_EACH_VEC_ELT (*all_translation_units, i, t)
    m2_write_global_declarations (BLOCK_VARS (DECL_INITIAL (t)));

#if 0
  /* We're done parsing; proceed to optimize and emit assembly.
     FIXME: shouldn't be the front end's responsibility to call this.  */
  symtab->finalize_compilation_unit ();

  /* After cgraph has had a chance to emit everything that's going to
     be emitted, output debug information for globals.  */
  if (!seen_error ())
    {
      timevar_push (TV_SYMOUT);
      FOR_EACH_VEC_ELT (*all_translation_units, i, t)
	m2_write_global_declarations (BLOCK_VARS (DECL_INITIAL (t)));
      timevar_pop (TV_SYMOUT);
    }
#endif
}


/*  Gimplify an EXPR_STMT node.  */

static void
gimplify_expr_stmt (tree *stmt_p)
{
  gcc_assert (EXPR_STMT_EXPR (*stmt_p) != NULL_TREE);

  *stmt_p = EXPR_STMT_EXPR (*stmt_p);
}


/* Genericize a TRY_BLOCK.  */

static void
genericize_try_block (tree *stmt_p)
{
  tree body = TRY_STMTS (*stmt_p);
  tree cleanup = TRY_HANDLERS (*stmt_p);

  *stmt_p = build2 (TRY_CATCH_EXPR, void_type_node, body, cleanup);
}

/* Genericize a HANDLER by converting to a CATCH_EXPR.  */

static void
genericize_catch_block (tree *stmt_p)
{
  tree type = HANDLER_TYPE (*stmt_p);
  tree body = HANDLER_BODY (*stmt_p);

  /* FIXME should the caught type go in TREE_TYPE?  */
  *stmt_p = build2 (CATCH_EXPR, void_type_node, type, body);
}

/* Convert the tree representation of FNDECL from m2 frontend trees to
   GENERIC.  */

extern void pf (tree);

void
gm2_genericize (tree fndecl)
{
  tree t;
  struct cgraph_node *cgn;

#if 0
  pf (fndecl);
#endif
#if 1
  /* Fix up the types of parms passed by invisible reference.  */
  for (t = DECL_ARGUMENTS (fndecl); t; t = DECL_CHAIN (t))
    if (TREE_ADDRESSABLE (TREE_TYPE (t)))
      {
        /* If a function's arguments are copied to create a thunk,
           then DECL_BY_REFERENCE will be set -- but the type of the
           argument will be a pointer type, so we will never get
           here.  */
        gcc_assert (!DECL_BY_REFERENCE (t));
        gcc_assert (DECL_ARG_TYPE (t) != TREE_TYPE (t));
        TREE_TYPE (t) = DECL_ARG_TYPE (t);
        DECL_BY_REFERENCE (t) = 1;
        TREE_ADDRESSABLE (t) = 0;
        relayout_decl (t);
      }
#endif

  /* Dump all nested functions now.  */
  cgn = cgraph_node::get_create (fndecl);
  for (cgn = cgn->nested; cgn ; cgn = cgn->next_nested)
    gm2_genericize (cgn->decl);
}

/* gm2 gimplify expression, currently just change THROW in the same way as C++
 */

static int
gm2_langhook_gimplify_expr (tree *expr_p, gimple_seq *pre_p ATTRIBUTE_UNUSED, gimple_seq *post_p ATTRIBUTE_UNUSED)
{
  enum tree_code code = TREE_CODE (*expr_p);

  switch (code)
    {
#if 0
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
      {
        tree *op1_p = &TREE_OPERAND (*expr_p, 1);
        if (TREE_CODE (TREE_TYPE (*op1_p)) != VECTOR_TYPE
            && !types_compatible_p (TYPE_MAIN_VARIANT (TREE_TYPE (*op1_p)),
                                    unsigned_type_node)
            && !types_compatible_p (TYPE_MAIN_VARIANT (TREE_TYPE (*op1_p)),
                                    integer_type_node))
          *op1_p = convert (unsigned_type_node, *op1_p);
	return GS_OK;
      }
#endif

    case THROW_EXPR:
      /* FIXME communicate throw type to back end, probably by moving
	 THROW_EXPR into ../tree.def.  */
      *expr_p = TREE_OPERAND (*expr_p, 0);
      return GS_OK;

    case EXPR_STMT:
      gimplify_expr_stmt (expr_p);
      return GS_OK;

    case TRY_BLOCK:
      genericize_try_block (expr_p);
      return GS_OK;

    case HANDLER:
      genericize_catch_block (expr_p);
      return GS_OK;

    default:
      return GS_UNHANDLED;
    }
}

/* FIXME: This is a hack to preserve trees that we create from the
   garbage collector.  */

static GTY(()) tree gm2_gc_root;
static tree personality_decl = NULL_TREE;


static void
gm2_preserve_from_gc (tree t)
{
  gm2_gc_root = tree_cons (NULL_TREE, t, gm2_gc_root);
}

/* Return a decl for the exception personality function.  The function
   itself is implemented in libgo/runtime/go-unwind.c.  */

static tree
gm2_langhook_eh_personality (void)
{
  if (personality_decl == NULL_TREE)
    {
      personality_decl = build_personality_function ("gxx");
      gm2_preserve_from_gc (personality_decl);
    }
  return personality_decl;
}

/* Functions called directly by the generic backend.  */

tree
convert_loc (location_t location, tree type, tree expr)
{
  if (type == error_mark_node
      || expr == error_mark_node
      || TREE_TYPE (expr) == error_mark_node)
    return error_mark_node;

  if (type == TREE_TYPE (expr))
    return expr;

  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (expr)))
    return fold_convert (type, expr);

  expr = m2convert_GenericToType (location, type, expr);
  switch (TREE_CODE (type))
    {
    case VOID_TYPE:
    case BOOLEAN_TYPE:
      return fold_convert (type, expr);
    case INTEGER_TYPE:
      return fold (convert_to_integer (type, expr));
    case POINTER_TYPE:
      return fold (convert_to_pointer (type, expr));
    case REAL_TYPE:
      return fold (convert_to_real (type, expr));
    case COMPLEX_TYPE:
      return fold (convert_to_complex (type, expr));
    case ENUMERAL_TYPE:
      return fold (convert_to_integer (type, expr));
    default:
      break;
    }

  gcc_unreachable ();
}


/* Functions called directly by the generic backend.  */

tree
convert (tree type, tree expr)
{
  return convert_loc (m2linemap_UnknownLocation (), type, expr);
}


/* Mark EXP saying that we need to be able to take the
   address of it; it should not be allocated in a register.
   Returns true if successful.  */

bool
gm2_mark_addressable (tree exp)
{
  tree x = exp;

  while (TRUE)
    switch (TREE_CODE (x))
      {
      case COMPONENT_REF:
	if (DECL_PACKED (TREE_OPERAND (x, 1)))
	  return false;

	/* ... fall through ...  */

      case ADDR_EXPR:
      case ARRAY_REF:
      case REALPART_EXPR:
      case IMAGPART_EXPR:
	x = TREE_OPERAND (x, 0);
	break;

      case COMPOUND_LITERAL_EXPR:
      case CONSTRUCTOR:
	TREE_ADDRESSABLE (x) = 1;
	return true;

      case STRING_CST:
      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	/* drops in */
      case FUNCTION_DECL:
	TREE_ADDRESSABLE (x) = 1;
	/* drops out */
      default:
	return true;
    }
  /* never reach here */
}

/* Return an integer type with BITS bits of precision,
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed.  */

tree
gm2_type_for_size (unsigned int bits, int unsignedp)
{
  tree type;

  if (unsignedp)
    {
      if (bits == INT_TYPE_SIZE)
        type = unsigned_type_node;
      else if (bits == CHAR_TYPE_SIZE)
        type = unsigned_char_type_node;
      else if (bits == SHORT_TYPE_SIZE)
        type = short_unsigned_type_node;
      else if (bits == LONG_TYPE_SIZE)
        type = long_unsigned_type_node;
      else if (bits == LONG_LONG_TYPE_SIZE)
        type = long_long_unsigned_type_node;
      else
        type = make_unsigned_type (bits);
    }
  else
    {
      if (bits == INT_TYPE_SIZE)
        type = integer_type_node;
      else if (bits == CHAR_TYPE_SIZE)
        type = signed_char_type_node;
      else if (bits == SHORT_TYPE_SIZE)
        type = short_integer_type_node;
      else if (bits == LONG_TYPE_SIZE)
        type = long_integer_type_node;
      else if (bits == LONG_LONG_TYPE_SIZE)
        type = long_long_integer_type_node;
      else
        type = make_signed_type (bits);
    }
  return type;
}


#undef LANG_HOOKS_NAME
#undef LANG_HOOKS_INIT
#undef LANG_HOOKS_INIT_OPTIONS
#undef LANG_HOOKS_OPTION_LANG_MASK
#undef LANG_HOOKS_INIT_OPTIONS_STRUCT
#undef LANG_HOOKS_HANDLE_OPTION
#undef LANG_HOOKS_POST_OPTIONS
#undef LANG_HOOKS_PARSE_FILE
#undef LANG_HOOKS_TYPE_FOR_MODE
#undef LANG_HOOKS_TYPE_FOR_SIZE
#undef LANG_HOOKS_BUILTIN_FUNCTION
#undef LANG_HOOKS_GLOBAL_BINDINGS_P
#undef LANG_HOOKS_PUSHDECL
#undef LANG_HOOKS_GETDECLS
#undef LANG_HOOKS_GIMPLIFY_EXPR
#undef LANG_HOOKS_EH_PERSONALITY

#define LANG_HOOKS_NAME			"GNU Modula-2"
#define LANG_HOOKS_INIT			gm2_langhook_init
#define LANG_HOOKS_INIT_OPTIONS         gm2_langhook_init_options
#define LANG_HOOKS_OPTION_LANG_MASK	gm2_langhook_option_lang_mask
#define LANG_HOOKS_INIT_OPTIONS_STRUCT	gm2_langhook_init_options_struct
#define LANG_HOOKS_HANDLE_OPTION	gm2_langhook_handle_option
#define LANG_HOOKS_POST_OPTIONS		gm2_langhook_post_options
#define LANG_HOOKS_PARSE_FILE		gm2_langhook_parse_file
#define LANG_HOOKS_TYPE_FOR_MODE	gm2_langhook_type_for_mode
#define LANG_HOOKS_TYPE_FOR_SIZE	gm2_langhook_type_for_size
#define LANG_HOOKS_BUILTIN_FUNCTION	gm2_langhook_builtin_function
#define LANG_HOOKS_GLOBAL_BINDINGS_P	gm2_langhook_global_bindings_p
#define LANG_HOOKS_PUSHDECL		gm2_langhook_pushdecl
#define LANG_HOOKS_GETDECLS		gm2_langhook_getdecls
#define LANG_HOOKS_GIMPLIFY_EXPR	gm2_langhook_gimplify_expr
#define LANG_HOOKS_EH_PERSONALITY	gm2_langhook_eh_personality

struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

#include "gt-gm2-gm2-lang.h"
#include "gtype-gm2.h"
