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
along with GNU Modula-2; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/*
 *
 * IMPLEMENTATION MODULE gccgm2
 *     (* thus all external functions will be prefixed by gccgm2_
 *        which will allow us to construct a DEFINITION MODULE
 *        so that m2f can interface to gcc.
 *      *)
 *      
 */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "toplev.h"
#include "tm_p.h"
#include "flags.h"
#include <stdio.h>

#undef DEBUG_PROCEDURE_CALLS

/*
 *  utilize some of the C build routines
 */

#include "c-tree.h"
#include "rtl.h"
#include "function.h"
#include "expr.h"
#include "output.h"
#include "ggc.h"
#include "convert.h"


/* In grokdeclarator, distinguish syntactic contexts of declarators.  */
enum decl_context
{ NORMAL,			/* Ordinary declaration */
  FUNCDEF,			/* Function definition */
  PARM,				/* Declaration of parm before function body */
  FIELD,			/* Declaration inside struct or union */
  BITFIELD,			/* Likewise but with specified width */
  TYPENAME};			/* Typename (inside cast or sizeof)  */

enum attrs {A_PACKED, A_NOCOMMON, A_COMMON, A_NORETURN, A_CONST, A_T_UNION,
	    A_CONSTRUCTOR, A_DESTRUCTOR, A_MODE, A_SECTION, A_ALIGNED,
	    A_UNUSED, A_FORMAT, A_FORMAT_ARG, A_WEAK, A_ALIAS};

static void add_attribute		PROTO((enum attrs, char *,
					       int, int, int));
static void init_attributes		PROTO((void));
       tree gccgm2_BuildIntegerConstant PROTO((int value));
       tree gccgm2_GetIntegerType       PROTO((void));
       tree gccgm2_BuildConvert         PROTO((tree, tree, int));


/*
 *  and define some type sizes
 */

#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_LONG_TYPE_SIZE
#define LONG_LONG_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef SHORT_TYPE_SIZE
#define SHORT_TYPE_SIZE (BITS_PER_UNIT * MIN ((UNITS_PER_WORD + 1) / 2, 2))
#endif

#ifndef FLOAT_TYPE_SIZE
#define FLOAT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef DOUBLE_TYPE_SIZE
#define DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"
#endif

#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

/* We let tm.h override the types used here, to handle trivial differences
   such as the choice of unsigned int or long unsigned int for size_t.
   When machines start needing nontrivial differences in the size type,
   it would be best to do something here to figure out automatically
   from other information what type to use.  */

#ifndef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"
#endif



#define ORDINAL_TYPE(code) \
 ((code) == INTEGER_TYPE || (code) == CHAR_TYPE || \
  (code) == BOOLEAN_TYPE || (code) == ENUMERAL_TYPE)

tree proc_type_node;
tree gm2_memcpy_node;
tree gm2_alloca_node;


/* Global Variables for the various types and nodes we create.  */ 

#if 0 /* (GCC_VERSION <= 2007) */
/* we must keep the C base types as the GCC depends on them */

tree error_mark_node;
tree integer_type_node;              /* INTEGER  */
tree void_type_node;                 /* VOID     */
tree char_type_node;                 /* CHAR     */
tree ptr_type_node;                  /* ADDRESS  */
tree unsigned_char_type_node;        /* BYTE     */
tree long_unsigned_type_node;        /* internal */
tree float_type_node;                /* REAL     */
tree double_type_node;               /* LONGREAL */
tree long_long_integer_type_node;    /* LONGINT  */
tree long_long_unsigned_type_node;   /* LONGCARD */

tree long_integer_type_node;         /* internal */
tree long_double_type_node;          /* internal */
tree unsigned_type_node;             /* CARDINAL & WORD */
tree signed_char_type_node;          /* internal */
tree short_unsigned_type_node;       /* internal */
tree short_integer_type_node;        /* internal */
tree ptrdiff_type_node;              /* internal */
tree char_array_type_node;           /* internal */

tree integer_zero_node;
tree integer_one_node;
tree null_pointer_node;

tree boolean_type_node;
tree boolean_false_node;
tree boolean_true_node;
#else
/* The following symbols are subsumed in the c_global_trees array, and
   listed here individually for documentation purposes. 

   INTEGER_TYPE and REAL_TYPE nodes for the standard data types.

	tree short_integer_type_node;
	tree long_integer_type_node;
	tree long_long_integer_type_node;

	tree short_unsigned_type_node;
	tree long_unsigned_type_node;
	tree long_long_unsigned_type_node;

	tree boolean_type_node;
	tree boolean_false_node;
	tree boolean_true_node;

	tree ptrdiff_type_node;

	tree unsigned_char_type_node;
	tree signed_char_type_node;
	tree wchar_type_node;
	tree signed_wchar_type_node;
	tree unsigned_wchar_type_node;

	tree float_type_node;
	tree double_type_node;
	tree long_double_type_node;

	tree complex_integer_type_node;
	tree complex_float_type_node;
	tree complex_double_type_node;
	tree complex_long_double_type_node;

	tree intQI_type_node;
	tree intHI_type_node;
	tree intSI_type_node;
	tree intDI_type_node;
	tree intTI_type_node;

	tree unsigned_intQI_type_node;
	tree unsigned_intHI_type_node;
	tree unsigned_intSI_type_node;
	tree unsigned_intDI_type_node;
	tree unsigned_intTI_type_node;

	tree widest_integer_literal_type_node;
	tree widest_unsigned_literal_type_node;

   Nodes for types `void *' and `const void *'.

	tree ptr_type_node, const_ptr_type_node;

   Nodes for types `char *' and `const char *'.

	tree string_type_node, const_string_type_node;

   Type `char[SOMENUMBER]'.
   Used when an array of char is needed and the size is irrelevant.

	tree char_array_type_node;

   Type `int[SOMENUMBER]' or something like it.
   Used when an array of int needed and the size is irrelevant.

	tree int_array_type_node;

   Type `wchar_t[SOMENUMBER]' or something like it.
   Used when a wide string literal is created.

	tree wchar_array_type_node;

   Type `int ()' -- used for implicit declaration of functions.

	tree default_function_type;

   Function types `int (int)', etc.

	tree int_ftype_int;
	tree void_ftype;
	tree void_ftype_ptr;
	tree int_ftype_int;
	tree ptr_ftype_sizetype;

   A VOID_TYPE node, packaged in a TREE_LIST.

	tree void_list_node;

*/

tree c_global_trees[CTI_MAX];
#endif

tree current_function_decl;


/* While defining an enum type, this is 1 plus the last enumerator
   constant value.  Note that will do not have to save this or `enum_overflow'
   around nested function definition since such a definition could only
   occur in an enum value expression and we don't use these variables in
   that case.  */

static tree enum_next_value;


/* Used in build_enumerator() */
tree current_enum_type;

/* Used in BuildEnumerator */
tree enumvalues=NULL_TREE;

/* Used in BuildStartFunctionType */
tree param_type_list;
tree param_list = NULL_TREE;   /* ready for the next time we call/define a function */
static tree last_function=NULL_TREE;

/* A list (chain of TREE_LIST nodes) of all LABEL_DECLs in the function
   that have names.  Here so we can clear out their names' definitions
   at the end of the function.  */

static tree named_labels;

/* A list of LABEL_DECLs from outer contexts that are currently shadowed.  */

static tree shadowed_labels;


static tree qualify_type		PROTO ((tree, tree));
static tree build_c_type_variant        PROTO ((tree, int, int));
#if 0
static int  type_lists_compatible_p     PROTO ((tree, tree));
#endif


/* Nonzero means allow type mismatches in conditional expressions;
   just make their values `void'.   */

int flag_cond_mismatch=1;

/* Nonzero means the expression being parsed will never be evaluated.
   This is a count, since unevaluated expressions can nest.  */
int skip_evaluation=0;

/* Nonzero means warn about sizeof(function) or addition/subtraction
   of function pointers.  */
int warn_pointer_arith=1;

/* Warn if a type conversion is done that might have confusing results.  */
int warn_conversion=1;

/* Warn about comparison of signed and unsigned values.
   If -1, neither -Wsign-compare nor -Wno-sign-compare has been specified.  */
int warn_sign_compare = -1;

/* Nonzero means warn about extern declarations of objects not at
   file-scope level and about *all* declarations of functions (whether
   extern or static) not at file-scope level.  Note that we exclude
   implicit function declarations.  To get warnings about those, use
   -Wimplicit.  */
int warn_nested_externs = 0;

/* Nonzero means warn about multiple (redundant) decls for the same single
   variable or function.  */
int warn_redundant_decls = 0;

/* arguments given to compiler */

extern int save_argc;
extern char **save_argv;

extern void gccgm2front ();

/* Global Variables Expected by gcc: */

const char *language_string = "Modula-2";
int flag_traditional=FALSE;	/* Used by dwarfout.c.  */
int ggc_p=FALSE;                /* don't bother garbage collecting */


/*
 *  tree.c - interface
 */

void
gccgm2_EndTemporaryAllocation ()
{
  end_temporary_allocation();
}

void
gccgm2_ResumeTemporaryAllocation ()
{
  resume_temporary_allocation();
}

void
gccgm2_PushObstacksNochange ()
{
  push_obstacks_nochange();
}

void
gccgm2_PopObstacks ()
{
  pop_obstacks();
}

/*
 *  gccgm2_SetFileNameAndLineNo - allows m2f to set the filename and line number
 *                                at relevent points during the declaration of types
 *                                and construction of code. Also remember that m2f must
 *                                pass a static string as no copy is made.
 */

void
gccgm2_SetFileNameAndLineNo (fn, line)
     char *fn;
     int   line;
{
  input_filename = fn;       /* remember that both these variables are actually external to this file */
  lineno         = line;
}


/*
 *  EmitLineNote - emits line and file information to gcc whilst constructing code.
 *                 Should be used when generating code (after a call to SetFileNameAndLineNo).
 */

void
gccgm2_EmitLineNote (fn, line)
     char *fn;
     int   line;
{
  emit_line_note(fn, line);
}


#if 0
/* Here are the three functions needed to compile our language and the
   variables they use.  */

/* Number of arguments to the current function and the decls for the args.  */
static int num_args;
static tree *arg_decls;

/* Make a FUNCTION_DECL for a function whose single-character name is
   NAME, that has NARGS integer operands, and returns integer.  */

tree
build_function_decl (name, nargs)
     char name;
     int nargs;
{
  tree param_list = NULL_TREE;
  tree param_type_list = tree_cons (NULL_TREE, void_type_node, NULL_TREE);
  tree parm_decl;
  tree fntype;
  tree fndecl;
  char *p;
  int i;

  /* Allocate space for our PARM_DECLS and create them.  */

  num_args = nargs;
  arg_decls = (tree *) xmalloc (nargs * sizeof (tree));
  for (i = nargs - 1; i >= 0; i--)
    {
      p = xmalloc (2);
      p[0] = 'a' + i;
      p[1] = '\0';
      parm_decl = build_decl (PARM_DECL, get_identifier (p),
			      integer_type_node);
      DECL_ARG_TYPE (parm_decl) = integer_type_node;
      arg_decls[i] = parm_decl;
      param_list = chainon (parm_decl, param_list);
      param_type_list = tree_cons (NULL_TREE, integer_type_node,
				   param_type_list);
    }

  /* The function type depends on the return type and type of args.  */
  fntype = build_function_type (integer_type_node, param_type_list);

  /* Now make the function decl.  */
  p = xmalloc (2);
  p[0] = name;
  p[1] = '\0';

  fndecl = build_decl (FUNCTION_DECL, get_identifier (p), fntype);
  DECL_EXTERNAL (fndecl) = 0;
  TREE_PUBLIC (fndecl) = 1;
  TREE_STATIC (fndecl) = 1;
  DECL_ARGUMENTS (fndecl) = param_list;
  DECL_RESULT (fndecl)
    = build_decl (RESULT_DECL, NULL_TREE, integer_type_node);
  DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;

  rest_of_decl_compilation (fndecl, NULL_PTR, 1, 0);
  param_list = NULL_TREE;   /* ready for the next time we call/define a function */
  return fndecl;
}

/* Get the PARM_DECL for the Nth operand.  */

#if 0
tree
get_arg_decl (n)
     int n;
{
  if (n >= num_args)
    {
      fprintf (stderr, "arg number too high\n");
      n = 0;
    }

  return arg_decls[n];
}

/* Generate code for FNDECL that returns EXP.  */

void
build_function (fndecl, exp)
     tree fndecl;
     tree exp;
{
  tree param_decl, next_param;

  /* Set line number information; everything is line 1 for us.  */
  DECL_SOURCE_FILE (fndecl) = input_filename;
  DECL_SOURCE_LINE (fndecl) = lineno;

  /* Announce we are compiling this function.  */
  announce_function (fndecl);

  /* Set up to compile the function and enter it.  */
  current_function_decl = fndecl;
  DECL_INITIAL (fndecl) = error_mark_node;

  temporary_allocation ();
  pushlevel (0);
  make_function_rtl (fndecl);

  /* Push all the PARM_DECL nodes onto the current scope (i.e. the scope of the
     subprogram body) so that they can be recognized as local variables in the
     subprogram.   */

  for (param_decl = nreverse (DECL_ARGUMENTS (fndecl));
       param_decl; param_decl = next_param)
    {
      next_param = TREE_CHAIN (param_decl);
      TREE_CHAIN (param_decl) = NULL;
      pushdecl (param_decl);
    }

  /* Store back the PARM_DECL nodes. They appear in the right order. */
  DECL_ARGUMENTS (fndecl) = getdecls ();

  init_function_start (fndecl, input_filename, 1);
  expand_function_start (fndecl, 0);
  expand_start_bindings (0);

  /* The only thing in this function is a single return statement, which
     we now generate.  Note that EXP will have been allocated in
     permanent_obstack.  To do this properly, we need to enter the function 
     ontext before parsing the expression, but then error recovery is more
     complex, so we don't do that here.  */

  expand_return (build (MODIFY_EXPR, void_type_node,
			DECL_RESULT (fndecl), exp));

  /* Now get back out of the function and compile it.  */
  expand_end_bindings (NULL_TREE, 1, 0);
  poplevel (1, 0, 1);
  expand_function_end (fndecl, 1, 0);
  rest_of_compilation (fndecl);
  current_function_decl = 0;
  permanent_allocation (1);
}
#endif
#endif

/* Routines Expected by gcc:  */

/* These are used to build types for various sizes.  The code below
   is a simplified version of that of GNAT.  */

#ifndef MAX_BITS_PER_WORD
#define MAX_BITS_PER_WORD  BITS_PER_WORD
#endif

/* This variable keeps a table for types for each precision so that we only 
   allocate each of them once. Signed and unsigned types are kept separate.  */
static tree signed_and_unsigned_types[MAX_BITS_PER_WORD + 1][2];

/* Return an integer type with the number of bits of precision given by  
   PRECISION.  UNSIGNEDP is nonzero if the type is unsigned; otherwise
   it is a signed type.  */

tree
type_for_size (precision, unsignedp)
     unsigned precision;
     int unsignedp;
{
  tree t;
  int moment;

  if (precision <= MAX_BITS_PER_WORD
      && signed_and_unsigned_types[precision][unsignedp] != 0)
    return signed_and_unsigned_types[precision][unsignedp];

  /* Since we will keep these types around, they must be permanent.  */
  moment = suspend_momentary ();
  push_obstacks_nochange ();
  end_temporary_allocation ();

 if (unsignedp)
    t = signed_and_unsigned_types[precision][1]
      = make_unsigned_type (precision);
  else
    t = signed_and_unsigned_types[precision][0]
      = make_signed_type (precision);

  pop_obstacks ();
  resume_momentary (moment);

  return t;
}

/* Return a data type that has machine mode MODE.  UNSIGNEDP selects
   an unsigned type; otherwise a signed type is returned.  */

tree
type_for_mode (mode, unsignedp)
     enum machine_mode mode;
     int unsignedp;
{
  return type_for_size (GET_MODE_BITSIZE (mode), unsignedp);
}

/* Return the unsigned version of a TYPE_NODE, a scalar type.  */

tree
unsigned_type (type_node)
     tree type_node;
{
  return type_for_size (TYPE_PRECISION (type_node), 1);
}

/* Return the signed version of a TYPE_NODE, a scalar type.  */

tree
signed_type (type_node)
     tree type_node;
{
  return type_for_size (TYPE_PRECISION (type_node), 0);
}

/* Return a type the same as TYPE except unsigned or signed according to
   UNSIGNEDP.  */

tree
signed_or_unsigned_type (unsignedp, type)
     int unsignedp;
     tree type;
{
  if (! INTEGRAL_TYPE_P (type) || TREE_UNSIGNED (type) == unsignedp)
    return type;
  else
    return type_for_size (TYPE_PRECISION (type), unsignedp);
}

/* For each binding contour we allocate a binding_level structure
 * which records the names defined in that contour.
 * Contours include:
 *  0) the global one
 *  1) one for each function definition,
 *     where internal declarations of the parameters appear.
 *  2) one for each compound statement,
 *     to record its declarations.
 *
 * The current meaning of a name can be found by searching the levels from
 * the current one out to the global one.
 */

/* Note that the information in the `names' component of the global contour
   is duplicated in the IDENTIFIER_GLOBAL_VALUEs of all identifiers.  */

struct binding_level
  {
    /* A chain of _DECL nodes for all variables, constants, functions,
       and typedef types.  These are in the reverse of the order supplied.
     */
    tree names;

    /* A list of structure, union and enum definitions,
     * for looking up tag names.
     * It is a chain of TREE_LIST nodes, each of whose TREE_PURPOSE is a name,
     * or NULL_TREE; and whose TREE_VALUE is a RECORD_TYPE, UNION_TYPE,
     * or ENUMERAL_TYPE node.
     */
    tree tags;

    /* For each level, a list of shadowed outer-level local definitions
       to be restored when this level is popped.
       Each link is a TREE_LIST whose TREE_PURPOSE is an identifier and
       whose TREE_VALUE is its old definition (a kind of ..._DECL node).  */
    tree shadowed;

    /* For each level (except not the global one),
       a chain of BLOCK nodes for all the levels
       that were entered and exited one level down.  */
    tree blocks;

    /* The BLOCK node for this level, if one has been preallocated.
       If 0, the BLOCK is allocated (if needed) when the level is popped.  */
    tree this_block;

    /* The binding level which this one is contained in (inherits from).  */
    struct binding_level *level_chain;

    /* Nonzero for the level that holds the parameters of a function.  */
    char parm_flag;

    /* Nonzero if this level "doesn't exist" for tags.  */
    char tag_transparent;

    /* Nonzero if sublevels of this level "don't exist" for tags.
       This is set in the parm level of a function definition
       while reading the function body, so that the outermost block
       of the function body will be tag-transparent.  */
    char subblocks_tag_transparent;

    /* Nonzero means make a BLOCK for this level regardless of all else.  */
    char keep;

    /* Nonzero means make a BLOCK if this level has any subblocks.  */
    char keep_if_subblocks;

    /* Number of decls in `names' that have incomplete 
       structure or union types.  */
    int n_incomplete;

    /* A list of decls giving the (reversed) specified order of parms,
       not including any forward-decls in the parmlist.
       This is so we can put the parms in proper order for assign_parms.  */
    tree parm_order;
    /* The back end may need, for its own internal processing, to create a BLOCK
       node. This field is set aside for this purpose. If this field is non-null
       when the level is popped, i.e. when poplevel is invoked, we will use such
       block instead of creating a new one from the 'names' field, that is the
       ..._DECL nodes accumulated so far.  Typically the routine 'pushlevel'
       will be called before setting this field, so that if the front-end had
       inserted ..._DECL nodes in the current block they will not be lost.   */
    tree block_created_by_back_end;
  };

#define NULL_BINDING_LEVEL (struct binding_level *) NULL
  
/* The binding level currently in effect.  */

static struct binding_level *current_binding_level;

/* A chain of binding_level structures awaiting reuse.  */

static struct binding_level *free_binding_level;

/* The outermost binding level, for names of file scope.
   This is created when the compiler is started and exists
   through the entire run.  */

static struct binding_level *global_binding_level;

/* Binding level structures are initialized by copying this one.  */

static struct binding_level clear_binding_level
  = {NULL, NULL, NULL, NULL, NULL, NULL_BINDING_LEVEL, 0, 0, 0, 0, 0, 0,
     NULL};

/* Nonzero means unconditionally make a BLOCK for the next level pushed.  */

static int keep_next_level_flag;

/* Nonzero means make a BLOCK for the next level pushed
   if it has subblocks.  */

static int keep_next_if_subblocks;
  
/* Create a new `struct binding_level'.  */

static
struct binding_level *
make_binding_level ()
{
  /* NOSTRICT */
  return (struct binding_level *) xmalloc (sizeof (struct binding_level));
}

/* Return non-zero if we are currently in the global binding level.  */

int
global_bindings_p ()
{
  return current_binding_level == global_binding_level ? -1 : 0;
}

/* Return the list of declarations in the current level. Note that this list
   is in reverse order (it has to be so for back-end compatibility).  */

tree
getdecls ()
{
  return current_binding_level->names;
}

/* Nonzero if the current level needs to have a BLOCK made.  */

int
kept_level_p ()
{
  return (current_binding_level->names != 0);
}

/* Identify this binding level as a level of parameters.
   DEFINITION_FLAG is 1 for a definition, 0 for a declaration.
   But it turns out there is no way to pass the right value for
   DEFINITION_FLAG, so we ignore it.  */

void
declare_parm_level (definition_flag)
     int definition_flag ATTRIBUTE_UNUSED;
{
  current_binding_level->parm_flag = 1;
}

/* Nonzero if currently making parm declarations.  */

int
in_parm_level_p ()
{
  return current_binding_level->parm_flag;
}

#if 1
/* Enter a new binding level.
   If TAG_TRANSPARENT is nonzero, do so only for the name space of variables,
   not for that of tags.  */

void
pushlevel (tag_transparent)
     int tag_transparent;
{
  register struct binding_level *newlevel = NULL_BINDING_LEVEL;

  /* If this is the top level of a function,
     just make sure that NAMED_LABELS is 0.  */

  if (current_binding_level == global_binding_level)
    {
      named_labels = 0;
    }

  /* Reuse or create a struct for this binding level.  */

  if (free_binding_level)
    {
      newlevel = free_binding_level;
      free_binding_level = free_binding_level->level_chain;
    }
  else
    {
      newlevel = make_binding_level ();
    }

  /* Add this level to the front of the chain (stack) of levels that
     are active.  */

  *newlevel = clear_binding_level;
  newlevel->tag_transparent
    = (tag_transparent
       || (current_binding_level
	   ? current_binding_level->subblocks_tag_transparent
	   : 0));
  newlevel->level_chain = current_binding_level;
  current_binding_level = newlevel;
  newlevel->keep = keep_next_level_flag;
  keep_next_level_flag = 0;
  newlevel->keep_if_subblocks = keep_next_if_subblocks;
  keep_next_if_subblocks = 0;
}

/* Clear the limbo values of all identifiers defined in BLOCK or a subblock. */

static void
clear_limbo_values (block)
     tree block;
{
  tree tem;

  for (tem = BLOCK_VARS (block); tem; tem = TREE_CHAIN (tem))
    if (DECL_NAME (tem) != 0)
      IDENTIFIER_LIMBO_VALUE (DECL_NAME (tem)) = 0;

  for (tem = BLOCK_SUBBLOCKS (block); tem; tem = TREE_CHAIN (tem))
    clear_limbo_values (tem);
}

/* Make a label named NAME in the current function,
   shadowing silently any that may be inherited from containing functions
   or containing scopes.

   Note that valid use, if the label being shadowed
   comes from another scope in the same function,
   requires calling declare_nonlocal_label right away.  */

tree
shadow_label (name)
     tree name;
{
  register tree decl = IDENTIFIER_LABEL_VALUE (name);

  if (decl != 0)
    {
      register tree dup;

      /* Check to make sure that the label hasn't already been declared
	 at this label scope */
      for (dup = named_labels; dup; dup = TREE_CHAIN (dup))
	if (TREE_VALUE (dup) == decl)
	  {
	    error ("duplicate label declaration `%s'", 
		   IDENTIFIER_POINTER (name));
	    error_with_decl (TREE_VALUE (dup),
			     "this is a previous declaration");
	    /* Just use the previous declaration.  */
	    return lookup_label (name);
	  }

      shadowed_labels = tree_cons (NULL_TREE, decl, shadowed_labels);
      IDENTIFIER_LABEL_VALUE (name) = decl = 0;
    }

  return lookup_label (name);
}

/* Define a label, specifying the location in the source file.
   Return the LABEL_DECL node for the label, if the definition is valid.
   Otherwise return 0.  */

tree
define_label (filename, line, name)
     char *filename;
     int line;
     tree name;
{
  tree decl = lookup_label (name);

  /* If label with this name is known from an outer context, shadow it.  */
  if (decl != 0 && DECL_CONTEXT (decl) != current_function_decl)
    {
      shadowed_labels = tree_cons (NULL_TREE, decl, shadowed_labels);
      IDENTIFIER_LABEL_VALUE (name) = 0;
      decl = lookup_label (name);
    }

  if (DECL_INITIAL (decl) != 0)
    {
      error ("duplicate label `%s'", IDENTIFIER_POINTER (name));
      return 0;
    }
  else
    {
      /* Mark label as having been defined.  */
      DECL_INITIAL (decl) = error_mark_node;
      /* Say where in the source.  */
      DECL_SOURCE_FILE (decl) = filename;
      DECL_SOURCE_LINE (decl) = line;
      return decl;
    }
}
    
/* Exit a binding level.
   Pop the level off, and restore the state of the identifier-decl mappings
   that were in effect when this level was entered.

   If KEEP is nonzero, this level had explicit declarations, so
   and create a "block" (a BLOCK node) for the level
   to record its declarations and subblocks for symbol table output.

   If FUNCTIONBODY is nonzero, this level is the body of a function,
   so create a block as if KEEP were set and also clear out all
   label names.

   If REVERSE is nonzero, reverse the order of decls before putting
   them into the BLOCK.  */

tree
poplevel (keep, reverse, functionbody)
     int keep;
     int reverse;
     int functionbody;
{
  register tree link;
  /* The chain of decls was accumulated in reverse order.
     Put it into forward order, just for cleanliness.  */
  tree decls;
  tree tags = current_binding_level->tags;
  tree subblocks = current_binding_level->blocks;
  tree block = 0;
  tree decl;
  int block_previously_created;

  keep |= current_binding_level->keep;

  /* This warning is turned off because it causes warnings for
     declarations like `extern struct foo *x'.  */
#if 0
  /* Warn about incomplete structure types in this level.  */
  for (link = tags; link; link = TREE_CHAIN (link))
    if (TYPE_SIZE (TREE_VALUE (link)) == 0)
      {
	tree type = TREE_VALUE (link);
	tree type_name = TYPE_NAME (type);
	char *id = IDENTIFIER_POINTER (TREE_CODE (type_name) == IDENTIFIER_NODE
				       ? type_name
				       : DECL_NAME (type_name));
	switch (TREE_CODE (type))
	  {
	  case RECORD_TYPE:
	    error ("`struct %s' incomplete in scope ending here", id);
	    break;
	  case UNION_TYPE:
	    error ("`union %s' incomplete in scope ending here", id);
	    break;
	  case ENUMERAL_TYPE:
	    error ("`enum %s' incomplete in scope ending here", id);
	    break;
	  }
      }
#endif /* 0 */

  /* Get the decls in the order they were written.
     Usually current_binding_level->names is in reverse order.
     But parameter decls were previously put in forward order.  */

  if (reverse)
    current_binding_level->names
      = decls = nreverse (current_binding_level->names);
  else
    decls = current_binding_level->names;

  /* Output any nested inline functions within this block
     if they weren't already output.  */

  for (decl = decls; decl; decl = TREE_CHAIN (decl))
    if (TREE_CODE (decl) == FUNCTION_DECL
	&& ! TREE_ASM_WRITTEN (decl)
	&& DECL_INITIAL (decl) != 0
	&& TREE_ADDRESSABLE (decl))
      {
	/* If this decl was copied from a file-scope decl
	   on account of a block-scope extern decl,
	   propagate TREE_ADDRESSABLE to the file-scope decl.

	   DECL_ABSTRACT_ORIGIN can be set to itself if warn_return_type is
	   true, since then the decl goes through save_for_inline_copying.  */
	if (DECL_ABSTRACT_ORIGIN (decl) != 0
	    && DECL_ABSTRACT_ORIGIN (decl) != decl)
	  TREE_ADDRESSABLE (DECL_ABSTRACT_ORIGIN (decl)) = 1;
	else if (DECL_SAVED_INSNS (decl) != 0)
	  {
	    push_function_context ();
	    output_inline_function (decl);
	    pop_function_context ();
	  }
      }

  /* If there were any declarations or structure tags in that level,
     or if this level is a function body,
     create a BLOCK to record them for the life of this function.  */

  block = 0;
  block_previously_created = (current_binding_level->this_block != 0);
  if (block_previously_created)
    block = current_binding_level->this_block;
  else if (keep || functionbody
	   || (current_binding_level->keep_if_subblocks && subblocks != 0))
    block = make_node (BLOCK);
  if (block != 0)
    {
      BLOCK_VARS (block) = decls;
      BLOCK_SUBBLOCKS (block) = subblocks;
    }

  /* In each subblock, record that this is its superior.  */

  for (link = subblocks; link; link = TREE_CHAIN (link))
    BLOCK_SUPERCONTEXT (link) = block;

  /* Clear out the meanings of the local variables of this level.  */

  for (link = decls; link; link = TREE_CHAIN (link))
    {
      if (DECL_NAME (link) != 0)
	{
	  /* If the ident. was used or addressed via a local extern decl,
	     don't forget that fact.  */
	  if (DECL_EXTERNAL (link))
	    {
	      if (TREE_USED (link))
		TREE_USED (DECL_NAME (link)) = 1;
	      if (TREE_ADDRESSABLE (link))
		TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (link)) = 1;
	    }
	  IDENTIFIER_LOCAL_VALUE (DECL_NAME (link)) = 0;
	}
    }

  /* Restore all name-meanings of the outer levels
     that were shadowed by this level.  */

  for (link = current_binding_level->shadowed; link; link = TREE_CHAIN (link))
    IDENTIFIER_LOCAL_VALUE (TREE_PURPOSE (link)) = TREE_VALUE (link);

  /* If the level being exited is the top level of a function,
     check over all the labels, and clear out the current
     (function local) meanings of their names.  */

  if (functionbody)
    {
      clear_limbo_values (block);

      /* If this is the top level block of a function,
	 the vars are the function's parameters.
	 Don't leave them in the BLOCK because they are
	 found in the FUNCTION_DECL instead.  */

      BLOCK_VARS (block) = 0;

      /* Clear out the definitions of all label names,
	 since their scopes end here,
	 and add them to BLOCK_VARS.  */

      for (link = named_labels; link; link = TREE_CHAIN (link))
	{
	  register tree label = TREE_VALUE (link);

	  if (DECL_INITIAL (label) == 0)
	    {
	      error_with_decl (label, "label `%s' used but not defined");
	      /* Avoid crashing later.  */
	      define_label (input_filename, lineno,
			    DECL_NAME (label));
	    }
	  else if (warn_unused && !TREE_USED (label))
	    warning_with_decl (label, "label `%s' defined but not used");
	  IDENTIFIER_LABEL_VALUE (DECL_NAME (label)) = 0;

	  /* Put the labels into the "variables" of the
	     top-level block, so debugger can see them.  */
	  TREE_CHAIN (label) = BLOCK_VARS (block);
	  BLOCK_VARS (block) = label;
	}
    }

  /* Pop the current level, and free the structure for reuse.  */

  {
    register struct binding_level *level = current_binding_level;
    current_binding_level = current_binding_level->level_chain;

    level->level_chain = free_binding_level;
    free_binding_level = level;
  }

  /* Dispose of the block that we just made inside some higher level.  */
  if (functionbody)
    DECL_INITIAL (current_function_decl) = block;
  else if (block)
    {
      if (!block_previously_created)
        current_binding_level->blocks
          = chainon (current_binding_level->blocks, block);
    }
  /* If we did not make a block for the level just exited,
     any blocks made for inner levels
     (since they cannot be recorded as subblocks in that level)
     must be carried forward so they will later become subblocks
     of something else.  */
  else if (subblocks)
    current_binding_level->blocks
      = chainon (current_binding_level->blocks, subblocks);

  /* Set the TYPE_CONTEXTs for all of the tagged types belonging to this
     binding contour so that they point to the appropriate construct, i.e.
     either to the current FUNCTION_DECL node, or else to the BLOCK node
     we just constructed.

     Note that for tagged types whose scope is just the formal parameter
     list for some function type specification, we can't properly set
     their TYPE_CONTEXTs here, because we don't have a pointer to the
     appropriate FUNCTION_TYPE node readily available to us.  For those
     cases, the TYPE_CONTEXTs of the relevant tagged type nodes get set
     in `grokdeclarator' as soon as we have created the FUNCTION_TYPE
     node which will represent the "scope" for these "parameter list local"
     tagged types.
  */

  if (functionbody)
    for (link = tags; link; link = TREE_CHAIN (link))
      TYPE_CONTEXT (TREE_VALUE (link)) = current_function_decl;
  else if (block)
    for (link = tags; link; link = TREE_CHAIN (link))
      TYPE_CONTEXT (TREE_VALUE (link)) = block;

  if (block)
    TREE_USED (block) = 1;
  return block;
}

/* Delete the node BLOCK from the current binding level.
   This is used for the block inside a stmt expr ({...})
   so that the block can be reinserted where appropriate.  */

void
delete_block (block)
     tree block;
{
  tree t;
  if (current_binding_level->blocks == block)
    current_binding_level->blocks = TREE_CHAIN (block);
  for (t = current_binding_level->blocks; t;)
    {
      if (TREE_CHAIN (t) == block)
	TREE_CHAIN (t) = TREE_CHAIN (block);
      else
	t = TREE_CHAIN (t);
    }
  TREE_CHAIN (block) = NULL;
  /* Clear TREE_USED which is always set by poplevel.
     The flag is set again if insert_block is called.  */
  TREE_USED (block) = 0;
}

/* Insert BLOCK at the end of the list of subblocks of the
   current binding level.  This is used when a BIND_EXPR is expanded,
   to handle the BLOCK node inside the BIND_EXPR.  */

void
insert_block (block)
     tree block;
{
  TREE_USED (block) = 1;
  current_binding_level->blocks
    = chainon (current_binding_level->blocks, block);
}

/* Set the BLOCK node for the innermost scope
   (the one we are currently in).  */

void
set_block (block)
     register tree block;
{
  current_binding_level->this_block = block;
}

#else
/* *********** old gcc-2.8.1 version **************************/


/* Enter a new binding level. The input parameter is ignored, but has to be
   specified for back-end compatibility.  */

void
pushlevel (ignore)
     int ignore ATTRIBUTE_UNUSED;
{
  struct binding_level *newlevel
    = (struct binding_level *) xmalloc (sizeof (struct binding_level));

  *newlevel = clear_binding_level;

  /* Add this level to the front of the chain (stack) of levels that are
     active.  */
  newlevel->level_chain = current_binding_level;
  current_binding_level = newlevel;
}

/* Exit a binding level.
   Pop the level off, and restore the state of the identifier-decl mappings
   that were in effect when this level was entered.

   If KEEP is nonzero, this level had explicit declarations, so
   and create a "block" (a BLOCK node) for the level
   to record its declarations and subblocks for symbol table output.

   If FUNCTIONBODY is nonzero, this level is the body of a function,
   so create a block as if KEEP were set and also clear out all
   label names.

   If REVERSE is nonzero, reverse the order of decls before putting
   them into the BLOCK.  */

tree
poplevel (keep, reverse, functionbody)
     int keep;
     int reverse;
     int functionbody;
{
  /* Points to a BLOCK tree node. This is the BLOCK node construted for the
     binding level that we are about to exit and which is returned by this
     routine.  */
  tree block_node = NULL_TREE;
  tree decl_chain;
  tree subblock_chain = current_binding_level->blocks;
  tree subblock_node;
  tree block_created_by_back_end;

  /* Reverse the list of XXXX_DECL nodes if desired.  Note that the ..._DECL
     nodes chained through the `names' field of current_binding_level are in
     reverse order except for PARM_DECL node, which are explicitely stored in
     the right order.  */
  decl_chain = (reverse) ? nreverse (current_binding_level->names)
                         : current_binding_level->names;

  block_created_by_back_end = current_binding_level->block_created_by_back_end;
  if (block_created_by_back_end != 0)
    {
      block_node = block_created_by_back_end;

      /* Check if we are about to discard some information that was gathered
	 by the front-end. Nameley check if the back-end created a new block 
	 without calling pushlevel first. To understand why things are lost
	 just look at the next case (i.e. no block created by back-end.  */
      if ((keep || functionbody) && (decl_chain || subblock_chain))
	abort ();
    }

  /* If there were any declarations in the current binding level, or if this
     binding level is a function body, or if there are any nested blocks then
     create a BLOCK node to record them for the life of this function.  */
  else if (keep || functionbody)
    block_node = build_block (keep ? decl_chain : 0, 0, subblock_chain, 0, 0);

  /* Record the BLOCK node just built as the subblock its enclosing scope.  */
  for (subblock_node = subblock_chain; subblock_node;
       subblock_node = TREE_CHAIN (subblock_node))
    BLOCK_SUPERCONTEXT (subblock_node) = block_node;

  /* Clear out the meanings of the local variables of this level.  */

  for (subblock_node = decl_chain; subblock_node;
       subblock_node = TREE_CHAIN (subblock_node))
    if (DECL_NAME (subblock_node) != 0)
      /* If the identifier was used or addressed via a local extern decl,  
	 don't forget that fact.   */
      if (DECL_EXTERNAL (subblock_node))
	{
	  if (TREE_USED (subblock_node))
	    TREE_USED (DECL_NAME (subblock_node)) = 1;
	  if (TREE_ADDRESSABLE (subblock_node))
	    TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (subblock_node)) = 1;
	}

  /* Pop the current level.  */
  current_binding_level = current_binding_level->level_chain;

  if (functionbody)
    {
      /* This is the top level block of a function. The ..._DECL chain stored
	 in BLOCK_VARS are the function's parameters (PARM_DECL nodes). Don't
	 leave them in the BLOCK because they are found in the FUNCTION_DECL
	 instead.  */
      DECL_INITIAL (current_function_decl) = block_node;
      BLOCK_VARS (block_node) = 0;
    }
  else if (block_node)
    {
      if (block_created_by_back_end == NULL)
	current_binding_level->blocks
	  = chainon (current_binding_level->blocks, block_node);
    }

  /* If we did not make a block for the level just exited, any blocks made for
     inner levels (since they cannot be recorded as subblocks in that level)
     must be carried forward so they will later become subblocks of something
     else.  */
  else if (subblock_chain)
    current_binding_level->blocks
      = chainon (current_binding_level->blocks, subblock_chain);
  if (block_node)
    TREE_USED (block_node) = 1;

  return block_node;
}

/* Insert BLOCK at the end of the list of subblocks of the
   current binding level.  This is used when a BIND_EXPR is expanded,
   to handle the BLOCK node inside the BIND_EXPR.  */

void
insert_block (block)
     tree block;
{
  TREE_USED (block) = 1;
  current_binding_level->blocks
    = chainon (current_binding_level->blocks, block);
}

/* Set the BLOCK node for the innermost scope
   (the one we are currently in).  */

void
set_block (block)
     tree block;
{
  current_binding_level->block_created_by_back_end = block;
}
#endif

/* Look up NAME in the current binding level and its superiors
   in the namespace of variables, functions and typedefs.
   Return a ..._DECL node of some kind representing its definition,
   or return 0 if it is undefined.  */

tree
lookup_name (name)
     tree name;
{
  register tree val;
  if (current_binding_level != global_binding_level
      && IDENTIFIER_LOCAL_VALUE (name))
    val = IDENTIFIER_LOCAL_VALUE (name);
  else
    val = IDENTIFIER_GLOBAL_VALUE (name);
  return val;
}

/* Similar to `lookup_name' but look only at current binding level.  */

tree
lookup_name_current_level (name)
     tree name;
{
  register tree t;

  if (current_binding_level == global_binding_level)
    return IDENTIFIER_GLOBAL_VALUE (name);

  if (IDENTIFIER_LOCAL_VALUE (name) == 0)
    return 0;

  for (t = current_binding_level->names; t; t = TREE_CHAIN (t))
    if (DECL_NAME (t) == name)
      return t;

  return 0;
}

/* Return zero if the declaration NEWDECL is valid
   when the declaration OLDDECL (assumed to be for the same name)
   has already been seen.
   Otherwise return 1 if NEWDECL is a redefinition, 2 if it is a redeclaration,
   and 3 if it is a conflicting declaration.  */

static int
redeclaration_error_message (newdecl, olddecl)
     tree newdecl, olddecl;
{
  if (TREE_CODE (newdecl) == TYPE_DECL)
    {
      if (flag_traditional && TREE_TYPE (newdecl) == TREE_TYPE (olddecl))
	return 0;
      /* pushdecl creates distinct types for TYPE_DECLs by calling
	 build_type_copy, so the above comparison generally fails.  We do
	 another test against the TYPE_MAIN_VARIANT of the olddecl, which
	 is equivalent to what this code used to do before the build_type_copy
	 call.  The variant type distinction should not matter for traditional
	 code, because it doesn't have type qualifiers.  */
      if (flag_traditional 
	  && TYPE_MAIN_VARIANT (TREE_TYPE (olddecl)) == TREE_TYPE (newdecl))
	return 0;
      if (DECL_IN_SYSTEM_HEADER (olddecl) || DECL_IN_SYSTEM_HEADER (newdecl))
	return 0;
      return 1;
    }
  else if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      /* Declarations of functions can insist on internal linkage
	 but they can't be inconsistent with internal linkage,
	 so there can be no error on that account.
	 However defining the same name twice is no good.  */
      if (DECL_INITIAL (olddecl) != 0 && DECL_INITIAL (newdecl) != 0
	  /* However, defining once as extern inline and a second
	     time in another way is ok.  */
	  && !(DECL_INLINE (olddecl) && DECL_EXTERNAL (olddecl)
	       && !(DECL_INLINE (newdecl) && DECL_EXTERNAL (newdecl))))
	return 1;
      return 0;
    }
  else if (current_binding_level == global_binding_level)
    {
      /* Objects declared at top level:  */
      /* If at least one is a reference, it's ok.  */
      if (DECL_EXTERNAL (newdecl) || DECL_EXTERNAL (olddecl))
	return 0;
      /* Reject two definitions.  */
      if (DECL_INITIAL (olddecl) != 0 && DECL_INITIAL (newdecl) != 0)
	return 1;
      /* Now we have two tentative defs, or one tentative and one real def.  */
      /* Insist that the linkage match.  */
      if (TREE_PUBLIC (olddecl) != TREE_PUBLIC (newdecl))
	return 3;
      return 0;
    }
  else if (current_binding_level->parm_flag
	   && TREE_ASM_WRITTEN (olddecl) && !TREE_ASM_WRITTEN (newdecl))
    return 0;
  else
    {
      /* Newdecl has block scope.  If olddecl has block scope also, then
	 reject two definitions, and reject a definition together with an
	 external reference.  Otherwise, it is OK, because newdecl must
	 be an extern reference to olddecl.  */
      if (!(DECL_EXTERNAL (newdecl) && DECL_EXTERNAL (olddecl))
	  && DECL_CONTEXT (newdecl) == DECL_CONTEXT (olddecl))
	return 2;
      return 0;
    }
}

/* Handle when a new declaration NEWDECL
   has the same name as an old one OLDDECL
   in the same binding contour.
   Prints an error message if appropriate.

   If safely possible, alter OLDDECL to look like NEWDECL, and return 1.
   Otherwise, return 0.

   When DIFFERENT_BINDING_LEVEL is true, NEWDECL is an external declaration,
   and OLDDECL is in an outer binding level and should thus not be changed.  */

static int
duplicate_decls (newdecl, olddecl, different_binding_level)
     register tree newdecl, olddecl;
     int different_binding_level;
{
  int types_match = comptypes (TREE_TYPE (newdecl), TREE_TYPE (olddecl));
  int new_is_definition = (TREE_CODE (newdecl) == FUNCTION_DECL
			   && DECL_INITIAL (newdecl) != 0);
  tree oldtype = TREE_TYPE (olddecl);
  tree newtype = TREE_TYPE (newdecl);
  int errmsg = 0;

  /*
   * --fixme-- gaius does M2 ever use this function, if so why?
   */

  if (TREE_CODE_CLASS (TREE_CODE (olddecl)) == 'd')
    DECL_MACHINE_ATTRIBUTES (newdecl)
      =  merge_machine_decl_attributes (olddecl, newdecl);

  if (TREE_CODE (newtype) == ERROR_MARK
      || TREE_CODE (oldtype) == ERROR_MARK)
    types_match = 0;

  /* New decl is completely inconsistent with the old one =>
     tell caller to replace the old one.
     This is always an error except in the case of shadowing a builtin.  */
  if (TREE_CODE (olddecl) != TREE_CODE (newdecl))
    {
      if (TREE_CODE (olddecl) == FUNCTION_DECL
	  && (DECL_BUILT_IN (olddecl)
	      || DECL_BUILT_IN_NONANSI (olddecl)))
	{
	  /* If you declare a built-in or predefined function name as static,
	     the old definition is overridden,
	     but optionally warn this was a bad choice of name.  */
	  if (!TREE_PUBLIC (newdecl))
	    {
	      if (!warn_shadow)
		;
	      else if (DECL_BUILT_IN (olddecl))
		warning_with_decl (newdecl, "shadowing built-in function `%s'");
	      else
		warning_with_decl (newdecl, "shadowing library function `%s'");
	    }
	  /* Likewise, if the built-in is not ansi, then programs can
	     override it even globally without an error.  */
	  else if (! DECL_BUILT_IN (olddecl))
	    warning_with_decl (newdecl,
			       "library function `%s' declared as non-function");

	  else if (DECL_BUILT_IN_NONANSI (olddecl))
	    warning_with_decl (newdecl,
			       "built-in function `%s' declared as non-function");
	  else
	    warning_with_decl (newdecl,
			     "built-in function `%s' declared as non-function");
	}
      else
	{
	  error_with_decl (newdecl, "`%s' redeclared as different kind of symbol");
	  error_with_decl (olddecl, "previous declaration of `%s'");
	}

      return 0;
    }

  /* For real parm decl following a forward decl,
     return 1 so old decl will be reused.  */
  if (types_match && TREE_CODE (newdecl) == PARM_DECL
      && TREE_ASM_WRITTEN (olddecl) && ! TREE_ASM_WRITTEN (newdecl))
    return 1;

  /* The new declaration is the same kind of object as the old one.
     The declarations may partially match.  Print warnings if they don't
     match enough.  Ultimately, copy most of the information from the new
     decl to the old one, and keep using the old one.  */

  if (flag_traditional && TREE_CODE (newdecl) == FUNCTION_DECL
      && IDENTIFIER_IMPLICIT_DECL (DECL_NAME (newdecl)) == olddecl
      && DECL_INITIAL (olddecl) == 0)
    /* If -traditional, avoid error for redeclaring fcn
       after implicit decl.  */
    ;
  else if (TREE_CODE (olddecl) == FUNCTION_DECL
	   && DECL_BUILT_IN (olddecl))
    {
      /* A function declaration for a built-in function.  */
      if (!TREE_PUBLIC (newdecl))
	{
	  /* If you declare a built-in function name as static, the
	     built-in definition is overridden,
	     but optionally warn this was a bad choice of name.  */
	  if (warn_shadow)
	    warning_with_decl (newdecl, "shadowing built-in function `%s'");
	  /* Discard the old built-in function.  */
	  return 0;
	}
      else if (!types_match)
	{
          /* Accept the return type of the new declaration if same modes.  */
	  tree oldreturntype = TREE_TYPE (oldtype);
	  tree newreturntype = TREE_TYPE (newtype);

	  /* Make sure we put the new type in the same obstack as the old ones.
	     If the old types are not both in the same obstack, use the
	     permanent one.  */
	  if (TYPE_OBSTACK (oldtype) == TYPE_OBSTACK (newtype))
	    push_obstacks (TYPE_OBSTACK (oldtype), TYPE_OBSTACK (oldtype));
	  else
	    {
	      push_obstacks_nochange ();
	      end_temporary_allocation ();
	    }

          if (TYPE_MODE (oldreturntype) == TYPE_MODE (newreturntype))
            {
	      /* Function types may be shared, so we can't just modify
		 the return type of olddecl's function type.  */
	      tree trytype
		= build_function_type (newreturntype,
				       TYPE_ARG_TYPES (oldtype));
	      
              types_match = comptypes (newtype, trytype);
	      if (types_match)
		oldtype = trytype;
	    }
	  /* Accept harmless mismatch in first argument type also.
	     This is for ffs.  */
	  if (TYPE_ARG_TYPES (TREE_TYPE (newdecl)) != 0
	      && TYPE_ARG_TYPES (oldtype) != 0
	      && TREE_VALUE (TYPE_ARG_TYPES (newtype)) != 0
	      && TREE_VALUE (TYPE_ARG_TYPES (oldtype)) != 0
	      && (TYPE_MODE (TREE_VALUE (TYPE_ARG_TYPES (newtype)))
		  == TYPE_MODE (TREE_VALUE (TYPE_ARG_TYPES (oldtype)))))
	    {
	      /* Function types may be shared, so we can't just modify
		 the return type of olddecl's function type.  */
	      tree trytype
		= build_function_type (TREE_TYPE (oldtype),
				       tree_cons (NULL_TREE, 
						  TREE_VALUE (TYPE_ARG_TYPES (newtype)),
						  TREE_CHAIN (TYPE_ARG_TYPES (oldtype))));
	      
              types_match = comptypes (newtype, trytype);
	      if (types_match)
		oldtype = trytype;
	    }
	  if (! different_binding_level)
	    TREE_TYPE (olddecl) = oldtype;

	  pop_obstacks ();
	}
      if (!types_match)
	{
	  /* If types don't match for a built-in, throw away the built-in.  */
	  warning_with_decl (newdecl, "conflicting types for built-in function `%s'");
	  return 0;
	}
    }
  else if (TREE_CODE (olddecl) == FUNCTION_DECL
	   && DECL_SOURCE_LINE (olddecl) == 0)
    {
      /* A function declaration for a predeclared function
	 that isn't actually built in.  */
      if (!TREE_PUBLIC (newdecl))
	{
	  /* If you declare it as static, the
	     default definition is overridden.  */
	  return 0;
	}
      else if (!types_match)
	{
	  /* If the types don't match, preserve volatility indication.
	     Later on, we will discard everything else about the
	     default declaration.  */
	  TREE_THIS_VOLATILE (newdecl) |= TREE_THIS_VOLATILE (olddecl);
	}
    }
#if NOT_NEEDED_BY_M2
  /* Permit char *foo () to match void *foo (...) if not pedantic,
     if one of them came from a system header file.  */
  else if (!types_match
	   && TREE_CODE (olddecl) == FUNCTION_DECL
	   && TREE_CODE (newdecl) == FUNCTION_DECL
	   && TREE_CODE (TREE_TYPE (oldtype)) == POINTER_TYPE
	   && TREE_CODE (TREE_TYPE (newtype)) == POINTER_TYPE
	   && (DECL_IN_SYSTEM_HEADER (olddecl)
	       || DECL_IN_SYSTEM_HEADER (newdecl))
	   && ((TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (newtype))) == void_type_node
		&& TYPE_ARG_TYPES (oldtype) == 0
		&& self_promoting_args_p (TYPE_ARG_TYPES (newtype))
		&& TREE_TYPE (TREE_TYPE (oldtype)) == char_type_node)
	       ||
	       (TREE_TYPE (TREE_TYPE (newtype)) == char_type_node
		&& TYPE_ARG_TYPES (newtype) == 0
		&& self_promoting_args_p (TYPE_ARG_TYPES (oldtype))
		&& TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (oldtype))) == void_type_node)))
    {
      if (pedantic)
	pedwarn_with_decl (newdecl, "conflicting types for `%s'");
      /* Make sure we keep void * as ret type, not char *.  */
      if (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (oldtype))) == void_type_node)
	TREE_TYPE (newdecl) = newtype = oldtype;

      /* Set DECL_IN_SYSTEM_HEADER, so that if we see another declaration
	 we will come back here again.  */
      DECL_IN_SYSTEM_HEADER (newdecl) = 1;
    }
  else if (!types_match
	   /* Permit char *foo (int, ...); followed by char *foo ();
	      if not pedantic.  */
	   && ! (TREE_CODE (olddecl) == FUNCTION_DECL
		 && ! pedantic
		 /* Return types must still match.  */
		 && comptypes (TREE_TYPE (oldtype),
			       TREE_TYPE (newtype))
		 && TYPE_ARG_TYPES (newtype) == 0))
    {
      error_with_decl (newdecl, "conflicting types for `%s'");
      /* Check for function type mismatch
	 involving an empty arglist vs a nonempty one.  */
      if (TREE_CODE (olddecl) == FUNCTION_DECL
	  && comptypes (TREE_TYPE (oldtype),
			TREE_TYPE (newtype))
	  && ((TYPE_ARG_TYPES (oldtype) == 0
	       && DECL_INITIAL (olddecl) == 0)
	      ||
	      (TYPE_ARG_TYPES (newtype) == 0
	       && DECL_INITIAL (newdecl) == 0)))
	{
	  /* Classify the problem further.  */
	  register tree t = TYPE_ARG_TYPES (oldtype);
	  if (t == 0)
	    t = TYPE_ARG_TYPES (newtype);
	  for (; t; t = TREE_CHAIN (t))
	    {
	      register tree type = TREE_VALUE (t);

	      if (TREE_CHAIN (t) == 0
		  && TYPE_MAIN_VARIANT (type) != void_type_node)
		{
		  error ("A parameter list with an ellipsis can't match an empty parameter name list declaration.");
		  break;
		}

	      if (simple_type_promotes_to (type) != NULL_TREE)
		{
		  error ("An argument type that has a default promotion can't match an empty parameter name list declaration.");
		  break;
		}
	    }
	}
      error_with_decl (olddecl, "previous declaration of `%s'");
    }
#endif
  else
    {
      errmsg = redeclaration_error_message (newdecl, olddecl);
      if (errmsg)
	{
	  switch (errmsg)
	    {
	    case 1:
	      error_with_decl (newdecl, "redefinition of `%s'");
	      break;
	    case 2:
	      error_with_decl (newdecl, "redeclaration of `%s'");
	      break;
	    case 3:
	      error_with_decl (newdecl, "conflicting declarations of `%s'");
	      break;
	    default:
	      abort ();
	    }

	  error_with_decl (olddecl,
			   ((DECL_INITIAL (olddecl)
			     && current_binding_level == global_binding_level)
			    ? "`%s' previously defined here"
			    : "`%s' previously declared here"));
	}
      else if (TREE_CODE (newdecl) == TYPE_DECL
               && (DECL_IN_SYSTEM_HEADER (olddecl) 
                   || DECL_IN_SYSTEM_HEADER (newdecl)))
	{
	  warning_with_decl (newdecl, "redefinition of `%s'");
	  warning_with_decl 
	    (olddecl,
	     ((DECL_INITIAL (olddecl)
	       && current_binding_level == global_binding_level)
	      ? "`%s' previously defined here"
	      : "`%s' previously declared here"));
	}
      else if (TREE_CODE (olddecl) == FUNCTION_DECL
	       && DECL_INITIAL (olddecl) != 0
	       && TYPE_ARG_TYPES (oldtype) == 0
	       && TYPE_ARG_TYPES (newtype) != 0
	       && TYPE_ACTUAL_ARG_TYPES (oldtype) != 0)
	{
	  register tree type, parm;
	  register int nargs;
	  /* Prototype decl follows defn w/o prototype.  */

	  for (parm = TYPE_ACTUAL_ARG_TYPES (oldtype),
	       type = TYPE_ARG_TYPES (newtype),
	       nargs = 1;
	       ;
	       parm = TREE_CHAIN (parm), type = TREE_CHAIN (type), nargs++)
	    {
	      if (TYPE_MAIN_VARIANT (TREE_VALUE (parm)) == void_type_node
		  && TYPE_MAIN_VARIANT (TREE_VALUE (type)) == void_type_node)
		{
		  warning_with_decl (newdecl, "prototype for `%s' follows");
		  warning_with_decl (olddecl, "non-prototype definition here");
		  break;
		}
	      if (TYPE_MAIN_VARIANT (TREE_VALUE (parm)) == void_type_node
		  || TYPE_MAIN_VARIANT (TREE_VALUE (type)) == void_type_node)
		{
		  error_with_decl (newdecl, "prototype for `%s' follows and number of arguments doesn't match");
		  error_with_decl (olddecl, "non-prototype definition here");
		  errmsg = 1;
		  break;
		}
	      /* Type for passing arg must be consistent
		 with that declared for the arg.  */
	      if (! comptypes (TREE_VALUE (parm), TREE_VALUE (type))
		  /* If -traditional, allow `unsigned int' instead of `int'
		     in the prototype.  */
		  && (! (flag_traditional
			 && TYPE_MAIN_VARIANT (TREE_VALUE (parm)) == integer_type_node
			 && TYPE_MAIN_VARIANT (TREE_VALUE (type)) == unsigned_type_node)))
		{
		  error_with_decl (newdecl,
				   "prototype for `%s' follows and argument %d doesn't match",
				   nargs);
		  error_with_decl (olddecl, "non-prototype definition here");
		  errmsg = 1;
		  break;
		}
	    }
	}
      /* Warn about mismatches in various flags.  */
      else
	{
	  /* Warn if function is now inline
	     but was previously declared not inline and has been called.  */
	  if (TREE_CODE (olddecl) == FUNCTION_DECL
	      && ! DECL_INLINE (olddecl) && DECL_INLINE (newdecl)
	      && TREE_USED (olddecl))
	    warning_with_decl (newdecl,
			       "`%s' declared inline after being called");
	  if (TREE_CODE (olddecl) == FUNCTION_DECL
	      && ! DECL_INLINE (olddecl) && DECL_INLINE (newdecl)
	      && DECL_INITIAL (olddecl) != 0)
	    warning_with_decl (newdecl,
			       "`%s' declared inline after its definition");

	  /* If pedantic, warn when static declaration follows a non-static
	     declaration.  Otherwise, do so only for functions.  */
	  if ((pedantic || TREE_CODE (olddecl) == FUNCTION_DECL)
	      && TREE_PUBLIC (olddecl)
	      && !TREE_PUBLIC (newdecl))
	    warning_with_decl (newdecl, "static declaration for `%s' follows non-static");

	  /* Warn when const declaration follows a non-const
	     declaration, but not for functions.  */
	  if (TREE_CODE (olddecl) != FUNCTION_DECL
	      && !TREE_READONLY (olddecl)
	      && TREE_READONLY (newdecl))
	    warning_with_decl (newdecl, "const declaration for `%s' follows non-const");
	  /* These bits are logically part of the type, for variables.
	     But not for functions
	     (where qualifiers are not valid ANSI anyway).  */
	  else if (pedantic && TREE_CODE (olddecl) != FUNCTION_DECL
	      && (TREE_READONLY (newdecl) != TREE_READONLY (olddecl)
		  || TREE_THIS_VOLATILE (newdecl) != TREE_THIS_VOLATILE (olddecl)))
	    pedwarn_with_decl (newdecl, "type qualifiers for `%s' conflict with previous decl");
	}
    }

  /* Optionally warn about more than one declaration for the same name.  */
  if (errmsg == 0 && warn_redundant_decls && DECL_SOURCE_LINE (olddecl) != 0
      /* Don't warn about a function declaration
	 followed by a definition.  */
      && !(TREE_CODE (newdecl) == FUNCTION_DECL && DECL_INITIAL (newdecl) != 0
	   && DECL_INITIAL (olddecl) == 0)
      /* Don't warn about extern decl followed by (tentative) definition.  */
      && !(DECL_EXTERNAL (olddecl) && ! DECL_EXTERNAL (newdecl)))
    {
      warning_with_decl (newdecl, "redundant redeclaration of `%s' in same scope");
      warning_with_decl (olddecl, "previous declaration of `%s'");
    }

  /* Copy all the DECL_... slots specified in the new decl
     except for any that we copy here from the old type.

     Past this point, we don't change OLDTYPE and NEWTYPE
     even if we change the types of NEWDECL and OLDDECL.  */

  if (types_match)
    {
      /* When copying info to olddecl, we store into write_olddecl
	 instead.  This allows us to avoid modifying olddecl when
	 different_binding_level is true.  */
      tree write_olddecl = different_binding_level ? newdecl : olddecl;

      /* Make sure we put the new type in the same obstack as the old ones.
	 If the old types are not both in the same obstack, use the permanent
	 one.  */
      if (TYPE_OBSTACK (oldtype) == TYPE_OBSTACK (newtype))
	push_obstacks (TYPE_OBSTACK (oldtype), TYPE_OBSTACK (oldtype));
      else
	{
	  push_obstacks_nochange ();
	  end_temporary_allocation ();
	}
		       
      /* Merge the data types specified in the two decls.  */
      if (TREE_CODE (newdecl) != FUNCTION_DECL || !DECL_BUILT_IN (olddecl))
	{
	  if (different_binding_level)
	    TREE_TYPE (newdecl)
	      = build_type_attribute_variant
		(newtype,
		 merge_attributes (TYPE_ATTRIBUTES (newtype),
				   TYPE_ATTRIBUTES (oldtype)));
	  else
	    TREE_TYPE (newdecl)
	      = TREE_TYPE (olddecl)
		= common_type (newtype, oldtype);
	}

      /* Lay the type out, unless already done.  */
      if (oldtype != TREE_TYPE (newdecl))
	{
	  if (TREE_TYPE (newdecl) != error_mark_node)
	    layout_type (TREE_TYPE (newdecl));
	  if (TREE_CODE (newdecl) != FUNCTION_DECL
	      && TREE_CODE (newdecl) != TYPE_DECL
	      && TREE_CODE (newdecl) != CONST_DECL)
	    layout_decl (newdecl, 0);
	}
      else
	{
	  /* Since the type is OLDDECL's, make OLDDECL's size go with.  */
	  DECL_SIZE (newdecl) = DECL_SIZE (olddecl);
	  DECL_MODE (newdecl) = DECL_MODE (olddecl);
	  if (TREE_CODE (olddecl) != FUNCTION_DECL)
	    if (DECL_ALIGN (olddecl) > DECL_ALIGN (newdecl))
	      DECL_ALIGN (newdecl) = DECL_ALIGN (olddecl);
	}

      /* Keep the old rtl since we can safely use it.  */
      DECL_RTL (newdecl) = DECL_RTL (olddecl);

      /* Merge the type qualifiers.  */
      if (DECL_BUILT_IN_NONANSI (olddecl) && TREE_THIS_VOLATILE (olddecl)
	  && !TREE_THIS_VOLATILE (newdecl))
	TREE_THIS_VOLATILE (write_olddecl) = 0;
      if (TREE_READONLY (newdecl))
	TREE_READONLY (write_olddecl) = 1;
      if (TREE_THIS_VOLATILE (newdecl))
	{
	  TREE_THIS_VOLATILE (write_olddecl) = 1;
	  if (TREE_CODE (newdecl) == VAR_DECL)
	    make_var_volatile (newdecl);
	}

      /* Keep source location of definition rather than declaration.  */
      /* When called with different_binding_level set, keep the old
	 information so that meaningful diagnostics can be given.  */
      if (DECL_INITIAL (newdecl) == 0 && DECL_INITIAL (olddecl) != 0
	  && ! different_binding_level)
	{
	  DECL_SOURCE_LINE (newdecl) = DECL_SOURCE_LINE (olddecl);
	  DECL_SOURCE_FILE (newdecl) = DECL_SOURCE_FILE (olddecl);
	}

      /* Merge the unused-warning information.  */
      if (DECL_IN_SYSTEM_HEADER (olddecl))
	DECL_IN_SYSTEM_HEADER (newdecl) = 1;
      else if (DECL_IN_SYSTEM_HEADER (newdecl))
	DECL_IN_SYSTEM_HEADER (write_olddecl) = 1;

      /* Merge the initialization information.  */
      /* When called with different_binding_level set, don't copy over
	 DECL_INITIAL, so that we don't accidentally change function
	 declarations into function definitions.  */
      if (DECL_INITIAL (newdecl) == 0 && ! different_binding_level)
	DECL_INITIAL (newdecl) = DECL_INITIAL (olddecl);

      /* Merge the section attribute.
         We want to issue an error if the sections conflict but that must be
	 done later in decl_attributes since we are called before attributes
	 are assigned.  */
      if (DECL_SECTION_NAME (newdecl) == NULL_TREE)
	DECL_SECTION_NAME (newdecl) = DECL_SECTION_NAME (olddecl);

      /* Copy the assembler name.
	 Currently, it can only be defined in the prototype.  */
      DECL_ASSEMBLER_NAME (newdecl) = DECL_ASSEMBLER_NAME (olddecl);

      if (TREE_CODE (newdecl) == FUNCTION_DECL)
	{
	  DECL_STATIC_CONSTRUCTOR(newdecl) |= DECL_STATIC_CONSTRUCTOR(olddecl);
	  DECL_STATIC_DESTRUCTOR (newdecl) |= DECL_STATIC_DESTRUCTOR (olddecl);

	  DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (newdecl)
	    |= DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (olddecl);
	  DECL_NO_CHECK_MEMORY_USAGE (newdecl)
	    |= DECL_NO_CHECK_MEMORY_USAGE (olddecl);
	  DECL_NO_LIMIT_STACK (newdecl)
	    |= DECL_NO_LIMIT_STACK (olddecl);
	}

      pop_obstacks ();
    }
  /* If cannot merge, then use the new type and qualifiers,
     and don't preserve the old rtl.  */
  else if (! different_binding_level)
    {
      TREE_TYPE (olddecl) = TREE_TYPE (newdecl);
      TREE_READONLY (olddecl) = TREE_READONLY (newdecl);
      TREE_THIS_VOLATILE (olddecl) = TREE_THIS_VOLATILE (newdecl);
      TREE_SIDE_EFFECTS (olddecl) = TREE_SIDE_EFFECTS (newdecl);
    }

  /* Merge the storage class information.  */
  DECL_WEAK (newdecl) |= DECL_WEAK (olddecl);	  
  /* For functions, static overrides non-static.  */
  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      TREE_PUBLIC (newdecl) &= TREE_PUBLIC (olddecl);
      /* This is since we don't automatically
	 copy the attributes of NEWDECL into OLDDECL.  */
      /* No need to worry about different_binding_level here because
	 then TREE_PUBLIC (newdecl) was true.  */
      TREE_PUBLIC (olddecl) = TREE_PUBLIC (newdecl);
      /* If this clears `static', clear it in the identifier too.  */
      if (! TREE_PUBLIC (olddecl))
	TREE_PUBLIC (DECL_NAME (olddecl)) = 0;
    }
  if (DECL_EXTERNAL (newdecl))
    {
      TREE_STATIC (newdecl) = TREE_STATIC (olddecl);
      DECL_EXTERNAL (newdecl) = DECL_EXTERNAL (olddecl);
      /* An extern decl does not override previous storage class.  */
      TREE_PUBLIC (newdecl) = TREE_PUBLIC (olddecl);
      if (! DECL_EXTERNAL (newdecl))
	DECL_CONTEXT (newdecl) = DECL_CONTEXT (olddecl);
    }
  else
    {
      TREE_STATIC (olddecl) = TREE_STATIC (newdecl);
      TREE_PUBLIC (olddecl) = TREE_PUBLIC (newdecl);
    }

  /* If either decl says `inline', this fn is inline,
     unless its definition was passed already.  */
  if (DECL_INLINE (newdecl) && DECL_INITIAL (olddecl) == 0)
    DECL_INLINE (olddecl) = 1;
  DECL_INLINE (newdecl) = DECL_INLINE (olddecl);

  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      if (DECL_BUILT_IN (olddecl))
	{
	  /* Get rid of any built-in function if new arg types don't match it
	     or if we have a function definition.  */
	  if (! types_match || new_is_definition)
	    {
	      if (! different_binding_level)
		{
		  TREE_TYPE (olddecl) = TREE_TYPE (newdecl);
		  DECL_BUILT_IN_CLASS (olddecl) = NOT_BUILT_IN;
		}
	    }
	  else
	    {
	      /* If redeclaring a builtin function, and not a definition,
		 it stays built in.  */
	      DECL_BUILT_IN_CLASS (newdecl) = DECL_BUILT_IN_CLASS (olddecl);
	      DECL_FUNCTION_CODE (newdecl) = DECL_FUNCTION_CODE (olddecl);
	    }
	}
      /* Also preserve various other info from the definition.  */
      else if (! new_is_definition)
	DECL_FRAME_SIZE (newdecl) = DECL_FRAME_SIZE (olddecl);
      if (! new_is_definition)
	{
	  DECL_RESULT (newdecl) = DECL_RESULT (olddecl);
	  /* When called with different_binding_level set, don't copy over
	     DECL_INITIAL, so that we don't accidentally change function
	     declarations into function definitions.  */
	  if (! different_binding_level)
	    DECL_INITIAL (newdecl) = DECL_INITIAL (olddecl);
	  DECL_SAVED_INSNS (newdecl) = DECL_SAVED_INSNS (olddecl);
	  DECL_ARGUMENTS (newdecl) = DECL_ARGUMENTS (olddecl);
	  if (DECL_INLINE (newdecl))
	    DECL_ABSTRACT_ORIGIN (newdecl) = DECL_ORIGIN (olddecl);
	}
    }
  if (different_binding_level)
    {
      /* Don't output a duplicate symbol or debugging information for this
	 declaration.

	 Do not set TREE_ASM_WRITTEN for a FUNCTION_DECL since we may actually
	 just have two declarations without a definition.  VAR_DECLs may need
	 the same treatment, I'm not sure.  */
      if (TREE_CODE (newdecl) == FUNCTION_DECL)
	DECL_IGNORED_P (newdecl) = 1;
      else
	TREE_ASM_WRITTEN (newdecl) = DECL_IGNORED_P (newdecl) = 1;
      return 0;
    }

  /* Copy most of the decl-specific fields of NEWDECL into OLDDECL.
     But preserve OLDDECL's DECL_UID.  */
  {
    register unsigned olddecl_uid = DECL_UID (olddecl);

    bcopy ((char *) newdecl + sizeof (struct tree_common),
	   (char *) olddecl + sizeof (struct tree_common),
	   sizeof (struct tree_decl) - sizeof (struct tree_common));
    DECL_UID (olddecl) = olddecl_uid;
  }

  /* NEWDECL contains the merged attribute lists.
     Update OLDDECL to be the same.  */
  DECL_MACHINE_ATTRIBUTES (olddecl) = DECL_MACHINE_ATTRIBUTES (newdecl);

  return 1;
}

/* Records a ..._DECL node DECL as belonging to the current lexical scope.
   Returns the ..._DECL node. */

/* Record a decl-node X as belonging to the current lexical scope.
   Check for errors (such as an incompatible declaration for the same
   name already seen in the same scope).

   Returns either X or an old decl for the same name.
   If an old decl is returned, it may have been smashed
   to agree with what X says.  */

tree
pushdecl (x)
     tree x;
{
  register tree t;
  register tree name = DECL_NAME (x);
  register struct binding_level *b = current_binding_level;

  DECL_CONTEXT (x) = current_function_decl;
  /* A local extern declaration for a function doesn't constitute nesting.
     A local auto declaration does, since it's a forward decl
     for a nested function coming later.  */
  if (TREE_CODE (x) == FUNCTION_DECL && DECL_INITIAL (x) == 0
      && DECL_EXTERNAL (x))
    DECL_CONTEXT (x) = 0;

  if (warn_nested_externs && DECL_EXTERNAL (x) && b != global_binding_level
      && x != IDENTIFIER_IMPLICIT_DECL (name)
      /* Don't print error messages for __FUNCTION__ and __PRETTY_FUNCTION__ */
      && !DECL_IN_SYSTEM_HEADER (x))
    warning ("nested extern declaration of `%s'", IDENTIFIER_POINTER (name));

  if (name)
    {
      char *file;
      int line;
      int different_binding_level = 0;

      t = lookup_name_current_level (name);
      /* Don't type check externs here when -traditional.  This is so that
	 code with conflicting declarations inside blocks will get warnings
	 not errors.  X11 for instance depends on this.  */
      if (! t && DECL_EXTERNAL (x) && TREE_PUBLIC (x) && ! flag_traditional)
	{
	  t = IDENTIFIER_GLOBAL_VALUE (name);
	  /* Type decls at global scope don't conflict with externs declared
	     inside lexical blocks.  */
	  if (t && TREE_CODE (t) == TYPE_DECL)
	    t = 0;
	  different_binding_level = 1;
	}
      if (t != 0 && t == error_mark_node)
	/* error_mark_node is 0 for a while during initialization!  */
	{
	  t = 0;
	  error_with_decl (x, "`%s' used prior to declaration");
	}

      if (t != 0)
	{
	  file = DECL_SOURCE_FILE (t);
	  line = DECL_SOURCE_LINE (t);
	}

      /* If this decl is `static' and an implicit decl was seen previously,
	 warn.  But don't complain if -traditional,
	 since traditional compilers don't complain.  */
      if (! flag_traditional && TREE_PUBLIC (name)
	  /* Don't test for DECL_EXTERNAL, because grokdeclarator
	     sets this for all functions.  */
	  && ! TREE_PUBLIC (x)
	  && (TREE_CODE (x) == FUNCTION_DECL || b == global_binding_level)
	  /* We used to warn also for explicit extern followed by static,
	     but sometimes you need to do it that way.  */
	  && IDENTIFIER_IMPLICIT_DECL (name) != 0)
	{
	  pedwarn ("`%s' was declared implicitly `extern' and later `static'",
		   IDENTIFIER_POINTER (name));
	  pedwarn_with_file_and_line
	    (DECL_SOURCE_FILE (IDENTIFIER_IMPLICIT_DECL (name)),
	     DECL_SOURCE_LINE (IDENTIFIER_IMPLICIT_DECL (name)),
	     "previous declaration of `%s'",
	     IDENTIFIER_POINTER (name));
	  TREE_THIS_VOLATILE (name) = 1;
	}

      if (t != 0 && duplicate_decls (x, t, different_binding_level))
	{
	  if (TREE_CODE (t) == PARM_DECL)
	    {
	      /* Don't allow more than one "real" duplicate
		 of a forward parm decl.  */
	      TREE_ASM_WRITTEN (t) = TREE_ASM_WRITTEN (x);
	      return t;
	    }
	  return t;
	}

      /* If we are processing a typedef statement, generate a whole new
	 ..._TYPE node (which will be just an variant of the existing
	 ..._TYPE node with identical properties) and then install the
	 TYPE_DECL node generated to represent the typedef name as the
	 TYPE_NAME of this brand new (duplicate) ..._TYPE node.

	 The whole point here is to end up with a situation where each
	 and every ..._TYPE node the compiler creates will be uniquely
	 associated with AT MOST one node representing a typedef name.
	 This way, even though the compiler substitutes corresponding
	 ..._TYPE nodes for TYPE_DECL (i.e. "typedef name") nodes very
	 early on, later parts of the compiler can always do the reverse
	 translation and get back the corresponding typedef name.  For
	 example, given:

		typedef struct S MY_TYPE;
		MY_TYPE object;

	 Later parts of the compiler might only know that `object' was of
	 type `struct S' if it were not for code just below.  With this
	 code however, later parts of the compiler see something like:

		struct S' == struct S
		typedef struct S' MY_TYPE;
		struct S' object;

	 And they can then deduce (from the node for type struct S') that
	 the original object declaration was:

		MY_TYPE object;

	 Being able to do this is important for proper support of protoize,
	 and also for generating precise symbolic debugging information
	 which takes full account of the programmer's (typedef) vocabulary.

         Obviously, we don't want to generate a duplicate ..._TYPE node if
	 the TYPE_DECL node that we are now processing really represents a
	 standard built-in type.

         Since all standard types are effectively declared at line zero
         in the source file, we can easily check to see if we are working
         on a standard type by checking the current value of lineno.  */

      if (TREE_CODE (x) == TYPE_DECL)
        {
          if (DECL_SOURCE_LINE (x) == 0)
            {
	      if (TYPE_NAME (TREE_TYPE (x)) == 0)
	        TYPE_NAME (TREE_TYPE (x)) = x;
            }
          else if (TREE_TYPE (x) != error_mark_node
		   && DECL_ORIGINAL_TYPE (x) == NULL_TREE)
            {
              tree tt = TREE_TYPE (x);
	      DECL_ORIGINAL_TYPE (x) = tt;
              tt = build_type_copy (tt);
              TYPE_NAME (tt) = x;
              TREE_TYPE (x) = tt;
            }
        }

      /* Multiple external decls of the same identifier ought to match.
	 Check against both global declarations (when traditional) and out of
	 scope (limbo) block level declarations.

	 We get warnings about inline functions where they are defined.
	 Avoid duplicate warnings where they are used.  */
      if (TREE_PUBLIC (x) && ! DECL_INLINE (x))
	{
	  tree decl;

	  if (flag_traditional && IDENTIFIER_GLOBAL_VALUE (name) != 0
	      && (DECL_EXTERNAL (IDENTIFIER_GLOBAL_VALUE (name))
		  || TREE_PUBLIC (IDENTIFIER_GLOBAL_VALUE (name))))
	    decl = IDENTIFIER_GLOBAL_VALUE (name);
	  else if (IDENTIFIER_LIMBO_VALUE (name) != 0)
	    /* Decls in limbo are always extern, so no need to check that.  */
	    decl = IDENTIFIER_LIMBO_VALUE (name);
	  else
	    decl = 0;

	  if (decl && ! comptypes (TREE_TYPE (x), TREE_TYPE (decl))
	      /* If old decl is built-in, we already warned if we should.  */
	      && !DECL_BUILT_IN (decl))
	    {
	      pedwarn_with_decl (x,
				 "type mismatch with previous external decl");
	      pedwarn_with_decl (decl, "previous external decl of `%s'");
	    }
	}

      /* If a function has had an implicit declaration, and then is defined,
	 make sure they are compatible.  */

      if (IDENTIFIER_IMPLICIT_DECL (name) != 0
	  && IDENTIFIER_GLOBAL_VALUE (name) == 0
	  && TREE_CODE (x) == FUNCTION_DECL
	  && ! comptypes (TREE_TYPE (x),
			  TREE_TYPE (IDENTIFIER_IMPLICIT_DECL (name))))
	{
	  warning_with_decl (x, "type mismatch with previous implicit declaration");
	  warning_with_decl (IDENTIFIER_IMPLICIT_DECL (name),
			     "previous implicit declaration of `%s'");
	}

#if NOT_NEEDED_BY_M2
      /* In PCC-compatibility mode, extern decls of vars with no current decl
	 take effect at top level no matter where they are.  */
      if (flag_traditional && DECL_EXTERNAL (x)
	  && lookup_name (name) == 0)
	{
	  tree type = TREE_TYPE (x);

	  /* But don't do this if the type contains temporary nodes.  */
	  while (type)
	    {
	      if (type == error_mark_node)
		break;
	      if (! TYPE_CONTEXT (type))
		{
		  warning_with_decl (x, "type of external `%s' is not global");
		  /* By exiting the loop early, we leave TYPE nonzero,
		     and thus prevent globalization of the decl.  */
		  break;
		}
	      else if (TREE_CODE (type) == FUNCTION_TYPE
		       && TYPE_ARG_TYPES (type) != 0)
		/* The types might not be truly local,
		   but the list of arg types certainly is temporary.
		   Since prototypes are nontraditional,
		   ok not to do the traditional thing.  */
		break;
	      type = TREE_TYPE (type);
	    }

	  if (type == 0)
	    b = global_binding_level;
	}
#endif

      /* This name is new in its binding level.
	 Install the new declaration and return it.  */
      if (b == global_binding_level)
	{
	  /* Install a global value.  */
	  
	  /* If the first global decl has external linkage,
	     warn if we later see static one.  */
	  if (IDENTIFIER_GLOBAL_VALUE (name) == 0 && TREE_PUBLIC (x))
	    TREE_PUBLIC (name) = 1;

	  IDENTIFIER_GLOBAL_VALUE (name) = x;

	  /* We no longer care about any previous block level declarations.  */
	  IDENTIFIER_LIMBO_VALUE (name) = 0;

	  /* Don't forget if the function was used via an implicit decl.  */
	  if (IDENTIFIER_IMPLICIT_DECL (name)
	      && TREE_USED (IDENTIFIER_IMPLICIT_DECL (name)))
	    TREE_USED (x) = 1, TREE_USED (name) = 1;

	  /* Don't forget if its address was taken in that way.  */
	  if (IDENTIFIER_IMPLICIT_DECL (name)
	      && TREE_ADDRESSABLE (IDENTIFIER_IMPLICIT_DECL (name)))
	    TREE_ADDRESSABLE (x) = 1;

	  /* Warn about mismatches against previous implicit decl.  */
	  if (IDENTIFIER_IMPLICIT_DECL (name) != 0
	      /* If this real decl matches the implicit, don't complain.  */
	      && ! (TREE_CODE (x) == FUNCTION_DECL
		    && (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (x)))
			== integer_type_node)))
	    pedwarn ("`%s' was previously implicitly declared to return `int'",
		     IDENTIFIER_POINTER (name));

	  /* If this decl is `static' and an `extern' was seen previously,
	     that is erroneous.  */
	  if (TREE_PUBLIC (name)
	      && ! TREE_PUBLIC (x) && ! DECL_EXTERNAL (x))
	    {
	      /* Okay to redeclare an ANSI built-in as static.  */
	      if (t != 0 && DECL_BUILT_IN (t))
		;
	      /* Okay to declare a non-ANSI built-in as anything.  */
	      else if (t != 0 && DECL_BUILT_IN_NONANSI (t))
		;
	      /* Okay to have global type decl after an earlier extern
		 declaration inside a lexical block.  */
	      else if (TREE_CODE (x) == TYPE_DECL)
		;
	      else if (IDENTIFIER_IMPLICIT_DECL (name))
		{
		  if (! TREE_THIS_VOLATILE (name))
		    pedwarn ("`%s' was declared implicitly `extern' and later `static'",
			     IDENTIFIER_POINTER (name));
		}
	      else
		pedwarn ("`%s' was declared `extern' and later `static'",
			 IDENTIFIER_POINTER (name));
	    }
	}
      else
	{
	  /* Here to install a non-global value.  */
	  tree oldlocal = IDENTIFIER_LOCAL_VALUE (name);
	  tree oldglobal = IDENTIFIER_GLOBAL_VALUE (name);
	  IDENTIFIER_LOCAL_VALUE (name) = x;

	  /* If this is an extern function declaration, see if we
	     have a global definition or declaration for the function.  */
	  if (oldlocal == 0
	      && DECL_EXTERNAL (x) && !DECL_INLINE (x)
	      && oldglobal != 0
	      && TREE_CODE (x) == FUNCTION_DECL
	      && TREE_CODE (oldglobal) == FUNCTION_DECL)
	    {
	      /* We have one.  Their types must agree.  */
	      if (! comptypes (TREE_TYPE (x),
			       TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (name))))
		pedwarn_with_decl (x, "extern declaration of `%s' doesn't match global one");
	      else
		{
		  /* Inner extern decl is inline if global one is.
		     Copy enough to really inline it.  */
		  if (DECL_INLINE (oldglobal))
		    {
		      DECL_INLINE (x) = DECL_INLINE (oldglobal);
		      DECL_INITIAL (x) = (current_function_decl == oldglobal
					  ? 0 : DECL_INITIAL (oldglobal));
		      DECL_SAVED_INSNS (x) = DECL_SAVED_INSNS (oldglobal);
		      DECL_FRAME_SIZE (x) = DECL_FRAME_SIZE (oldglobal);
		      DECL_ARGUMENTS (x) = DECL_ARGUMENTS (oldglobal);
		      DECL_RESULT (x) = DECL_RESULT (oldglobal);
		      TREE_ASM_WRITTEN (x) = TREE_ASM_WRITTEN (oldglobal);
		      DECL_ABSTRACT_ORIGIN (x) = DECL_ORIGIN (oldglobal);
		    }
		  /* Inner extern decl is built-in if global one is.  */
		  if (DECL_BUILT_IN (oldglobal))
		    {
		      DECL_BUILT_IN_CLASS (x) = DECL_BUILT_IN_CLASS (oldglobal);
		      DECL_FUNCTION_CODE (x) = DECL_FUNCTION_CODE (oldglobal);
		    }
		  /* Keep the arg types from a file-scope fcn defn.  */
		  if (TYPE_ARG_TYPES (TREE_TYPE (oldglobal)) != 0
		      && DECL_INITIAL (oldglobal)
		      && TYPE_ARG_TYPES (TREE_TYPE (x)) == 0)
		    TREE_TYPE (x) = TREE_TYPE (oldglobal);
		}
	    }

#if 0 /* This case is probably sometimes the right thing to do.  */
	  /* If we have a local external declaration,
	     then any file-scope declaration should not
	     have been static.  */
	  if (oldlocal == 0 && oldglobal != 0
	      && !TREE_PUBLIC (oldglobal)
	      && DECL_EXTERNAL (x) && TREE_PUBLIC (x))
	    warning ("`%s' locally external but globally static",
		     IDENTIFIER_POINTER (name));
#endif

	  /* If we have a local external declaration,
	     and no file-scope declaration has yet been seen,
	     then if we later have a file-scope decl it must not be static.  */
	  if (oldlocal == 0
	      && DECL_EXTERNAL (x)
	      && TREE_PUBLIC (x))
	    {
	      if (oldglobal == 0)
	        TREE_PUBLIC (name) = 1;

	      /* Save this decl, so that we can do type checking against
		 other decls after it falls out of scope.

		 Only save it once.  This prevents temporary decls created in
		 expand_inline_function from being used here, since this
		 will have been set when the inline function was parsed.
		 It also helps give slightly better warnings.  */
	      if (IDENTIFIER_LIMBO_VALUE (name) == 0)
		IDENTIFIER_LIMBO_VALUE (name) = x;
	    }

	  /* Warn if shadowing an argument at the top level of the body.  */
	  if (oldlocal != 0 && !DECL_EXTERNAL (x)
	      /* This warning doesn't apply to the parms of a nested fcn.  */
	      && ! current_binding_level->parm_flag
	      /* Check that this is one level down from the parms.  */
	      && current_binding_level->level_chain->parm_flag
	      /* Check that the decl being shadowed
		 comes from the parm level, one level up.  */
	      && chain_member (oldlocal, current_binding_level->level_chain->names))
	    {
	      if (TREE_CODE (oldlocal) == PARM_DECL)
		pedwarn ("declaration of `%s' shadows a parameter",
			 IDENTIFIER_POINTER (name));
	      else
		pedwarn ("declaration of `%s' shadows a symbol from the parameter list",
			 IDENTIFIER_POINTER (name));
	    }

	  /* Maybe warn if shadowing something else.  */
	  else if (warn_shadow && !DECL_EXTERNAL (x)
		   /* No shadow warnings for internally generated vars.  */
		   && DECL_SOURCE_LINE (x) != 0
		   /* No shadow warnings for vars made for inlining.  */
		   && ! DECL_FROM_INLINE (x))
	    {
	      char *id = IDENTIFIER_POINTER (name);

	      if (TREE_CODE (x) == PARM_DECL
		  && current_binding_level->level_chain->parm_flag)
		/* Don't warn about the parm names in function declarator
		   within a function declarator.
		   It would be nice to avoid warning in any function
		   declarator in a declaration, as opposed to a definition,
		   but there is no way to tell it's not a definition.  */
		;
	      else if (oldlocal != 0 && TREE_CODE (oldlocal) == PARM_DECL)
		warning ("declaration of `%s' shadows a parameter", id);
	      else if (oldlocal != 0)
		warning ("declaration of `%s' shadows previous local", id);
	      else if (IDENTIFIER_GLOBAL_VALUE (name) != 0
		       && IDENTIFIER_GLOBAL_VALUE (name) != error_mark_node)
		warning ("declaration of `%s' shadows global declaration", id);
	    }

	  /* If storing a local value, there may already be one (inherited).
	     If so, record it for restoration when this binding level ends.  */
	  if (oldlocal != 0)
	    b->shadowed = tree_cons (name, oldlocal, b->shadowed);
	}

      /* Keep count of variables in this level with incomplete type.  */
      if (TYPE_SIZE (TREE_TYPE (x)) == 0)
	++b->n_incomplete;
    }

  /* Put decls on list in reverse order.
     We will reverse them later if necessary.  */
  TREE_CHAIN (x) = b->names;
  b->names = x;

  return x;
}

/* Like pushdecl, only it places X in GLOBAL_BINDING_LEVEL, if appropriate.  */

tree
pushdecl_top_level (x)
     tree x;
{
  register tree t;
  register struct binding_level *b = current_binding_level;

  current_binding_level = global_binding_level;
  t = pushdecl (x);
  current_binding_level = b;
  return t;
}

/* used by print-tree.c */

void
lang_print_xnode (file, node, indent)
     FILE *file ATTRIBUTE_UNUSED;
     tree node ATTRIBUTE_UNUSED;
     int indent ATTRIBUTE_UNUSED;
{
}

/* Mark ARG for GC.  */
void
lang_mark_false_label_stack (arg)
     struct label_node *arg;
{
  /* C doesn't use false_label_stack.  It better be NULL.  */
  if (arg != NULL)
    abort();
}

/* Mark the language specific bits in T for GC.  */
void
lang_mark_tree (t)
     tree t;
{
  if (TREE_CODE (t) == IDENTIFIER_NODE)
    {
      struct lang_identifier *i = (struct lang_identifier *) t;
      ggc_mark_tree (i->global_value);
      ggc_mark_tree (i->local_value);
      ggc_mark_tree (i->label_value);
      ggc_mark_tree (i->implicit_decl);
      ggc_mark_tree (i->error_locus);
      ggc_mark_tree (i->limbo_value);
    }
  else if (TYPE_P (t) && TYPE_LANG_SPECIFIC (t))
    ggc_mark (TYPE_LANG_SPECIFIC (t));
}

/* Hook used by expand_expr to expand language-specific tree codes.  */

static rtx
gm2_expand_expr (exp, target, tmode, modifier)
     tree exp;
     rtx target;
     enum machine_mode tmode ATTRIBUTE_UNUSED;
     enum expand_modifier modifier ATTRIBUTE_UNUSED;
{
  switch (TREE_CODE (exp))
    {
    case ERROR_MARK:
      return( NULL_RTX );
      break;

    case IDENTIFIER_NODE:
    case TREE_LIST:
      if (exp == boolean_false_node) {
	return( const0_rtx );
      } else if (exp == boolean_true_node) {
	return( const1_rtx );
      }
      /* fall through */
    default:
      fprintf(stderr, "gm2 front end has been asked to expand the following expression\n");
      debug_tree(exp);
      fprintf(stderr, "halting\n");
      do_abort();
      return( target );
    }
}

/*
 * initialize the lang_expand_expr function
 */

void
init_gm2_expand ()
{
  lang_expand_expr     = gm2_expand_expr;
#if 0
  lang_expand_constant = gm2_expand_constant;  /* eventually should handle the string constants? */
#endif
}

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
    DECL_ASSEMBLER_NAME (decl) = get_identifier (library_name);
  make_decl_rtl (decl, NULL_PTR, 1);
  pushdecl (decl);
  DECL_BUILT_IN_CLASS (decl) = class;
  DECL_FUNCTION_CODE (decl) = function_code;

  /* Warn if a function in the namespace for users
     is used without an occasion to consider it declared.  */
  if (name[0] != '_' || name[1] != '_')
    C_DECL_ANTICIPATED (decl) = 1;

  return decl;
}

/* init_m2_builtins - build tree nodes and builtin functions for GNU Modula-2
 */

void
init_m2_builtins ()
{
  tree memcpy_ftype;
  tree alloca_ftype;
  tree sizetype_endlink;
  tree endlink;

  endlink = void_list_node;
  sizetype_endlink = tree_cons (NULL_TREE, sizetype, endlink);
  /* Prototype for memcpy.  */
  memcpy_ftype
    = build_function_type (ptr_type_node,
			   tree_cons (NULL_TREE, ptr_type_node,
				      tree_cons (NULL_TREE, const_ptr_type_node,
						 sizetype_endlink)));

  /* Currently under experimentation.  */
  gm2_memcpy_node = builtin_function ("__builtin_memcpy", memcpy_ftype, BUILT_IN_MEMCPY,
				      BUILT_IN_NORMAL, "memcpy");

  alloca_ftype     = build_function_type (ptr_type_node, sizetype_endlink);

  gm2_alloca_node  = builtin_function ("__builtin_alloca", alloca_ftype,
				       BUILT_IN_ALLOCA, BUILT_IN_NORMAL, "alloca");
}

/* Create the predefined scalar types such as `integer_type_node' needed 
   in the gcc back-end and initialize the global binding level.  */

void
init_decl_processing ()
{
#if 0
  register tree endlink;
  /* Either char* or void*.  */
  tree traditional_ptr_type_node;
  /* Data types of memcpy and strlen.  */
  tree memcpy_ftype, memset_ftype, strlen_ftype;
  tree void_ftype_any, ptr_ftype_void, ptr_ftype_ptr;
  int wchar_type_size;
#endif
  tree array_domain_type;

  current_function_decl = NULL;
  current_binding_level = NULL_BINDING_LEVEL;
  free_binding_level    = NULL_BINDING_LEVEL;
  pushlevel (0);	/* make the binding_level structure for global names */
  global_binding_level  = current_binding_level;

  /* Define `int' and `char' first so that dbx will output them first.  */

  /* INTEGER */
  integer_type_node = make_signed_type (INT_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier("int"),
			integer_type_node));

  /* CHAR */
  char_type_node = make_unsigned_type(CHAR_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("char"),
			char_type_node));

#if 1
  sizetype = type_for_size (GET_MODE_BITSIZE (Pmode), 1);
  /* pushdecl (build_decl (TYPE_DECL, get_identifier (SIZE_TYPE), sizetype)); */
#endif

  error_mark_node = make_node (ERROR_MARK);
  TREE_TYPE (error_mark_node) = error_mark_node;

  /* VOID */
  void_type_node = make_node (VOID_TYPE);
  layout_type (void_type_node);
  TYPE_ALIGN (void_type_node) = BITS_PER_UNIT;

  /* PROC */
  proc_type_node = build_pointer_type(build_function_type (void_type_node, NULL_TREE));
  layout_type (proc_type_node);
  TYPE_ALIGN (proc_type_node) = BITS_PER_UNIT;

  /* ADDRESS */
  ptr_type_node = build_pointer_type (void_type_node);

  /* BYTE */
  unsigned_char_type_node = make_unsigned_type(CHAR_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned char"),
			unsigned_char_type_node));

  /* internal */
  long_unsigned_type_node = make_unsigned_type(LONG_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned long"),
			long_unsigned_type_node));

  /* REAL */

  float_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (float_type_node) = FLOAT_TYPE_SIZE;
  pushdecl (build_decl (TYPE_DECL, get_identifier("float"),
			float_type_node));
  layout_type (float_type_node);

  /* LONGREAL */

  double_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (double_type_node) = DOUBLE_TYPE_SIZE;
  pushdecl (build_decl (TYPE_DECL, get_identifier("double"),
			double_type_node));
  layout_type (double_type_node);

  /* LONGINT */
  long_long_integer_type_node = make_signed_type (LONG_LONG_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long long int"),
			long_long_integer_type_node));

  /* LONGCARD */
  long_long_unsigned_type_node = make_unsigned_type (LONG_LONG_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned long long"),
			long_long_unsigned_type_node));

  /* some others to keep gcc happy */
  long_integer_type_node = make_signed_type (LONG_TYPE_SIZE);
#if 0
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long int"),
			long_integer_type_node));
#endif

  long_double_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (long_double_type_node) = LONG_DOUBLE_TYPE_SIZE;
#if 0
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long double"),
			long_double_type_node));
#endif
  layout_type (long_double_type_node);

  /* WORD  &  CARDINAL */
  unsigned_type_node = make_unsigned_type (INT_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned int"),
			unsigned_type_node));

  signed_char_type_node = make_signed_type (CHAR_TYPE_SIZE);
#if 0
  pushdecl (build_decl (TYPE_DECL, get_identifier ("signed char"),
			signed_char_type_node));
#endif

  short_unsigned_type_node = make_unsigned_type (SHORT_TYPE_SIZE);
#if 0
  pushdecl (build_decl (TYPE_DECL, get_identifier ("short unsigned int"),
			short_unsigned_type_node));
#endif

  short_integer_type_node = make_signed_type (SHORT_TYPE_SIZE);
#if 0
  pushdecl (build_decl (TYPE_DECL, get_identifier ("short int"),
			short_integer_type_node));
#endif

  ptrdiff_type_node
    = TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (get_identifier (PTRDIFF_TYPE)));

  /* CONSTANTS */

  integer_zero_node = build_int_2 (0, 0);
  integer_one_node = build_int_2 (1, 0);

  size_zero_node = build_int_2 (0, 0);
  TREE_TYPE (size_zero_node) = sizetype;
  size_one_node = build_int_2 (1, 0);
  TREE_TYPE (size_one_node) = sizetype;

  /* NIL */

  null_pointer_node = build_int_2 (0, 0);
  TREE_TYPE (null_pointer_node) = build_pointer_type (void_type_node);
  layout_type (TREE_TYPE (null_pointer_node));

#if defined(NOT_NEEDED_IN_M2)
  boolean_type_node  = integer_type_node;
  boolean_true_node  = integer_one_node;
  boolean_false_node = integer_zero_node;
#endif

  TREE_TYPE (TYPE_SIZE (integer_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (char_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (unsigned_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (long_unsigned_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (long_integer_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (long_long_integer_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (long_long_unsigned_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (short_integer_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (short_unsigned_type_node)) = sizetype;

  /* Make a type to be the domain of a few array types
     whose domains don't really matter.
     200 is small enough that it always fits in size_t
     and large enough that it can hold most function names for the
     initializations of __FUNCTION__ and __PRETTY_FUNCTION__.  */
  array_domain_type = build_index_type (build_int_2 (200, 0));

  /* make a type for arrays of characters.
     With luck nothing will ever really depend on the length of this
     array type.  */
  char_array_type_node
    = build_array_type (char_type_node, array_domain_type);

  init_m2_builtins();

  init_gm2_expand ();
}

/* Decode all the language specific options that cannot be decoded by GCC. The
   option decoding phase of GCC calls this routine on the flags that it cannot
   decode.  Return 1 if successful, otherwise return 0. */

int
lang_decode_option (argc, argv)
     int argc ATTRIBUTE_UNUSED;
     char **argv ATTRIBUTE_UNUSED;
{
#if 0
  fprintf(stderr, "asked to decode arg = %s\n", argv[0]);
#endif
  if (strcmp(argv[0], "-Wreturn") == 0) {
    return( TRUE );
  } else if (strcmp(argv[0], "-Wbounds") == 0) {
    return( TRUE );
  } else if (strcmp(argv[0], "-Wquiet") == 0) {
    return( TRUE );
  }
  return FALSE;
}

/* Perform all the initialization steps that are language-specific.  */

void
lang_init ()
{}

/* Perform all the finalization steps that are language-specific.  */

void
lang_finish ()
{}

char *
init_parse (filename)
     char *filename;
{
  /* The structure `tree_identifier' is the GCC tree data structure that holds
     IDENTIFIER_NODE nodes. We need to call `set_identifier_size' to tell GCC
     that we have not added any language specific fields to IDENTIFIER_NODE
     nodes.  */

  set_identifier_size (sizeof (struct lang_identifier));
  lineno = 0;
  return( filename );
}

void
finish_parse ()
{}

/* Return a short string identifying this language to the debugger.  */

const char *
lang_identify ()
{ return "m2"; }

/* If DECL has a cleanup, build and return that cleanup here.
   This is a callback called by expand_expr.  */

tree
maybe_build_cleanup (decl)
     tree decl ATTRIBUTE_UNUSED;
{ return NULL_TREE; }

/* Print an error message for invalid use of an incomplete type.  */

void
incomplete_type_error (dont_care_1, dont_care_2)
     tree dont_care_1 ATTRIBUTE_UNUSED, dont_care_2 ATTRIBUTE_UNUSED;
{ abort (); }

tree
truthvalue_conversion (expr)
     tree expr;
{ return expr;}

/* Print any language-specific compilation statistics.  */

void
print_lang_statistics ()
{}

void
lang_init_options ()
{
  /* Mark as "unspecified".  */
  /* flag_bounds_check = -1; */
}

/* Since we don't use the DECL_LANG_SPECIFIC field, this is a no-op.  */

void
copy_lang_decl (node)
     tree node ATTRIBUTE_UNUSED;
{}

/* Hooks for print-tree.c:  */

void
print_lang_decl (file, node, indent)
     FILE *file ATTRIBUTE_UNUSED;
     tree node ATTRIBUTE_UNUSED;
     int indent ATTRIBUTE_UNUSED;
{}

void
print_lang_type (file, node, indent)
     FILE *file ATTRIBUTE_UNUSED;
     tree node ATTRIBUTE_UNUSED;
     int indent ATTRIBUTE_UNUSED;
{}

void
print_lang_identifier (file, node, indent)
     FILE *file ATTRIBUTE_UNUSED;
     tree node ATTRIBUTE_UNUSED;
     int indent ATTRIBUTE_UNUSED;
{}

/* Performs whatever initialization steps are needed by the language-dependent
   lexical analyzer.  */

void
init_lex ()
{
}

/* Sets some debug flags for the parser. It does nothing here.  */

void
set_yydebug (value)
     int value ATTRIBUTE_UNUSED;
{}

/* Routine to print parse error message.  */

void
yyerror (str)
     char *str;
{
  fprintf (stderr, "%s\n", str);
}


int
yyparse (void)
{
  gccgm2front(save_argc, save_argv);
  return( 0 );
}

/* Methods for storing and printing names for error messages.  */

/* Implement a spelling stack that allows components of a name to be pushed
   and popped.  Each element on the stack is this structure.  */

struct spelling
{
  int kind;
  union
    {
      int i;
      char *s;
    } u;
};

#define SPELLING_STRING 1
#define SPELLING_MEMBER 2
#define SPELLING_BOUNDS 3

static struct spelling *spelling;	/* Next stack element (unused).  */
static struct spelling *spelling_base;	/* Spelling stack base.  */
#if 0
static int spelling_size;		/* Size of the spelling stack.  */
#endif

/* Macros to save and restore the spelling stack around push_... functions.
   Alternative to SAVE_SPELLING_STACK.  */

#define SPELLING_DEPTH() (spelling - spelling_base)
#define RESTORE_SPELLING_DEPTH(depth) (spelling = spelling_base + depth)

/* Save and restore the spelling stack around arbitrary C code.  */

#define SAVE_SPELLING_DEPTH(code)		\
{						\
  int __depth = SPELLING_DEPTH ();		\
  code;						\
  RESTORE_SPELLING_DEPTH (__depth);		\
}

/* Push an element on the spelling stack with type KIND and assign VALUE
   to MEMBER.  */

#define PUSH_SPELLING(KIND, VALUE, MEMBER)				\
{									\
  int depth = SPELLING_DEPTH ();					\
									\
  if (depth >= spelling_size)						\
    {									\
      spelling_size += 10;						\
      if (spelling_base == 0)						\
	spelling_base							\
	  = (struct spelling *) xmalloc (spelling_size * sizeof (struct spelling));	\
      else								\
        spelling_base							\
	  = (struct spelling *) xrealloc (spelling_base,		\
					  spelling_size * sizeof (struct spelling));	\
      RESTORE_SPELLING_DEPTH (depth);					\
    }									\
									\
  spelling->kind = (KIND);						\
  spelling->MEMBER = (VALUE);						\
  spelling++;								\
}

#if 0
/* Push STRING on the stack.  Printed literally.  */

static void
push_string (string)
     char *string;
{
  PUSH_SPELLING (SPELLING_STRING, string, u.s);
}

/* Push a member name on the stack.  Printed as '.' STRING.  */

static void
push_member_name (decl)
     tree decl;
     
{
  char *string
    = DECL_NAME (decl) ? IDENTIFIER_POINTER (DECL_NAME (decl)) : "<anonymous>";
  PUSH_SPELLING (SPELLING_MEMBER, string, u.s);
}

/* Push an array bounds on the stack.  Printed as [BOUNDS].  */

static void
push_array_bounds (bounds)
     int bounds;
{
  PUSH_SPELLING (SPELLING_BOUNDS, bounds, u.i);
}
#endif

/* Compute the maximum size in bytes of the printed spelling.  */

static int
spelling_length ()
{
  register int size = 0;
  register struct spelling *p;

  for (p = spelling_base; p < spelling; p++)
    {
      if (p->kind == SPELLING_BOUNDS)
	size += 25;
      else
	size += strlen (p->u.s) + 1;
    }

  return size;
}

/* Print the spelling to BUFFER and return it.  */

static char *
print_spelling (buffer)
     register char *buffer;
{
  register char *d = buffer;
  register char *s;
  register struct spelling *p;

  for (p = spelling_base; p < spelling; p++)
    if (p->kind == SPELLING_BOUNDS)
      {
	sprintf (d, "[%d]", p->u.i);
	d += strlen (d);
      }
    else
      {
	if (p->kind == SPELLING_MEMBER)
	  *d++ = '.';
	for (s = p->u.s; (*d = *s++); d++)
	  ;
      }
  *d++ = '\0';
  return buffer;
}

/* Provide a means to pass component names derived from the spelling stack.  */

char initialization_message;

/* Interpret the spelling of the given ERRTYPE message.  */

static char *
get_spelling (errtype)
     char *errtype;
{
  static char *buffer;
  static int size = -1;

  if (errtype == &initialization_message)
    {
      /* Avoid counting chars */
      static char message[] = "initialization of `%s'";
      register int needed = sizeof (message) + spelling_length () + 1;
      char *temp;

      if (size < 0)
	buffer = (char *) xmalloc (size = needed);
      if (needed > size)
	buffer = (char *) xrealloc (buffer, size = needed);

      temp = (char *) alloca (needed);
      sprintf (buffer, message, print_spelling (temp));
      return buffer;
    }

  return errtype;
}


static
tree
build_c_type_variant (type, constp, volatilep)
     tree type;
     int constp, volatilep;
{
  if (TREE_CODE (type) == ARRAY_TYPE)
    return build_array_type (build_c_type_variant (TREE_TYPE (type),
						   constp, volatilep),
			     TYPE_DOMAIN (type));
  return build_type_variant (type, constp, volatilep);
}

/* Return a variant of TYPE which has all the type qualifiers of LIKE
   as well as those of TYPE.  */

static tree
qualify_type (type, like)
     tree type, like;
{
  int constflag = TYPE_READONLY (type) || TYPE_READONLY (like);
  int volflag = TYPE_VOLATILE (type) || TYPE_VOLATILE (like);
  return build_c_type_variant (type, constflag, volflag);
}

/* Return the common type of two types.
   We assume that comptypes has already been done and returned 1;
   if that isn't so, this may crash.  In particular, we assume that qualifiers
   match.

   This is the type for the result of most arithmetic operations
   if the operands have the given two types.  */

tree
common_type (t1, t2)
     tree t1, t2;
{
  register enum tree_code code1;
  register enum tree_code code2;
  tree attributes;

  /* Extend subranges of ordinal types to their full types.
   * Otherwise, operations leaving the range won't work.
   */
  if (ORDINAL_TYPE (TREE_CODE (t1)) && TREE_TYPE (t1))
    t1 = TREE_TYPE (t1);
  if (ORDINAL_TYPE (TREE_CODE (t2)) && TREE_TYPE (t2))
    t2 = TREE_TYPE (t2);

  /* Save time if the two types are the same.  */

  if (t1 == t2) return t1;

  /* If one type is nonsense, use the other.  */
  if (t1 == error_mark_node)
    return t2;
  if (t2 == error_mark_node)
    return t1;

  /* Merge the attributes */
  attributes = merge_attributes (TYPE_ATTRIBUTES (t1), TYPE_ATTRIBUTES (t2));

  /* Treat an enum type as the unsigned integer type of the same width.  */

  if (TREE_CODE (t1) == ENUMERAL_TYPE)
    t1 = type_for_size (TYPE_PRECISION (t1), 1);
  if (TREE_CODE (t2) == ENUMERAL_TYPE)
    t2 = type_for_size (TYPE_PRECISION (t2), 1);

  code1 = TREE_CODE (t1);
  code2 = TREE_CODE (t2);

  /* If one type is complex, form the common type of the non-complex
     components, then make that complex.  Use T1 or T2 if it is the
     required type.  */
  if (code1 == COMPLEX_TYPE || code2 == COMPLEX_TYPE)
    {
      tree subtype1 = code1 == COMPLEX_TYPE ? TREE_TYPE (t1) : t1;
      tree subtype2 = code2 == COMPLEX_TYPE ? TREE_TYPE (t2) : t2;
      tree subtype = common_type (subtype1, subtype2);

      if (code1 == COMPLEX_TYPE && TREE_TYPE (t1) == subtype)
	return build_type_attribute_variant (t1, attributes);
      else if (code2 == COMPLEX_TYPE && TREE_TYPE (t2) == subtype)
	return build_type_attribute_variant (t2, attributes);
      else
	return build_type_attribute_variant (build_complex_type (subtype),
					     attributes);
    }

  switch (code1)
    {
    case INTEGER_TYPE:
    case REAL_TYPE:
      /* If only one is complex, use that for the result type */
      if (code2 == COMPLEX_TYPE)
	  return build_type_attribute_variant (t2, attributes);

      /* If only one is real, use it as the result.  */

      if (code1 == REAL_TYPE && code2 != REAL_TYPE)
	return build_type_attribute_variant (t1, attributes);

      if (code2 == REAL_TYPE && code1 != REAL_TYPE)
	return build_type_attribute_variant (t2, attributes);

      /* Both real or both integers; use the one with greater precision.  */

      if (TYPE_PRECISION (t1) > TYPE_PRECISION (t2))
	return build_type_attribute_variant (t1, attributes);
      else if (TYPE_PRECISION (t2) > TYPE_PRECISION (t1))
	return build_type_attribute_variant (t2, attributes);

      /* Same precision.  Prefer longs to ints even when same size.  */

      if (TYPE_MAIN_VARIANT (t1) == long_unsigned_type_node
	  || TYPE_MAIN_VARIANT (t2) == long_unsigned_type_node)
	return build_type_attribute_variant (long_unsigned_type_node,
					     attributes);

      if (TYPE_MAIN_VARIANT (t1) == long_integer_type_node
	  || TYPE_MAIN_VARIANT (t2) == long_integer_type_node)
	{
	  /* But preserve unsignedness from the other type,
	     since long cannot hold all the values of an unsigned int.  */
	  if (TREE_UNSIGNED (t1) || TREE_UNSIGNED (t2))
	     t1 = long_unsigned_type_node;
	  else
	     t1 = long_integer_type_node;
	  return build_type_attribute_variant (t1, attributes);
	}

      /* Likewise, prefer long double to double even if same size.  */
      if (TYPE_MAIN_VARIANT (t1) == long_double_type_node
	  || TYPE_MAIN_VARIANT (t2) == long_double_type_node)
	return build_type_attribute_variant (long_double_type_node,
					     attributes);

      /* Otherwise prefer the unsigned one.  */

      if (TREE_UNSIGNED (t1))
	return build_type_attribute_variant (t1, attributes);
      else
	return build_type_attribute_variant (t2, attributes);

    /* Since comptypes checks that code2 is ok, return this. */
    case COMPLEX_TYPE:
      return build_type_attribute_variant (t1, attributes);

    case SET_TYPE:
      return build_type_attribute_variant (t1, attributes);

    case POINTER_TYPE:
      /* For two pointers, do this recursively on the target type,
	 and combine the qualifiers of the two types' targets.  */
      /* This code was turned off; I don't know why.
	 But ANSI C specifies doing this with the qualifiers.
	 So I turned it on again.  */
      {
	tree target = common_type (TYPE_MAIN_VARIANT (TREE_TYPE (t1)),
				   TYPE_MAIN_VARIANT (TREE_TYPE (t2)));
	int constp
	  = TYPE_READONLY (TREE_TYPE (t1)) || TYPE_READONLY (TREE_TYPE (t2));
	int volatilep
	  = TYPE_VOLATILE (TREE_TYPE (t1)) || TYPE_VOLATILE (TREE_TYPE (t2));
	t1 = build_pointer_type (build_c_type_variant (target, constp,
				 volatilep));
	return build_type_attribute_variant (t1, attributes);
      }
#if 0
      t1 = build_pointer_type (common_type (TREE_TYPE (t1), TREE_TYPE (t2)));
      return build_type_attribute_variant (t1, attributes);
#endif

    case ARRAY_TYPE:
      {
	tree elt = common_type (TREE_TYPE (t1), TREE_TYPE (t2));
	/* Save space: see if the result is identical to one of the args.  */
	if (elt == TREE_TYPE (t1) && TYPE_DOMAIN (t1))
	  return build_type_attribute_variant (t1, attributes);
	if (elt == TREE_TYPE (t2) && TYPE_DOMAIN (t2))
	  return build_type_attribute_variant (t2, attributes);
	/* Merge the element types, and have a size if either arg has one.  */
	t1 = build_array_type (elt, TYPE_DOMAIN (TYPE_DOMAIN (t1) ? t1 : t2));
	return build_type_attribute_variant (t1, attributes);
      }

    case FUNCTION_TYPE:
      /* Function types: prefer the one that specified arg types.
	 If both do, merge the arg types.  Also merge the return types.  */
      {
	tree valtype = common_type (TREE_TYPE (t1), TREE_TYPE (t2));
	tree p1 = TYPE_ARG_TYPES (t1);
	tree p2 = TYPE_ARG_TYPES (t2);
	int len;
	tree newargs, n;
	int i;

	/* Save space: see if the result is identical to one of the args.  */
	if (valtype == TREE_TYPE (t1) && ! TYPE_ARG_TYPES (t2))
	  return build_type_attribute_variant (t1, attributes);
	if (valtype == TREE_TYPE (t2) && ! TYPE_ARG_TYPES (t1))
	  return build_type_attribute_variant (t2, attributes);

	/* Simple way if one arg fails to specify argument types.  */
	if (TYPE_ARG_TYPES (t1) == 0)
	 {
	   t1 = build_function_type (valtype, TYPE_ARG_TYPES (t2));
	   return build_type_attribute_variant (t1, attributes);
	 }
	if (TYPE_ARG_TYPES (t2) == 0)
	 {
	   t1 = build_function_type (valtype, TYPE_ARG_TYPES (t1));
	   return build_type_attribute_variant (t1, attributes);
	 }

	/* If both args specify argument types, we must merge the two
	   lists, argument by argument.  */

	len = list_length (p1);
	newargs = 0;

	for (i = 0; i < len; i++)
	  newargs = tree_cons (NULL_TREE, NULL_TREE, newargs);

	n = newargs;

	for (; p1;
	     p1 = TREE_CHAIN (p1), p2 = TREE_CHAIN (p2), n = TREE_CHAIN (n))
	  {
	    /* A null type means arg type is not specified.
	       Take whatever the other function type has.  */
	    if (TREE_VALUE (p1) == 0)
	      {
		TREE_VALUE (n) = TREE_VALUE (p2);
		goto parm_done;
	      }
	    if (TREE_VALUE (p2) == 0)
	      {
		TREE_VALUE (n) = TREE_VALUE (p1);
		goto parm_done;
	      }

	    /* Given  wait (union {union wait *u; int *i} *)
	       and  wait (union wait *),
	       prefer  union wait *  as type of parm.  */
	    if (TREE_CODE (TREE_VALUE (p1)) == UNION_TYPE
		&& TREE_VALUE (p1) != TREE_VALUE (p2))
	      {
		tree memb;
		for (memb = TYPE_FIELDS (TREE_VALUE (p1));
		     memb; memb = TREE_CHAIN (memb))
		  if (comptypes (TREE_TYPE (memb), TREE_VALUE (p2)))
		    {
		      TREE_VALUE (n) = TREE_VALUE (p2);
		      if (pedantic)
			pedwarn ("function types not truly compatible in ANSI C");
		      goto parm_done;
		    }
	      }
	    if (TREE_CODE (TREE_VALUE (p2)) == UNION_TYPE
		&& TREE_VALUE (p2) != TREE_VALUE (p1))
	      {
		tree memb;
		for (memb = TYPE_FIELDS (TREE_VALUE (p2));
		     memb; memb = TREE_CHAIN (memb))
		  if (comptypes (TREE_TYPE (memb), TREE_VALUE (p1)))
		    {
		      TREE_VALUE (n) = TREE_VALUE (p1);
		      if (pedantic)
			pedwarn ("function types not truly compatible in ANSI C");
		      goto parm_done;
		    }
	      }
	    TREE_VALUE (n) = common_type (TREE_VALUE (p1), TREE_VALUE (p2));
	  parm_done: ;
	  }

	t1 = build_function_type (valtype, newargs);
	/* ... falls through ...  */
      }

    default:
      return build_type_attribute_variant (t1, attributes);
    }

}

/* Returns the base type of an ordinal subrange, or the type
 * itself if it is not a subrange.
 */
tree
base_type (type)
     tree type;
{
  if (type == error_mark_node)
    return error_mark_node;

  /* Check for ordinal subranges.
   */
  if (ORDINAL_TYPE (TREE_CODE (type))
      && TREE_TYPE (type))
    type = TREE_TYPE (type);
  return TYPE_MAIN_VARIANT (type);
}

/* Return 1 if TYPE1 and TYPE2 are compatible types for assignment
   or various other operations.  Return 2 if they are compatible
   but a warning may be needed if you use them together.  */

int
comptypes (type1, type2)
     tree type1, type2;
{
  register tree t1 = type1;
  register tree t2 = type2;
  int attrval, val;

  /* Suppress errors caused by previously reported errors.  */

  if (t1 == t2 || TREE_CODE (t1) == ERROR_MARK || TREE_CODE (t2) == ERROR_MARK)
    return 1;


  /* Enumeral types must match almost exactly.
   */
  if (TREE_CODE (t1) == ENUMERAL_TYPE || TREE_CODE (t2) == ENUMERAL_TYPE)
    return base_type (t1) == base_type (t2) ? 1 : 0;

  if (t1 == t2)
    return 1;

  /* Different classes of types can't be compatible.  */

  if (TREE_CODE (t1) != TREE_CODE (t2)) return 0;

#if NOT_NEEDED_FOR_M2
  if ((TREE_CODE (t1) == ARRAY_TYPE || TREE_CODE (t1) == RECORD_TYPE)
      && PASCAL_TYPE_PACKED (t1) != PASCAL_TYPE_PACKED (t2))
    return 0;
#endif

  /* Qualifiers must match.  */

  if (TYPE_READONLY (t1) != TYPE_READONLY (t2))
    return 0;
  if (TYPE_VOLATILE (t1) != TYPE_VOLATILE (t2))
    return 0;

  /* Allow for two different type nodes which have essentially the same
     definition.  Note that we already checked for equality of the type
     qualifiers (just above).  */

  if (TYPE_MAIN_VARIANT (t1) == TYPE_MAIN_VARIANT (t2))
    return 1;

#ifndef COMP_TYPE_ATTRIBUTES
#define COMP_TYPE_ATTRIBUTES(t1,t2)	1
#endif

  /* 1 if no need for warning yet, 2 if warning cause has been seen.  */
  if (! (attrval = COMP_TYPE_ATTRIBUTES (t1, t2)))
     return 0;

  /* 1 if no need for warning yet, 2 if warning cause has been seen.  */
  val = 0;

  switch (TREE_CODE (t1))
    {
    case INTEGER_TYPE:  /* All integer types are compatible. */
    case CHAR_TYPE:  /* All char types are compatible. */
    case BOOLEAN_TYPE:  /* All Boolean types are compatible. */
      if (TREE_CODE (t1) == TREE_CODE (t2))
        return 1;
      break;

    /* @@@@ Check SET_TYPE & FILE_TYPE more.
     */
    case SET_TYPE:
      val = (TREE_TYPE (t1) == TREE_TYPE (t2)
	      ? 1 : TREE_TYPE (t1) == void_type_node  /* emtpy set */
                    || TREE_TYPE (t2) == void_type_node
                    || comptypes (TREE_TYPE (t1), TREE_TYPE (t2)));
      break;

    case FILE_TYPE:
      val = (TREE_TYPE (t1) == TREE_TYPE (t2)
	      ? 1 : comptypes (TREE_TYPE (t1), TREE_TYPE (t2)));
      break;

    /* Complex type may be mixed with real and integer */
    /* @@@: But does it need some warnings? */
    case COMPLEX_TYPE:
      return TREE_CODE (t2) == COMPLEX_TYPE ||
	     TREE_CODE (t2) == INTEGER_TYPE ||
	     TREE_CODE (t2) == REAL_TYPE;

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      if (! TREE_TYPE (t1) || ! (TREE_TYPE (t2)))
        {
	  abort ();
	}
      val = (TREE_TYPE (t1) == TREE_TYPE (t2)
	      ? 1 : comptypes (TREE_TYPE (t1), TREE_TYPE (t2)));
      break;
    case FUNCTION_TYPE:
      /* M2 front end checks this */
#if NOT_NEEDED_FOR_M2
      val = function_types_compatible_p (t1, t2);
#endif
      val = 1;
      break;

    case ARRAY_TYPE:
      {
	tree d1 = TYPE_DOMAIN (t1);
	tree d2 = TYPE_DOMAIN (t2);
	val = 1;

	/* Target types must match incl. qualifiers.  */
	if (TREE_TYPE (t1) != TREE_TYPE (t2)
	    && 0 == (val = comptypes (TREE_TYPE (t1), TREE_TYPE (t2))))
	  return 0;
#if NOT_NEEDED_FOR_M2
	/* Sizes must match unless one is missing or variable.  */
        /* ... or unless one of both is an "open array" parameter.  */
        if (PASCAL_TYPE_OPEN_ARRAY (t1) || PASCAL_TYPE_OPEN_ARRAY (t2))
          break;
#endif
	if (d1 == 0 || d2 == 0 || d1 == d2
	    || TREE_CODE (TYPE_MIN_VALUE (d1)) != INTEGER_CST
	    || TREE_CODE (TYPE_MIN_VALUE (d2)) != INTEGER_CST
	    || TREE_CODE (TYPE_MAX_VALUE (d1)) != INTEGER_CST
	    || TREE_CODE (TYPE_MAX_VALUE (d2)) != INTEGER_CST)
	  break;

	if (! ((TREE_INT_CST_LOW (TYPE_MIN_VALUE (d1))
		  == TREE_INT_CST_LOW (TYPE_MIN_VALUE (d2)))
		 && (TREE_INT_CST_HIGH (TYPE_MIN_VALUE (d1))
		     == TREE_INT_CST_HIGH (TYPE_MIN_VALUE (d2)))
		 && (TREE_INT_CST_LOW (TYPE_MAX_VALUE (d1))
		     == TREE_INT_CST_LOW (TYPE_MAX_VALUE (d2)))
		 && (TREE_INT_CST_HIGH (TYPE_MAX_VALUE (d1))
		     == TREE_INT_CST_HIGH (TYPE_MAX_VALUE (d2)))))
	   val = 0;
        break;
      }

    case RECORD_TYPE:
#if defined(THOUGHT_OUT_YET)
      /* If one and the same object is loaded from two or more
       * GPI files, it might have two RECORD_TYPE nodes.
       */
      if (PASCAL_TYPE_OBJECT (t1)
          && PASCAL_TYPE_OBJECT (t2)
	  && TYPE_LANG_NAME (t1) == TYPE_LANG_NAME (t2))
	val = 1;

      /* The same problem can also occur with ordinary records, strings,
       * and schemata.  Check it the hard way.  :-(
       */
      else if (PASCAL_TYPE_STRING (t1) && PASCAL_TYPE_STRING (t2))
        {
          if (PASCAL_TYPE_UNDISCRIMINATED_STRING (t1)
              || PASCAL_TYPE_UNDISCRIMINATED_STRING (t2))
            val = 1;
          else
            {
              /* Compare the arrays.
               */
              tree field1 = TREE_CHAIN (TREE_CHAIN (TYPE_FIELDS (t1)));
              tree field2 = TREE_CHAIN (TREE_CHAIN (TYPE_FIELDS (t2)));
              val = comptypes (TREE_TYPE (field1), TREE_TYPE (field2));
            }
        }
      else if (! PASCAL_TYPE_OBJECT (t1) && ! PASCAL_TYPE_OBJECT (t2))
        {
          tree field1, field2;
	  for (field1 = TYPE_FIELDS (t1), field2 = TYPE_FIELDS (t2);
               field1 && field2;
               field1 = TREE_CHAIN (field1), field2 = TREE_CHAIN (field2))
            {
              if (DECL_NAME (field1) != DECL_NAME (field2)
                  || (TYPE_NAME (TREE_TYPE (field1))
                      && TYPE_NAME (TREE_TYPE (field2))
                      && TYPE_NAME (TREE_TYPE (field1)) != TYPE_NAME (TREE_TYPE (field2)))
#if 0  /* This leads to an infinite loop in the case of a recursive type. )-: */
                  || comptypes (TREE_TYPE (field1), TREE_TYPE (field2)) == 0)
#else  /* This, OTOH, is not strong enough :-( */
                  || TREE_CODE (TREE_TYPE (field1)) != TREE_CODE (TREE_TYPE (field2)))
#endif  /* )-: */
              break;
            }
          if (field1 == NULL_TREE && field2 == NULL_TREE)
            val = 1;
        }
#else
        val = 1;
#endif
      break;

    default:
      break;
    }
  return attrval == 2 && val == 1 ? 2 : val;
}

/* If pedantic, warn about improper lvalue.   CODE is either COND_EXPR
   COMPOUND_EXPR, or CONVERT_EXPR (for casts).  */

static void
pedantic_lvalue_warning (code)
     enum tree_code code;
{
  if (pedantic)
    pedwarn ("ANSI C forbids use of %s expressions as lvalues",
	     code == COND_EXPR ? "conditional"
	     : code == COMPOUND_EXPR ? "compound" : "cast");
}

/* Warn about storing in something that is `const'.  */

void
readonly_warning (arg, string)
     tree arg;
     const char *string;
{
  char buf[80];
  strcpy (buf, string);

  /* Forbid assignments to iterators.  */
  if (TREE_CODE (arg) == VAR_DECL && ITERATOR_P (arg))
    {
      strcat (buf, " of iterator `%s'");
      pedwarn (buf, IDENTIFIER_POINTER (DECL_NAME (arg)));
    }

  if (TREE_CODE (arg) == COMPONENT_REF)
    {
      if (TREE_READONLY (TREE_OPERAND (arg, 0))
          || TYPE_READONLY (TREE_TYPE (TREE_OPERAND (arg, 0))))
	readonly_warning (TREE_OPERAND (arg, 0), string);
      else
	{
	  strcat (buf, " of read-only member `%s'");
	  pedwarn (buf, IDENTIFIER_POINTER (DECL_NAME (TREE_OPERAND (arg, 1))));
	}
    }
  else if (TREE_CODE (arg) == VAR_DECL)
    {
      strcat (buf, " of read-only variable `%s'");
      pedwarn (buf, IDENTIFIER_POINTER (DECL_NAME (arg)));
    }
  else
    {
      pedwarn ("%s of read-only location", buf);
    }
}

/* Mark EXP saying that we need to be able to take the
   address of it; it should not be allocated in a register.
   Value is 1 if successful.  */

int
mark_addressable (exp)
     tree exp;
{
  register tree x = exp;
  while (1)
    switch (TREE_CODE (x))
      {
      case COMPONENT_REF:
	if (DECL_C_BIT_FIELD (TREE_OPERAND (x, 1)))
	  {
	    error ("cannot take address of bitfield `%s'",
		   IDENTIFIER_POINTER (DECL_NAME (TREE_OPERAND (x, 1))));
	    return 0;
	  }

	/* ... fall through ...  */

      case ADDR_EXPR:
      case ARRAY_REF:
      case REALPART_EXPR:
      case IMAGPART_EXPR:
	x = TREE_OPERAND (x, 0);
	break;

      case CONSTRUCTOR:
	TREE_ADDRESSABLE (x) = 1;
	return 1;

      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x)
	    && DECL_NONLOCAL (x))
	  {
	    if (TREE_PUBLIC (x))
	      {
		error ("global register variable `%s' used in nested function",
		       IDENTIFIER_POINTER (DECL_NAME (x)));
		return 0;
	      }
	    pedwarn ("register variable `%s' used in nested function",
		     IDENTIFIER_POINTER (DECL_NAME (x)));
	  }
	else if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x))
	  {
	    if (TREE_PUBLIC (x))
	      {
		error ("address of global register variable `%s' requested",
		       IDENTIFIER_POINTER (DECL_NAME (x)));
		return 0;
	      }

	    /* If we are making this addressable due to its having
	       volatile components, give a different error message.  Also
	       handle the case of an unnamed parameter by not trying
	       to give the name.  */

	    else if (C_TYPE_FIELDS_VOLATILE (TREE_TYPE (x)))
	      {
		error ("cannot put object with volatile field into register");
		return 0;
	      }

	    pedwarn ("address of register variable `%s' requested",
		     IDENTIFIER_POINTER (DECL_NAME (x)));
	  }
	put_var_into_stack (x);

	/* drops in */
      case FUNCTION_DECL:
	TREE_ADDRESSABLE (x) = 1;
#if 0  /* poplevel deals with this now.  */
	if (DECL_CONTEXT (x) == 0)
	  TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (x)) = 1;
#endif

      default:
	return 1;
    }
}

int
is_of_string_type (type, warn)
     tree type;
     int warn ATTRIBUTE_UNUSED;
{
  return(
	 (TREE_CODE (type) == ARRAY_TYPE) &&
	 (TYPE_MAIN_VARIANT (TREE_TYPE (type)) == char_type_node)
	);
}

/*
 * Return 1 if the type of the node STRING is a character array node
 *  or it's a string constant, or it's a string schema.
 * Return 2 if it is a valid conformant array whose low bound should
 *  be checked at runtime.
 *
 * Return 0 if we know it's not a valid string.
 */
int
is_string_type (string, warn)
     tree string;
     int warn;
{
  if (TREE_CODE (string) == STRING_CST)
    return 1;

  return is_of_string_type (TREE_TYPE (string), warn);
}


/* Return either DECL or its known constant value (if it has one).  */

static tree
decl_constant_value (decl)
     tree decl;
{
  if (/* Don't change a variable array bound or initial value to a constant
	 in a place where a variable is invalid.  */
      current_function_decl != 0
      && ! pedantic
      && ! TREE_THIS_VOLATILE (decl)
      && TREE_READONLY (decl) && ! ITERATOR_P (decl)
      && DECL_INITIAL (decl) != 0
      && TREE_CODE (DECL_INITIAL (decl)) != ERROR_MARK
      /* This is invalid if initial value is not constant.
	 If it has either a function call, a memory reference,
	 or a variable, then re-evaluating it could give different results.  */
      && TREE_CONSTANT (DECL_INITIAL (decl))
      /* Check for cases where this is sub-optimal, even though valid.  */
      && TREE_CODE (DECL_INITIAL (decl)) != CONSTRUCTOR
      && DECL_MODE (decl) != BLKmode)
    return DECL_INITIAL (decl);
  return decl;
}


/* Return nonzero if REF is an lvalue valid for this language.
   Lvalues can be assigned, unless their type has TYPE_READONLY.
   Lvalues can have their address taken, unless they have DECL_REGISTER.  */

int
lvalue_p (ref)
     tree ref;
{
  register enum tree_code code = TREE_CODE (ref);

  switch (code)
    {
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case COMPONENT_REF:
      return lvalue_p (TREE_OPERAND (ref, 0));

    case STRING_CST:
      return 1;

    case INDIRECT_REF:
    case ARRAY_REF:
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
    case ERROR_MARK:
      return (TREE_CODE (TREE_TYPE (ref)) != FUNCTION_TYPE
	      && TREE_CODE (TREE_TYPE (ref)) != METHOD_TYPE);

    case BIND_EXPR:
    case RTL_EXPR:
      return TREE_CODE (TREE_TYPE (ref)) == ARRAY_TYPE;

    default:
      return 0;
    }
}

/* Return nonzero if REF is an lvalue valid for this language;
   otherwise, print an error message and return zero.  */

int
lvalue_or_else (ref, string)
     tree ref;
     const char *string;
{
  int win = lvalue_p (ref);
  if (! win)
    error ("invalid lvalue in %s", string);
  return win;
}

/* Apply unary lvalue-demanding operator CODE to the expression ARG
   for certain kinds of expressions which are not really lvalues
   but which we can accept as lvalues.

   If ARG is not a kind of expression we can handle, return zero.  */
   
static tree
unary_complex_lvalue (code, arg)
     enum tree_code code;
     tree arg;
{
  /* Handle (a, b) used as an "lvalue".  */
  if (TREE_CODE (arg) == COMPOUND_EXPR)
    {
      tree real_result = build_unary_op (code, TREE_OPERAND (arg, 1), 0);

      /* If this returns a function type, it isn't really being used as
	 an lvalue, so don't issue a warning about it.  */
      if (TREE_CODE (TREE_TYPE (arg)) != FUNCTION_TYPE)
	pedantic_lvalue_warning (COMPOUND_EXPR);

      return build (COMPOUND_EXPR, TREE_TYPE (real_result),
		    TREE_OPERAND (arg, 0), real_result);
    }

  /* Handle (a ? b : c) used as an "lvalue".  */
  if (TREE_CODE (arg) == COND_EXPR)
    {
      pedantic_lvalue_warning (COND_EXPR);
      if (TREE_CODE (TREE_TYPE (arg)) != FUNCTION_TYPE)
	pedantic_lvalue_warning (COMPOUND_EXPR);

      return (build_conditional_expr
	      (TREE_OPERAND (arg, 0),
	       build_unary_op (code, TREE_OPERAND (arg, 1), 0),
	       build_unary_op (code, TREE_OPERAND (arg, 2), 0)));
    }

  return 0;
}

/* Perform default promotions for C data used in expressions.
   Arrays and functions are converted to pointers;
   enumeral types or short or char, to int.
   In addition, manifest constants symbols are replaced by their values.  */

tree
default_conversion (exp)
     tree exp;
{
  register tree type = TREE_TYPE (exp);
  register enum tree_code code = TREE_CODE (type);

  /* Constants can be used directly unless they're not loadable.  */
  if (TREE_CODE (exp) == CONST_DECL)
    exp = DECL_INITIAL (exp);

  /* Replace a nonvolatile const static variable with its value unless
     it is an array, in which case we must be sure that taking the
     address of the array produces consistent results.  */
  else if (optimize && TREE_CODE (exp) == VAR_DECL && code != ARRAY_TYPE)
    {
      exp = decl_constant_value (exp);
      type = TREE_TYPE (exp);
    }

  /* Strip NON_LVALUE_EXPRs and no-op conversions, since we aren't using as
     an lvalue.  */
  /* Do not use STRIP_NOPS here!  It will remove conversions from pointer
     to integer and cause infinite recursion.  */
  while (TREE_CODE (exp) == NON_LVALUE_EXPR
	 || (TREE_CODE (exp) == NOP_EXPR
	     && TREE_TYPE (TREE_OPERAND (exp, 0)) == TREE_TYPE (exp)))
    exp = TREE_OPERAND (exp, 0);

  /* Normally convert enums to int,
     but convert wide enums to something wider.  */
#ifdef GPC
  if (code == ENUMERAL_TYPE || code == CHAR_TYPE || code == BOOLEAN_TYPE)
#else
  if (code == ENUMERAL_TYPE)
#endif /* GPC */
    {
      type = type_for_size (MAX (TYPE_PRECISION (type),
				 TYPE_PRECISION (integer_type_node)),
			    ((flag_traditional
			      || (TYPE_PRECISION (type)
				  >= TYPE_PRECISION (integer_type_node)))
			     && TREE_UNSIGNED (type)));
      return convert (type, exp);
    }

  if (TREE_CODE (exp) == COMPONENT_REF
      && DECL_C_BIT_FIELD (TREE_OPERAND (exp, 1)))
    {
      tree width = DECL_SIZE (TREE_OPERAND (exp, 1));
      HOST_WIDE_INT low = TREE_INT_CST_LOW (width);

      /* If it's thinner than an int, promote it like a
	 C_PROMOTING_INTEGER_TYPE_P, otherwise leave it alone.  */

      if (low < TYPE_PRECISION (integer_type_node))
	{
	  if (flag_traditional && TREE_UNSIGNED (type))
	    return convert (unsigned_type_node, exp);
	  else
	    return convert (integer_type_node, exp);
	}
    }

  if (C_PROMOTING_INTEGER_TYPE_P (type))
    {
      /* Traditionally, unsignedness is preserved in default promotions.
         Also preserve unsignedness if not really getting any wider.  */
      if (TREE_UNSIGNED (type)
	  && (flag_traditional
	      || TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node)))
	return convert (unsigned_type_node, exp);
      return convert (integer_type_node, exp);
    }
#if 0
  if (flag_traditional && !flag_allow_single_precision
      && TYPE_MAIN_VARIANT (type) == float_type_node)
    return convert (double_type_node, exp);
#endif
  if (code == VOID_TYPE)
    {
      error ("void value not ignored as it ought to be");
      return error_mark_node;
    }
  if (code == FUNCTION_TYPE)
    {
      return build_unary_op (ADDR_EXPR, exp, 0);
    }
  /* Get rid of var parameter REFERENCE_TYPE */
  if (code == REFERENCE_TYPE)
    return convert (build_pointer_type (TREE_TYPE (type)), exp);

  if (code == ARRAY_TYPE)
    {
      register tree adr;
      tree restype = TREE_TYPE (type);
      tree ptrtype;
      int constp = 0;
      int volatilep = 0;

      if (TREE_CODE_CLASS (TREE_CODE (exp)) == 'r'
	  || TREE_CODE_CLASS (TREE_CODE (exp)) == 'd')
	{
	  constp = TREE_READONLY (exp);
	  volatilep = TREE_THIS_VOLATILE (exp);
	}

      if (TYPE_READONLY (type) || TYPE_VOLATILE (type)
	  || constp || volatilep)
	restype = build_c_type_variant (restype,
					TYPE_READONLY (type) || constp,
					TYPE_VOLATILE (type) || volatilep);

      if (TREE_CODE (exp) == INDIRECT_REF)
	return convert (TYPE_POINTER_TO (restype),
			TREE_OPERAND (exp, 0));

      if (TREE_CODE (exp) == COMPOUND_EXPR)
	{
	  tree op1 = default_conversion (TREE_OPERAND (exp, 1));
	  return build (COMPOUND_EXPR, TREE_TYPE (op1),
			TREE_OPERAND (exp, 0), op1);
	}

      if (! lvalue_p (exp)
	  && ! (TREE_CODE (exp) == CONSTRUCTOR && TREE_STATIC (exp)))
	{
	  error ("invalid use of non-lvalue array");
	  return error_mark_node;
	}

      ptrtype = build_pointer_type (restype);

      if (TREE_CODE (exp) == VAR_DECL)
	{
	  /* ??? This is not really quite correct
	     in that the type of the operand of ADDR_EXPR
	     is not the target type of the type of the ADDR_EXPR itself.
	     Question is, can this lossage be avoided?  */
	  adr = build1 (ADDR_EXPR, ptrtype, exp);
	  if (mark_addressable (exp) == 0)
	    return error_mark_node;
	  TREE_CONSTANT (adr) = staticp (exp);
	  TREE_SIDE_EFFECTS (adr) = 0;   /* Default would be, same as EXP.  */
	  return adr;
	}
      /* This way is better for a COMPONENT_REF since it can
	 simplify the offset for a component.  */
      adr = build_unary_op (ADDR_EXPR, exp, 1);
      return convert (ptrtype, adr);
    }
  return exp;
}

/* Return 1 if TTL and TTR are pointers to types that are equivalent,
   ignoring their qualifiers.  */

static int
comp_target_types (ttl, ttr)
     tree ttl, ttr;
{
  int val;

#if NOT_NEEDED_FOR_M2
  /* Give maybe_objc_comptypes a crack at letting these types through.  */
  if (val = maybe_objc_comptypes (ttl, ttr, 1) >= 0)
    return val;
#endif

  val = comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (ttl)),
		   TYPE_MAIN_VARIANT (TREE_TYPE (ttr)));

  if (val == 2 && pedantic)
    pedwarn ("types are not quite compatible");
  return val;
}

/* Subroutines of `comptypes'.  */

#if 0
/* Return 1 if two function types F1 and F2 are compatible.
   If either type specifies no argument types,
   the other must specify a fixed number of self-promoting arg types.
   Otherwise, if one type specifies only the number of arguments, 
   the other must specify that number of self-promoting arg types.
   Otherwise, the argument types must match.  */

static int
function_types_compatible_p (f1, f2)
     tree f1, f2;
{
  tree args1, args2;
  /* 1 if no need for warning yet, 2 if warning cause has been seen.  */
  int val = 1;
  int val1;

  if (!(TREE_TYPE (f1) == TREE_TYPE (f2)
	|| (val = comptypes (TREE_TYPE (f1), TREE_TYPE (f2)))))
    return 0;

  args1 = TYPE_ARG_TYPES (f1);
  args2 = TYPE_ARG_TYPES (f2);

  /* Both types have argument lists: compare them and propagate results.  */
  val1 = type_lists_compatible_p (args1, args2);
  return val1 != 1 ? val1 : val;
}

/* Check two lists of types for compatibility,
   returning 0 for incompatible, 1 for compatible,
   or 2 for compatible with warning.  */

static int
type_lists_compatible_p (args1, args2)
     tree args1, args2;
{
  /* 1 if no need for warning yet, 2 if warning cause has been seen.  */
  int val = 1;
  int newval = 0;

  while (1)
    {
      if (args1 == 0 && args2 == 0)
	return val;
      /* If one list is shorter than the other,
	 they fail to match.  */
      if (args1 == 0 || args2 == 0)
	return 0;
#if NOT_NEEDED_FOR_M2
      /* A null pointer instead of a type
	 means there is supposed to be an argument
	 but nothing is specified about what type it has.
	 So match anything that self-promotes.  */
      if (TREE_VALUE (args1) == 0)
	{
	  if (! self_promoting_type_p (TREE_VALUE (args2)))
	    return 0;
	}
      else if (TREE_VALUE (args2) == 0)
	{
	  if (! self_promoting_type_p (TREE_VALUE (args1)))
	    return 0;
	}
#endif
      else if (! (newval = comptypes (TREE_VALUE (args1), TREE_VALUE (args2))))
	{
	  /* Allow  wait (union {union wait *u; int *i} *)
	     and  wait (union wait *)  to be compatible.  */
	  if (TREE_CODE (TREE_VALUE (args1)) == UNION_TYPE
	      && (TYPE_NAME (TREE_VALUE (args1)) == 0
		  || TYPE_TRANSPARENT_UNION (TREE_VALUE (args1)))
	      && TREE_CODE (TYPE_SIZE (TREE_VALUE (args1))) == INTEGER_CST
	      && tree_int_cst_equal (TYPE_SIZE (TREE_VALUE (args1)),
				     TYPE_SIZE (TREE_VALUE (args2))))
	    {
	      tree memb;
	      for (memb = TYPE_FIELDS (TREE_VALUE (args1));
		   memb; memb = TREE_CHAIN (memb))
		if (comptypes (TREE_TYPE (memb), TREE_VALUE (args2)))
		  break;
	      if (memb == 0)
		return 0;
	    }
	  else if (TREE_CODE (TREE_VALUE (args2)) == UNION_TYPE
		   && (TYPE_NAME (TREE_VALUE (args2)) == 0
		       || TYPE_TRANSPARENT_UNION (TREE_VALUE (args2)))
		   && TREE_CODE (TYPE_SIZE (TREE_VALUE (args2))) == INTEGER_CST
		   && tree_int_cst_equal (TYPE_SIZE (TREE_VALUE (args2)),
					  TYPE_SIZE (TREE_VALUE (args1))))
	    {
	      tree memb;
	      for (memb = TYPE_FIELDS (TREE_VALUE (args2));
		   memb; memb = TREE_CHAIN (memb))
		if (comptypes (TREE_TYPE (memb), TREE_VALUE (args1)))
		  break;
	      if (memb == 0)
		return 0;
	    }
	  else
	    return 0;
	}

      /* comptypes said ok, but record if it said to warn.  */
      if (newval > val)
	val = newval;

      args1 = TREE_CHAIN (args1);
      args2 = TREE_CHAIN (args2);
    }
}
#endif

/* Print a warning using MSG.
   It gets OPNAME as its one parameter.
   If OPNAME is null, it is replaced by "passing arg ARGNUM of `FUNCTION'".
   FUNCTION and ARGNUM are handled specially if we are building an
   Objective-C selector.  */

static void
warn_for_assignment (msg, opname, function, argnum)
     char *msg;
     char *opname;
     tree function;
     int argnum;
{
  static char argstring[] = "passing arg %d of `%s'";
  static char argnofun[] =  "passing arg %d";

  if (opname == 0)
    {
#if NOT_NEEDED_FOR_M2
      tree selector = maybe_building_objc_message_expr ();
      
      if (selector && argnum > 2)
	{
	  function = selector;
	  argnum -= 2;
	}
#endif
      if (function)
	{
	  /* Function name is known; supply it.  */
	  opname = (char *) alloca (IDENTIFIER_LENGTH (function)
				    + sizeof (argstring) + 25 /*%d*/ + 1);
	  sprintf (opname, argstring, argnum, IDENTIFIER_POINTER (function));
	}
      else
	{
	  /* Function name unknown (call through ptr); just give arg number.  */
	  opname = (char *) alloca (sizeof (argnofun) + 25 /*%d*/ + 1);
	  sprintf (opname, argnofun, argnum);
	}
    }
  pedwarn (msg, opname);
}

/* Print a warning if a constant expression had overflow in folding.
   Invoke this function on every expression that the language
   requires to be a constant expression.
   Note the ANSI C standard says it is erroneous for a
   constant expression to overflow.  */

void
constant_expression_warning (value)
     tree value;
{
  if ((TREE_CODE (value) == INTEGER_CST || TREE_CODE (value) == REAL_CST
       || TREE_CODE (value) == COMPLEX_CST)
      && TREE_CONSTANT_OVERFLOW (value) && pedantic)
    pedwarn ("overflow in constant expression");
}

/* Print a warning if an expression had overflow in folding.
   Invoke this function on every expression that
   (1) appears in the source code, and
   (2) might be a constant expression that overflowed, and
   (3) is not already checked by convert_and_check;
   however, do not invoke this function on operands of explicit casts.  */

void
overflow_warning (value)
     tree value;
{
  if ((TREE_CODE (value) == INTEGER_CST
       || (TREE_CODE (value) == COMPLEX_CST
	   && TREE_CODE (TREE_REALPART (value)) == INTEGER_CST))
      && TREE_OVERFLOW (value))
    {
      TREE_OVERFLOW (value) = 0;
      if (skip_evaluation == 0)
	warning ("integer overflow in expression");
    }
  else if ((TREE_CODE (value) == REAL_CST
	    || (TREE_CODE (value) == COMPLEX_CST
		&& TREE_CODE (TREE_REALPART (value)) == REAL_CST))
	   && TREE_OVERFLOW (value))
    {
      TREE_OVERFLOW (value) = 0;
      if (skip_evaluation == 0)
	warning ("floating point overflow in expression");
    }
}

/* Print a warning if a large constant is truncated to unsigned,
   or if -Wconversion is used and a constant < 0 is converted to unsigned.
   Invoke this function on every expression that might be implicitly
   converted to an unsigned type.  */

void
unsigned_conversion_warning (result, operand)
     tree result, operand;
{
  if (TREE_CODE (operand) == INTEGER_CST
      && TREE_CODE (TREE_TYPE (result)) == INTEGER_TYPE
      && TREE_UNSIGNED (TREE_TYPE (result))
      && skip_evaluation == 0
      && !int_fits_type_p (operand, TREE_TYPE (result)))
    {
      if (!int_fits_type_p (operand, signed_type (TREE_TYPE (result))))
	/* This detects cases like converting -129 or 256 to unsigned char.  */
	warning ("large integer implicitly truncated to unsigned type");
      else if (warn_conversion)
	warning ("negative integer implicitly converted to unsigned type");
    }
}

/* Convert EXPR to TYPE, warning about conversion problems with constants.
   Invoke this function on every expression that is converted implicitly,
   i.e. because of language rules and not because of an explicit cast.  */

tree
convert_and_check (type, expr)
     tree type, expr;
{
  tree t = convert (type, expr);
  if (TREE_CODE (t) == INTEGER_CST)
    {
      if (TREE_OVERFLOW (t))
	{
	  TREE_OVERFLOW (t) = 0;

	  /* Do not diagnose overflow in a constant expression merely
	     because a conversion overflowed.  */
	  TREE_CONSTANT_OVERFLOW (t) = TREE_CONSTANT_OVERFLOW (expr);

	  /* No warning for converting 0x80000000 to int.  */
	  if (!(TREE_UNSIGNED (type) < TREE_UNSIGNED (TREE_TYPE (expr))
		&& TREE_CODE (TREE_TYPE (expr)) == INTEGER_TYPE
		&& TYPE_PRECISION (type) == TYPE_PRECISION (TREE_TYPE (expr))))
	    /* If EXPR fits in the unsigned version of TYPE,
	       don't warn unless pedantic.  */
	    if ((pedantic
		 || TREE_UNSIGNED (type)
		 || ! int_fits_type_p (expr, unsigned_type (type)))
	        && skip_evaluation == 0)
		warning ("overflow in implicit constant conversion");
	}
      else
	unsigned_conversion_warning (t, expr);
    }
  return t;
}

/* Build and return a conditional expression IFEXP ? OP1 : OP2.  */

tree
build_conditional_expr (ifexp, op1, op2)
     tree ifexp, op1, op2;
{
  register tree type1;
  register tree type2;
  register enum tree_code code1;
  register enum tree_code code2;
  register tree result_type = NULL;
  tree orig_op1 = op1, orig_op2 = op2;

  ifexp = truthvalue_conversion (default_conversion (ifexp));

#if 0 /* Produces wrong result if within sizeof.  */
  /* Don't promote the operands separately if they promote
     the same way.  Return the unpromoted type and let the combined
     value get promoted if necessary.  */

  if (TREE_TYPE (op1) == TREE_TYPE (op2)
      && TREE_CODE (TREE_TYPE (op1)) != ARRAY_TYPE
      && TREE_CODE (TREE_TYPE (op1)) != ENUMERAL_TYPE
      && TREE_CODE (TREE_TYPE (op1)) != FUNCTION_TYPE)
    {
      if (TREE_CODE (ifexp) == INTEGER_CST)
	return pedantic_non_lvalue (integer_zerop (ifexp) ? op2 : op1);

      return fold (build (COND_EXPR, TREE_TYPE (op1), ifexp, op1, op2));
    }
#endif

  /* Promote both alternatives.  */

  if (TREE_CODE (TREE_TYPE (op1)) != VOID_TYPE)
    op1 = default_conversion (op1);
  if (TREE_CODE (TREE_TYPE (op2)) != VOID_TYPE)
    op2 = default_conversion (op2);

  if (TREE_CODE (ifexp) == ERROR_MARK
      || TREE_CODE (TREE_TYPE (op1)) == ERROR_MARK
      || TREE_CODE (TREE_TYPE (op2)) == ERROR_MARK)
    return error_mark_node;

  type1 = TREE_TYPE (op1);
  code1 = TREE_CODE (type1);
  type2 = TREE_TYPE (op2);
  code2 = TREE_CODE (type2);

  /* Quickly detect the usual case where op1 and op2 have the same type
     after promotion.  */
  if (TYPE_MAIN_VARIANT (type1) == TYPE_MAIN_VARIANT (type2))
    {
      if (type1 == type2)
	result_type = type1;
      else
	result_type = TYPE_MAIN_VARIANT (type1);
    }
  else if ((code1 == INTEGER_TYPE || code1 == REAL_TYPE)
           && (code2 == INTEGER_TYPE || code2 == REAL_TYPE))
    {
      result_type = common_type (type1, type2);
    }
  else if (code1 == VOID_TYPE || code2 == VOID_TYPE)
    {
      if (pedantic && (code1 != VOID_TYPE || code2 != VOID_TYPE))
	pedwarn ("ANSI C forbids conditional expr with only one void side");
      result_type = void_type_node;
    }
  else if (code1 == POINTER_TYPE && code2 == POINTER_TYPE)
    {
      if (comp_target_types (type1, type2))
	result_type = common_type (type1, type2);
      else if (integer_zerop (op1) && TREE_TYPE (type1) == void_type_node
	       && TREE_CODE (orig_op1) != NOP_EXPR)
	result_type = qualify_type (type2, type1);
      else if (integer_zerop (op2) && TREE_TYPE (type2) == void_type_node
	       && TREE_CODE (orig_op2) != NOP_EXPR)
	result_type = qualify_type (type1, type2);
      else if (TYPE_MAIN_VARIANT (TREE_TYPE (type1)) == void_type_node)
	{
	  if (pedantic && TREE_CODE (TREE_TYPE (type2)) == FUNCTION_TYPE)
	    pedwarn ("ANSI C forbids conditional expr between untyped pointer and function pointer");
	  result_type = qualify_type (type1, type2);
	}
      else if (TYPE_MAIN_VARIANT (TREE_TYPE (type2)) == void_type_node)
	{
	  if (pedantic && TREE_CODE (TREE_TYPE (type1)) == FUNCTION_TYPE)
	    pedwarn ("ANSI C forbids conditional expr between untyped pointer and function pointer");
	  result_type = qualify_type (type2, type1);
	}
      else if (code1 == SET_TYPE && code2 == SET_TYPE)
	{
	  /* @@@ Check for set compatibility */
#if 0
	  converted = 1;
#endif
	}
      else if (code1 == ARRAY_TYPE && code2 == ARRAY_TYPE
	       && comptypes(type1, type2))
	{
#if 0
	  converted = 1;
#endif
	  if (! is_string_type (op1, 1) || ! is_string_type (op2, 1))
	    {
	      pedwarn ("compared arrays are not of string type");
	      result_type = NULL_TREE;
	    }
	}
      else
	{
	  pedwarn ("pointer type mismatch in conditional expression");
	  result_type = build_pointer_type (void_type_node);
	}
    }
  else if (code1 == POINTER_TYPE && code2 == INTEGER_TYPE)
    {
      if (! integer_zerop (op2))
	pedwarn ("pointer/integer type mismatch in conditional expression");
      else
	{
	  op2 = null_pointer_node;
#if 0  /* The spec seems to say this is permitted.  */
	  if (pedantic && TREE_CODE (type1) == FUNCTION_TYPE)
	    pedwarn ("ANSI C forbids conditional expr between 0 and function pointer");
#endif
	}
      result_type = type1;
    }
  else if (code2 == POINTER_TYPE && code1 == INTEGER_TYPE)
    {
      if (!integer_zerop (op1))
	pedwarn ("pointer/integer type mismatch in conditional expression");
      else
	{
	  op1 = null_pointer_node;
#if 0  /* The spec seems to say this is permitted.  */
	  if (pedantic && TREE_CODE (type2) == FUNCTION_TYPE)
	    pedwarn ("ANSI C forbids conditional expr between 0 and function pointer");
#endif
	}
      result_type = type2;
    }

  if (!result_type)
    {
      if (flag_cond_mismatch)
	result_type = void_type_node;
      else
	{
	  error ("type mismatch in conditional expression");
	  return error_mark_node;
	}
    }

  /* Merge const and volatile flags of the incoming types.  */
  result_type
    = build_type_variant (result_type,
			  TREE_READONLY (op1) || TREE_READONLY (op2),
			  TREE_THIS_VOLATILE (op1) || TREE_THIS_VOLATILE (op2));

  if (result_type != TREE_TYPE (op1))
    op1 = convert_and_check (result_type, op1);
  if (result_type != TREE_TYPE (op2))
    op2 = convert_and_check (result_type, op2);

#if 0
  if (code1 == RECORD_TYPE || code1 == UNION_TYPE)
    {
      result_type = TREE_TYPE (op1);
      if (TREE_CONSTANT (ifexp))
	return pedantic_non_lvalue (integer_zerop (ifexp) ? op2 : op1);

      if (TYPE_MODE (result_type) == BLKmode)
	{
	  register tree tempvar
	    = build_decl (VAR_DECL, NULL_TREE, result_type);
	  register tree xop1 = build_modify_expr (tempvar, op1);
	  register tree xop2 = build_modify_expr (tempvar, op2);
	  register tree result = fold (build (COND_EXPR, result_type,
					      ifexp, xop1, xop2));

	  layout_decl (tempvar, TYPE_ALIGN (result_type));
	  /* No way to handle variable-sized objects here.
	     I fear that the entire handling of BLKmode conditional exprs
	     needs to be redone.  */
	  if (TREE_CODE (DECL_SIZE (tempvar)) != INTEGER_CST)
	    abort ();
	  DECL_RTL (tempvar)
	    = assign_stack_local (DECL_MODE (tempvar),
				  (TREE_INT_CST_LOW (DECL_SIZE (tempvar))
				   + BITS_PER_UNIT - 1)
				  / BITS_PER_UNIT,
				  0);

	  TREE_SIDE_EFFECTS (result)
	    = TREE_SIDE_EFFECTS (ifexp) | TREE_SIDE_EFFECTS (op1)
	      | TREE_SIDE_EFFECTS (op2);
	  return build (COMPOUND_EXPR, result_type, result, tempvar);
	}
    }
#endif /* 0 */

  if (TREE_CODE (ifexp) == INTEGER_CST)
    return pedantic_non_lvalue (integer_zerop (ifexp) ? op2 : op1);

  return fold (build (COND_EXPR, result_type, ifexp, op1, op2));
}


/* Compute the size to increment a pointer by.  */

tree
c_size_in_bytes (type)
     tree type;
{
  enum tree_code code = TREE_CODE (type);
  tree t;

  if (code == FUNCTION_TYPE)
    return size_int (1);
  if (code == VOID_TYPE)
    return size_int (1);
  if (code == ERROR_MARK)
    return size_int (1);
  if (TYPE_SIZE (type) == 0)
    {
      error ("arithmetic on pointer to an incomplete type");
      return size_int (1);
    }

  /* Convert in case a char is more than one unit.  */
  t = size_binop (CEIL_DIV_EXPR, TYPE_SIZE (type), 
		     size_int (BITS_PER_UNIT));
  force_fit_type (t, 0);
  return t;
}

/* Subroutine of build_binary_op, used for comparison operations.
   See if the operands have both been converted from subword integer types
   and, if so, perhaps change them both back to their original type.
   This function is also responsible for converting the two operands
   to the proper common type for comparison.

   The arguments of this function are all pointers to local variables
   of build_binary_op: OP0_PTR is &OP0, OP1_PTR is &OP1,
   RESTYPE_PTR is &RESULT_TYPE and RESCODE_PTR is &RESULTCODE.

   If this function returns nonzero, it means that the comparison has
   a constant value.  What this function returns is an expression for
   that value.  */

tree
shorten_compare (op0_ptr, op1_ptr, restype_ptr, rescode_ptr)
     tree *op0_ptr, *op1_ptr;
     tree *restype_ptr;
     enum tree_code *rescode_ptr;
{
  register tree type;
  tree op0 = *op0_ptr;
  tree op1 = *op1_ptr;
  int unsignedp0, unsignedp1;
  int real1, real2;
  tree primop0, primop1;
  enum tree_code code = *rescode_ptr;

  /* Throw away any conversions to wider types
     already present in the operands.  */

  primop0 = get_narrower (op0, &unsignedp0);
  primop1 = get_narrower (op1, &unsignedp1);

  /* Handle the case that OP0 does not *contain* a conversion
     but it *requires* conversion to FINAL_TYPE.  */

  if (op0 == primop0 && TREE_TYPE (op0) != *restype_ptr)
    unsignedp0 = TREE_UNSIGNED (TREE_TYPE (op0));
  if (op1 == primop1 && TREE_TYPE (op1) != *restype_ptr)
    unsignedp1 = TREE_UNSIGNED (TREE_TYPE (op1));

  /* If one of the operands must be floated, we cannot optimize.  */
  real1 = TREE_CODE (TREE_TYPE (primop0)) == REAL_TYPE;
  real2 = TREE_CODE (TREE_TYPE (primop1)) == REAL_TYPE;

  /* If first arg is constant, swap the args (changing operation
     so value is preserved), for canonicalization.  Don't do this if
     the second arg is 0.  */

  if (TREE_CONSTANT (primop0)
      && ! integer_zerop (primop1) && ! real_zerop (primop1))
    {
      register tree tem = primop0;
      register int temi = unsignedp0;
      primop0 = primop1;
      primop1 = tem;
      tem = op0;
      op0 = op1;
      op1 = tem;
      *op0_ptr = op0;
      *op1_ptr = op1;
      unsignedp0 = unsignedp1;
      unsignedp1 = temi;
      temi = real1;
      real1 = real2;
      real2 = temi;

      switch (code)
	{
	case LT_EXPR:
	  code = GT_EXPR;
	  break;
	case GT_EXPR:
	  code = LT_EXPR;
	  break;
	case LE_EXPR:
	  code = GE_EXPR;
	  break;
	case GE_EXPR:
	  code = LE_EXPR;
	  break;
	default:
	  break;
	}
      *rescode_ptr = code;
    }

  /* If comparing an integer against a constant more bits wide,
     maybe we can deduce a value of 1 or 0 independent of the data.
     Or else truncate the constant now
     rather than extend the variable at run time.

     This is only interesting if the constant is the wider arg.
     Also, it is not safe if the constant is unsigned and the
     variable arg is signed, since in this case the variable
     would be sign-extended and then regarded as unsigned.
     Our technique fails in this case because the lowest/highest
     possible unsigned results don't follow naturally from the
     lowest/highest possible values of the variable operand.
     For just EQ_EXPR and NE_EXPR there is another technique that
     could be used: see if the constant can be faithfully represented
     in the other operand's type, by truncating it and reextending it
     and see if that preserves the constant's value.  */

  if (!real1 && !real2
      && TREE_CODE (primop1) == INTEGER_CST
      && TYPE_PRECISION (TREE_TYPE (primop0)) < TYPE_PRECISION (*restype_ptr))
    {
      int min_gt, max_gt, min_lt, max_lt;
      tree maxval, minval;
      /* 1 if comparison is nominally unsigned.  */
      int unsignedp = TREE_UNSIGNED (*restype_ptr);
      tree val;

      type = signed_or_unsigned_type (unsignedp0, TREE_TYPE (primop0));

      maxval = TYPE_MAX_VALUE (type);
      minval = TYPE_MIN_VALUE (type);

      if (unsignedp && !unsignedp0)
	*restype_ptr = signed_type (*restype_ptr);

      if (TREE_TYPE (primop1) != *restype_ptr)
	primop1 = convert (*restype_ptr, primop1);
      if (type != *restype_ptr)
	{
	  minval = convert (*restype_ptr, minval);
	  maxval = convert (*restype_ptr, maxval);
	}

      if (unsignedp && unsignedp0)
	{
	  min_gt = INT_CST_LT_UNSIGNED (primop1, minval);
	  max_gt = INT_CST_LT_UNSIGNED (primop1, maxval);
	  min_lt = INT_CST_LT_UNSIGNED (minval, primop1);
	  max_lt = INT_CST_LT_UNSIGNED (maxval, primop1);
	}
      else
	{
	  min_gt = INT_CST_LT (primop1, minval);
	  max_gt = INT_CST_LT (primop1, maxval);
	  min_lt = INT_CST_LT (minval, primop1);
	  max_lt = INT_CST_LT (maxval, primop1);
	}

      val = 0;
      /* This used to be a switch, but Genix compiler can't handle that.  */
      if (code == NE_EXPR)
	{
	  if (max_lt || min_gt)
	    val = boolean_true_node;
	}
      else if (code == EQ_EXPR)
	{
	  if (max_lt || min_gt)
	    val = boolean_false_node;
	}
      else if (code == LT_EXPR)
	{
	  if (max_lt)
	    val = boolean_true_node;
	  if (!min_lt)
	    val = boolean_false_node;
	}
      else if (code == GT_EXPR)
	{
	  if (min_gt)
	    val = boolean_true_node;
	  if (!max_gt)
	    val = boolean_false_node;
	}
      else if (code == LE_EXPR)
	{
	  if (!max_gt)
	    val = boolean_true_node;
	  if (min_gt)
	    val = boolean_false_node;
	}
      else if (code == GE_EXPR)
	{
	  if (!min_lt)
	    val = boolean_true_node;
	  if (max_lt)
	    val = boolean_false_node;
	}

      /* If primop0 was sign-extended and unsigned comparison specd,
	 we did a signed comparison above using the signed type bounds.
	 But the comparison we output must be unsigned.

	 Also, for inequalities, VAL is no good; but if the signed
	 comparison had *any* fixed result, it follows that the
	 unsigned comparison just tests the sign in reverse
	 (positive values are LE, negative ones GE).
	 So we can generate an unsigned comparison
	 against an extreme value of the signed type.  */

      if (unsignedp && !unsignedp0)
	{
	  if (val != 0)
	    switch (code)
	      {
	      case LT_EXPR:
	      case GE_EXPR:
		primop1 = TYPE_MIN_VALUE (type);
		val = 0;
		break;

	      case LE_EXPR:
	      case GT_EXPR:
		primop1 = TYPE_MAX_VALUE (type);
		val = 0;
		break;

	      default:
		break;
	      }
	  type = unsigned_type (type);
	}

      if (!max_gt && !unsignedp0 && TREE_CODE (primop0) != INTEGER_CST)
	{
	  /* This is the case of (char)x >?< 0x80, which people used to use
	     expecting old C compilers to change the 0x80 into -0x80.  */
	  if (val == boolean_false_node)
	    warning ("comparison is always 0 due to limited range of data type");
	  if (val == boolean_true_node)
	    warning ("comparison is always 1 due to limited range of data type");
	}

      if (!min_lt && unsignedp0 && TREE_CODE (primop0) != INTEGER_CST)
	{
	  /* This is the case of (unsigned char)x >?< -1 or < 0.  */
	  if (val == boolean_false_node)
	    warning ("comparison is always 0 due to limited range of data type");
	  if (val == boolean_true_node)
	    warning ("comparison is always 1 due to limited range of data type");
	}

      if (val != 0)
	{
	  /* Don't forget to evaluate PRIMOP0 if it has side effects.  */
	  if (TREE_SIDE_EFFECTS (primop0))
	    return build (COMPOUND_EXPR, TREE_TYPE (val), primop0, val);
	  return val;
	}

      /* Value is not predetermined, but do the comparison
	 in the type of the operand that is not constant.
	 TYPE is already properly set.  */
    }
  else if (real1 && real2
	   && (TYPE_PRECISION (TREE_TYPE (primop0))
	       == TYPE_PRECISION (TREE_TYPE (primop1))))
    type = TREE_TYPE (primop0);

  /* If args' natural types are both narrower than nominal type
     and both extend in the same manner, compare them
     in the type of the wider arg.
     Otherwise must actually extend both to the nominal
     common type lest different ways of extending
     alter the result.
     (eg, (short)-1 == (unsigned short)-1  should be 0.)  */

  else if (unsignedp0 == unsignedp1 && real1 == real2
	   && TYPE_PRECISION (TREE_TYPE (primop0)) < TYPE_PRECISION (*restype_ptr)
	   && TYPE_PRECISION (TREE_TYPE (primop1)) < TYPE_PRECISION (*restype_ptr))
    {
      type = common_type (TREE_TYPE (primop0), TREE_TYPE (primop1));
      type = signed_or_unsigned_type (unsignedp0
				      || TREE_UNSIGNED (*restype_ptr),
				      type);
      /* Make sure shorter operand is extended the right way
	 to match the longer operand.  */
      primop0 = convert (signed_or_unsigned_type (unsignedp0, TREE_TYPE (primop0)),
			 primop0);
      primop1 = convert (signed_or_unsigned_type (unsignedp1, TREE_TYPE (primop1)),
			 primop1);
    }
  else
    {
      /* Here we must do the comparison on the nominal type
	 using the args exactly as we received them.  */
      type = *restype_ptr;
      primop0 = op0;
      primop1 = op1;

      if (!real1 && !real2 && integer_zerop (primop1)
	  && TREE_UNSIGNED (*restype_ptr))
	{
	  tree value = 0;
	  switch (code)
	    {
	    case GE_EXPR:
	      /* All unsigned values are >= 0, so we warn if extra warnings
		 are requested.  However, if OP0 is a constant that is
		 >= 0, the signedness of the comparison isn't an issue,
		 so suppress the warning.  */
	      if (extra_warnings
		  && ! (TREE_CODE (primop0) == INTEGER_CST
			&& ! TREE_OVERFLOW (convert (signed_type (type),
						     primop0))))
		warning ("unsigned value >= 0 is always 1");
	      value = boolean_true_node;
	      break;

	    case LT_EXPR:
	      if (extra_warnings
		  && ! (TREE_CODE (primop0) == INTEGER_CST
			&& ! TREE_OVERFLOW (convert (signed_type (type),
						     primop0))))
		warning ("unsigned value < 0 is always 0");
	      value = boolean_false_node;
	      break;

	    default:
	      break;
	    }

	  if (value != 0)
	    {
	      /* Don't forget to evaluate PRIMOP0 if it has side effects.  */
	      if (TREE_SIDE_EFFECTS (primop0))
		return build (COMPOUND_EXPR, TREE_TYPE (value),
			      primop0, value);
	      return value;
	    }
	}
    }

  *op0_ptr = convert (type, primop0);
  *op1_ptr = convert (type, primop1);

  *restype_ptr = boolean_type_node;

  return 0;
}


#if 0
/* Return a tree for the sum or difference (RESULTCODE says which)
   of pointer PTROP and integer INTOP.  */

static tree
pointer_int_sum (resultcode, ptrop, intop)
     enum tree_code resultcode;
     register tree ptrop, intop;
{
  tree size_exp;

  register tree result;
  register tree folded;

  /* The result is a pointer of the same type that is being added.  */

  register tree result_type = TREE_TYPE (ptrop);

  if (TREE_CODE (TREE_TYPE (result_type)) == VOID_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	pedwarn ("untyped pointer used in arithmetic");
      size_exp = integer_one_node;
    }
  else if (TREE_CODE (TREE_TYPE (result_type)) == FUNCTION_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	pedwarn ("pointer to a function used in arithmetic");
      size_exp = integer_one_node;
    }
  else
    size_exp = c_size_in_bytes (TREE_TYPE (result_type));

  /* If what we are about to multiply by the size of the elements
     contains a constant term, apply distributive law
     and multiply that constant term separately.
     This helps produce common subexpressions.  */

  if ((TREE_CODE (intop) == PLUS_EXPR || TREE_CODE (intop) == MINUS_EXPR)
      && ! TREE_CONSTANT (intop)
      && TREE_CONSTANT (TREE_OPERAND (intop, 1))
      && TREE_CONSTANT (size_exp)
      /* If the constant comes from pointer subtraction,
	 skip this optimization--it would cause an error.  */
      && TREE_CODE (TREE_TYPE (TREE_OPERAND (intop, 0))) == INTEGER_TYPE
      /* If the constant is unsigned, and smaller than the pointer size,
	 then we must skip this optimization.  This is because it could cause
	 an overflow error if the constant is negative but INTOP is not.  */
      && (! TREE_UNSIGNED (TREE_TYPE (intop))
	  || (TYPE_PRECISION (TREE_TYPE (intop))
	      == TYPE_PRECISION (TREE_TYPE (ptrop)))))
    {
      enum tree_code subcode = resultcode;
      tree int_type = TREE_TYPE (intop);
      if (TREE_CODE (intop) == MINUS_EXPR)
	subcode = (subcode == PLUS_EXPR ? MINUS_EXPR : PLUS_EXPR);
      /* Convert both subexpression types to the type of intop,
	 because weird cases involving pointer arithmetic
	 can result in a sum or difference with different type args.  */
      ptrop = build_binary_op (subcode, ptrop,
			       convert (int_type, TREE_OPERAND (intop, 1)), 1);
      intop = convert (int_type, TREE_OPERAND (intop, 0));
    }

  /* Convert the integer argument to a type the same size as sizetype
     so the multiply won't overflow spuriously.  */

  if (TYPE_PRECISION (TREE_TYPE (intop)) != TYPE_PRECISION (sizetype)
      || TREE_UNSIGNED (TREE_TYPE (intop)) != TREE_UNSIGNED (sizetype))
    intop = convert (type_for_size (TYPE_PRECISION (sizetype),
				    TREE_UNSIGNED (sizetype)), intop);

  /* Replace the integer argument with a suitable product by the object size.
     Do this multiplication as signed, then convert to the appropriate
     pointer type (actually unsigned integral).  */

  intop = convert (result_type,
		   build_binary_op (MULT_EXPR, intop,
				    convert (TREE_TYPE (intop), size_exp), 1));

  /* Create the sum or difference.  */

  result = build (resultcode, result_type, ptrop, intop);

  folded = fold (result);
  if (folded == result)
    TREE_CONSTANT (folded) = TREE_CONSTANT (ptrop) & TREE_CONSTANT (intop);
  return folded;
}

/* Return a tree for the difference of pointers OP0 and OP1.
   The resulting tree has type int.  */

static tree
pointer_diff (op0, op1)
     register tree op0, op1;
{
  register tree result, folded;
  tree restype = ptrdiff_type_node;

  tree target_type = TREE_TYPE (TREE_TYPE (op0));

  if (pedantic || warn_pointer_arith)
    {
      if (TREE_CODE (target_type) == VOID_TYPE)
	pedwarn ("untyped pointer used in subtraction");
      if (TREE_CODE (target_type) == FUNCTION_TYPE)
	pedwarn ("pointer to a function used in subtraction");
    }

  /* First do the subtraction as integers;
     then drop through to build the divide operator.
     Do not do default conversions on the minus operator
     in case restype is a short type.  */

  op0 = build_binary_op (MINUS_EXPR, convert (restype, op0),
			 convert (restype, op1), 0);
  /* This generates an error if op1 is pointer to incomplete type.  */
  if (TYPE_SIZE (TREE_TYPE (TREE_TYPE (op1))) == 0)
    error ("arithmetic on pointer to an incomplete type");

  /* This generates an error if op0 is pointer to incomplete type.  */
  op1 = c_size_in_bytes (target_type);

  /* Divide by the size, in easiest possible way.  */

  result = build (EXACT_DIV_EXPR, restype, op0, convert (restype, op1));

  folded = fold (result);
  if (folded == result)
    TREE_CONSTANT (folded) = TREE_CONSTANT (op0) & TREE_CONSTANT (op1);
  return folded;
}
#endif

/* Do `exp = require_complete_type (exp);' to make sure exp
   does not have an incomplete type.  (That includes void types.)  */

tree
require_complete_type (value)
     tree value;
{
  tree type = TREE_TYPE (value);

  /* First, detect a valid value with a complete type.  */
  if (TYPE_SIZE (type) != 0
      && type != void_type_node)
    return value;

  incomplete_type_error (value, type);
  return error_mark_node;
}

/* Convert value RHS to type TYPE as preparation for an assignment
   to an lvalue of type TYPE.
   The real work of conversion is done by `convert'.
   The purpose of this function is to generate error messages
   for assignments that are not allowed in C.
   ERRTYPE is a string to use in error messages:
   "assignment", "return", etc.  If it is null, this is parameter passing
   for a function call (and different error messages are output).  Otherwise,
   it may be a name stored in the spelling stack and interpreted by
   get_spelling.

   FUNNAME is the name of the function being called,
   as an IDENTIFIER_NODE, or null.
   PARMNUM is the number of the argument, for printing in error messages.  */

static tree
convert_for_assignment (type, rhs, errtype, fundecl, funname, parmnum)
     tree type, rhs;
     char *errtype;
     tree fundecl, funname;
     int parmnum;
{
  register enum tree_code codel = TREE_CODE (type);
  register tree rhstype;
  register enum tree_code coder;

  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
  /* Do not use STRIP_NOPS here.  We do not want an enumerator
     whose value is 0 to count as a null pointer constant.  */
  if (TREE_CODE (rhs) == NON_LVALUE_EXPR)
    rhs = TREE_OPERAND (rhs, 0);

  if (TREE_CODE (TREE_TYPE (rhs)) == ARRAY_TYPE
      || TREE_CODE (TREE_TYPE (rhs)) == FUNCTION_TYPE)
    rhs = default_conversion (rhs);
  else if (optimize && TREE_CODE (rhs) == VAR_DECL)
    rhs = decl_constant_value (rhs);

  rhstype = TREE_TYPE (rhs);
  coder = TREE_CODE (rhstype);

  if (coder == ERROR_MARK)
    return error_mark_node;

  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (rhstype))
    {
      overflow_warning (rhs);
#if NOT_NEEDED_FOR_M2
      /* Check for Objective-C protocols.  This will issue a warning if
	 there are protocol violations.  No need to use the return value.  */
      maybe_objc_comptypes (type, rhstype, 0);
#endif
      return rhs;
    }

  if (coder == VOID_TYPE)
    {
      error ("void value not ignored as it ought to be");
      return error_mark_node;
    }
  /* Arithmetic types all interconvert, and enum is treated like int.  */
  if ((codel == INTEGER_TYPE || codel == REAL_TYPE || codel == ENUMERAL_TYPE
       || codel == COMPLEX_TYPE)
      && (coder == INTEGER_TYPE || coder == REAL_TYPE || coder == ENUMERAL_TYPE
	  || coder == COMPLEX_TYPE))
    return convert_and_check (type, rhs);

  /* Conversion to a transparent union from its member types.
     This applies only to function arguments.  */
  else if (codel == UNION_TYPE && TYPE_TRANSPARENT_UNION (type) && ! errtype)
    {
      tree memb_types;
      tree marginal_memb_type = 0;

      for (memb_types = TYPE_FIELDS (type); memb_types;
	   memb_types = TREE_CHAIN (memb_types))
	{
	  tree memb_type = TREE_TYPE (memb_types);

	  if (comptypes (TYPE_MAIN_VARIANT (memb_type),
			 TYPE_MAIN_VARIANT (rhstype)))
	    break;

	  if (TREE_CODE (memb_type) != POINTER_TYPE)
	    continue;

	  if (coder == POINTER_TYPE)
	    {
	      register tree ttl = TREE_TYPE (memb_type);
	      register tree ttr = TREE_TYPE (rhstype);

	      /* Any non-function converts to a [const][volatile] void *
		 and vice versa; otherwise, targets must be the same.
		 Meanwhile, the lhs target must have all the qualifiers of
		 the rhs.  */
	      if (TYPE_MAIN_VARIANT (ttl) == void_type_node
		  || TYPE_MAIN_VARIANT (ttr) == void_type_node
		  || comp_target_types (memb_type, rhstype))
		{
		  /* If this type won't generate any warnings, use it.  */
		  if ((TREE_CODE (ttr) == FUNCTION_TYPE
		       && TREE_CODE (ttl) == FUNCTION_TYPE)
		      ? ((! TYPE_READONLY (ttl) | TYPE_READONLY (ttr))
			 & (! TYPE_VOLATILE (ttl) | TYPE_VOLATILE (ttr)))
		      : ((TYPE_READONLY (ttl) | ! TYPE_READONLY (ttr))
			 & (TYPE_VOLATILE (ttl) | ! TYPE_VOLATILE (ttr))))
		    break;

		  /* Keep looking for a better type, but remember this one.  */
		  if (! marginal_memb_type)
		    marginal_memb_type = memb_type;
		}
	    }

	  /* Can convert integer zero to any pointer type.  */
	  if (integer_zerop (rhs)
	      || (TREE_CODE (rhs) == NOP_EXPR
		  && integer_zerop (TREE_OPERAND (rhs, 0))))
	    {
	      rhs = null_pointer_node;
	      break;
	    }
	}

      if (memb_types || marginal_memb_type)
	{
	  if (! memb_types)
	    {
	      /* We have only a marginally acceptable member type;
		 it needs a warning.  */
	      register tree ttl = TREE_TYPE (marginal_memb_type);
	      register tree ttr = TREE_TYPE (rhstype);

	      /* Const and volatile mean something different for function
		 types, so the usual warnings are not appropriate.  */
	      if (TREE_CODE (ttr) == FUNCTION_TYPE
		  && TREE_CODE (ttl) == FUNCTION_TYPE)
		{
		  /* Because const and volatile on functions are
		     restrictions that say the function will not do
		     certain things, it is okay to use a const or volatile
		     function where an ordinary one is wanted, but not
		     vice-versa.  */
		  if (TYPE_READONLY (ttl) && ! TYPE_READONLY (ttr))
		    warn_for_assignment ("%s makes `const *' function pointer from non-const",
					 get_spelling (errtype), funname,
					 parmnum);
		  if (TYPE_VOLATILE (ttl) && ! TYPE_VOLATILE (ttr))
		    warn_for_assignment ("%s makes `volatile *' function pointer from non-volatile",
					 get_spelling (errtype), funname,
					 parmnum);
		}
	      else
		{
		  if (! TYPE_READONLY (ttl) && TYPE_READONLY (ttr))
		    warn_for_assignment ("%s discards `const' from pointer target type",
					 get_spelling (errtype), funname,
					 parmnum);
		  if (! TYPE_VOLATILE (ttl) && TYPE_VOLATILE (ttr))
		    warn_for_assignment ("%s discards `volatile' from pointer target type",
					 get_spelling (errtype), funname,
					 parmnum);
		}
	    }
	  
	  if (pedantic && ! DECL_IN_SYSTEM_HEADER (fundecl))
	    pedwarn ("ANSI C prohibits argument conversion to union type");

	  return build1 (NOP_EXPR, type, rhs);
	}
    }

  /* Conversions among pointers */
  else if (codel == POINTER_TYPE && coder == POINTER_TYPE)
    {
      register tree ttl = TREE_TYPE (type);
      register tree ttr = TREE_TYPE (rhstype);

      /* Any non-function converts to a [const][volatile] void *
	 and vice versa; otherwise, targets must be the same.
	 Meanwhile, the lhs target must have all the qualifiers of the rhs.  */
      if (TYPE_MAIN_VARIANT (ttl) == void_type_node
	  || TYPE_MAIN_VARIANT (ttr) == void_type_node
	  || comp_target_types (type, rhstype)
	  || (unsigned_type (TYPE_MAIN_VARIANT (ttl))
	      == unsigned_type (TYPE_MAIN_VARIANT (ttr))))
	{
	  if (pedantic
	      && ((TYPE_MAIN_VARIANT (ttl) == void_type_node
		   && TREE_CODE (ttr) == FUNCTION_TYPE)
		  ||
		  (TYPE_MAIN_VARIANT (ttr) == void_type_node
		   /* Check TREE_CODE to catch cases like (void *) (char *) 0
		      which are not ANSI null ptr constants.  */
		   && (!integer_zerop (rhs) || TREE_CODE (rhs) == NOP_EXPR)
		   && TREE_CODE (ttl) == FUNCTION_TYPE)))
	    warn_for_assignment ("ANSI forbids %s between function pointer and `void *'",
				 get_spelling (errtype), funname, parmnum);
	  /* Const and volatile mean something different for function types,
	     so the usual warnings are not appropriate.  */
	  else if (TREE_CODE (ttr) != FUNCTION_TYPE
		   && TREE_CODE (ttl) != FUNCTION_TYPE)
	    {
	      if (! TYPE_READONLY (ttl) && TYPE_READONLY (ttr))
		warn_for_assignment ("%s discards `const' from pointer target type",
				     get_spelling (errtype), funname, parmnum);
	      else if (! TYPE_VOLATILE (ttl) && TYPE_VOLATILE (ttr))
		warn_for_assignment ("%s discards `volatile' from pointer target type",
				     get_spelling (errtype), funname, parmnum);
	      /* If this is not a case of ignoring a mismatch in signedness,
		 no warning.  */
	      else if (TYPE_MAIN_VARIANT (ttl) == void_type_node
		       || TYPE_MAIN_VARIANT (ttr) == void_type_node
		       || comp_target_types (type, rhstype))
		;
	      /* If there is a mismatch, do warn.  */
	      else if (pedantic)
		warn_for_assignment ("pointer targets in %s differ in signedness",
				     get_spelling (errtype), funname, parmnum);
	    }
	  else if (TREE_CODE (ttl) == FUNCTION_TYPE
		   && TREE_CODE (ttr) == FUNCTION_TYPE)
	    {
	      /* Because const and volatile on functions are restrictions
		 that say the function will not do certain things,
		 it is okay to use a const or volatile function
		 where an ordinary one is wanted, but not vice-versa.  */
	      if (TYPE_READONLY (ttl) && ! TYPE_READONLY (ttr))
		warn_for_assignment ("%s makes `const *' function pointer from non-const",
				     get_spelling (errtype), funname, parmnum);
	      if (TYPE_VOLATILE (ttl) && ! TYPE_VOLATILE (ttr))
		warn_for_assignment ("%s makes `volatile *' function pointer from non-volatile",
				     get_spelling (errtype), funname, parmnum);
	    }
	}
      else
	warn_for_assignment ("%s from incompatible pointer type",
			     get_spelling (errtype), funname, parmnum);
      return convert (type, rhs);
    }
  else if (codel == POINTER_TYPE && coder == INTEGER_TYPE)
    {
      /* An explicit constant 0 can convert to a pointer,
	 or one that results from arithmetic, even including
	 a cast to integer type.  */
      if (! (TREE_CODE (rhs) == INTEGER_CST && integer_zerop (rhs))
	  &&
	  ! (TREE_CODE (rhs) == NOP_EXPR
	     && TREE_CODE (TREE_TYPE (rhs)) == INTEGER_TYPE
	     && TREE_CODE (TREE_OPERAND (rhs, 0)) == INTEGER_CST
	     && integer_zerop (TREE_OPERAND (rhs, 0))))
	{
	  warn_for_assignment ("%s makes pointer from integer without a cast",
			       get_spelling (errtype), funname, parmnum);
	  return convert (type, rhs);
	}
      return null_pointer_node;
    }
  else if (codel == INTEGER_TYPE && coder == POINTER_TYPE)
    {
      warn_for_assignment ("%s makes integer from pointer without a cast",
			   get_spelling (errtype), funname, parmnum);
      return convert (type, rhs);
    }

  if (!errtype)
    {
      if (funname)
 	{
#if NOT_NEEDED_FOR_M2
 	  tree selector = maybe_building_objc_message_expr ();
 
 	  if (selector && parmnum > 2)
 	    error ("incompatible type for argument %d of `%s'",
		   parmnum - 2, IDENTIFIER_POINTER (selector));
 	  else
#endif
	    error ("incompatible type for argument %d of `%s'",
		   parmnum, IDENTIFIER_POINTER (funname));
	}
      else
	error ("incompatible type for argument %d of indirect function call",
	       parmnum);
    }
  else
    error ("incompatible types in %s", get_spelling (errtype));

  return error_mark_node;
}

/* Build an assignment expression of lvalue LHS from value RHS.
   MODIFYCODE is the code for a binary operator that we use
   to combine the old value of LHS with RHS to get the new value.
   Or else MODIFYCODE is NOP_EXPR meaning do a simple assignment.  */

tree
build_modify_expr (lhs, modifycode, rhs)
     tree lhs, rhs;
     enum tree_code modifycode;
{
  register tree result;
  tree newrhs;
  tree lhstype = TREE_TYPE (lhs);
  tree olhstype = lhstype;

  /* Types that aren't fully specified cannot be used in assignments.  */
  lhs = require_complete_type (lhs);

  /* Avoid duplicate error messages from operands that had errors.  */
  if (TREE_CODE (lhs) == ERROR_MARK || TREE_CODE (rhs) == ERROR_MARK)
    return error_mark_node;

  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
  /* Do not use STRIP_NOPS here.  We do not want an enumerator
     whose value is 0 to count as a null pointer constant.  */
  if (TREE_CODE (rhs) == NON_LVALUE_EXPR)
    rhs = TREE_OPERAND (rhs, 0);

  newrhs = rhs;

  /* Handle control structure constructs used as "lvalues".  */

  switch (TREE_CODE (lhs))
    {
      /* Handle (a, b) used as an "lvalue".  */
    case COMPOUND_EXPR:
      pedantic_lvalue_warning (COMPOUND_EXPR);
      newrhs = build_modify_expr (TREE_OPERAND (lhs, 1),
				  modifycode, rhs);
      if (TREE_CODE (newrhs) == ERROR_MARK)
	return error_mark_node;
      return build (COMPOUND_EXPR, lhstype,
		    TREE_OPERAND (lhs, 0), newrhs);
 
      /* Handle (a ? b : c) used as an "lvalue".  */
    case COND_EXPR:
      pedantic_lvalue_warning (COND_EXPR);
      rhs = save_expr (rhs);
      {
	/* Produce (a ? (b = rhs) : (c = rhs))
	   except that the RHS goes through a save-expr
	   so the code to compute it is only emitted once.  */
	tree cond
	  = build_conditional_expr (TREE_OPERAND (lhs, 0),
				    build_modify_expr (TREE_OPERAND (lhs, 1),
						       modifycode, rhs),
				    build_modify_expr (TREE_OPERAND (lhs, 2),
						       modifycode, rhs));
	if (TREE_CODE (cond) == ERROR_MARK)
	  return cond;
	/* Make sure the code to compute the rhs comes out
	   before the split.  */
	return build (COMPOUND_EXPR, TREE_TYPE (lhs),
		      /* But cast it to void to avoid an "unused" error.  */
		      convert (void_type_node, rhs), cond);
      }
    default:
      break;
    }

  /* If a binary op has been requested, combine the old LHS value with the RHS
     producing the value we should actually store into the LHS.  */

  if (modifycode != NOP_EXPR)
    {
      lhs = stabilize_reference (lhs);
      newrhs = build_binary_op (modifycode, lhs, rhs, 1);
    }

  /* Handle a cast used as an "lvalue".
     We have already performed any binary operator using the value as cast.
     Now convert the result to the cast type of the lhs,
     and then true type of the lhs and store it there;
     then convert result back to the cast type to be the value
     of the assignment.  */

  switch (TREE_CODE (lhs))
    {
    case NOP_EXPR:
    case CONVERT_EXPR:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_ROUND_EXPR:
    case FIX_CEIL_EXPR:
      if (TREE_CODE (TREE_TYPE (newrhs)) == ARRAY_TYPE
	  || TREE_CODE (TREE_TYPE (newrhs)) == FUNCTION_TYPE)
	newrhs = default_conversion (newrhs);
      {
	tree inner_lhs = TREE_OPERAND (lhs, 0);
	tree result;
	result = build_modify_expr (inner_lhs, NOP_EXPR,
				    convert (TREE_TYPE (inner_lhs),
					     convert (lhstype, newrhs)));
	if (TREE_CODE (result) == ERROR_MARK)
	  return result;
	pedantic_lvalue_warning (CONVERT_EXPR);
	return convert (TREE_TYPE (lhs), result);
      }
      
    default:
      break;
    }

  /* Now we have handled acceptable kinds of LHS that are not truly lvalues.
     Reject anything strange now.  */

  if (!lvalue_or_else (lhs, "assignment"))
    return error_mark_node;

  /* Warn about storing in something that is `const'.  */

  if (TREE_READONLY (lhs) || TYPE_READONLY (lhstype)
      || ((TREE_CODE (lhstype) == RECORD_TYPE
	   || TREE_CODE (lhstype) == UNION_TYPE)
	  && C_TYPE_FIELDS_READONLY (lhstype)))
    readonly_warning (lhs, "assignment");

  /* If storing into a structure or union member,
     it has probably been given type `int'.
     Compute the type that would go with
     the actual amount of storage the member occupies.  */

  if (TREE_CODE (lhs) == COMPONENT_REF
      && (TREE_CODE (lhstype) == INTEGER_TYPE
	  || TREE_CODE (lhstype) == REAL_TYPE
	  || TREE_CODE (lhstype) == ENUMERAL_TYPE))
    lhstype = TREE_TYPE (get_unwidened (lhs, 0));

  /* If storing in a field that is in actuality a short or narrower than one,
     we must store in the field in its actual type.  */

  if (lhstype != TREE_TYPE (lhs))
    {
      lhs = copy_node (lhs);
      TREE_TYPE (lhs) = lhstype;
    }

  /* Convert new value to destination type.  */

  newrhs = convert_for_assignment (lhstype, newrhs, "assignment",
				   NULL_TREE, NULL_TREE, 0);
  if (TREE_CODE (newrhs) == ERROR_MARK)
    return error_mark_node;

  result = build (MODIFY_EXPR, lhstype, lhs, newrhs);
  TREE_SIDE_EFFECTS (result) = 1;

  /* If we got the LHS in a different type for storing in,
     convert the result back to the nominal type of LHS
     so that the value we return always has the same type
     as the LHS argument.  */

  if (olhstype == TREE_TYPE (result))
    return result;
  return convert_for_assignment (olhstype, result, "assignment",
				 NULL_TREE, NULL_TREE, 0);
}

/* Construct and perhaps optimize a tree representation
   for a unary operation.  CODE, a tree_code, specifies the operation
   and XARG is the operand.  NOCONVERT nonzero suppresses
   the default promotions (such as from short to int).  */

tree
build_unary_op (code, xarg, noconvert)
     enum tree_code code;
     tree xarg;
     int noconvert;
{
  /* No default_conversion here.  It causes trouble for ADDR_EXPR.  */
  register tree arg = xarg;
  register tree argtype = 0;
  register enum tree_code typecode = TREE_CODE (TREE_TYPE (arg));
  char *errstring = NULL;
  tree val;

  if (typecode == ERROR_MARK)
    return error_mark_node;
  if (typecode == ENUMERAL_TYPE)
    typecode = INTEGER_TYPE;

  switch (code)
    {
    case CONVERT_EXPR:
      /* This is used for unary plus, because a CONVERT_EXPR
	 is enough to prevent anybody from looking inside for
	 associativity, but won't generate any code.  */
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE
	    || typecode == COMPLEX_TYPE))
        errstring = "wrong type argument to unary plus";
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case NEGATE_EXPR:
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE
	    || typecode == COMPLEX_TYPE))
        errstring = "wrong type argument to unary minus";
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case BIT_NOT_EXPR:
      if (typecode == COMPLEX_TYPE)
	{
	  code = CONJ_EXPR;
	  if (!noconvert)
	    arg = default_conversion (arg);
	}
      else if (typecode != INTEGER_TYPE)
        errstring = "wrong type argument to bit-complement";
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case ABS_EXPR:
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE
	    || typecode == COMPLEX_TYPE))
        errstring = "wrong type argument to abs";
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case CONJ_EXPR:
      /* Conjugating a real value is a no-op, but allow it anyway.  */
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE
	    || typecode == COMPLEX_TYPE))
	errstring = "wrong type argument to conjugation";
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case TRUTH_NOT_EXPR:
      if (typecode != INTEGER_TYPE
	  && typecode != REAL_TYPE && typecode != POINTER_TYPE
	  && typecode != COMPLEX_TYPE
	  /* These will convert to a pointer.  */
	  && typecode != ARRAY_TYPE && typecode != FUNCTION_TYPE)
	{
	  errstring = "wrong type argument to unary exclamation mark";
	  break;
	}
      arg = truthvalue_conversion (arg);
      return invert_truthvalue (arg);

    case NOP_EXPR:
      break;

    case REALPART_EXPR:
      if (TREE_CODE (arg) == COMPLEX_CST)
	return TREE_REALPART (arg);
      else if (TREE_CODE (TREE_TYPE (arg)) == COMPLEX_TYPE)
	return fold (build1 (REALPART_EXPR, TREE_TYPE (TREE_TYPE (arg)), arg));
      else
	return arg;

    case IMAGPART_EXPR:
      if (TREE_CODE (arg) == COMPLEX_CST)
	return TREE_IMAGPART (arg);
      else if (TREE_CODE (TREE_TYPE (arg)) == COMPLEX_TYPE)
	return fold (build1 (IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (arg)), arg));
      else
	return convert (TREE_TYPE (arg), integer_zero_node);
      
    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      /* Handle complex lvalues (when permitted)
	 by reduction to simpler cases.  */

      val = unary_complex_lvalue (code, arg);
      if (val != 0)
	return val;

      /* Increment or decrement the real part of the value,
	 and don't change the imaginary part.  */
      if (typecode == COMPLEX_TYPE)
	{
	  tree real, imag;

	  arg = stabilize_reference (arg);
	  real = build_unary_op (REALPART_EXPR, arg, 1);
	  imag = build_unary_op (IMAGPART_EXPR, arg, 1);
	  return build (COMPLEX_EXPR, TREE_TYPE (arg),
			build_unary_op (code, real, 1), imag);
	}

      /* Report invalid types.  */

      if (typecode != POINTER_TYPE
	  && typecode != INTEGER_TYPE && typecode != REAL_TYPE)
	{
	  if (code == PREINCREMENT_EXPR || code == POSTINCREMENT_EXPR)
	    errstring ="wrong type argument to increment";
	  else
	    errstring ="wrong type argument to decrement";
	  break;
	}

      {
	register tree inc;
	tree result_type = TREE_TYPE (arg);

	arg = get_unwidened (arg, 0);
	argtype = TREE_TYPE (arg);

	/* Compute the increment.  */

	if (typecode == POINTER_TYPE)
	  {
	    /* If pointer target is an undefined struct,
	       we just cannot know how to do the arithmetic.  */
	    if (TYPE_SIZE (TREE_TYPE (result_type)) == 0)
	      error ("%s of pointer to unknown structure",
		       ((code == PREINCREMENT_EXPR
			 || code == POSTINCREMENT_EXPR)
			? "increment" : "decrement"));
	    else if ((pedantic || warn_pointer_arith)
		     && (TREE_CODE (TREE_TYPE (result_type)) == FUNCTION_TYPE
			 || TREE_CODE (TREE_TYPE (result_type)) == VOID_TYPE))
	      pedwarn ("wrong type argument to %s",
		       ((code == PREINCREMENT_EXPR
			 || code == POSTINCREMENT_EXPR)
			? "increment" : "decrement"));
	    inc = c_size_in_bytes (TREE_TYPE (result_type));
	  }
	else
	  inc = integer_one_node;

	inc = convert (argtype, inc);

	/* Handle incrementing a cast-expression.  */

	while (1)
	  switch (TREE_CODE (arg))
	    {
	    case NOP_EXPR:
	    case CONVERT_EXPR:
	    case FLOAT_EXPR:
	    case FIX_TRUNC_EXPR:
	    case FIX_FLOOR_EXPR:
	    case FIX_ROUND_EXPR:
	    case FIX_CEIL_EXPR:
	      pedantic_lvalue_warning (CONVERT_EXPR);
	      /* If the real type has the same machine representation
		 as the type it is cast to, we can make better output
		 by adding directly to the inside of the cast.  */
	      if ((TREE_CODE (TREE_TYPE (arg))
		   == TREE_CODE (TREE_TYPE (TREE_OPERAND (arg, 0))))
		  && (TYPE_MODE (TREE_TYPE (arg))
		      == TYPE_MODE (TREE_TYPE (TREE_OPERAND (arg, 0)))))
		arg = TREE_OPERAND (arg, 0);
	      else
		{
		  tree incremented, modify, value;
		  arg = stabilize_reference (arg);
		  if (code == PREINCREMENT_EXPR || code == PREDECREMENT_EXPR)
		    value = arg;
		  else
		    value = save_expr (arg);
		  incremented = build (((code == PREINCREMENT_EXPR
					 || code == POSTINCREMENT_EXPR)
					? PLUS_EXPR : MINUS_EXPR),
				       argtype, value, inc);
		  TREE_SIDE_EFFECTS (incremented) = 1;
		  modify = build_modify_expr (arg, NOP_EXPR, incremented);
		  value = build (COMPOUND_EXPR, TREE_TYPE (arg), modify, value);
		  TREE_USED (value) = 1;
		  return value;
		}
	      break;

	    default:
	      goto give_up;
	    }
      give_up:

	/* Complain about anything else that is not a true lvalue.  */
	if (!lvalue_or_else (arg, ((code == PREINCREMENT_EXPR
				    || code == POSTINCREMENT_EXPR)
				   ? "increment" : "decrement")))
	  return error_mark_node;

	/* Report a read-only lvalue.  */
	if (TREE_READONLY (arg))
	  readonly_warning (arg, 
			    ((code == PREINCREMENT_EXPR
			      || code == POSTINCREMENT_EXPR)
			     ? "increment" : "decrement"));

	val = build (code, TREE_TYPE (arg), arg, inc);
	TREE_SIDE_EFFECTS (val) = 1;
	val = convert (result_type, val);
	if (TREE_CODE (val) != code)
	  TREE_NO_UNUSED_WARNING (val) = 1;
	return val;
      }

    case ADDR_EXPR:
      /* Note that this operation never does default_conversion
	 regardless of NOCONVERT.  */

      /* Let &* cancel out to simplify resulting code.  */
      if (TREE_CODE (arg) == INDIRECT_REF)
	{
	  /* Don't let this be an lvalue.  */
	  if (lvalue_p (TREE_OPERAND (arg, 0)))
	    return non_lvalue (TREE_OPERAND (arg, 0));
	  return TREE_OPERAND (arg, 0);
	}

      /* For &x[y], return x+y */
      if (TREE_CODE (arg) == ARRAY_REF)
	{
	  if (mark_addressable (TREE_OPERAND (arg, 0)) == 0)
	    return error_mark_node;
	  return build_binary_op (PLUS_EXPR, TREE_OPERAND (arg, 0),
				  TREE_OPERAND (arg, 1), 1);
	}

      /* Handle complex lvalues (when permitted)
	 by reduction to simpler cases.  */
      val = unary_complex_lvalue (code, arg);
      if (val != 0)
	return val;

#if 0 /* Turned off because inconsistent;
	 float f; *&(int)f = 3.4 stores in int format
	 whereas (int)f = 3.4 stores in float format.  */
      /* Address of a cast is just a cast of the address
	 of the operand of the cast.  */
      switch (TREE_CODE (arg))
	{
	case NOP_EXPR:
	case CONVERT_EXPR:
	case FLOAT_EXPR:
	case FIX_TRUNC_EXPR:
	case FIX_FLOOR_EXPR:
	case FIX_ROUND_EXPR:
	case FIX_CEIL_EXPR:
	  if (pedantic)
	    pedwarn ("ANSI C forbids the address of a cast expression");
	  return convert (build_pointer_type (TREE_TYPE (arg)),
			  build_unary_op (ADDR_EXPR, TREE_OPERAND (arg, 0),
					  0));
	}
#endif

      /* Allow the address of a constructor if all the elements
	 are constant.  */
      if (TREE_CODE (arg) == CONSTRUCTOR && TREE_CONSTANT (arg))
	;
      /* Anything not already handled and not a true memory reference
	 is an error.  */
      else if (typecode != FUNCTION_TYPE && !lvalue_or_else (arg, "unary `&'"))
	return error_mark_node;

      /* Ordinary case; arg is a COMPONENT_REF or a decl.  */
      argtype = TREE_TYPE (arg);
      /* If the lvalue is const or volatile,
	 merge that into the type that the address will point to.  */
      if (TREE_CODE_CLASS (TREE_CODE (arg)) == 'd'
	  || TREE_CODE_CLASS (TREE_CODE (arg)) == 'r')
	{
	  if (TREE_READONLY (arg) || TREE_THIS_VOLATILE (arg))
	    argtype = build_c_type_variant (argtype,
					    TREE_READONLY (arg),
					    TREE_THIS_VOLATILE (arg));
	}

      argtype = build_pointer_type (argtype);

      if (mark_addressable (arg) == 0)
	return error_mark_node;

      {
	tree addr;

	if (TREE_CODE (arg) == COMPONENT_REF)
	  {
	    tree field = TREE_OPERAND (arg, 1);

	    addr = build_unary_op (ADDR_EXPR, TREE_OPERAND (arg, 0), 0);

	    if (DECL_C_BIT_FIELD (field))
	      {
		error ("attempt to take address of bit-field structure member `%s'",
		       IDENTIFIER_POINTER (DECL_NAME (field)));
		return error_mark_node;
	      }

	    addr = convert (argtype, addr);

	    if (! integer_zerop (DECL_FIELD_BITPOS (field)))
	      {
		tree offset
		  = size_binop (EASY_DIV_EXPR, DECL_FIELD_BITPOS (field),
				size_int (BITS_PER_UNIT));
		int flag = TREE_CONSTANT (addr);
		addr = fold (build (PLUS_EXPR, argtype,
				    addr, convert (argtype, offset)));
		TREE_CONSTANT (addr) = flag;
	      }
	  }
	else
	  addr = build1 (code, argtype, arg);

	/* Address of a static or external variable or
	   file-scope function counts as a constant.  */
	if (staticp (arg)
	    && ! (TREE_CODE (arg) == FUNCTION_DECL
		  && DECL_CONTEXT (arg) != 0))
	  TREE_CONSTANT (addr) = 1;
	return addr;
      }

    default:
      break;
    }

  if (!errstring)
    {
      if (argtype == 0)
	argtype = TREE_TYPE (arg);
      return fold (build1 (code, argtype, arg));
    }

  error (errstring);
  return error_mark_node;
}


/* Print an error message for invalid operands to arith operation CODE.
   NOP_EXPR is used as a special case (see truthvalue_conversion).  */

void
binary_op_error (code)
     enum tree_code code;
{
  register char *opname;

  switch (code)
    {
    case NOP_EXPR:
      error ("invalid truth-value expression");
      return;

    case PLUS_EXPR:
      opname = "+"; break;
    case MINUS_EXPR:
      opname = "-"; break;
    case MULT_EXPR:
      opname = "*"; break;
    case MAX_EXPR:
      opname = "max"; break;
    case MIN_EXPR:
      opname = "min"; break;
    case EQ_EXPR:
      opname = "=="; break;
    case NE_EXPR:
      opname = "!="; break;
    case LE_EXPR:
      opname = "<="; break;
    case GE_EXPR:
      opname = ">="; break;
    case LT_EXPR:
      opname = "<"; break;
    case GT_EXPR:
      opname = ">"; break;
    case LSHIFT_EXPR:
      opname = "<<"; break;
    case RSHIFT_EXPR:
      opname = ">>"; break;
    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
      opname = "%"; break;
    case TRUNC_DIV_EXPR:
    case FLOOR_DIV_EXPR:
      opname = "/"; break;
    case BIT_AND_EXPR:
      opname = "&"; break;
    case BIT_IOR_EXPR:
      opname = "|"; break;
    case TRUTH_ANDIF_EXPR:
      opname = "&&"; break;
    case TRUTH_ORIF_EXPR:
      opname = "||"; break;
    case BIT_XOR_EXPR:
      opname = "^"; break;
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      opname = "rotate"; break;
    default:
      opname = "unknown"; break;
    }
  error ("invalid operands to binary %s", opname);
}


/* Build a binary-operation expression without default conversions.
   CODE is the kind of expression to build.
   This function differs from `build' in several ways:
   the data type of the result is computed and recorded in it,
   warnings are generated if arg data types are invalid,
   special handling for addition and subtraction of pointers is known,
   and some optimization is done (operations on narrow ints
   are done in the narrower type when that gives the same result).
   Constant folding is also done before the result is returned.

   Note that the operands will never have enumeral types, or function
   or array types, because either they will have the default conversions
   performed or they have both just been converted to some other type in which
   the arithmetic is to be done.  */

tree
build_binary_op (code, orig_op0, orig_op1, convert_p)
     enum tree_code code;
     tree orig_op0, orig_op1;
     int convert_p;
{
  tree type0, type1;
  register enum tree_code code0, code1;
  tree op0, op1;

  /* Expression code to give to the expression when it is built.
     Normally this is CODE, which is what the caller asked for,
     but in some special cases we change it.  */
  register enum tree_code resultcode = code;

  /* Data type in which the computation is to be performed.
     In the simplest cases this is the common type of the arguments.  */
  register tree result_type = NULL;

  /* Nonzero means operands have already been type-converted
     in whatever way is necessary.
     Zero means they need to be converted to RESULT_TYPE.  */
  int converted = 0;

  /* Nonzero means create the expression with this type, rather than
     RESULT_TYPE.  */
  tree build_type = 0;

  /* Nonzero means after finally constructing the expression
     convert it to this type.  */
  tree final_type = 0;

  /* Nonzero if this is an operation like MIN or MAX which can
     safely be computed in short if both args are promoted shorts.
     Also implies COMMON.
     -1 indicates a bitwise operation; this makes a difference
     in the exact conditions for when it is safe to do the operation
     in a narrower mode.  */
  int shorten = 0;

  /* Nonzero if this is a comparison operation;
     if both args are promoted shorts, compare the original shorts.
     Also implies COMMON.  */
  int short_compare = 0;

  /* Nonzero if this is a right-shift operation, which can be computed on the
     original short and then promoted if the operand is a promoted short.  */
  int short_shift = 0;

  /* Nonzero means set RESULT_TYPE to the common type of the args.  */
  int common = 0;

  if (convert_p)
    {
      op0 = default_conversion (orig_op0);
      op1 = default_conversion (orig_op1);
    }
  else
    {
      op0 = orig_op0;
      op1 = orig_op1;
    }

  type0 = TREE_TYPE (op0);
  type1 = TREE_TYPE (op1);

  /* The expression codes of the data types of the arguments tell us
     whether the arguments are integers, floating, pointers, etc.  */
  code0 = TREE_CODE (base_type (type0));
  code1 = TREE_CODE (base_type (type1));

  /* Strip NON_LVALUE_EXPRs, etc., since we aren't using as an lvalue.  */
  STRIP_TYPE_NOPS (op0);
  STRIP_TYPE_NOPS (op1);

  /* If an error was already reported for one of the arguments,
     avoid reporting another error.  */

  if (code0 == ERROR_MARK || code1 == ERROR_MARK)
    return error_mark_node;

  switch (code)
    {
    case PLUS_EXPR:
      /* Handle the pointer + int case.  */
      if ((code0 == POINTER_TYPE && code1 == INTEGER_TYPE) ||
	  (code1 == POINTER_TYPE && code0 == INTEGER_TYPE)) {
	result_type = integer_type_node;
      } else if ((code0 == POINTER_TYPE) && (code1 == POINTER_TYPE)) {
	result_type = integer_type_node;
      }
      common = 1;
      break;

    case MINUS_EXPR:
      /* Handle the pointer + int case.  */
      if ((code0 == POINTER_TYPE && code1 == INTEGER_TYPE) ||
	  (code1 == POINTER_TYPE && code0 == INTEGER_TYPE))
	result_type = integer_type_node;
      common = 1;
      break;

    case MULT_EXPR:
      common = 1;
      break;

    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE
	   || code0 == COMPLEX_TYPE)
	  && (code1 == INTEGER_TYPE || code1 == REAL_TYPE
	      || code1 == COMPLEX_TYPE))
	{
	  if (!(code0 == INTEGER_TYPE && code1 == INTEGER_TYPE))
	    resultcode = RDIV_EXPR;
	  else
	    {
	      /* Although it would be tempting to shorten always here, that
		 loses on some targets, since the modulo instruction is
		 undefined if the quotient can't be represented in the
		 computation mode.  We shorten only if unsigned or if
		 dividing by something we know != -1.  */
	      shorten = (TREE_UNSIGNED (TREE_TYPE (orig_op0))
			 || (TREE_CODE (op1) == INTEGER_CST
			     && (TREE_INT_CST_LOW (op1) != -1
				 || TREE_INT_CST_HIGH (op1) != -1)));
	    }
	  common = 1;
	}
      break;

    case BIT_AND_EXPR:
    case BIT_ANDTC_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	shorten = -1;
      /* If one operand is a constant, and the other is a short type
	 that has been converted to an int,
	 really do the work in the short type and then convert the
	 result to int.  If we are lucky, the constant will be 0 or 1
	 in the short type, making the entire operation go away.  */
      if (TREE_CODE (op0) == INTEGER_CST
	  && TREE_CODE (op1) == NOP_EXPR
	  && TYPE_PRECISION (type1) > TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (op1, 0)))
	  && TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (op1, 0))))
	{
	  final_type = result_type;
	  op1 = TREE_OPERAND (op1, 0);
	  result_type = TREE_TYPE (op1);
	}
      if (TREE_CODE (op1) == INTEGER_CST
	  && TREE_CODE (op0) == NOP_EXPR
	  && TYPE_PRECISION (type0) > TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (op0, 0)))
	  && TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (op0, 0))))
	{
	  final_type = result_type;
	  op0 = TREE_OPERAND (op0, 0);
	  result_type = TREE_TYPE (op0);
	}
      break;

    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	{
	  /* Although it would be tempting to shorten always here, that loses
	     on some targets, since the modulo instruction is undefined if the
	     quotient can't be represented in the computation mode.  We shorten
	     only if unsigned or if dividing by something we know != -1.  */
	  shorten = (TREE_UNSIGNED (TREE_TYPE (orig_op0))
		     || (TREE_CODE (op1) == INTEGER_CST
			 && (TREE_INT_CST_LOW (op1) != -1
			     || TREE_INT_CST_HIGH (op1) != -1)));
	  common = 1;
	}
      break;

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
      if ((code0 == INTEGER_TYPE || code0 == POINTER_TYPE
	   || code0 == REAL_TYPE || code0 == COMPLEX_TYPE)
	  && (code1 == INTEGER_TYPE || code1 == POINTER_TYPE
	      || code1 == REAL_TYPE || code1 == COMPLEX_TYPE))
	{
	  /* Result of these operations is always an int,
	     but that does not mean the operands should be
	     converted to ints!  */
	  result_type = integer_type_node;
	  op0 = truthvalue_conversion (op0);
	  op1 = truthvalue_conversion (op1);
	  converted = 1;
	}
      break;

      /* Shift operations: result has same type as first operand;
	 always convert second operand to int.
	 Also set SHORT_SHIFT if shifting rightward.  */

    case RSHIFT_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	{
	  if (TREE_CODE (op1) == INTEGER_CST && skip_evaluation == 0)
	    {
	      if (tree_int_cst_sgn (op1) < 0)
		warning ("right shift count is negative");
	      else
		{
		  if (TREE_INT_CST_LOW (op1) | TREE_INT_CST_HIGH (op1))
		    short_shift = 1;
		  if (TREE_INT_CST_HIGH (op1) != 0
		      || ((unsigned HOST_WIDE_INT) TREE_INT_CST_LOW (op1)
			  >= TYPE_PRECISION (type0)))
		    warning ("right shift count >= width of type");
		}
	    }
	  /* Use the type of the value to be shifted.
	     This is what most traditional C compilers do.  */
	  result_type = type0;
	  /* Unless traditional, convert the shift-count to an integer,
	     regardless of size of value being shifted.  */
	  if (! flag_traditional)
	    {
	      if (TYPE_MAIN_VARIANT (TREE_TYPE (op1)) != integer_type_node)
		op1 = convert (integer_type_node, op1);
	      /* Avoid converting op1 to result_type later.  */
	      converted = 1;
	    }
	}
      break;

    case LSHIFT_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	{
	  if (TREE_CODE (op1) == INTEGER_CST && skip_evaluation == 0)
	    {
	      if (tree_int_cst_sgn (op1) < 0)
		warning ("left shift count is negative");
	      else if (TREE_INT_CST_HIGH (op1) != 0
		       || ((unsigned HOST_WIDE_INT) TREE_INT_CST_LOW (op1)
			   >= TYPE_PRECISION (type0)))
		warning ("left shift count >= width of type");
	    }
	  /* Use the type of the value to be shifted.
	     This is what most traditional C compilers do.  */
	  result_type = type0;
	  /* Unless traditional, convert the shift-count to an integer,
	     regardless of size of value being shifted.  */
	  if (! flag_traditional)
	    {
	      if (TYPE_MAIN_VARIANT (TREE_TYPE (op1)) != integer_type_node)
		op1 = convert (integer_type_node, op1);
	      /* Avoid converting op1 to result_type later.  */
	      converted = 1;
	    }
	}
      break;

    case RROTATE_EXPR:
    case LROTATE_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	{
	  if (TREE_CODE (op1) == INTEGER_CST && skip_evaluation == 0)
	    {
	      if (tree_int_cst_sgn (op1) < 0)
		warning ("shift count is negative");
	      else if (TREE_INT_CST_HIGH (op1) != 0
		       || ((unsigned HOST_WIDE_INT) TREE_INT_CST_LOW (op1)
			   >= TYPE_PRECISION (type0)))
		warning ("shift count >= width of type");
	    }
	  /* Use the type of the value to be shifted.
	     This is what most traditional C compilers do.  */
	  result_type = type0;
	  /* Unless traditional, convert the shift-count to an integer,
	     regardless of size of value being shifted.  */
	  if (! flag_traditional)
	    {
	      if (TYPE_MAIN_VARIANT (TREE_TYPE (op1)) != integer_type_node)
		op1 = convert (integer_type_node, op1);
	      /* Avoid converting op1 to result_type later.  */
	      converted = 1;
	    }
	}
      break;

    case EQ_EXPR:
    case NE_EXPR:
      /* Result of comparison is always int,
	 but don't convert the args to int!  */
      build_type = integer_type_node;
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE
	   || code0 == COMPLEX_TYPE)
	  && (code1 == INTEGER_TYPE || code1 == REAL_TYPE
	      || code1 == COMPLEX_TYPE))
	short_compare = 1;
      else if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
	{
	  register tree tt0 = TREE_TYPE (type0);
	  register tree tt1 = TREE_TYPE (type1);
	  /* Anything compares with void *.  void * compares with anything.
	     Otherwise, the targets must be compatible
	     and both must be object or both incomplete.  */
	  if (comp_target_types (type0, type1))
	    result_type = common_type (type0, type1);
	  else if (TYPE_MAIN_VARIANT (tt0) == void_type_node)
	    {
	      /* op0 != orig_op0 detects the case of something
		 whose value is 0 but which isn't a valid null ptr const.  */
	      if (pedantic && (!integer_zerop (op0) || op0 != orig_op0)
		  && TREE_CODE (tt1) == FUNCTION_TYPE)
		pedwarn ("ANSI C forbids comparison of `void *' with function pointer");
	    }
	  else if (TYPE_MAIN_VARIANT (tt1) == void_type_node)
	    {
	      if (pedantic && (!integer_zerop (op1) || op1 != orig_op1)
		  && TREE_CODE (tt0) == FUNCTION_TYPE)
		pedwarn ("ANSI C forbids comparison of `void *' with function pointer");
	    }
	  else
	    pedwarn ("comparison of distinct pointer types lacks a cast");

	  if (result_type == NULL_TREE)
	    result_type = ptr_type_node;
	}
      else if (code0 == POINTER_TYPE && TREE_CODE (op1) == INTEGER_CST
	       && integer_zerop (op1))
	result_type = type0;
      else if (code1 == POINTER_TYPE && TREE_CODE (op0) == INTEGER_CST
	       && integer_zerop (op0))
	result_type = type1;
      else if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
	{
	  result_type = type0;
	  if (! flag_traditional)
	    pedwarn ("comparison between pointer and integer");
	}
      else if (code0 == INTEGER_TYPE && code1 == POINTER_TYPE)
	{
	  result_type = type1;
	  if (! flag_traditional)
	    pedwarn ("comparison between pointer and integer");
	}
      break;

    case MAX_EXPR:
    case MIN_EXPR:
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE)
	  && (code1 == INTEGER_TYPE || code1 == REAL_TYPE))
	shorten = 1;
      else if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
	{
	  if (comp_target_types (type0, type1))
	    {
	      result_type = common_type (type0, type1);
	      if (pedantic 
		  && TREE_CODE (TREE_TYPE (type0)) == FUNCTION_TYPE)
		pedwarn ("ANSI C forbids ordered comparisons of pointers to functions");
	    }
	  else
	    {
	      result_type = ptr_type_node;
	      pedwarn ("comparison of distinct pointer types lacks a cast");
	    }
	}
      break;

    case LE_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
      build_type = integer_type_node;
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE)
	  && (code1 == INTEGER_TYPE || code1 == REAL_TYPE))
	short_compare = 1;
      else if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
	{
	  if (comp_target_types (type0, type1))
	    {
	      result_type = common_type (type0, type1);
	      if ((TYPE_SIZE (TREE_TYPE (type0)) != 0)
		  != (TYPE_SIZE (TREE_TYPE (type1)) != 0))
		pedwarn ("comparison of complete and incomplete pointers");
	      else if (pedantic 
		       && TREE_CODE (TREE_TYPE (type0)) == FUNCTION_TYPE)
		pedwarn ("ANSI C forbids ordered comparisons of pointers to functions");
	    }
	  else
	    {
	      result_type = ptr_type_node;
	      pedwarn ("comparison of distinct pointer types lacks a cast");
	    }
	}
      else if (code0 == POINTER_TYPE && TREE_CODE (op1) == INTEGER_CST
	       && integer_zerop (op1))
	{
	  result_type = type0;
	  if (pedantic || extra_warnings)
	    pedwarn ("ordered comparison of pointer with integer zero");
	}
      else if (code1 == POINTER_TYPE && TREE_CODE (op0) == INTEGER_CST
	       && integer_zerop (op0))
	{
	  result_type = type1;
	  if (pedantic)
	    pedwarn ("ordered comparison of pointer with integer zero");
	}
      else if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
	{
	  result_type = type0;
	  if (! flag_traditional)
	    pedwarn ("comparison between pointer and integer");
	}
      else if (code0 == INTEGER_TYPE && code1 == POINTER_TYPE)
	{
	  result_type = type1;
	  if (! flag_traditional)
	    pedwarn ("comparison between pointer and integer");
	}
      break;
      
    default:
      break;
    }

  if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE || code0 == COMPLEX_TYPE)
      &&
      (code1 == INTEGER_TYPE || code1 == REAL_TYPE || code1 == COMPLEX_TYPE))
    {
      int none_complex = (code0 != COMPLEX_TYPE && code1 != COMPLEX_TYPE);

      if (shorten || common || short_compare)
	result_type = common_type (type0, type1);

      /* For certain operations (which identify themselves by shorten != 0)
	 if both args were extended from the same smaller type,
	 do the arithmetic in that type and then extend.

	 shorten !=0 and !=1 indicates a bitwise operation.
	 For them, this optimization is safe only if
	 both args are zero-extended or both are sign-extended.
	 Otherwise, we might change the result.
	 Eg, (short)-1 | (unsigned short)-1 is (int)-1
	 but calculated in (unsigned short) it would be (unsigned short)-1.  */

      if (shorten && none_complex)
	{
	  int unsigned0, unsigned1;
	  tree arg0 = get_narrower (op0, &unsigned0);
	  tree arg1 = get_narrower (op1, &unsigned1);
	  /* UNS is 1 if the operation to be done is an unsigned one.  */
	  int uns = TREE_UNSIGNED (result_type);
	  tree type;

	  final_type = result_type;

	  /* Handle the case that OP0 (or OP1) does not *contain* a conversion
	     but it *requires* conversion to FINAL_TYPE.  */

	  if ((TYPE_PRECISION (TREE_TYPE (op0))
	       == TYPE_PRECISION (TREE_TYPE (arg0)))
	      && TREE_TYPE (op0) != final_type)
	    unsigned0 = TREE_UNSIGNED (TREE_TYPE (op0));
	  if ((TYPE_PRECISION (TREE_TYPE (op1))
	       == TYPE_PRECISION (TREE_TYPE (arg1)))
	      && TREE_TYPE (op1) != final_type)
	    unsigned1 = TREE_UNSIGNED (TREE_TYPE (op1));

	  /* Now UNSIGNED0 is 1 if ARG0 zero-extends to FINAL_TYPE.  */

	  /* For bitwise operations, signedness of nominal type
	     does not matter.  Consider only how operands were extended.  */
	  if (shorten == -1)
	    uns = unsigned0;

	  /* Note that in all three cases below we refrain from optimizing
	     an unsigned operation on sign-extended args.
	     That would not be valid.  */

	  /* Both args variable: if both extended in same way
	     from same width, do it in that width.
	     Do it unsigned if args were zero-extended.  */
	  if ((TYPE_PRECISION (TREE_TYPE (arg0))
	       < TYPE_PRECISION (result_type))
	      && (TYPE_PRECISION (TREE_TYPE (arg1))
		  == TYPE_PRECISION (TREE_TYPE (arg0)))
	      && unsigned0 == unsigned1
	      && (unsigned0 || !uns))
	    result_type
	      = signed_or_unsigned_type (unsigned0,
					 common_type (TREE_TYPE (arg0), TREE_TYPE (arg1)));
	  else if (TREE_CODE (arg0) == INTEGER_CST
		   && (unsigned1 || !uns)
		   && (TYPE_PRECISION (TREE_TYPE (arg1))
		       < TYPE_PRECISION (result_type))
		   && (type = signed_or_unsigned_type (unsigned1,
						       TREE_TYPE (arg1)),
		       int_fits_type_p (arg0, type)))
	    result_type = type;
	  else if (TREE_CODE (arg1) == INTEGER_CST
		   && (unsigned0 || !uns)
		   && (TYPE_PRECISION (TREE_TYPE (arg0))
		       < TYPE_PRECISION (result_type))
		   && (type = signed_or_unsigned_type (unsigned0,
						       TREE_TYPE (arg0)),
		       int_fits_type_p (arg1, type)))
	    result_type = type;
	}

      /* Shifts can be shortened if shifting right.  */

      if (short_shift)
	{
	  int unsigned_arg;
	  tree arg0 = get_narrower (op0, &unsigned_arg);

	  final_type = result_type;

	  if (arg0 == op0 && final_type == TREE_TYPE (op0))
	    unsigned_arg = TREE_UNSIGNED (TREE_TYPE (op0));

	  if (TYPE_PRECISION (TREE_TYPE (arg0)) < TYPE_PRECISION (result_type)
	      /* We can shorten only if the shift count is less than the
		 number of bits in the smaller type size.  */
	      && TREE_INT_CST_HIGH (op1) == 0
	      && TYPE_PRECISION (TREE_TYPE (arg0)) > TREE_INT_CST_LOW (op1)
	      /* If arg is sign-extended and then unsigned-shifted,
		 we can simulate this with a signed shift in arg's type
		 only if the extended result is at least twice as wide
		 as the arg.  Otherwise, the shift could use up all the
		 ones made by sign-extension and bring in zeros.
		 We can't optimize that case at all, but in most machines
		 it never happens because available widths are 2**N.  */
	      && (!TREE_UNSIGNED (final_type)
		  || unsigned_arg
		  || 2 * TYPE_PRECISION (TREE_TYPE (arg0)) <= TYPE_PRECISION (result_type)))
	    {
	      /* Do an unsigned shift if the operand was zero-extended.  */
	      result_type
		= signed_or_unsigned_type (unsigned_arg,
					   TREE_TYPE (arg0));
	      /* Convert value-to-be-shifted to that type.  */
	      if (TREE_TYPE (op0) != result_type)
		op0 = convert (result_type, op0);
	      converted = 1;
	    }
	}

      /* Comparison operations are shortened too but differently.
	 They identify themselves by setting short_compare = 1.  */

      if (short_compare)
	{
	  /* Don't write &op0, etc., because that would prevent op0
	     from being kept in a register.
	     Instead, make copies of the our local variables and
	     pass the copies by reference, then copy them back afterward.  */
	  tree xop0 = op0, xop1 = op1, xresult_type = result_type;
	  enum tree_code xresultcode = resultcode;
	  tree val 
	    = shorten_compare (&xop0, &xop1, &xresult_type, &xresultcode);
	  if (val != 0)
	    return val;
	  op0 = xop0, op1 = xop1;
	  converted = 1;
	  resultcode = xresultcode;

	  if ((warn_sign_compare < 0 ? extra_warnings : warn_sign_compare != 0)
	      && skip_evaluation == 0)
	    {
	      int op0_signed = ! TREE_UNSIGNED (TREE_TYPE (orig_op0));
	      int op1_signed = ! TREE_UNSIGNED (TREE_TYPE (orig_op1));

	      int unsignedp0, unsignedp1;
	      tree primop0 = get_narrower (op0, &unsignedp0);
	      tree primop1 = get_narrower (op1, &unsignedp1);

	      /* Avoid spurious warnings for comparison with enumerators.  */
 
	      xop0 = orig_op0;
	      xop1 = orig_op1;
	      STRIP_TYPE_NOPS (xop0);
	      STRIP_TYPE_NOPS (xop1);

	      /* Give warnings for comparisons between signed and unsigned
		 quantities that may fail.  */
	      /* Do the checking based on the original operand trees, so that
		 casts will be considered, but default promotions won't be.  */

	      /* Do not warn if the comparison is being done in a signed type,
		 since the signed type will only be chosen if it can represent
		 all the values of the unsigned type.  */
	      if (! TREE_UNSIGNED (result_type))
		/* OK */;
              /* Do not warn if both operands are unsigned.  */
              else if (op0_signed == op1_signed)
                /* OK */;
	      /* Do not warn if the signed quantity is an unsuffixed
		 integer literal (or some static constant expression
		 involving such literals) and it is non-negative.  */
	      else if ((op0_signed && TREE_CODE (xop0) == INTEGER_CST
			&& tree_int_cst_sgn (xop0) >= 0)
		       || (op1_signed && TREE_CODE (xop1) == INTEGER_CST
			   && tree_int_cst_sgn (xop1) >= 0))
		/* OK */;
	      /* Do not warn if the comparison is an equality operation,
                 the unsigned quantity is an integral constant and it does
                 not use the most significant bit of result_type.  */
	      else if ((resultcode == EQ_EXPR || resultcode == NE_EXPR)
		       && ((op0_signed && TREE_CODE (xop1) == INTEGER_CST
			    && int_fits_type_p (xop1, signed_type (result_type)))
			   || (op1_signed && TREE_CODE (xop0) == INTEGER_CST
			       && int_fits_type_p (xop0, signed_type (result_type)))))
		/* OK */;
	      else
		warning ("comparison between signed and unsigned");

	      /* Warn if two unsigned values are being compared in a size
		 larger than their original size, and one (and only one) is the
		 result of a `~' operator.  This comparison will always fail.

		 Also warn if one operand is a constant, and the constant
		 does not have all bits set that are set in the ~ operand
		 when it is extended.  */

	      if ((TREE_CODE (primop0) == BIT_NOT_EXPR)
		  != (TREE_CODE (primop1) == BIT_NOT_EXPR))
		{
		  if (TREE_CODE (primop0) == BIT_NOT_EXPR)
		    primop0 = get_narrower (TREE_OPERAND (primop0, 0),
					    &unsignedp0);
		  else
		    primop1 = get_narrower (TREE_OPERAND (primop1, 0),
					    &unsignedp1);
	      
		  if (TREE_CODE (primop0) == INTEGER_CST
		      || TREE_CODE (primop1) == INTEGER_CST)
		    {
		      tree primop;
		      long constant, mask;
		      int unsignedp, bits;

		      if (TREE_CODE (primop0) == INTEGER_CST)
			{
			  primop = primop1;
			  unsignedp = unsignedp1;
			  constant = TREE_INT_CST_LOW (primop0);
			}
		      else
			{
			  primop = primop0;
			  unsignedp = unsignedp0;
			  constant = TREE_INT_CST_LOW (primop1);
			}

		      bits = TYPE_PRECISION (TREE_TYPE (primop));
		      if (bits < TYPE_PRECISION (result_type)
			  && bits < HOST_BITS_PER_LONG && unsignedp)
			{
			  mask = (~0L) << bits;
			  if ((mask & constant) != mask)
			    warning ("comparison of promoted ~unsigned with constant");
			}
		    }
		  else if (unsignedp0 && unsignedp1
			   && (TYPE_PRECISION (TREE_TYPE (primop0))
			       < TYPE_PRECISION (result_type))
			   && (TYPE_PRECISION (TREE_TYPE (primop1))
			       < TYPE_PRECISION (result_type)))
		    warning ("comparison of promoted ~unsigned with unsigned");
		}
	    }
	}
    }

  /* At this point, RESULT_TYPE must be nonzero to avoid an error message.
     If CONVERTED is zero, both args will be converted to type RESULT_TYPE.
     Then the expression will be built.
     It will be given type FINAL_TYPE if that is nonzero;
     otherwise, it will be given type RESULT_TYPE.  */

  if (!result_type)
    {
#if 1
      fprintf(stderr, "\noperand 1\n"); fflush(stderr);
      debug_tree(orig_op0);
      fprintf(stderr, "\noperand 2\n"); fflush(stderr);
      debug_tree(orig_op1);
      fprintf(stderr, "end of tree for parameter\n"); fflush(stderr);
#endif

      binary_op_error (code);
      return error_mark_node;
    }

  if (! converted)
    {
      if (TREE_TYPE (op0) != result_type)
	op0 = convert (result_type, op0); 
      if (TREE_TYPE (op1) != result_type)
	op1 = convert (result_type, op1); 
    }

  if (build_type == NULL_TREE)
    build_type = result_type;

  {
    register tree result = build (resultcode, build_type, op0, op1);
    register tree folded;

    folded = fold (result);
    if (folded == result)
      TREE_CONSTANT (folded) = TREE_CONSTANT (op0) & TREE_CONSTANT (op1);
    if (final_type != 0)
      return convert (final_type, folded);
    return folded;
  }
}


/*
 *  routines which interface with m2f
 */

tree
gccgm2_DeclareKnownType (name, type)
     char *name;
     tree type;
{
    tree cp = build_type_copy (type);
    tree id = get_identifier  (name);
    tree decl;

    TYPE_NAME(cp) = id;
    TREE_TYPE(id) = cp;
    decl = build_decl (TYPE_DECL, id, cp);
    DECL_COMMON (decl) = 1;

    layout_type (type);
    layout_type (cp);

    /* The corresponding pop_obstacks is in finish_decl.  */
    push_obstacks_nochange ();
    pushdecl (decl);
    finish_decl (decl, NULL_TREE, NULL_TREE);

    return( cp );
}

/*
 *  GetMinFrom - given a, type, return a constant representing the minimum
 *               legal value.
 */

tree
gccgm2_GetMinFrom (type)
     tree type;
{
  return( TYPE_MIN_VALUE(type) );
}

/*
 *  GetMaxFrom - given a, type, return a constant representing the maximum
 *               legal value.
 */

tree
gccgm2_GetMaxFrom (type)
     tree type;
{
  return( TYPE_MAX_VALUE(type) );
}


int
gccgm2_GetBitsPerWord ()
{
  return( BITS_PER_WORD );
}

void
trythis ()
{
  /*
   *  testing how to create global variables
   */

  {  
    tree id;
    tree decl;

    if (current_binding_level != global_binding_level) {
      error("declaring a global variable outside of the global binding level");
    }

    id   = get_identifier ("another_global_int");
    decl = build_decl (VAR_DECL, id, integer_type_node);

    pushdecl(decl);

    DECL_SOURCE_LINE(decl)  = 1;

    DECL_EXTERNAL (decl)    = 0;
    TREE_STATIC   (decl)    = 1;           /* declaration and definition */

    TREE_PUBLIC   (decl)    = 0;
    DECL_CONTEXT  (decl)    = NULL_TREE;

    TREE_USED     (integer_type_node)    = 1;
    TREE_USED     (decl)    = 1;

#if 1
    /* now for the id */

    DECL_EXTERNAL (id)      = 0;
    TREE_STATIC   (id)      = 1;           /* declaration and definition */
    TREE_PUBLIC   (id)      = 1;
    TREE_USED     (id)      = 1;
#endif

    layout_type (integer_type_node);
    layout_decl (decl, 0);
    expand_decl(decl);
  }
  /*
   *  end of test code
   */
}


/*
 *  DeclareKnownVariable - declares a variable in scope, funcscope. Note that the global
 *                         variable, current_function_decl, is altered if isglobal is TRUE.
 */

tree
gccgm2_DeclareKnownVariable (name, type, exported, imported, istemporary, isglobal, scope)
     char *name;
     tree type;
     int  exported;
     int  imported;
     int  istemporary ATTRIBUTE_UNUSED;
     int  isglobal;
     tree scope;
{
  tree id;
  tree decl;

  id   = get_identifier (name);
  decl = build_decl (VAR_DECL, id, type);

  DECL_SOURCE_LINE(decl)  = lineno;

  DECL_EXTERNAL (decl)    = imported;
  if (isglobal) {
    TREE_PUBLIC   (decl)  = exported;
    DECL_CONTEXT  (decl)  = NULL_TREE;
    TREE_STATIC   (decl)  = isglobal;           /* declaration and definition */
  } else {
    TREE_PUBLIC   (decl)  = 1;
    DECL_CONTEXT  (decl)  = scope;              /* scope is actually the current function */
  }
  TREE_USED     (type)    = 1;
  TREE_USED     (decl)    = 1;

  /* now for the id */

  DECL_EXTERNAL (id)      = imported;
  if (imported) {
    TREE_STATIC   (id)    = 0;           /* declaration and definition */
  } else {
    TREE_STATIC   (id)    = 1;           /* declaration and definition */
  }
  if (isglobal) {
    TREE_PUBLIC   (id)    = exported;
  }
  TREE_USED     (id)      = 1;

  pushdecl(decl);

  if (DECL_SIZE(decl) == 0) {
    error_with_decl (decl, "storage size of `%s' hasn't been resolved");
  }
  expand_decl(decl);
  if (isglobal) {
    layout_decl (decl, 0);
    rest_of_decl_compilation (decl, 0, 1, 0);
  }

  return( decl );
}


/*
 *  DeclareKnownConstant - given a constant, value, of, type, create a constant in the GCC
 *                         symbol table. Note that the name of the constant is not used
 *                         as _all_ constants are declared in the global scope. The front end
 *                         deals with scoping rules - here we declare all constants with no names
 *                         in the global scope. This allows M2SubExp and constant folding routines
 *                         the liberty of operating with quadruples which all assume constants can
 *                         always be referenced.
 */

tree
gccgm2_DeclareKnownConstant (type, value)
     tree type, value;
{
    tree id = make_node (IDENTIFIER_NODE);  /* ignore the name of the constant */
    tree decl;

    layout_type (type);

    decl = build_decl (CONST_DECL, id, type);

    DECL_INITIAL (decl) = value;
    TREE_TYPE (decl)    = type;
    pushdecl (decl);

    return( decl );
}


#if 0
/* --fixme--  (gaius) we may need this inside declareknownvariable */

  /* For a local variable, define the RTL now.  */
  if (current_binding_level != global_binding_level
      /* But not if this is a duplicate decl
	 and we preserved the rtl from the previous one
	 (which may or may not happen).  */
      && DECL_RTL (tem) == 0)
    {
      if (TYPE_SIZE (TREE_TYPE (tem)) != 0)
	expand_decl (tem);
      else if (TREE_CODE (TREE_TYPE (tem)) == ARRAY_TYPE
	       && DECL_INITIAL (tem) != 0)
	expand_decl (tem);
    }
#endif


#if 0
/* Decode a declarator in an ordinary declaration or data definition.
   This is called as soon as the type information and variable name
   have been parsed, before parsing the initializer if any.
   Here we create the ..._DECL node, fill in its type,
   and put it on the list of decls for the current context.
   The ..._DECL node is returned as the value.

   Exception: for arrays where the length is not specified,
   the type is left null, to be filled in by `finish_decl'.

   Function definitions do not come here; they go to start_function
   instead.  However, external and forward declarations of functions
   do go through here.  Structure field declarations are done by
   grokfield and not through here.  */

/* Set this to zero to debug not using the temporary obstack
   to parse initializers.  */
int debug_temp_inits = 1;

tree
start_decl (declarator, declspecs, initialized, attributes, prefix_attributes)
     tree declarator, declspecs;
     int initialized;
     tree attributes, prefix_attributes;
{
  register tree decl = grokdeclarator (declarator, declspecs,
				       NORMAL, initialized);
  register tree tem;
  int init_written = initialized;

  /* The corresponding pop_obstacks is in finish_decl.  */
  push_obstacks_nochange ();

  if (warn_main && !strcmp (IDENTIFIER_POINTER (declarator), "main"))
    warning_with_decl (decl, "`%s' is usually a function");

  if (initialized)
    /* Is it valid for this decl to have an initializer at all?
       If not, set INITIALIZED to zero, which will indirectly
       tell `finish_decl' to ignore the initializer once it is parsed.  */
    switch (TREE_CODE (decl))
      {
      case TYPE_DECL:
	/* typedef foo = bar  means give foo the same type as bar.
	   We haven't parsed bar yet, so `finish_decl' will fix that up.
	   Any other case of an initialization in a TYPE_DECL is an error.  */
	if (pedantic || list_length (declspecs) > 1)
	  {
	    error ("typedef `%s' is initialized",
		   IDENTIFIER_POINTER (DECL_NAME (decl)));
	    initialized = 0;
	  }
	break;

      case FUNCTION_DECL:
	error ("function `%s' is initialized like a variable",
	       IDENTIFIER_POINTER (DECL_NAME (decl)));
	initialized = 0;
	break;

      case PARM_DECL:
	/* DECL_INITIAL in a PARM_DECL is really DECL_ARG_TYPE.  */
	error ("parameter `%s' is initialized",
	       IDENTIFIER_POINTER (DECL_NAME (decl)));
	initialized = 0;
	break;

      default:
	/* Don't allow initializations for incomplete types
	   except for arrays which might be completed by the initialization.  */
	if (TYPE_SIZE (TREE_TYPE (decl)) != 0)
	  {
	    /* A complete type is ok if size is fixed.  */

	    if (TREE_CODE (TYPE_SIZE (TREE_TYPE (decl))) != INTEGER_CST
		|| C_DECL_VARIABLE_SIZE (decl))
	      {
		error ("variable-sized object may not be initialized");
		initialized = 0;
	      }
	  }
	else if (TREE_CODE (TREE_TYPE (decl)) != ARRAY_TYPE)
	  {
	    error ("variable `%s' has initializer but incomplete type",
		   IDENTIFIER_POINTER (DECL_NAME (decl)));
	    initialized = 0;
	  }
	else if (TYPE_SIZE (TREE_TYPE (TREE_TYPE (decl))) == 0)
	  {
	    error ("elements of array `%s' have incomplete type",
		   IDENTIFIER_POINTER (DECL_NAME (decl)));
	    initialized = 0;
	  }
      }

  if (initialized)
    {
#if 0  /* Seems redundant with grokdeclarator.  */
      if (current_binding_level != global_binding_level
	  && DECL_EXTERNAL (decl)
	  && TREE_CODE (decl) != FUNCTION_DECL)
	warning ("declaration of `%s' has `extern' and is initialized",
		 IDENTIFIER_POINTER (DECL_NAME (decl)));
#endif
      DECL_EXTERNAL (decl) = 0;
      if (current_binding_level == global_binding_level)
	TREE_STATIC (decl) = 1;

      /* Tell `pushdecl' this is an initialized decl
	 even though we don't yet have the initializer expression.
	 Also tell `finish_decl' it may store the real initializer.  */
      DECL_INITIAL (decl) = error_mark_node;
    }

  /* If this is a function declaration, write a record describing it to the
     prototypes file (if requested).  */

  if (TREE_CODE (decl) == FUNCTION_DECL)
    gen_aux_info_record (decl, 0, 0, TYPE_ARG_TYPES (TREE_TYPE (decl)) != 0);

  /* ANSI specifies that a tentative definition which is not merged with
     a non-tentative definition behaves exactly like a definition with an
     initializer equal to zero.  (Section 3.7.2)
     -fno-common gives strict ANSI behavior.  Usually you don't want it.
     This matters only for variables with external linkage.  */
  if (! flag_no_common || ! TREE_PUBLIC (decl))
    DECL_COMMON (decl) = 1;

  /* Set attributes here so if duplicate decl, will have proper attributes.  */
  decl_attributes (decl, attributes, prefix_attributes);

  /* Add this decl to the current binding level.
     TEM may equal DECL or it may be a previous decl of the same name.  */
  tem = pushdecl (decl);

  /* For a local variable, define the RTL now.  */
  if (current_binding_level != global_binding_level
      /* But not if this is a duplicate decl
	 and we preserved the rtl from the previous one
	 (which may or may not happen).  */
      && DECL_RTL (tem) == 0)
    {
      if (TYPE_SIZE (TREE_TYPE (tem)) != 0)
	expand_decl (tem);
      else if (TREE_CODE (TREE_TYPE (tem)) == ARRAY_TYPE
	       && DECL_INITIAL (tem) != 0)
	expand_decl (tem);
    }

  if (init_written)
    {
      /* When parsing and digesting the initializer,
	 use temporary storage.  Do this even if we will ignore the value.  */
      if (current_binding_level == global_binding_level && debug_temp_inits)
	temporary_allocation ();
    }

  return tem;
}
#endif

/* Make TYPE a complete type based on INITIAL_VALUE.
   Return 0 if successful, 1 if INITIAL_VALUE can't be deciphered,
   2 if there was no information (in which case assume 1 if DO_DEFAULT).  */

int
complete_array_type (type, initial_value, do_default)
     tree type;
     tree initial_value;
     int do_default;
{
  register tree maxindex = NULL_TREE;
  int value = 0;

  if (initial_value)
    {
      /* Note MAXINDEX  is really the maximum index,
	 one less than the size.  */
      if (TREE_CODE (initial_value) == STRING_CST)
	{
	  int eltsize
	    = int_size_in_bytes (TREE_TYPE (TREE_TYPE (initial_value)));
	  maxindex = build_int_2 ((TREE_STRING_LENGTH (initial_value)
				   / eltsize) - 1, 0);
	}
      else if (TREE_CODE (initial_value) == CONSTRUCTOR)
	{
	  tree elts = CONSTRUCTOR_ELTS (initial_value);
	  maxindex = size_binop (MINUS_EXPR, integer_zero_node, size_one_node);
	  for (; elts; elts = TREE_CHAIN (elts))
	    {
	      if (TREE_PURPOSE (elts))
		maxindex = TREE_PURPOSE (elts);
	      else
		maxindex = size_binop (PLUS_EXPR, maxindex, size_one_node);
	    }
	  maxindex = copy_node (maxindex);
	}
      else
	{
	  /* Make an error message unless that happened already.  */
	  if (initial_value != error_mark_node)
	    value = 1;

	  /* Prevent further error messages.  */
	  maxindex = build_int_2 (0, 0);
	}
    }

  if (!maxindex)
    {
      if (do_default)
	maxindex = build_int_2 (0, 0);
      value = 2;
    }

  if (maxindex)
    {
      TYPE_DOMAIN (type) = build_index_type (maxindex);
      if (!TREE_TYPE (maxindex))
	TREE_TYPE (maxindex) = TYPE_DOMAIN (type);
    }

  /* Lay out the type now that we can get the real answer.  */

  layout_type (type);

  return value;
}

/* Finish processing of a declaration;
   install its initial value.
   If the length of an array type is not known before,
   it must be determined now, from the initial value, or it is an error.  */

void
finish_decl (decl, init, asmspec_tree)
     tree decl, init;
     tree asmspec_tree;
{
  register tree type = TREE_TYPE (decl);
  int was_incomplete = (DECL_SIZE (decl) == 0);
  int temporary = allocation_temporary_p ();
  char *asmspec = 0;

  /* If a name was specified, get the string.   */
  if (asmspec_tree)
    asmspec = TREE_STRING_POINTER (asmspec_tree);

  /* If `start_decl' didn't like having an initialization, ignore it now.  */

  if (init != 0 && DECL_INITIAL (decl) == 0)
    init = 0;
  /* Don't crash if parm is initialized.  */
  if (TREE_CODE (decl) == PARM_DECL)
    init = 0;

  if (ITERATOR_P (decl))
    {
      if (init == 0)
	error_with_decl (decl, "iterator has no initial value");
      else
	init = save_expr (init);
    }

#if NOT_NEEDED_BY_M2
  if (init)
    {
      if (TREE_CODE (decl) != TYPE_DECL)
	store_init_value (decl, init);
      else
	{
	  /* typedef foo = bar; store the type of bar as the type of foo.  */
	  TREE_TYPE (decl) = TREE_TYPE (init);
	  DECL_INITIAL (decl) = init = 0;
	}
    }
#endif

  /* Pop back to the obstack that is current for this binding level.
     This is because MAXINDEX, rtl, etc. to be made below
     must go in the permanent obstack.  But don't discard the
     temporary data yet.  */
  pop_obstacks ();
#if 0 /* pop_obstacks was near the end; this is what was here.  */
  if (current_binding_level == global_binding_level && temporary)
    end_temporary_allocation ();
#endif

  /* Deduce size of array from initialization, if not already known */

  if (TREE_CODE (type) == ARRAY_TYPE
      && TYPE_DOMAIN (type) == 0
      && TREE_CODE (decl) != TYPE_DECL)
    {
      int do_default
	= (TREE_STATIC (decl)
	   /* Even if pedantic, an external linkage array
	      may have incomplete type at first.  */
	   ? pedantic && !TREE_PUBLIC (decl)
	   : !DECL_EXTERNAL (decl));
      int failure
	= complete_array_type (type, DECL_INITIAL (decl), do_default);

      /* Get the completed type made by complete_array_type.  */
      type = TREE_TYPE (decl);

      if (failure == 1)
	error_with_decl (decl, "initializer fails to determine size of `%s'");

      if (failure == 2)
	{
	  if (do_default)
	    error_with_decl (decl, "array size missing in `%s'");
	  /* If a `static' var's size isn't known,
	     make it extern as well as static, so it does not get
	     allocated.
	     If it is not `static', then do not mark extern;
	     finish_incomplete_decl will give it a default size
	     and it will get allocated.  */
	  else if (!pedantic && TREE_STATIC (decl) && ! TREE_PUBLIC (decl))
	    DECL_EXTERNAL (decl) = 1;
	}

      /* TYPE_MAX_VALUE is always one less than the number of elements
	 in the array, because we start counting at zero.  Therefore,
	 warn only if the value is less than zero.  */
      if (pedantic && TYPE_DOMAIN (type) != 0
	  && tree_int_cst_sgn (TYPE_MAX_VALUE (TYPE_DOMAIN (type))) < 0)
	error_with_decl (decl, "zero or negative size array `%s'");

      layout_decl (decl, 0);
    }

  if (TREE_CODE (decl) == VAR_DECL)
    {
      if (DECL_SIZE (decl) == 0
	  && TYPE_SIZE (TREE_TYPE (decl)) != 0)
	layout_decl (decl, 0);

      if (DECL_SIZE (decl) == 0
	  && (TREE_STATIC (decl)
	      ?
		/* A static variable with an incomplete type
		   is an error if it is initialized.
		   Also if it is not file scope.
		   Otherwise, let it through, but if it is not `extern'
		   then it may cause an error message later.  */
	      /* A duplicate_decls call could have changed an extern
		 declaration into a file scope one.  This can be detected
		 by TREE_ASM_WRITTEN being set.  */
		(DECL_INITIAL (decl) != 0
		 || (DECL_CONTEXT (decl) != 0 && ! TREE_ASM_WRITTEN (decl)))
	      :
		/* An automatic variable with an incomplete type
		   is an error.  */
		!DECL_EXTERNAL (decl)))
	{
	  error_with_decl (decl, "storage size of `%s' isn't known");
	  TREE_TYPE (decl) = error_mark_node;
	}

      if ((DECL_EXTERNAL (decl) || TREE_STATIC (decl))
	  && DECL_SIZE (decl) != 0)
	{
	  if (TREE_CODE (DECL_SIZE (decl)) == INTEGER_CST)
	    constant_expression_warning (DECL_SIZE (decl));
	  else
	    error_with_decl (decl, "storage size of `%s' isn't constant");
	}

      if (TREE_USED  (type))
	TREE_USED (decl) = 1;
    }

  /* If this is a function and an assembler name is specified, it isn't
     builtin any more.  Also reset DECL_RTL so we can give it its new
     name.  */
  if (TREE_CODE (decl) == FUNCTION_DECL && asmspec)
      {
	DECL_BUILT_IN_CLASS (decl) = NOT_BUILT_IN;
	DECL_RTL (decl) = 0;
	DECL_ASSEMBLER_NAME (decl) = get_identifier (asmspec);
      }

  /* Output the assembler code and/or RTL code for variables and functions,
     unless the type is an undefined structure or union.
     If not, it will get done when the type is completed.  */

  if (TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == FUNCTION_DECL)
    {
      if ((flag_traditional || TREE_PERMANENT (decl))
	  && allocation_temporary_p ())
	{
	  push_obstacks_nochange ();
	  end_temporary_allocation ();
#if NOT_NEEDED_IN_M2
	  /* This is a no-op in c-lang.c or something real in objc-actions.c.  */
	  maybe_objc_check_decl (decl);
#endif
	  rest_of_decl_compilation (decl, asmspec,
				    (DECL_CONTEXT (decl) == 0
				     || TREE_ASM_WRITTEN (decl)),
				    0);
	  pop_obstacks ();
	}
      else
	{
	  /* This is a no-op in c-lang.c or something real in objc-actions.c.  */
#if NOT_NEEDED_IN_M2
	  maybe_objc_check_decl (decl);
#endif
	  rest_of_decl_compilation (decl, asmspec,
				    (DECL_CONTEXT (decl) == 0
				     || TREE_ASM_WRITTEN (decl)),
				    0);
	}
      if (DECL_CONTEXT (decl) != 0)
	{
	  /* Recompute the RTL of a local array now
	     if it used to be an incomplete type.  */
	  if (was_incomplete
	      && ! TREE_STATIC (decl) && ! DECL_EXTERNAL (decl))
	    {
	      /* If we used it already as memory, it must stay in memory.  */
	      TREE_ADDRESSABLE (decl) = TREE_USED (decl);
	      /* If it's still incomplete now, no init will save it.  */
	      if (DECL_SIZE (decl) == 0)
		DECL_INITIAL (decl) = 0;
	      expand_decl (decl);
	    }
	  /* Compute and store the initial value.  */
	  if (TREE_CODE (decl) != FUNCTION_DECL)
	    expand_decl_init (decl);
	}
    }

  if (TREE_CODE (decl) == TYPE_DECL)
    {
      /* This is a no-op in c-lang.c or something real in objc-actions.c.  */
#if NOT_NEEDED_IN_M2
      maybe_objc_check_decl (decl);
#endif
      rest_of_decl_compilation (decl, NULL_PTR, DECL_CONTEXT (decl) == 0,
				0);
    }

  /* ??? After 2.3, test (init != 0) instead of TREE_CODE.  */
  /* This test used to include TREE_PERMANENT, however, we have the same
     problem with initializers at the function level.  Such initializers get
     saved until the end of the function on the momentary_obstack.  */
  if (!(TREE_CODE (decl) == FUNCTION_DECL && DECL_INLINE (decl))
      && temporary
      /* DECL_INITIAL is not defined in PARM_DECLs, since it shares
	 space with DECL_ARG_TYPE.  */
      && TREE_CODE (decl) != PARM_DECL)
    {
      /* We need to remember that this array HAD an initialization,
	 but discard the actual temporary nodes,
	 since we can't have a permanent node keep pointing to them.  */
      /* We make an exception for inline functions, since it's
	 normal for a local extern redeclaration of an inline function
	 to have a copy of the top-level decl's DECL_INLINE.  */
      if (DECL_INITIAL (decl) != 0 && DECL_INITIAL (decl) != error_mark_node)
	{
	  /* If this is a const variable, then preserve the
	     initializer instead of discarding it so that we can optimize
	     references to it.  */
	  /* This test used to include TREE_STATIC, but this won't be set
	     for function level initializers.  */
	  if (TREE_READONLY (decl) || ITERATOR_P (decl))
	    {
	      preserve_initializer ();
	      /* Hack?  Set the permanent bit for something that is permanent,
		 but not on the permanent obstack, so as to convince
		 output_constant_def to make its rtl on the permanent
		 obstack.  */
	      TREE_PERMANENT (DECL_INITIAL (decl)) = 1;

	      /* The initializer and DECL must have the same (or equivalent
		 types), but if the initializer is a STRING_CST, its type
		 might not be on the right obstack, so copy the type
		 of DECL.  */
	      TREE_TYPE (DECL_INITIAL (decl)) = type;
	    }
	  else
	    DECL_INITIAL (decl) = error_mark_node;
	}
    }

  /* If requested, warn about definitions of large data objects.  */

  if (warn_larger_than
      && (TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == PARM_DECL)
      && !DECL_EXTERNAL (decl))
    {
      register tree decl_size = DECL_SIZE (decl);

      if (decl_size && TREE_CODE (decl_size) == INTEGER_CST)
	{
	   unsigned units = TREE_INT_CST_LOW(decl_size) / BITS_PER_UNIT;

	  if (units > larger_than_size)
	    warning_with_decl (decl, "size of `%s' is %u bytes", units);
	}
    }

#if 0
  /* Resume permanent allocation, if not within a function.  */
  /* The corresponding push_obstacks_nochange is in start_decl,
     and in push_parm_decl and in grokfield.  */
  pop_obstacks ();
#endif

  /* If we have gone back from temporary to permanent allocation,
     actually free the temporary space that we no longer need.  */
  if (temporary && !allocation_temporary_p ())
    permanent_allocation (0);

  /* At the end of a declaration, throw away any variable type sizes
     of types defined inside that declaration.  There is no use
     computing them in the following function definition.  */
  if (current_binding_level == global_binding_level)
    get_pending_sizes ();
}

/* To speed up processing of attributes, we maintain an array of
   IDENTIFIER_NODES and the corresponding attribute types.  */

/* Array to hold attribute information.  */

static struct {enum attrs id; tree name; int min, max, decl_req;} attrtab[50];

static int attrtab_idx = 0;

/* Add an entry to the attribute table above.  */

static void
add_attribute (id, string, min_len, max_len, decl_req)
     enum attrs id;
     char *string;
     int min_len, max_len;
     int decl_req;
{
  char buf[100];

  attrtab[attrtab_idx].id = id;
  attrtab[attrtab_idx].name = get_identifier (string);
  attrtab[attrtab_idx].min = min_len;
  attrtab[attrtab_idx].max = max_len;
  attrtab[attrtab_idx++].decl_req = decl_req;

  sprintf (buf, "__%s__", string);

  attrtab[attrtab_idx].id = id;
  attrtab[attrtab_idx].name = get_identifier (buf);
  attrtab[attrtab_idx].min = min_len;
  attrtab[attrtab_idx].max = max_len;
  attrtab[attrtab_idx++].decl_req = decl_req;
}

/* Initialize attribute table.  */

static void
init_attributes ()
{
  add_attribute (A_PACKED, "packed", 0, 0, 0);
  add_attribute (A_NOCOMMON, "nocommon", 0, 0, 1);
  add_attribute (A_COMMON, "common", 0, 0, 1);
  add_attribute (A_NORETURN, "noreturn", 0, 0, 1);
  add_attribute (A_NORETURN, "volatile", 0, 0, 1);
  add_attribute (A_UNUSED, "unused", 0, 0, 0);
  add_attribute (A_CONST, "const", 0, 0, 1);
  add_attribute (A_T_UNION, "transparent_union", 0, 0, 0);
  add_attribute (A_CONSTRUCTOR, "constructor", 0, 0, 1);
  add_attribute (A_DESTRUCTOR, "destructor", 0, 0, 1);
  add_attribute (A_MODE, "mode", 1, 1, 1);
  add_attribute (A_SECTION, "section", 1, 1, 1);
  add_attribute (A_ALIGNED, "aligned", 0, 1, 0);
  add_attribute (A_FORMAT, "format", 3, 3, 1);
  add_attribute (A_FORMAT_ARG, "format_arg", 1, 1, 1);
  add_attribute (A_WEAK, "weak", 0, 0, 1);
  add_attribute (A_ALIAS, "alias", 1, 1, 1);
}

/* Default implementation of valid_lang_attribute, below.  By default, there
   are no language-specific attributes.  */

static int
default_valid_lang_attribute (attr_name, attr_args, decl, type)
  tree attr_name ATTRIBUTE_UNUSED;
  tree attr_args ATTRIBUTE_UNUSED;
  tree decl ATTRIBUTE_UNUSED;
  tree type ATTRIBUTE_UNUSED;
{
  return 0;
}

/* Return a 1 if ATTR_NAME and ATTR_ARGS denote a valid language-specific
   attribute for either declaration DECL or type TYPE and 0 otherwise.  */

int (*valid_lang_attribute) PROTO ((tree, tree, tree, tree))
     = default_valid_lang_attribute;

/* Process the attributes listed in ATTRIBUTES and PREFIX_ATTRIBUTES
   and install them in NODE, which is either a DECL (including a TYPE_DECL)
   or a TYPE.  PREFIX_ATTRIBUTES can appear after the declaration specifiers
   and declaration modifiers but before the declaration proper.  */

void
decl_attributes (node, attributes, prefix_attributes)
     tree node, attributes, prefix_attributes;
{
  tree decl = 0, type = 0;
  int is_type = 0;
  tree a;

  if (attrtab_idx == 0)
    init_attributes ();

  if (TREE_CODE_CLASS (TREE_CODE (node)) == 'd')
    {
      decl = node;
      type = TREE_TYPE (decl);
      is_type = TREE_CODE (node) == TYPE_DECL;
    }
  else if (TREE_CODE_CLASS (TREE_CODE (node)) == 't')
    type = node, is_type = 1;

  attributes = chainon (prefix_attributes, attributes);

  for (a = attributes; a; a = TREE_CHAIN (a))
    {
      tree name = TREE_PURPOSE (a);
      tree args = TREE_VALUE (a);
      int i;
      enum attrs id;
      
      for (i = 0; i < attrtab_idx; i++)
	if (attrtab[i].name == name)
	  break;

      if (i == attrtab_idx)
	{
	  if (! valid_machine_attribute (name, args, decl, type)
	      && ! (* valid_lang_attribute) (name, args, decl, type))
	    warning ("`%s' attribute directive ignored",
		     IDENTIFIER_POINTER (name));
	  else if (decl != 0)
	    type = TREE_TYPE (decl);
	  continue;
	}
      else if (attrtab[i].decl_req && decl == 0)
	{
	  warning ("`%s' attribute does not apply to types",
		   IDENTIFIER_POINTER (name));
	  continue;
	}
      else if (list_length (args) < attrtab[i].min
	       || list_length (args) > attrtab[i].max)
	{
	  error ("wrong number of arguments specified for `%s' attribute",
		 IDENTIFIER_POINTER (name));
	  continue;
	}

      id = attrtab[i].id;
      switch (id)
	{
	case A_PACKED:
	  if (is_type)
	    TYPE_PACKED (type) = 1;
	  else if (TREE_CODE (decl) == FIELD_DECL)
	    DECL_PACKED (decl) = 1;
	  /* We can't set DECL_PACKED for a VAR_DECL, because the bit is
	     used for DECL_REGISTER.  It wouldn't mean anything anyway.  */
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;

	case A_NOCOMMON:
	  if (TREE_CODE (decl) == VAR_DECL)
	    DECL_COMMON (decl) = 0;
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;

	case A_COMMON:
	  if (TREE_CODE (decl) == VAR_DECL)
	    DECL_COMMON (decl) = 1;
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;

	case A_NORETURN:
	  if (TREE_CODE (decl) == FUNCTION_DECL)
	    TREE_THIS_VOLATILE (decl) = 1;
	  else if (TREE_CODE (type) == POINTER_TYPE
		   && TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
	    TREE_TYPE (decl) = type
	      = build_pointer_type
		(build_type_variant (TREE_TYPE (type),
				     TREE_READONLY (TREE_TYPE (type)), 1));
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;

#if NOT_NEEDED_IN_M2
	case A_MALLOC:
	  if (TREE_CODE (decl) == FUNCTION_DECL)
	    DECL_IS_MALLOC (decl) = 1;
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;
#endif
	case A_UNUSED:
	  if (is_type)
	    TREE_USED (type) = 1;
	  else if (TREE_CODE (decl) == PARM_DECL
		   || TREE_CODE (decl) == VAR_DECL
		   || TREE_CODE (decl) == FUNCTION_DECL
		   || TREE_CODE (decl) == LABEL_DECL)
	    TREE_USED (decl) = 1;
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;

	case A_CONST:
	  if (TREE_CODE (decl) == FUNCTION_DECL)
	    TREE_READONLY (decl) = 1;
	  else if (TREE_CODE (type) == POINTER_TYPE
		   && TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
	    TREE_TYPE (decl) = type
	      = build_pointer_type
		(build_type_variant (TREE_TYPE (type), 1,
				     TREE_THIS_VOLATILE (TREE_TYPE (type))));
	  else
	    warning ( "`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;

	case A_T_UNION:
	  if (is_type
	      && TREE_CODE (type) == UNION_TYPE
	      && (decl == 0
		  || (TYPE_FIELDS (type) != 0
		      && TYPE_MODE (type) == DECL_MODE (TYPE_FIELDS (type)))))
	    TYPE_TRANSPARENT_UNION (type) = 1;
	  else if (decl != 0 && TREE_CODE (decl) == PARM_DECL
		   && TREE_CODE (type) == UNION_TYPE
		   && TYPE_MODE (type) == DECL_MODE (TYPE_FIELDS (type)))
	    DECL_TRANSPARENT_UNION (decl) = 1;
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;

	case A_CONSTRUCTOR:
	  if (TREE_CODE (decl) == FUNCTION_DECL
	      && TREE_CODE (type) == FUNCTION_TYPE
	      && decl_function_context (decl) == 0)
	    {
	      DECL_STATIC_CONSTRUCTOR (decl) = 1;
	      TREE_USED (decl) = 1;
	    }
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;

	case A_DESTRUCTOR:
	  if (TREE_CODE (decl) == FUNCTION_DECL
	      && TREE_CODE (type) == FUNCTION_TYPE
	      && decl_function_context (decl) == 0)
	    {
	      DECL_STATIC_DESTRUCTOR (decl) = 1;
	      TREE_USED (decl) = 1;
	    }
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;

	case A_MODE:
	  if (TREE_CODE (TREE_VALUE (args)) != IDENTIFIER_NODE)
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  else
	    {
	      int j;
	      const char *p = IDENTIFIER_POINTER (TREE_VALUE (args));
	      int len = strlen (p);
	      enum machine_mode mode = VOIDmode;
	      tree typefm;

	      if (len > 4 && p[0] == '_' && p[1] == '_'
		  && p[len - 1] == '_' && p[len - 2] == '_')
		{
		  char *newp = (char *) alloca (len - 1);

		  strcpy (newp, &p[2]);
		  newp[len - 4] = '\0';
		  p = newp;
		}

	      /* Give this decl a type with the specified mode.
		 First check for the special modes.  */
	      if (! strcmp (p, "byte"))
		mode = byte_mode;
	      else if (!strcmp (p, "word"))
		mode = word_mode;
	      else if (! strcmp (p, "pointer"))
		mode = ptr_mode;
	      else
		for (j = 0; j < NUM_MACHINE_MODES; j++)
		  if (!strcmp (p, GET_MODE_NAME (j)))
		    mode = (enum machine_mode) j;

	      if (mode == VOIDmode)
		error ("unknown machine mode `%s'", p);
	      else if (0 == (typefm = type_for_mode (mode,
						     TREE_UNSIGNED (type))))
		error ("no data type for mode `%s'", p);
	      else
		{
		  TREE_TYPE (decl) = type = typefm;
		  DECL_SIZE (decl) = 0;
		  layout_decl (decl, 0);
		}
	    }
	  break;

	case A_SECTION:
#ifdef ASM_OUTPUT_SECTION_NAME
	  if ((TREE_CODE (decl) == FUNCTION_DECL
	       || TREE_CODE (decl) == VAR_DECL)
	      && TREE_CODE (TREE_VALUE (args)) == STRING_CST)
	    {
	      if (TREE_CODE (decl) == VAR_DECL
		  && current_function_decl != NULL_TREE
		  && ! TREE_STATIC (decl))
		error_with_decl (decl,
		  "section attribute cannot be specified for local variables");
	      /* The decl may have already been given a section attribute from
		 a previous declaration.  Ensure they match.  */
	      else if (DECL_SECTION_NAME (decl) != NULL_TREE
		       && strcmp (TREE_STRING_POINTER (DECL_SECTION_NAME (decl)),
				  TREE_STRING_POINTER (TREE_VALUE (args))) != 0)
		error_with_decl (node,
				 "section of `%s' conflicts with previous declaration");
	      else
		DECL_SECTION_NAME (decl) = TREE_VALUE (args);
	    }
	  else
	    error_with_decl (node,
			   "section attribute not allowed for `%s'");
#else
	  error_with_decl (node,
		  "section attributes are not supported for this target");
#endif
	  break;

	case A_ALIGNED:
	  {
	    tree align_expr
	      = (args ? TREE_VALUE (args)
		 : size_int (BIGGEST_ALIGNMENT / BITS_PER_UNIT));
	    int align;

	    /* Strip any NOPs of any kind.  */
	    while (TREE_CODE (align_expr) == NOP_EXPR
		   || TREE_CODE (align_expr) == CONVERT_EXPR
		   || TREE_CODE (align_expr) == NON_LVALUE_EXPR)
	      align_expr = TREE_OPERAND (align_expr, 0);
	  
	    if (TREE_CODE (align_expr) != INTEGER_CST)
	      {
		error ("requested alignment is not a constant");
		continue;
	      }

	    align = TREE_INT_CST_LOW (align_expr) * BITS_PER_UNIT;

	    if (exact_log2 (align) == -1)
	      error ("requested alignment is not a power of 2");
	    else if (is_type)
	      TYPE_ALIGN (type) = align;
	    else if (TREE_CODE (decl) != VAR_DECL
		     && TREE_CODE (decl) != FIELD_DECL)
	      error_with_decl (decl,
			       "alignment may not be specified for `%s'");
	    else
	      DECL_ALIGN (decl) = align;
	  }
	  break;

	case A_FORMAT:
	  {
#if NOT_NEEDED_FOR_M2
	    tree format_type = TREE_VALUE (args);
	    tree format_num_expr = TREE_VALUE (TREE_CHAIN (args));
	    tree first_arg_num_expr
	      = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (args)));
	    int format_num;
	    int first_arg_num;
	    int is_scan;
	    tree argument;
	    int arg_num;
	
	    if (TREE_CODE (decl) != FUNCTION_DECL)
	      {
		error_with_decl (decl,
			 "argument format specified for non-function `%s'");
		continue;
	      }
	
	    if (TREE_CODE (format_type) == IDENTIFIER_NODE
		&& (!strcmp (IDENTIFIER_POINTER (format_type), "printf")
		    || !strcmp (IDENTIFIER_POINTER (format_type),
				"__printf__")))
	      is_scan = 0;
	    else if (TREE_CODE (format_type) == IDENTIFIER_NODE
		     && (!strcmp (IDENTIFIER_POINTER (format_type), "scanf")
			 || !strcmp (IDENTIFIER_POINTER (format_type),
				     "__scanf__")))
	      is_scan = 1;
	    else if (TREE_CODE (format_type) == IDENTIFIER_NODE)
	      {
		error ("`%s' is an unrecognized format function type",
		       IDENTIFIER_POINTER (format_type));
		continue;
	      }
	    else
	      {
		error ("unrecognized format specifier");
		continue;
	      }

	    /* Strip any conversions from the string index and first arg number
	       and verify they are constants.  */
	    while (TREE_CODE (format_num_expr) == NOP_EXPR
		   || TREE_CODE (format_num_expr) == CONVERT_EXPR
		   || TREE_CODE (format_num_expr) == NON_LVALUE_EXPR)
	      format_num_expr = TREE_OPERAND (format_num_expr, 0);

	    while (TREE_CODE (first_arg_num_expr) == NOP_EXPR
		   || TREE_CODE (first_arg_num_expr) == CONVERT_EXPR
		   || TREE_CODE (first_arg_num_expr) == NON_LVALUE_EXPR)
	      first_arg_num_expr = TREE_OPERAND (first_arg_num_expr, 0);

	    if (TREE_CODE (format_num_expr) != INTEGER_CST
		|| TREE_CODE (first_arg_num_expr) != INTEGER_CST)
	      {
		error ("format string has non-constant operand number");
		continue;
	      }

	    format_num = TREE_INT_CST_LOW (format_num_expr);
	    first_arg_num = TREE_INT_CST_LOW (first_arg_num_expr);
	    if (first_arg_num != 0 && first_arg_num <= format_num)
	      {
		error ("format string arg follows the args to be formatted");
		continue;
	      }

	    /* If a parameter list is specified, verify that the format_num
	       argument is actually a string, in case the format attribute
	       is in error.  */
	    argument = TYPE_ARG_TYPES (type);
	    if (argument)
	      {
		for (arg_num = 1; ; ++arg_num)
		  {
		    if (argument == 0 || arg_num == format_num)
		      break;
		    argument = TREE_CHAIN (argument);
		  }
		if (! argument
		    || TREE_CODE (TREE_VALUE (argument)) != POINTER_TYPE
		  || (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_VALUE (argument)))
		      != char_type_node))
		  {
		    error ("format string arg not a string type");
		    continue;
		  }
		if (first_arg_num != 0)
		  {
		    /* Verify that first_arg_num points to the last arg,
		       the ...  */
		    while (argument)
		      arg_num++, argument = TREE_CHAIN (argument);
		  if (arg_num != first_arg_num)
		    {
		      error ("args to be formatted is not ...");
		      continue;
		    }
		  }
	      }

	    record_function_format (DECL_NAME (decl),
				    DECL_ASSEMBLER_NAME (decl),
				    is_scan, format_num, first_arg_num);
#endif
	    break;
	  }

	case A_FORMAT_ARG:
	  {
#if NOT_NEEDED_FOR_M2
	    tree format_num_expr = TREE_VALUE (args);
	    int format_num, arg_num;
	    tree argument;
	
	    if (TREE_CODE (decl) != FUNCTION_DECL)
	      {
		error_with_decl (decl,
			 "argument format specified for non-function `%s'");
		continue;
	      }
	
	    /* Strip any conversions from the first arg number and verify it
	       is a constant.  */
	    while (TREE_CODE (format_num_expr) == NOP_EXPR
		   || TREE_CODE (format_num_expr) == CONVERT_EXPR
		   || TREE_CODE (format_num_expr) == NON_LVALUE_EXPR)
	      format_num_expr = TREE_OPERAND (format_num_expr, 0);

	    if (TREE_CODE (format_num_expr) != INTEGER_CST)
	      {
		error ("format string has non-constant operand number");
		continue;
	      }

	    format_num = TREE_INT_CST_LOW (format_num_expr);

	    /* If a parameter list is specified, verify that the format_num
	       argument is actually a string, in case the format attribute
	       is in error.  */
	    argument = TYPE_ARG_TYPES (type);
	    if (argument)
	      {
		for (arg_num = 1; ; ++arg_num)
		  {
		    if (argument == 0 || arg_num == format_num)
		      break;
		    argument = TREE_CHAIN (argument);
		  }
		if (! argument
		    || TREE_CODE (TREE_VALUE (argument)) != POINTER_TYPE
		  || (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_VALUE (argument)))
		      != char_type_node))
		  {
		    error ("format string arg not a string type");
		    continue;
		  }
	      }

	    if (TREE_CODE (TREE_TYPE (TREE_TYPE (decl))) != POINTER_TYPE
		|| (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (TREE_TYPE (decl))))
		    != char_type_node))
	      {
		error ("function does not return string type");
		continue;
	      }

	    record_international_format (DECL_NAME (decl),
					 DECL_ASSEMBLER_NAME (decl),
					 format_num);
#endif
	    break;
	  }

	case A_WEAK:
	  declare_weak (decl);
	  break;

	case A_ALIAS:
	  if ((TREE_CODE (decl) == FUNCTION_DECL && DECL_INITIAL (decl))
	      || (TREE_CODE (decl) != FUNCTION_DECL && ! DECL_EXTERNAL (decl)))
	    error_with_decl (decl,
			     "`%s' defined both normally and as an alias");
	  else if (decl_function_context (decl) == 0)
	    {
	      tree id = get_identifier (TREE_STRING_POINTER
					(TREE_VALUE (args)));
	      if (TREE_CODE (decl) == FUNCTION_DECL)
		DECL_INITIAL (decl) = error_mark_node;
	      else
		DECL_EXTERNAL (decl) = 0;
	      assemble_alias (decl, id);
	    }
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;
	}
    }
}

/* Split SPECS_ATTRS, a list of declspecs and prefix attributes, into two
   lists.  SPECS_ATTRS may also be just a typespec (eg: RECORD_TYPE).

   The head of the declspec list is stored in DECLSPECS.
   The head of the attribute list is stored in PREFIX_ATTRIBUTES.

   Note that attributes in SPECS_ATTRS are stored in the TREE_PURPOSE of
   the list elements.  We drop the containing TREE_LIST nodes and link the
   resulting attributes together the way decl_attributes expects them.  */

void
split_specs_attrs (specs_attrs, declspecs, prefix_attributes)
     tree specs_attrs;
     tree *declspecs, *prefix_attributes;
{
  tree t, s, a, next, specs, attrs;

  /* This can happen in c++ (eg: decl: typespec initdecls ';').  */
  if (specs_attrs != NULL_TREE
      && TREE_CODE (specs_attrs) != TREE_LIST)
    {
      *declspecs = specs_attrs;
      *prefix_attributes = NULL_TREE;
      return;
    }

  /* Remember to keep the lists in the same order, element-wise.  */

  specs = s = NULL_TREE;
  attrs = a = NULL_TREE;
  for (t = specs_attrs; t; t = next)
    {
      next = TREE_CHAIN (t);
      /* Declspecs have a non-NULL TREE_VALUE.  */
      if (TREE_VALUE (t) != NULL_TREE)
	{
	  if (specs == NULL_TREE)
	    specs = s = t;
	  else
	    {
	      TREE_CHAIN (s) = t;
	      s = t;
	    }
	}
      else
	{
	  if (attrs == NULL_TREE)
	    attrs = a = TREE_PURPOSE (t);
	  else
	    {
	      TREE_CHAIN (a) = TREE_PURPOSE (t);
	      a = TREE_PURPOSE (t);
	    }
	  /* More attrs can be linked here, move A to the end.  */
	  while (TREE_CHAIN (a) != NULL_TREE)
	    a = TREE_CHAIN (a);
	}
    }

  /* Terminate the lists.  */
  if (s != NULL_TREE)
    TREE_CHAIN (s) = NULL_TREE;
  if (a != NULL_TREE)
    TREE_CHAIN (a) = NULL_TREE;

  /* All done.  */
  *declspecs = specs;
  *prefix_attributes = attrs;
}


/* Get the LABEL_DECL corresponding to identifier ID as a label.
   Create one if none exists so far for the current function.
   This function is called for both label definitions and label references.  */

tree
lookup_label (id)
     tree id;
{
  register tree decl = IDENTIFIER_LABEL_VALUE (id);

  if (current_function_decl == 0)
    {
      error ("label %s referenced outside of any function",
	     IDENTIFIER_POINTER (id));
      return 0;
    }

  /* Use a label already defined or ref'd with this name.  */
  if (decl != 0)
    {
#if NOT_NEEDED_FOR_MODULA_2
      /* But not if it is inherited and wasn't declared to be inheritable.  */
      if (DECL_CONTEXT (decl) != current_function_decl
	  && ! C_DECLARED_LABEL_FLAG (decl))
	return shadow_label (id);
#endif
      return decl;
    }

  decl = build_decl (LABEL_DECL, id, void_type_node);

  /* Make sure every label has an rtx.  */
  label_rtx (decl);

  /* A label not explicitly declared must be local to where it's ref'd.  */
  DECL_CONTEXT (decl) = current_function_decl;

  DECL_MODE (decl) = VOIDmode;

  /* Say where one reference is to the label,
     for the sake of the error if it is not defined.  */
  DECL_SOURCE_LINE (decl) = lineno;
  DECL_SOURCE_FILE (decl) = input_filename;

  IDENTIFIER_LABEL_VALUE (id) = decl;

  /* named_labels = tree_cons (NULL_TREE, decl, named_labels); */

  return decl;
}


tree
gccgm2_BuildStartEnumeration (name)
     char *name;
{
  tree id;

  if ((name == NULL) || (strcmp(name, "") == 0)) {
    id = NULL_TREE;
  } else {
    id = get_identifier (name);
  }
  enumvalues = NULL_TREE;
  return( start_enum(id) );
}

tree
gccgm2_BuildEndEnumeration (enumtype)
     tree enumtype;
{
  tree finished ATTRIBUTE_UNUSED = finish_enum(enumtype, enumvalues, NULL_TREE);
  enumvalues = NULL_TREE;
  return( enumtype );
}

tree
gccgm2_BuildEnumerator (name, value)
     char *name;
     tree value;
{
  tree id = get_identifier (name);
  tree copy_of_value = copy_node (value);
  tree gccenum = build_enumerator(id, copy_of_value);
  /* TREE_TYPE(id)   = integer_type_node; */

  /* choose value for enum value */
  enumvalues = chainon(gccenum, enumvalues);
  return( value );
}

/*
 *  BuildPointerType - returns a type which is a pointer to, totype.
 */

tree
gccgm2_BuildPointerType (totype)
     tree totype;
{
  return( build_pointer_type (totype) );
}


/*
 *  BuildArrayType - returns a type which is an array indexed by IndexType
 *                   and which has ElementType elements.
 */

tree
gccgm2_BuildArrayType (elementtype, indextype)
     tree elementtype, indextype;
{
  if (TREE_CODE (elementtype) == FUNCTION_TYPE) {
    return( build_array_type (ptr_type_node, indextype) );
  } else {
    return( build_array_type (elementtype, indextype) );
  }
}


/*
 *  BuildSubrangeType - creates a subrange of, type, with, lowval, highval.
 */

tree
gccgm2_BuildSubrangeType (name, type, lowval, highval)
     char *name;
     tree type, lowval, highval;
{
  tree id=build_range_type(type, lowval, highval);

  layout_type (id);
  if ((name == NULL) || (strcmp(name, "") == 0)) {
    /* no modula-2 name thus return id */
    return( id );
  } else {
    /* obviously declared as TYPE foo = [x..y] ; */
    id = gccgm2_DeclareKnownType(name, id);
    layout_type (id);
    return( id );
  }
}


/*
 *  BuildArrayIndexType - creates an integer index which accesses an array.
 *                        low and high are the min, max elements of the array.
 */

tree
gccgm2_BuildArrayIndexType (low, high)
     tree low, high;
{
  tree maxval = build_binary_op(MINUS_EXPR, default_conversion(high), default_conversion(low), 0);
  return( build_index_type(maxval) );
}


/*
 *  BuildVariableArrayAndDeclare - creates a variable length array.
 *                                 high is the maximum legal elements (which is a runtime variable).
 *                                 This creates and array index, array type and local variable.
 */

tree
gccgm2_BuildVariableArrayAndDeclare (elementtype, high, name, scope)
     tree elementtype, high;
     char *name;
     tree scope;
{
  tree indextype = build_index_type(variable_size(high));
  tree arraytype = build_array_type (elementtype, indextype);
  tree id        = get_identifier(name);
  tree decl;

  C_TYPE_VARIABLE_SIZE (arraytype) = TRUE;
  decl = build_decl (VAR_DECL, id, arraytype);

  DECL_SOURCE_LINE(decl)  = lineno;
  DECL_EXTERNAL   (decl)  = FALSE;
  TREE_PUBLIC     (decl)  = 1;
  DECL_CONTEXT    (decl)  = scope;
  TREE_USED       (arraytype)  = 1;
  TREE_USED       (decl)  = 1;

  C_DECL_VARIABLE_SIZE (decl) = TRUE;
  pushdecl(decl);
  expand_decl(decl);
  layout_decl (decl, 0);
#if 0
  rest_of_decl_compilation (decl, 0, 1, 0);
#endif
  return( decl );
}

/*
 *  BuildStartFunctionType - initializes global variables ready for building
 *                           a function.
 */

void
gccgm2_BuildStartFunctionType ()
{
  param_type_list = tree_cons (NULL_TREE, void_type_node, NULL_TREE);
  param_list = NULL_TREE;   /* ready for the next time we call/define a function */
}


/*
 *
 *  BuildEndFunctionType - build a function type which would return a, value.
 *                         The arguments have been created by BuildParameterDeclaration.
 */

tree
gccgm2_BuildEndFunctionType (value)
     tree value;
{
  if (value == NULL_TREE) {
    value = void_type_node;
  }
  return( build_pointer_type(build_function_type (value, param_type_list)) );
}


/*
 *  BuildParameterDeclaration - creates and returns one parameter from, name, and, type.
 *                              It appends this parameter to the internal param_type_list.
 *                              If name is nul then we assume we are creating a function
 *                              type declaration and we ignore names.
 */

tree
gccgm2_BuildParameterDeclaration (name, type, isreference)
     char *name;
     tree  type;
     int   isreference;
{
  tree parm_decl;

  layout_type (type);
  if (isreference) {
    type = build_reference_type (type);
  }
  if ((name != NULL) && (strcmp(name, "") != 0)) {
    /* creating a function with parameters */
    parm_decl = build_decl (PARM_DECL, get_identifier (name), type);
    DECL_ARG_TYPE (parm_decl) = type;
    param_list = chainon (parm_decl, param_list);
    layout_type (parm_decl);  /* testing (gaius) */
    layout_type (type);  /* testing (gaius) */
#if 0
    printf("making parameter %s known to gcc\n", name);
#endif
    param_type_list = tree_cons (NULL_TREE, type, param_type_list);
    return( parm_decl );
  } else {
    param_type_list = tree_cons (NULL_TREE, type, param_type_list);
    return( type );
  }
}


/*
 *  BuildStartFunctionDeclaration - initializes global variables ready for building
 *                                  a function.
 */

void
gccgm2_BuildStartFunctionDeclaration ()
{
  param_type_list = tree_cons (NULL_TREE, void_type_node, NULL_TREE);
  param_list = NULL_TREE;   /* ready for when we define a function */
}

/*
 *
 *  BuildEndFunctionDeclaration - build a function which will return a value of returntype.
 *                                The arguments have been created by BuildParameterDeclaration.
 */

tree
gccgm2_BuildEndFunctionDeclaration (name, returntype, isexternal)
     char *name;
     tree  returntype;
     int   isexternal;
{
  tree fntype;
  tree fndecl;

  /*
   *  the function type depends on the return type and type of args,
   *  both of which we have created in BuildParameterDeclaration
   */
  if (returntype == NULL_TREE) {
    returntype = void_type_node;
  } else if (TREE_CODE(returntype) == FUNCTION_TYPE) {
    returntype = ptr_type_node; // build_unary_op(ADDR_EXPR, returntype, 0);
  }

  fntype = build_function_type (returntype, param_type_list);
  fndecl = build_decl (FUNCTION_DECL, get_identifier (name), fntype);

  DECL_EXTERNAL (fndecl)    = isexternal;
  TREE_PUBLIC (fndecl)      = 1;
  TREE_STATIC (fndecl)      = 1;
  DECL_ARGUMENTS (fndecl)   = param_list;
  DECL_RESULT (fndecl)      = build_decl (RESULT_DECL, NULL_TREE, returntype);
  DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;
  TREE_TYPE(fndecl)         = fntype;

  DECL_SOURCE_FILE (fndecl) = input_filename;
  DECL_SOURCE_LINE (fndecl) = lineno;

  rest_of_decl_compilation (fndecl, NULL_PTR, 1, 0);
  param_list = NULL_TREE;   /* ready for the next time we call/define a function */
  return( fndecl );
}


/*
 *  BuildStartFunctionCode - generate function entry code.
 */

void
gccgm2_BuildStartFunctionCode (fndecl, isexported)
     tree fndecl;
     int  isexported;
{
  tree param_decl, next_param;

  /* set line number information */
  DECL_SOURCE_FILE (fndecl) = input_filename;
  DECL_SOURCE_LINE (fndecl) = lineno;

  /* Announce we are compiling this function.  */
  announce_function (fndecl);

  /* Set up to compile the function and enter it.  */
  current_function_decl = fndecl;
  DECL_INITIAL (fndecl) = error_mark_node;

  temporary_allocation ();
  pushlevel (0);
  make_function_rtl (fndecl);

  /* Push all the PARM_DECL nodes onto the current scope (i.e. the scope of the
     subprogram body) so that they can be recognized as local variables in the
     subprogram.   */

  for (param_decl = nreverse (DECL_ARGUMENTS (fndecl));
       param_decl; param_decl = next_param)
    {
      next_param = TREE_CHAIN (param_decl);
      TREE_CHAIN (param_decl) = NULL;
      pushdecl (param_decl);
    }

  /* Store back the PARM_DECL nodes. They appear in the right order. */
  DECL_ARGUMENTS (fndecl) = getdecls ();
  /* This function exists in static storage.
     (This does not mean `static' in the C sense!)  */
  TREE_STATIC (fndecl)     = 1;
  TREE_PUBLIC (fndecl)     = isexported;
  TREE_ADDRESSABLE(fndecl) = 1;   /* could be improved, if we want inline it make this a 0 */

  init_function_start (fndecl, input_filename, lineno);
  expand_function_start (fndecl, 0);
  expand_start_bindings (0);

  /*
   *  create a block at the BEGIN for the local variables
   */
  pushlevel (0);
}


/*
 *  BuildEndFunctionCode - generates the function epilogue.
 */

void
gccgm2_BuildEndFunctionCode (fndecl)
     tree fndecl;
{
  tree block;

  /* pop the block level */
  block = poplevel(1, 1, 0);
#if 0
  debug_tree(block);
#endif
  expand_end_bindings(block, 1, 0);

  /* get back out of the function and compile it */
  block = poplevel (1, 0, 1);
  DECL_INITIAL(fndecl) = block;
  BLOCK_SUPERCONTEXT (DECL_INITIAL (fndecl)) = fndecl;
  /* expand_end_bindings (block, 1, 0); */

  expand_function_end (input_filename, lineno, 0);
  rest_of_compilation (fndecl);
  current_function_decl = 0;
  permanent_allocation (1);
}


#if 1
void
iterative_factorial()
{
  /****************************
   **
   ** 2. iterative factorial
   **
   ****************************/

  tree fakdecl;

  tree locdecl, functype, funcid, resultdecl, parmdecl, block, parmid, arglist;
  struct nesting *loop;
  tree assign;

  /* build & initialize declaration */
  arglist = listify(integer_type_node);
  functype = build_function_type(integer_type_node, arglist);
  funcid = get_identifier("fak");  
  fakdecl = build_decl(FUNCTION_DECL, funcid, functype);
  temporary_allocation();
  TREE_TYPE(fakdecl) = functype;
  resultdecl = build_decl(RESULT_DECL, NULL_TREE, integer_type_node);
  TREE_TYPE(resultdecl) = integer_type_node;
  DECL_CONTEXT(resultdecl) = fakdecl;
  DECL_RESULT(fakdecl) = resultdecl;
  layout_decl(resultdecl, 0);
  parmid = get_identifier("n");
  parmdecl = build_decl(PARM_DECL, parmid, integer_type_node);
  DECL_CONTEXT(parmdecl) = fakdecl;
  DECL_ARG_TYPE(parmdecl) = integer_type_node;
  DECL_ARGUMENTS(fakdecl) = parmdecl;
  layout_decl(parmdecl, 0);
  TREE_STATIC(fakdecl) = 1;
  TREE_ADDRESSABLE(fakdecl) = 1;
  TREE_PUBLIC(fakdecl) = 1;

  /* let's go! */
  current_function_decl = fakdecl;
  announce_function(fakdecl);
  rest_of_decl_compilation(fakdecl, NULL, 1, 0);
  make_function_rtl(fakdecl);
  init_function_start(fakdecl, NULL, 0);
  expand_function_start(fakdecl, 0);
  pushlevel(0);
  expand_start_bindings(0);
  pushdecl(parmdecl);

  /* implement function body */
  /* build tree for "Result := 1" and convert it to RTL : */
  assign = build(MODIFY_EXPR, void_type_node, resultdecl, integer_one_node);
  TREE_SIDE_EFFECTS(assign) = 1;
  TREE_USED(resultdecl) = 1;
  expand_expr_stmt(assign);

  /* build local variable declaration for 'i' : */  
  locdecl = build_decl(VAR_DECL, get_identifier("i"), integer_type_node);
  /* set its scope */
  DECL_CONTEXT(locdecl) = fakdecl;
  /* set its initial value */
  DECL_INITIAL(locdecl) = integer_one_node;
  /* insert it to debugging symbol table */
  pushdecl(locdecl);
  /* convert it to RTL */
  expand_decl(locdecl);
  /* and also emit initialization assignment (i := 1) */
  expand_decl_init(locdecl);

  /* emit RTL for head of "while"-loop */
  loop = expand_start_loop(0);

  /* emit RTL for test of exit condition "i < n" : */
  expand_exit_loop_if_false(loop, build(LE_EXPR, integer_type_node, 
					locdecl, parmdecl));

  /* build tree for "Result := Result * i" and convert it to RTL: */
  assign = build(MODIFY_EXPR, void_type_node, 
		 resultdecl,
		 build(MULT_EXPR, integer_type_node, 
		       resultdecl,
		       locdecl
		       )
		 );
  TREE_SIDE_EFFECTS(assign) = 1; 
  expand_expr_stmt(assign);

  /* build tree for "i := i + 1" and convert it to RTL: */
  assign = build(MODIFY_EXPR, void_type_node, 
		 locdecl, 
		 build(PLUS_EXPR, integer_type_node, 
		       locdecl,
		       integer_one_node
		       )
		 );
  TREE_SIDE_EFFECTS(assign) = 1; 
  expand_expr_stmt(assign);

  /* emit RTL for end of "while" loop */
  expand_end_loop();

  /* emit RTL for return statement, see commentary above */
  expand_return(resultdecl);

  /* cleanup tail */
  block = poplevel(1,0,1);
  DECL_INITIAL(fakdecl) = block;
  expand_end_bindings(block, 1, 1);
  expand_function_end(NULL, 0, 0);
  rest_of_compilation(fakdecl);
  end_temporary_allocation();
  current_function_decl = NULL_TREE;
}
#endif

/*
 *  BuildReturnValueCode - generates the code associated with: RETURN( value )
 */

void
gccgm2_BuildReturnValueCode (fndecl, value)
     tree fndecl, value;
{
  if (TREE_CODE (TREE_TYPE (value)) == FUNCTION_TYPE) {
    expand_return( build (MODIFY_EXPR, void_type_node,
			  DECL_RESULT (fndecl), build1 (CONVERT_EXPR, ptr_type_node, value)) );

  } else {
    expand_return( build (MODIFY_EXPR, void_type_node,
			  DECL_RESULT (fndecl),
			  gccgm2_BuildConvert (TREE_TYPE (DECL_RESULT (fndecl)), value, FALSE)) );
  }
}


/*
 *  BuildAssignment - builds the assignment of, des, and, expr.
 *                    It returns, des.
 */

tree
gccgm2_BuildAssignment (des, expr)
     tree des, expr;
{
  if (TREE_CODE(expr) == FUNCTION_DECL) {
    expr = build_unary_op(ADDR_EXPR, expr, 0);
  }

  expand_assignment( des, expr, 0, 0 );
  return( des );
}


/*
 *  BuildAdd - builds an addition tree.
 */

tree
gccgm2_BuildAdd (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return( build_binary_op(PLUS_EXPR, op1, op2, needconvert) );
}


/*
 *  BuildSub - builds a subtraction tree.
 */

tree
gccgm2_BuildSub (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return( build_binary_op(MINUS_EXPR, op1, op2, needconvert) );
}


/*
 *  BuildMult - builds a multiplication tree.
 */

tree
gccgm2_BuildMult (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return( build_binary_op(MULT_EXPR, op1, op2, needconvert) );
}


/*
 *  BuildDiv - builds a division tree.
 */

tree
gccgm2_BuildDiv (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return( build_binary_op(TRUNC_DIV_EXPR, op1, op2, needconvert) );
}


/*
 *  BuildMod - builds a modulus tree.
 */

tree
gccgm2_BuildMod (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return( build_binary_op(TRUNC_MOD_EXPR, op1, op2, needconvert) );
}


/*
 *  BuildLSL - builds and returns tree (op1 << op2)
 */

tree
gccgm2_BuildLSL (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return( build_binary_op(LSHIFT_EXPR, op1, op2, needconvert) );
}


/*
 *  BuildConvert - build and return tree VAL(op1, op2)
 *                 where op1 is the type to which op2 is to be converted.
 */

tree
gccgm2_BuildConvert (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert ATTRIBUTE_UNUSED;
{
  return( convert_and_check(op1, op2) );
}


/*
 *  BuildTrunc - returns an integer expression from a REAL or LONGREAL op1.
 */

tree
gccgm2_BuildTrunc (op1)
     tree op1;
{
  return( convert_to_integer(gccgm2_GetIntegerType(), op1) );
}

/*
 *  BuildNegate - builds a negate expression and returns the tree.
 */

tree
gccgm2_BuildNegate (op1, needconvert)
     tree op1;
     int  needconvert;
{
  return( build_unary_op (NEGATE_EXPR, op1, needconvert) );
}


/*
 *  gccgm2_GetSizeOf - taken from c-typeck.c (c_sizeof).
 */

tree
gccgm2_GetSizeOf (type)
     tree type;
{
  enum tree_code code = TREE_CODE (type);
  tree t;

  if (code == FUNCTION_TYPE)
    {
      return( gccgm2_GetSizeOf(ptr_type_node) );
    }
  if (code == VOID_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	pedwarn ("sizeof applied to a void type");
      return size_int (1);
    }
  if (code == VAR_DECL)
    return( gccgm2_GetSizeOf(TREE_TYPE(type)) );

  if (code == PARM_DECL)
    return( gccgm2_GetSizeOf(TREE_TYPE(type)) );

  if (code == ERROR_MARK)
    return size_int (1);
  if (TYPE_SIZE (type) == 0)
    {
      error ("sizeof applied to an incomplete type");
      return size_int (0);
    }
  /* Convert in case a char is more than one unit.  */
  t = size_binop (CEIL_DIV_EXPR, TYPE_SIZE (type), 
		  size_int (TYPE_PRECISION (char_type_node)));
  t = convert (sizetype, t);
  /* size_binop does not put the constant in range, so do it now.  */
  if (TREE_CODE (t) == INTEGER_CST && force_fit_type (t, 0))
    TREE_CONSTANT_OVERFLOW (t) = TREE_OVERFLOW (t) = 1;
  return t;
}


/*
 *  BuildSize - builds a SIZE function expression and returns the tree.
 */

tree
gccgm2_BuildSize (op1, needconvert)
     tree op1;
     int  needconvert ATTRIBUTE_UNUSED;
{
  return( gccgm2_GetSizeOf(op1) );
}


/*
 *  BuildAddr - builds an expression which calculates the address of op1 and returns the tree.
 */

tree
gccgm2_BuildAddr (op1, needconvert)
     tree op1;
     int  needconvert;
{
  return( convert (integer_type_node, build_unary_op (ADDR_EXPR, op1, needconvert)) );
}


/*
 *  BuildOffset - builds an expression containing the number of bytes the field
 *                is offset from the start of the record structure.
 *                The expression is returned.
 */

tree
gccgm2_BuildOffset (field, needconvert)
     tree field;
     int  needconvert ATTRIBUTE_UNUSED;
{
  return( gccgm2_BuildConvert(gccgm2_GetIntegerType(),
			      gccgm2_BuildDiv(DECL_FIELD_BITPOS (field), gccgm2_BuildIntegerConstant(BITS_PER_UNIT), FALSE),
			      FALSE) );
}


/*
 *  BuildLogicalOr - build a logical or expressions and return the tree.
 */

tree
gccgm2_BuildLogicalOr (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return( build_binary_op (TRUTH_OR_EXPR, op1, op2, needconvert) );
}


/*
 *  BuildLogicalAnd - build a logical and expression and return the tree.
 */

tree
gccgm2_BuildLogicalAnd (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return( build_binary_op (TRUTH_AND_EXPR, op1, op2, needconvert) );
}


/*
 *  BuildSymmetricalDifference - build a logical xor expression and return the tree.
 */

tree
gccgm2_BuildSymmetricDifference (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return( build_binary_op (TRUTH_XOR_EXPR, op1, op2, needconvert) );
}


/*
 *  create_label_from_name - returns a tree label.
 */

tree
create_label_from_name (name)
     char *name;
{
  tree id   = get_identifier (name);   /* name must never conflict with the scope universe */
  tree decl = lookup_label   (id);
  
  if (decl == 0) {
    error ("problems trying to create a label");
  }
  return( decl );
}


/*
 *  BuildGoto - builds a goto operation.
 */

void
gccgm2_BuildGoto (char *name)
{
  tree decl = create_label_from_name(name);

  TREE_USED (decl) = 1;
  expand_goto (decl);
}


/*
 *  DeclareLabel - create a label, name, and return the tree.
 */

tree
gccgm2_DeclareLabel (name)
     char *name;
{
  tree id   = get_identifier (name);
  tree decl = lookup_label   (id);

  if (decl == 0) {
    error ("problems trying to declare a label");
  } else {
    TREE_USED (decl) = 1;
  }
  expand_label( decl );
  return( decl );
}


/*
 *  BuildLessThan - return a tree which computes <
 */

tree
gccgm2_BuildLessThan (op1, op2)
     tree op1, op2;
{
  return( build_binary_op (LT_EXPR, op1, op2, 0) );
}


/*
 *  BuildGreaterThan - return a tree which computes >
 */

tree
gccgm2_BuildGreaterThan (op1, op2)
     tree op1, op2;
{
  return( build_binary_op (GT_EXPR, op1, op2, 0) );
}


/*
 *  BuildLessThanOrEqual - return a tree which computes <
 */

tree
gccgm2_BuildLessThanOrEqual (op1, op2)
     tree op1, op2;
{
  return( build_binary_op (LE_EXPR, op1, op2, TRUE) );
}


/*
 *  BuildGreaterThanOrEqual - return a tree which computes >=
 */

tree
gccgm2_BuildGreaterThanOrEqual (op1, op2)
     tree op1, op2;
{
  return( build_binary_op (GE_EXPR, op1, op2, 0) );
}


/*
 *  BuildEqualTo - return a tree which computes =
 */

tree
gccgm2_BuildEqualTo (op1, op2)
     tree op1, op2;
{
  return( build_binary_op (EQ_EXPR, op1, op2, 0) );
}


/*
 *  BuildEqualNotTo - return a tree which computes #
 */

tree
gccgm2_BuildNotEqualTo (op1, op2)
     tree op1, op2;
{
  return( build_binary_op (NE_EXPR, op1, op2, 0) );
}


/*
 *  BuildIfIn - return a tree which computes (op1 IN op2)
 */

tree
gccgm2_BuildIfIn (op1, op2)
     tree op1, op2;
{
  return( build_binary_op(NE_EXPR,
			  build_binary_op(TRUTH_AND_EXPR, op1, op2, 0),
			  integer_zero_node, 0));
}


/*
 *  BuildIfNotIn - return a tree which computes NOT (op1 IN op2)
 */

tree
gccgm2_BuildIfNotIn (op1, op2)
     tree op1, op2;
{
  return( build_binary_op(EQ_EXPR,
			  build_binary_op(TRUTH_AND_EXPR, op1, op2, 0),
			  integer_zero_node, 0) );
}


/*
 *  BuildIndirect - build: (*target) given that the object to be copied is of, type.
 */

tree
gccgm2_BuildIndirect (target, type)
     tree target, type;
{ 
  /*
   *   Note that the second argument to build1 is:
   *
   *   TYPE_QUALS is a list of modifiers such as const or volatile
   *   to apply to the pointer type, represented as identifiers.
   *
   *   it also determines the type of arithmetic and size of
   *   the object to be indirectly moved.
   */

  return( build1 (INDIRECT_REF, type, target) );
}


/*
 *  DoJump - jump to the appropriate label depending whether
 *           result of the expression is TRUE or FALSE.
 */

void
gccgm2_DoJump (exp, falselabel, truelabel)
     tree exp;
     char *falselabel;
     char *truelabel;
{
  if ((falselabel != 0) && (truelabel == 0)) {
    do_jump(exp, label_rtx(create_label_from_name(falselabel)), NULL_RTX);
  } else if ((falselabel == 0) && (truelabel != 0)) {
    do_jump(exp, NULL_RTX, label_rtx(create_label_from_name(truelabel)));
  } else {
    error("expecting one and only one label to be declared");
  }
}

/*
 *  BuildParam - build a list of parameters, ready for a subsequent procedure call.
 */

void
gccgm2_BuildParam (param)
     tree param;
{
#if 0
  fprintf(stderr, "tree for parameter containing "); fflush(stderr);
  fprintf(stderr, "list of elements\n"); fflush(stderr);
#endif
  if (TREE_CODE(param) == FUNCTION_DECL) {
    param = build_unary_op(ADDR_EXPR, param, 0);
  }
  param_list = chainon (build_tree_list(NULL_TREE, param), param_list);
#if 0
  debug_tree(param_list);
  fprintf(stderr, "end of tree for parameter\n"); fflush(stderr);
#endif
}

/*
 *  BuildProcedureCall - creates a procedure call from a procedure and
 *                       parameter list and the return type, rettype.
 */

tree
gccgm2_BuildProcedureCall (procedure, rettype)
     tree procedure, rettype;
{
  tree functype = TREE_TYPE(procedure);
  tree funcptr  = build1(ADDR_EXPR, build_pointer_type(functype), procedure);
  tree call;

  TREE_USED(procedure) = TRUE;

  if (rettype == NULL_TREE) {
    rettype = void_type_node;
    call = build(CALL_EXPR, rettype, funcptr, param_list, NULL_TREE);
    TREE_USED(call)         = TRUE;
    TREE_SIDE_EFFECTS(call) = TRUE ;

#if 0
    fprintf(stderr, "built the modula-2 call, here are the params\n"); fflush(stderr);
    debug_tree(param_list);
#endif
#if defined(DEBUG_PROCEDURE_CALLS)
    fprintf(stderr, "built the modula-2 call, here is the tree\n"); fflush(stderr);
    debug_tree(call);
#endif
    expand_expr_stmt(call);
    last_function   = NULL_TREE;
  } else {
    last_function   = build(CALL_EXPR, rettype, funcptr, param_list, NULL_TREE);
  }

  param_list = NULL_TREE;   /* ready for the next time we call a procedure */
  return( last_function );
}

/*
 *  BuildIndirectProcedureCall - creates a procedure call from a procedure and
 *                               parameter list and the return type, rettype.
 */

tree
gccgm2_BuildIndirectProcedureCall (procedure, rettype)
     tree procedure, rettype;
{
  tree call;

  TREE_USED(procedure) = TRUE;

  if (rettype == NULL_TREE) {
    rettype = void_type_node;
    call = build(CALL_EXPR, rettype, procedure, param_list, NULL_TREE);
    TREE_USED(call)         = TRUE;
    TREE_SIDE_EFFECTS(call) = TRUE ;

#if 0
    fprintf(stderr, "built the modula-2 call, here are the params\n"); fflush(stderr);
    debug_tree(param_list);
#endif
#if defined(DEBUG_PROCEDURE_CALLS)
    fprintf(stderr, "built the modula-2 call, here is the tree\n"); fflush(stderr);
    debug_tree(call);
#endif
    expand_expr_stmt(call);
    last_function   = NULL_TREE;
  } else {
    last_function   = build(CALL_EXPR, rettype, procedure, param_list, NULL_TREE);
  }

  param_list = NULL_TREE;   /* ready for the next time we call a procedure */
  return( last_function );
}

/*
 *  BuildFunctValue - generates code for value := last_function(foobar);
 */

void
gccgm2_BuildFunctValue (value)
     tree value;
{
  tree assign = build_modify_expr(value, NOP_EXPR, last_function);

  TREE_SIDE_EFFECTS(assign) = TRUE;
  TREE_USED(assign) = TRUE;
#if 0
  debug_tree(assign);
#endif
  expand_expr_stmt(assign);
}

/*
 *  BuildAsm - generates an inline assembler instruction.
 */

void
gccgm2_BuildAsm (instr, IsVolatile, inputs, outputs, trash)
     tree instr;
     int  IsVolatile;
     tree inputs, outputs, trash;
{
  expand_asm_operands (instr, outputs, inputs, trash, IsVolatile,
		       input_filename, lineno);
}

/*
 *  AssignBooleanTrueFalse - assigns the tree nodes to the internal gcc types.
 *                           This allows gm2 to declare boolean as an enumerated type.
 */

void
gccgm2_AssignBooleanTrueFalse (booleanid, trueid, falseid)
     tree booleanid, trueid, falseid;
{
  boolean_type_node  = booleanid;
  boolean_true_node  = trueid;
  boolean_false_node = falseid;
  TREE_TYPE(boolean_false_node) = booleanid;
  TREE_TYPE(boolean_true_node)  = booleanid;
}

tree
gccgm2_GetIntegerType ()
{
  return( integer_type_node );
}

tree
gccgm2_GetCharType ()
{
  return( char_type_node );
}

tree
gccgm2_GetByteType ()
{
  return( unsigned_char_type_node );
}

tree
gccgm2_GetVoidType ()
{
  return( void_type_node );
}

tree
gccgm2_GetPointerType ()
{
  return( ptr_type_node );
}

tree
gccgm2_GetCardinalType ()
{
  return( unsigned_type_node );
}

tree
gccgm2_GetBitsetType ()
{
  return( unsigned_type_node );   /* --fixme-- */
}

tree
gccgm2_GetRealType ()
{
  return( float_type_node );
}

tree
gccgm2_GetLongRealType ()
{
  return( double_type_node );
}

tree
gccgm2_GetLongIntType ()
{
  return( long_unsigned_type_node );
}

tree
gccgm2_GetWordType ()
{
  return( long_unsigned_type_node );
}

tree
gccgm2_GetProcType ()
{
  return( proc_type_node );
}

tree
gccgm2_GetIntegerZero ()
{
  return( integer_zero_node );
}

tree
gccgm2_GetIntegerOne ()
{
  return( integer_one_node );
}

tree
gccgm2_GetCurrentFunction ()
{
  return( current_function_decl );
}

tree
gccgm2_GetErrorNode ()
{
  return( error_mark_node );
}

/*
 *  convertToPtr - if the type of tree, t, is not a ptr_type_node then convert it.
 */

tree
convertToPtr (t)
     tree t;
{
  if (TREE_CODE(TREE_TYPE(t)) == POINTER_TYPE) {
    return( t );
  } else {
    return( gccgm2_BuildConvert(ptr_type_node, t, FALSE) );
  }
}

tree
gccgm2_BuiltInMemCopy (dest, src, n)
     tree dest, src, n;
{
  tree params   = chainon(chainon(build_tree_list(NULL_TREE, convertToPtr(dest)),
				  build_tree_list(NULL_TREE, convertToPtr(src))),
			  build_tree_list(NULL_TREE, n));
  tree functype = TREE_TYPE(gm2_memcpy_node);
  tree funcptr  = build1(ADDR_EXPR, build_pointer_type(functype), gm2_memcpy_node);
  tree call     = build(CALL_EXPR, ptr_type_node, funcptr, params, NULL_TREE);

  TREE_USED(call)         = TRUE;
  TREE_SIDE_EFFECTS(call) = TRUE ;

#if 0
  fprintf(stderr, "built the modula-2 call, here are the params\n"); fflush(stderr);
  debug_tree(params);
  fprintf(stderr, "built the modula-2 call, here is the tree\n"); fflush(stderr);
  debug_tree(call);
#endif
  return( call );
}

/*
 *   BuiltInAlloca - given an expression, n, allocate, n, bytes on the stack for the life
 *                   of the current function.
 */

tree
gccgm2_BuiltInAlloca (n)
     tree n;
{
  tree params   = listify(n);
  tree functype = TREE_TYPE(gm2_alloca_node);
  tree funcptr  = build1(ADDR_EXPR, build_pointer_type(functype), gm2_alloca_node);
  tree call     = build(CALL_EXPR, ptr_type_node, funcptr, params, NULL_TREE);

  TREE_USED(call)         = TRUE;
  TREE_SIDE_EFFECTS(call) = TRUE ;

  return( call );
}

/*
 *  AreConstantsEqual - maps onto tree.c (tree_int_cst_equal). It returns
 *                      TRUE if the value of e1 is the same as e2.
 */

int
gccgm2_AreConstantsEqual (e1, e2)
  tree e1, e2;
{
  return( tree_int_cst_equal(e1, e2) != 0 );
}

/*
 *  DetermineSign - returns -1 if e<0
 *                           0 if e==0
 *                           1 if e>0
 *
 *                  an unsigned constant will never return -1
 */

int
gccgm2_DetermineSign (e)
     tree e;
{
  return( tree_int_cst_sgn(e) );
}

tree
gccgm2_BuildIntegerConstant (int value)
{
  tree id;

  switch (value) {

  case 0:  return( integer_zero_node );
           break;
  case 1:  return( integer_one_node );
           break;

  default:
      id = build_int_2 (value, 0);
      return( id );
  }
}

tree mystr;

/*
 *  BuildStringConstant - creates a string constant given a, string, and, length.
 */

tree
gccgm2_BuildStringConstant (string, length)
     char *string;
     int   length;
{
  tree id=build_string(length, string);

  TREE_TYPE(id) = char_array_type_node;
  mystr = id;
  return( id );
}


/*
 *  BuildCharConstant - creates a character constant given a, string.
 */

tree
gccgm2_BuildCharConstant (string)
     char *string;
{
  unsigned num_bits = TYPE_PRECISION (char_type_node) * strlen(string);
  int result        = (int) string[0];
  tree id;

  if (TREE_UNSIGNED (char_type_node) || ((result >> (num_bits - 1)) & 1) == 0)
    id = build_int_2 (result & ((unsigned HOST_WIDE_INT) ~0
				 >> (HOST_BITS_PER_WIDE_INT - num_bits)),
		       0);
  else
    id = build_int_2 (result | ~((unsigned HOST_WIDE_INT) ~0
				 >> (HOST_BITS_PER_WIDE_INT - num_bits)),
		      -1);
  TREE_TYPE (id) = integer_type_node;
  return( id );
}

tree
gccgm2_BuildRealConstant (value)
     REAL_VALUE_TYPE value;
{
  return( build_real(gccgm2_GetRealType(), value) );
}

tree
gccgm2_BuildLongRealConstant (value)
     REAL_VALUE_TYPE value;
{
  return( build_real(gccgm2_GetLongRealType(), value) );
}

/*
 *  ConvertConstantAndCheck - in Modula-2 sementics: return( VAL(type, expr) )
 *                            Only to be used for a constant expr,
 *                            overflow checking is performed. 
 */

tree
gccgm2_ConvertConstantAndCheck (type, expr)
     tree type, expr;
{
  return( convert_and_check(type, expr) );
}

/*
 *  BuildStart - creates a module initialization function. We make this function
 *               public if it is not an inner module. The linker will create
 *               a call list for all linked modules which determines the
 *               initialization sequence for all modules.
 */

tree
gccgm2_BuildStart (name, inner_module)
     char *name;
     int   inner_module;
{
  tree fntype;
  tree fndecl;

  /* The function type depends on the return type and type of args.  */
  fntype = build_function_type (integer_type_node, NULL_TREE);
  fndecl = build_decl (FUNCTION_DECL, get_identifier (name), fntype);

  DECL_EXTERNAL (fndecl) = 0;
  if (! inner_module) {
    TREE_PUBLIC (fndecl) = 1;
  }
  TREE_STATIC (fndecl)   = 1;
  DECL_RESULT (fndecl)   = build_decl (RESULT_DECL, NULL_TREE, integer_type_node);
  DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;

  rest_of_decl_compilation (fndecl, NULL_PTR, 1, 0);

  /* Announce we are compiling this function.  */
  announce_function (fndecl);

  /* Set up to compile the function and enter it.  */
  current_function_decl = fndecl;
  DECL_INITIAL (fndecl) = error_mark_node;

  temporary_allocation ();
  pushlevel (0);
  make_function_rtl (fndecl);

#if 0
  /* Push all the PARM_DECL nodes onto the current scope (i.e. the scope of the
     subprogram body) so that they can be recognized as local variables in the
     subprogram.   */

  for (param_decl = nreverse (DECL_ARGUMENTS (fndecl));
       param_decl; param_decl = next_param)
    {
      next_param = TREE_CHAIN (param_decl);
      TREE_CHAIN (param_decl) = NULL;
      pushdecl (param_decl);
    }

  /* Store back the PARM_DECL nodes. They appear in the right order. */
  DECL_ARGUMENTS (fndecl) = getdecls ();
#endif

  init_function_start (fndecl, input_filename, lineno);
  expand_function_start (fndecl, 0);
  expand_start_bindings (0);
  return( fndecl );
}


void
gccgm2_BuildEnd (fndecl)
     tree fndecl;
{
  /* Now get back out of the function and compile it.  */
  expand_end_bindings (NULL_TREE, 1, 0);
  poplevel (1, 0, 1);
  expand_function_end (input_filename, lineno, 0);
  rest_of_compilation (fndecl);
  current_function_decl = 0;
  permanent_allocation (1);
}

void
gccgm2_BuildCallInnerInit (fndecl)
     tree fndecl;
{
  expand_expr_stmt( build_function_call(fndecl, NULL_TREE) );
}


/*
 *  BuildStartMainModule - expands all the global variables ready for the main module.
 */

void
gccgm2_BuildStartMainModule ()
{
  /* nothing to do here */
}


/*
 *  BuildEndMainModule - tidies up the end of the main module. It moves
 *                       back to global scope.
 */

void
gccgm2_BuildEndMainModule ()
{
  /* nothing to do here */
}


tree
gccgm2_BuildExpand ()
{
  return( gccgm2_DeclareKnownVariable("gaius", gccgm2_GetIntegerType(), 0, 0, 0, 0, current_function_decl) );
}


/*
 *  DebugTree - display the tree, t.
 */

void
gccgm2_DebugTree (t)
     tree t;
{
  debug_tree(t);
}

/* taken from c-typeck.c line 1534 */

/* Build a function call to function FUNCTION with parameters PARAMS.
   PARAMS is a list--a chain of TREE_LIST nodes--in which the
   TREE_VALUE of each node is a parameter-expression.
   FUNCTION's data type may be a function type or a pointer-to-function.  */

tree
build_function_call (function, params)
     tree function, params;
{
  register tree fntype, fundecl = 0;
  register tree coerced_params;
  tree name = NULL_TREE, assembler_name = NULL_TREE;

  /* Strip NON_LVALUE_EXPRs, etc., since we aren't using as an lvalue.  */
  STRIP_TYPE_NOPS (function);

  /* Convert anything with function type to a pointer-to-function.  */
  if (TREE_CODE (function) == FUNCTION_DECL)
    {
      name = DECL_NAME (function);
      assembler_name = DECL_ASSEMBLER_NAME (function);

      /* Differs from default_conversion by not setting TREE_ADDRESSABLE
	 (because calling an inline function does not mean the function
	 needs to be separately compiled).  */
      fntype = build_type_variant (TREE_TYPE (function),
				   TREE_READONLY (function),
				   TREE_THIS_VOLATILE (function));
      fundecl = function;
      function = build1 (ADDR_EXPR, build_pointer_type (fntype), function);
    }
  else
    function = default_conversion (function);

  fntype = TREE_TYPE (function);

  if (TREE_CODE (fntype) == ERROR_MARK)
    return error_mark_node;

  if (!(TREE_CODE (fntype) == POINTER_TYPE
	&& TREE_CODE (TREE_TYPE (fntype)) == FUNCTION_TYPE))
    {
      error ("called object is not a function");
      return error_mark_node;
    }

  /* fntype now gets the type of function pointed to.  */
  fntype = TREE_TYPE (fntype);


#if NOT_NEEDED_FOR_MODULA
  /* not needed as the front end will convert any parameters using
     assignment to temps and will build unbounded arrays using
     specific operators.
  */


  /* Convert the parameters to the types declared in the
     function prototype, or apply default promotions.  */

  coerced_params
    = convert_arguments (TYPE_ARG_TYPES (fntype), params, name, fundecl);

  /* Check for errors in format strings.  */

  if (warn_format && (name || assembler_name))
    check_function_format (name, assembler_name, coerced_params);
#else
  coerced_params = params;
#endif

  /* Recognize certain built-in functions so we can make tree-codes
     other than CALL_EXPR.  We do this when it enables fold-const.c
     to do something useful.  */

  if (TREE_CODE (function) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (function, 0)) == FUNCTION_DECL
      && DECL_BUILT_IN (TREE_OPERAND (function, 0)))
    switch (DECL_FUNCTION_CODE (TREE_OPERAND (function, 0)))
      {
      case BUILT_IN_ABS:
      case BUILT_IN_LABS:
      case BUILT_IN_FABS:
	if (coerced_params == 0)
	  return integer_zero_node;
	return build_unary_op (ABS_EXPR, TREE_VALUE (coerced_params), 0);
      default:
	break;
      }

  {
    register tree result
      = build (CALL_EXPR, TREE_TYPE (fntype),
	       function, coerced_params, NULL_TREE);

    TREE_SIDE_EFFECTS (result) = 1;
    if (TREE_TYPE (result) == void_type_node)
      return result;
    return require_complete_type (result);
  }
}


/* Push a definition or a declaration of struct, union or enum tag "name".
   "type" should be the type node.
   We assume that the tag "name" is not already defined.

   Note that the definition may really be just a forward reference.
   In that case, the TYPE_SIZE will be zero.  */

/*
 *   --fixme-- does Modula-2 really need this?
 */
#if 0
void
pushtag (name, type)
     tree name, type;
{
  register struct binding_level *b;

  /* Find the proper binding level for this type tag.  */

  for (b = current_binding_level; b->tag_transparent; b = b->level_chain)
    continue;

  if (name)
    {
      /* Record the identifier as the type's name if it has none.  */

      if (TYPE_NAME (type) == 0)
	TYPE_NAME (type) = name;
    }

  if (b == global_binding_level)
    b->tags = perm_tree_cons (name, type, b->tags);
  else
    b->tags = saveable_tree_cons (name, type, b->tags);

  /* Create a fake NULL-named TYPE_DECL node whose TREE_TYPE will be the
     tagged type we just added to the current binding level.  This fake
     NULL-named TYPE_DECL node helps dwarfout.c to know when it needs
     to output a representation of a tagged type, and it also gives
     us a convenient place to record the "scope start" address for the
     tagged type.  */

  TYPE_STUB_DECL (type) = pushdecl (build_decl (TYPE_DECL, NULL_TREE, type));

  /* An approximation for now, so we can tell this is a function-scope tag.
     This will be updated in poplevel.  */
  TYPE_CONTEXT (type) = DECL_CONTEXT (TYPE_STUB_DECL (type));
}
#endif

#if 0
/* taken from c-decl.c */

/* Begin compiling the definition of an enumeration type.
   NAME is its name (or null if anonymous).
   Returns the type object, as yet incomplete.
   Also records info about it so that build_enumerator
   may be used to declare the individual values as they are read.  */

tree
start_enum (name)
     tree name;
{
  tree enumtype = 0;

  /* If this is the real definition for a previous forward reference,
     fill in the contents in the same object that used to be the
     forward reference.  */

  if (name != 0)
    enumtype = lookup_tag (ENUMERAL_TYPE, name, current_binding_level, 1);

  /* The corresponding pop_obstacks is in finish_enum.  */
  push_obstacks_nochange ();
  /* If these symbols and types are global, make them permanent.  */
  if (current_binding_level == global_binding_level)
    end_temporary_allocation ();

  if (enumtype == 0 || TREE_CODE (enumtype) != ENUMERAL_TYPE)
    {
      enumtype = make_node (ENUMERAL_TYPE);
      pushtag (name, enumtype);
    }

  C_TYPE_BEING_DEFINED (enumtype) = 1;

  if (TYPE_VALUES (enumtype) != 0)
    {
      /* This enum is a named one that has been declared already.  */
      error ("redeclaration of `enum %s'", IDENTIFIER_POINTER (name));

      /* Completely replace its old definition.
	 The old enumerators remain defined, however.  */
      TYPE_VALUES (enumtype) = 0;
    }

  enum_next_value = integer_zero_node;
  enum_overflow = 0;

  if (flag_short_enums)
    TYPE_PACKED (enumtype) = 1;

  return enumtype;
}
#endif


/* Push a definition or a declaration of struct, union or enum tag "name".
   "type" should be the type node.
   We assume that the tag "name" is not already defined.

   Note that the definition may really be just a forward reference.
   In that case, the TYPE_SIZE will be zero.  */

void
pushtag (name, type)
     tree name, type;
{
  register struct binding_level *b;

  /* Find the proper binding level for this type tag.  */

  for (b = current_binding_level; b->tag_transparent; b = b->level_chain)
    continue;

  if (name)
    {
      /* Record the identifier as the type's name if it has none.  */

      if (TYPE_NAME (type) == 0)
	TYPE_NAME (type) = name;
    }

  if (b == global_binding_level)
    b->tags = perm_tree_cons (name, type, b->tags);
  else
    b->tags = saveable_tree_cons (name, type, b->tags);

  /* Create a fake NULL-named TYPE_DECL node whose TREE_TYPE will be the
     tagged type we just added to the current binding level.  This fake
     NULL-named TYPE_DECL node helps dwarfout.c to know when it needs
     to output a representation of a tagged type, and it also gives
     us a convenient place to record the "scope start" address for the
     tagged type.  */

  TYPE_STUB_DECL (type) = pushdecl (build_decl (TYPE_DECL, NULL_TREE, type));

  /* An approximation for now, so we can tell this is a function-scope tag.
     This will be updated in poplevel.  */
  TYPE_CONTEXT (type) = DECL_CONTEXT (TYPE_STUB_DECL (type));
}


/* Make sure that the tag NAME is defined *in the current binding level*
   at least as a forward reference.
   CODE says which kind of tag NAME ought to be.

   We also do a push_obstacks_nochange
   whose matching pop is in finish_struct.  */

tree
start_struct (code, name)
     enum tree_code code;
     tree name;
{
  /* If there is already a tag defined at this binding level
     (as a forward reference), just return it.  */

  register tree ref = 0;

  push_obstacks_nochange ();

  if (current_binding_level == global_binding_level)
    end_temporary_allocation ();

#if NOT_NEEDED_FOR_M2
  if (name != 0)
    ref = lookup_tag (code, name, current_binding_level, 1);
#endif

  if (ref && TREE_CODE (ref) == code)
    {
      C_TYPE_BEING_DEFINED (ref) = 1;
      TYPE_PACKED (ref) = flag_pack_struct;
      if (TYPE_FIELDS (ref))
	error ((code == UNION_TYPE ? "redefinition of `union %s'"
		: "redefinition of `struct %s'"),
	       IDENTIFIER_POINTER (name));

      return ref;
    }

  /* Otherwise create a forward-reference just so the tag is in scope.  */

  ref = make_node (code);
  pushtag (name, ref);
  C_TYPE_BEING_DEFINED (ref) = 1;
  TYPE_PACKED (ref) = flag_pack_struct;
  return ref;
}


tree
gccgm2_BuildStartRecord (name)
     char *name;
{
  tree id;

  if ((name == NULL) || (strcmp(name, "") == 0)) {
    id = NULL_TREE;
  } else {
    id = get_identifier (name);
  }
  return( start_struct(RECORD_TYPE, id) );
}


tree
gccgm2_BuildStartVarientRecord (name)
     char *name;
{
  tree id;

  if ((name == NULL) || (strcmp(name, "") == 0)) {
    id = NULL_TREE;
  } else {
    id = get_identifier (name);
  }
  return( start_struct(UNION_TYPE, id) );
}


/* Lay out the type T, and its element type, and so on.  */

static void
layout_array_type (t)
     tree t;
{
  if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
    layout_array_type (TREE_TYPE (t));
  layout_type (t);
}


#if 0
static char *
get_name_from_decl (declarator)
     tree declarator;
{
  /* Look inside a declarator for the name being declared
     and get it as a string, for an error message.  */
  
  tree decl = declarator;
  char *name = 0;

  while (decl)
    switch (TREE_CODE (decl))
      {
      case ARRAY_REF:
      case INDIRECT_REF:
      case CALL_EXPR:
	decl = TREE_OPERAND (decl, 0);
	break;

      case IDENTIFIER_NODE:
	name = IDENTIFIER_POINTER (decl);
	decl = 0;
	break;

      default:
	abort ();
      }
  if (name == 0)
    name = "type name";
  return( name );
}

/* for modula-2 it is simpler to inline the single useful statement in BuildFieldRecord */
static tree
build_field(declarator)
     tree declarator;
{
  tree type = NULL_TREE;
  tree decl;

  push_obstacks_nochange ();

  if (flag_traditional && allocation_temporary_p ())
    end_temporary_allocation ();

  /* Structure field.  It may not be a function.  */
  if (TREE_CODE (type) == FUNCTION_TYPE)
    {
      error ("field `%s' declared as a function", get_name_from_decl(declarator));
      type = build_pointer_type (type);
    }
  else if (TREE_CODE (type) != ERROR_MARK && TYPE_SIZE (type) == 0)
    {
      error ("field `%s' has incomplete type", get_name_from_decl(declarator));
      type = error_mark_node;
    }
  decl = build_decl (FIELD_DECL, declarator, type);
  pop_obstacks ();

  return decl;
}
#endif


/* Build a record field with name (name maybe NULL),
   returning the new field declaration, FIELD_DECL.

   This is done during the parsing of the struct declaration.
   The FIELD_DECL nodes are chained together and the lot of them
   are ultimately passed to `build_struct' to make the RECORD_TYPE node.  */

tree
gccgm2_BuildFieldRecord (name, type)
     char *name;
     tree type;
{
  tree field, declarator;
  
  if ((name == NULL) || (strcmp(name, "") == 0)) {
    declarator = NULL_TREE;
  } else {
    declarator = get_identifier (name);
  }

  /* The corresponding pop_obstacks is in finish_decl.  */
  push_obstacks_nochange ();
  field = build_decl (FIELD_DECL, declarator, type);
  finish_decl (field, NULL_TREE, NULL_TREE);

  return field;
}


/*
 *  ChainOn - interface so that Modula-2 can also create chains of declarations.
 */

tree
gccgm2_ChainOn (t1, t2)
     tree t1, t2;
{
#if 1
  return( chainon(t1, t2) );
#else
  fprintf(stderr, "tree t1\n");
  debug_tree(t1);
  fprintf(stderr, "tree t2\n");
  debug_tree(t2);
  {
    tree t=chainon(t1, t2);

    debug_tree(t);
    return( t );
  }
#endif
}


/*
 *  ChainOnParamValue - adds a list node {parm, value} into the tree list.
 */

tree
gccgm2_ChainOnParamValue (list, parm, value)
     tree list, parm, value;
{
  return( chainon(list, build_tree_list(parm, value)) );
}

/*
 *  AddStringToTreeList - adds, string, to list.
 */

tree
gccgm2_AddStringToTreeList (list, string)
     tree list, string;
{
  return( tree_cons (NULL_TREE, string, list) );
}

/* Function to help qsort sort FIELD_DECLs by name order.  */

static int
field_decl_cmp (xp, yp)
     const void *xp;
     const void *yp;
{
  tree *x = (tree *)xp, *y = (tree *)yp;

  if (DECL_NAME (*x) == DECL_NAME (*y))
    return 0;
  if (DECL_NAME (*x) == NULL)
    return -1;
  if (DECL_NAME (*y) == NULL)
    return 1;
  if (DECL_NAME (*x) < DECL_NAME (*y))
    return -1;
  return 1;
}

/* Fill in the fields of a RECORD_TYPE or UNION_TYPE node, T.
   FIELDLIST is a chain of FIELD_DECL nodes for the fields.
   ATTRIBUTES are attributes to be applied to the structure.

   We also do a pop_obstacks to match the push in BuildStartRecord.  */

tree
gccgm2_BuildEndRecord (t, fieldlist /* , attributes */ )
     tree t;
     tree fieldlist;
{
  tree x;
  int old_momentary;
  int toplevel = global_binding_level == current_binding_level;

  /* If this type was previously laid out as a forward reference,
     make sure we lay it out again.  */

  TYPE_SIZE (t) = 0;

#if NOT_NEEDED_FOR_M2
  decl_attributes (t, attributes, NULL_TREE);

  /* Nameless union parm types are useful as GCC extension.  */
  if (! (TREE_CODE (t) == UNION_TYPE && TYPE_NAME (t) == 0) && !pedantic)
    /* Otherwise, warn about any struct or union def. in parmlist.  */
    if (in_parm_level_p ())
      {
	if (pedantic)
	  pedwarn ((TREE_CODE (t) == UNION_TYPE ? "union defined inside parms"
		    : "structure defined inside parms"));
	else if (! flag_traditional)
	  warning ((TREE_CODE (t) == UNION_TYPE ? "union defined inside parms"
		    : "structure defined inside parms"));
      }
#endif

  old_momentary = suspend_momentary ();

  if (pedantic)
    {
      for (x = fieldlist; x; x = TREE_CHAIN (x))
	if (DECL_NAME (x) != 0)
	  break;

      if (x == 0)
	pedwarn ("%s has no %smembers",
		 (TREE_CODE (t) == UNION_TYPE ? "union" : "structure"),
		 (fieldlist ? "named " : ""));
    }

  /* Install struct as DECL_CONTEXT of each field decl.
     Also process specified field sizes.
     Set DECL_FIELD_SIZE to the specified size, or 0 if none specified.
     The specified size is found in the DECL_INITIAL.
     Store 0 there, except for ": 0" fields (so we can find them
     and delete them, below).  */

  for (x = fieldlist; x; x = TREE_CHAIN (x))
    {
      DECL_CONTEXT (x) = t;
      DECL_PACKED (x) |= TYPE_PACKED (t);
      DECL_FIELD_SIZE (x) = 0;

      /* If any field is const, the structure type is pseudo-const.  */
      if (TREE_READONLY (x))
	C_TYPE_FIELDS_READONLY (t) = 1;
      else
	{
	  /* A field that is pseudo-const makes the structure likewise.  */
	  tree t1 = TREE_TYPE (x);
	  while (TREE_CODE (t1) == ARRAY_TYPE)
	    t1 = TREE_TYPE (t1);
	  if ((TREE_CODE (t1) == RECORD_TYPE || TREE_CODE (t1) == UNION_TYPE)
	      && C_TYPE_FIELDS_READONLY (t1))
	    C_TYPE_FIELDS_READONLY (t) = 1;
	}

      /* Any field that is volatile means variables of this type must be
	 treated in some ways as volatile.  */
      if (TREE_THIS_VOLATILE (x))
	C_TYPE_FIELDS_VOLATILE (t) = 1;

      /* Any field of nominal variable size implies structure is too.  */
      if (C_DECL_VARIABLE_SIZE (x))
	C_TYPE_VARIABLE_SIZE (t) = 1;

      /* Detect invalid nested redefinition.  */
      if (TREE_TYPE (x) == t)
	error ("nested redefinition of `%s'",
	       IDENTIFIER_POINTER (TYPE_NAME (t)));

      /* Detect invalid bit-field size.  */
      if (DECL_INITIAL (x))
	STRIP_NOPS (DECL_INITIAL (x));
      if (DECL_INITIAL (x))
	{
	  if (TREE_CODE (DECL_INITIAL (x)) == INTEGER_CST)
	    constant_expression_warning (DECL_INITIAL (x));
	  else
	    {
	      error_with_decl (x, "bit-field `%s' width not an integer constant");
	      DECL_INITIAL (x) = NULL;
	    }
	}

      /* Detect invalid bit-field type.  */
      if (DECL_INITIAL (x)
	  && TREE_CODE (TREE_TYPE (x)) != INTEGER_TYPE
	  && TREE_CODE (TREE_TYPE (x)) != ENUMERAL_TYPE)
	{
	  error_with_decl (x, "bit-field `%s' has invalid type");
	  DECL_INITIAL (x) = NULL;
	}
      if (DECL_INITIAL (x) && pedantic
	  && TYPE_MAIN_VARIANT (TREE_TYPE (x)) != integer_type_node
	  && TYPE_MAIN_VARIANT (TREE_TYPE (x)) != unsigned_type_node
	  /* Accept an enum that's equivalent to int or unsigned int.  */
	  && !(TREE_CODE (TREE_TYPE (x)) == ENUMERAL_TYPE
	       && (TYPE_PRECISION (TREE_TYPE (x))
		   == TYPE_PRECISION (integer_type_node))))
	pedwarn_with_decl (x, "bit-field `%s' type invalid in ANSI C");

      /* Detect and ignore out of range field width.  */
      if (DECL_INITIAL (x))
	{
	  unsigned HOST_WIDE_INT width = TREE_INT_CST_LOW (DECL_INITIAL (x));

	  if (tree_int_cst_sgn (DECL_INITIAL (x)) < 0)
	    {
	      DECL_INITIAL (x) = NULL;
	      error_with_decl (x, "negative width in bit-field `%s'");
	    }
	  else if (TREE_INT_CST_HIGH (DECL_INITIAL (x)) != 0
		   || width > TYPE_PRECISION (TREE_TYPE (x)))
	    {
	      DECL_INITIAL (x) = NULL;
	      pedwarn_with_decl (x, "width of `%s' exceeds its type");
	    }
	  else if (width == 0 && DECL_NAME (x) != 0)
	    {
	      error_with_decl (x, "zero width for bit-field `%s'");
	      DECL_INITIAL (x) = NULL;
	    }
	}

      /* Process valid field width.  */
      if (DECL_INITIAL (x))
	{
	  register int width = TREE_INT_CST_LOW (DECL_INITIAL (x));

	  if (TREE_CODE (TREE_TYPE (x)) == ENUMERAL_TYPE
	      && (width < min_precision (TYPE_MIN_VALUE (TREE_TYPE (x)),
					 TREE_UNSIGNED (TREE_TYPE (x)))
		  || width < min_precision (TYPE_MAX_VALUE (TREE_TYPE (x)),
					    TREE_UNSIGNED (TREE_TYPE (x)))))
	    warning_with_decl (x, "`%s' is narrower than values of its type");

	  DECL_FIELD_SIZE (x) = width;
	  DECL_BIT_FIELD (x) = DECL_C_BIT_FIELD (x) = 1;
	  DECL_INITIAL (x) = NULL;

	  if (width == 0)
	    {
	      /* field size 0 => force desired amount of alignment.  */
#ifdef EMPTY_FIELD_BOUNDARY
	      DECL_ALIGN (x) = MAX (DECL_ALIGN (x), EMPTY_FIELD_BOUNDARY);
#endif
#ifdef PCC_BITFIELD_TYPE_MATTERS
	      if (PCC_BITFIELD_TYPE_MATTERS)
		DECL_ALIGN (x) = MAX (DECL_ALIGN (x),
				      TYPE_ALIGN (TREE_TYPE (x)));
#endif
	    }
	}
      else if (TREE_TYPE (x) != error_mark_node)
	{
	  unsigned int min_align = (DECL_PACKED (x) ? BITS_PER_UNIT
			   : TYPE_ALIGN (TREE_TYPE (x)));
	  /* Non-bit-fields are aligned for their type, except packed
	     fields which require only BITS_PER_UNIT alignment.  */
	  DECL_ALIGN (x) = MAX (DECL_ALIGN (x), min_align);
	}
    }

  /* Now DECL_INITIAL is null on all members.  */

  /* Delete all duplicate fields from the fieldlist */
  for (x = fieldlist; x && TREE_CHAIN (x);)
    /* Anonymous fields aren't duplicates.  */
    if (DECL_NAME (TREE_CHAIN (x)) == 0)
      x = TREE_CHAIN (x);
    else
      {
	register tree y = fieldlist;
	  
	while (1)
	  {
	    if (DECL_NAME (y) == DECL_NAME (TREE_CHAIN (x)))
	      break;
	    if (y == x)
	      break;
	    y = TREE_CHAIN (y);
	  }
	if (DECL_NAME (y) == DECL_NAME (TREE_CHAIN (x)))
	  {
	    error_with_decl (TREE_CHAIN (x), "duplicate member `%s'");
	    TREE_CHAIN (x) = TREE_CHAIN (TREE_CHAIN (x));
	  }
	else x = TREE_CHAIN (x);
      }

  /* Now we have the nearly final fieldlist.  Record it,
     then lay out the structure or union (including the fields).  */

  TYPE_FIELDS (t) = fieldlist;

  layout_type (t);

  /* Delete all zero-width bit-fields from the front of the fieldlist */
  while (fieldlist
	 && DECL_INITIAL (fieldlist))
    fieldlist = TREE_CHAIN (fieldlist);
  /* Delete all such members from the rest of the fieldlist */
  for (x = fieldlist; x;)
    {
      if (TREE_CHAIN (x) && DECL_INITIAL (TREE_CHAIN (x)))
	TREE_CHAIN (x) = TREE_CHAIN (TREE_CHAIN (x));
      else x = TREE_CHAIN (x);
    }

  /*  Now we have the truly final field list.
      Store it in this type and in the variants.  */

  TYPE_FIELDS (t) = fieldlist;

  /* If there are lots of fields, sort so we can look through them fast.
     We arbitrarily consider 16 or more elts to be "a lot".  */
  {
    int len = 0;

    for (x = fieldlist; x; x = TREE_CHAIN (x))
      {
	if (len > 15)
	  break;
	len += 1;
      }
    if (len > 15)
      {
	tree *field_array;
	char *space;

	len += list_length (x);
	/* Use the same allocation policy here that make_node uses, to
	   ensure that this lives as long as the rest of the struct decl.
	   All decls in an inline function need to be saved.  */
	if (allocation_temporary_p ())
	  space = savealloc (sizeof (struct lang_type) + len * sizeof (tree));
	else
	  space = oballoc (sizeof (struct lang_type) + len * sizeof (tree));

	TYPE_LANG_SPECIFIC (t) = (struct lang_type *) space;
	TYPE_LANG_SPECIFIC (t)->len = len;

	field_array = &TYPE_LANG_SPECIFIC (t)->elts[0];
	len = 0;
	for (x = fieldlist; x; x = TREE_CHAIN (x))
	  field_array[len++] = x;

	qsort (field_array, len, sizeof (tree), field_decl_cmp);
      }
  }

  for (x = TYPE_MAIN_VARIANT (t); x; x = TYPE_NEXT_VARIANT (x))
    {
      TYPE_FIELDS (x) = TYPE_FIELDS (t);
      TYPE_LANG_SPECIFIC (x) = TYPE_LANG_SPECIFIC (t);
      TYPE_ALIGN (x) = TYPE_ALIGN (t);
    }

  /* If this was supposed to be a transparent union, but we can't
     make it one, warn and turn off the flag.  */
  if (TREE_CODE (t) == UNION_TYPE
      && TYPE_TRANSPARENT_UNION (t)
      && TYPE_MODE (t) != DECL_MODE (TYPE_FIELDS (t)))
    {
      TYPE_TRANSPARENT_UNION (t) = 0;
      warning ("union cannot be made transparent");
    }

  /* If this structure or union completes the type of any previous
     variable declaration, lay it out and output its rtl.  */

  if (current_binding_level->n_incomplete != 0)
    {
      tree decl;
      for (decl = current_binding_level->names; decl; decl = TREE_CHAIN (decl))
	{
	  if (TREE_TYPE (decl) == t
	      && TREE_CODE (decl) != TYPE_DECL)
	    {
	      layout_decl (decl, 0);
	      /* This is a no-op in c-lang.c or something real in objc-actions.c.  */
#if NOT_NEEDED_FOR_M2
	      maybe_objc_check_decl (decl);
#endif
	      rest_of_decl_compilation (decl, NULL_PTR, toplevel, 0);
	      if (! toplevel)
		expand_decl (decl);
	      --current_binding_level->n_incomplete;
	    }
	  else if (TYPE_SIZE (TREE_TYPE (decl)) == 0
		   && TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
	    {
	      tree element = TREE_TYPE (decl);
	      while (TREE_CODE (element) == ARRAY_TYPE)
		element = TREE_TYPE (element);
	      if (element == t)
		layout_array_type (TREE_TYPE (decl));
	    }
	}
    }

  resume_momentary (old_momentary);

  /* Finish debugging output for this type.  */
  rest_of_type_compilation (t, toplevel);

  /* The matching push is in start_struct.  */
  pop_obstacks ();

  return t;
}


/* Begin compiling the definition of an enumeration type.
   NAME is its name (or null if anonymous).
   Returns the type object, as yet incomplete.
   Also records info about it so that build_enumerator
   may be used to declare the individual values as they are read.  */

tree
start_enum (name)
     tree name;
{
  tree enumtype = 0;

  /* If this is the real definition for a previous forward reference,
     fill in the contents in the same object that used to be the
     forward reference.  */

#if NOT_NEEDED_FOR_M2
  if (name != 0)
    enumtype = lookup_tag (ENUMERAL_TYPE, name, current_binding_level, 1);
#endif

  /* The corresponding pop_obstacks is in finish_enum.  */
  push_obstacks_nochange ();
  /* If these symbols and types are global, make them permanent.  */
  if (current_binding_level == global_binding_level)
    end_temporary_allocation ();

  if (enumtype == 0 || TREE_CODE (enumtype) != ENUMERAL_TYPE)
    {
      enumtype = make_node (ENUMERAL_TYPE);
      pushtag (name, enumtype);
    }

#if NOT_NEEDED_FOR_M2
  C_TYPE_BEING_DEFINED (enumtype) = 1;
#endif

  if (TYPE_VALUES (enumtype) != 0)
    {
      /* This enum is a named one that has been declared already.  */
      error ("redeclaration of `enum %s'", IDENTIFIER_POINTER (name));

      /* Completely replace its old definition.
	 The old enumerators remain defined, however.  */
      TYPE_VALUES (enumtype) = 0;
    }

  enum_next_value = integer_zero_node;

  if (flag_short_enums)
    TYPE_PACKED (enumtype) = 1;

  TREE_TYPE(enumtype) = gccgm2_GetIntegerType();  /* gaius added this, hope it is ok */

  return enumtype;
}

/* Return the minimum number of bits needed to represent VALUE in a
   signed or unsigned type, UNSIGNEDP says which.  */

int
min_precision (value, unsignedp)
     tree value;
     int unsignedp;
{
  int log;

  /* If the value is negative, compute its negative minus 1.  The latter
     adjustment is because the absolute value of the largest negative value
     is one larger than the largest positive value.  This is equivalent to
     a bit-wise negation, so use that operation instead.  */

  if (tree_int_cst_sgn (value) < 0)
    value = fold (build1 (BIT_NOT_EXPR, TREE_TYPE (value), value));

  /* Return the number of bits needed, taking into account the fact
     that we need one more bit for a signed than unsigned type.  */

  if (integer_zerop (value))
    log = 0;
  else if (TREE_INT_CST_HIGH (value) != 0)
    log = HOST_BITS_PER_WIDE_INT + floor_log2 (TREE_INT_CST_HIGH (value));
  else
    log = floor_log2 (TREE_INT_CST_LOW (value));

  return log + 1 + ! unsignedp;
}

/* After processing and defining all the values of an enumeration type,
   install their decls in the enumeration type and finish it off.
   ENUMTYPE is the type object, VALUES a list of decl-value pairs,
   and ATTRIBUTES are the specified attributes.
   Returns ENUMTYPE.  */

tree
finish_enum (enumtype, values, attributes)
     tree enumtype;
     tree values;
     tree attributes ATTRIBUTE_UNUSED;
{
  register tree pair, tem;
  tree minnode = 0, maxnode = 0;
  int lowprec, highprec, precision;
  int toplevel = global_binding_level == current_binding_level;

#if NOT_NEEDED_FOR_M2
  if (in_parm_level_p ())
    warning ("enum defined inside parms");

  decl_attributes (enumtype, attributes, NULL_TREE);
#endif

  /* Calculate the maximum value of any enumerator in this type.  */

  if (values == error_mark_node)
    minnode = maxnode = integer_zero_node;
  else
    for (pair = values; pair; pair = TREE_CHAIN (pair))
      {
	tree value = TREE_VALUE (pair);
	if (pair == values)
	  minnode = maxnode = TREE_VALUE (pair);
	else
	  {
	    if (tree_int_cst_lt (maxnode, value))
	      maxnode = value;
	    if (tree_int_cst_lt (value, minnode))
	      minnode = value;
	  }
      }

  TYPE_MIN_VALUE (enumtype) = minnode;
  TYPE_MAX_VALUE (enumtype) = maxnode;

  /* An enum can have some negative values; then it is signed.  */
  TREE_UNSIGNED (enumtype) = tree_int_cst_sgn (minnode) >= 0;

  /* Determine the precision this type needs.  */

  lowprec = min_precision (minnode, TREE_UNSIGNED (enumtype));
  highprec = min_precision (maxnode, TREE_UNSIGNED (enumtype));
  precision = MAX (lowprec, highprec);

  TYPE_PRECISION (enumtype) = TYPE_PRECISION (integer_type_node);

  TYPE_SIZE (enumtype) = 0;
#ifdef GPC
#ifdef EGCS
  TYPE_SIZE_UNIT (enumtype) = 0;
#endif /* GPC */
#endif /* EGCS */
  layout_type (enumtype);

  if (values != error_mark_node)
    {
      /* Change the type of the enumerators to be the enum type.
	 Formerly this was done only for enums that fit in an int,
	 but the comment said it was done only for enums wider than int.
	 It seems necessary to do this for wide enums,
	 and best not to change what's done for ordinary narrower ones.  */
      for (pair = values; pair; pair = TREE_CHAIN (pair))
	{
	  TREE_TYPE (TREE_PURPOSE (pair)) = enumtype;
	  DECL_SIZE (TREE_PURPOSE (pair)) = TYPE_SIZE (enumtype);
	  if (TREE_CODE (TREE_PURPOSE (pair)) != FUNCTION_DECL)
	    DECL_ALIGN (TREE_PURPOSE (pair)) = TYPE_ALIGN (enumtype);
	}

      /* Replace the decl nodes in VALUES with their names.  */
      for (pair = values; pair; pair = TREE_CHAIN (pair))
	TREE_PURPOSE (pair) = DECL_NAME (TREE_PURPOSE (pair));

      TYPE_VALUES (enumtype) = values;
    }

  /* Fix up all variant types of this enum type.  */
  for (tem = TYPE_MAIN_VARIANT (enumtype); tem; tem = TYPE_NEXT_VARIANT (tem))
    {
      TYPE_VALUES (tem) = TYPE_VALUES (enumtype);
      TYPE_MIN_VALUE (tem) = TYPE_MIN_VALUE (enumtype);
      TYPE_MAX_VALUE (tem) = TYPE_MAX_VALUE (enumtype);
      TYPE_SIZE (tem) = TYPE_SIZE (enumtype);
#ifdef GPC
#ifdef EGCS
      TYPE_SIZE_UNIT (tem) = TYPE_SIZE_UNIT (enumtype);
#endif EGCS
#endif /* GPC */
      TYPE_MODE (tem) = TYPE_MODE (enumtype);
      TYPE_PRECISION (tem) = TYPE_PRECISION (enumtype);
      TYPE_ALIGN (tem) = TYPE_ALIGN (enumtype);
      TREE_UNSIGNED (tem) = TREE_UNSIGNED (enumtype);
    }

  /* Finish debugging output for this type.  */
  rest_of_type_compilation (enumtype, toplevel);

  /* This matches a push in start_enum.  */
  pop_obstacks ();

  return enumtype;
}

/* Build and install a CONST_DECL for one value of the
   current enumeration type (one that was begun with start_enum).
   Return a tree-list containing the CONST_DECL and its value.
   Assignment of sequential values by default is handled here.  */

tree
build_enumerator (name, value)
     tree name, value;
{
  tree decl, type;

  /* Validate and default VALUE.  */

  /* Remove no-op casts from the value.  */
  if (value)
    STRIP_TYPE_NOPS (value);

  if (value != 0)
    {
      if (TREE_CODE (value) == INTEGER_CST)
	{
	  value = default_conversion (value);
	  constant_expression_warning (value);
	}
      else
	{
	  error ("enumerator value for `%s' not integer constant",
		 IDENTIFIER_POINTER (name));
	  value = 0;
	}
    }

  /* Default based on previous value.  */
  /* It should no longer be possible to have NON_LVALUE_EXPR
     in the default.  */
  if (value == 0)
    {
      value = enum_next_value;
    }

  if (pedantic && ! int_fits_type_p (value, integer_type_node))
    {
      pedwarn ("ANSI C restricts enumerator values to range of `int'");
      value = integer_zero_node;
    }

  /* Set basis for default for next value.  */
  enum_next_value = build_binary_op (PLUS_EXPR, value, integer_one_node, 0);
#if 0
  enum_overflow = tree_int_cst_lt (enum_next_value, value);
#endif

  /* Now create a declaration for the enum value name.  */

  type = TREE_TYPE (value);
  type = type_for_size (MAX (TYPE_PRECISION (type),
			     TYPE_PRECISION (integer_type_node)),
			((flag_traditional
			  || TYPE_PRECISION (type) >= TYPE_PRECISION (integer_type_node))
			 && TREE_UNSIGNED (type)));

  decl = build_decl (CONST_DECL, name, type);
#ifdef GPC
  /* @@@? DO NOT change the type of value */
  DECL_INITIAL (decl) = copy_node (value);
  TREE_TYPE (DECL_INITIAL (decl)) = current_enum_type;
#else
  DECL_INITIAL (decl) = value;
  TREE_TYPE (value) = type;
#endif /* GPC */
  pushdecl (decl);

  return saveable_tree_cons (decl, value, NULL_TREE);
}


/*
 *  ExpandExpressionStatement - maps onto expand_expr_stmt in stmt.c
 */

void
gccgm2_ExpandExpressionStatement (t)
     tree t;
{
  expand_expr_stmt(t);
}


/*
 * Local variables:
 *  compile-command: "gcc -c  -DIN_GCC    -g -Wall -Wtraditional     -I. -I.. -I. -I./.. -I./../config -I./../../include gm2.c"
 * End:
 */
