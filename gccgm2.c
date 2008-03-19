/* Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007
 * Free Software Foundation, Inc.
 *
 *  Gaius Mulley <gaius@glam.ac.uk> constructed this file.
 *  It was built by borrowing code from the gcc-.../gcc/c-*.c files
 *  and its function is to provide an interface between the
 *  Modula-2 source and C back end.
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

/*
 * IMPLEMENTATION MODULE gccgm2
 *     thus all external functions will be prefixed by gccgm2_
 *     which will allow us to construct a DEFINITION MODULE
 *     so that the Modula-2 source code front end can interface to gcc.
 *      
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

#include <stdio.h>

#undef DEBUG_PROCEDURE_CALLS
static int broken_set_debugging_info = TRUE;
static int insideCppArgs = FALSE;

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
#include "gm2-tree.h"
#include "m2pp.h"

#define GM2
#define GM2_BUG_REPORT "Please report this crash to the GNU Modula-2 mailing list <gm2@nongnu.org>\n"

#define ASSERT(X,Y)   { if (!(X)) { debug_tree(Y); internal_error("[%s:%d]:condition `%s' failed", \
                                                                   __FILE__, __LINE__, #X); } }


#define ASSERT_BOOL(X)   { if ((X != 0) && (X != 1)) { internal_error("[%s:%d]:the value `%s' is not a BOOLEAN, it's value was %d", \
                                                                   __FILE__, __LINE__, #X, X); } }
#define ASSERT_CONDITION(X)   { if (!(X)) { internal_error("[%s:%d]:condition `%s' failed", \
                                                                   __FILE__, __LINE__, #X); } }

#define USE_BOOLEAN

enum attrs {A_PACKED, A_NOCOMMON, A_COMMON, A_NORETURN, A_CONST, A_T_UNION,
            A_CONSTRUCTOR, A_DESTRUCTOR, A_MODE, A_SECTION, A_ALIGNED,
            A_UNUSED, A_FORMAT, A_FORMAT_ARG, A_WEAK, A_ALIAS};

/*
 *  and define some type sizes
 */

#ifndef SET_WORD_SIZE
/* gross hack */
#define SET_WORD_SIZE INT_TYPE_SIZE
#endif

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

/* taken from decl.c */
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

static GTY(()) tree proc_type_node;
static GTY(()) tree bitset_type_node;
static GTY(()) tree bitnum_type_node;
static GTY(()) tree m2_char_type_node;
static GTY(()) tree m2_integer_type_node;
static GTY(()) tree m2_cardinal_type_node;
static GTY(()) tree m2_short_real_type_node;
static GTY(()) tree m2_real_type_node;
static GTY(()) tree m2_long_real_type_node;
static GTY(()) tree m2_long_int_type_node;
static GTY(()) tree m2_long_card_type_node;
static GTY(()) tree m2_short_int_type_node;
static GTY(()) tree m2_short_card_type_node;
static GTY(()) tree m2_z_type_node;
static GTY(()) tree m2_iso_loc_type_node;
static GTY(()) tree m2_iso_byte_type_node;
static GTY(()) tree m2_iso_word_type_node;
static GTY(()) tree set_full_complement;

/* The current statement tree.  */
static GTY(()) struct stmt_tree_s c_stmt_tree;

/* Global Variables for the various types and nodes we create.  */ 

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

extern GTY(()) tree current_function_decl;


/* While defining an enum type, this is 1 plus the last enumerator
   constant value.  Note that will do not have to save this or `enum_overflow'
   around nested function definition since such a definition could only
   occur in an enum value expression and we don't use these variables in
   that case.  */

static GTY(()) tree enum_next_value;

/* Nonzero means that there was overflow computing enum_next_value.  */

static int enum_overflow;

/* Used in build_enumerator() */
static GTY(()) tree current_enum_type;

/* Used in BuildEnumerator */
static GTY(()) tree enumvalues=NULL_TREE;

/* Used in BuildStartFunctionType */
static GTY(()) tree param_type_list;
static GTY(()) tree param_list = NULL_TREE;   /* ready for the next time we call/define a function */
static GTY(()) tree last_function=NULL_TREE;
static int label_count = 0;

/* A list (chain of TREE_LIST nodes) of all LABEL_DECLs in the function
   that have names.  Here so we can clear out their names' definitions
   at the end of the function. */

static GTY(()) tree named_labels;

/* A list of LABEL_DECLs from outer contexts that are currently shadowed. */

static GTY(()) tree shadowed_labels;

tree c_global_trees[CTI_MAX];

enum c_language_kind c_language = clk_c;

static tree qualify_type                PARAMS ((tree, tree));
static tree build_c_type_variant        PARAMS ((tree, int, int));

/* Nonzero means allow type mismatches in conditional expressions;
   just make their values `void'.   */

int flag_cond_mismatch=1;

/* Nonzero means the expression being parsed will never be evaluated.
   This is a count, since unevaluated expressions can nest.  */
int skip_evaluation=0;

/* Nonzero means give `double' the same size as `float'.  */

int flag_short_double=0;

/* Nonzero means to allow single precision math even if we're generally
   being traditional.  */
int flag_allow_single_precision = 0;

/* Nonzero whenever Objective-C functionality is being used.  */
int flag_objc = 0;

/* by default this is 1 but the Modula-2 front end turns it off
   when generating runtime checking, so we dont get warnings about
   testing cardinal values < 0 */
int want_warnings = 0;


/* arguments given to compiler */

static unsigned int gm2_argc;
static const char **gm2_argv;

extern void gccgm2front                     PARAMS((unsigned int argc, const char *argv[]));
extern void gm2builtins_init                PARAMS((void));
extern tree gm2builtins_BuiltInHugeVal      PARAMS((void));
extern tree gm2builtins_BuiltInHugeValShort PARAMS((void));
extern tree gm2builtins_BuiltInHugeValLong  PARAMS((void));

/* Global Variables Expected by gcc: */

int flag_traditional=FALSE;     /* Used by dwarfout.c.  */

/* Possible cases of implicit bad conversions.  Used to select
   diagnostic messages in convert_for_assignment.  */
enum impl_conv {
  ic_argpass,
  ic_argpass_nonproto,
  ic_assign,
  ic_init,
  ic_return
};


/* function prototypes */
#define EXTERN 
#include "gm2-common.h"
#undef EXTERN

#define EXTERN extern
#include "gm2-lang.h"
#undef EXTERN


struct struct_constructor GTY(())
{
  /*
   *  constructor_type, the type that we are constructing
   */
  tree constructor_type;
  /*
   *  constructor_fields, the list of fields belonging to
   *  constructor_type.  Used by SET and RECORD constructors.
   */
  tree constructor_fields;
  /*
   *  constructor_element_list, the list of constants
   *  used by SET and RECORD constructors.
   */
  tree constructor_element_list;
  /*
   *  constructor_elements, used by an ARRAY initializer all
   *  elements are held in reverse order.
   */
  VEC(constructor_elt,gc) *constructor_elements;
  /*
   *  level, the next level down in the constructor stack.
   */
  struct struct_constructor *level;
};

static GTY(()) struct struct_constructor *top_constructor = NULL;

  
tree                   finish_enum                                (tree, tree, tree);
void                   gccgm2_EndTemporaryAllocation              (void);
void                   gccgm2_ResumeTemporaryAllocation           (void);
void                   gccgm2_SetFileNameAndLineNo                (char *fn, int line);
void                   gccgm2_EmitLineNote                        (char *fn, int line);
static int             is_integer                                 (enum tree_code code);
static int             is_pointer                                 (enum tree_code code);
int                    kept_level_p                               (void);
int                    in_parm_level_p                            (void);
tree                   define_label                               (location_t location, tree name);
void                   delete_block                               (tree block);
tree                   lookup_name                                (tree name);
tree                   lookup_name_current_level                  (tree name);
tree                   pushdecl_top_level                         (tree x);
void                   lang_mark_tree                             (tree t);
void                   init_m2_builtins                           (void);
void                   lang_finish                                (void);
const char            *init_parse                                 (const char *filename);
void                   finish_parse                               (void);
const char            *lang_identify                              (void);
void                   print_lang_statistics                      (void);
void                   init_lex                                   (void);
void                   yyerror                                    (char *str);
static tree            build_c_type_variant                       (tree type, int constp, int volatilep);
static tree            qualify_type                               (tree type, tree like);
tree                   common_type                                (tree t1, tree t2);
tree                   base_type                                  (tree type);
int                    comptypes                                  (tree type1, tree type2);
void                   readonly_warning                                   (tree arg, const char *string);
int                    is_string_type                             (tree string, int warn);
static int             lvalue_p                                   (tree ref);
static int             lvalue_or_else                             (tree, enum lvalue_use);
tree                   default_conversion                         (tree exp);
static int             comp_target_types                          (tree, tree);
tree                   convert_set                                (tree type, tree expr);
tree                   convert                                    (tree type, tree expr);
tree                   gccgm2_BuildCoerce                         (tree op1, tree op2, tree op3);
void                   constant_expression_warning                (tree value);
void                   overflow_warning                           (tree value);
void                   gm2_unsigned_conversion_error              (tree result, tree operand, tree totype);
tree                   convert_and_check                          (tree type, tree expr);
tree                   build_conditional_expr                     (tree ifexp, tree op1, tree op2);
tree                   shorten_compare                            (tree *op0_ptr, tree *op1_ptr,
                                                                   tree *restype_ptr,
                                                                   enum tree_code *rescode_ptr);
tree                   require_complete_type                      (tree value);
static tree            convert_for_assignment                     (tree, tree, enum impl_conv, tree, tree, int);
tree                   build_modify_expr                          (tree lhs, enum tree_code modifycode, tree rhs);
tree                   c_build_qualified_type                     (tree type, int type_quals);
tree                   build_unary_op                             (enum tree_code code, tree xarg, int noconvert);
void                   binary_op_error                            (enum tree_code code);
tree                   build_binary_op                            (enum tree_code code, tree orig_op0,
                                                                   tree orig_op1, int convert_p);
tree                   gccgm2_DeclareKnownType                    (char *name, tree type);
tree                   skip_type_decl                             (tree type);
tree                   skip_const_decl                            (tree exp);
tree                   gccgm2_DeclareKnownVariable                (char *, tree, int, int, int, int, tree);

tree                   gccgm2_GetMinFrom                          (tree type);
tree                   gccgm2_GetMaxFrom                          (tree type);
int                    gccgm2_GetBitsPerWord                      (void);
int                    gccgm2_GetBitsPerUnit                      (void);
int                    gccgm2_GetBitsPerInt                       (void);
int                    gccgm2_GetBitsPerBitset                    (void);
void                   trythis                                    (void);
tree                   gccgm2_DeclareKnownConstant                (tree type, tree value);
void                   finish_decl                                (tree decl, tree init, tree asmspec_tree);
tree                   gccgm2_BuildStartEnumeration               (char *name);
tree                   gccgm2_BuildEndEnumeration                 (tree enumtype);
tree                   gccgm2_BuildEnumerator                     (char *name, tree value);
tree                   gccgm2_BuildPointerType                    (tree totype);
tree                   gccgm2_BuildArrayType                      (tree elementtype, tree indextype);
tree                   build_set_type                             (tree domain, tree type, int allow_void);
tree                   gccgm2_BuildSetTypeFromSubrange            (char *, tree, tree, tree);
tree                   gccgm2_BuildSetType                        (char *name, tree type, tree lowval, tree highval);
tree                   gccgm2_BuildSubrangeType                   (char *name, tree type, tree lowval, tree highval);
tree                   gccgm2_BuildArrayIndexType                 (tree low, tree high);
tree                   gccgm2_BuildVariableArrayAndDeclare        (tree elementtype, tree high, char *name, tree scope);
tree                   gccgm2_BuildStartFunctionType              (char *name);
tree                   gccgm2_BuildEndFunctionType                (tree func, tree type);
static tree            finish_build_pointer_type                  (tree t, tree to_type, enum machine_mode mode, bool can_alias_all);

tree                   gccgm2_BuildParameterDeclaration           (char *name, tree type, int isreference);
tree                   completeParameterDeclaration               (char *name, tree actual_type, tree parm_type);
tree                   gccgm2_BuildParameterDeclaration           (char *name, tree type, int isreference);
void                   gccgm2_BuildStartFunctionDeclaration       (int uses_varargs);
tree                   gccgm2_BuildEndFunctionDeclaration         (char *name, tree returntype,
  int isexternal,
  int isnested);
void                   gccgm2_BuildStartFunctionCode              (tree fndecl, int isexported, int isinline);
void                   gccgm2_BuildEndFunctionCode                (tree fndecl, int nested);
void                   iterative_factorial                        (void);
void                   gccgm2_BuildReturnValueCode                (tree fndecl, tree value);
tree                   gccgm2_BuildAssignment                     (tree des, tree expr);
tree                   gccgm2_BuildAdd                            (tree op1, tree op2, int needconvert);
tree                   gccgm2_BuildSub                            (tree op1, tree op2, int needconvert);
tree                   gccgm2_BuildMult                           (tree op1, tree op2, int needconvert);
tree                   gccgm2_BuildDivTrunc                       (tree op1, tree op2, int needconvert);
tree                   gccgm2_BuildModTrunc                       (tree op1, tree op2, int needconvert);
tree                   gccgm2_BuildDivFloor                       (tree op1, tree op2, int needconvert);
tree                   gccgm2_BuildModFloor                       (tree op1, tree op2, int needconvert);
tree                   gccgm2_BuildLSL                            (tree op1, tree op2, int needconvert);
tree                   gccgm2_BuildLSR                             (tree op1, tree op2, int needconvert);
tree                   gccgm2_BuildConvert                        (tree op1, tree op2, int checkOverflow);
tree                   gccgm2_BuildTrunc                          (tree op1);
tree                   gccgm2_BuildNegate                         (tree op1, int needconvert);
tree                   gccgm2_BuildSetNegate                      (tree op1, int needconvert);
tree                   gccgm2_GetSizeOf                                   (tree type);
tree                   gccgm2_GetSizeOfInBits                      (tree type);
tree                   gccgm2_BuildAddr                                   (tree op1, int needconvert);
tree                   gccgm2_BuildLogicalOrAddress                       (tree op1, tree op2, int needconvert);
tree                   gccgm2_BuildLogicalOr                      (tree op1, tree op2, int needconvert);
tree                   gccgm2_BuildLogicalAnd                     (tree op1, tree op2, int needconvert);
tree                   gccgm2_BuildSymmetricDifference            (tree op1, tree op2, int needconvert);
tree                   gccgm2_BuildLogicalDifference              (tree op1, tree op2, int needconvert);
void                   gccgm2_BuildLogicalShift                    (tree op1, tree op2, tree op3, tree nBits, int needconvert);
tree                   gccgm2_BuildLRL                             (tree op1, tree op2, int needconvert);
tree                   gccgm2_BuildLRR                             (tree op1, tree op2, int needconvert);
tree                   gccgm2_BuildLRLn                            (tree op1, tree op2, tree nBits, int needconvert);
tree                   gccgm2_BuildLRRn                            (tree op1, tree op2, tree nBits, int needconvert);
tree                   gccgm2_BuildMask                            (tree nBits, int needconvert);
void                   gccgm2_BuildLogicalRotate                   (tree op1, tree op2, tree op3, tree nBits, int needconvert);
void                   gccgm2_BuildBinarySetDo                     (tree settype, tree op1, tree op2, tree op3, tree (*binop)(tree, tree, tree, tree, int), int is_op1lvalue, int is_op2lvalue, int is_op3lvalue, tree nBits, tree unbounded, tree varproc, tree leftproc, tree rightproc);
tree                   buildUnboundedArrayOf                       (tree, tree, tree);
tree                   create_label_from_name                     (char *name);
void                   gccgm2_DeclareLabel                        (char *name);
static char *                 createUniqueLabel                           (void);
tree                   gccgm2_BuildLessThan                       (tree op1, tree op2);
tree                   gccgm2_BuildGreaterThan                    (tree op1, tree op2);
tree                   gccgm2_BuildLessThanOrEqual                (tree op1, tree op2);
tree                   gccgm2_BuildGreaterThanOrEqual             (tree op1, tree op2);
tree                   gccgm2_BuildEqualTo                        (tree op1, tree op2);
tree                   gccgm2_BuildNotEqualTo                     (tree op1, tree op2);
tree                   gccgm2_BuildIsSuperset                      (tree op1, tree op2);
tree                   gccgm2_BuildIsNotSuperset                   (tree op1, tree op2);
tree                   gccgm2_BuildIsSubset                        (tree op1, tree op2);
tree                   gccgm2_BuildIsNotSubset                     (tree op1, tree op2);
tree                   gccgm2_BuildIndirect                       (tree target, tree type);
void                   gccgm2_DoJump                              (tree exp, char *falselabel, char *truelabel);
void                   gccgm2_BuildParam                          (tree param);
tree                   gccgm2_BuildProcedureCall                  (tree procedure, tree rettype);
tree                   gccgm2_BuildIndirectProcedureCall          (tree procedure, tree rettype);
void                   gccgm2_BuildFunctValue                     (tree value);
void                   gccgm2_BuildAsm                            (tree instr, int IsVolatile, tree inputs,
  tree outputs, tree trash);
tree                   gccgm2_GetIntegerType                      (void);
tree                   gccgm2_GetCardinalType                     (void);
tree                   gccgm2_GetCharType                         (void);
tree                   gccgm2_GetByteType                         (void);
tree                   gccgm2_GetLocType                          (void);
tree                   gccgm2_GetVoidType                         (void);
tree                   gccgm2_GetPointerType                      (void);
tree                   gccgm2_GetBitsetType                       (void);
tree                   gccgm2_GetBitnumType                       (void);
tree                   gccgm2_GetRealType                         (void);
tree                   gccgm2_GetLongRealType                     (void);
tree                   gccgm2_GetShortRealType                    (void);
tree                   gccgm2_GetLongIntType                      (void);
tree                   gccgm2_GetWordType                         (void);
tree                   gccgm2_GetProcType                         (void);
tree                   gccgm2_GetIntegerZero                      (void);
tree                   gccgm2_GetIntegerOne                       (void);
tree                   gccgm2_GetWordZero                         (void);
tree                   gccgm2_GetWordOne                          (void);
tree                   gccgm2_GetPointerZero                      (void);
tree                   gccgm2_GetPointerOne                       (void);
tree                   gccgm2_ToWord                               (tree);
tree                   gccgm2_GetCurrentFunction                  (void);
tree                   gccgm2_GetErrorNode                        (void);
tree                   build_int_2_type                            (int low, int hi, tree type);
tree                   convertToPtr                               (tree t);
int                    gccgm2_AreConstantsEqual                           (tree e1, tree e2);
int                    gccgm2_DetermineSign                       (tree e);
tree                   gccgm2_BuildStringConstant                 (char *string, int length);
tree                   gccgm2_BuildCharConstant                           (char *string);
tree                   gccgm2_ConvertConstantAndCheck             (tree type, tree expr);
tree                   gccgm2_RealToTree                          (char *name);
tree                   gccgm2_BuildStart                          (char *name, int line, int inner_module);
void                   gccgm2_BuildEnd                            (tree fndecl, int nested);
void                   gccgm2_BuildCallInnerInit                  (tree fndecl);
void                   gccgm2_BuildStartMainModule                (void);
void                   gccgm2_BuildEndMainModule                  (void);
tree                   gccgm2_BuildCap                            (tree t);
tree                   gccgm2_BuildAbs                            (tree t);
void                   gccgm2_DebugTree                           (tree t);
void                   gccgm2_DebugTreeChain                      (tree t);
tree                   build_function_call                        (tree function, tree params);
tree                   start_enum                                 (tree name);
void                   pushtag                                    (tree name, tree type);
tree                   start_struct                               (enum tree_code code, tree name);
tree                   gccgm2_BuildStartRecord                    (char *name);
tree                   gccgm2_BuildStartVarientRecord             (char *name);
static void                   layout_array_type                           (tree t);
tree                   gccgm2_BuildFieldRecord                    (char *name, tree type);
tree                   gccgm2_ChainOn                             (tree t1, tree t2);
tree                   gccgm2_ChainOnParamValue                           (tree list, tree parm, tree value);
tree                   gccgm2_AddStringToTreeList                 (tree list, tree string);
tree                   gccgm2_BuildEndRecord                      (tree t, tree fieldlist);
tree                   start_enum                                 (tree name);
unsigned int           min_precision                              (tree value, int unsignedp);
tree                   build_enumerator                                   (tree name, tree value);
void                   gccgm2_ExpandExpressionStatement           (tree t);
int                    is_of_string_type                           (tree type, int warn);
tree                   gccgm2_BuildSize                            (tree op1, int  needconvert);
tree                   gccgm2_BuildOffset                          (tree record, tree field, int needconvert);
tree                   gccgm2_BuildOffset1                         (tree field, int needconvert);
int                    gccgm2_TreeOverflow                         (tree t);
tree                   gccgm2_BuildIntegerConstant                 (int value);
void                   gccgm2_BuildGoto                            (char *name);
tree                   gccgm2_RememberConstant                     (tree t);
tree                   gccgm2_RememberInitModuleFunction           (tree t);
tree                   gccgm2_RememberType                         (tree t);
static tree                   determinePenultimateField                   (tree record, tree field);
tree                   global_constant                             (tree t);
tree                   gccgm2_FoldAndStrip                         (tree t);
void                   stop                                        (void);
static int                    is_a_constant                               (tree t);
static tree                   decl_constant_value_for_broken_optimization (tree decl);
tree                   maybe_apply_renaming_pragma                 (tree decl, tree asmname);
static void                   internal_error_function                     (const char *msgid, va_list *ap);
static int                    function_types_compatible_p                 (tree f1, tree f2);
static int                    type_lists_compatible_p                     (tree args1, tree args2);
static tree                   build_bitset_type                           (void);
static tree                   build_m2_char_node                          (void);
static tree                   build_m2_integer_node                       (void);
static tree                   build_m2_cardinal_node                      (void);
static tree                   build_m2_short_real_node                    (void);
static tree                   build_m2_real_node                          (void);
static tree                   build_m2_long_real_node                     (void);
static tree                   build_m2_long_int_node                      (void);
static tree                   build_m2_long_card_node                     (void);
static tree                   build_m2_short_int_node                     (void);
static tree                   build_m2_short_card_node                     (void);
static tree                   build_m2_iso_loc_node                       (void);
static tree                   build_m2_iso_byte_node                      (void);
static tree                   build_m2_iso_word_node                      (void);
       tree                   convert_type_to_range                       (tree type);
       tree                   gccgm2_GetDefaultType                       (char *name, tree type);
struct struct_constructor*    gccgm2_BuildStartSetConstructor             (tree type);
       void                   gccgm2_BuildSetConstructorElement           (struct struct_constructor*, tree);
       tree                   gccgm2_BuildEndSetConstructor               (struct struct_constructor *p);
struct struct_constructor*    gccgm2_BuildStartRecordConstructor          (tree t);
       tree                   gccgm2_BuildEndRecordConstructor            (struct struct_constructor *p);
       void                   gccgm2_BuildRecordConstructorElement        (struct struct_constructor *p, tree value);
struct struct_constructor*    gccgm2_BuildStartArrayConstructor           (tree t);
       tree                   gccgm2_BuildEndArrayConstructor             (struct struct_constructor *p);
       void                   gccgm2_BuildArrayConstructorElement         (struct struct_constructor *p, tree value, tree indice);
tree                   gccgm2_GetM2CharType                        (void);
tree                   gccgm2_GetM2IntegerType                     (void);
tree                   gccgm2_GetM2ShortRealType                   (void);
tree                   gccgm2_GetM2RealType                        (void);
tree                   gccgm2_GetM2LongRealType                    (void);
tree                   gccgm2_GetM2LongIntType                     (void);
tree                   gccgm2_GetM2ShortIntType                    (void);
tree                   gccgm2_GetShortIntType                      (void);
tree                   gccgm2_GetM2ShortCardType                   (void);
tree                   gccgm2_GetShortCardType                     (void);
tree                   gccgm2_GetISOWordType                       (void);
tree                   gccgm2_GetISOByteType                       (void);
tree                   gccgm2_GetISOLocType                        (void);

int                    gccgm2_CompareTrees                         (tree e1, tree e2);
static int                    is_type                                     (tree type);
void                   gccgm2_BuildBinaryForeachWordDo             (tree, tree, tree, tree, tree (*binop)(tree, tree, int), int, int, int, int, int, int);
void                   gccgm2_BuildUnaryForeachWordDo              (tree, tree, tree, tree (*unop)(tree, int), int, int, int, int);
static tree                   get_rvalue                                  (tree, tree, int);
static tree                   get_set_address                             (tree, int);
static tree                   get_set_field_lhs                           (tree, tree);
static tree                   get_set_field_rhs                           (tree, tree);
void                   gccgm2_BuildExcludeVarConst                 (tree, tree, tree, int, int);
void                   gccgm2_BuildIncludeVarConst                 (tree, tree, tree, int, int);
void                   gccgm2_BuildExcludeVarVar                   (tree, tree, tree, int, tree);
void                   gccgm2_BuildIncludeVarVar                   (tree, tree, tree, int, tree);
void                   gccgm2_BuildIfConstInVar                    (tree, tree, tree, int, int, char *);
void                   gccgm2_BuildIfNotConstInVar                 (tree, tree, tree, int, int, char *);
void                   gccgm2_BuildIfVarInVar                      (tree, tree, tree, int, tree, tree, char *);
void                   gccgm2_BuildIfNotVarInVar                   (tree, tree, tree, int, tree, tree, char *);
static void                   do_jump_if_bit                              (enum tree_code, tree, tree, char *);
void                   gccgm2_BuildIfInRangeGoto                   (tree, tree, tree, char *);
void                   gccgm2_BuildIfNotInRangeGoto                (tree, tree, tree, char *);
void                   gccgm2_BuildForeachWordInSetDoIfExpr        (tree, tree, tree, int, int, int, int, tree (*expr) (tree, tree), char *);
static tree                   get_set_address_if_var                      (tree, int, int);
static tree            get_set_value (tree p, tree field, int is_const, tree op, unsigned int fieldNo);
static tree            get_field_no (tree type, tree op, int is_const, unsigned int fieldNo);
void                   gccgm2_InitFunctionTypeParameters           (int uses_varargs);
tree                   gccgm2_GetM2CardinalType                    (void);
static tree                   convert_arguments                           (tree, tree, tree, tree);
static tree                   default_function_array_conversion           (tree exp);
void                   gccgm2_BuildPushFunctionContext             (void);
void                   gccgm2_BuildPopFunctionContext              (void);
void                   gccgm2_InitGlobalContext                    (void);
tree                   gccgm2_GetM2LongCardType                    (void);
static bool                   flexible_array_type_p                       (tree type);
int                    maybe_objc_comptypes                        (tree, tree, int);
void                   gm2_incomplete_type_error                   (tree, tree);
tree                   c_type_promotes_to                          (tree);
int                    objc_comptypes                              (tree, tree, int);
static tree                   build_set_full_complement                   (void);
void                   gccgm2_GarbageCollect                       (void);
tree                   gccgm2_BuildConstLiteralNumber              (const char *str, unsigned int base);
static int                    interpret_integer                           (const char *str, unsigned int base, unsigned HOST_WIDE_INT *low, HOST_WIDE_INT *high);
static int                    append_digit                                (unsigned HOST_WIDE_INT *low, HOST_WIDE_INT *high, unsigned int digit, unsigned int base);
static int                    interpret_m2_integer                        (const char *str, unsigned int base, unsigned int *low, int *high);
static int                    append_m2_digit                             (unsigned int *low, int *high, unsigned int digit, unsigned int base);
void                   gccgm2_DetermineSizeOfConstant              (const char *str, unsigned int base, int *needsLong, int *needsUnsigned);
int                    gccgm2_Overflow                             (tree t);
tree                   gccgm2_GetM2ZType                           (void);
tree                   gccgm2_GetM2RType                           (void);
tree                   gccgm2_RemoveOverflow                       (tree t);
static int                    getLineNo                                   (void);
       void                   gm2_register_builtin_type                   (tree type, const char* name);
       void                   debug_name                                  (tree t);
       tree                   composite_type                              (tree t1, tree t2);
static void                   readonly_error                              (tree, enum lvalue_use);
       tree                   lookup_label                                (tree id);
       tree                   expand_tree_builtin                         (tree function, tree params ATTRIBUTE_UNUSED, tree coerced_params);
       void                   setId                                       (tree t);
       void                   gccgm2_SetLastFunction                      (tree t);
       void                   gccgm2_SetParamList                         (tree t);
       tree                   gccgm2_GetLastFunction                      (void);
       tree                   gccgm2_GetParamList                         (void);
       void                   gm2_enter_nested                            (struct function *f);
       void                   gm2_leave_nested                            (struct function *f);
       tree                   chainon_stmt_list                           (void);
       tree                   boolean_to_unsigned                         (tree t);
       tree                   gccgm2_GetBooleanType                       (void);
       tree                   gccgm2_GetBooleanFalse                      (void);
       tree                   gccgm2_GetBooleanTrue                       (void);
       tree                   gccgm2_BuildConstPointerType                (tree totype);
       void                   gccgm2_printStmt                            (void);
       void                   gccgm2_BuildTypeDeclaration                 (tree type);
       void                   gccgm2_MarkFunctionReferenced               (tree f);
       void                   gccgm2_FinishBackend                        (void);
       void                   gccgm2_SetFlagUnitAtATime                   (int b);
static void                   gccgm2_RememberVariables                    (tree l);

#if 0
static void                   gm2_write_global_declarations_2             (tree globals);
#endif


  /* PROTOTYPES: ADD HERE */
  
  
  /* end of prototypes */
  

/* externals */
extern int M2Options_SetReturnCheck (int value);
extern int M2Options_SetNilCheck (int value);
extern int M2Options_SetCaseCheck(int value);
extern int M2Options_SetCheckAll(int value);
extern int M2Options_SetVerboseUnbounded(int value);
extern int M2Options_SetQuiet(int value);
extern int M2Options_SetCpp(int value);
extern int M2Options_SetMakeall(int value);
extern int M2Options_SetMakeall0(int value);
extern int M2Options_SetIncludePath(const char *arg);
extern int M2Options_SetUnboundedByReference(int value);


void stop (void) {}
  
tree
maybe_apply_renaming_pragma (tree decl ATTRIBUTE_UNUSED, tree asmname)
{
  return asmname;
}

/*
 *  gccgm2_SetFileNameAndLineNo - allows GM2 to set the filename and line number
 *                                at relevent points during the declaration of types
 *                                and construction of code. We make a copy of the string
 *                                as ggc will collect input_filename.
 */

void
gccgm2_SetFileNameAndLineNo (char *fn, int line)
{
  /* remember that both these variables are actually external to this file */
#if defined(GCC_3_3_X)
  lineno = line;
#elif defined(GCC_3_4_X)
  input_line = line;
#elif defined(GCC_4_1_X)
  input_line = line;
#else
#   error "unsupported gcc source version"
#endif
  if (cfun && fn)
    input_filename = ggc_strdup(fn);
}


static
int
getLineNo (void)
{
#if defined(GCC_3_3_X)
  return lineno;
#elif defined(GCC_3_4_X)
  return input_line;
#elif defined(GCC_4_1_X)
  return input_line;
#else
#   error "unsupported gcc source version"
#endif
}

/*
 *  EmitLineNote - emits line and file information to gcc whilst constructing code.
 *                 Should be used when generating code (after a call to SetFileNameAndLineNo).
 */

void
gccgm2_EmitLineNote (char *fn, int line)
{
#if defined(GCC_3_4_X) || defined(GCC_4_1_X)
  if (cfun && fn) {
    location_t location;

    location.file = ggc_strdup (fn);
    location.line = line;
    emit_line_note (location);
  }
#elif defined(GCC_3_3_X)
  if (cfun && fn)
    emit_line_note (ggc_strdup (fn), line);
#else
#   error "unsupported gcc source version"
#endif
}

/* Routines Expected by gcc:  */

/* These are used to build types for various sizes.  The code below
   is a simplified version of that of GNAT.  */

#ifndef MAX_BITS_PER_WORD
#define MAX_BITS_PER_WORD  BITS_PER_WORD
#endif

/* Used for communication between gm2_common_type_for_mode and
   gm2_register_builtin_type.  */
static GTY(()) tree registered_builtin_types;

/* Return an integer type with BITS bits of precision,
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed.  */

tree
gm2_type_for_size (unsigned int bits, int unsignedp)
{
  if (bits == TYPE_PRECISION (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (bits == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;

  if (bits == TYPE_PRECISION (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;

  if (bits == TYPE_PRECISION (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (bits == TYPE_PRECISION (long_long_integer_type_node))
    return (unsignedp ? long_long_unsigned_type_node
            : long_long_integer_type_node);

  if (bits == TYPE_PRECISION (widest_integer_literal_type_node))
    return (unsignedp ? widest_unsigned_literal_type_node
            : widest_integer_literal_type_node);

  if (bits <= TYPE_PRECISION (intQI_type_node))
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

  if (bits <= TYPE_PRECISION (intHI_type_node))
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;

  if (bits <= TYPE_PRECISION (intSI_type_node))
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;

  if (bits <= TYPE_PRECISION (intDI_type_node))
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;

  return 0;
}

/* Return a data type that has machine mode MODE.
   If the mode is an integer,
   then UNSIGNEDP selects between signed and unsigned types.  */

tree
gm2_type_for_mode (enum machine_mode mode, int unsignedp)
{
  tree t;

  if (mode == TYPE_MODE (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (mode == TYPE_MODE (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;

  if (mode == TYPE_MODE (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;

  if (mode == TYPE_MODE (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (mode == TYPE_MODE (long_long_integer_type_node))
    return unsignedp ? long_long_unsigned_type_node : long_long_integer_type_node;

  if (mode == TYPE_MODE (widest_integer_literal_type_node))
    return unsignedp ? widest_unsigned_literal_type_node
                     : widest_integer_literal_type_node;

  if (mode == QImode)
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

  if (mode == HImode)
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;

  if (mode == SImode)
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;

  if (mode == DImode)
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;

#if HOST_BITS_PER_WIDE_INT >= 64
  if (mode == TYPE_MODE (intTI_type_node))
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;
#endif

  if (mode == TYPE_MODE (float_type_node))
    return float_type_node;

  if (mode == TYPE_MODE (double_type_node))
    return double_type_node;

  if (mode == TYPE_MODE (long_double_type_node))
    return long_double_type_node;

  if (mode == TYPE_MODE (void_type_node))
    return void_type_node;

  if (mode == TYPE_MODE (build_pointer_type (char_type_node)))
    return (unsignedp
            ? make_unsigned_type (GET_MODE_PRECISION (mode))
            : make_signed_type (GET_MODE_PRECISION (mode)));

  if (mode == TYPE_MODE (build_pointer_type (integer_type_node)))
    return (unsignedp
            ? make_unsigned_type (GET_MODE_PRECISION (mode))
            : make_signed_type (GET_MODE_PRECISION (mode)));

  if (COMPLEX_MODE_P (mode))
    {
      enum machine_mode inner_mode;
      tree inner_type;

      if (mode == TYPE_MODE (complex_float_type_node))
        return complex_float_type_node;
      if (mode == TYPE_MODE (complex_double_type_node))
        return complex_double_type_node;
      if (mode == TYPE_MODE (complex_long_double_type_node))
        return complex_long_double_type_node;

      if (mode == TYPE_MODE (complex_integer_type_node) && !unsignedp)
        return complex_integer_type_node;

      inner_mode = GET_MODE_INNER (mode);
      inner_type = gm2_type_for_mode (inner_mode, unsignedp);
      if (inner_type != NULL_TREE)
        return build_complex_type (inner_type);
    }
  else if (VECTOR_MODE_P (mode))
    {
      enum machine_mode inner_mode = GET_MODE_INNER (mode);
      tree inner_type = gm2_type_for_mode (inner_mode, unsignedp);
      if (inner_type != NULL_TREE)
        return build_vector_type_for_mode (inner_type, mode);
    }

  for (t = registered_builtin_types; t; t = TREE_CHAIN (t))
    if (TYPE_MODE (TREE_VALUE (t)) == mode)
      return TREE_VALUE (t);

  return 0;
}

/* Return an unsigned type the same as TYPE in other respects.  */
tree
gm2_unsigned_type (tree type)
{
  tree type1 = TYPE_MAIN_VARIANT (type);
  if (type1 == signed_char_type_node || type1 == char_type_node)
    return unsigned_char_type_node;
  if (type1 == integer_type_node)
    return unsigned_type_node;
  if (type1 == short_integer_type_node)
    return short_unsigned_type_node;
  if (type1 == long_integer_type_node)
    return long_unsigned_type_node;
  if (type1 == long_long_integer_type_node)
    return long_long_unsigned_type_node;
  if (type1 == widest_integer_literal_type_node)
    return widest_unsigned_literal_type_node;
#if HOST_BITS_PER_WIDE_INT >= 64
  if (type1 == intTI_type_node)
    return unsigned_intTI_type_node;
#endif
  if (type1 == intDI_type_node)
    return unsigned_intDI_type_node;
  if (type1 == intSI_type_node)
    return unsigned_intSI_type_node;
  if (type1 == intHI_type_node)
    return unsigned_intHI_type_node;
  if (type1 == intQI_type_node)
    return unsigned_intQI_type_node;

  return gm2_signed_or_unsigned_type (1, type);
}

/* Return a signed type the same as TYPE in other respects.  */

tree
gm2_signed_type (tree type)
{
  tree type1 = TYPE_MAIN_VARIANT (type);
  if (type1 == unsigned_char_type_node || type1 == char_type_node)
    return signed_char_type_node;
  if (type1 == unsigned_type_node)
    return integer_type_node;
  if (type1 == short_unsigned_type_node)
    return short_integer_type_node;
  if (type1 == long_unsigned_type_node)
    return long_integer_type_node;
  if (type1 == long_long_unsigned_type_node)
    return long_long_integer_type_node;
  if (type1 == widest_unsigned_literal_type_node)
    return widest_integer_literal_type_node;
#if HOST_BITS_PER_WIDE_INT >= 64
  if (type1 == unsigned_intTI_type_node)
    return intTI_type_node;
#endif
  if (type1 == unsigned_intDI_type_node)
    return intDI_type_node;
  if (type1 == unsigned_intSI_type_node)
    return intSI_type_node;
  if (type1 == unsigned_intHI_type_node)
    return intHI_type_node;
  if (type1 == unsigned_intQI_type_node)
    return intQI_type_node;

  return gm2_signed_or_unsigned_type (0, type);
}

/* Return a type the same as TYPE except unsigned or
   signed according to UNSIGNEDP.  */

tree
gm2_signed_or_unsigned_type (int unsignedp, tree type)
{
  if (!INTEGRAL_TYPE_P (type)
      || TYPE_UNSIGNED (type) == unsignedp)
    return type;

  /* For ENUMERAL_TYPEs in C++, must check the mode of the types, not
     the precision; they have precision set to match their range, but
     may use a wider mode to match an ABI.  If we change modes, we may
     wind up with bad conversions.  For INTEGER_TYPEs in C, must check
     the precision as well, so as to yield correct results for
     bit-field types.  C++ does not have these separate bit-field
     types, and producing a signed or unsigned variant of an
     ENUMERAL_TYPE may cause other problems as well.  */

#define TYPE_OK(node)                                                       \
  (TYPE_MODE (type) == TYPE_MODE (node)                                     \
   && (c_dialect_cxx () || TYPE_PRECISION (type) == TYPE_PRECISION (node)))
  if (TYPE_OK (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;
  if (TYPE_OK (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;
  if (TYPE_OK (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;
  if (TYPE_OK (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;
  if (TYPE_OK (long_long_integer_type_node))
    return (unsignedp ? long_long_unsigned_type_node
            : long_long_integer_type_node);
  if (TYPE_OK (widest_integer_literal_type_node))
    return (unsignedp ? widest_unsigned_literal_type_node
            : widest_integer_literal_type_node);

#if HOST_BITS_PER_WIDE_INT >= 64
  if (TYPE_OK (intTI_type_node))
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;
#endif
  if (TYPE_OK (intDI_type_node))
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;
  if (TYPE_OK (intSI_type_node))
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;
  if (TYPE_OK (intHI_type_node))
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;
  if (TYPE_OK (intQI_type_node))
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;
#undef TYPE_OK

  return type;
}

/* The gm2 version of the register_builtin_type langhook.  */

void
gm2_register_builtin_type (tree type, const char* name)
{
  tree decl;

  decl = build_decl (TYPE_DECL, get_identifier (name), type);
  DECL_ARTIFICIAL (decl) = 1;
  if (!TYPE_NAME (type))
    TYPE_NAME (type) = decl;
  pushdecl (decl);
  registered_builtin_types = tree_cons (0, type, registered_builtin_types);
}


/* For each binding contour we allocate a binding_level structure which records
   the entities defined or declared in that contour. Contours include:

	the global one
	one for each subprogram definition

   Binding contours are used to create GCC tree BLOCK nodes.  */


struct binding_level GTY(())
  {
    /* A chain of _DECL nodes for all variables, constants, functions,
       and typedef types.  These are in the reverse of the order supplied.
     */
    tree names;

    /* The binding level which this one is contained in (inherits from).  */
    struct binding_level *level_chain;

    /* For each level (except the global one), a chain of BLOCK nodes for all
       the levels that were entered and exited one level down from this one.  */
    tree blocks;

    /* A list of constants (only kept in the global binding level).
       Constants need to be kept through the life of the compilation,
       as the same constants can be used in any scope. */
    tree constants;

    /* A list of inner module initialisation functions */
    tree init_functions;

    /* A list of types created by M2GCCDeclare prior to code generation
       and those which may not be specifically declared and saved via
       a push_decl */
    tree types;
  };

#define NULL_BINDING_LEVEL (struct binding_level *) NULL
  
/* The binding level currently in effect.  */

static GTY(()) struct binding_level *current_binding_level;

/* A chain of binding_level structures awaiting reuse.  */

static GTY((deletable (""))) struct binding_level *free_binding_level;

/* The outermost binding level, for names of file scope.
   This is created when the compiler is started and exists
   through the entire run.  */

static GTY(()) struct binding_level *global_binding_level;

/* Binding level structures are initialized by copying this one.  */

static struct binding_level clear_binding_level
  = {NULL, NULL, NULL, NULL, NULL, NULL};

/* The resulting tree type.  */

union lang_tree_node 
  GTY((desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE"),
       chain_next ("(union lang_tree_node *)TREE_CHAIN (&%h.generic)")))
{
  union tree_node GTY ((tag ("0"), 
                        desc ("tree_node_structure (&%h)"))) 
    generic;
  struct lang_identifier GTY ((tag ("1"))) identifier;
};

/* Language-specific declaration information.  */

struct lang_decl GTY(())
{
  char dummy;
};

struct lang_type GTY(())
{
  char dummy;
};

/* Macros for access to language-specific slots in an identifier.  */
/* Each of these slots contains a DECL node or null.  */

/* This represents the value which the identifier has in the current
   scope.  */
#define IDENTIFIER_LOCAL_VALUE(NODE)    \
  (((struct lang_identifier *) (NODE))->local_value)
/* This represents the value which the identifier has as a label in
   the current label scope.  */
#define IDENTIFIER_LABEL_VALUE(NODE)    \
  (((struct lang_identifier *) (NODE))->label_value)
/* This records the extern decl of this identifier, if it has had one
   at any point in this compilation.  */
#define IDENTIFIER_LIMBO_VALUE(NODE)    \
  (((struct lang_identifier *) (NODE))->limbo_value)
/* This records the implicit function decl of this identifier, if it
   has had one at any point in this compilation.  */
#define IDENTIFIER_IMPLICIT_DECL(NODE)  \
  (((struct lang_identifier *) (NODE))->implicit_decl)
/* This is the last function in which we printed an "undefined variable"
   message for this identifier.  Value is a FUNCTION_DECL or null.  */
#define IDENTIFIER_ERROR_LOCUS(NODE)    \
  (((struct lang_identifier *) (NODE))->error_locus)

/* In identifiers, C uses the following fields in a special way:
   TREE_PUBLIC        to record that there was a previous local extern decl.
   TREE_USED          to record that such a decl was used.
   TREE_ADDRESSABLE   to record that the address of such a decl was used.  */

/* In a RECORD_TYPE or UNION_TYPE, nonzero if any component is read-only.  */
#define C_TYPE_FIELDS_READONLY(TYPE) TREE_LANG_FLAG_1 (TYPE)

/* In a RECORD_TYPE or UNION_TYPE, nonzero if any component is volatile.  */
#define C_TYPE_FIELDS_VOLATILE(TYPE) TREE_LANG_FLAG_2 (TYPE)

/* In a RECORD_TYPE or UNION_TYPE or ENUMERAL_TYPE
   nonzero if the definition of the type has already started.  */
#define C_TYPE_BEING_DEFINED(TYPE) TYPE_LANG_FLAG_0 (TYPE)

/* In an IDENTIFIER_NODE, nonzero if this identifier is actually a
   keyword.  C_RID_CODE (node) is then the RID_* value of the keyword,
   and C_RID_YYCODE is the token number wanted by Yacc.  */
#define C_IS_RESERVED_WORD(ID) TREE_LANG_FLAG_0 (ID)

/* Record whether a type or decl was written with nonconstant size.
   Note that TYPE_SIZE may have simplified to a constant.  */
#define C_TYPE_VARIABLE_SIZE(TYPE) TYPE_LANG_FLAG_1 (TYPE)
#define C_DECL_VARIABLE_SIZE(TYPE) DECL_LANG_FLAG_0 (TYPE)

/* For FUNCTION_TYPE, a hidden list of types of arguments.  The same as
   TYPE_ARG_TYPES for functions with prototypes, but created for functions
   without prototypes.  */
#define TYPE_ACTUAL_ARG_TYPES(NODE) TYPE_BINFO (NODE)


#define c_build_type_variant(TYPE, CONST_P, VOLATILE_P)           \
  c_build_qualified_type ((TYPE),                                 \
                          ((CONST_P) ? TYPE_QUAL_CONST : 0) |     \
                          ((VOLATILE_P) ? TYPE_QUAL_VOLATILE : 0))


/* The binding level currently in effect.  */
static struct binding_level *current_binding_level = NULL;

/* The outermost binding level. This binding level is created when the
   compiler is started and it will exist through the entire compilation.  */
static struct binding_level *global_binding_level;

/* Return the list of declarations in the current level. Note that this list
   is in reverse order (it has to be so for back-end compatibility).  */

tree
getdecls (void)
{
  return current_binding_level->names;
}

int
global_bindings_p (void)
{
  return current_binding_level == global_binding_level;
}

/* Enter a new binding level. The input parameter is ignored, but has to be
   specified for back-end compatibility.  */

void
pushlevel (int ignore ATTRIBUTE_UNUSED)
{
  struct binding_level *newlevel = xmalloc (sizeof (struct binding_level));

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
poplevel (int keep, int reverse, int functionbody)
{
  /* Points to a BLOCK tree node. This is the BLOCK node constructed for the
     binding level that we are about to exit and which is returned by this
     routine.  */
  tree block_node = NULL_TREE;
  tree decl_chain;
  tree subblock_chain = current_binding_level->blocks;
  tree subblock_node;

  /* Reverse the list of *_DECL nodes if desired.  Note that the ..._DECL
     nodes chained through the `names' field of current_binding_level are in
     reverse order except for PARM_DECL node, which are explicitly stored in
     the right order.  */
  decl_chain = (reverse) ? nreverse (current_binding_level->names)
			 : current_binding_level->names;

  /* If there were any declarations in the current binding level, or if this
     binding level is a function body, or if there are any nested blocks then
     create a BLOCK node to record them for the life of this function.  */
  if (keep || functionbody)
    block_node = build_block (keep ? decl_chain : 0, subblock_chain, 0, 0);

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
	}

  /* Pop the current level.  */
  current_binding_level = current_binding_level->level_chain;

  if (functionbody)
    {
      /* This is the top level block of a function.  */
      DECL_INITIAL (current_function_decl) = block_node;
    }
  else if (block_node)
    {
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
insert_block (tree block)
{
  TREE_USED (block) = 1;
  current_binding_level->blocks
    = chainon (current_binding_level->blocks, block);
}


/* Records a ..._DECL node DECL as belonging to the current lexical scope.
   Returns the ..._DECL node. */

tree
pushdecl (tree decl)
{
  /* External objects aren't nested, other objects may be.  */
    
  if ((DECL_EXTERNAL (decl)) || (decl==current_function_decl))
    DECL_CONTEXT (decl) = 0;
  else
    DECL_CONTEXT (decl) = current_function_decl;

  /* Put the declaration on the list.  The list of declarations is in reverse
     order. The list will be reversed later if necessary.  This needs to be
     this way for compatibility with the back-end.  */

  TREE_CHAIN (decl) = current_binding_level->names;
  current_binding_level->names = decl;

  /* For the declaration of a type, set its name if it is not already set. */

  if (TREE_CODE (decl) == TYPE_DECL
      && TYPE_NAME (TREE_TYPE (decl)) == 0)
    TYPE_NAME (TREE_TYPE (decl)) = DECL_NAME (decl);

  return decl;
}

#define NULL_BINDING_LEVEL (struct binding_level *) NULL                        





enum gimplify_status
gm2_gimplify_expr (tree *expr_p,
                   tree *pre_p,
                   tree *post_p ATTRIBUTE_UNUSED)
{
#if 0
  return GS_UNHANDLED;
#else
  tree expr = *expr_p;
  tree op0;

  switch (TREE_CODE (expr))
    {
#if 0
    case BIND_EXPR:

      stop();
      gimplify_stmt (&BIND_EXPR_VARS (expr));

      break;

    case BLOCK:
      stop();
      break;
#endif
    case INDIRECT_REF:
      op0 = TREE_OPERAND (expr, 0);
      if (! SSA_VAR_P (op0)) {
	tree var = create_tmp_var (TREE_TYPE (op0), "indirect");
	tree mod = build (MODIFY_EXPR, TREE_TYPE (op0), var, op0);
	gimplify_and_add (mod, pre_p);
	TREE_OPERAND (expr, 0) = var;
#if 0
	debug_tree(expr);
#endif
	op0 = var;
      }
      gimplify_type_sizes (TREE_TYPE (expr), pre_p);
      gimplify_type_sizes (TREE_TYPE (op0), pre_p);
      return GS_OK;
      break;
    case ADDR_EXPR:
      op0 = TREE_OPERAND (expr, 0);
      gimplify_type_sizes (TREE_TYPE (op0), pre_p);
      gimplify_type_sizes (TREE_TYPE (expr), pre_p);
      return GS_OK;
      break;
    default:
      break;
    }
  return GS_UNHANDLED;
#endif
}

void find_arg0 (tree t);

void find_arg0 (tree t)
{
  printf ("value of arg0(t) = %p\n", TREE_OPERAND (t, 0));
  printf ("address of arg0(t) = %p\n", &TREE_OPERAND (t, 0));
  printf ("watch (*(tree *)%p) != %p\n",
          &TREE_OPERAND (t, 0), TREE_OPERAND (t, 0));
}


void
debug_name (tree t)
{
  char class = TREE_CODE_CLASS (TREE_CODE (t));
  tree name = DECL_NAME (t);
  char buf[4096];
  
  if (class == 'd') {
    snprintf (buf, sizeof(buf), "tree code = <%s>\n", tree_code_name[(int) TREE_CODE (t)]);
    fprintf(stderr, buf);
    if (name) {
      snprintf (buf, sizeof(buf), "name = <%s>\n", IDENTIFIER_POINTER (name));
      fprintf(stderr, buf);
    }
  }
}

/* Hook used by expand_expr to expand language-specific tree codes.  */

rtx
gm2_expand_expr (tree exp, rtx target, enum machine_mode tmode ATTRIBUTE_UNUSED,
                 int modifier ATTRIBUTE_UNUSED, rtx *alt_rtl ATTRIBUTE_UNUSED)
{
  enum tree_code code = TREE_CODE (exp);

  switch (code)
    {
    case ERROR_MARK:
      return NULL_RTX;
      break;

    case IDENTIFIER_NODE:
    case TREE_LIST:
      if (exp == boolean_false_node)
        return const0_rtx;
      else if (exp == boolean_true_node)
        return const1_rtx;

      /* fall through */
    default:
      fprintf(stderr, "gm2 front end has been asked to expand the following expression\n");
      debug_tree (exp);
      fprintf (stderr, "halting\n");
      fancy_abort (__FILE__, __LINE__, __FUNCTION__);
      return target;
    }
}

#if 0
/* A subroutine of gm2_write_global_declarations Emit debug information for each
   of the declarations in GLOBALS.  */

static void
gm2_write_global_declarations_2 (tree globals)
{
  tree decl;

  for (decl = globals; decl ; decl = TREE_CHAIN (decl))
    debug_hooks->global_decl (decl);
}
#endif

void
gm2_write_global_declarations (void)
{
  /* Don't waste time on further processing if -fsyntax-only or we've
     encountered errors.  */
  if (flag_syntax_only || errorcount || sorrycount)
    return;

  /* We're done parsing; proceed to optimize and emit assembly.
     FIXME: shouldn't be the front end's responsibility to call this.  */
  cgraph_optimize ();

#if 0
  /* After cgraph has had a chance to emit everything that's going to
     be emitted, output debug information for globals.  */
  if (errorcount == 0 && sorrycount == 0)
    gm2_write_global_declarations_2 (BLOCK_VARS (global_binding_level->names));
#endif
}

/* gm2 expand function langhook.  */

void
gm2_expand_function (tree fndecl)
{
  /* We have nothing special to do while expanding functions for GNU Modula-2.  */
  tree_rest_of_compilation (fndecl);
}

/* init_m2_builtins - build tree nodes and builtin functions for GNU Modula-2
 */

void
init_m2_builtins (void)
{
  ptr_type_node
    = build_pointer_type (void_type_node);

  proc_type_node
    = build_pointer_type (build_function_type (void_type_node, NULL_TREE));

  set_full_complement = build_set_full_complement ();

  bitset_type_node = build_bitset_type ();
  m2_char_type_node = build_m2_char_node ();
  m2_integer_type_node = build_m2_integer_node ();
  m2_cardinal_type_node = build_m2_cardinal_node ();
  m2_short_real_type_node = build_m2_short_real_node ();
  m2_real_type_node = build_m2_real_node ();
  m2_long_real_type_node = build_m2_long_real_node ();
  m2_long_int_type_node = build_m2_long_int_node ();
  m2_long_card_type_node = build_m2_long_card_node ();
  m2_short_int_type_node = build_m2_short_int_node ();
  m2_short_card_type_node = build_m2_short_card_node ();
  m2_z_type_node = build_m2_long_int_node ();
  m2_iso_loc_type_node = build_m2_iso_loc_node ();
  m2_iso_byte_type_node = build_m2_iso_byte_node ();
  m2_iso_word_type_node = build_m2_iso_word_node ();


  /*
   * --fixme-- this is a hack which allows us to generate correct stabs for CHAR and sets of CHAR
   * in gcc-3.2. The command line must include -gstabs
   * Note that -g doesn't work as gdb 5.2.1 will give up even if the following line is removed
   * if we use sets. Presumably a dwarf gdb/gcc/gm2 bug..
   */
  use_gnu_debug_info_extensions = 1;

  gm2builtins_init ();
}

/* Build the void_list_node (void_type_node having been created).  */
tree
build_void_list_node (void)
{
  tree t = build_tree_list (NULL_TREE, void_type_node);
  return t;
}

static
tree
build_m2_integer_node (void)
{
  tree c;

  /* Define `INTEGER' */

  c = make_node (INTEGER_TYPE);
  TYPE_PRECISION (c) = INT_TYPE_SIZE;
  TYPE_SIZE (c) = 0;
  fixup_signed_type (c);
  TYPE_UNSIGNED (c) = 0;

  return c;
}

static
tree
build_m2_cardinal_node (void)
{
  tree c;

  /* Define `CARDINAL' */

  c = make_node (INTEGER_TYPE);
  TYPE_PRECISION (c) = INT_TYPE_SIZE;
  TYPE_SIZE (c) = 0;
  fixup_unsigned_type (c);
  TYPE_UNSIGNED (c) = 1;

  return c;
}

static
tree
build_m2_char_node (void)
{
  tree c;

  /* Define `CHAR', to be an unsigned char. */

  /* Modula-2 CHAR type, borrowed from GNU Pascal and modified
     so that we only use unsigned char */
  c = make_node (CHAR_TYPE);
  TYPE_PRECISION (c) = CHAR_TYPE_SIZE;
  TYPE_SIZE (c) = 0;

  fixup_unsigned_type (c);

  return c;
}

static
tree
build_m2_short_real_node (void)
{
  tree c;

  /* Define `REAL' */

  c = make_node (REAL_TYPE);
  TYPE_PRECISION (c) = FLOAT_TYPE_SIZE;
  layout_type (c);

  return c;
}

static
tree
build_m2_real_node (void)
{
  tree c;

  /* Define `REAL' */

  c = make_node (REAL_TYPE);
  TYPE_PRECISION (c) = DOUBLE_TYPE_SIZE;
  layout_type (c);

  return c;
}

static
tree
build_m2_long_real_node (void)
{
  tree c;

  /* Define `LONGREAL' */

  c = make_node (REAL_TYPE);
  TYPE_PRECISION (c) = LONG_DOUBLE_TYPE_SIZE;
  layout_type (c);

  return c;
}

static
tree
build_m2_long_int_node (void)
{
  tree c;

  /* Define `LONGINT' */

  c = make_signed_type (LONG_LONG_TYPE_SIZE);
  layout_type (c);

  return c;
}

static
tree
build_m2_long_card_node (void)
{
  tree c;

  /* Define `LONGCARD' */

  c = make_unsigned_type (LONG_LONG_TYPE_SIZE);
  layout_type (c);

  return c;
}

static
tree
build_m2_short_int_node (void)
{
  tree c;

  /* Define `SHORTINT' */

  c = make_signed_type (SHORT_TYPE_SIZE);
  layout_type (c);

  return c;
}

static
tree
build_m2_short_card_node (void)
{
  tree c;

  /* Define `SHORTCARD' */

  c = make_unsigned_type (SHORT_TYPE_SIZE);
  layout_type (c);

  return c;
}

static
tree
build_m2_iso_loc_node (void)
{
  tree c;

  /* Define `LOC' as specified in ISO m2 */
  
  c = make_node (INTEGER_TYPE);
  TYPE_PRECISION (c) = BITS_PER_UNIT;
  TYPE_SIZE (c) = 0;

  fixup_unsigned_type (c);
  TYPE_UNSIGNED (c) = 1;

  return c;
}

static
tree
build_m2_iso_byte_node (void)
{
  tree c;

  /*
   * Define `BYTE' as specified in ISO m2
   *
   * BYTE = ARRAY [0..SizeOfByte / SizeOfLoc] OF LOC ;
   */

  if (BITS_PER_UNIT == 8)
    c = gccgm2_GetISOLocType ();
  else
    c = gccgm2_BuildArrayType (gccgm2_GetISOLocType (),
                               gccgm2_BuildArrayIndexType (gccgm2_GetIntegerZero (),
                                                           gccgm2_BuildIntegerConstant (BITS_PER_UNIT/8)));
  return c;
}

static
tree
build_m2_iso_word_node (void)
{
  tree c;

  /*
   * Define `WORD' as specified in ISO m2
   *
   * WORD = ARRAY [0..SizeOfWord / SizeOfLoc] OF LOC ;
   */

  if (gccgm2_GetBitsPerInt () == BITS_PER_UNIT)
    c = gccgm2_GetISOLocType ();
  else
    c = gccgm2_BuildArrayType (gccgm2_GetISOLocType (),
                               gccgm2_BuildArrayIndexType (gccgm2_GetIntegerZero (),
                                                           (gccgm2_BuildSub (gccgm2_BuildIntegerConstant (gccgm2_GetBitsPerInt ()/BITS_PER_UNIT),
                                                                             integer_one_node,
                                                                             FALSE))));
  return c;
}

/* Create the predefined scalar types such as `integer_type_node' needed 
   in the gcc back-end and initialize the global binding level.  */

void
gm2_init_decl_processing ()
{
  tree array_domain_type;

  current_function_decl = NULL;
  current_binding_level = NULL_BINDING_LEVEL;
  free_binding_level    = NULL_BINDING_LEVEL;

  pushlevel (0);        /* make the binding_level structure for global names */
  global_binding_level  = current_binding_level;

  build_common_tree_nodes (flag_signed_char, false);

  /* Define `int' and `char' first so that dbx will output them first.  */
  pushdecl (build_decl (TYPE_DECL, get_identifier("int"),
                        integer_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("char"),
                        char_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long int"),
                        long_integer_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned int"),
                        unsigned_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long unsigned int"),
                        long_unsigned_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long long int"),
                        long_long_integer_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long long unsigned int"),
                        long_long_unsigned_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("short int"),
                        short_integer_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("short unsigned int"),
                        short_unsigned_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("signed char"),
                        signed_char_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned char"),
                        unsigned_char_type_node));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, intQI_type_node));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, intHI_type_node));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, intSI_type_node));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, intDI_type_node));
#if HOST_BITS_PER_WIDE_INT >= 64
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, intTI_type_node));
#endif
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, unsigned_intQI_type_node));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, unsigned_intHI_type_node));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, unsigned_intSI_type_node));
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, unsigned_intDI_type_node));
#if HOST_BITS_PER_WIDE_INT >= 64
  pushdecl (build_decl (TYPE_DECL, NULL_TREE, unsigned_intTI_type_node));
#endif

  /* Create the widest literal types.  */
  widest_integer_literal_type_node
    = make_signed_type (HOST_BITS_PER_WIDE_INT * 2);
  pushdecl (build_decl (TYPE_DECL, NULL_TREE,
                        widest_integer_literal_type_node));

  widest_unsigned_literal_type_node
    = make_unsigned_type (HOST_BITS_PER_WIDE_INT * 2);
  pushdecl (build_decl (TYPE_DECL, NULL_TREE,
                        widest_unsigned_literal_type_node));

  size_type_node = make_unsigned_type (POINTER_SIZE);
  signed_size_type_node = gm2_signed_type (size_type_node);
  set_sizetype (size_type_node);

  build_common_tree_nodes_2 (flag_short_double);

  void_list_node = build_void_list_node ();

#if 1
  /* Make a type to be the domain of a few array types
     whose domains don't really matter.
     200 is small enough that it always fits in size_t
     and large enough that it can hold most function names for the
     initializations of __FUNCTION__ and __PRETTY_FUNCTION__.  */
  array_domain_type = build_index_type (size_int (200));
#endif

  /* Make a type for arrays of characters.
     With luck nothing will ever really depend on the length of this
     array type.  */
  char_array_type_node
    = build_array_type (char_type_node, array_domain_type);

  /* Likewise for arrays of ints.  */
  int_array_type_node
    = build_array_type (integer_type_node, array_domain_type);

  string_type_node = build_pointer_type (char_type_node);
  const_string_type_node
    = build_pointer_type (build_qualified_type
                          (char_type_node, TYPE_QUAL_CONST));

  default_function_type = build_function_type (integer_type_node, NULL_TREE);

#if 0
  ptrdiff_type_node
    = TREE_TYPE (identifier_global_value (get_identifier (PTRDIFF_TYPE)));
  unsigned_ptrdiff_type_node = gm2_unsigned_type (ptrdiff_type_node);
#endif

  pedantic_lvalues = pedantic;

  /* truthvalue nodes for 'C' */
  truthvalue_type_node = integer_type_node;
  truthvalue_true_node = integer_one_node;
  truthvalue_false_node = integer_zero_node;

  init_m2_builtins();
}

/*
 *  is_a_constant - returns TRUE if tree, t, is a constant.
 */

static int
is_a_constant (tree t)
{
  return (TREE_CODE (t) == INTEGER_CST) ||
         (TREE_CODE (t) == REAL_CST) ||
         (TREE_CODE (t) == REAL_CST) ||
         (TREE_CODE (t) == COMPLEX_CST) ||
         (TREE_CODE (t) == STRING_CST);
}

/*
 *  RememberConstant - adds a tree, t, onto the list of constants to be marked
 *                     whenever the ggc re-marks all used storage. Constants
 *                     live throughout the whole compilation - and they
 *                     can be used by many different functions if necessary.
 */

tree
gccgm2_RememberConstant (tree t)
{
  if ((t != NULL) && (is_a_constant(t)))
    return global_constant(t);
  return t;
}

/*
 *  global_constant - t is a constant, we keep a chain of all constants
 *                    in the global binding level.
 */

tree
global_constant (tree t)
{
  tree t1;

  if (global_binding_level->constants != NULL)
    for (t1 = global_binding_level->constants; TREE_CHAIN (t1); t1 = TREE_CHAIN (t1))
      if (t1 == t)
        return t;
  
  global_binding_level->constants = tree_cons (NULL_TREE,
                                               t, global_binding_level->constants);
  return t;
}

/*
 *  RememberInitModuleFunction - records tree, t, in the global binding level.
 *                               So that it will not be garbage collected.
 *                               In theory the inner modules could be placed
 *                               inside the current_binding_level I suspect.
 */

tree
gccgm2_RememberInitModuleFunction (tree t)
{
  global_binding_level->init_functions = tree_cons (NULL_TREE,
                                                    t, global_binding_level->init_functions);
  return t;
}

/*
 *  RememberType - remember the type, t, in the ggc marked list.
 */

tree
gccgm2_RememberType (tree t)
{
  global_binding_level->types = tree_cons (NULL_TREE,
                                           t, global_binding_level->types);
  return t;
}

tree
gccgm2_FoldAndStrip (tree t)
{
  if (t != NULL) {
    t = fold (t);
    STRIP_NOPS (t);
    if (TREE_CODE (t) == CONST_DECL)
      return gccgm2_FoldAndStrip (DECL_INITIAL (t));
  }

  return t;
}

/* Handle command-line options.  Returns 0 if unrecognized, 1 if
   recognized and handled.  */

int
gm2_handle_option (size_t scode, const char *arg, int value)
{
  enum opt_code code = (enum opt_code) scode;

  /* Ignore file names.  */
  if (code == N_OPTS)
    return 1;

#if 0
  if (insideCppArgs) {
    if (code == OPT_fcppend)
      insideCppArgs = FALSE;
    return 1;
  }
#endif

  switch (code) {

  case OPT_freturn:
    return M2Options_SetReturnCheck(value);
  case OPT_fnil:
    return M2Options_SetNilCheck(value);
  case OPT_fcase:
    return M2Options_SetCaseCheck(value);
  case OPT_fcheck_all:
    return M2Options_SetCheckAll(value);
  case OPT_Wverbose_unbounded:
    return M2Options_SetVerboseUnbounded(value);
  case OPT_quiet:
    return M2Options_SetQuiet(value);
  case OPT_fcpp:
    return M2Options_SetCpp(value);
  case OPT_fmakeall:
    return M2Options_SetMakeall(value);
  case OPT_fmakeall0:
    return M2Options_SetMakeall0(value);
  case OPT_fmake_I_:
    return M2Options_SetIncludePath(arg);
  case OPT_funbounded_by_reference:
    return M2Options_SetUnboundedByReference(value);
  case OPT_fcppbegin:
    insideCppArgs = TRUE;
    return 1;
  default:
    return 1;
  }
  return 0;
}

/* Perform all the initialization steps that are language-specific.  */

/* Here we have the function to handle the compiler error processing in GCC.  */

static void
internal_error_function (const char *msgid, va_list *ap)
{
  fprintf (stderr, "Internal error: ");
  vfprintf (stderr, msgid, *ap);
  fputc ('\n', stderr);
  fnotice (stderr, GM2_BUG_REPORT);
  exit (FATAL_EXIT_CODE);
}

/* Perform all the finalization steps that are language-specific.  */

bool
gm2_init (void)
{
  input_line = 0;
  gm2_init_decl_processing ();
  global_dc->internal_error = &internal_error_function;

  return true;
}

tree
gm2_truthvalue_conversion (tree expr)
{
#if defined(USE_BOOLEAN)
  return expr;
#else
  return convert (integer_type_node, expr);
#endif
}

/* Print any language-specific compilation statistics.  */

void
print_lang_statistics (void)
{}

int
maybe_objc_comptypes (tree lhs ATTRIBUTE_UNUSED,
		      tree rhs ATTRIBUTE_UNUSED,
		      int reflexive ATTRIBUTE_UNUSED)
{
  return -1;
}

/* Return 1 if PARMS specifies a fixed number of parameters
   and none of their types is affected by default promotions.  */

int
self_promoting_args_p (tree parms ATTRIBUTE_UNUSED)
{
  return 0;
}

/* Performs whatever initialization steps are needed by the language-dependent
   lexical analyzer.  */

/* Routine to print parse error message.  */

void
yyerror (char *str)
{
  fprintf (stderr, "%s\n", str);
}

void
gm2_parse_file (int debug_flag ATTRIBUTE_UNUSED)
{
  gccgm2front(gm2_argc, gm2_argv);
}

/* Common initialization before parsing options.  */
unsigned int
gm2_init_options (unsigned int argc, const char **argv)
{
  gm2_argc = argc;
  gm2_argv = argv;
  return CL_ModulaX2;
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

/* Macros to save and restore the spelling stack around push_... functions.
   Alternative to SAVE_SPELLING_STACK.  */

#define SPELLING_DEPTH() (spelling - spelling_base)
#define RESTORE_SPELLING_DEPTH(depth) (spelling = spelling_base + depth)

/* Save and restore the spelling stack around arbitrary C code.  */

#define SAVE_SPELLING_DEPTH(code)               \
{                                               \
  int __depth = SPELLING_DEPTH ();              \
  code;                                         \
  RESTORE_SPELLING_DEPTH (__depth);             \
}

/* Push an element on the spelling stack with type KIND and assign VALUE
   to MEMBER.  */

#define PUSH_SPELLING(KIND, VALUE, MEMBER)                              \
{                                                                       \
  int depth = SPELLING_DEPTH ();                                        \
                                                                        \
  if (depth >= spelling_size)                                           \
    {                                                                   \
      spelling_size += 10;                                              \
      if (spelling_base == 0)                                           \
        spelling_base                                                   \
          = (struct spelling *) xmalloc (spelling_size * sizeof (struct spelling));     \
      else                                                              \
        spelling_base                                                   \
          = (struct spelling *) xrealloc (spelling_base,                \
                                          spelling_size * sizeof (struct spelling));    \
      RESTORE_SPELLING_DEPTH (depth);                                   \
    }                                                                   \
                                                                        \
  spelling->kind = (KIND);                                              \
  spelling->MEMBER = (VALUE);                                           \
  spelling++;                                                           \
}

/* Provide a means to pass component names derived from the spelling stack.  */

char initialization_message;

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
qualify_type (tree type, tree like)
{
  int constflag = TYPE_READONLY (type) || TYPE_READONLY (like);
  int volflag = TYPE_VOLATILE (type) || TYPE_VOLATILE (like);
  return build_c_type_variant (type, constflag, volflag);
}

/* FROM ../c-typeck.c IMPORT */
/* Return the common type for two arithmetic types under the usual
   arithmetic conversions.  The default conversions have already been
   applied, and enumerated types converted to their compatible integer
   types.  The resulting type is unqualified and has no attributes.

   This is the type for the result of most arithmetic operations
   if the operands have the given two types.  */

static tree
c_common_type (tree t1, tree t2)
{
  enum tree_code code1;
  enum tree_code code2;

  /* If one type is nonsense, use the other.  */
  if (t1 == error_mark_node)
    return t2;
  if (t2 == error_mark_node)
    return t1;

  if (TYPE_QUALS (t1) != TYPE_UNQUALIFIED)
    t1 = TYPE_MAIN_VARIANT (t1);

  if (TYPE_QUALS (t2) != TYPE_UNQUALIFIED)
    t2 = TYPE_MAIN_VARIANT (t2);

  if (TYPE_ATTRIBUTES (t1) != NULL_TREE)
    t1 = build_type_attribute_variant (t1, NULL_TREE);

  if (TYPE_ATTRIBUTES (t2) != NULL_TREE)
    t2 = build_type_attribute_variant (t2, NULL_TREE);

  /* Save time if the two types are the same.  */

  if (t1 == t2) return t1;

  code1 = TREE_CODE (t1);
  code2 = TREE_CODE (t2);

  gcc_assert (code1 == VECTOR_TYPE || code1 == COMPLEX_TYPE
              || code1 == REAL_TYPE || code1 == INTEGER_TYPE);
  gcc_assert (code2 == VECTOR_TYPE || code2 == COMPLEX_TYPE
              || code2 == REAL_TYPE || code2 == INTEGER_TYPE);

  /* If one type is a vector type, return that type.  (How the usual
     arithmetic conversions apply to the vector types extension is not
     precisely specified.)  */
  if (code1 == VECTOR_TYPE)
    return t1;

  if (code2 == VECTOR_TYPE)
    return t2;

  /* If one type is complex, form the common type of the non-complex
     components, then make that complex.  Use T1 or T2 if it is the
     required type.  */
  if (code1 == COMPLEX_TYPE || code2 == COMPLEX_TYPE)
    {
      tree subtype1 = code1 == COMPLEX_TYPE ? TREE_TYPE (t1) : t1;
      tree subtype2 = code2 == COMPLEX_TYPE ? TREE_TYPE (t2) : t2;
      tree subtype = c_common_type (subtype1, subtype2);

      if (code1 == COMPLEX_TYPE && TREE_TYPE (t1) == subtype)
        return t1;
      else if (code2 == COMPLEX_TYPE && TREE_TYPE (t2) == subtype)
        return t2;
      else
        return build_complex_type (subtype);
    }

  /* If only one is real, use it as the result.  */

  if (code1 == REAL_TYPE && code2 != REAL_TYPE)
    return t1;

  if (code2 == REAL_TYPE && code1 != REAL_TYPE)
    return t2;

  /* Both real or both integers; use the one with greater precision.  */

  if (TYPE_PRECISION (t1) > TYPE_PRECISION (t2))
    return t1;
  else if (TYPE_PRECISION (t2) > TYPE_PRECISION (t1))
    return t2;

  /* Same precision.  Prefer long longs to longs to ints when the
     same precision, following the C99 rules on integer type rank
     (which are equivalent to the C90 rules for C90 types).  */

  if (TYPE_MAIN_VARIANT (t1) == long_long_unsigned_type_node
      || TYPE_MAIN_VARIANT (t2) == long_long_unsigned_type_node)
    return long_long_unsigned_type_node;

  if (TYPE_MAIN_VARIANT (t1) == long_long_integer_type_node
      || TYPE_MAIN_VARIANT (t2) == long_long_integer_type_node)
    {
      if (TYPE_UNSIGNED (t1) || TYPE_UNSIGNED (t2))
        return long_long_unsigned_type_node;
      else
        return long_long_integer_type_node;
    }

  if (TYPE_MAIN_VARIANT (t1) == long_unsigned_type_node
      || TYPE_MAIN_VARIANT (t2) == long_unsigned_type_node)
    return long_unsigned_type_node;

  if (TYPE_MAIN_VARIANT (t1) == long_integer_type_node
      || TYPE_MAIN_VARIANT (t2) == long_integer_type_node)
    {
      /* But preserve unsignedness from the other type,
         since long cannot hold all the values of an unsigned int.  */
      if (TYPE_UNSIGNED (t1) || TYPE_UNSIGNED (t2))
        return long_unsigned_type_node;
      else
        return long_integer_type_node;
    }

  /* Likewise, prefer long double to double even if same size.  */
  if (TYPE_MAIN_VARIANT (t1) == long_double_type_node
      || TYPE_MAIN_VARIANT (t2) == long_double_type_node)
    return long_double_type_node;

  /* Otherwise prefer the unsigned one.  */

  if (TYPE_UNSIGNED (t1))
    return t1;
  else
    return t2;
}

/* FROM ../c-typeck.c IMPORT */
/* Wrapper around c_common_type that is used by c-common.c and other
   front end optimizations that remove promotions.  ENUMERAL_TYPEs
   are allowed here and are converted to their compatible integer types.
   BOOLEAN_TYPEs are allowed here and return either boolean_type_node or
   preferably a non-Boolean type as the common type.  */
tree
common_type (tree t1, tree t2)
{
  if (TREE_CODE (t1) == ENUMERAL_TYPE)
    t1 = gm2_type_for_size (TYPE_PRECISION (t1), 1);
  if (TREE_CODE (t2) == ENUMERAL_TYPE)
    t2 = gm2_type_for_size (TYPE_PRECISION (t2), 1);

  /* If both types are BOOLEAN_TYPE, then return boolean_type_node.  */
  if (TREE_CODE (t1) == BOOLEAN_TYPE
      && TREE_CODE (t2) == BOOLEAN_TYPE)
    return boolean_type_node;

  /* If either type is BOOLEAN_TYPE, then return the other.  */
  if (TREE_CODE (t1) == BOOLEAN_TYPE)
    return t2;
  if (TREE_CODE (t2) == BOOLEAN_TYPE)
    return t1;

  return c_common_type (t1, t2);
}

/* Returns the base type of an ordinal subrange, or the type
 * itself if it is not a subrange.
 */
tree
base_type (tree type)
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
comptypes (tree type1, tree type2)
{
  tree t1 = type1;
  tree t2 = type2;
  int attrval, val;

  /* Suppress errors caused by previously reported errors.  */

  if (t1 == t2 || !t1 || !t2
      || TREE_CODE (t1) == ERROR_MARK || TREE_CODE (t2) == ERROR_MARK)
    return 1;

  /* If either type is the internal version of sizetype, return the
     language version.  */
  if (TREE_CODE (t1) == INTEGER_TYPE && TYPE_IS_SIZETYPE (t1)
      && TYPE_DOMAIN (t1) != 0)
    t1 = TYPE_DOMAIN (t1);

  if (TREE_CODE (t2) == INTEGER_TYPE && TYPE_IS_SIZETYPE (t2)
      && TYPE_DOMAIN (t2) != 0)
    t2 = TYPE_DOMAIN (t2);

  /* Treat an enum type as the integer type of the same width and 
     signedness.  */

  if (TREE_CODE (t1) == ENUMERAL_TYPE)
    t1 = gm2_type_for_size (TYPE_PRECISION (t1), TYPE_UNSIGNED (t1));
  if (TREE_CODE (t2) == ENUMERAL_TYPE)
    t2 = gm2_type_for_size (TYPE_PRECISION (t2), TYPE_UNSIGNED (t2));

  if (t1 == t2)
    return 1;

  /* Different classes of types can't be compatible.  */

  if (TREE_CODE (t1) != TREE_CODE (t2)) return 0;

  /* Qualifiers must match.  */

  if (TYPE_QUALS (t1) != TYPE_QUALS (t2))
    return 0;

  /* Allow for two different type nodes which have essentially the same
     definition.  Note that we already checked for equality of the type
     qualifiers (just above).  */

  if (TYPE_MAIN_VARIANT (t1) == TYPE_MAIN_VARIANT (t2))
    return 1;

  /* 1 if no need for warning yet, 2 if warning cause has been seen.  */
  if (! (attrval = (*targetm.comp_type_attributes) (t1, t2)))
     return 0;

  /* 1 if no need for warning yet, 2 if warning cause has been seen.  */
  val = 0;

  switch (TREE_CODE (t1))
    {
    case POINTER_TYPE:
      val = (TREE_TYPE (t1) == TREE_TYPE (t2)
              ? 1 : comptypes (TREE_TYPE (t1), TREE_TYPE (t2)));
      break;

    case FUNCTION_TYPE:
      val = function_types_compatible_p (t1, t2);
      break;

    case ARRAY_TYPE:
      {
        tree d1 = TYPE_DOMAIN (t1);
        tree d2 = TYPE_DOMAIN (t2);
        bool d1_variable, d2_variable;
        bool d1_zero, d2_zero;
        val = 1;

        /* Target types must match incl. qualifiers.  */
        if (TREE_TYPE (t1) != TREE_TYPE (t2)
            && 0 == (val = comptypes (TREE_TYPE (t1), TREE_TYPE (t2))))
          return 0;

        /* Sizes must match unless one is missing or variable.  */
        if (d1 == 0 || d2 == 0 || d1 == d2)
          break;

        d1_zero = ! TYPE_MAX_VALUE (d1);
        d2_zero = ! TYPE_MAX_VALUE (d2);

        d1_variable = (! d1_zero
                       && (TREE_CODE (TYPE_MIN_VALUE (d1)) != INTEGER_CST
                           || TREE_CODE (TYPE_MAX_VALUE (d1)) != INTEGER_CST));
        d2_variable = (! d2_zero
                       && (TREE_CODE (TYPE_MIN_VALUE (d2)) != INTEGER_CST
                           || TREE_CODE (TYPE_MAX_VALUE (d2)) != INTEGER_CST));

        if (d1_variable || d2_variable)
          break;
        if (d1_zero && d2_zero)
          break;
        if (d1_zero || d2_zero
            || ! tree_int_cst_equal (TYPE_MIN_VALUE (d1), TYPE_MIN_VALUE (d2))
            || ! tree_int_cst_equal (TYPE_MAX_VALUE (d1), TYPE_MAX_VALUE (d2)))
          val = 0;

        break;
      }

    case RECORD_TYPE:
      if (maybe_objc_comptypes (t1, t2, 0) == 1)
        val = 1;
      break;

    default:
      break;
    }
  return attrval == 2 && val == 1 ? 2 : val;
}

/* FROM ../c-typeck.c IMPORT */

/* Return 1 if two function types F1 and F2 are compatible.
   If either type specifies no argument types,
   the other must specify a fixed number of self-promoting arg types.
   Otherwise, if one type specifies only the number of arguments, 
   the other must specify that number of self-promoting arg types.
   Otherwise, the argument types must match.  */

static int
function_types_compatible_p (tree f1, tree f2)
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

  /* An unspecified parmlist matches any specified parmlist
     whose argument types don't need default promotions.  */

  if (args1 == 0)
    {
      if (!self_promoting_args_p (args2))
        return 0;
      /* If one of these types comes from a non-prototype fn definition,
         compare that with the other type's arglist.
         If they don't match, ask for a warning (but no error).  */
      if (TYPE_ACTUAL_ARG_TYPES (f1)
          && 1 != type_lists_compatible_p (args2, TYPE_ACTUAL_ARG_TYPES (f1)))
        val = 2;
      return val;
    }
  if (args2 == 0)
    {
      if (!self_promoting_args_p (args1))
        return 0;
      if (TYPE_ACTUAL_ARG_TYPES (f2)
          && 1 != type_lists_compatible_p (args1, TYPE_ACTUAL_ARG_TYPES (f2)))
        val = 2;
      return val;
    }

  /* Both types have argument lists: compare them and propagate results.  */
  val1 = type_lists_compatible_p (args1, args2);
  return val1 != 1 ? val1 : val;
}

/* FROM ../c-typeck.c IMPORT */

/* Given a type, apply default promotions wrt unnamed function
   arguments and return the new type.  */

tree
c_type_promotes_to (tree type)
{
  if (TYPE_MAIN_VARIANT (type) == float_type_node)
    return double_type_node;

  if (c_promoting_integer_type_p (type))
    {
      /* Preserve unsignedness if not really getting any wider.  */
      if (TYPE_UNSIGNED (type)
          && (TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node)))
        return unsigned_type_node;
      return integer_type_node;
    }

  return type;
}

/* from c-decl.c */

/* Returns the stmt_tree (if any) to which statements are currently
   being added.  If there is no active statement-tree, NULL is
   returned.  */

stmt_tree
current_stmt_tree (void)
{
  return &c_stmt_tree;
}

tree
add_stmt (tree t)
{
  enum tree_code code = TREE_CODE (t);

  if (EXPR_P (t) && code != LABEL_EXPR)
    {
      if (!EXPR_HAS_LOCATION (t))
        SET_EXPR_LOCATION (t, input_location);
    }

  if (code == LABEL_EXPR || code == CASE_LABEL_EXPR)
    STATEMENT_LIST_HAS_LABEL (cur_stmt_list) = 1;

  /* Add T to the statement-tree.  Non-side-effect statements need to be
     recorded during statement expressions.  */
  append_to_statement_list_force (t, &cur_stmt_list);

  return t;
}

/* FROM ../c-typeck.c IMPORT */

/* Check two lists of types for compatibility,
   returning 0 for incompatible, 1 for compatible,
   or 2 for compatible with warning.  */

static int
type_lists_compatible_p (tree args1, tree args2)
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
      /* A null pointer instead of a type
         means there is supposed to be an argument
         but nothing is specified about what type it has.
         So match anything that self-promotes.  */
      if (TREE_VALUE (args1) == 0)
        {
          if (c_type_promotes_to (TREE_VALUE (args2)) != TREE_VALUE (args2))
            return 0;
        }
      else if (TREE_VALUE (args2) == 0)
        {
          if (c_type_promotes_to (TREE_VALUE (args1)) != TREE_VALUE (args1))
            return 0;
        }
      else if (! (newval = comptypes (TYPE_MAIN_VARIANT (TREE_VALUE (args1)), 
                                      TYPE_MAIN_VARIANT (TREE_VALUE (args2)))))
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

/* Warn about storing in something that is `const'.  */

void
readonly_warning (tree arg, const char *string)
{
  char buf[80];
  strcpy (buf, string);

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

int
is_of_string_type (tree type, int warn ATTRIBUTE_UNUSED)
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
is_string_type (tree string, int warn)
{
  if (TREE_CODE (string) == STRING_CST)
    return 1;

  return is_of_string_type (TREE_TYPE (string), warn);
}


/* Return either DECL or its known constant value (if it has one).  */

tree
decl_constant_value (tree decl)
{
  if (/* Don't change a variable array bound or initial value to a constant
         in a place where a variable is invalid.  */
      current_function_decl != 0
      && ! pedantic
      && ! TREE_THIS_VOLATILE (decl)
      && TREE_READONLY (decl)
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
   Lvalues can have their address taken, unless they have C_DECL_REGISTER.  */

static int
lvalue_p (tree ref)
{
  enum tree_code code = TREE_CODE (ref);

  switch (code)
    {
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case COMPONENT_REF:
      return lvalue_p (TREE_OPERAND (ref, 0));

    case COMPOUND_LITERAL_EXPR:
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
      return TREE_CODE (TREE_TYPE (ref)) == ARRAY_TYPE;

    default:
      return 0;
    }
}

/* Build the result of __builtin_offsetof.  EXPR is a nested sequence of
   component references, with an INDIRECT_REF at the bottom; much like
   the traditional rendering of offsetof as a macro.  Returns the folded
   and properly cast result.  */

static tree
fold_offsetof_1 (tree expr)
{
  enum tree_code code = PLUS_EXPR;
  tree base, off, t;

  switch (TREE_CODE (expr))
    {
    case ERROR_MARK:
      return expr;

    case INDIRECT_REF:
      return size_zero_node;

    case COMPONENT_REF:
      base = fold_offsetof_1 (TREE_OPERAND (expr, 0));
      if (base == error_mark_node)
        return base;

      t = TREE_OPERAND (expr, 1);
      if (DECL_C_BIT_FIELD (t))
        {
          error ("attempt to take address of bit-field structure "
                 "member %qD", t);
          return error_mark_node;
        }
      off = size_binop (PLUS_EXPR, DECL_FIELD_OFFSET (t),
                        size_int (tree_low_cst (DECL_FIELD_BIT_OFFSET (t), 1)
                                  / BITS_PER_UNIT));
      break;

    case ARRAY_REF:
      base = fold_offsetof_1 (TREE_OPERAND (expr, 0));
      if (base == error_mark_node)
        return base;

      t = TREE_OPERAND (expr, 1);
      if (TREE_CODE (t) == INTEGER_CST && tree_int_cst_sgn (t) < 0)
        {
          code = MINUS_EXPR;
          t = fold_build1 (NEGATE_EXPR, TREE_TYPE (t), t);
        }
      t = convert (sizetype, t);
      off = size_binop (MULT_EXPR, TYPE_SIZE_UNIT (TREE_TYPE (expr)), t);
      break;

    default:
      gcc_unreachable ();
    }

  return size_binop (code, base, off);
}

tree
fold_offsetof (tree expr)
{
  /* Convert back from the internal sizetype to size_t.  */
  return convert (size_type_node, fold_offsetof_1 (expr));
}

/* Print an error message for an invalid lvalue.  USE says
   how the lvalue is being used and so selects the error message.  */

void
lvalue_error (enum lvalue_use use)
{
  switch (use)
    {
    case lv_assign:
      error ("invalid lvalue in assignment");
      break;
    case lv_increment:
      error ("invalid lvalue in increment");
      break;
    case lv_decrement:
      error ("invalid lvalue in decrement");
      break;
    case lv_addressof:
      error ("invalid lvalue in unary %<&%>");
      break;
    case lv_asm:
      error ("invalid lvalue in asm statement");
      break;
    default:
      gcc_unreachable ();
    }
}

/* Return nonzero if REF is an lvalue valid for this language;
   otherwise, print an error message and return zero.  USE says
   how the lvalue is being used and so selects the error message.  */

static int
lvalue_or_else (tree ref, enum lvalue_use use)
{
  int win = lvalue_p (ref);

  if (!win)
    lvalue_error (use);

  return win;
}

/* Nonzero if the type T promotes to int.  This is (nearly) the
   integral promotions defined in ISO C99 6.3.1.1/2.  */

bool
c_promoting_integer_type_p (tree t)
{
  switch (TREE_CODE (t))
    {
    case INTEGER_TYPE:
      return (TYPE_MAIN_VARIANT (t) == char_type_node
              || TYPE_MAIN_VARIANT (t) == signed_char_type_node
              || TYPE_MAIN_VARIANT (t) == unsigned_char_type_node
              || TYPE_MAIN_VARIANT (t) == short_integer_type_node
              || TYPE_MAIN_VARIANT (t) == short_unsigned_type_node);

    case ENUMERAL_TYPE:
      /* ??? Technically all enumerations not larger than an int
         promote to an int.  But this is used along code paths
         that only want to notice a size change.  */
      return TYPE_PRECISION (t) < TYPE_PRECISION (integer_type_node);

    case BOOLEAN_TYPE:
      return 1;

    default:
      return 0;
    }
}

/* Return either DECL or its known constant value (if it has one), but
   return DECL if pedantic or DECL has mode BLKmode.  This is for
   bug-compatibility with the old behavior of decl_constant_value
   (before GCC 3.0); every use of this function is a bug and it should
   be removed before GCC 3.1.  It is not appropriate to use pedantic
   in a way that affects optimization, and BLKmode is probably not the
   right test for avoiding misoptimizations either.  */

static tree
decl_constant_value_for_broken_optimization (tree decl)
{
  if (pedantic || DECL_MODE (decl) == BLKmode)
    return decl;
  else
    return decl_constant_value (decl);
}

/* EXP is an expression of integer type.  Apply the integer promotions
   to it and return the promoted value.  */

tree
perform_integral_promotions (tree exp)
{
  tree type = TREE_TYPE (exp);
#if defined(GM2)
  enum tree_code code = TREE_CODE (base_type (type));
#else
  enum tree_code code = TREE_CODE (type);
#endif

  gcc_assert (INTEGRAL_TYPE_P (type));

  /* Normally convert enums to int,
     but convert wide enums to something wider.  */
  if (code == ENUMERAL_TYPE)
    {
      type = gm2_type_for_size (MAX (TYPE_PRECISION (type),
                                     TYPE_PRECISION (integer_type_node)),
                                ((TYPE_PRECISION (type)
                                  >= TYPE_PRECISION (integer_type_node))
                                 && TYPE_UNSIGNED (type)));

      return convert (type, exp);
    }

  if (c_promoting_integer_type_p (type))
    {
      /* Preserve unsignedness if not really getting any wider.  */
      if (TYPE_UNSIGNED (type)
          && TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node))
        return convert (unsigned_type_node, exp);

      return convert (integer_type_node, exp);
    }

  return exp;
}

/* Perform default promotions for C data used in expressions.
   Arrays and functions are converted to pointers;
   enumeral types or short or char, to int.
   In addition, manifest constants symbols are replaced by their values.  */

tree
default_conversion (tree exp)
{
  tree orig_exp;
  tree type = TREE_TYPE (exp);
#if defined(GM2)
  enum tree_code code = TREE_CODE (base_type (type));
#else
  enum tree_code code = TREE_CODE (type);
#endif

  if (code == FUNCTION_TYPE || code == ARRAY_TYPE)
    return default_function_array_conversion (exp);

  /* Constants can be used directly unless they're not loadable.  */
  if (TREE_CODE (exp) == CONST_DECL)
    exp = DECL_INITIAL (exp);

  /* Replace a nonvolatile const static variable with its value unless
     it is an array, in which case we must be sure that taking the
     address of the array produces consistent results.  */
  else if (optimize && TREE_CODE (exp) == VAR_DECL && code != ARRAY_TYPE)
    {
      exp = decl_constant_value_for_broken_optimization (exp);
      type = TREE_TYPE (exp);
    }

  /* Strip no-op conversions.  */
  orig_exp = exp;
  STRIP_TYPE_NOPS (exp);

  if (TREE_NO_WARNING (orig_exp))
    TREE_NO_WARNING (exp) = 1;

  if (INTEGRAL_TYPE_P (type))
    return perform_integral_promotions (exp);

  if (code == VOID_TYPE)
    {
      error ("void value not ignored as it ought to be");
      return error_mark_node;
    }
  return exp;
}

/* Return 1 if TTL and TTR are pointers to types that are equivalent,
   ignoring their qualifiers.  */

static int
comp_target_types (tree ttl, tree ttr)
{
  int val;
  tree mvl, mvr;

  /* Do not lose qualifiers on element types of array types that are
     pointer targets by taking their TYPE_MAIN_VARIANT.  */
  mvl = TREE_TYPE (ttl);
  mvr = TREE_TYPE (ttr);
  if (TREE_CODE (mvl) != ARRAY_TYPE)
    mvl = TYPE_MAIN_VARIANT (mvl);
  if (TREE_CODE (mvr) != ARRAY_TYPE)
    mvr = TYPE_MAIN_VARIANT (mvr);
  val = comptypes (mvl, mvr);

  if (val == 2 && pedantic)
    pedwarn ("types are not quite compatible");
  return val;
}

/*
 *  convert_set - returns NULL if no set was found,
 *                otherwise convert set to type.
 */

tree
convert_set (tree type, tree expr)
{
  tree intype            = TREE_TYPE (expr);
  unsigned int inprec    = TYPE_PRECISION (intype);
  unsigned int outprec   = TYPE_PRECISION (type);

  switch (TREE_CODE (intype))
    {
    case SET_TYPE:
      /* If we are widening the type, put in an explicit conversion.
         Similarly if we are not changing the width.  After this, we know
         we are truncating EXPR.  */

      if (outprec >= inprec)
        return build1 (NOP_EXPR, type, expr);

      /* If TYPE is an enumeral type or a type with a precision less
         than the number of bits in its mode, do the conversion to the
         type corresponding to its mode, then do a nop conversion
         to TYPE.  */
      else if (TREE_CODE (type) == ENUMERAL_TYPE
               || outprec != GET_MODE_BITSIZE (TYPE_MODE (type)))
        return build1 (NOP_EXPR, type,
                       convert (gm2_type_for_mode (TYPE_MODE (type),
                                                   TYPE_UNSIGNED (type)),
                                expr));
      else
        return build1 (NOP_EXPR, type, expr);

    default:
      return NULL_TREE;
    }
}

/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.  */

tree
convert (tree type, tree expr)
{
  tree e = expr;
  enum tree_code code = TREE_CODE (type);

  if (type == error_mark_node
      || expr == error_mark_node
      || TREE_TYPE (expr) == error_mark_node)
    return error_mark_node;

  if (type == TREE_TYPE (expr))
    return expr;
#if 0
  /* was */
  if (type == TREE_TYPE (expr)
      || TREE_CODE (expr) == ERROR_MARK
      || code == ERROR_MARK || TREE_CODE (TREE_TYPE (expr)) == ERROR_MARK)
    return expr;
#endif

  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (expr)))
    return fold_build1 (NOP_EXPR, type, expr);
  if (TREE_CODE (TREE_TYPE (expr)) == ERROR_MARK)
    return error_mark_node;
  if (TREE_CODE (TREE_TYPE (expr)) == VOID_TYPE)
    {
      error ("void value not ignored as it ought to be");
      return error_mark_node;
    }
  if (code == VOID_TYPE)
    return build1 (CONVERT_EXPR, type, e);
#if 0
  /* This is incorrect.  A truncation can't be stripped this way.
     Extensions will be stripped by the use of get_unwidened.  */
  if (TREE_CODE (expr) == NOP_EXPR)
    return convert (type, TREE_OPERAND (expr, 0));
#endif

#if defined(GM2)
  /* check for set type conversion */
  if ((TREE_TYPE (expr) != NULL_TREE) && (TREE_CODE (TREE_TYPE (expr)) == SET_TYPE))
    return fold (convert_set (type, expr));
#endif

  if (code == INTEGER_TYPE || code == ENUMERAL_TYPE)
    return fold (convert_to_integer (type, e));
  if (code == BOOLEAN_TYPE)
    {
      tree t = gm2_truthvalue_conversion (expr);

      /* If it returns a NOP_EXPR, we must fold it here to avoid
         infinite recursion between fold () and convert ().  */
      if (TREE_CODE (t) == NOP_EXPR)
        return fold_build1 (NOP_EXPR, type, TREE_OPERAND (t, 0));
      else
        return fold_build1 (NOP_EXPR, type, t);
    }
  if (code == POINTER_TYPE || code == REFERENCE_TYPE)
    return fold (convert_to_pointer (type, e));
  if (code == REAL_TYPE)
    return fold (convert_to_real (type, e));
  if (code == COMPLEX_TYPE)
    return fold (convert_to_complex (type, e));
  if (code == VECTOR_TYPE)
    return fold (convert_to_vector (type, e));

#if defined(GM2)
  if (code == CHAR_TYPE)
    {
      tree intype = TREE_TYPE (e);
      enum tree_code form = TREE_CODE (intype);
      if (form == CHAR_TYPE)
        return fold (build1 (NOP_EXPR, type, e));
      /* @@ If it does not fit? */
      if (ORDINAL_TYPE (form))
        return fold (build1 (CONVERT_EXPR, type, e));
      error ("cannot convert to a char type");
      return error_mark_node;
    }

  if (code == SET_TYPE)
    return expr;
#endif

  error ("conversion to non-scalar type requested");
  return error_mark_node;
}

/* Print a warning if a constant expression had overflow in folding.
   Invoke this function on every expression that the language
   requires to be a constant expression.
   Note the ANSI C standard says it is erroneous for a
   constant expression to overflow.  */

void
constant_expression_warning (tree value)
{
  if ((TREE_CODE (value) == INTEGER_CST || TREE_CODE (value) == REAL_CST
       || TREE_CODE (value) == COMPLEX_CST)
      && TREE_CONSTANT_OVERFLOW (value))
    pedwarn ("overflow in constant expression");
}

/* Print a warning if an expression had overflow in folding.
   Invoke this function on every expression that
   (1) appears in the source code, and
   (2) might be a constant expression that overflowed, and
   (3) is not already checked by convert_and_check;
   however, do not invoke this function on operands of explicit casts.  */

void
overflow_warning (tree value)
{
  if ((TREE_CODE (value) == INTEGER_CST
       || (TREE_CODE (value) == COMPLEX_CST
           && TREE_CODE (TREE_REALPART (value)) == INTEGER_CST))
      && TREE_OVERFLOW (value))
    {
      TREE_OVERFLOW (value) = 0;
      if (skip_evaluation == 0)
        warning (0, "integer overflow in expression");
    }
  else if ((TREE_CODE (value) == REAL_CST
            || (TREE_CODE (value) == COMPLEX_CST
                && TREE_CODE (TREE_REALPART (value)) == REAL_CST))
           && TREE_OVERFLOW (value))
    {
      TREE_OVERFLOW (value) = 0;
      if (skip_evaluation == 0)
        warning (0, "floating point overflow in expression");
    }
  else if (TREE_CODE (value) == VECTOR_CST && TREE_OVERFLOW (value))
    {
      TREE_OVERFLOW (value) = 0;
      if (skip_evaluation == 0)
        warning (0, "vector overflow in expression");
    }
}

/* FROM ../c-common.c IMPORT */

/* Print an error if a large constant is truncated to unsigned,
   or a constant < 0 is converted to unsigned.
   Invoke this function on every expression that might be implicitly
   converted to an unsigned type.  */

void
gm2_unsigned_conversion_error (tree result, tree operand, tree totype)
{
  tree type = TREE_TYPE (result);

  if (TREE_CODE (operand) == INTEGER_CST
      && TREE_CODE (type) == INTEGER_TYPE
      && TYPE_UNSIGNED (type)
      && skip_evaluation == 0
      && !int_fits_type_p (operand, type))
    error ("constant overflow");
}

/* Nonzero if constant C has a value that is permissible
   for type TYPE (an INTEGER_TYPE).  */

static int
constant_fits_type_p (tree c, tree type)
{
  if (TREE_CODE (c) == INTEGER_CST)
    return int_fits_type_p (c, type);

  c = convert (type, c);
  return !TREE_OVERFLOW (c);
}

/* Convert EXPR to TYPE, warning about conversion problems with constants.
   Invoke this function on every expression that is converted implicitly,
   i.e. because of language rules and not because of an explicit cast.  */

tree
convert_and_check (tree type, tree expr)
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
          if (!(TYPE_UNSIGNED (type) < TYPE_UNSIGNED (TREE_TYPE (expr))
                && TREE_CODE (TREE_TYPE (expr)) == INTEGER_TYPE
                && TYPE_PRECISION (type) == TYPE_PRECISION (TREE_TYPE (expr))))
            /* If EXPR fits in the unsigned version of TYPE,
               don't warn unless pedantic.  */
            if ((pedantic
                 || TYPE_UNSIGNED (type)
                 || !constant_fits_type_p (expr,
                                           gm2_unsigned_type (type)))
                && skip_evaluation == 0)
              warning (0, "overflow in implicit constant conversion");
        }
      else
        gm2_unsigned_conversion_error (t, expr, type);
    }
  return t;
}

/* FROM ../c-typeck.c IMPORT */
/* Return the composite type of two compatible types.

   We assume that comptypes has already been done and returned
   nonzero; if that isn't so, this may crash.  In particular, we
   assume that qualifiers match.  */

tree
composite_type (tree t1, tree t2)
{
  enum tree_code code1;
  enum tree_code code2;
  tree attributes;

  /* Save time if the two types are the same.  */

  if (t1 == t2) return t1;

  /* If one type is nonsense, use the other.  */
  if (t1 == error_mark_node)
    return t2;
  if (t2 == error_mark_node)
    return t1;

  code1 = TREE_CODE (t1);
  code2 = TREE_CODE (t2);

  /* Merge the attributes.  */
  attributes = targetm.merge_type_attributes (t1, t2);

  /* If one is an enumerated type and the other is the compatible
     integer type, the composite type might be either of the two
     (DR#013 question 3).  For consistency, use the enumerated type as
     the composite type.  */

  if (code1 == ENUMERAL_TYPE && code2 == INTEGER_TYPE)
    return t1;
  if (code2 == ENUMERAL_TYPE && code1 == INTEGER_TYPE)
    return t2;

  gcc_assert (code1 == code2);

  switch (code1)
    {
    case POINTER_TYPE:
      /* For two pointers, do this recursively on the target type.  */
      {
        tree pointed_to_1 = TREE_TYPE (t1);
        tree pointed_to_2 = TREE_TYPE (t2);
        tree target = composite_type (pointed_to_1, pointed_to_2);
        t1 = build_pointer_type (target);
        t1 = build_type_attribute_variant (t1, attributes);
        return qualify_type (t1, t2);
      }
    default:
      return build_type_attribute_variant (t1, attributes);
    }
}

/* FROM ../c-typeck.c IMPORT */

/* Return the type of a conditional expression between pointers to
   possibly differently qualified versions of compatible types.

   We assume that comp_target_types has already been done and returned
   nonzero; if that isn't so, this may crash.  */

static tree
common_pointer_type (tree t1, tree t2)
{
  tree attributes;
  tree pointed_to_1, mv1;
  tree pointed_to_2, mv2;
  tree target;

  /* Save time if the two types are the same.  */

  if (t1 == t2) return t1;

  /* If one type is nonsense, use the other.  */
  if (t1 == error_mark_node)
    return t2;
  if (t2 == error_mark_node)
    return t1;

  gcc_assert (TREE_CODE (t1) == POINTER_TYPE
              && TREE_CODE (t2) == POINTER_TYPE);

  /* Merge the attributes.  */
  attributes = targetm.merge_type_attributes (t1, t2);

  /* Find the composite type of the target types, and combine the
     qualifiers of the two types' targets.  Do not lose qualifiers on
     array element types by taking the TYPE_MAIN_VARIANT.  */
  mv1 = pointed_to_1 = TREE_TYPE (t1);
  mv2 = pointed_to_2 = TREE_TYPE (t2);
  if (TREE_CODE (mv1) != ARRAY_TYPE)
    mv1 = TYPE_MAIN_VARIANT (pointed_to_1);
  if (TREE_CODE (mv2) != ARRAY_TYPE)
    mv2 = TYPE_MAIN_VARIANT (pointed_to_2);
  target = composite_type (mv1, mv2);
  t1 = build_pointer_type (c_build_qualified_type
                           (target,
                            TYPE_QUALS (pointed_to_1) |
                            TYPE_QUALS (pointed_to_2)));
  return build_type_attribute_variant (t1, attributes);
}

/* FROM ../c-typeck.c IMPORT */
/* Build and return a conditional expression IFEXP ? OP1 : OP2.  */

tree
build_conditional_expr (tree ifexp, tree op1, tree op2)
{
  tree type1;
  tree type2;
  enum tree_code code1;
  enum tree_code code2;
  tree result_type = NULL;
  tree orig_op1 = op1, orig_op2 = op2;

  ifexp = gm2_truthvalue_conversion (default_conversion (ifexp));

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
  else if ((code1 == INTEGER_TYPE || code1 == REAL_TYPE
            || code1 == COMPLEX_TYPE)
           && (code2 == INTEGER_TYPE || code2 == REAL_TYPE
               || code2 == COMPLEX_TYPE))
    {
      result_type = common_type (type1, type2);

      /* If -Wsign-compare, warn here if type1 and type2 have
         different signedness.  We'll promote the signed to unsigned
         and later code won't know it used to be different.
         Do this check on the original types, so that explicit casts
         will be considered, but default promotions won't.  */
      if (warn_sign_compare && !skip_evaluation)
        {
          int unsigned_op1 = TYPE_UNSIGNED (TREE_TYPE (orig_op1));
          int unsigned_op2 = TYPE_UNSIGNED (TREE_TYPE (orig_op2));

          if (unsigned_op1 ^ unsigned_op2)
            {
              /* Do not warn if the result type is signed, since the
                 signed type will only be chosen if it can represent
                 all the values of the unsigned type.  */
              if (!TYPE_UNSIGNED (result_type))
                /* OK */;
              /* Do not warn if the signed quantity is an unsuffixed
                 integer literal (or some static constant expression
                 involving such literals) and it is non-negative.  */
              else if ((unsigned_op2 && tree_expr_nonnegative_p (op1))
                       || (unsigned_op1 && tree_expr_nonnegative_p (op2)))
                /* OK */;
              else
                warning (0, "signed and unsigned type in conditional expression");
            }
        }
    }
  else if (code1 == VOID_TYPE || code2 == VOID_TYPE)
    {
      if (pedantic && (code1 != VOID_TYPE || code2 != VOID_TYPE))
        pedwarn ("ISO C forbids conditional expr with only one void side");
      result_type = void_type_node;
    }
  else if (code1 == POINTER_TYPE && code2 == POINTER_TYPE)
    {
      if (comp_target_types (type1, type2))
        result_type = common_pointer_type (type1, type2);
      else if (integer_zerop (op1) && TREE_TYPE (type1) == void_type_node
               && TREE_CODE (orig_op1) != NOP_EXPR)
        result_type = qualify_type (type2, type1);
      else if (integer_zerop (op2) && TREE_TYPE (type2) == void_type_node
               && TREE_CODE (orig_op2) != NOP_EXPR)
        result_type = qualify_type (type1, type2);
      else if (VOID_TYPE_P (TREE_TYPE (type1)))
        {
          if (pedantic && TREE_CODE (TREE_TYPE (type2)) == FUNCTION_TYPE)
            pedwarn ("ISO C forbids conditional expr between "
                     "%<void *%> and function pointer");
          result_type = build_pointer_type (qualify_type (TREE_TYPE (type1),
                                                          TREE_TYPE (type2)));
        }
      else if (VOID_TYPE_P (TREE_TYPE (type2)))
        {
          if (pedantic && TREE_CODE (TREE_TYPE (type1)) == FUNCTION_TYPE)
            pedwarn ("ISO C forbids conditional expr between "
                     "%<void *%> and function pointer");
          result_type = build_pointer_type (qualify_type (TREE_TYPE (type2),
                                                          TREE_TYPE (type1)));
        }
      else
        {
          pedwarn ("pointer type mismatch in conditional expression");
          result_type = build_pointer_type (void_type_node);
        }
    }
  else if (code1 == POINTER_TYPE && code2 == INTEGER_TYPE)
    {
      if (!integer_zerop (op2))
        pedwarn ("pointer/integer type mismatch in conditional expression");
      else
        {
          op2 = null_pointer_node;
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

  return fold_build3 (COND_EXPR, result_type, ifexp, op1, op2);
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
shorten_compare (tree *op0_ptr, tree *op1_ptr, tree *restype_ptr,
                 enum tree_code *rescode_ptr)
{
  tree type;
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
    unsignedp0 = TYPE_UNSIGNED (TREE_TYPE (op0));
  if (op1 == primop1 && TREE_TYPE (op1) != *restype_ptr)
    unsignedp1 = TYPE_UNSIGNED (TREE_TYPE (op1));

  /* If one of the operands must be floated, we cannot optimize.  */
  real1 = TREE_CODE (TREE_TYPE (primop0)) == REAL_TYPE;
  real2 = TREE_CODE (TREE_TYPE (primop1)) == REAL_TYPE;

  /* If first arg is constant, swap the args (changing operation
     so value is preserved), for canonicalization.  Don't do this if
     the second arg is 0.  */

  if (TREE_CONSTANT (primop0)
      && !integer_zerop (primop1) && !real_zerop (primop1))
    {
      tree tem = primop0;
      int temi = unsignedp0;
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
      int unsignedp = TYPE_UNSIGNED (*restype_ptr);
      tree val;

      type = gm2_signed_or_unsigned_type (unsignedp0,
                                          TREE_TYPE (primop0));

      maxval = TYPE_MAX_VALUE (type);
      minval = TYPE_MIN_VALUE (type);

      if (unsignedp && !unsignedp0)
        *restype_ptr = gm2_signed_type (*restype_ptr);

      if (TREE_TYPE (primop1) != *restype_ptr)
        {
          /* Convert primop1 to target type, but do not introduce
             additional overflow.  We know primop1 is an int_cst.  */
          tree tmp = build_int_cst_wide (*restype_ptr,
                                         TREE_INT_CST_LOW (primop1),
                                         TREE_INT_CST_HIGH (primop1));

          primop1 = force_fit_type (tmp, 0, TREE_OVERFLOW (primop1),
                                    TREE_CONSTANT_OVERFLOW (primop1));
        }
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
            val = truthvalue_true_node;
        }
      else if (code == EQ_EXPR)
        {
          if (max_lt || min_gt)
            val = truthvalue_false_node;
        }
      else if (code == LT_EXPR)
        {
          if (max_lt)
            val = truthvalue_true_node;
          if (!min_lt)
            val = truthvalue_false_node;
        }
      else if (code == GT_EXPR)
        {
          if (min_gt)
            val = truthvalue_true_node;
          if (!max_gt)
            val = truthvalue_false_node;
        }
      else if (code == LE_EXPR)
        {
          if (!max_gt)
            val = truthvalue_true_node;
          if (min_gt)
            val = truthvalue_false_node;
        }
      else if (code == GE_EXPR)
        {
          if (!min_lt)
            val = truthvalue_true_node;
          if (max_lt)
            val = truthvalue_false_node;
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
          type = gm2_unsigned_type (type);
        }

      if (TREE_CODE (primop0) != INTEGER_CST)
        {
          if (val == truthvalue_false_node)
            warning (0, "comparison is always false due to limited range of data type");
          if (val == truthvalue_true_node)
            warning (0, "comparison is always true due to limited range of data type");
        }

      if (val != 0)
        {
          /* Don't forget to evaluate PRIMOP0 if it has side effects.  */
          if (TREE_SIDE_EFFECTS (primop0))
            return build2 (COMPOUND_EXPR, TREE_TYPE (val), primop0, val);
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
      type = gm2_signed_or_unsigned_type (unsignedp0
                                          || TYPE_UNSIGNED (*restype_ptr),
                                          type);
      /* Make sure shorter operand is extended the right way
         to match the longer operand.  */
      primop0
        = convert (gm2_signed_or_unsigned_type (unsignedp0,
                                                TREE_TYPE (primop0)),
                   primop0);
      primop1
        = convert (gm2_signed_or_unsigned_type (unsignedp1,
                                                TREE_TYPE (primop1)),
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
          && TYPE_UNSIGNED (*restype_ptr))
        {
          tree value = 0;
          switch (code)
            {
            case GE_EXPR:
              /* All unsigned values are >= 0, so we warn if extra warnings
                 are requested.  However, if OP0 is a constant that is
                 >= 0, the signedness of the comparison isn't an issue,
                 so suppress the warning.  */
              if (extra_warnings && !in_system_header
                  && !(TREE_CODE (primop0) == INTEGER_CST
                       && !TREE_OVERFLOW (convert (gm2_signed_type (type),
                                                   primop0))))
                warning (0, "comparison of unsigned expression >= 0 is always true");
              value = truthvalue_true_node;
              break;

            case LT_EXPR:
              if (extra_warnings && !in_system_header
                  && !(TREE_CODE (primop0) == INTEGER_CST
                       && !TREE_OVERFLOW (convert (gm2_signed_type (type),
                                                   primop0))))
                warning (0, "comparison of unsigned expression < 0 is always false");
              value = truthvalue_false_node;
              break;

            default:
              break;
            }

          if (value != 0)
            {
              /* Don't forget to evaluate PRIMOP0 if it has side effects.  */
              if (TREE_SIDE_EFFECTS (primop0))
                return build2 (COMPOUND_EXPR, TREE_TYPE (value),
                               primop0, value);
              return value;
            }
        }
    }

  *op0_ptr = convert (type, primop0);
  *op1_ptr = convert (type, primop1);

  *restype_ptr = truthvalue_type_node;

  return 0;
}

/* Print an error message for invalid use of an incomplete type.
   VALUE is the expression that was used (or 0 if that isn't known)
   and TYPE is the type that was invalid.  */

void
gm2_incomplete_type_error (tree value, tree type)
{
  const char *type_code_string;

  /* Avoid duplicate error message.  */
  if (TREE_CODE (type) == ERROR_MARK)
    return;

  if (value != 0 && (TREE_CODE (value) == VAR_DECL
                     || TREE_CODE (value) == PARM_DECL))
    error ("%sT has an incomplete type",
           IDENTIFIER_POINTER (DECL_NAME (value)));
  else
    {
    retry:
      /* We must print an error message.  Be clever about what it says.  */

      switch (TREE_CODE (type))
        {
        case RECORD_TYPE:
          type_code_string = "struct";
          break;

        case UNION_TYPE:
          type_code_string = "union";
          break;

        case ENUMERAL_TYPE:
          type_code_string = "enum";
          break;

        case VOID_TYPE:
          error ("invalid use of void expression");
          return;

        case ARRAY_TYPE:
          if (TYPE_DOMAIN (type))
            {
              if (TYPE_MAX_VALUE (TYPE_DOMAIN (type)) == NULL)
                {
                  error ("invalid use of flexible array member");
                  return;
                }
              type = TREE_TYPE (type);
              goto retry;
            }
          error ("invalid use of array with unspecified bounds");
          return;

        default:
          abort ();
        }

      if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
        error ("invalid use of undefined type `%s %s'",
               type_code_string, IDENTIFIER_POINTER (TYPE_NAME (type)));
      else
        /* If this type has a typedef-name, the TYPE_NAME is a TYPE_DECL.  */
        error ("invalid use of incomplete typedef `%s'",
               IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type))));
    }
}

/* FROM ../ IMPORT */
/* Do `exp = require_complete_type (exp);' to make sure exp
   does not have an incomplete type.  (That includes void types.)  */

tree
require_complete_type (tree value)
{
  tree type = TREE_TYPE (value);

  if (value == error_mark_node || type == error_mark_node)
    return error_mark_node;

  /* First, detect a valid value with a complete type.  */
  if (COMPLETE_TYPE_P (type))
    return value;

  gm2_incomplete_type_error (value, type);
  return error_mark_node;
}

/* Convert value RHS to type TYPE as preparation for an assignment
   to an lvalue of type TYPE.

   This is currently dummy routine in Modula-2.
*/

static tree
convert_for_assignment (tree type, tree rhs, enum impl_conv errtype ATTRIBUTE_UNUSED,
                        tree fundecl ATTRIBUTE_UNUSED, tree function ATTRIBUTE_UNUSED,
                        int parmnum ATTRIBUTE_UNUSED)
{
  return convert (type, rhs);
}

/* Build an assignment expression of lvalue LHS from value RHS.
   MODIFYCODE is the code for a binary operator that we use
   to combine the old value of LHS with RHS to get the new value.
   Or else MODIFYCODE is NOP_EXPR meaning do a simple assignment.  */

tree
build_modify_expr (tree lhs, enum tree_code modifycode, tree rhs)
{
  tree result;
  tree newrhs;
  tree lhstype = TREE_TYPE (lhs);
  tree olhstype = lhstype;

  /* Types that aren't fully specified cannot be used in assignments.  */
  lhs = require_complete_type (lhs);

  /* Avoid duplicate error messages from operands that had errors.  */
  if (TREE_CODE (lhs) == ERROR_MARK || TREE_CODE (rhs) == ERROR_MARK)
    return error_mark_node;

  STRIP_TYPE_NOPS (rhs);

  newrhs = rhs;

  /* If a binary op has been requested, combine the old LHS value with the RHS
     producing the value we should actually store into the LHS.  */

  if (modifycode != NOP_EXPR)
    {
      lhs = stabilize_reference (lhs);
      newrhs = build_binary_op (modifycode, lhs, rhs, 1);
    }

  if (!lvalue_or_else (lhs, lv_assign))
    return error_mark_node;

  /* Give an error for storing in something that is 'const'.  */

  if (TREE_READONLY (lhs) || TYPE_READONLY (lhstype)
      || ((TREE_CODE (lhstype) == RECORD_TYPE
           || TREE_CODE (lhstype) == UNION_TYPE)
          && C_TYPE_FIELDS_READONLY (lhstype)))
    readonly_error (lhs, lv_assign);

  /* If storing into a structure or union member,
     it has probably been given type `int'.
     Compute the type that would go with
     the actual amount of storage the member occupies.  */

  if (TREE_CODE (lhs) == COMPONENT_REF
      && (TREE_CODE (lhstype) == INTEGER_TYPE
          || TREE_CODE (lhstype) == BOOLEAN_TYPE
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

  newrhs = convert_for_assignment (lhstype, newrhs, ic_assign,
                                   NULL_TREE, NULL_TREE, 0);
  if (TREE_CODE (newrhs) == ERROR_MARK)
    return error_mark_node;

#if !defined(GM2)
  /* Emit ObjC write barrier, if necessary.  */
  if (c_dialect_objc () && flag_objc_gc)
    {
      result = objc_generate_write_barrier (lhs, modifycode, newrhs);
      if (result)
        return result;
    }
#endif

  /* Scan operands.  */

  result = build2 (MODIFY_EXPR, lhstype, lhs, newrhs);
  TREE_SIDE_EFFECTS (result) = 1;

  /* If we got the LHS in a different type for storing in,
     convert the result back to the nominal type of LHS
     so that the value we return always has the same type
     as the LHS argument.  */

  if (olhstype == TREE_TYPE (result))
    return result;
  return convert_for_assignment (olhstype, result, ic_assign,
                                 NULL_TREE, NULL_TREE, 0);
}

/* Make a variant type in the proper way for C/C++, propagating qualifiers
   down to the element type of an array.  */

tree
c_build_qualified_type (tree type, int type_quals)
{
  /* A restrict-qualified pointer type must be a pointer to object or
     incomplete type.  Note that the use of POINTER_TYPE_P also allows
     REFERENCE_TYPEs, which is appropriate for C++.  Unfortunately,
     the C++ front-end also use POINTER_TYPE for pointer-to-member
     values, so even though it should be illegal to use `restrict'
     with such an entity we don't flag that here.  Thus, special case
     code for that case is required in the C++ front-end.  */
  if ((type_quals & TYPE_QUAL_RESTRICT)
      && (!POINTER_TYPE_P (type)
          || !C_TYPE_OBJECT_OR_INCOMPLETE_P (TREE_TYPE (type))))
    {
      error ("invalid use of `restrict'");
      type_quals &= ~TYPE_QUAL_RESTRICT;
    }

  if (TREE_CODE (type) == ARRAY_TYPE)
    return build_array_type (c_build_qualified_type (TREE_TYPE (type),
                                                     type_quals),
                             TYPE_DOMAIN (type));
  return build_qualified_type (type, type_quals);
}

/* Given a boolean expression ARG, return a tree representing an increment
   or decrement (as indicated by CODE) of ARG.  The front end must check for
   invalid cases (e.g., decrement in C++).  */
tree
boolean_increment (enum tree_code code, tree arg)
{
  tree val;
  tree true_res = boolean_true_node;
  arg = stabilize_reference (arg);
  switch (code)
    {
    case PREINCREMENT_EXPR:
      val = build (MODIFY_EXPR, TREE_TYPE (arg), arg, true_res);
      break;
    case POSTINCREMENT_EXPR:
      val = build (MODIFY_EXPR, TREE_TYPE (arg), arg, true_res);
      arg = save_expr (arg);
      val = build (COMPOUND_EXPR, TREE_TYPE (arg), val, arg);
      val = build (COMPOUND_EXPR, TREE_TYPE (arg), arg, val);
      break;
    case PREDECREMENT_EXPR:
      val = build (MODIFY_EXPR, TREE_TYPE (arg), arg, invert_truthvalue (arg));
      break;
    case POSTDECREMENT_EXPR:
      val = build (MODIFY_EXPR, TREE_TYPE (arg), arg, invert_truthvalue (arg));
      arg = save_expr (arg);
      val = build (COMPOUND_EXPR, TREE_TYPE (arg), val, arg);
      val = build (COMPOUND_EXPR, TREE_TYPE (arg), arg, val);
      break;
    default:
      abort ();
    }
  TREE_SIDE_EFFECTS (val) = 1;
  return val;
}

/* FROM c-typeck.c IMPORT .. */

/* Give an error for storing in something that is 'const'.  */

static void
readonly_error (tree arg, enum lvalue_use use)
{
  gcc_assert (use == lv_assign || use == lv_increment || use == lv_decrement
              || use == lv_asm);
  /* Using this macro rather than (for example) arrays of messages
     ensures that all the format strings are checked at compile
     time.  */
#define READONLY_MSG(A, I, D, AS) (use == lv_assign ? (A)               \
                                   : (use == lv_increment ? (I)         \
                                   : (use == lv_decrement ? (D) : (AS))))
  if (TREE_CODE (arg) == COMPONENT_REF)
    {
      if (TYPE_READONLY (TREE_TYPE (TREE_OPERAND (arg, 0))))
        readonly_error (TREE_OPERAND (arg, 0), use);
      else
        error (READONLY_MSG (G_("assignment of read-only member %qD"),
                             G_("increment of read-only member %qD"),
                             G_("decrement of read-only member %qD"),
                             G_("read-only member %qD used as %<asm%> output")),
               TREE_OPERAND (arg, 1));
    }
  else if (TREE_CODE (arg) == VAR_DECL)
    error (READONLY_MSG (G_("assignment of read-only variable %qD"),
                         G_("increment of read-only variable %qD"),
                         G_("decrement of read-only variable %qD"),
                         G_("read-only variable %qD used as %<asm%> output")),
           arg);
  else
    error (READONLY_MSG (G_("assignment of read-only location"),
                         G_("increment of read-only location"),
                         G_("decrement of read-only location"),
                         G_("read-only location used as %<asm%> output")));
}

/* Prepare expr to be an argument of a TRUTH_NOT_EXPR,
   or for an `if' or `while' statement or ?..: exp.  It should already
   have been validated to be of suitable type; otherwise, a bad
   diagnostic may result.

   This preparation consists of taking the ordinary
   representation of an expression expr and producing a valid tree
   boolean expression describing whether expr is nonzero.  We could
   simply always do build_binary_op (NE_EXPR, expr, truthvalue_false_node, 1),
   but we optimize comparisons, &&, ||, and !.

   The resulting type should always be `truthvalue_type_node'.  */

tree
c_common_truthvalue_conversion (tree expr)
{
  switch (TREE_CODE (expr))
    {
    case EQ_EXPR:   case NE_EXPR:   case UNEQ_EXPR: case LTGT_EXPR:
    case LE_EXPR:   case GE_EXPR:   case LT_EXPR:   case GT_EXPR:
    case UNLE_EXPR: case UNGE_EXPR: case UNLT_EXPR: case UNGT_EXPR:
    case ORDERED_EXPR: case UNORDERED_EXPR:
      if (TREE_TYPE (expr) == truthvalue_type_node)
        return expr;
      return build2 (TREE_CODE (expr), truthvalue_type_node,
                     TREE_OPERAND (expr, 0), TREE_OPERAND (expr, 1));

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
      if (TREE_TYPE (expr) == truthvalue_type_node)
        return expr;
      return build2 (TREE_CODE (expr), truthvalue_type_node,
                 c_common_truthvalue_conversion (TREE_OPERAND (expr, 0)),
                 c_common_truthvalue_conversion (TREE_OPERAND (expr, 1)));

    case TRUTH_NOT_EXPR:
      if (TREE_TYPE (expr) == truthvalue_type_node)
        return expr;
      return build1 (TREE_CODE (expr), truthvalue_type_node,
                 c_common_truthvalue_conversion (TREE_OPERAND (expr, 0)));

    case ERROR_MARK:
      return expr;

    case INTEGER_CST:
      /* Avoid integer_zerop to ignore TREE_CONSTANT_OVERFLOW.  */
      return (TREE_INT_CST_LOW (expr) != 0 || TREE_INT_CST_HIGH (expr) != 0)
             ? truthvalue_true_node
             : truthvalue_false_node;

    case REAL_CST:
      return real_compare (NE_EXPR, &TREE_REAL_CST (expr), &dconst0)
             ? truthvalue_true_node
             : truthvalue_false_node;

    case FUNCTION_DECL:
      expr = build_unary_op (ADDR_EXPR, expr, 0);
      /* Fall through.  */

    case ADDR_EXPR:
      {
        if (TREE_CODE (TREE_OPERAND (expr, 0)) == FUNCTION_DECL
            && !DECL_WEAK (TREE_OPERAND (expr, 0)))
          {
            /* Common Ada/Pascal programmer's mistake.  We always warn
               about this since it is so bad.  */
            warning (0, "the address of %qD, will always evaluate as %<true%>",
                     TREE_OPERAND (expr, 0));
            return truthvalue_true_node;
          }

        /* If we are taking the address of an external decl, it might be
           zero if it is weak, so we cannot optimize.  */
        if (DECL_P (TREE_OPERAND (expr, 0))
            && DECL_EXTERNAL (TREE_OPERAND (expr, 0)))
          break;

        if (TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 0)))
          return build2 (COMPOUND_EXPR, truthvalue_type_node,
                         TREE_OPERAND (expr, 0), truthvalue_true_node);
        else
          return truthvalue_true_node;
      }

    case COMPLEX_EXPR:
      return build_binary_op ((TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 1))
                               ? TRUTH_OR_EXPR : TRUTH_ORIF_EXPR),
                c_common_truthvalue_conversion (TREE_OPERAND (expr, 0)),
                c_common_truthvalue_conversion (TREE_OPERAND (expr, 1)),
                              0);

    case NEGATE_EXPR:
    case ABS_EXPR:
    case FLOAT_EXPR:
      /* These don't change whether an object is nonzero or zero.  */
      return c_common_truthvalue_conversion (TREE_OPERAND (expr, 0));

    case LROTATE_EXPR:
    case RROTATE_EXPR:
      /* These don't change whether an object is zero or nonzero, but
         we can't ignore them if their second arg has side-effects.  */
      if (TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 1)))
        return build2 (COMPOUND_EXPR, truthvalue_type_node,
                       TREE_OPERAND (expr, 1),
                       c_common_truthvalue_conversion (TREE_OPERAND (expr, 0)));
      else
        return c_common_truthvalue_conversion (TREE_OPERAND (expr, 0));

    case COND_EXPR:
      /* Distribute the conversion into the arms of a COND_EXPR.  */
      return fold_build3 (COND_EXPR, truthvalue_type_node,
                TREE_OPERAND (expr, 0),
                c_common_truthvalue_conversion (TREE_OPERAND (expr, 1)),
                c_common_truthvalue_conversion (TREE_OPERAND (expr, 2)));

    case CONVERT_EXPR:
      /* Don't cancel the effect of a CONVERT_EXPR from a REFERENCE_TYPE,
         since that affects how `default_conversion' will behave.  */
      if (TREE_CODE (TREE_TYPE (expr)) == REFERENCE_TYPE
          || TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0))) == REFERENCE_TYPE)
        break;
      /* Fall through....  */
    case NOP_EXPR:
      /* If this is widening the argument, we can ignore it.  */
      if (TYPE_PRECISION (TREE_TYPE (expr))
          >= TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (expr, 0))))
        return c_common_truthvalue_conversion (TREE_OPERAND (expr, 0));
      break;

    case MINUS_EXPR:
      /* Perhaps reduce (x - y) != 0 to (x != y).  The expressions
         aren't guaranteed to the be same for modes that can represent
         infinity, since if x and y are both +infinity, or both
         -infinity, then x - y is not a number.

         Note that this transformation is safe when x or y is NaN.
         (x - y) is then NaN, and both (x - y) != 0 and x != y will
         be false.  */
      if (HONOR_INFINITIES (TYPE_MODE (TREE_TYPE (TREE_OPERAND (expr, 0)))))
        break;
      /* Fall through....  */
    case BIT_XOR_EXPR:
      /* This and MINUS_EXPR can be changed into a comparison of the
         two objects.  */
      if (TREE_TYPE (TREE_OPERAND (expr, 0))
          == TREE_TYPE (TREE_OPERAND (expr, 1)))
        return fold_build2 (NE_EXPR, truthvalue_type_node,
                            TREE_OPERAND (expr, 0), TREE_OPERAND (expr, 1));
      return fold_build2 (NE_EXPR, truthvalue_type_node,
                          TREE_OPERAND (expr, 0),
                          fold_convert (TREE_TYPE (TREE_OPERAND (expr, 0)),
                                        TREE_OPERAND (expr, 1)));

    case BIT_AND_EXPR:
      if (integer_onep (TREE_OPERAND (expr, 1))
          && TREE_TYPE (expr) != truthvalue_type_node)
        /* Using convert here would cause infinite recursion.  */
        return build1 (NOP_EXPR, truthvalue_type_node, expr);
      break;

    case MODIFY_EXPR:
      if (!TREE_NO_WARNING (expr))
        warning (OPT_Wparentheses,
                 "suggest parentheses around assignment used as truth value");
      break;

    default:
      break;
    }

  if (TREE_CODE (TREE_TYPE (expr)) == COMPLEX_TYPE)
    {
      tree t = save_expr (expr);
      return (build_binary_op
              ((TREE_SIDE_EFFECTS (expr)
                ? TRUTH_OR_EXPR : TRUTH_ORIF_EXPR),
        c_common_truthvalue_conversion (build_unary_op (REALPART_EXPR, t, 0)),
        c_common_truthvalue_conversion (build_unary_op (IMAGPART_EXPR, t, 0)),
               0));
    }

  return build_binary_op (NE_EXPR, expr, integer_zero_node, 1);
}

/* Convert the array expression EXP to a pointer.  */
static tree
array_to_pointer_conversion (tree exp)
{
  tree orig_exp = exp;
  tree type = TREE_TYPE (exp);
  tree adr;
  tree restype = TREE_TYPE (type);
  tree ptrtype;

  gcc_assert (TREE_CODE (type) == ARRAY_TYPE);

  STRIP_TYPE_NOPS (exp);

  if (TREE_NO_WARNING (orig_exp))
    TREE_NO_WARNING (exp) = 1;

  ptrtype = build_pointer_type (restype);

  if (TREE_CODE (exp) == INDIRECT_REF)
    return convert (ptrtype, TREE_OPERAND (exp, 0));

  if (TREE_CODE (exp) == VAR_DECL)
    {
      /* We are making an ADDR_EXPR of ptrtype.  This is a valid
         ADDR_EXPR because it's the best way of representing what
         happens in C when we take the address of an array and place
         it in a pointer to the element type.  */
      adr = build1 (ADDR_EXPR, ptrtype, exp);
      if (! gm2_mark_addressable (exp))
        return error_mark_node;
      TREE_SIDE_EFFECTS (adr) = 0;   /* Default would be, same as EXP.  */
      return adr;
    }

  /* This way is better for a COMPONENT_REF since it can
     simplify the offset for a component.  */
  adr = build_unary_op (ADDR_EXPR, exp, 1);
  return convert (ptrtype, adr);
}

/* Construct and perhaps optimize a tree representation
   for a unary operation.  CODE, a tree_code, specifies the operation
   and XARG is the operand.
   For any CODE other than ADDR_EXPR, FLAG nonzero suppresses
   the default promotions (such as from short to int).
   For ADDR_EXPR, the default promotions are not applied; FLAG nonzero
   allows non-lvalues; this is only used to handle conversion of non-lvalue
   arrays to pointers in C99.  */

tree
build_unary_op (enum tree_code code, tree xarg, int flag)
{
  /* No default_conversion here.  It causes trouble for ADDR_EXPR.  */
  tree arg = xarg;
  tree argtype = 0;
  enum tree_code typecode = TREE_CODE (TREE_TYPE (arg));
  tree val;
  int noconvert = flag;
  const char *invalid_op_diag;

  if (typecode == ERROR_MARK)
    return error_mark_node;
  if (typecode == ENUMERAL_TYPE || typecode == BOOLEAN_TYPE)
    typecode = INTEGER_TYPE;

  if ((invalid_op_diag
       = targetm.invalid_unary_op (code, TREE_TYPE (xarg))))
    {
      error (invalid_op_diag);
      return error_mark_node;
    }

  switch (code)
    {
    case CONVERT_EXPR:
      /* This is used for unary plus, because a CONVERT_EXPR
         is enough to prevent anybody from looking inside for
         associativity, but won't generate any code.  */
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE
            || typecode == COMPLEX_TYPE
            || typecode == VECTOR_TYPE))
        {
          error ("wrong type argument to unary plus");
          return error_mark_node;
        }
      else if (!noconvert)
        arg = default_conversion (arg);
      arg = non_lvalue (arg);
      break;

    case NEGATE_EXPR:
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE
            || typecode == COMPLEX_TYPE
            || typecode == VECTOR_TYPE))
        {
          error ("wrong type argument to unary minus");
          return error_mark_node;
        }
      else if (!noconvert)
        arg = default_conversion (arg);
      break;

    case BIT_NOT_EXPR:
      if (typecode == INTEGER_TYPE || typecode == VECTOR_TYPE)
        {
          if (!noconvert)
            arg = default_conversion (arg);
        }
      else if (typecode == COMPLEX_TYPE)
        {
          code = CONJ_EXPR;
          if (pedantic)
            pedwarn ("ISO C does not support %<~%> for complex conjugation");
          if (!noconvert)
            arg = default_conversion (arg);
        }
      else
        {
          error ("wrong type argument to bit-complement");
          return error_mark_node;
        }
      break;

    case ABS_EXPR:
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE))
        {
          error ("wrong type argument to abs");
          return error_mark_node;
        }
      else if (!noconvert)
        arg = default_conversion (arg);
      break;

    case CONJ_EXPR:
      /* Conjugating a real value is a no-op, but allow it anyway.  */
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE
            || typecode == COMPLEX_TYPE))
        {
          error ("wrong type argument to conjugation");
          return error_mark_node;
        }
      else if (!noconvert)
        arg = default_conversion (arg);
      break;

    case TRUTH_NOT_EXPR:
      if (typecode != INTEGER_TYPE
          && typecode != REAL_TYPE && typecode != POINTER_TYPE
          && typecode != COMPLEX_TYPE)
        {
          error ("wrong type argument to unary exclamation mark");
          return error_mark_node;
        }
#if !defined(GM2)
      arg = c_objc_common_truthvalue_conversion (arg);
#endif
      return invert_truthvalue (arg);

    case NOP_EXPR:
      break;

    case REALPART_EXPR:
      if (TREE_CODE (arg) == COMPLEX_CST)
        return TREE_REALPART (arg);
      else if (TREE_CODE (TREE_TYPE (arg)) == COMPLEX_TYPE)
        return fold_build1 (REALPART_EXPR, TREE_TYPE (TREE_TYPE (arg)), arg);
      else
        return arg;

    case IMAGPART_EXPR:
      if (TREE_CODE (arg) == COMPLEX_CST)
        return TREE_IMAGPART (arg);
      else if (TREE_CODE (TREE_TYPE (arg)) == COMPLEX_TYPE)
        return fold_build1 (IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (arg)), arg);
      else
        return convert (TREE_TYPE (arg), integer_zero_node);

    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:

      /* Increment or decrement the real part of the value,
         and don't change the imaginary part.  */
      if (typecode == COMPLEX_TYPE)
        {
          tree real, imag;

          if (pedantic)
            pedwarn ("ISO C does not support %<++%> and %<--%>"
                     " on complex types");

          arg = stabilize_reference (arg);
          real = build_unary_op (REALPART_EXPR, arg, 1);
          imag = build_unary_op (IMAGPART_EXPR, arg, 1);
          return build2 (COMPLEX_EXPR, TREE_TYPE (arg),
                         build_unary_op (code, real, 1), imag);
        }

      /* Report invalid types.  */

      if (typecode != POINTER_TYPE
          && typecode != INTEGER_TYPE && typecode != REAL_TYPE)
        {
          if (code == PREINCREMENT_EXPR || code == POSTINCREMENT_EXPR)
            error ("wrong type argument to increment");
          else
            error ("wrong type argument to decrement");

          return error_mark_node;
        }

      {
        tree inc;
        tree result_type = TREE_TYPE (arg);

        arg = get_unwidened (arg, 0);
        argtype = TREE_TYPE (arg);

        /* Compute the increment.  */

#if !defined(GM2)
        if (typecode == POINTER_TYPE)
          {
            /* If pointer target is an undefined struct,
               we just cannot know how to do the arithmetic.  */
            if (!COMPLETE_OR_VOID_TYPE_P (TREE_TYPE (result_type)))
              {
                if (code == PREINCREMENT_EXPR || code == POSTINCREMENT_EXPR)
                  error ("increment of pointer to unknown structure");
                else
                  error ("decrement of pointer to unknown structure");
              }
            else if ((pedantic || warn_pointer_arith)
                     && (TREE_CODE (TREE_TYPE (result_type)) == FUNCTION_TYPE
                         || TREE_CODE (TREE_TYPE (result_type)) == VOID_TYPE))
              {
                if (code == PREINCREMENT_EXPR || code == POSTINCREMENT_EXPR)
                  pedwarn ("wrong type argument to increment");
                else
                  pedwarn ("wrong type argument to decrement");
              }

            inc = c_size_in_bytes (TREE_TYPE (result_type));
          }
        else
#endif
          inc = integer_one_node;

        inc = convert (argtype, inc);

        /* Complain about anything else that is not a true lvalue.  */
        if (!lvalue_or_else (arg, ((code == PREINCREMENT_EXPR
                                    || code == POSTINCREMENT_EXPR)
                                   ? lv_increment
                                   : lv_decrement)))
          return error_mark_node;

        /* Report a read-only lvalue.  */
        if (TREE_READONLY (arg))
          readonly_error (arg,
                          ((code == PREINCREMENT_EXPR
                            || code == POSTINCREMENT_EXPR)
                           ? lv_increment : lv_decrement));

        if (TREE_CODE (TREE_TYPE (arg)) == BOOLEAN_TYPE)
          val = boolean_increment (code, arg);
        else
          val = build2 (code, TREE_TYPE (arg), arg, inc);
        TREE_SIDE_EFFECTS (val) = 1;
        val = convert (result_type, val);
        if (TREE_CODE (val) != code)
          TREE_NO_WARNING (val) = 1;
        return val;
      }

    case ADDR_EXPR:
      /* Note that this operation never does default_conversion.  */

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
          tree op0 = TREE_OPERAND (arg, 0);
          if (!gm2_mark_addressable (op0))
            return error_mark_node;
          return build_binary_op (PLUS_EXPR,
                                  (TREE_CODE (TREE_TYPE (op0)) == ARRAY_TYPE
                                   ? array_to_pointer_conversion (op0)
                                   : op0),
                                  TREE_OPERAND (arg, 1), 1);
        }

      /* Anything not already handled and not a true memory reference
         or a non-lvalue array is an error.  */
      else if (typecode != FUNCTION_TYPE && !flag
               && !lvalue_or_else (arg, lv_addressof))
        return error_mark_node;

      /* Ordinary case; arg is a COMPONENT_REF or a decl.  */
      argtype = TREE_TYPE (arg);

      /* If the lvalue is const or volatile, merge that into the type
         to which the address will point.  Note that you can't get a
         restricted pointer by taking the address of something, so we
         only have to deal with `const' and `volatile' here.  */
      if ((DECL_P (arg) || REFERENCE_CLASS_P (arg))
          && (TREE_READONLY (arg) || TREE_THIS_VOLATILE (arg)))
          argtype = c_build_type_variant (argtype,
                                          TREE_READONLY (arg),
                                          TREE_THIS_VOLATILE (arg));

      if (! gm2_mark_addressable (arg))
        return error_mark_node;

      gcc_assert (TREE_CODE (arg) != COMPONENT_REF
                  || !DECL_C_BIT_FIELD (TREE_OPERAND (arg, 1)));

      argtype = build_pointer_type (argtype);

      /* ??? Cope with user tricks that amount to offsetof.  Delete this
         when we have proper support for integer constant expressions.  */
      val = get_base_address (arg);
      if (val && TREE_CODE (val) == INDIRECT_REF
          && TREE_CONSTANT (TREE_OPERAND (val, 0)))
        {
          tree op0 = fold_convert (argtype, fold_offsetof (arg)), op1;

          op1 = fold_convert (argtype, TREE_OPERAND (val, 0));
          return fold_build2 (PLUS_EXPR, argtype, op0, op1);
        }

      val = build1 (ADDR_EXPR, argtype, arg);

      return val;

    default:
      break;
    }

  if (argtype == 0)
    argtype = TREE_TYPE (arg);
#if defined(GM2)
  return fold_build1 (code, argtype, arg);
#else
  return require_constant_value ? fold_build1_initializer (code, argtype, arg)
                                : fold_build1 (code, argtype, arg);
#endif
}

/* Print an error message for invalid operands to arith operation
   CODE.  NOP_EXPR is used as a special case (see
   c_common_truthvalue_conversion).   */

void
binary_op_error (enum tree_code code)
{
  const char *opname;

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
    default:
      gcc_unreachable ();
    }
  error ("invalid operands to binary %s", opname);
}


static
int
is_pointer (enum tree_code code)
{
  return (code == POINTER_TYPE) || (code == REFERENCE_TYPE);
}

static
int
is_integer (enum tree_code code)
{
  return (code == INTEGER_TYPE);
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
build_binary_op (enum tree_code code, tree orig_op0, tree orig_op1,
                 int convert_p)
{
  tree type0, type1;
  enum tree_code code0, code1;
  tree op0, op1;

  /* Expression code to give to the expression when it is built.
     Normally this is CODE, which is what the caller asked for,
     but in some special cases we change it.  */
  enum tree_code resultcode = code;

  /* Data type in which the computation is to be performed.
     In the simplest cases this is the common type of the arguments.  */
  tree result_type = NULL;

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
#if defined(GM2)
  if (base_type (type0) == m2_char_type_node)
    type0 = char_type_node;
  if (base_type (type1) == m2_char_type_node)
    type1 = char_type_node;

  code0 = TREE_CODE (base_type (type0));
  code1 = TREE_CODE (base_type (type1));
#else
  code0 = TREE_CODE (type0);
  code1 = TREE_CODE (type1);
#endif

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
#if defined(GM2)
      if ((is_pointer (code0) && is_integer (code1)) ||
          (is_pointer (code1) && is_integer (code0)) ||
          (is_pointer (code0) && is_pointer (code1)))
        result_type = ptr_type_node;
      else
        common = 1;
#else
      if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
        return pointer_int_sum (PLUS_EXPR, op0, op1);
      else if (code1 == POINTER_TYPE && code0 == INTEGER_TYPE)
        return pointer_int_sum (PLUS_EXPR, op1, op0);
      else
        common = 1;
#endif
      break;

    case MINUS_EXPR:
#if defined(GM2)
      if ((is_pointer (code0) && is_integer (code1)) ||
          (is_pointer (code1) && is_integer (code0)) ||
          (is_pointer (code0) && is_pointer (code1)))
        result_type = ptr_type_node;
      else
        common = 1;
#else
      /* Subtraction of two similar pointers.
         We must subtract them as integers, then divide by object size.  */
      if (code0 == POINTER_TYPE && code1 == POINTER_TYPE
          && comp_target_types (type0, type1))
        return pointer_diff (op0, op1);
      /* Handle pointer minus int.  Just like pointer plus int.  */
      else if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
        return pointer_int_sum (MINUS_EXPR, op0, op1);
      common = 1;
#endif
      break;

    case MULT_EXPR:
#if defined(GM2)
      if ((code0 == POINTER_TYPE && code1 == INTEGER_TYPE) ||
          (code1 == POINTER_TYPE && code0 == INTEGER_TYPE) ||
          (code0 == POINTER_TYPE && code1 == POINTER_TYPE))
        result_type = ptr_type_node;
      else
        common = 1;
#else
      common = 1;
#endif
      break;

    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
      /* Floating point division by zero is a legitimate way to obtain
         infinities and NaNs.  */
      if (skip_evaluation == 0 && integer_zerop (op1))
        warning (OPT_Wdiv_by_zero, "division by zero");

#if defined(GM2)
      if ((code0 == POINTER_TYPE && code1 == INTEGER_TYPE) ||
          (code1 == POINTER_TYPE && code0 == INTEGER_TYPE) ||
          (code0 == POINTER_TYPE && code1 == POINTER_TYPE))
        result_type = ptr_type_node;
#endif

      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE
           || code0 == COMPLEX_TYPE || code0 == VECTOR_TYPE)
          && (code1 == INTEGER_TYPE || code1 == REAL_TYPE
              || code1 == COMPLEX_TYPE || code1 == VECTOR_TYPE))
        {
          enum tree_code tcode0 = code0, tcode1 = code1;

          if (code0 == COMPLEX_TYPE || code0 == VECTOR_TYPE)
            tcode0 = TREE_CODE (TREE_TYPE (TREE_TYPE (op0)));
          if (code1 == COMPLEX_TYPE || code1 == VECTOR_TYPE)
            tcode1 = TREE_CODE (TREE_TYPE (TREE_TYPE (op1)));

          if (!(tcode0 == INTEGER_TYPE && tcode1 == INTEGER_TYPE))
            resultcode = RDIV_EXPR;
          else
            /* Although it would be tempting to shorten always here, that
               loses on some targets, since the modulo instruction is
               undefined if the quotient can't be represented in the
               computation mode.  We shorten only if unsigned or if
               dividing by something we know != -1.  */
            shorten = (TYPE_UNSIGNED (TREE_TYPE (orig_op0))
                       || (TREE_CODE (op1) == INTEGER_CST
                           && !integer_all_onesp (op1)));
          common = 1;
        }
      break;

    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
#if defined(GM2)
      if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
        result_type = ptr_type_node;
#endif

      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
        shorten = -1;
      else if (code0 == VECTOR_TYPE && code1 == VECTOR_TYPE)
        common = 1;
      break;

    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
      if (skip_evaluation == 0 && integer_zerop (op1))
        warning (OPT_Wdiv_by_zero, "division by zero");

#if defined(GM2)
      if ((code0 == POINTER_TYPE && code1 == INTEGER_TYPE) ||
          (code1 == POINTER_TYPE && code0 == INTEGER_TYPE) ||
          (code0 == POINTER_TYPE && code1 == POINTER_TYPE))
        result_type = ptr_type_node;
#endif

      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
        {
          /* Although it would be tempting to shorten always here, that loses
             on some targets, since the modulo instruction is undefined if the
             quotient can't be represented in the computation mode.  We shorten
             only if unsigned or if dividing by something we know != -1.  */
          shorten = (TYPE_UNSIGNED (TREE_TYPE (orig_op0))
                     || (TREE_CODE (op1) == INTEGER_CST
                         && !integer_all_onesp (op1)));
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
          op0 = gm2_truthvalue_conversion (op0);
          op1 = gm2_truthvalue_conversion (op1);
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
                warning (0, "right shift count is negative");
              else
                {
                  if (! integer_zerop (op1))
                    short_shift = 1;

                  if (compare_tree_int (op1, TYPE_PRECISION (type0)) >= 0)
                    warning (0, "right shift count >= width of type");
                }
            }

          /* Use the type of the value to be shifted. */
          result_type = type0;
          /* Convert the shift-count to an integer, regardless of size
             of value being shifted.  */
          if (TYPE_MAIN_VARIANT (TREE_TYPE (op1)) != integer_type_node)
            op1 = convert (integer_type_node, op1);
          /* Avoid converting op1 to result_type later.  */
          converted = 1;
        }
      break;

    case LSHIFT_EXPR:
      if ((code0 == INTEGER_TYPE && code1 == INTEGER_TYPE) ||
          (code0 == POINTER_TYPE && code1 == INTEGER_TYPE))
        {
          if (TREE_CODE (op1) == INTEGER_CST && skip_evaluation == 0)
            {
              if (tree_int_cst_sgn (op1) < 0)
                warning (0, "left shift count is negative");

              else if (compare_tree_int (op1, TYPE_PRECISION (type0)) >= 0)
                warning (0, "left shift count >= width of type");
            }

          /* Use the type of the value to be shifted. */
          result_type = type0;
          /* Convert the shift-count to an integer, regardless of size
             of value being shifted.  */
          if (TYPE_MAIN_VARIANT (TREE_TYPE (op1)) != integer_type_node)
            op1 = convert (integer_type_node, op1);
          /* Avoid converting op1 to result_type later.  */
          converted = 1;
        }
      break;

    case RROTATE_EXPR:
    case LROTATE_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
        {
          if (TREE_CODE (op1) == INTEGER_CST && skip_evaluation == 0)
            {
              if (tree_int_cst_sgn (op1) < 0)
                warning (0, "shift count is negative");
              else if (compare_tree_int (op1, TYPE_PRECISION (type0)) >= 0)
                warning (0, "shift count >= width of type");
            }

          /* Use the type of the value to be shifted.  */
          result_type = type0;
          /* Convert the shift-count to an integer, regardless of size
             of value being shifted.  */
          if (TYPE_MAIN_VARIANT (TREE_TYPE (op1)) != integer_type_node)
            op1 = convert (integer_type_node, op1);
          /* Avoid converting op1 to result_type later.  */
          converted = 1;
        }
      break;

    case EQ_EXPR:
    case NE_EXPR:
      if (warn_float_equal && (code0 == REAL_TYPE || code1 == REAL_TYPE))
        warning (OPT_Wfloat_equal,
                 "comparing floating point with == or != is unsafe");
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
          tree tt0 = TREE_TYPE (type0);
          tree tt1 = TREE_TYPE (type1);
          /* Anything compares with void *.  void * compares with anything.
             Otherwise, the targets must be compatible
             and both must be object or both incomplete.  */
          if (comp_target_types (type0, type1))
            result_type = common_pointer_type (type0, type1);
          else if (VOID_TYPE_P (tt0))
            {
              /* op0 != orig_op0 detects the case of something
                 whose value is 0 but which isn't a valid null ptr const.  */
              if (pedantic && (!integer_zerop (op0) || op0 != orig_op0)
                  && TREE_CODE (tt1) == FUNCTION_TYPE)
                pedwarn ("ISO C forbids comparison of %<void *%>"
                         " with function pointer");
            }
          else if (VOID_TYPE_P (tt1))
            {
              if (pedantic && (!integer_zerop (op1) || op1 != orig_op1)
                  && TREE_CODE (tt0) == FUNCTION_TYPE)
                pedwarn ("ISO C forbids comparison of %<void *%>"
                         " with function pointer");
            }
          else
#if !defined(GM2)
            /* Avoid warning about the volatile ObjC EH puts on decls.  */
            if (!objc_ok)
#endif
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
#if !defined(GM2)
          pedwarn ("comparison between pointer and integer");
#endif
        }
      else if (code0 == INTEGER_TYPE && code1 == POINTER_TYPE)
        {
          result_type = type1;
#if !defined(GM2)
          pedwarn ("comparison between pointer and integer");
#endif
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
                pedwarn ("ISO C forbids ordered comparisons of pointers to functions");
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
              if (!COMPLETE_TYPE_P (TREE_TYPE (type0))
                  != !COMPLETE_TYPE_P (TREE_TYPE (type1)))
                pedwarn ("comparison of complete and incomplete pointers");
              else if (pedantic 
                       && TREE_CODE (TREE_TYPE (type0)) == FUNCTION_TYPE)
                pedwarn ("ISO C forbids ordered comparisons of pointers to functions");
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
          pedwarn ("comparison between pointer and integer");
        }
      else if (code0 == INTEGER_TYPE && code1 == POINTER_TYPE)
        {
          result_type = type1;
          pedwarn ("comparison between pointer and integer");
        }
      break;

    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
      build_type = integer_type_node;
      if (code0 != REAL_TYPE || code1 != REAL_TYPE)
        {
          error ("unordered comparison on non-floating point argument");
          return error_mark_node;
        }
      common = 1;
      break;

    default:
      break;
    }

  if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE || code0 == COMPLEX_TYPE
       || code0 == VECTOR_TYPE)
      &&
      (code1 == INTEGER_TYPE || code1 == REAL_TYPE || code1 == COMPLEX_TYPE
       || code1 == VECTOR_TYPE))
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
          int uns = TYPE_UNSIGNED (result_type);
          tree type;

          final_type = result_type;

          /* Handle the case that OP0 (or OP1) does not *contain* a conversion
             but it *requires* conversion to FINAL_TYPE.  */

          if ((TYPE_PRECISION (TREE_TYPE (op0))
               == TYPE_PRECISION (TREE_TYPE (arg0)))
              && TREE_TYPE (op0) != final_type)
            unsigned0 = TYPE_UNSIGNED (TREE_TYPE (op0));
          if ((TYPE_PRECISION (TREE_TYPE (op1))
               == TYPE_PRECISION (TREE_TYPE (arg1)))
              && TREE_TYPE (op1) != final_type)
            unsigned1 = TYPE_UNSIGNED (TREE_TYPE (op1));

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
              = gm2_signed_or_unsigned_type
              (unsigned0, common_type (TREE_TYPE (arg0), TREE_TYPE (arg1)));
          else if (TREE_CODE (arg0) == INTEGER_CST
                   && (unsigned1 || !uns)
                   && (TYPE_PRECISION (TREE_TYPE (arg1))
                       < TYPE_PRECISION (result_type))
                   && (type
                       = gm2_signed_or_unsigned_type (unsigned1,
                                                           TREE_TYPE (arg1)),
                       int_fits_type_p (arg0, type)))
            result_type = type;
          else if (TREE_CODE (arg1) == INTEGER_CST
                   && (unsigned0 || !uns)
                   && (TYPE_PRECISION (TREE_TYPE (arg0))
                       < TYPE_PRECISION (result_type))
                   && (type
                       = gm2_signed_or_unsigned_type (unsigned0,
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
            unsigned_arg = TYPE_UNSIGNED (TREE_TYPE (op0));

          if (TYPE_PRECISION (TREE_TYPE (arg0)) < TYPE_PRECISION (result_type)
              /* We can shorten only if the shift count is less than the
                 number of bits in the smaller type size.  */
              && compare_tree_int (op1, TYPE_PRECISION (TREE_TYPE (arg0))) < 0
              /* We cannot drop an unsigned shift after sign-extension.  */
              && (!TYPE_UNSIGNED (final_type) || unsigned_arg))
            {
              /* Do an unsigned shift if the operand was zero-extended.  */
              result_type
                = gm2_signed_or_unsigned_type (unsigned_arg,
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
              int op0_signed = ! TYPE_UNSIGNED (TREE_TYPE (orig_op0));
              int op1_signed = ! TYPE_UNSIGNED (TREE_TYPE (orig_op1));
              int unsignedp0, unsignedp1;
              tree primop0 = get_narrower (op0, &unsignedp0);
              tree primop1 = get_narrower (op1, &unsignedp1);

              xop0 = orig_op0;
              xop1 = orig_op1;
              STRIP_TYPE_NOPS (xop0);
              STRIP_TYPE_NOPS (xop1);

              /* Give warnings for comparisons between signed and unsigned
                 quantities that may fail. 

                 Do the checking based on the original operand trees, so that
                 casts will be considered, but default promotions won't be.

                 Do not warn if the comparison is being done in a signed type,
                 since the signed type will only be chosen if it can represent
                 all the values of the unsigned type.  */
              if (! TYPE_UNSIGNED (result_type))
                /* OK */;
              /* Do not warn if both operands are the same signedness.  */
              else if (op0_signed == op1_signed)
                /* OK */;
              else
                {
                  tree sop, uop;

                  if (op0_signed)
                    sop = xop0, uop = xop1;
                  else
                    sop = xop1, uop = xop0;

                  /* Do not warn if the signed quantity is an
                     unsuffixed integer literal (or some static
                     constant expression involving such literals or a
                     conditional expression involving such literals)
                     and it is non-negative.  */
                  if (tree_expr_nonnegative_p (sop))
                    /* OK */;
                  /* Do not warn if the comparison is an equality operation,
                     the unsigned quantity is an integral constant, and it
                     would fit in the result if the result were signed.  */
                  else if (TREE_CODE (uop) == INTEGER_CST
                           && (resultcode == EQ_EXPR || resultcode == NE_EXPR)
                           && int_fits_type_p
                           (uop, gm2_signed_type (result_type)))
                    /* OK */;
                  /* Do not warn if the unsigned quantity is an enumeration
                     constant and its maximum value would fit in the result
                     if the result were signed.  */
                  else if (TREE_CODE (uop) == INTEGER_CST
                           && TREE_CODE (TREE_TYPE (uop)) == ENUMERAL_TYPE
                           && int_fits_type_p
                           (TYPE_MAX_VALUE (TREE_TYPE(uop)),
                            gm2_signed_type (result_type)))
                    /* OK */;
                  else
                    warning (0, "comparison between signed and unsigned");
                }

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
              
                  if (host_integerp (primop0, 0) || host_integerp (primop1, 0))
                    {
                      tree primop;
                      HOST_WIDE_INT constant, mask;
                      int unsignedp, bits;

                      if (host_integerp (primop0, 0))
                        {
                          primop = primop1;
                          unsignedp = unsignedp1;
                          constant = tree_low_cst (primop0, 0);
                        }
                      else
                        {
                          primop = primop0;
                          unsignedp = unsignedp0;
                          constant = tree_low_cst (primop1, 0);
                        }

                      bits = TYPE_PRECISION (TREE_TYPE (primop));
                      if (bits < TYPE_PRECISION (result_type)
                          && bits < HOST_BITS_PER_WIDE_INT && unsignedp)
                        {
                          mask = (~ (HOST_WIDE_INT) 0) << bits;
                          if ((mask & constant) != mask)
                            warning (0, "comparison of promoted ~unsigned with constant");
                        }
                    }
                  else if (unsignedp0 && unsignedp1
                           && (TYPE_PRECISION (TREE_TYPE (primop0))
                               < TYPE_PRECISION (result_type))
                           && (TYPE_PRECISION (TREE_TYPE (primop1))
                               < TYPE_PRECISION (result_type)))
                    warning (0, "comparison of promoted ~unsigned with unsigned");
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
    tree result = fold_build2 (resultcode, build_type, op0, op1);

    if (final_type != 0)
      result = convert (final_type, result);
    return result;
  }
}

/*
 *  routines which interface with the Modula-2 source
 */

tree
skip_type_decl (tree type)
{
  if (type == error_mark_node)
    return error_mark_node;

  if (type == NULL_TREE)
    return NULL_TREE;

  if (TREE_CODE (type) == TYPE_DECL)
    return TREE_TYPE (type);
  return type;
}

/*
 *  routines which interface with the Modula-2 source
 */

tree
skip_const_decl (tree exp)
{
  if (exp == error_mark_node)
    return error_mark_node;

  if (exp == NULL_TREE)
    return NULL_TREE;

  if (TREE_CODE (exp) == CONST_DECL)
    return DECL_INITIAL (exp);
  return exp;
}

/*
 *  DeclareKnownType - given a, type, with a, name, return a GCC declaration of this type.
 *                     TYPE
 *                        name = INTEGER ;
 */

tree
gccgm2_DeclareKnownType (char *name, tree type)
{
  tree id = get_identifier (name);
  tree decl, tem;

  ASSERT(is_type(type), type);
  type = skip_type_decl (type);
  decl = build_decl (TYPE_DECL, id, type);

  layout_type (type);

  tem = pushdecl (decl);
  finish_decl (decl, NULL_TREE, NULL_TREE);
  ASSERT(is_type(decl), decl);

  return tem;
}

/*
 *  GetDefaultType - given a, type, with a, name, return a GCC declaration of this type.
 *                   Checks to see whether the type name has already been declared as a
 *                   default type and if so it returns this declaration. Otherwise it
 *                   declares the type. In Modula-2 this is equivalent to:
 *                   
 *                   TYPE
 *                      name = type ;
 *                   
 *                   We need this function as the initialization to gccgm2.c will
 *                   declare C default types and _some_ M2 default types.
 */

tree
gccgm2_GetDefaultType (char *name, tree type)
{
  tree id = maybe_get_identifier (name);

  if (id == NULL) {
    tree prev = type;
    tree t;

    while (prev != NULL) {
      if (TYPE_NAME (prev) == NULL)
        TYPE_NAME (prev) = get_identifier (name);
      prev = TREE_TYPE (prev);
    } 
    t  = gccgm2_DeclareKnownType (name, type);
    return t;
  }
  else
    return id;
}

static tree check_name = NULL;

void setId (tree t)
{
  check_name = t;
}

/*
 *  GetMinFrom - given a, type, return a constant representing the minimum
 *               legal value.
 */

tree
gccgm2_GetMinFrom (tree type)
{
  if (type == m2_real_type_node || type == gccgm2_GetRealType())
    return fold (gccgm2_BuildNegate (fold (gm2builtins_BuiltInHugeVal ()),
                                     FALSE));
  if (type == m2_long_real_type_node || type == gccgm2_GetLongRealType())
    return fold (gccgm2_BuildNegate (fold (gm2builtins_BuiltInHugeValLong ()),
                                     FALSE));
  if (type == m2_short_real_type_node || type == gccgm2_GetShortRealType())
    return fold (gccgm2_BuildNegate (fold (gm2builtins_BuiltInHugeValShort ()),
                                     FALSE));
  if (type == ptr_type_node)
    return gccgm2_GetPointerZero();

  return TYPE_MIN_VALUE (skip_type_decl (type));
}

/*
 *  GetMaxFrom - given a, type, return a constant representing the maximum
 *               legal value.
 */

tree
gccgm2_GetMaxFrom (tree type)
{
  if (type == m2_real_type_node || type == gccgm2_GetRealType ())
    return fold (gm2builtins_BuiltInHugeVal ());
  if (type == m2_long_real_type_node || type == gccgm2_GetLongRealType())
    return fold (gm2builtins_BuiltInHugeValLong ());
  if (type == m2_short_real_type_node || type == gccgm2_GetShortRealType())
    return fold (gm2builtins_BuiltInHugeValShort ());
  if (type == ptr_type_node)
    return fold (gccgm2_BuildSub (gccgm2_GetPointerZero (),
                                  gccgm2_GetPointerOne (),
                                  FALSE));

  return TYPE_MAX_VALUE (skip_type_decl (type));
}

int
gccgm2_GetBitsPerWord (void)
{
  return BITS_PER_WORD;
}

int
gccgm2_GetBitsPerInt (void)
{
  return INT_TYPE_SIZE;
}

int
gccgm2_GetBitsPerBitset (void)
{
  return SET_WORD_SIZE;
}

int
gccgm2_GetBitsPerUnit (void)
{
  return BITS_PER_UNIT;
}

#if defined(DEBUGGING)
static tree watch;

static int                    is_var                                      PARAMS ((tree var));
static void                   debug_watch                                 PARAMS ((tree));
static int                    is_array                                    PARAMS ((tree));

static void debug_watch (tree t)
{
  watch = t;
}

static int
is_var (tree var)
{
  return TREE_CODE(var) == VAR_DECL;
}

static int
is_array (tree array)
{
  return TREE_CODE(array) == ARRAY_TYPE;
}
#endif

static int
is_type (tree type)
{
  switch (TREE_CODE(type)) {

  case TYPE_DECL:
  case ARRAY_TYPE:
  case RECORD_TYPE:
  case SET_TYPE:
  case ENUMERAL_TYPE:
  case POINTER_TYPE:
  case INTEGER_TYPE:
  case REAL_TYPE:
  case CHAR_TYPE:
  case UNION_TYPE:
  case BOOLEAN_TYPE:
    return TRUE;
  default:
    return FALSE;
  }
}

/*
 *  DeclareKnownVariable - declares a variable in scope,
 *                         funcscope. Note that the global variable,
 *                         current_function_decl, is altered if
 *                         isglobal is TRUE.
 */

tree
gccgm2_DeclareKnownVariable (char *name, tree type, int exported,
                             int imported, int istemporary,
                             int isglobal, tree scope)
{
  tree id;
  tree decl;

  ASSERT (is_type(type), type);
  ASSERT_BOOL (isglobal);

  if (strcmp (name, "TimePattern_TimeIO_head") == 0)
    stop();

  id   = get_identifier (name);
  type = skip_type_decl (type);
  decl = build_decl (VAR_DECL, id, type);

  DECL_SOURCE_LINE (decl) = getLineNo();

  DECL_EXTERNAL (decl)    = imported;
  if (isglobal && (scope == NULL_TREE)) {
    TREE_PUBLIC (decl)  = exported;
    TREE_STATIC (decl)  = isglobal;           /* declaration and definition */
  } 
  else if (imported) {
    TREE_STATIC (decl)  = 0;
    TREE_PUBLIC (decl)  = 1; /* imported; */  /* was 1 */
  }
  else {
    TREE_STATIC (decl)  = 0;
    TREE_PUBLIC (decl)  = 0;
  }

  if (istemporary) {
    /* The variable was declared by the compiler.  */
    DECL_ARTIFICIAL (decl) = 1;
    /* and we don't want debug info for it.  */
    DECL_IGNORED_P (decl) = 1;
  }

  DECL_CONTEXT (decl)    = scope;

  /* now for the id */

  if (imported)
    TREE_STATIC (id)    = 0;           /* declaration and definition */
  else
    TREE_STATIC (id)    = 1;           /* declaration and definition */

  TREE_PUBLIC (id)      = TREE_PUBLIC (decl);
  pushdecl (decl);

  if (DECL_SIZE (decl) == 0)
    error ("storage size of %q+D' hasn't been resolved", decl);

  if ((TREE_PUBLIC (decl) == 0) && DECL_EXTERNAL (decl))
    internal_error ("inconsistant because PUBLIC_DECL(decl) == 0 && DECL_EXTERNAL(decl) == 1");

  if (! isglobal)
    add_stmt (build_stmt (DECL_EXPR, decl));

  return decl;
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
gccgm2_DeclareKnownConstant (tree type, tree value)
{
    tree id = make_node (IDENTIFIER_NODE);  /* ignore the name of the constant */
    tree decl;

    constant_expression_warning(value);
    type = skip_type_decl (type);
    layout_type (type);

    decl = build_decl (CONST_DECL, id, type);

    DECL_INITIAL (decl) = value;
    TREE_TYPE (decl)    = type;
    pushdecl (decl);

    return decl;
}

tree
chainon_stmt_list (void)
{
  tree t;
  t = alloc_stmt_list ();
  TREE_CHAIN (t) = cur_stmt_list;
  cur_stmt_list = t;
  return t;
}

/* from c-semantic.c import */

/* Create an empty statement tree rooted at T.  */

tree
push_stmt_list (void)
{
  tree t;
  t = alloc_stmt_list ();
  TREE_CHAIN (t) = cur_stmt_list;
  cur_stmt_list = t;
  return t;
}

/* Finish the statement tree rooted at T.  */

tree
pop_stmt_list (tree t)
{
  tree u = cur_stmt_list, chain;

  /* Pop statement lists until we reach the target level.  The extra
     nestings will be due to outstanding cleanups.  */
  while (1)
    {
      chain = TREE_CHAIN (u);
      TREE_CHAIN (u) = NULL_TREE;
      if (t == u)
        break;
      u = chain;
    }
  cur_stmt_list = chain;

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


/* Build a generic statement based on the given type of node and
   arguments. Similar to `build_nt', except that we set
   EXPR_LOCATION to be the current source location.  */
/* ??? This should be obsolete with the lineno_stmt productions
   in the grammar.  */

tree
build_stmt (enum tree_code code, ...)
{
  tree ret;
  int length, i;
  va_list p;
  bool side_effects;

  va_start (p, code);

  ret = make_node (code);
  TREE_TYPE (ret) = void_type_node;
  length = TREE_CODE_LENGTH (code);
  SET_EXPR_LOCATION (ret, input_location);

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

/* FROM ../c-decl.c IMPORT */
/* Finish processing of a declaration;
   install its initial value.
   If the length of an array type is not known before,
   it must be determined now, from the initial value, or it is an error.  */

void
finish_decl (tree decl, tree init, tree asmspec_tree)
{
  tree type = TREE_TYPE (decl);
  int was_incomplete = (DECL_SIZE (decl) == 0);
  const char *asmspec = 0;

  /* If a name was specified, get the string.  */
  if ((TREE_CODE (decl) == FUNCTION_DECL || TREE_CODE (decl) == VAR_DECL)
      && DECL_FILE_SCOPE_P (decl))
    asmspec_tree = maybe_apply_renaming_pragma (decl, asmspec_tree);
  if (asmspec_tree)
    asmspec = TREE_STRING_POINTER (asmspec_tree);

  /* If `start_decl' didn't like having an initialization, ignore it now.  */
  if (init != 0 && DECL_INITIAL (decl) == 0)
    init = 0;

  /* Don't crash if parm is initialized.  */
  if (TREE_CODE (decl) == PARM_DECL)
    init = 0;

#if !defined(GM2)
  if (init)
    store_init_value (decl, init);

  if (c_dialect_objc () && (TREE_CODE (decl) == VAR_DECL
                            || TREE_CODE (decl) == FUNCTION_DECL
                            || TREE_CODE (decl) == FIELD_DECL))
    objc_check_decl (decl);
#endif

  /* Deduce size of array from initialization, if not already known.  */
  if (TREE_CODE (type) == ARRAY_TYPE
      && TYPE_DOMAIN (type) == 0
      && TREE_CODE (decl) != TYPE_DECL)
    {
#if defined(GM2)
      error("internal error: not expecting array to be unknown in Modula-2");
#else
      bool do_default
        = (TREE_STATIC (decl)
           /* Even if pedantic, an external linkage array
              may have incomplete type at first.  */
           ? pedantic && !TREE_PUBLIC (decl)
           : !DECL_EXTERNAL (decl));
      int failure
        = complete_array_type (&TREE_TYPE (decl), DECL_INITIAL (decl),
                               do_default);

      /* Get the completed type made by complete_array_type.  */
      type = TREE_TYPE (decl);

      switch (failure)
        {
        case 1:
          error ("initializer fails to determine size of %q+D", decl);
          break;

        case 2:
          if (do_default)
            error ("array size missing in %q+D", decl);
          /* If a `static' var's size isn't known,
             make it extern as well as static, so it does not get
             allocated.
             If it is not `static', then do not mark extern;
             finish_incomplete_decl will give it a default size
             and it will get allocated.  */
          else if (!pedantic && TREE_STATIC (decl) && !TREE_PUBLIC (decl))
            DECL_EXTERNAL (decl) = 1;
          break;

        case 3:
          error ("zero or negative size array %q+D", decl);
          break;

        case 0:
          /* For global variables, update the copy of the type that
             exists in the binding.  */
          if (TREE_PUBLIC (decl))
            {
              struct c_binding *b_ext = I_SYMBOL_BINDING (DECL_NAME (decl));
              while (b_ext && !B_IN_EXTERNAL_SCOPE (b_ext))
                b_ext = b_ext->shadowed;
              if (b_ext)
                {
                  if (b_ext->type)
                    b_ext->type = composite_type (b_ext->type, type);
                  else
                    b_ext->type = type;
                }
            }
          break;

        default:
          gcc_unreachable ();
        }

      if (DECL_INITIAL (decl))
        TREE_TYPE (DECL_INITIAL (decl)) = type;

      layout_decl (decl, 0);
#endif
    }

  if (TREE_CODE (decl) == VAR_DECL)
    {
#if !defined(GM2)
      if (init && TREE_CODE (init) == CONSTRUCTOR)
        add_flexible_array_elts_to_size (decl, init);
#endif

      if (DECL_SIZE (decl) == 0 && TREE_TYPE (decl) != error_mark_node
          && COMPLETE_TYPE_P (TREE_TYPE (decl)))
        layout_decl (decl, 0);

      if (DECL_SIZE (decl) == 0
          /* Don't give an error if we already gave one earlier.  */
          && TREE_TYPE (decl) != error_mark_node
          && (TREE_STATIC (decl)
              /* A static variable with an incomplete type
                 is an error if it is initialized.
                 Also if it is not file scope.
                 Otherwise, let it through, but if it is not `extern'
                 then it may cause an error message later.  */
              ? (DECL_INITIAL (decl) != 0
                 || !DECL_FILE_SCOPE_P (decl))
              /* An automatic variable with an incomplete type
                 is an error.  */
              : !DECL_EXTERNAL (decl)))
         {
           error ("storage size of %q+D isn%'t known", decl);
           TREE_TYPE (decl) = error_mark_node;
         }

      if ((DECL_EXTERNAL (decl) || TREE_STATIC (decl))
          && DECL_SIZE (decl) != 0)
        {
          if (TREE_CODE (DECL_SIZE (decl)) == INTEGER_CST)
            constant_expression_warning (DECL_SIZE (decl));
          else
            error ("storage size of %q+D isn%'t constant", decl);
        }

      if (TREE_USED (type))
        TREE_USED (decl) = 1;
    }

  /* If this is a function and an assembler name is specified, reset DECL_RTL
     so we can give it its new name.  Also, update built_in_decls if it
     was a normal built-in.  */
  if (TREE_CODE (decl) == FUNCTION_DECL && asmspec)
    {
#if !defined(GM2)
      if (DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL)
        set_builtin_user_assembler_name (decl, asmspec);
#endif
      set_user_assembler_name (decl, asmspec);
    }

#if !defined(GM2)
  /* If #pragma weak was used, mark the decl weak now.  */
  maybe_apply_pragma_weak (decl);

  /* If this is a variable definition, determine its ELF visibility.  */
  if (TREE_CODE (decl) == VAR_DECL 
      && TREE_STATIC (decl) 
      && !DECL_EXTERNAL (decl))
    c_determine_visibility (decl);
#endif

  /* Output the assembler code and/or RTL code for variables and functions,
     unless the type is an undefined structure or union.
     If not, it will get done when the type is completed.  */

  if (TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == FUNCTION_DECL)
    {
#if !defined(GM2)
      /* This is a no-op in c-lang.c or something real in objc-act.c.  */
      if (c_dialect_objc ())
        objc_check_decl (decl);

      if (asmspec) 
        {
          /* If this is not a static variable, issue a warning.
             It doesn't make any sense to give an ASMSPEC for an
             ordinary, non-register local variable.  Historically,
             GCC has accepted -- but ignored -- the ASMSPEC in
             this case.  */
          if (!DECL_FILE_SCOPE_P (decl)
              && TREE_CODE (decl) == VAR_DECL
              && !C_DECL_REGISTER (decl)
              && !TREE_STATIC (decl))
            warning (0, "ignoring asm-specifier for non-static local "
                     "variable %q+D", decl);
          else
            set_user_assembler_name (decl, asmspec);
        }
#endif
      
      if (DECL_FILE_SCOPE_P (decl))
        {
          if (DECL_INITIAL (decl) == NULL_TREE
              || DECL_INITIAL (decl) == error_mark_node)
            /* Don't output anything
               when a tentative file-scope definition is seen.
               But at end of compilation, do output code for them.  */
            DECL_DEFER_OUTPUT (decl) = 1;
          rest_of_decl_compilation (decl, true, 0);
        }
      else
        {
#if !defined(GM2)
          /* In conjunction with an ASMSPEC, the `register'
             keyword indicates that we should place the variable
             in a particular register.  */
          if (asmspec && C_DECL_REGISTER (decl))
            {
              DECL_HARD_REGISTER (decl) = 1;
              /* This cannot be done for a structure with volatile
                 fields, on which DECL_REGISTER will have been
                 reset.  */
              if (!DECL_REGISTER (decl))
                error ("cannot put object with volatile field into register");
            }
#endif

          if (TREE_CODE (decl) != FUNCTION_DECL)
            {
#if defined(GM2)
                ASSERT (FALSE, FALSE);
#endif
              /* If we're building a variable sized type, and we might be
                 reachable other than via the top of the current binding
                 level, then create a new BIND_EXPR so that we deallocate
                 the object at the right time.  */
              /* Note that DECL_SIZE can be null due to errors.  */
              if (DECL_SIZE (decl)
                  && !TREE_CONSTANT (DECL_SIZE (decl))
                  && STATEMENT_LIST_HAS_LABEL (cur_stmt_list))
                {
                  tree bind;
                  bind = build3 (BIND_EXPR, void_type_node, NULL, NULL, NULL);
                  TREE_SIDE_EFFECTS (bind) = 1;
                  add_stmt (bind);
#if defined(GM2)
                  BIND_EXPR_BODY (bind) = chainon_stmt_list ();
#else
                  BIND_EXPR_BODY (bind) = push_stmt_list ();
#endif
                }
              add_stmt (build_stmt (DECL_EXPR, decl));
            }
        }
  

      if (!DECL_FILE_SCOPE_P (decl))
        {
          /* Recompute the RTL of a local array now
             if it used to be an incomplete type.  */
          if (was_incomplete
              && !TREE_STATIC (decl) && !DECL_EXTERNAL (decl))
            {
              /* If we used it already as memory, it must stay in memory.  */
              TREE_ADDRESSABLE (decl) = TREE_USED (decl);
              /* If it's still incomplete now, no init will save it.  */
              if (DECL_SIZE (decl) == 0)
                DECL_INITIAL (decl) = 0;
            }
        }
    }

  /* If this was marked 'used', be sure it will be output.  */
  if (lookup_attribute ("used", DECL_ATTRIBUTES (decl)))
    mark_decl_referenced (decl);

  if (TREE_CODE (decl) == TYPE_DECL)
    {
      if (!DECL_FILE_SCOPE_P (decl)
          && variably_modified_type_p (TREE_TYPE (decl), NULL_TREE))
        add_stmt (build_stmt (DECL_EXPR, decl));

      rest_of_decl_compilation (decl, DECL_FILE_SCOPE_P (decl), 0);
    }

#if !defined(GM2)
  /* At the end of a declaration, throw away any variable type sizes
     of types defined inside that declaration.  There is no use
     computing them in the following function definition.  */
  if (current_scope == file_scope)
    get_pending_sizes ();

  /* Install a cleanup (aka destructor) if one was given.  */
  if (TREE_CODE (decl) == VAR_DECL && !TREE_STATIC (decl))
    {
      tree attr = lookup_attribute ("cleanup", DECL_ATTRIBUTES (decl));
      if (attr)
        {
          tree cleanup_id = TREE_VALUE (TREE_VALUE (attr));
          tree cleanup_decl = lookup_name (cleanup_id);
          tree cleanup;

          /* Build "cleanup(&decl)" for the destructor.  */
          cleanup = build_unary_op (ADDR_EXPR, decl, 0);
          cleanup = build_tree_list (NULL_TREE, cleanup);
          cleanup = build_function_call (cleanup_decl, cleanup);

          /* Don't warn about decl unused; the cleanup uses it.  */
          TREE_USED (decl) = 1;
          TREE_USED (cleanup_decl) = 1;

          /* Initialize EH, if we've been told to do so.  */
          if (flag_exceptions && !c_eh_initialized_p)
            {
              c_eh_initialized_p = true;
              eh_personality_libfunc
                = init_one_libfunc (USING_SJLJ_EXCEPTIONS
                                    ? "__gcc_personality_sj0"
                                    : "__gcc_personality_v0");
              default_init_unwind_resume_libfunc ();
              using_eh_for_cleanups ();
            }

          push_cleanup (decl, cleanup, false);
        }
    }
#endif
}

/*
 *  BuildTypeDeclaration - adds the, type, to the current statement list.
 */

void
gccgm2_BuildTypeDeclaration (tree type)
{
  if (cur_stmt_list != NULL) {
    enum tree_code code = TREE_CODE (type);

    if (code == TYPE_DECL || code == RECORD_TYPE || code == POINTER_TYPE) {
      // debug_tree(type);
      add_stmt (build_stmt (DECL_EXPR, build_decl (TYPE_DECL, NULL, type)));
    }
    else if (code == VAR_DECL) {
      gccgm2_BuildTypeDeclaration (TREE_TYPE (type));
      add_stmt (build_stmt (DECL_EXPR, type));
    }
  }
}

/* Get the LABEL_DECL corresponding to identifier ID as a label.
   Create one if none exists so far for the current function.
   This function is called for both label definitions and label references.  */

tree
lookup_label (tree id)
{
  tree decl = IDENTIFIER_LABEL_VALUE (id);

  if (current_function_decl == 0)
    {
      error ("label %s referenced outside of any function",
             IDENTIFIER_POINTER (id));
      return 0;
    }

  /* Use a label already defined or ref'd with this name.  */
  if (decl != 0)
    {
#if !defined(GM2)
      /* But not if it is inherited and wasn't declared to be inheritable.  */
      if (DECL_CONTEXT (decl) != current_function_decl
          && ! C_DECLARED_LABEL_FLAG (decl))
        return shadow_label (id);
#endif
      return decl;
    }

  decl = build_decl (LABEL_DECL, id, void_type_node);

  /* Make sure every label has an rtx.  */
  // label_rtx (decl);

  /* A label not explicitly declared must be local to where it's ref'd.  */
  DECL_CONTEXT (decl) = current_function_decl;

  DECL_MODE (decl) = VOIDmode;

  /* Say where one reference is to the label,
     for the sake of the error if it is not defined.  */
  DECL_SOURCE_LINE (decl) = getLineNo();
  DECL_SOURCE_FILE (decl) = input_filename;

  IDENTIFIER_LABEL_VALUE (id) = decl;

  // named_labels = tree_cons (NULL_TREE, decl, named_labels);

  return decl;
}

tree
gccgm2_BuildStartEnumeration (char *name)
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
gccgm2_BuildEndEnumeration (tree enumtype)
{
  tree finished ATTRIBUTE_UNUSED = finish_enum(enumtype, enumvalues, NULL_TREE);
  enumvalues = NULL_TREE;
  return enumtype;
}

tree
gccgm2_BuildEnumerator (char *name, tree value)
{
  tree id = get_identifier (name);
  tree copy_of_value = copy_node (value);
  tree gccenum = build_enumerator(id, copy_of_value);

  /* choose copy_of_value for enum value */
  enumvalues = chainon(gccenum, enumvalues);
  return copy_of_value;
}

/*
 *  BuildPointerType - returns a type which is a pointer to, totype.
 */

tree
gccgm2_BuildPointerType (tree totype)
{
  return build_pointer_type (skip_type_decl (totype));
}

/*
 *  BuildConstPointerType - returns a type which is a const pointer to, totype.
 */

tree
gccgm2_BuildConstPointerType (tree totype)
{
  tree t = build_pointer_type (skip_type_decl (totype));
  TYPE_READONLY (t) = TRUE;
  return t;
}

/*
 *  BuildArrayType - returns a type which is an array indexed by IndexType
 *                   and which has ElementType elements.
 */

tree
gccgm2_BuildArrayType (tree elementtype, tree indextype)
{
  if (TREE_CODE (elementtype) == FUNCTION_TYPE)
    return build_array_type (ptr_type_node, indextype);
  else
    return build_array_type (skip_type_decl (elementtype), indextype);
}


/*
 *  build_set_type - creates a set type from the, domain, [low..high]. The
 *                   values low..high all have type, range_type.
 */

tree
build_set_type (tree domain, tree range_type, int allow_void)
{
  tree type;

  if (! ORDINAL_TYPE (TREE_CODE (domain))
      && !(allow_void && TREE_CODE (domain) == VOID_TYPE))
    {
      error ("set base type must be an ordinal type");
      return NULL;
    }

  if (TYPE_SIZE (range_type) == 0)
    layout_type (range_type);

  if (TYPE_SIZE (domain) == 0)
    layout_type (domain);

  type = make_node (SET_TYPE);
  TREE_TYPE (type) = range_type;
  TYPE_DOMAIN (type) = domain;

  return type;
}

/*
 *  convert_type_to_range - does the conversion and copies the range type
 */

tree
convert_type_to_range (tree type)
{
  tree min, max;
  tree itype;

  if (! ORDINAL_TYPE (TREE_CODE (type)))
    {
      error ("ordinal type expected");
      return error_mark_node;
    }

  min = TYPE_MIN_VALUE (type);
  max = TYPE_MAX_VALUE (type);

  if (TREE_TYPE (min) != TREE_TYPE (max))
    {
      error ("range limits are not of the same type");
      return error_mark_node;
    }

  itype = build_range_type (TREE_TYPE (min), min, max);

  if (TREE_TYPE (type) == NULL_TREE) {
    layout_type (type);
    TREE_TYPE (itype) = type;
  }
  else {
    layout_type (TREE_TYPE (type));
    TREE_TYPE (itype) = TREE_TYPE (type);
  }

  layout_type (itype);
  return itype;
}

/*
 *  build_bitset_type - builds the type BITSET which is exported from
 *                      SYSTEM.  It also builds BITNUM (the subrange
 *                      from which BITSET is created).
 */

static
tree
build_bitset_type (void)
{
  bitnum_type_node = build_range_type (skip_type_decl (gccgm2_GetCardinalType()),
                                       gccgm2_BuildIntegerConstant (0),
                                       gccgm2_BuildIntegerConstant (gccgm2_GetBitsPerBitset()-1));
  layout_type (bitnum_type_node);

  if (broken_set_debugging_info)
    return unsigned_type_node;

  ASSERT((COMPLETE_TYPE_P (bitnum_type_node)), bitnum_type_node);

  return gccgm2_BuildSetTypeFromSubrange (NULL, bitnum_type_node,
                                          gccgm2_BuildIntegerConstant (0),
                                          gccgm2_BuildIntegerConstant (gccgm2_GetBitsPerBitset()-1));
}

/*
 *  BuildSetTypeFromSubrange - constructs a set type from a subrangeType.
 */

tree
gccgm2_BuildSetTypeFromSubrange (char *name, tree subrangeType,
                                 tree lowval, tree highval)
{
  lowval = gccgm2_FoldAndStrip (lowval);
  highval = gccgm2_FoldAndStrip (highval);

  if (broken_set_debugging_info)
    return unsigned_type_node;
  else {
    tree noelements = gccgm2_BuildAdd (gccgm2_BuildSub (highval, lowval, FALSE),
                                       integer_one_node,
                                       FALSE);
    tree settype;

    layout_type (subrangeType);
    settype = build_set_type (subrangeType,
                              convert_type_to_range (subrangeType), 0);
    if (gccgm2_CompareTrees (noelements, gccgm2_BuildIntegerConstant (SET_WORD_SIZE)) > 0)
      error("internal error: not expecting an internal set type to have more bits than a machine word");
  
    if (gccgm2_CompareTrees (noelements, gccgm2_BuildIntegerConstant (SET_WORD_SIZE)) == 0)
      TYPE_MAX_VALUE (settype) = TYPE_MAX_VALUE (gccgm2_GetWordType ());
    else
      TYPE_MAX_VALUE (settype) = gccgm2_FoldAndStrip (gccgm2_BuildSub (gccgm2_BuildLSL (gccgm2_GetWordOne(), noelements, FALSE),
                                                  integer_one_node,
                                                  FALSE));
    TYPE_MIN_VALUE (settype) = integer_zero_node;

    layout_type (settype);
    ASSERT((COMPLETE_TYPE_P (settype)), settype);
    
    if ((name == NULL) || (strcmp(name, "") == 0))
      /* no modula-2 name */
      return settype;
    else {
      /* declared as TYPE foo = SET OF [x..y] ; */
      settype = gccgm2_DeclareKnownType(name, settype);
      layout_type (skip_type_decl (settype));
      return settype;
    }
  }
}

/*
 *  BuildSetType - creates a SET OF [lowval..highval]
 */

tree
gccgm2_BuildSetType (char *name, tree type, tree lowval, tree highval)
{
  tree range = build_range_type (skip_type_decl (type),
                                 gccgm2_FoldAndStrip (lowval),
                                 gccgm2_FoldAndStrip (highval));

  return gccgm2_BuildSetTypeFromSubrange (name, range,
                                          gccgm2_FoldAndStrip (lowval),
                                          gccgm2_FoldAndStrip (highval));
}

/*
 *  push_constructor - returns a new compound constructor frame.
 */

static
struct struct_constructor *
push_constructor (void)
{
  struct struct_constructor *p =
    (struct struct_constructor *) xmalloc (sizeof (struct struct_constructor));

  p->level = top_constructor;
  top_constructor = p;
  return p;
}

/*
 *  pop_constructor - throws away the top constructor frame on the stack.
 */

static
void
pop_constructor (struct struct_constructor *p)
{
  ASSERT_CONDITION (p == top_constructor);  /* p should be the top_constructor */
  top_constructor = top_constructor->level;
}

/*
 *  BuildStartSetConstructor - starts to create a set constant.
 *                             Remember that type is really a record type.
 */

struct struct_constructor*
gccgm2_BuildStartSetConstructor (tree type)
{
  struct struct_constructor *p = push_constructor ();

  layout_type (type);
  p->constructor_type = type;
  p->constructor_fields = TYPE_FIELDS (type);
  p->constructor_element_list = NULL_TREE;
  p->constructor_elements = NULL;
  return p;
}

/*
 *  BuildSetConstructorElement - adds, value, to the constructor_element_list.
 */

void
gccgm2_BuildSetConstructorElement (struct struct_constructor *p, tree value)
{
  if (value == NULL_TREE) {
    internal_error ("set type cannot be initialized with a NULL_TREE");
    return;
  }

  if (p->constructor_fields == NULL) {
    internal_error ("set type does not take another integer value");
    return;
  }
  
  p->constructor_element_list = tree_cons (p->constructor_fields, value, p->constructor_element_list);
  p->constructor_fields = TREE_CHAIN (p->constructor_fields);
}

/*
 *  BuildEndSetConstructor - finishes building a set constant.
 */

tree
gccgm2_BuildEndSetConstructor (struct struct_constructor *p)
{
  tree constructor;
  tree link;

  for (link = p->constructor_element_list; link; link = TREE_CHAIN (link))
    {
      tree field = TREE_PURPOSE (link);
      DECL_SIZE (field) = bitsize_int (SET_WORD_SIZE);
      DECL_BIT_FIELD (field) = 1;
    }

  constructor = build_constructor_from_list (p->constructor_type,
					     nreverse (p->constructor_element_list));
  TREE_CONSTANT (constructor) = 1;
  TREE_STATIC (constructor) = 1;

  pop_constructor (p);

  return constructor;
}

/*
 *  BuildStartRecordConstructor - initializes a record compound
 *                                constructor frame.
 */

struct struct_constructor*
gccgm2_BuildStartRecordConstructor (tree type)
{
  struct struct_constructor *p = push_constructor ();

  layout_type (type);
  p->constructor_type = type;
  p->constructor_fields = TYPE_FIELDS (type);
  p->constructor_element_list = NULL_TREE;
  p->constructor_elements = NULL;
  return p;
}

/*
 *  BuildEndRecordConstructor - returns a tree containing the record compound literal.
 */

tree
gccgm2_BuildEndRecordConstructor (struct struct_constructor *p)
{
  tree constructor = build_constructor_from_list (p->constructor_type,
						  nreverse (p->constructor_element_list));
  TREE_CONSTANT (constructor) = 1;
  TREE_STATIC (constructor) = 1;

  pop_constructor (p);

  return constructor;
}

/*
 *  BuildRecordConstructorElement - adds, value, to the constructor_element_list.
 */

void
gccgm2_BuildRecordConstructorElement (struct struct_constructor *p, tree value)
{
  gccgm2_BuildSetConstructorElement (p, value);
}

/*
 *  BuildStartArrayConstructor - initializes an array compound
 *                               constructor frame.
 */

struct struct_constructor*
gccgm2_BuildStartArrayConstructor (tree type)
{
  struct struct_constructor *p = push_constructor ();

  layout_type (type);
  p->constructor_type = type;
  p->constructor_fields = TREE_TYPE (type);
  p->constructor_element_list = NULL_TREE;
  p->constructor_elements = NULL;
  return p;
}

/*
 *  BuildEndArrayConstructor - returns a tree containing the array
 *                             compound literal.
 */

tree
gccgm2_BuildEndArrayConstructor (struct struct_constructor *p)
{
  tree constructor;
  
  constructor = build_constructor (p->constructor_type, p->constructor_elements);
  TREE_CONSTANT (constructor) = TRUE;
  TREE_INVARIANT (constructor) = TRUE;
#if 0
  TREE_STATIC (constructor) = p->constructor_simple;
#endif
  TREE_STATIC (constructor) = TRUE;

  pop_constructor (p);

  return constructor;
}

/*
 *  BuildArrayConstructorElement - adds, value, to the constructor_element_list.
 */

void
gccgm2_BuildArrayConstructorElement (struct struct_constructor *p, tree value,
				     tree indice)
{
  constructor_elt *celt;

  if (value == NULL_TREE) {
    internal_error ("array cannot be initialized with a NULL_TREE");
    return;
  }

  if (p->constructor_fields == NULL_TREE) {
    internal_error ("array type must be initialized");
    return;
  }

  if (p->constructor_fields != TREE_TYPE (value)) {
    internal_error ("array element value must be the same as its declaration");
    return;
  }

  celt = VEC_safe_push (constructor_elt, gc, p->constructor_elements, NULL);
  celt->index = indice;
  celt->value = value;
}

/*
 *  BuildSubrangeType - creates a subrange of, type, with, lowval, highval.
 */

tree
gccgm2_BuildSubrangeType (char *name, tree type, tree lowval, tree highval)
{
  tree btype = skip_type_decl (type);
  tree lo = copy_node (gccgm2_BuildConvert (btype, gccgm2_FoldAndStrip (lowval), FALSE));
  tree hi = copy_node (gccgm2_BuildConvert (btype, gccgm2_FoldAndStrip (highval), FALSE));
  tree id;

  id = build_range_type (btype, lo, hi);
  if (tree_int_cst_sgn (lo) < 0)
    TYPE_UNSIGNED (id) = FALSE;
  else
    TYPE_UNSIGNED (id) = TRUE;
  layout_type (id);

  if ((name == NULL) || (strcmp(name, "") == 0)) {
    /* no modula-2 name thus return id */
    return id;
  } else {
    /* obviously declared as TYPE foo = [x..y] ; */
    id = gccgm2_DeclareKnownType(name, id);
    layout_type (skip_type_decl (id));
    return id;
  }
}

/*
 *  BuildArrayIndexType - creates an integer index which accesses an array.
 *                        low and high are the min, max elements of the array.
 */

tree
gccgm2_BuildArrayIndexType (tree low, tree high)
{
  tree sizelow = convert (m2_z_type_node, default_conversion (low));
  tree sizehigh = convert (m2_z_type_node, default_conversion (high));

  if (gccgm2_TreeOverflow (sizelow) || gccgm2_TreeOverflow (sizehigh))
    error("array bounds are too large or too small");
  
  return build_range_type (m2_z_type_node, sizelow, sizehigh);
}

/*
 *  BuildVariableArrayAndDeclare - creates a variable length array.
 *                                 high is the maximum legal elements (which is a runtime variable).
 *                                 This creates and array index, array type and local variable.
 */

tree
gccgm2_BuildVariableArrayAndDeclare (tree elementtype, tree high, char *name,
                                     tree scope)
{
  tree indextype = build_index_type (variable_size(high));
  tree arraytype = build_array_type (elementtype, indextype);
  tree id        = get_identifier (name);
  tree decl;

  C_TYPE_VARIABLE_SIZE (arraytype) = TRUE;
  decl = build_decl (VAR_DECL, id, arraytype);

  DECL_SOURCE_LINE(decl)  = getLineNo();
  DECL_EXTERNAL   (decl)  = FALSE;
  TREE_PUBLIC     (decl)  = 1;
  DECL_CONTEXT    (decl)  = scope;
  TREE_USED       (arraytype)  = 1;
  TREE_USED       (decl)  = 1;

  C_DECL_VARIABLE_SIZE (decl) = TRUE;
  pushdecl (decl);

  finish_decl (indextype, NULL, NULL);
  finish_decl (arraytype, NULL, NULL);
  add_stmt (build_stmt (DECL_EXPR, decl));

  return decl;
}

/*
 *  InitFunctionTypeParameters - resets the current function type parameter list.
 */

void
gccgm2_InitFunctionTypeParameters (int uses_varargs)
{
  if (uses_varargs)
    param_type_list = NULL_TREE;
  else
    param_type_list = tree_cons (NULL_TREE, void_type_node, NULL_TREE);
  param_list = NULL_TREE;   /* ready for the next time we call/define a function */
}

/*
 *  BuildStartFunctionType - creates a pointer type, necessary to
 *                           create a function type.
 */

tree
gccgm2_BuildStartFunctionType (char *name ATTRIBUTE_UNUSED)
{
  tree n = make_node (POINTER_TYPE);

  return n;
}

/*
 *  finish_build_pointer_type - finish building a POINTER_TYPE node.
 *                              necessary to solve self references in
 *                              procedure types.
 */

/* code taken from tree.c:build_pointer_type_for_mode */

static
tree
finish_build_pointer_type (tree t, tree to_type,
                           enum machine_mode mode,
                           bool can_alias_all)
{
  TREE_TYPE (t) = to_type;
  TYPE_MODE (t) = mode;
  TYPE_REF_CAN_ALIAS_ALL (t) = can_alias_all;
  TYPE_NEXT_PTR_TO (t) = TYPE_POINTER_TO (to_type);
  TYPE_POINTER_TO (to_type) = t;

  /* Lay out the type. */
  layout_type (t);

  return t;
}

/*
 *
 *  BuildEndFunctionType - build a function type which would return a, value.
 *                         The arguments have been created by BuildParameterDeclaration.
 */

tree
gccgm2_BuildEndFunctionType (tree func, tree type)
{
  if (type == NULL_TREE)
    type = void_type_node;

  type = skip_type_decl (type);
  func = finish_build_pointer_type (func,
                                    build_function_type (type, param_type_list),
                                    ptr_mode, false);
  layout_type (func);
  return func;
}

/*
 *  BuildParameterDeclaration - creates and returns one parameter from, name, and, type.
 *                              It appends this parameter to the internal param_type_list.
 *                              If name is nul then we assume we are creating a function
 *                              type declaration and we ignore names.
 */

tree
gccgm2_BuildParameterDeclaration (char *name, tree type,
				  int isreference)
{
  tree parm_decl;

  ASSERT_BOOL (isreference);
  type = skip_type_decl(type);
  layout_type (type);
  if (isreference)
    type = build_reference_type (type);

  if ((name != NULL) && (strcmp(name, "") != 0)) {
    /* creating a function with parameters */
    parm_decl = build_decl (PARM_DECL, get_identifier (name), type);
    DECL_ARG_TYPE (parm_decl) = type;
    param_list = chainon (parm_decl, param_list);
    layout_type (type);
#if 0
    printf("making parameter %s known to gcc\n", name);
#endif
    param_type_list = tree_cons (NULL_TREE, type, param_type_list);
    return parm_decl;
  } else {
    param_type_list = tree_cons (NULL_TREE, type, param_type_list);
    return type;
  }
}

/*
 *  BuildStartFunctionDeclaration - initializes global variables ready
 *                                  for building a function.
 */

void
gccgm2_BuildStartFunctionDeclaration (int uses_varargs)
{
  if (uses_varargs)
    param_type_list = NULL_TREE;
  else
    param_type_list = tree_cons (NULL_TREE, void_type_node, NULL_TREE);
  param_list = NULL_TREE;   /* ready for when we define a function */
}

tree gm2_debugging_func;

/*
 *  BuildEndFunctionDeclaration - build a function which will return a value of returntype.
 *                                The arguments have been created by BuildParameterDeclaration.
 */

tree
gccgm2_BuildEndFunctionDeclaration (char *name, tree returntype,
                                    int isexternal, int isnested)
{
  tree fntype;
  tree fndecl;

  ASSERT_BOOL (isexternal);
  ASSERT_BOOL (isnested);
  returntype = skip_type_decl (returntype);
  /*
   *  the function type depends on the return type and type of args,
   *  both of which we have created in BuildParameterDeclaration
   */
  if (returntype == NULL_TREE)
    returntype = void_type_node;
  else if (TREE_CODE (returntype) == FUNCTION_TYPE)
    returntype = ptr_type_node;

  fntype = build_function_type (returntype, param_type_list);
  fndecl = build_decl (FUNCTION_DECL, get_identifier (name), fntype);

  gm2_debugging_func = global_binding_level->names;

  DECL_EXTERNAL (fndecl)    = isexternal;
  TREE_PUBLIC (fndecl)      = (! isnested);
  TREE_STATIC (fndecl)      = 1;
  DECL_ARGUMENTS (fndecl)   = param_list;
  DECL_RESULT (fndecl)      = build_decl (RESULT_DECL, NULL_TREE, returntype);
  DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;
  TREE_TYPE (fndecl)        = fntype;

  DECL_SOURCE_FILE (fndecl) = input_filename;
  DECL_SOURCE_LINE (fndecl) = getLineNo();

  /* Prevent the optimizer from removing it if it is public. */
  if (TREE_PUBLIC (fndecl))
    gm2_mark_addressable (fndecl);

  pushdecl (fndecl);

  rest_of_decl_compilation (fndecl, 1, 0);
  param_list = NULL_TREE;   /* ready for the next time we call/define a function */
  return fndecl;
}

/*
 *  BuildStartFunctionCode - generate function entry code.
 */

void
gccgm2_BuildStartFunctionCode (tree fndecl, int isexported, int isinline)
{
  tree param_decl;
#if 0
  tree param_decl, next_param;
#endif

  ASSERT_BOOL (isexported);
  ASSERT_BOOL (isinline);
  /* Announce we are compiling this function.  */
  announce_function (fndecl);

  /* Set up to compile the function and enter it.  */

  /* Make the init_value nonzero so pushdecl knows this is not tentative.
     error_mark_node is replaced below (in poplevel) with the BLOCK.  */
  DECL_INITIAL (fndecl) = error_mark_node;

  current_function_decl = fndecl;
  pushlevel (0);

  make_decl_rtl (current_function_decl);

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

  /* set the context of these parameters to this function */
  for (param_decl = DECL_ARGUMENTS (fndecl);
       param_decl; param_decl = TREE_CHAIN (param_decl))
    DECL_CONTEXT (param_decl) = fndecl;

  /* This function exists in static storage.
     (This does not mean `static' in the C sense!)  */
  TREE_STATIC (fndecl)     = 1;
  TREE_PUBLIC (fndecl)     = isexported;
  TREE_ADDRESSABLE(fndecl) = 1;  /* (--fixme-- not sure about this) */
  DECL_INLINE (fndecl)     = 0; /* isinline; */

  init_function_start (fndecl);

#if 0
  /* open a new nesting level */
  pushlevel (0);   /* outer nesting level contains parameters and inner contains local variables */
#endif
  cur_stmt_list = chainon_stmt_list ();

  // printf("starting scope %s\n", IDENTIFIER_POINTER(DECL_NAME (fndecl)));
}

/*
 *  RememberVariables - 
 */

static void
gccgm2_RememberVariables (tree l)
{
  tree t;

  for (t = l; t; t = TREE_CHAIN (t))
    if (TREE_CODE (t) == VAR_DECL)
      add_stmt (build_stmt (DECL_EXPR, t));
}

/*
 *  BuildEndFunctionCode - generates the function epilogue.
 */

void
gccgm2_BuildEndFunctionCode (tree fndecl, int nested)
{
  tree block = poplevel (1, 0, 1);   /* shutdown parameters and function */

  BLOCK_SUPERCONTEXT (block) = fndecl;

  /* Must mark the RESULT_DECL as being in this function.  */
  DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;

  /* And attach it to the function.  */
  DECL_INITIAL (fndecl) = block;

  DECL_SAVED_TREE (fndecl) = build3 (BIND_EXPR, void_type_node,
                                     BLOCK_VARS (block),   /* was BLOCK_VARS(local) */
                                     cur_stmt_list, block);
  if (cfun != NULL)
    cfun->function_end_locus = input_location;

  cur_stmt_list = NULL;

  // dump_function (TDI_original, fndecl);
  gimplify_function_tree (fndecl);
  // dump_function (TDI_generic, fndecl);

  if (nested) {
    (void) cgraph_node (fndecl);
    current_function_decl = DECL_CONTEXT (fndecl);
  }
  else {
    current_function_decl = fndecl;
    cgraph_finalize_function (fndecl, nested);
    current_function_decl = NULL;
  }

  // printf("ending scope %s\n", IDENTIFIER_POINTER(DECL_NAME (fndecl)));
}

/*
 *  BuildReturnValueCode - generates the code associated with: RETURN( value )
 */

void
gccgm2_BuildReturnValueCode (tree fndecl, tree value)
{
  tree ret_stmt;

  DECL_SOURCE_LINE (DECL_RESULT (fndecl)) = getLineNo();
  DECL_SOURCE_FILE (DECL_RESULT (fndecl)) = input_filename;

  if (TREE_CODE (TREE_TYPE (value)) == FUNCTION_TYPE) {
    tree t = build2 (MODIFY_EXPR,
                     TREE_TYPE (DECL_RESULT (fndecl)),
                     DECL_RESULT (fndecl),
                     build1 (CONVERT_EXPR, ptr_type_node, value));
    ret_stmt = build_stmt (RETURN_EXPR, t);
  } else {
    tree t = build2 (MODIFY_EXPR,
                     TREE_TYPE (DECL_RESULT (fndecl)),
                     DECL_RESULT (fndecl),
                     gccgm2_BuildConvert (TREE_TYPE (DECL_RESULT (fndecl)), value, FALSE));
    ret_stmt = build_stmt (RETURN_EXPR, t);
  }
  add_stmt (ret_stmt);
}

/*
 *  BuildPushFunctionContext - pushes the current function context.
 *                             Maps onto push_function_context in ../function.c
 */

void
gccgm2_BuildPushFunctionContext (void)
{
  push_function_context();
}

/*
 *  BuildPopFunctionContext - pops the current function context.
 *                            Maps onto pop_function_context in ../function.c
 */

void
gccgm2_BuildPopFunctionContext (void)
{
  pop_function_context();
}

/* Save and reinitialize the variables
   used during compilation of a function.  */

void
gm2_enter_nested (struct function *f)
{
  struct language_function *p;
  p = GGC_NEW (struct language_function);
  f->language = p;

  p->stmt_tree = cur_stmt_list;
  cur_stmt_list = NULL_TREE;
}

/* Restore the variables used during compilation of a function.  */

void
gm2_leave_nested (struct function *f)
{
  struct language_function *p = f->language;

  if (DECL_STRUCT_FUNCTION (current_function_decl) == 0
      && DECL_SAVED_TREE (current_function_decl) == NULL_TREE)
    {
      /* Stop pointing to the local nodes about to be freed.  */
      /* But DECL_INITIAL must remain nonzero so we know this
         was an actual function definition.  */
      DECL_INITIAL (current_function_decl) = error_mark_node;
      DECL_ARGUMENTS (current_function_decl) = 0;
    }

  cur_stmt_list = p->stmt_tree;
  f->language = NULL;
}

/*
 *  BuildAssignment - builds the assignment of, des, and, expr.
 *                    It returns, des.
 */

tree
gccgm2_BuildAssignment (tree des, tree expr)
{
  if (TREE_CODE (expr) == FUNCTION_DECL)
    expr = build_unary_op (ADDR_EXPR, expr, 0);

  if (TREE_TYPE (expr) != TREE_TYPE (des))
    add_stmt (build2 (MODIFY_EXPR, TREE_TYPE (des), des,
		      gccgm2_BuildConvert (TREE_TYPE (des), expr, FALSE)));
  else
    add_stmt (build2 (MODIFY_EXPR, TREE_TYPE (des), des, expr));
  return des;
}

/*
 *  BuildAdd - builds an addition tree.
 */

tree
gccgm2_BuildAdd (tree op1, tree op2, int needconvert)
{
  return build_binary_op (PLUS_EXPR,
                          gccgm2_FoldAndStrip (op1),
                          gccgm2_FoldAndStrip (op2), needconvert);
}

/*
 *  BuildSub - builds a subtraction tree.
 */

tree
gccgm2_BuildSub (tree op1, tree op2, int needconvert)
{
  return build_binary_op (MINUS_EXPR,
                          gccgm2_FoldAndStrip (op1),
                          gccgm2_FoldAndStrip (op2), needconvert);
}

/*
 *  BuildMult - builds a multiplication tree.
 */

tree
gccgm2_BuildMult (tree op1, tree op2, int needconvert)
{
  return build_binary_op (MULT_EXPR,
                          gccgm2_FoldAndStrip (op1),
                          gccgm2_FoldAndStrip (op2), needconvert);
}

/*
 *  BuildDivTrunc - builds a division tree.
 */

tree
gccgm2_BuildDivTrunc (tree op1, tree op2, int needconvert)
{
  return build_binary_op (TRUNC_DIV_EXPR,
                          gccgm2_FoldAndStrip (op1),
                          gccgm2_FoldAndStrip (op2), needconvert);
}

/*
 *  BuildModTrunc - builds a modulus tree.
 */

tree
gccgm2_BuildModTrunc (tree op1, tree op2, int needconvert)
{
  return build_binary_op (TRUNC_MOD_EXPR,
                          gccgm2_FoldAndStrip (op1),
                          gccgm2_FoldAndStrip (op2), needconvert);
}

/*
 *  BuildDivFloor - builds a division tree.
 */

tree
gccgm2_BuildDivFloor (tree op1, tree op2, int needconvert)
{
  return build_binary_op (FLOOR_DIV_EXPR,
                          gccgm2_FoldAndStrip (op1),
                          gccgm2_FoldAndStrip (op2), needconvert);
}

/*
 *  BuildModFloor - builds a modulus tree.
 */

tree
gccgm2_BuildModFloor (tree op1, tree op2, int needconvert)
{
  return build_binary_op (FLOOR_MOD_EXPR,
                          gccgm2_FoldAndStrip (op1),
                          gccgm2_FoldAndStrip (op2), needconvert);
}

/*
 *  BuildLSL - builds and returns tree (op1 << op2)
 */

tree
gccgm2_BuildLSL (tree op1, tree op2, int needconvert)
{
  return build_binary_op (LSHIFT_EXPR,
                          gccgm2_FoldAndStrip (op1),
                          gccgm2_FoldAndStrip (op2), needconvert);
}

/*
 *  BuildLSR - builds and returns tree (op1 >> op2)
 */

tree
gccgm2_BuildLSR (tree op1, tree op2, int needconvert)
{
  return build_binary_op (RSHIFT_EXPR,
                          gccgm2_FoldAndStrip (op1),
                          gccgm2_FoldAndStrip (op2), needconvert);
}

/*
 *  createUniqueLabel - returns a unique label which has been
 *                      xmalloc'ed.
 */

static char *
createUniqueLabel (void)
{
  int size, i;
  char *label;

  label_count++;  
  i = label_count;
  size = strlen(".LSHIFT")+2;
  while (i>0) {
    i /= 10;
    size++;
  }
  label = (char *)xmalloc (size);
  sprintf(label, ".LSHIFT%d", label_count);
  return label;
}

/*
 *  BuildLogicalShift - builds the ISO Modula-2 SHIFT operator
 *                      for a fundamental data type.
 */

void
gccgm2_BuildLogicalShift (tree op1, tree op2, tree op3,
                          tree nBits ATTRIBUTE_UNUSED,
                          int needconvert)
{
  tree res;

  op2 = gccgm2_FoldAndStrip (op2);
  op3 = gccgm2_FoldAndStrip (op3);
  if (TREE_CODE (op3) == INTEGER_CST) {
    if (tree_int_cst_sgn (op3) < 0)
      res = gccgm2_BuildLSR (op2,
                             gccgm2_BuildNegate (op3, needconvert),
                             needconvert);
    else
      res = gccgm2_BuildLSL (op2, op3, needconvert);
    gccgm2_BuildAssignment (op1, res);
  }
  else {
    char *labelElseName = createUniqueLabel ();
    char *labelEndName  = createUniqueLabel ();
    tree  is_less       = gccgm2_BuildLessThan (op3, gccgm2_GetIntegerZero ());

    gccgm2_DoJump (is_less, NULL, labelElseName);
    res = gccgm2_BuildLSL (op2, op3, needconvert);
    gccgm2_BuildAssignment (op1, res);
    gccgm2_BuildGoto (labelEndName);
    gccgm2_DeclareLabel (labelElseName);
    res = gccgm2_BuildLSR (op2,
                           gccgm2_BuildNegate (op3, needconvert),
                           needconvert);
    gccgm2_BuildAssignment (op1, res);
    gccgm2_DeclareLabel (labelEndName);
  }
}

/*
 *  BuildLRL - builds and returns tree (op1 rotate left by op2 bits)
 */

tree
gccgm2_BuildLRL (tree op1, tree op2, int needconvert)
{
  return build_binary_op (LROTATE_EXPR, op1, op2, needconvert);
}

/*
 *  BuildLRR - builds and returns tree (op1 rotate right by op2 bits)
 */

tree
gccgm2_BuildLRR (tree op1, tree op2, int needconvert)
{
  return build_binary_op (RROTATE_EXPR, op1, op2, needconvert);
}

/*
 *  gccgm2_BuildMask - returns a tree for the mask of a set
 *                     of nBits. It assumes nBits is <= TSIZE(WORD)
 */

tree
gccgm2_BuildMask (tree nBits, int needconvert)
{
  tree mask = gccgm2_BuildLSL (gccgm2_GetIntegerOne (),
                               nBits, needconvert);
  return gccgm2_BuildSub (mask, gccgm2_GetIntegerOne (),
                          needconvert);
}

/*
 *  BuildLRLn - builds and returns tree (op1 rotate left by op2 bits)
 *              it rotates a set of size, nBits.
 */

tree
gccgm2_BuildLRLn (tree op1, tree op2, tree nBits,
		  int  needconvert)
{
  /*
   *  ensure we wrap the rotate
   */
  op2 = gccgm2_BuildModTrunc (op2, nBits, needconvert);
  /*
   *  optimize if we are we going to rotate a TSIZE(BITSET) set
   */
  if (gccgm2_CompareTrees (gccgm2_BuildIntegerConstant (gccgm2_GetBitsPerBitset ()),
                           nBits) == 0)
    return build_binary_op (LROTATE_EXPR, op1, op2, needconvert);
  else {
    tree mask = gccgm2_BuildMask (nBits, needconvert);
    tree left, right;

    /*
     *  make absolutely sure there are no high order bits lying around
     */
    op1 = gccgm2_BuildLogicalAnd (op1, mask, needconvert);
    left = gccgm2_BuildLSL (op1, op2, needconvert);
    left = gccgm2_BuildLogicalAnd (left, mask, needconvert);
    right = gccgm2_BuildLSR (op1, gccgm2_BuildSub (nBits, op2, needconvert),
                             needconvert);
    return gccgm2_BuildLogicalOr (left, right, needconvert);
  }
}

/*
 *  BuildLRRn - builds and returns tree (op1 rotate right by op2 bits)
 *              it rotates a set of size, nBits.
 */

tree
gccgm2_BuildLRRn (tree op1, tree op2, tree nBits,
		  int  needconvert)
{
  /*
   *  ensure we wrap the rotate
   */
  op2 = gccgm2_BuildModTrunc (op2, nBits, needconvert);
  /*
   *  optimize if we are we going to rotate a TSIZE(BITSET) set
   */
  if (gccgm2_CompareTrees (gccgm2_BuildIntegerConstant (gccgm2_GetBitsPerBitset ()),
                           nBits) == 0)
    return build_binary_op (RROTATE_EXPR, op1, op2, needconvert);
  else {
    tree mask = gccgm2_BuildMask (nBits, needconvert);
    tree left, right;

    /*
     *  make absolutely sure there are no high order bits lying around
     */
    op1 = gccgm2_BuildLogicalAnd (op1, mask, needconvert);
    right = gccgm2_BuildLSR (op1, op2, needconvert);
    left = gccgm2_BuildLSL (op1, gccgm2_BuildSub (nBits, op2, needconvert),
                            needconvert);
    left = gccgm2_BuildLogicalAnd (left, mask, needconvert);
    return gccgm2_BuildLogicalOr (left, right, needconvert);
  }
}

/*
 *  BuildLogicalRotate - builds the ISO Modula-2 ROTATE operator
 *                       for a fundamental data type.
 */

void
gccgm2_BuildLogicalRotate (tree op1, tree op2, tree op3,
                           tree nBits,
                           int needconvert)
{
  tree res;

  op2 = gccgm2_FoldAndStrip (op2);
  op3 = gccgm2_FoldAndStrip (op3);
  if (TREE_CODE (op3) == INTEGER_CST) {
    if (tree_int_cst_sgn (op3) < 0)
      res = gccgm2_BuildLRRn (op2,
                             gccgm2_BuildNegate (op3, needconvert),
                             nBits,
                             needconvert);
    else
      res = gccgm2_BuildLRLn (op2, op3, nBits, needconvert);
    gccgm2_BuildAssignment (op1, res);
  }
  else {
    char *labelElseName = createUniqueLabel ();
    char *labelEndName  = createUniqueLabel ();
    tree  is_less       = gccgm2_BuildLessThan (op3, gccgm2_GetIntegerZero ());

    gccgm2_DoJump (is_less, NULL, labelElseName);
    res = gccgm2_BuildLRLn (op2, op3, nBits, needconvert);
    gccgm2_BuildAssignment (op1, res);
    gccgm2_BuildGoto (labelEndName);
    gccgm2_DeclareLabel (labelElseName);
    res = gccgm2_BuildLRRn (op2,
                            gccgm2_BuildNegate (op3, needconvert),
                            nBits,
                            needconvert);
    gccgm2_BuildAssignment (op1, res);
    gccgm2_DeclareLabel (labelEndName);
  }
}

/*
 *  buildUnboundedArrayOf - constructs an unbounded struct and returns
 *                          the gcc tree. The two fields of the structure
 *                          are initialized to contentsPtr and high.
 */

tree
buildUnboundedArrayOf (tree unbounded, tree contentsPtr, tree high)
{
  tree fields     = TYPE_FIELDS (unbounded);
  tree field_list = NULL_TREE;
  tree constructor;

  field_list = tree_cons (fields, contentsPtr, field_list);
  fields = TREE_CHAIN (fields);

  field_list = tree_cons (fields, high, field_list);

  constructor = build_constructor_from_list (unbounded, nreverse (field_list));
  TREE_CONSTANT (constructor) = 0;
  TREE_STATIC (constructor) = 0;

  return constructor;
}

/*
 *  BuildBinarySetDo - if the size of the set is <= TSIZE(WORD) then
 *                        op1 := binop(op2, op3)
 *                     else
 *                        call m2rtsprocedure(op1, op2, op3)
 */

void
gccgm2_BuildBinarySetDo (tree settype, tree op1, tree op2, tree op3,
			 tree (*binop)(tree, tree, tree, tree, int),
			 int is_op1lvalue, int is_op2lvalue, int is_op3lvalue,
			 tree nBits,
			 tree unbounded,
			 tree varproc, tree leftproc, tree rightproc)
{
  tree size     = gccgm2_GetSizeOf (settype);
  int  is_const = FALSE;
  int  is_left  = FALSE;

  ASSERT_BOOL (is_op1lvalue);
  ASSERT_BOOL (is_op2lvalue);
  ASSERT_BOOL (is_op3lvalue);

  if (gccgm2_CompareTrees (size, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    (*binop) (get_rvalue (op1, settype, is_op1lvalue),
              get_rvalue (op2, settype, is_op2lvalue),
              get_rvalue (op3, settype, is_op3lvalue),
              nBits,
              FALSE);
  else {
    tree result;
    tree high = gccgm2_BuildSub (gccgm2_BuildDivTrunc (size,
                                                       gccgm2_GetSizeOf (bitset_type_node), FALSE),
                                 gccgm2_GetIntegerOne (), FALSE);

    /*
     * if op3 is constant
     * then
     *    make op3 positive and remember which direction we are shifting
     * fi
     */
    op3 = skip_const_decl (op3);
    if (TREE_CODE (op3) == INTEGER_CST) {
      is_const = TRUE;
      if (tree_int_cst_sgn (op3) < 0)
        op3 = gccgm2_BuildNegate (op3, FALSE);
      else
        is_left = TRUE;
      op3 = gccgm2_BuildConvert (gccgm2_GetM2CardinalType (), op3, FALSE);
    }

    /*
     *  these parameters must match the prototypes of the procedures:
     *  ShiftLeft, ShiftRight, ShiftVal, RotateLeft, RotateRight, RotateVal
     *  inside gm2-iso/SYSTEM.mod.
     *
     *  Remember we must build the parameters in reverse.
     */

    /* parameter 4 amount */
    gccgm2_BuildParam (get_rvalue (op3, skip_type_decl (TREE_TYPE (op3)),
                                   is_op3lvalue));

    /* parameter 3 nBits */
    gccgm2_BuildParam (nBits);

    /* parameter 2 destination set */
    gccgm2_BuildParam (buildUnboundedArrayOf (unbounded,
                                              get_set_address (op1, 
                                                               is_op1lvalue),
                                              high));

    /* parameter 1 source set */
    gccgm2_BuildParam (buildUnboundedArrayOf (unbounded,
                                              get_set_address (op2,
                                                               is_op2lvalue),
                                              high));

    /* now call the appropriate procedure inside SYSTEM.mod */
    if (is_const)
      if (is_left)
        result = gccgm2_BuildProcedureCall (leftproc, NULL_TREE);
      else
        result = gccgm2_BuildProcedureCall (rightproc, NULL_TREE);
    else
      result = gccgm2_BuildProcedureCall (varproc, NULL_TREE);
    add_stmt (result);
  }
}

/*
 *  BuildConvert - build and return tree VAL(op1, op2)
 *                 where op1 is the type to which op2
 *                 is to be converted.
 *                 checkOverflow determines whether we
 *                 should suppress overflow checking.
 */

tree
gccgm2_BuildConvert (tree op1, tree op2, int checkOverflow)
{
  if (checkOverflow)
    return convert_and_check (skip_type_decl (op1), op2);
  else
    return convert (skip_type_decl (op1), op2);
}

/*
 *  gccgm2_TreeOverflow - returns TRUE if the contant expression, t, has
 *                        caused an overflow. No error message or warning
 *                        is emitted and no modification is made to, t.
 */

int
gccgm2_TreeOverflow (tree t)
{
  if ((TREE_CODE (t) == INTEGER_CST
       || (TREE_CODE (t) == COMPLEX_CST
           && TREE_CODE (TREE_REALPART (t)) == INTEGER_CST))
      && TREE_OVERFLOW (t))
    return TRUE;
  else if ((TREE_CODE (t) == REAL_CST
            || (TREE_CODE (t) == COMPLEX_CST
                && TREE_CODE (TREE_REALPART (t)) == REAL_CST))
           && TREE_OVERFLOW (t))
    return TRUE;
  else
    return FALSE;
}

/*
 *  RemoveOverflow - if tree, t, is a constant expression it removes
 *                   any overflow flag and returns, t.
 */

tree
gccgm2_RemoveOverflow (tree t)
{
  if (TREE_CODE (t) == INTEGER_CST
      || (TREE_CODE (t) == COMPLEX_CST
          && TREE_CODE (TREE_REALPART (t)) == INTEGER_CST)) {
    TREE_OVERFLOW (t) = 0;
    TREE_CONSTANT_OVERFLOW (t) = 0;
  }
  else if (TREE_CODE (t) == REAL_CST
           || (TREE_CODE (t) == COMPLEX_CST
               && TREE_CODE (TREE_REALPART (t)) == REAL_CST)) {
    TREE_OVERFLOW (t) = 0;
    TREE_CONSTANT_OVERFLOW (t) = 0;
  }
  return t;
}

/*
 *  BuildCoerce - returns a tree containing the expression, expr, after
 *                it has been coersed to, type.
 */

tree
gccgm2_BuildCoerce (tree des, tree type, tree expr)
{
  tree copy = copy_node (expr);
  TREE_TYPE (copy) = type;

  return build_modify_expr (des, NOP_EXPR, copy);
}

/*
 *  BuildTrunc - returns an integer expression from a REAL or LONGREAL op1.
 */

tree
gccgm2_BuildTrunc (tree op1)
{
  return convert_to_integer (gccgm2_GetIntegerType (), op1);
}

/*
 *  BuildNegate - builds a negate expression and returns the tree.
 */

tree
gccgm2_BuildNegate (op1, needconvert)
     tree op1;
     int  needconvert;
{
  tree type = TREE_TYPE (op1);
  enum tree_code code = TREE_CODE (type);

  if (code == ENUMERAL_TYPE)
    error ("not expecting to negate an enumerated value");

  return build_unary_op (NEGATE_EXPR, op1, needconvert);
}

/*
 *  BuildSetNegate - builds a set negate expression and returns the tree.
 */

tree
gccgm2_BuildSetNegate (tree op1, int needconvert)
{
  return build_binary_op (BIT_XOR_EXPR,
                          gccgm2_BuildConvert(gccgm2_GetWordType (), op1, FALSE),
                          set_full_complement,
                          needconvert);
}

/*
 *  gccgm2_GetSizeOfInBits - returns the number of bits used to contain, type.
 */

tree
gccgm2_GetSizeOfInBits (tree type)
{
  enum tree_code code = TREE_CODE (type);

  if (code == FUNCTION_TYPE)
    return gccgm2_GetSizeOfInBits (ptr_type_node);

  if (code == VOID_TYPE) {
    error ("sizeof applied to a void type");
    return size_one_node;
  }

  if (code == VAR_DECL)
    return gccgm2_GetSizeOfInBits (TREE_TYPE (type));

  if (code == PARM_DECL)
    return gccgm2_GetSizeOfInBits (TREE_TYPE (type));

  if (code == TYPE_DECL)
    return gccgm2_GetSizeOfInBits (TREE_TYPE (type));

  if (code == ERROR_MARK)
    return size_one_node;

  if (!COMPLETE_TYPE_P (type))
    {
      error ("sizeof applied to an incomplete type");
      return size_zero_node;
    }

  return gccgm2_BuildIntegerConstant (TYPE_PRECISION (type));
}

/*
 *  gccgm2_GetSizeOf - taken from c-typeck.c (c_sizeof).
 */

tree
gccgm2_GetSizeOf (tree type)
{
  enum tree_code code = TREE_CODE (type);

  if (code == FUNCTION_TYPE)
    return gccgm2_GetSizeOf(ptr_type_node);

  if (code == VOID_TYPE) {
    if (pedantic || warn_pointer_arith)
      pedwarn ("sizeof applied to a void type");
    return size_one_node;
  }

  if (code == VAR_DECL)
    return gccgm2_GetSizeOf(TREE_TYPE(type));

  if (code == PARM_DECL)
    return gccgm2_GetSizeOf(TREE_TYPE(type));

  if (code == TYPE_DECL)
    return gccgm2_GetSizeOf(TREE_TYPE(type));

  if (code == ERROR_MARK)
    return size_one_node;

  if (code == CONSTRUCTOR)
    return gccgm2_GetSizeOf(TREE_TYPE(type));

  if (!COMPLETE_TYPE_P (type))
    {
      error ("sizeof applied to an incomplete type");
      return size_zero_node;
    }

  /* Convert in case a char is more than one unit.  */
  return size_binop (CEIL_DIV_EXPR, TYPE_SIZE_UNIT (type),
                     size_int (TYPE_PRECISION (char_type_node)
                               / BITS_PER_UNIT));
}

/*
 *  BuildSize - builds a SIZE function expression and returns the tree.
 */

tree
gccgm2_BuildSize (tree op1, int needconvert ATTRIBUTE_UNUSED)
{
  return gccgm2_GetSizeOf(op1);
}

/*
 *  BuildAddr - builds an expression which calculates the address of
 *              op1 and returns the tree.
 */

tree
gccgm2_BuildAddr (tree op1, int needconvert)
{
  return build_unary_op (ADDR_EXPR, op1, needconvert);
}

#if defined(DEBUGGING)
static void debug_print_value (t)
     tree t;
{
  if (TREE_CODE(t) == INTEGER_CST) {
    fprintf(stderr, "value of tree [low = %d, high = %d]\n",
            TREE_INT_CST_LOW (t),
            TREE_INT_CST_HIGH (t));
  }
}
#endif

/*
 *  determinePenultimateField - returns the field associated with the DECL_CONTEXT (field)
 *                              within a record or varient.
 *                              record, is a record/varient but it maybe an outer nested record
 *                              to the field that we are searching. Ie:
 *
 *                              record = RECORD
 *                                          x: CARDINAL ;
 *                                          y: RECORD
 *                                                field: CARDINAL ;
 *                                             END
 *                                       END ;
 *
 *                              determinePenultimateField (record, field) returns, y.
 *
 *                              we are assurred that the chain of records leading to field
 *                              will be unique as they are built on the fly to implement varient
 *                              records.
 */

static
tree
determinePenultimateField (tree record, tree field)
{
  tree fieldlist = TYPE_FIELDS (record);
  tree x, r;

  for (x = fieldlist; x; x = TREE_CHAIN (x)) {
    if (DECL_CONTEXT (field) == TREE_TYPE (x))
      return x;
    switch (TREE_CODE (TREE_TYPE (x))) {
      case RECORD_TYPE:
      case UNION_TYPE:
        r = determinePenultimateField (TREE_TYPE (x), field);
        if (r != NULL)
          return r;
        break;
      default:
        break;
    }
  }
  return NULL_TREE;
}

/*
 *  BuildOffset1 - builds an expression containing the number of bytes the field
 *                 is offset from the start of the record structure.
 *                 This function is the same as the above, except that it
 *                 derives the record from the field and then calls BuildOffset.
 *                 The expression is returned.
 */

tree
gccgm2_BuildOffset1 (tree field, int needconvert ATTRIBUTE_UNUSED)
{
  return gccgm2_BuildOffset (DECL_CONTEXT (field), field, needconvert);
}

/*
 *  BuildOffset - builds an expression containing the number of bytes the field
 *                is offset from the start of the record structure.
 *                The expression is returned.
 */

tree
gccgm2_BuildOffset (tree record, tree field,
		    int  needconvert ATTRIBUTE_UNUSED)
{
  if (DECL_CONTEXT (field) == record)
    return gccgm2_BuildConvert (gccgm2_GetIntegerType (),
                                gccgm2_BuildAdd (DECL_FIELD_OFFSET (field),
                                                 gccgm2_BuildDivTrunc (DECL_FIELD_BIT_OFFSET (field),
                                                                       gccgm2_BuildIntegerConstant (BITS_PER_UNIT),
                                                                       FALSE),
                                                 FALSE),
                                FALSE);
  else {
    tree r1 = DECL_CONTEXT (field);
    tree r2 = determinePenultimateField (record, field);
    return gccgm2_BuildConvert (gccgm2_GetIntegerType (),
                                gccgm2_BuildAdd (gccgm2_BuildOffset (r1, field, needconvert),
                                                 gccgm2_BuildOffset (record, r2, needconvert),
                                                 FALSE),
                                FALSE);
  }
}

/*
 *  BuildLogicalOrAddress - build a logical or expressions and return the tree.
 */

tree
gccgm2_BuildLogicalOrAddress (tree op1, tree op2,
			      int  needconvert)
{
  return build_binary_op (BIT_IOR_EXPR, op1, op2, needconvert);
}

/*
 *  BuildLogicalOr - build a logical or expressions and return the tree.
 */

tree
gccgm2_BuildLogicalOr (tree op1, tree op2,
		       int  needconvert)
{
  return build_binary_op (BIT_IOR_EXPR,
                          gccgm2_BuildConvert (gccgm2_GetWordType (), op1, FALSE),
                          gccgm2_BuildConvert (gccgm2_GetWordType (), op2, FALSE), needconvert);
}

/*
 *  BuildLogicalAnd - build a logical and expression and return the tree.
 */

tree
gccgm2_BuildLogicalAnd (tree op1, tree op2,
			int  needconvert)
{
  return build_binary_op (BIT_AND_EXPR,
                          gccgm2_BuildConvert (gccgm2_GetWordType (), op1, FALSE),
                          gccgm2_BuildConvert (gccgm2_GetWordType (), op2, FALSE), needconvert);
}

/*
 *  BuildSymmetricalDifference - build a logical xor expression and return the tree.
 */

tree
gccgm2_BuildSymmetricDifference (tree op1, tree op2,
				 int  needconvert)
{
  return build_binary_op (BIT_XOR_EXPR,
                          gccgm2_BuildConvert (gccgm2_GetWordType (), op1, FALSE),
                          gccgm2_BuildConvert (gccgm2_GetWordType (), op2, FALSE), needconvert);
}


/*
 *  BuildLogicalDifference - build a logical difference expression and
 *                           return the tree.
 *                           (op1 and (not op2))
 */

tree
gccgm2_BuildLogicalDifference (tree op1, tree op2,
			       int needconvert)
{
  return build_binary_op (BIT_AND_EXPR,
                          gccgm2_BuildConvert (gccgm2_GetWordType (), op1, FALSE),
                          gccgm2_BuildSetNegate (op2, needconvert),
                          needconvert);
}

/*
 *  create_label_from_name - returns a tree label.
 */

tree
create_label_from_name (char *name)
{
  tree id   = get_identifier (name);   /* name must never conflict with the scope universe */
  tree decl = lookup_label (id);
  
  if (decl == 0)
    error ("problems trying to create a label");
 
  return decl;
}

/*
 *  BuildGoto - builds a goto operation.
 */

void
gccgm2_BuildGoto (char *name)
{
  tree decl = create_label_from_name (name);
  tree t;

  TREE_USED (decl) = 1;
  t = build (GOTO_EXPR, void_type_node, decl);
  add_stmt (t);
}

/*
 *  DeclareLabel - create a label, name.
 */

void
gccgm2_DeclareLabel (char *name)
{
  tree id = get_identifier (name);
  tree decl = lookup_label (id);

  if (decl == 0)
    error ("problems trying to declare a label");
  else {
    TREE_USED (decl) = 1;
    DECL_SOURCE_FILE (decl) = input_filename;
    DECL_SOURCE_LINE (decl) = getLineNo();
  }
  DECL_CONTEXT (decl) = current_function_decl;

  add_stmt (build1 (LABEL_EXPR, void_type_node, decl));
}

tree
boolean_to_unsigned (tree t)
{
  tree type = TREE_TYPE (t);

  if (TREE_CODE (skip_type_decl (type)) == BOOLEAN_TYPE)
    return convert (integer_type_node, t);
  else
    return t;
}

/*
 *  BuildLessThan - return a tree which computes <
 */

tree
gccgm2_BuildLessThan (tree op1, tree op2)
{
  return build_binary_op (LT_EXPR,
                          boolean_to_unsigned (op1),
                          boolean_to_unsigned (op2), 0);
}

/*
 *  BuildGreaterThan - return a tree which computes >
 */

tree
gccgm2_BuildGreaterThan (tree op1, tree op2)
{
  return build_binary_op (GT_EXPR,
                          boolean_to_unsigned (op1),
                          boolean_to_unsigned (op2), 0);
}


/*
 *  BuildLessThanOrEqual - return a tree which computes <
 */

tree
gccgm2_BuildLessThanOrEqual (tree op1, tree op2)
{
  return build_binary_op (LE_EXPR,
                          boolean_to_unsigned (op1),
                          boolean_to_unsigned (op2), TRUE);
}


/*
 *  BuildGreaterThanOrEqual - return a tree which computes >=
 */

tree
gccgm2_BuildGreaterThanOrEqual (tree op1, tree op2)
{
  return build_binary_op (GE_EXPR,
                          boolean_to_unsigned (op1),
                          boolean_to_unsigned (op2), 0);
}


/*
 *  BuildEqualTo - return a tree which computes =
 */

tree
gccgm2_BuildEqualTo (tree op1, tree op2)
{
  return build_binary_op (EQ_EXPR,
                          boolean_to_unsigned (op1),
                          boolean_to_unsigned (op2), 0);
}


/*
 *  BuildEqualNotTo - return a tree which computes #
 */

tree
gccgm2_BuildNotEqualTo (tree op1, tree op2)
{
  return build_binary_op (NE_EXPR,
                          boolean_to_unsigned (op1),
                          boolean_to_unsigned (op2), 0);
}

/*
 *  BuildIsSuperset - return a tree which computes:  op1 & op2 == op2
 */

tree
gccgm2_BuildIsSuperset (tree op1, tree op2)
{
  return gccgm2_BuildEqualTo (op2,
                              gccgm2_BuildLogicalAnd (op1, op2, FALSE));
}

/*
 *  BuildIsNotSuperset - return a tree which computes: op1 & op2 != op2
 */

tree
gccgm2_BuildIsNotSuperset (tree op1, tree op2)
{
  return gccgm2_BuildNotEqualTo (op2,
                                 gccgm2_BuildLogicalAnd (op1, op2, FALSE));
}

/*
 *  BuildIsSubset - return a tree which computes:  op1 & op2 == op1
 */

tree
gccgm2_BuildIsSubset (tree op1, tree op2)
{
  return gccgm2_BuildEqualTo (op1,
                              gccgm2_BuildLogicalAnd (op1, op2, FALSE));
}

/*
 *  BuildIsNotSubset - return a tree which computes: op1 & op2 != op1
 */

tree
gccgm2_BuildIsNotSubset (tree op1, tree op2)
{
  return gccgm2_BuildNotEqualTo (op1,
                                 gccgm2_BuildLogicalAnd (op1, op2, FALSE));
}


/*
 *  do_jump_if_bit - tests bit in word against integer zero using operator, code.
 *                   If the result is true then jump to label.
 */

static
void
do_jump_if_bit (enum tree_code code, tree word, tree bit, char *label)
{
  gccgm2_DoJump (build_binary_op (code,
                                  build_binary_op (BIT_AND_EXPR,
                                                   gccgm2_BuildConvert (gccgm2_GetWordType (), word, FALSE),
                                                   gccgm2_BuildConvert (gccgm2_GetWordType (), gccgm2_BuildLSL (gccgm2_GetWordOne(),
                                                                                                                gccgm2_BuildConvert (gccgm2_GetWordType (), bit, FALSE),
                                                                                                                FALSE), FALSE),
                                                   FALSE),
                                  integer_zero_node, FALSE),
                 NULL, label);
}

/*
 *  BuildIfConstInVar - generates: if constel in varset then goto label.
 */

void
gccgm2_BuildIfConstInVar (tree type, tree varset, tree constel,
			  int is_lvalue, int fieldno,
			  char *label)
{
  tree size = gccgm2_GetSizeOf (type);

  ASSERT_BOOL (is_lvalue);
  if (gccgm2_CompareTrees (size, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    do_jump_if_bit (NE_EXPR, get_rvalue (varset, type, is_lvalue), constel, label);
  else {
    tree p1 = get_set_address (varset, is_lvalue);
    tree fieldlist = TYPE_FIELDS (type);
    tree field;

    for (field = fieldlist; (field != NULL) && (fieldno>0); field = TREE_CHAIN (field))
      fieldno--;

    do_jump_if_bit (NE_EXPR, get_set_field_rhs (p1, field), constel, label);
  }
}

/*
 *  BuildIfConstInVar - generates: if not (constel in varset) then goto label.
 */

void
gccgm2_BuildIfNotConstInVar (tree type, tree varset, tree constel,
			     int is_lvalue, int fieldno,
			     char *label)
{
  tree size = gccgm2_GetSizeOf (type);

  ASSERT_BOOL (is_lvalue);
  if (gccgm2_CompareTrees (size, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    do_jump_if_bit (EQ_EXPR, get_rvalue (varset, type, is_lvalue), constel, label);
  else {
    tree p1 = get_set_address (varset, is_lvalue);
    tree fieldlist = TYPE_FIELDS (type);
    tree field;

    for (field = fieldlist; (field != NULL) && (fieldno>0); field = TREE_CHAIN (field))
      fieldno--;

    do_jump_if_bit (EQ_EXPR, get_set_field_rhs (p1, field), constel, label);
  }
}

/*
 *  BuildIfVarInVar - generates: if varel in varset then goto label
 */

void
gccgm2_BuildIfVarInVar (tree type, tree varset, tree varel,
			int is_lvalue,
			tree  low,
			tree  high ATTRIBUTE_UNUSED,
			char *label)
{
  tree size = gccgm2_GetSizeOf (type);
  /* calculate the index from the first bit, ie bit 0 represents low value */
  tree index = gccgm2_BuildSub (gccgm2_BuildConvert (gccgm2_GetIntegerType(), varel, FALSE),
                                gccgm2_BuildConvert (gccgm2_GetIntegerType(), low, FALSE), FALSE);

  if (gccgm2_CompareTrees (size, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    do_jump_if_bit (NE_EXPR, get_rvalue (varset, type, is_lvalue), index, label);
  else {
    tree p1               = get_set_address (varset, is_lvalue);
    /* which word do we need to fetch? */
    tree word_index       = gccgm2_BuildDivTrunc (index, gccgm2_BuildIntegerConstant (SET_WORD_SIZE), FALSE);
    /* calculate the bit in this word */
    tree offset_into_word = gccgm2_BuildModTrunc (index, gccgm2_BuildIntegerConstant (SET_WORD_SIZE), FALSE);

    /* calculate the address of the word we are interested in */
    p1 = gccgm2_BuildAdd (convertToPtr (p1),
                          gccgm2_BuildMult (word_index, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT),
                                            FALSE),
                          FALSE);

    /* fetch the word, extract the bit and test for != 0 */
    do_jump_if_bit (NE_EXPR, gccgm2_BuildIndirect (p1, bitset_type_node), offset_into_word, label);
  }
}


/*
 *  BuildIfNotVarInVar - generates: if not (varel in varset) then goto label
 */

void
gccgm2_BuildIfNotVarInVar (tree type, tree varset, tree varel,
			   int is_lvalue,
			   tree  low,
			   tree  high ATTRIBUTE_UNUSED,
			   char *label)
{
  tree size = gccgm2_GetSizeOf (type);
  /* calculate the index from the first bit, ie bit 0 represents low value */
  tree index = gccgm2_BuildSub (gccgm2_BuildConvert (gccgm2_GetIntegerType(), varel, FALSE),
                                gccgm2_BuildConvert (gccgm2_GetIntegerType(), low, FALSE), FALSE);

  if (gccgm2_CompareTrees (size, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    do_jump_if_bit (EQ_EXPR, get_rvalue (varset, type, is_lvalue), index, label);
  else {
    tree p1               = get_set_address (varset, is_lvalue);
    /* calculate the index from the first bit */

    /* which word do we need to fetch? */
    tree word_index       = gccgm2_BuildDivTrunc (index, gccgm2_BuildIntegerConstant (SET_WORD_SIZE), FALSE);
    /* calculate the bit in this word */
    tree offset_into_word = gccgm2_BuildModTrunc (index, gccgm2_BuildIntegerConstant (SET_WORD_SIZE), FALSE);

    /* calculate the address of the word we are interested in */
    p1 = gccgm2_BuildAdd (convertToPtr (p1),
                          gccgm2_BuildMult (word_index,
                                            gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT),
                                            FALSE),
                          FALSE);

    /* fetch the word, extract the bit and test for == 0 */
    do_jump_if_bit (EQ_EXPR, gccgm2_BuildIndirect (p1, bitset_type_node), offset_into_word, label);
  }
}

/*
 *  get_set_address_if_var - returns the address of, op, providing
 *                           it is not a constant.
 *                           NULL is returned if, op, is a constant.
 */

static
tree
get_set_address_if_var (tree op, int is_lvalue, int is_const)
{
  if (is_const)
    return NULL;
  else
    return get_set_address (op, is_lvalue);
}

/*
 *  get_field_no - returns the field no for, op.  The, op, is either
 *                 a constructor or a variable of type record.
 *                 If, op, is a constructor (a set constant in GNU Modula-2)
 *                 then this function is essentially a no-op and it returns op.
 *                 Else we iterate over the field list and return the
 *                 appropriate field number.
 */

static
tree
get_field_no (tree type, tree op, int is_const, unsigned int fieldNo)
{
  ASSERT_BOOL (is_const);
  if (is_const)
    return op;
  else {
    tree list = TYPE_FIELDS (type);
    while (fieldNo > 0 && list != NULL_TREE) {
      list = TREE_CHAIN (list);
      fieldNo--;
    }
    return list;
  }
}

/*
 *  get_set_value - returns the value indicated by, field, in the set.
 *                  Either p->field or the constant(op.fieldNo) is returned.
 */

static
tree
get_set_value (tree p, tree field, int is_const, tree op, unsigned int fieldNo)
{
  ASSERT_BOOL (is_const);
  if (is_const) {
    gcc_assert( !VEC_empty (constructor_elt, CONSTRUCTOR_ELTS (op)));
    unsigned int size = VEC_length (constructor_elt, CONSTRUCTOR_ELTS (op));
    if (size < fieldNo)
      internal_error ("field number exceeds definition of set");
    return VEC_index (constructor_elt, CONSTRUCTOR_ELTS (op), fieldNo)->value;
  }
  else
    return gccgm2_BuildIndirect (get_set_field_lhs (p, field), gccgm2_GetWordType ());
}

/*
 *  BuildForeachWordInSetDoIfExpr - foreach word in set, type, compute the expression, expr, and if true
 *                                  goto label.
 */

void
gccgm2_BuildForeachWordInSetDoIfExpr (tree type, tree op1, tree op2,
				      int is_op1lvalue, int is_op2lvalue,
				      int  is_op1const, int is_op2const,
				      tree (*expr) (tree, tree),
				      char *label)
{
  tree p1 = get_set_address_if_var (op1, is_op1lvalue, is_op1const);
  tree p2 = get_set_address_if_var (op2, is_op2lvalue, is_op2const);
  unsigned int fieldNo = 0;
  tree field1 = get_field_no (type, op1, is_op1const, fieldNo);
  tree field2 = get_field_no (type, op2, is_op2const, fieldNo);

  while (field1 != NULL && field2 != NULL) {
    gccgm2_DoJump ((*expr) (get_set_value (p1, field1, is_op1const, op1, fieldNo),
                            get_set_value (p2, field2, is_op2const, op2, fieldNo)),
                   NULL, label);
    fieldNo++;
    field1 = get_field_no (type, op1, is_op1const, fieldNo);
    field2 = get_field_no (type, op2, is_op2const, fieldNo);
  }
}

/*
 *  BuildIfInRangeGoto - if var is in the range low..high then goto label
 */

void
gccgm2_BuildIfInRangeGoto (tree var, tree low, tree high, char *label)
{
  if (gccgm2_CompareTrees (low, high) == 0)
    gccgm2_DoJump (gccgm2_BuildEqualTo (var, low),
                   NULL, label);
  else
    gccgm2_DoJump (build_binary_op (TRUTH_ANDIF_EXPR,
                                    gccgm2_BuildGreaterThanOrEqual (var, low),
                                    gccgm2_BuildLessThanOrEqual (var, high), FALSE),
                   NULL, label);
}

/*
 *  BuildIfNotInRangeGoto - if var is not in the range low..high then goto label
 */

void
gccgm2_BuildIfNotInRangeGoto (tree var, tree low, tree high, char *label)
{
  if (gccgm2_CompareTrees (low, high) == 0)
    gccgm2_DoJump (gccgm2_BuildNotEqualTo (var, low),
                   NULL, label);
  else
    gccgm2_DoJump (build_binary_op (TRUTH_ORIF_EXPR,
                                    gccgm2_BuildLessThan (var, low),
                                    gccgm2_BuildGreaterThan (var, high), FALSE),
                   NULL, label);
}

/*
 *  BuildIndirect - build: (*target) given that the object to be copied is of, type.
 */

tree
gccgm2_BuildIndirect (tree target, tree type)
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

  tree t1 = skip_type_decl (type);
  tree t2 = build_pointer_type (t1);

  return build1 (INDIRECT_REF, t1, gccgm2_BuildConvert (t2, target, FALSE));
}

/*
 *  DoJump - jump to the appropriate label depending whether
 *           result of the expression is TRUE or FALSE.
 */

void
gccgm2_DoJump (tree exp, char *falselabel, char *truelabel)
{
  tree c = NULL_TREE;

  if (TREE_CODE (TREE_TYPE (exp)) != BOOLEAN_TYPE)
    exp = convert (boolean_type_node, exp);

  if ((falselabel != NULL) && (truelabel == NULL)) {
    tree stmt = cur_stmt_list;
    cur_stmt_list = alloc_stmt_list ();
    
    gccgm2_BuildGoto (falselabel);
    c = build3 (COND_EXPR, void_type_node, exp,
                cur_stmt_list, alloc_stmt_list ());
    cur_stmt_list = stmt;
  }
  else if ((falselabel == NULL) && (truelabel != NULL)) {
    tree stmt = cur_stmt_list;
    cur_stmt_list = alloc_stmt_list ();
    
    gccgm2_BuildGoto (truelabel);
    c = build3 (COND_EXPR, void_type_node, exp,
                cur_stmt_list, alloc_stmt_list ());
    cur_stmt_list = stmt;
  }
  else
    error ("expecting one and only one label to be declared");
  if (c != NULL_TREE)
    add_stmt (c);
}

/*
 *  BuildParam - build a list of parameters, ready for a subsequent procedure call.
 */

void
gccgm2_BuildParam (tree param)
{
#if 0
  fprintf(stderr, "tree for parameter containing "); fflush(stderr);
  fprintf(stderr, "list of elements\n"); fflush(stderr);
#endif
  if (TREE_CODE(param) == FUNCTION_DECL)
    param = build_unary_op (ADDR_EXPR, param, 0);

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
gccgm2_BuildProcedureCall (tree procedure, tree rettype)
{
  tree functype = TREE_TYPE (procedure);
  tree funcptr  = build1 (ADDR_EXPR, build_pointer_type (functype), procedure);
  tree call;

  TREE_USED (procedure) = TRUE;

  if (rettype == NULL_TREE) {
    rettype = void_type_node;
    call = build (CALL_EXPR, rettype, funcptr, param_list, NULL_TREE);
    TREE_USED (call)         = TRUE;
    TREE_SIDE_EFFECTS (call) = TRUE ;

#if defined(DEBUG_PROCEDURE_CALLS)
    fprintf(stderr, "built the modula-2 call, here is the tree\n"); fflush(stderr);
    debug_tree (call);
#endif

    add_stmt (call);
    last_function = NULL_TREE;
  } else
    last_function = build (CALL_EXPR, skip_type_decl (rettype), funcptr, param_list, NULL_TREE);
  
  param_list = NULL_TREE;   /* ready for the next time we call a procedure */
  return last_function;
}

/*
 *  BuildIndirectProcedureCall - creates a procedure call from a procedure and
 *                               parameter list and the return type, rettype.
 */

tree
gccgm2_BuildIndirectProcedureCall (tree procedure, tree rettype)
{
  tree call;

  TREE_USED (procedure) = TRUE;

  if (rettype == NULL_TREE) {
    rettype = void_type_node;
    call = build (CALL_EXPR, rettype, procedure, param_list, NULL_TREE);
    TREE_USED (call)         = TRUE;
    TREE_SIDE_EFFECTS (call) = TRUE ;

#if 0
    fprintf(stderr, "built the modula-2 call, here are the params\n"); fflush(stderr);
    debug_tree (param_list);
#endif
#if defined(DEBUG_PROCEDURE_CALLS)
    fprintf (stderr, "built the modula-2 call, here is the tree\n"); fflush(stderr);
    debug_tree (call);
#endif

    add_stmt (call);
    last_function = NULL_TREE;
  } else
    last_function = build (CALL_EXPR, skip_type_decl (rettype),
                           procedure, param_list, NULL_TREE);

  param_list = NULL_TREE;   /* ready for the next time we call a procedure */
  return last_function;
}

/*
 *  BuildFunctValue - generates code for value := last_function(foobar);
 */

void
gccgm2_BuildFunctValue (tree value)
{
  tree assign = build_modify_expr (value, NOP_EXPR, last_function);

  TREE_SIDE_EFFECTS (assign) = TRUE;
  TREE_USED (assign) = TRUE;
  add_stmt (assign);
}

/*
 *  SetLastFunction - assigns last_function to, t.
 */

void
gccgm2_SetLastFunction (tree t)
{
  last_function = t;
}

/*
 *  SetParamList - assigs param_list to, t.
 */

void
gccgm2_SetParamList (tree t)
{
  param_list = t;
}


/*
 *  GetLastFunction - returns, last_function.
 */

tree
gccgm2_GetLastFunction (void)
{
  return last_function;
}

/*
 *  GetParamList - returns, param_list.
 */

tree
gccgm2_GetParamList (void)
{
  return param_list;
}

/*
 *  BuildAsm - generates an inline assembler instruction.
 */

void
gccgm2_BuildAsm (tree instr, int IsVolatile,
		 tree inputs, tree outputs, tree trash)
{
  tree args = build_stmt (ASM_EXPR, instr, outputs, inputs, trash);

  /* asm statements without outputs, including simple ones, are treated
     as volatile.  */
  ASM_INPUT_P (args) = (outputs == NULL_TREE);
  ASM_VOLATILE_P (args) = IsVolatile;

  add_stmt (args);
}

tree
gccgm2_GetBooleanType (void)
{
#if defined(USE_BOOLEAN)
  return boolean_type_node;
#else
  return integer_type_node;
#endif
}

tree
gccgm2_GetBooleanFalse (void)
{
#if defined(USE_BOOLEAN)
  return boolean_false_node;
#else
  return gccgm2_GetIntegerZero ();
#endif
}

tree
gccgm2_GetBooleanTrue (void)
{
#if defined(USE_BOOLEAN)
  return boolean_true_node;
#else
  return gccgm2_GetIntegerOne ();
#endif
}

tree
gccgm2_GetIntegerType (void)
{
  return integer_type_node;
}

tree
gccgm2_GetCharType (void)
{
  return char_type_node;
}

tree
gccgm2_GetByteType (void)
{
  return unsigned_char_type_node;
}

tree
gccgm2_GetVoidType (void)
{
  return void_type_node;
}

tree
gccgm2_GetPointerType (void)
{
  return ptr_type_node;
}

tree
gccgm2_GetCardinalType (void)
{
  return unsigned_type_node;
}

tree
gccgm2_GetBitsetType (void)
{
  return bitset_type_node;
}

tree
gccgm2_GetBitnumType (void)
{
  return bitnum_type_node;
}

tree
gccgm2_GetRealType (void)
{
  return double_type_node;
}

tree
gccgm2_GetLongRealType (void)
{
  return long_double_type_node;
}

tree
gccgm2_GetShortRealType (void)
{
  return float_type_node;
}

tree
gccgm2_GetLongIntType (void)
{
  return long_integer_type_node;
}

tree
gccgm2_GetWordType (void)
{
  return unsigned_type_node;
}

tree
gccgm2_GetISOLocType (void)
{
  return m2_iso_loc_type_node;
}

tree
gccgm2_GetISOByteType (void)
{
  return m2_iso_byte_type_node;
}

tree
gccgm2_GetISOWordType (void)
{
  return m2_iso_word_type_node;
}

tree
gccgm2_GetProcType (void)
{
  return proc_type_node;
}

tree
gccgm2_GetM2CharType (void)
{
  return m2_char_type_node;
}

tree
gccgm2_GetM2IntegerType (void)
{
  return m2_integer_type_node;
}

tree
gccgm2_GetM2CardinalType (void)
{
  return m2_cardinal_type_node;
}

tree
gccgm2_GetM2ShortRealType (void)
{
  return m2_short_real_type_node;
}

tree
gccgm2_GetM2RealType (void)
{
  return m2_real_type_node;
}

tree
gccgm2_GetM2LongRealType (void)
{
  return m2_long_real_type_node;
}

tree
gccgm2_GetM2LongIntType (void)
{
  return m2_long_int_type_node;
}

tree
gccgm2_GetM2LongCardType (void)
{
  return m2_long_card_type_node;
}

tree
gccgm2_GetM2ShortIntType (void)
{
  return m2_short_int_type_node;
}

tree
gccgm2_GetShortIntType (void)
{
  return short_integer_type_node;
}

tree
gccgm2_GetM2ShortCardType (void)
{
  return m2_short_card_type_node;
}

tree
gccgm2_GetShortCardType (void)
{
  return short_unsigned_type_node;
}

tree
gccgm2_GetM2ZType (void)
{
  return m2_z_type_node;
}

tree
gccgm2_GetM2RType (void)
{
  return long_double_type_node;
}

tree
gccgm2_GetIntegerZero (void)
{
  return integer_zero_node;
}

tree
gccgm2_GetIntegerOne (void)
{
  return integer_one_node;
}

tree
gccgm2_GetWordZero (void)
{
  return gccgm2_ToWord(integer_zero_node);
}

tree
gccgm2_GetWordOne (void)
{
  return gccgm2_ToWord(integer_one_node);
}

tree
gccgm2_GetPointerZero (void)
{
  return convertToPtr (integer_zero_node);
}

tree
gccgm2_GetPointerOne (void)
{
  return convertToPtr (integer_one_node);
}

tree
gccgm2_GetCurrentFunction (void)
{
  return current_function_decl;
}

tree
gccgm2_GetErrorNode (void)
{
  return error_mark_node;
}

/*
 *  convertToPtr - if the type of tree, t, is not a ptr_type_node then convert it.
 */

tree
convertToPtr (tree t)
{
  if (TREE_CODE (TREE_TYPE (t)) == POINTER_TYPE)
    return t;
  else
    return gccgm2_BuildConvert (ptr_type_node, t, FALSE);
}

/*
 *  AreConstantsEqual - maps onto tree.c (tree_int_cst_equal). It returns
 *                      TRUE if the value of e1 is the same as e2.
 */

int
gccgm2_AreConstantsEqual (tree e1, tree e2)
{
  return tree_int_cst_equal (e1, e2) != 0;
}

/*
 *  DetermineSign - returns -1 if e<0
 *                           0 if e==0
 *                           1 if e>0
 *
 *                  an unsigned constant will never return -1
 */

int
gccgm2_DetermineSign (tree e)
{
  return tree_int_cst_sgn(e);
}

/*
 *  CompareTrees - returns -1 if e1 < e2, 0 if e1 == e2, and 1 if e1 > e2.
 */

int gccgm2_CompareTrees (tree e1, tree e2)
{
  return tree_int_cst_compare (gccgm2_FoldAndStrip (e1), gccgm2_FoldAndStrip (e2));
}

/*
 *  get_rvalue - returns the rvalue of t. The, type, is the object type to be
 *               copied upon indirection.
 */

static
tree
get_rvalue (tree t, tree type, int is_lvalue)
{
  if (is_lvalue)
    return gccgm2_BuildIndirect(t, type);
  else
    return t;
}

/*
 *  get_set_address - returns the address of op1.
 */

static
tree
get_set_address (tree op1, int is_lvalue)
{
  if (is_lvalue)
    return op1;
  else
    return build_unary_op (ADDR_EXPR, op1, FALSE);
}

/*
 *  get_set_field_lhs - returns the address of p->field.
 */

static
tree
get_set_field_lhs (tree p, tree field)
{
  return gccgm2_BuildAdd (convertToPtr (p), gccgm2_BuildOffset1 (field, FALSE), FALSE);
}

/*
 *  get_set_field_rhs - returns the value of p->field.
 */

static
tree
get_set_field_rhs (tree p, tree field)
{
  return gccgm2_BuildIndirect (get_set_field_lhs (p, field), gccgm2_GetWordType ());
}

/*
 *  BuildBinaryForeachWordDo - provides the large set operators. Each word
 *                             (or less) of the set can be calculated by binop.
 *                             This procedure runs along each word of the
 *                             large set invoking the binop.
 */

void
gccgm2_BuildBinaryForeachWordDo (tree type, tree op1, tree op2, tree op3,
                                 tree  (*binop)(tree, tree, int),
                                 int is_op1lvalue, int is_op2lvalue, int is_op3lvalue,
                                 int is_op1const, int is_op2const, int is_op3const)
{
  tree size = gccgm2_GetSizeOf (type);


  ASSERT_BOOL (is_op1lvalue);
  ASSERT_BOOL (is_op2lvalue);
  ASSERT_BOOL (is_op3lvalue);
  ASSERT_BOOL (is_op1const);
  ASSERT_BOOL (is_op2const);
  ASSERT_BOOL (is_op3const);
  if (gccgm2_CompareTrees (size, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    gccgm2_BuildAssignment (get_rvalue (op1, type, is_op1lvalue),
                            (*binop) (get_rvalue (op2, type, is_op2lvalue),
                                      get_rvalue (op3, type, is_op3lvalue), FALSE));
  else {
    /* large set size > TSIZE(WORD) */

    tree p1     = get_set_address (op1, is_op1lvalue);
    tree p2     = get_set_address_if_var (op2, is_op2lvalue, is_op2const);
    tree p3     = get_set_address_if_var (op3, is_op3lvalue, is_op3const);
    unsigned int fieldNo = 0;
    tree field1 = get_field_no (type, op1, is_op1const, fieldNo);
    tree field2 = get_field_no (type, op2, is_op2const, fieldNo);
    tree field3 = get_field_no (type, op3, is_op3const, fieldNo);

    if (is_op1const)
      error("internal error: not expecting operand1 to be a constant set");

    while (field1 != NULL && field2 != NULL && field3 != NULL) {
      gccgm2_BuildAssignment (get_set_field_rhs (p1, field1),
                              (*binop) (get_set_value (p2, field2,
                                                       is_op2const, op2, fieldNo),
                                        get_set_value (p3, field3,
                                                       is_op3const, op3, fieldNo), FALSE));
      fieldNo++;
      field1 = get_field_no (type, op1, is_op1const, fieldNo);
      field2 = get_field_no (type, op2, is_op2const, fieldNo);
      field3 = get_field_no (type, op3, is_op3const, fieldNo);
    }
  }
}

/*
 *  BuildUnaryForeachWordDo - provides the large set operators.
 *                            Each word (or less) of the set can be
 *                            calculated by unop.
 *                            This procedure runs along each word
 *                            of the large set invoking the unop.
 */

void
gccgm2_BuildUnaryForeachWordDo (tree type, tree op1, tree op2,
                                tree (*unop)(tree, int),
                                int is_op1lvalue, int is_op2lvalue,
                                int is_op1const, int is_op2const)
{
  tree size = gccgm2_GetSizeOf (type);

  ASSERT_BOOL (is_op1lvalue);
  ASSERT_BOOL (is_op2lvalue);
  ASSERT_BOOL (is_op1const);
  ASSERT_BOOL (is_op2const);
  if (gccgm2_CompareTrees (size, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    gccgm2_BuildAssignment (get_rvalue (op1, type, is_op1lvalue),
                            (*unop) (get_rvalue (op2, type, is_op2lvalue), FALSE));
  else {
    /* large set size > TSIZE(WORD) */

    tree p1     = get_set_address (op1, is_op1lvalue);
    tree p2     = get_set_address_if_var (op2, is_op2lvalue, is_op2const);
    unsigned int fieldNo = 0;
    tree field1 = get_field_no (type, op1, is_op1const, fieldNo);
    tree field2 = get_field_no (type, op2, is_op2const, fieldNo);

    if (is_op1const)
      error("internal error: not expecting operand1 to be a constant set");

    while (field1 != NULL && field2 != NULL) {
      gccgm2_BuildAssignment (get_set_field_rhs (p1, field1),
                              (*unop) (get_set_value (p2, field2, is_op2const, op2, fieldNo),
                                       FALSE));
      fieldNo++;
      field1 = get_field_no (type, op1, is_op1const, fieldNo);
      field2 = get_field_no (type, op2, is_op2const, fieldNo);
    }
  }
}

/*
 *  BuildExcludeVarConst - builds the EXCL(op1, 1<<op2) operation for a small sets. Large
 *                         sets call this routine to exclude the bit in the particular word.
 *                         op2 is a constant.
 */

void
gccgm2_BuildExcludeVarConst (tree type, tree op1, tree op2,
                             int is_lvalue, int fieldno)
{
  tree size = gccgm2_GetSizeOf (type);

  ASSERT_BOOL (is_lvalue);
  if (gccgm2_CompareTrees (size, gccgm2_BuildIntegerConstant(SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    gccgm2_BuildAssignment (get_rvalue (op1, type, is_lvalue),
                            gccgm2_BuildLogicalAnd (get_rvalue (op1, type, is_lvalue),
                                                    gccgm2_BuildSetNegate (gccgm2_BuildLSL (gccgm2_GetWordOne(), op2, FALSE),
                                                                          FALSE),
                                                    FALSE));
  else {
    tree p1 = get_set_address (op1, is_lvalue);
    tree fieldlist = TYPE_FIELDS (type);
    tree field;

    for (field = fieldlist; (field != NULL) && (fieldno>0); field = TREE_CHAIN (field))
      fieldno--;
    gccgm2_BuildAssignment (get_set_field_rhs (p1, field),
                            gccgm2_BuildLogicalAnd (get_set_field_rhs (p1, field),
                                                    gccgm2_BuildSetNegate(gccgm2_BuildLSL (gccgm2_GetWordOne(), op2, FALSE),
                                                                          FALSE),
                                                    FALSE));
  }
}

/*
 *  BuildExcludeVarVar - builds the EXCL(varset, 1<<varel) operation for a small and large sets.
 *                       varel is a variable.
 */

void
gccgm2_BuildExcludeVarVar (tree type, tree varset, tree varel,
                           int is_lvalue, tree low)
{
  tree size = gccgm2_GetSizeOf (type);

  ASSERT_BOOL (is_lvalue);
  /* calculate the index from the first bit, ie bit 0 represents low value */
  tree index = gccgm2_BuildSub (gccgm2_BuildConvert (gccgm2_GetIntegerType(), varel, FALSE),
                                gccgm2_BuildConvert (gccgm2_GetIntegerType(), low, FALSE), FALSE);

  if (gccgm2_CompareTrees (size, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    gccgm2_BuildAssignment (get_rvalue (varset, type, is_lvalue),
                            gccgm2_BuildLogicalAnd (get_rvalue (varset, type, is_lvalue),
                                                    gccgm2_BuildSetNegate (gccgm2_BuildLSL (gccgm2_GetWordOne(), index, FALSE),
                                                                           FALSE),
                                                    FALSE));
  else {
    tree p1               = get_set_address (varset, is_lvalue);
    /* calculate the index from the first bit */

    /* which word do we need to fetch? */
    tree word_index       = gccgm2_BuildDivTrunc (index, gccgm2_BuildIntegerConstant (SET_WORD_SIZE), FALSE);
    /* calculate the bit in this word */
    tree offset_into_word = gccgm2_BuildModTrunc (index, gccgm2_BuildIntegerConstant (SET_WORD_SIZE), FALSE);

    /* calculate the address of the word we are interested in */
    p1 = gccgm2_BuildAdd (convertToPtr (p1),
                          gccgm2_BuildMult (word_index, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT),
                                            FALSE),
                          FALSE);

    /* set bit offset_into_word within the word pointer at by p1 */
    gccgm2_BuildAssignment (gccgm2_BuildIndirect (p1, bitset_type_node),
                            gccgm2_BuildLogicalAnd (gccgm2_BuildIndirect (p1, bitset_type_node),
                                                    gccgm2_BuildSetNegate (gccgm2_BuildLSL (gccgm2_GetWordOne(),
                                                                                            offset_into_word, FALSE),
                                                                           FALSE),
                                                    FALSE));
  }
}

/*
 *  BuildIncludeVarConst - builds the INCL(op1, 1<<op2) operation for a small sets. Large
 *                         sets call this routine to include the bit in the particular word.
 *                         op2 is a constant.
 */

void
gccgm2_BuildIncludeVarConst (tree type, tree op1, tree op2,
                             int is_lvalue, int fieldno)
{
  tree size = gccgm2_GetSizeOf (type);

  ASSERT_BOOL (is_lvalue);
  if (gccgm2_CompareTrees (size, gccgm2_BuildIntegerConstant(SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    gccgm2_BuildAssignment (get_rvalue (op1, type, is_lvalue),
                            gccgm2_BuildLogicalOr (get_rvalue (op1, type, is_lvalue),
                                                   gccgm2_BuildLSL (gccgm2_GetWordOne(), op2, FALSE),
                                                   FALSE));
  else {
    tree p1 = get_set_address (op1, is_lvalue);
    tree fieldlist = TYPE_FIELDS (type);
    tree field;

    for (field = fieldlist; (field != NULL) && (fieldno>0); field = TREE_CHAIN (field))
      fieldno--;
    gccgm2_BuildAssignment (get_set_field_rhs (p1, field),
                            gccgm2_BuildLogicalOr (get_set_field_rhs (p1, field),
                                                   gccgm2_BuildLSL (gccgm2_GetWordOne(), op2, FALSE),
                                                   FALSE));
  }
}

/*
 *  BuildIncludeVarVar - builds the INCL(varset, 1<<varel) operation for a small and large sets.
 *                       op2 is a variable.
 */

void
gccgm2_BuildIncludeVarVar (tree type, tree varset, tree varel,
                           int is_lvalue, tree low)
{
  tree size = gccgm2_GetSizeOf (type);

  ASSERT_BOOL (is_lvalue);
  /* calculate the index from the first bit, ie bit 0 represents low value */
  tree index = gccgm2_BuildSub (gccgm2_BuildConvert (gccgm2_GetIntegerType(), varel, FALSE),
                                gccgm2_BuildConvert (gccgm2_GetIntegerType(), low, FALSE), FALSE);

  if (gccgm2_CompareTrees (size, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    gccgm2_BuildAssignment (get_rvalue (varset, type, is_lvalue),
                            gccgm2_BuildLogicalOr (get_rvalue (varset, type, is_lvalue),
                                                   gccgm2_BuildLSL (gccgm2_GetWordOne(), index, FALSE),
                                                   FALSE));
  else {
    tree p1               = get_set_address (varset, is_lvalue);
    /* which word do we need to fetch? */
    tree word_index       = gccgm2_BuildDivTrunc (index, gccgm2_BuildIntegerConstant (SET_WORD_SIZE), FALSE);
    /* calculate the bit in this word */
    tree offset_into_word = gccgm2_BuildModTrunc (index, gccgm2_BuildIntegerConstant (SET_WORD_SIZE), FALSE);

    /* calculate the address of the word we are interested in */
    p1 = gccgm2_BuildAdd (convertToPtr (p1),
                          gccgm2_BuildMult (word_index, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT),
                                            FALSE),
                          FALSE);

    /* set bit offset_into_word within the word pointer at by p1 */
    gccgm2_BuildAssignment (gccgm2_BuildIndirect (p1, bitset_type_node),
                            gccgm2_BuildLogicalOr (gccgm2_BuildIndirect (p1, bitset_type_node),
                                                   gccgm2_BuildLSL (gccgm2_GetWordOne(), offset_into_word, FALSE),
                                                   FALSE));
  }
}

tree
gccgm2_BuildIntegerConstant (int value)
{
  switch (value) {

  case 0:  return integer_zero_node;
  case 1:  return integer_one_node;

  default:
    return gccgm2_RememberConstant (build_int_cst (NULL_TREE, value));
  }
}

/*
 *  DetermineSizeOfConstant - given, str, and, base, fill in
 *                            needsLong and needsUnsigned appropriately.
 */

void
gccgm2_DetermineSizeOfConstant (const char *str, unsigned int base,
                                int *needsLong, int *needsUnsigned)
{
  int low;
  int high;
  int overflow;

  overflow = interpret_m2_integer (str, base, (unsigned int *)&low, &high);
  *needsLong = (high != 0);
  *needsUnsigned = ((low < 0) || (high < 0));
}

/*
 *  BuildConstLiteralNumber - returns a GCC TREE built from the string, str.
 *                            It assumes that, str, represents a legal
 *                            number in Modula-2. It always returns a
 *                            positive value.
 */

tree
gccgm2_BuildConstLiteralNumber (const char *str, unsigned int base)
{
  tree value, type;
  unsigned HOST_WIDE_INT low;
  HOST_WIDE_INT high;
  int needLong, needUnsigned;
  int overflow;

  overflow = interpret_integer (str, base,
                                &low, (HOST_WIDE_INT *) &high);
  gccgm2_DetermineSizeOfConstant (str, base, &needLong, &needUnsigned);
  
  if (needUnsigned && needLong)
    type = gccgm2_GetM2LongCardType ();
  else
    type = gccgm2_GetM2LongIntType ();

  value = build_int_cst_wide (type, low, high);

  if (gccgm2_TreeOverflow (value))
    error("constant too large");

  return gccgm2_RememberConstant (value);
}

/*
 * interpret_integer - converts an integer constant into two integer
 *                     constants. Heavily borrowed from gcc/cppexp.c.
 */

static int
interpret_integer (const char *str, unsigned int base,
		   unsigned HOST_WIDE_INT *low, HOST_WIDE_INT *high)
{
  const unsigned char *p, *end;
  int overflow = FALSE;
  int len;

  *low = 0;
  *high = 0;
  p = (unsigned char *)str;
  len = strlen(str);
  end = p + len;

  /* Common case of a single digit.  */
  if (len == 1)
    *low = p[0] - '0';
  else
    {
      unsigned int c = 0;

      /* We can add a digit to numbers strictly less than this without
         needing the precision and slowness of double integers.  */

      unsigned HOST_WIDE_INT max = ~(unsigned HOST_WIDE_INT) 0;
      max = (max - base + 1) / base + 1;

      for (; p < end; p++)
        {
          c = *p;

          if (ISDIGIT (c) || (base == 16 && ISXDIGIT (c)))
            c = hex_value (c);
          else
            return overflow;

          /* Strict inequality for when max is set to zero.  */
          if (*low < max)
            *low = (*low) * base + c;
          else
            {
              overflow = append_digit (low, high, c, base);
              max = 0;  /* from now on we always use append_digit */
            }
        }
    }
  return overflow;
}

/* Append DIGIT to NUM, a number of PRECISION bits being read in base
   BASE.  */
static int
append_digit (unsigned HOST_WIDE_INT *low, HOST_WIDE_INT *high,
	      unsigned int digit, unsigned int base)
{
  unsigned int shift;
  int overflow;
  HOST_WIDE_INT add_high, res_high;
  unsigned HOST_WIDE_INT add_low, res_low;

  switch (base) {

  case 2:  shift = 1; break;
  case 8:  shift = 3; break;
  case 10: shift = 3; break;
  case 16: shift = 4; break;

  default:
    shift = 3;
    error("internal error: not expecting this base value for a constant");
  }

  /* Multiply by 2, 8 or 16.  Catching this overflow here means we don't
     need to worry about add_high overflowing.  */
  if (((*high) >> (INT_TYPE_SIZE - shift)) == 0)
    overflow = FALSE;
  else
    overflow = TRUE;

  res_high = *high << shift;
  res_low = *low << shift;
  res_high |= (*low) >> (INT_TYPE_SIZE - shift);

  if (base == 10)
    {
      add_low = (*low) << 1;
      add_high = ((*high) << 1) + ((*low) >> (INT_TYPE_SIZE - 1));
    }
  else
    add_high = add_low = 0;

  if (add_low + digit < add_low)
    add_high++;
  add_low += digit;
    
  if (res_low + add_low < res_low)
    add_high++;
  if (res_high + add_high < res_high)
    overflow = TRUE;

  *low = res_low + add_low;
  *high = res_high + add_high;

  return overflow;
}

/*
 * interpret_m2_integer - converts an integer constant into two integer
 *                        constants. Heavily borrowed from gcc/cppexp.c.
 *                        Note that this is a copy of the above code
 *                        except that it uses `int' rather than
 *                        HOST_WIDE_INT to allow gm2 to determine
 *                        what Modula-2 base type to use for this
 *                        constant.
 */

static int
interpret_m2_integer (const char *str, unsigned int base,
		      unsigned int *low, int *high)
{
  const unsigned char *p, *end;
  int overflow = FALSE;
  int len;

  *low = 0;
  *high = 0;
  p = (unsigned char *)str;
  len = strlen(str);
  end = p + len;

  /* Common case of a single digit.  */
  if (len == 1)
    *low = p[0] - '0';
  else
    {
      unsigned int c = 0;

      /* We can add a digit to numbers strictly less than this without
         needing the precision and slowness of double integers.  */

      unsigned int max = ~(unsigned int) 0;
      max = (max - base + 1) / base + 1;

      for (; p < end; p++)
        {
          c = *p;

          if (ISDIGIT (c) || (base == 16 && ISXDIGIT (c)))
            c = hex_value (c);
          else
            return overflow;

          /* Strict inequality for when max is set to zero.  */
          if (*low < max)
            *low = (*low) * base + c;
          else
            {
              overflow = append_m2_digit (low, high, c, base);
              max = 0;  /* from now on we always use append_digit */
            }
        }
    }
  return overflow;
}

/* Append DIGIT to NUM, a number of PRECISION bits being read in base
   BASE.  */
static int
append_m2_digit (unsigned int *low, int *high,
		 unsigned int digit, unsigned int base)
{
  unsigned int shift;
  int overflow;
  int add_high, res_high;
  unsigned int add_low, res_low;

  switch (base) {

  case 2:  shift = 1; break;
  case 8:  shift = 3; break;
  case 10: shift = 3; break;
  case 16: shift = 4; break;

  default:
    shift = 3;
    error("internal error: not expecting this base value for a constant");
  }

  /* Multiply by 2, 8 or 16.  Catching this overflow here means we don't
     need to worry about add_high overflowing.  */
  if (((*high) >> (INT_TYPE_SIZE - shift)) == 0)
    overflow = FALSE;
  else
    overflow = TRUE;

  res_high = *high << shift;
  res_low = *low << shift;
  res_high |= (*low) >> (INT_TYPE_SIZE - shift);

  if (base == 10)
    {
      add_low = (*low) << 1;
      add_high = ((*high) << 1) + ((*low) >> (INT_TYPE_SIZE - 1));
    }
  else
    add_high = add_low = 0;

  if (add_low + digit < add_low)
    add_high++;
  add_low += digit;
    
  if (res_low + add_low < res_low)
    add_high++;
  if (res_high + add_high < res_high)
    overflow = TRUE;

  *low = res_low + add_low;
  *high = res_high + add_high;

  return overflow;
}

static tree
build_set_full_complement (void)
{
  tree value = integer_zero_node;
  int i;

  for (i=0; i<SET_WORD_SIZE; i++) {
    value = gccgm2_BuildLogicalOr(value,
                                  gccgm2_BuildLSL (gccgm2_GetWordOne(),
                                                   gccgm2_BuildIntegerConstant (i),
                                                   FALSE),
                                  FALSE);
  }
  return value;
}

/*
 *  BuildStringConstant - creates a string constant given a, string,
 *                        and, length.
 */

tree
gccgm2_BuildStringConstant (char *string, int length)
{
  tree id, elem, index, type;

  /* +1 ensures that we always nul terminate our strings */
  id = build_string (length+1, string);
#if 0
  TREE_TYPE (id) = char_array_type_node;
#else
  elem = build_type_variant (char_type_node, 1, 0);
  index = build_index_type (build_int_cst (NULL_TREE, length+1));
  type = build_array_type (elem, index);
  TREE_TYPE (id) = type;
#endif
  TREE_CONSTANT (id) = 1;
  TREE_INVARIANT (id) = 1;
  TREE_READONLY (id) = 1;
  TREE_STATIC (id) = 1;

  return gccgm2_RememberConstant (id);
}

/*
 *  BuildCharConstant - creates a character constant given a, string.
 */

tree
gccgm2_BuildCharConstant (char *string)
{
  unsigned num_bits = TYPE_PRECISION (char_type_node) * strlen(string);
  int result        = (int) string[0];
  tree id;

  if (TYPE_UNSIGNED (char_type_node) || ((result >> (num_bits - 1)) & 1) == 0)
    id = build_int_cst (integer_type_node,
                        result & ((unsigned HOST_WIDE_INT) ~0
                                  >> (HOST_BITS_PER_WIDE_INT - num_bits)));
  else
    id = build_int_cst (integer_type_node,
                        result | ~((unsigned HOST_WIDE_INT) ~0
                                   >> (HOST_BITS_PER_WIDE_INT - num_bits)));

  return gccgm2_RememberConstant (id);
}

/*
 *  ConvertConstantAndCheck - in Modula-2 sementics: return( VAL(type, expr) )
 *                            Only to be used for a constant expr,
 *                            overflow checking is performed. 
 */

tree
gccgm2_ConvertConstantAndCheck (tree type, tree expr)
{
  expr = fold (expr);
  STRIP_NOPS (expr);
  return convert_and_check (skip_type_decl (type), gccgm2_FoldAndStrip (expr));
}

/*
 *  ToWord - converts an expression (Integer or Ordinal type) into
 *           a WORD.
 */

tree
gccgm2_ToWord (tree expr)
{
  return gccgm2_BuildConvert (gccgm2_GetWordType(), expr, FALSE);
}

/*
 *  RealToTree - convert a real number into a Tree.
 */

tree
gccgm2_RealToTree (char *name)
{
  return build_real (gccgm2_GetLongRealType(),
                     REAL_VALUE_ATOF (name, TYPE_MODE (gccgm2_GetLongRealType ())));
}

/*
 *  BuildStart - creates a module initialization function. We make
 *               this function public if it is not an inner module.
 *               The linker will create a call list for all linked
 *               modules which determines the initialization
 *               sequence for all modules.
 */

tree
gccgm2_BuildStart (char *name, int line ATTRIBUTE_UNUSED, int inner_module)
{
  tree fntype;
  tree fndecl;

  /* The function type depends on the return type and type of args.  */
  fntype = build_function_type (integer_type_node, NULL_TREE);
  fndecl = build_decl (FUNCTION_DECL, get_identifier (name), fntype);

  DECL_EXTERNAL (fndecl) = 0;
  if (inner_module)
    TREE_PUBLIC (fndecl) = 0;
  else
    TREE_PUBLIC (fndecl) = 1;

  TREE_STATIC (fndecl)   = 1;
  DECL_RESULT (fndecl)   = build_decl (RESULT_DECL, NULL_TREE, integer_type_node);
  DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;

  /* Prevent the optimizer from removing it if it is public. */
  if (TREE_PUBLIC (fndecl))
    gm2_mark_addressable (fndecl);

  gccgm2_BuildStartFunctionCode (fndecl, !inner_module, inner_module);
  return fndecl;
}

void
gccgm2_BuildEnd (tree fndecl, int nested)
{
  gccgm2_BuildEndFunctionCode (fndecl, nested);
  current_function_decl = NULL;
  cfun = NULL;
}

void
gccgm2_BuildCallInnerInit (fndecl)
     tree fndecl;
{
  add_stmt (build_function_call (fndecl, NULL_TREE));
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
gccgm2_BuildEndMainModule (void)
{
  /* nothing to do here */
}

/*
 *  DebugTree - display the tree, t.
 */

void
gccgm2_DebugTree (tree t)
{
  debug_tree(t);
}

void
gccgm2_DebugTreeChain (tree t)
{
  for (; t; t = TREE_CHAIN (t))
    debug_tree(t);
}

void
gccgm2_printStmt (void)
{
  if (cur_stmt_list != NULL)
    debug_tree(cur_stmt_list);
}

/*
 * Similar to build_int_2() but allows you to specify the type of the
 * integer constant that you are creating.
 */

tree
build_int_2_type(int low, int hi, tree type)
{
  tree t = make_node(INTEGER_CST);
  TREE_INT_CST_LOW(t) = low;
  TREE_INT_CST_HIGH(t) = hi;
  TREE_TYPE(t) = type;
  return t;
}

/*
 *  BuildCap - builds the Modula-2 function CAP(t) and returns
 *             the result in a gcc Tree.
 */

tree
gccgm2_BuildCap (tree t)
{
  tree tt;
  tree out_of_range, less_than, greater_than, translated;

  t = fold(t);
  if (t == error_mark_node)
    return error_mark_node;

  tt = TREE_TYPE(t);

  t = fold (convert (char_type_node, t));

  if (TREE_CODE(tt) == CHAR_TYPE ||
      TREE_CODE(tt) == INTEGER_TYPE) {
    less_than = build_binary_op (LT_EXPR, t,
                                 build_int_2_type( 'a', 0,
                                                   char_type_node), 0);
    greater_than = build_binary_op (GT_EXPR, t,
                                    build_int_2_type( 'z', 0,
                                                      char_type_node), 0);
    out_of_range = build_binary_op (TRUTH_ORIF_EXPR,
                                    less_than, greater_than, 0);
    
    translated = fold( build (MINUS_EXPR, char_type_node, t,
                              build_int_2_type ('a'-'A', 0,
                                                char_type_node)));
    
    return fold( build_conditional_expr (out_of_range, t, translated));
  }

  error ("argument to CAP is not a constant or variable of type CHAR");
  return error_mark_node;
}

/*
 *  BuildAbs - builds the Modula-2 function ABS(t) and returns
 *             the result in a gcc Tree.
 */

tree
gccgm2_BuildAbs (tree t)
{
  return build_unary_op (ABS_EXPR, t, 0);
}

/* taken from c-common.c:3614 and pruned */

/* Recognize certain built-in functions so we can make tree-codes
   other than CALL_EXPR.  We do this when it enables fold-const.c
   to do something useful.  */
/* ??? By rights this should go in builtins.c, but only C and C++
   implement build_{binary,unary}_op.  Not exactly sure what bits
   of functionality are actually needed from those functions, or
   where the similar functionality exists in the other front ends.  */

tree
expand_tree_builtin (tree function, tree params ATTRIBUTE_UNUSED,
		     tree coerced_params)
{
  if (DECL_BUILT_IN_CLASS (function) != BUILT_IN_NORMAL)
    return NULL_TREE;

  switch (DECL_FUNCTION_CODE (function))
    {
    case BUILT_IN_ABS:
    case BUILT_IN_FABS:
      if (coerced_params == 0)
        return integer_zero_node;
      return build_unary_op (ABS_EXPR, TREE_VALUE (coerced_params), 0);
    default:
      break;
    }
  return NULL_TREE;
}

/* the following few functions are taken from c-typeck.c */

/* Perform the default conversion of arrays and functions to pointers.
   Return the result of converting EXP.  For any other expression, just
   return EXP.  */

static tree
default_function_array_conversion (tree exp)
{
  /* not needed in Modula-2 */
  return exp;
}

/* Convert the argument expressions in the list VALUES
   to the types in the list TYPELIST.  The result is a list of converted
   argument expressions, unless there are too few arguments in which
   case it is error_mark_node.

   If TYPELIST is exhausted, or when an element has NULL as its type,
   perform the default conversions.

   PARMLIST is the chain of parm decls for the function being called.
   It may be 0, if that info is not available.
   It is used only for generating error messages.

   FUNCTION is a tree for the called function.  It is used only for
   error messages, where it is formatted with %qE.

   This is also where warnings about wrong number of args are generated.

   Both VALUES and the returned value are chains of TREE_LIST nodes
   with the elements of the list in the TREE_VALUE slots of those nodes.  */

static tree
convert_arguments (tree typelist, tree values, tree function, tree fundecl)
{
  tree typetail, valtail;
  tree result = NULL;
  int parmnum;

  /* Change pointer to function to the function itself for
     diagnostics.  */
  if (TREE_CODE (function) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (function, 0)) == FUNCTION_DECL)
    function = TREE_OPERAND (function, 0);

  /* Scan the given expressions and types, producing individual
     converted arguments and pushing them on RESULT in reverse order.  */

  for (valtail = values, typetail = typelist, parmnum = 0;
       valtail;
       valtail = TREE_CHAIN (valtail), parmnum++)
    {
      tree type = typetail ? TREE_VALUE (typetail) : 0;
      tree val = TREE_VALUE (valtail);
#if 0
      tree rname = function;
      int argnum = parmnum + 1;
#endif
      const char *invalid_func_diag;

      if (type == void_type_node)
        {
          error ("too many arguments to function %qF ", function);
          break;
        }

#if 0
      if (selector && argnum > 2)
        {
          rname = selector;
          argnum -= 2;
        }
#endif

      STRIP_TYPE_NOPS (val);

      val = require_complete_type (val);

      if (type != 0)
        {
          /* Formal parm type is specified by a function prototype.  */
          tree parmval;

          if (type == error_mark_node || !COMPLETE_TYPE_P (type))
            {
              error ("type of formal parameter %d is incomplete", parmnum + 1);
              parmval = val;
            }
          else
            {
#if !defined(GM2)
              /* Optionally warn about conversions that
                 differ from the default conversions.  */
              if (warn_conversion || warn_traditional)
                {
                  unsigned int formal_prec = TYPE_PRECISION (type);

                  if (INTEGRAL_TYPE_P (type)
                      && TREE_CODE (TREE_TYPE (val)) == REAL_TYPE)
                    warning (0, "passing argument %d of %qE as integer "
                             "rather than floating due to prototype",
                             argnum, rname);
                  if (INTEGRAL_TYPE_P (type)
                      && TREE_CODE (TREE_TYPE (val)) == COMPLEX_TYPE)
                    warning (0, "passing argument %d of %qE as integer "
                             "rather than complex due to prototype",
                             argnum, rname);
                  else if (TREE_CODE (type) == COMPLEX_TYPE
                           && TREE_CODE (TREE_TYPE (val)) == REAL_TYPE)
                    warning (0, "passing argument %d of %qE as complex "
                             "rather than floating due to prototype",
                             argnum, rname);
                  else if (TREE_CODE (type) == REAL_TYPE
                           && INTEGRAL_TYPE_P (TREE_TYPE (val)))
                    warning (0, "passing argument %d of %qE as floating "
                             "rather than integer due to prototype",
                             argnum, rname);
                  else if (TREE_CODE (type) == COMPLEX_TYPE
                           && INTEGRAL_TYPE_P (TREE_TYPE (val)))
                    warning (0, "passing argument %d of %qE as complex "
                             "rather than integer due to prototype",
                             argnum, rname);
                  else if (TREE_CODE (type) == REAL_TYPE
                           && TREE_CODE (TREE_TYPE (val)) == COMPLEX_TYPE)
                    warning (0, "passing argument %d of %qE as floating "
                             "rather than complex due to prototype",
                             argnum, rname);
                  /* ??? At some point, messages should be written about
                     conversions between complex types, but that's too messy
                     to do now.  */
                  else if (TREE_CODE (type) == REAL_TYPE
                           && TREE_CODE (TREE_TYPE (val)) == REAL_TYPE)
                    {
                      /* Warn if any argument is passed as `float',
                         since without a prototype it would be `double'.  */
                      if (formal_prec == TYPE_PRECISION (float_type_node))
                        warning (0, "passing argument %d of %qE as %<float%> "
                                 "rather than %<double%> due to prototype",
                                 argnum, rname);
                    }
                  /* Detect integer changing in width or signedness.
                     These warnings are only activated with
                     -Wconversion, not with -Wtraditional.  */
                  else if (warn_conversion && INTEGRAL_TYPE_P (type)
                           && INTEGRAL_TYPE_P (TREE_TYPE (val)))
                    {
                      tree would_have_been = default_conversion (val);
                      tree type1 = TREE_TYPE (would_have_been);

                      if (TREE_CODE (type) == ENUMERAL_TYPE
                          && (TYPE_MAIN_VARIANT (type)
                              == TYPE_MAIN_VARIANT (TREE_TYPE (val))))
                        /* No warning if function asks for enum
                           and the actual arg is that enum type.  */
                        ;
                      else if (formal_prec != TYPE_PRECISION (type1))
                        warning (OPT_Wconversion, "passing argument %d of %qE "
                                 "with different width due to prototype",
                                 argnum, rname);
                      else if (TYPE_UNSIGNED (type) == TYPE_UNSIGNED (type1))
                        ;
                      /* Don't complain if the formal parameter type
                         is an enum, because we can't tell now whether
                         the value was an enum--even the same enum.  */
                      else if (TREE_CODE (type) == ENUMERAL_TYPE)
                        ;
                      else if (TREE_CODE (val) == INTEGER_CST
                               && int_fits_type_p (val, type))
                        /* Change in signedness doesn't matter
                           if a constant value is unaffected.  */
                        ;
                      /* If the value is extended from a narrower
                         unsigned type, it doesn't matter whether we
                         pass it as signed or unsigned; the value
                         certainly is the same either way.  */
                      else if (TYPE_PRECISION (TREE_TYPE (val)) < TYPE_PRECISION (type)
                               && TYPE_UNSIGNED (TREE_TYPE (val)))
                        ;
                      else if (TYPE_UNSIGNED (type))
                        warning (OPT_Wconversion, "passing argument %d of %qE "
                                 "as unsigned due to prototype",
                                 argnum, rname);
                      else
                        warning (OPT_Wconversion, "passing argument %d of %qE "
                                 "as signed due to prototype", argnum, rname);
                    }
                }
#endif

              parmval = convert_for_assignment (type, val, ic_argpass,
                                                fundecl, function,
                                                parmnum + 1);

              if (targetm.calls.promote_prototypes (fundecl ? TREE_TYPE (fundecl) : 0)
                  && INTEGRAL_TYPE_P (type)
                  && (TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node)))
                parmval = default_conversion (parmval);
            }
          result = tree_cons (NULL_TREE, parmval, result);
        }
      else if (TREE_CODE (TREE_TYPE (val)) == REAL_TYPE
               && (TYPE_PRECISION (TREE_TYPE (val))
                   < TYPE_PRECISION (double_type_node)))
        /* Convert `float' to `double'.  */
        result = tree_cons (NULL_TREE, convert (double_type_node, val), result);
      else if ((invalid_func_diag = 
                targetm.calls.invalid_arg_for_unprototyped_fn (typelist, fundecl, val)))
        {
          error (invalid_func_diag);
          return error_mark_node; 
        }
      else
        /* Convert `short' and `char' to full-size `int'.  */
        result = tree_cons (NULL_TREE, default_conversion (val), result);

      if (typetail)
        typetail = TREE_CHAIN (typetail);
    }

  if (typetail != 0 && TREE_VALUE (typetail) != void_type_node)
    {
      error ("too few arguments to function %qF ", function);
      return error_mark_node;
    }

  return nreverse (result);
}

/* Build a function call to function FUNCTION with parameters PARAMS.
   PARAMS is a list--a chain of TREE_LIST nodes--in which the
   TREE_VALUE of each node is a parameter-expression.
   FUNCTION's data type may be a function type or a pointer-to-function.  */

tree
build_function_call (tree function, tree params)
{
  tree fntype, fundecl = 0;
  tree coerced_params;
  tree name = NULL_TREE, assembler_name = NULL_TREE, result;

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

#if !defined(GM2)
  if (fundecl && TREE_THIS_VOLATILE (fundecl))
    current_function_returns_abnormally = 1;
#endif

  /* fntype now gets the type of function pointed to.  */
  fntype = TREE_TYPE (fntype);

  /* Convert the parameters to the types declared in the
     function prototype, or apply default promotions.  */

  coerced_params
    = convert_arguments (TYPE_ARG_TYPES (fntype), params, name, fundecl);

  /* Check for errors in format strings.  */

#if !defined(GM2)
  if (warn_format)
    check_function_format (NULL, TYPE_ATTRIBUTES (fntype), coerced_params);
#endif

  /* Recognize certain built-in functions so we can make tree-codes
     other than CALL_EXPR.  We do this when it enables fold-const.c
     to do something useful.  */

  if (TREE_CODE (function) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (function, 0)) == FUNCTION_DECL
      && DECL_BUILT_IN (TREE_OPERAND (function, 0)))
    {
      result = expand_tree_builtin (TREE_OPERAND (function, 0),
                                    params, coerced_params);
      if (result)
        return result;
    }

  result = build (CALL_EXPR, TREE_TYPE (fntype),
                  function, coerced_params, NULL_TREE);
  TREE_SIDE_EFFECTS (result) = 1;
  result = fold (result);

  if (VOID_TYPE_P (TREE_TYPE (result)))
    return result;
  return require_complete_type (result);
}

/* Make sure that the tag NAME is defined *in the current binding level*
   at least as a forward reference.
   CODE says which kind of tag NAME ought to be.  */

tree
start_struct (enum tree_code code, tree name)
{
  /* If there is already a tag defined at this binding level
     (as a forward reference), just return it.  */

  tree ref = 0;

#if !defined(GM2)
  if (name != 0)
    ref = lookup_tag (code, name, current_binding_level, 1);
#endif

  if (ref && TREE_CODE (ref) == code)
    {
      C_TYPE_BEING_DEFINED (ref) = 1;
      TYPE_PACKED (ref) = flag_pack_struct;
      if (TYPE_FIELDS (ref))
        error ("redefinition of `%s %s'",
               code == UNION_TYPE ? "union" : "struct",
               IDENTIFIER_POINTER (name));

      return ref;
    }

  /* Otherwise create a forward-reference just so the tag is in scope.  */

  ref = make_node (code);
  C_TYPE_BEING_DEFINED (ref) = 1;
  TYPE_PACKED (ref) = flag_pack_struct;
  pushtag (name, ref);
  return ref;
}


tree
gccgm2_BuildStartRecord (char *name)
{
  tree id;

  if ((name == NULL) || (strcmp(name, "") == 0))
    id = NULL_TREE;
  else
    id = get_identifier (name);

  return start_struct (RECORD_TYPE, id);
}


tree
gccgm2_BuildStartVarientRecord (char *name)
{
  tree id;

  if ((name == NULL) || (strcmp(name, "") == 0))
    id = NULL_TREE;
  else
    id = get_identifier (name);

  return start_struct (UNION_TYPE, id);
}


/* Lay out the type T, and its element type, and so on.  */

static void
layout_array_type (tree t)
{
  if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
    layout_array_type (TREE_TYPE (t));
  layout_type (t);
}


/* Build a record field with name (name maybe NULL),
   returning the new field declaration, FIELD_DECL.

   This is done during the parsing of the struct declaration.
   The FIELD_DECL nodes are chained together and the lot of them
   are ultimately passed to `build_struct' to make the RECORD_TYPE node.  */

tree
gccgm2_BuildFieldRecord (char *name, tree type)
{
  tree field, declarator;
  
  if ((name == NULL) || (strcmp(name, "") == 0))
    declarator = NULL_TREE;
  else
    declarator = get_identifier (name);

  /* The corresponding pop_obstacks is in finish_decl.  */
  field = build_decl (FIELD_DECL, declarator, skip_type_decl (type)) ;
  finish_decl (field, NULL_TREE, NULL_TREE);

  return field;
}

/*
 *  ChainOn - interface so that Modula-2 can also create chains of
 *            declarations.
 */

tree
gccgm2_ChainOn (tree t1, tree t2)
{
  return chainon (t1, t2);
}

/*
 *  ChainOnParamValue - adds a list node {parm, value} into the tree list.
 */

tree
gccgm2_ChainOnParamValue (tree list, tree parm, tree value)
{
  return chainon (list, build_tree_list(parm, value));
}

/*
 *  AddStringToTreeList - adds, string, to list.
 */

tree
gccgm2_AddStringToTreeList (tree list, tree string)
{
  return tree_cons (NULL_TREE, string, list);
}

/* Determine whether TYPE is a structure with a flexible array member,
   or a union containing such a structure (possibly recursively).  */

static bool
flexible_array_type_p (tree type)
{
  tree x;
  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
      x = TYPE_FIELDS (type);
      if (x == NULL_TREE)
        return false;
      while (TREE_CHAIN (x) != NULL_TREE)
        x = TREE_CHAIN (x);
      if (TREE_CODE (TREE_TYPE (x)) == ARRAY_TYPE
          && TYPE_SIZE (TREE_TYPE (x)) == NULL_TREE
          && TYPE_DOMAIN (TREE_TYPE (x)) != NULL_TREE
          && TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (x))) == NULL_TREE)
        return true;
      return false;
    case UNION_TYPE:
      for (x = TYPE_FIELDS (type); x != NULL_TREE; x = TREE_CHAIN (x))
        {
          if (flexible_array_type_p (TREE_TYPE (x)))
            return true;
        }
      return false;
    default:
    return false;
  }
}

/* -- from c-decl.c import.. */

/* Build a bit-field integer type for the given WIDTH and UNSIGNEDP.  */
static tree
c_build_bitfield_integer_type (unsigned HOST_WIDE_INT width, int unsignedp)
{
  /* Extended integer types of the same width as a standard type have
     lesser rank, so those of the same width as int promote to int or
     unsigned int and are valid for printf formats expecting int or
     unsigned int.  To avoid such special cases, avoid creating
     extended integer types for bit-fields if a standard integer type
     is available.  */
  if (width == TYPE_PRECISION (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;
  if (width == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;
  if (width == TYPE_PRECISION (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;
  if (width == TYPE_PRECISION (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;
  if (width == TYPE_PRECISION (long_long_integer_type_node))
    return (unsignedp ? long_long_unsigned_type_node
            : long_long_integer_type_node);
  return build_nonstandard_integer_type (width, unsignedp);
}

/* Push a definition or a declaration of struct, union or enum tag "name".
   "type" should be the type node.
   We assume that the tag "name" is not already defined.

   Note that the definition may really be just a forward reference.
   In that case, the TYPE_SIZE will be zero.  */

void
pushtag (tree name, tree type)
{
  /* Record the identifier as the type's name if it has none.  */
  if (name && !TYPE_NAME (type))
    TYPE_NAME (type) = name;
#if 0
  bind (name, type, current_scope, /*invisible=*/false, /*nested=*/false);
#endif

  /* Create a fake NULL-named TYPE_DECL node whose TREE_TYPE will be the
     tagged type we just added to the current scope.  This fake
     NULL-named TYPE_DECL node helps dwarfout.c to know when it needs
     to output a representation of a tagged type, and it also gives
     us a convenient place to record the "scope start" address for the
     tagged type.  */

  TYPE_STUB_DECL (type) = pushdecl (build_decl (TYPE_DECL, NULL_TREE, type));

  /* An approximation for now, so we can tell this is a function-scope tag.
     This will be updated in pop_scope.  */
  TYPE_CONTEXT (type) = DECL_CONTEXT (TYPE_STUB_DECL (type));
}

/* Fill in the fields of a RECORD_TYPE or UNION_TYPE node, T.
   FIELDLIST is a chain of FIELD_DECL nodes for the fields.
   ATTRIBUTES are attributes to be applied to the structure.  */

/* -- from c-decl.c import finish_struct -- */

tree
gccgm2_BuildEndRecord (tree t, tree fieldlist /* , attributes */ )
{
  tree x;
  int toplevel = global_binding_level == current_binding_level;
  int saw_named_field;

  /* If this type was previously laid out as a forward reference,
     make sure we lay it out again.  */

  TYPE_SIZE (t) = 0;

#if !defined(GM2)
  decl_attributes (&t, attributes, (int) ATTR_FLAG_TYPE_IN_PLACE);
#endif

  if (pedantic)
    {
      for (x = fieldlist; x; x = TREE_CHAIN (x))
        if (DECL_NAME (x) != 0)
          break;

      if (x == 0)
        {
          if (TREE_CODE (t) == UNION_TYPE)
            {
              if (fieldlist)
                pedwarn ("union has no named members");
              else
                pedwarn ("union has no members");
            }
          else
            {
              if (fieldlist)
                pedwarn ("struct has no named members");
              else
                pedwarn ("struct has no members");
            }
        }
    }

  /* Install struct as DECL_CONTEXT of each field decl.
     Also process specified field sizes, found in the DECL_INITIAL,
     storing 0 there after the type has been changed to precision equal
     to its width, rather than the precision of the specified standard
     type.  (Correct layout requires the original type to have been preserved
     until now.)  */

  saw_named_field = 0;
  for (x = fieldlist; x; x = TREE_CHAIN (x))
    {
      DECL_CONTEXT (x) = t;

      if (TYPE_PACKED (t) && TYPE_ALIGN (TREE_TYPE (x)) > BITS_PER_UNIT)
        DECL_PACKED (x) = 1;

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

      if (DECL_INITIAL (x))
        {
          unsigned HOST_WIDE_INT width = tree_low_cst (DECL_INITIAL (x), 1);
          DECL_SIZE (x) = bitsize_int (width);
          DECL_BIT_FIELD (x) = 1;
          SET_DECL_C_BIT_FIELD (x);
        }

      /* Detect flexible array member in an invalid context.  */
      if (TREE_CODE (TREE_TYPE (x)) == ARRAY_TYPE
          && TYPE_SIZE (TREE_TYPE (x)) == NULL_TREE
          && TYPE_DOMAIN (TREE_TYPE (x)) != NULL_TREE
          && TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (x))) == NULL_TREE)
        {
          if (TREE_CODE (t) == UNION_TYPE)
            {
              error ("%Jflexible array member in union", x);
              TREE_TYPE (x) = error_mark_node;
            }
          else if (TREE_CHAIN (x) != NULL_TREE)
            {
              error ("%Jflexible array member not at end of struct", x);
              TREE_TYPE (x) = error_mark_node;
            }
          else if (!saw_named_field)
            {
              error ("%Jflexible array member in otherwise empty struct", x);
              TREE_TYPE (x) = error_mark_node;
            }
        }

      if (pedantic && !in_system_header && TREE_CODE (t) == RECORD_TYPE
          && flexible_array_type_p (TREE_TYPE (x)))
        pedwarn ("%Jinvalid use of structure with flexible array member", x);

      if (DECL_NAME (x))
        saw_named_field = 1;
    }

#if !defined(GM2)
  detect_field_duplicates (fieldlist);
#endif

  /* Now we have the nearly final fieldlist.  Record it,
     then lay out the structure or union (including the fields).  */

  TYPE_FIELDS (t) = fieldlist;

  layout_type (t);

  /* Give bit-fields their proper types.  */
  {
    tree *fieldlistp = &fieldlist;
    while (*fieldlistp)
      if (TREE_CODE (*fieldlistp) == FIELD_DECL && DECL_INITIAL (*fieldlistp)
          && TREE_TYPE (*fieldlistp) != error_mark_node)
        {
          unsigned HOST_WIDE_INT width
            = tree_low_cst (DECL_INITIAL (*fieldlistp), 1);
          tree type = TREE_TYPE (*fieldlistp);
          if (width != TYPE_PRECISION (type))
            {
              TREE_TYPE (*fieldlistp)
                = c_build_bitfield_integer_type (width, TYPE_UNSIGNED (type));
              DECL_MODE (*fieldlistp) = TYPE_MODE (TREE_TYPE (*fieldlistp));
            }
          DECL_INITIAL (*fieldlistp) = 0;
        }
      else
        fieldlistp = &TREE_CHAIN (*fieldlistp);
  }

  /* Now we have the truly final field list.
     Store it in this type and in the variants.  */

  TYPE_FIELDS (t) = fieldlist;

#if !defined(GM2)
  /* If there are lots of fields, sort so we can look through them fast.
     We arbitrarily consider 16 or more elts to be "a lot".  */

  {
    int len = 0;

    for (x = fieldlist; x; x = TREE_CHAIN (x))
      {
        if (len > 15 || DECL_NAME (x) == NULL)
          break;
        len += 1;
      }

    if (len > 15)
      {
        tree *field_array;
        struct lang_type *space;
        struct sorted_fields_type *space2;

        len += list_length (x);

        /* Use the same allocation policy here that make_node uses, to
          ensure that this lives as long as the rest of the struct decl.
          All decls in an inline function need to be saved.  */

        space = GGC_CNEW (struct lang_type);
        space2 = GGC_NEWVAR (struct sorted_fields_type,
                             sizeof (struct sorted_fields_type) + len * sizeof (tree));

        len = 0;
        space->s = space2;
        field_array = &space2->elts[0];
        for (x = fieldlist; x; x = TREE_CHAIN (x))
          {
            field_array[len++] = x;

            /* If there is anonymous struct or union, break out of the loop.  */
            if (DECL_NAME (x) == NULL)
              break;
          }
        /* Found no anonymous struct/union.  Add the TYPE_LANG_SPECIFIC.  */
        if (x == NULL)
          {
            TYPE_LANG_SPECIFIC (t) = space;
            TYPE_LANG_SPECIFIC (t)->s->len = len;
            field_array = TYPE_LANG_SPECIFIC (t)->s->elts;
            qsort (field_array, len, sizeof (tree), field_decl_cmp);
          }
      }
  }
#endif

  for (x = TYPE_MAIN_VARIANT (t); x; x = TYPE_NEXT_VARIANT (x))
    {
      TYPE_FIELDS (x) = TYPE_FIELDS (t);
      TYPE_LANG_SPECIFIC (x) = TYPE_LANG_SPECIFIC (t);
      TYPE_ALIGN (x) = TYPE_ALIGN (t);
      TYPE_USER_ALIGN (x) = TYPE_USER_ALIGN (t);
      C_TYPE_FIELDS_READONLY (x) = C_TYPE_FIELDS_READONLY (t);
      C_TYPE_FIELDS_VOLATILE (x) = C_TYPE_FIELDS_VOLATILE (t);
      C_TYPE_VARIABLE_SIZE (x) = C_TYPE_VARIABLE_SIZE (t);
    }

  /* If this was supposed to be a transparent union, but we can't
     make it one, warn and turn off the flag.  */
  if (TREE_CODE (t) == UNION_TYPE
      && TYPE_TRANSPARENT_UNION (t)
      && (!TYPE_FIELDS (t) || TYPE_MODE (t) != DECL_MODE (TYPE_FIELDS (t))))
    {
      TYPE_TRANSPARENT_UNION (t) = 0;
      warning (0, "union cannot be made transparent");
    }

#if !defined(GM2)
  /* If this structure or union completes the type of any previous
     variable declaration, lay it out and output its rtl.  */
  for (x = C_TYPE_INCOMPLETE_VARS (TYPE_MAIN_VARIANT (t));
       x;
       x = TREE_CHAIN (x))
    {
      tree decl = TREE_VALUE (x);
      if (TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
        layout_array_type (TREE_TYPE (decl));
      if (TREE_CODE (decl) != TYPE_DECL)
        {
          layout_decl (decl, 0);
          if (c_dialect_objc ())
            objc_check_decl (decl);
          rest_of_decl_compilation (decl, toplevel, 0);
          if (!toplevel)
            expand_decl (decl);
        }
    }
  C_TYPE_INCOMPLETE_VARS (TYPE_MAIN_VARIANT (t)) = 0;
#endif

  /* Finish debugging output for this type.  */
  rest_of_type_compilation (t, toplevel);

  /* If we're inside a function proper, i.e. not file-scope and not still
     parsing parameters, then arrange for the size of a variable sized type
     to be bound now.  */
  if (cur_stmt_list && variably_modified_type_p (t, NULL))
    add_stmt (build_stmt (DECL_EXPR, build_decl (TYPE_DECL, NULL, t)));

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

  if (enumtype == 0 || TREE_CODE (enumtype) != ENUMERAL_TYPE)
    {
      enumtype = make_node (ENUMERAL_TYPE);
      pushtag (name, enumtype);
    }
  
  if (TYPE_VALUES (enumtype) != 0)
    {
      /* This enum is a named one that has been declared already.  */
      error ("redeclaration of enum %qs", IDENTIFIER_POINTER (name));

      /* Completely replace its old definition.
         The old enumerators remain defined, however.  */
      TYPE_VALUES (enumtype) = 0;
    }

  enum_next_value = integer_zero_node;
  enum_overflow = 0;

  if (flag_short_enums)
    TYPE_PACKED (enumtype) = 1;

#if defined(GM2)
  TREE_TYPE (enumtype) = gccgm2_GetIntegerType();
#endif

  return enumtype;
}

/* Return the minimum number of bits needed to represent VALUE in a
   signed or unsigned type, UNSIGNEDP says which.  */

unsigned int
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
  else
    log = tree_floor_log2 (value);

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
  tree pair, tem;
  tree minnode = 0, maxnode = 0;
  int precision, unsign;
  int toplevel = (global_binding_level == current_binding_level);

#if !defined(GM2)
  struct lang_type *lt;
  decl_attributes (&enumtype, attributes, (int) ATTR_FLAG_TYPE_IN_PLACE);
#endif

  /* Calculate the maximum value of any enumerator in this type.  */

  if (values == error_mark_node)
    minnode = maxnode = integer_zero_node;
  else
    {
      minnode = maxnode = TREE_VALUE (values);
      for (pair = TREE_CHAIN (values); pair; pair = TREE_CHAIN (pair))
        {
          tree value = TREE_VALUE (pair);
          if (tree_int_cst_lt (maxnode, value))
            maxnode = value;
          if (tree_int_cst_lt (value, minnode))
            minnode = value;
        }
    }

  /* Construct the final type of this enumeration.  It is the same
     as one of the integral types - the narrowest one that fits, except
     that normally we only go as narrow as int - and signed iff any of
     the values are negative.  */
  unsign = (tree_int_cst_sgn (minnode) >= 0);
  precision = MAX (min_precision (minnode, unsign),
                   min_precision (maxnode, unsign));

  if (TYPE_PACKED (enumtype) || precision > TYPE_PRECISION (integer_type_node))
    {
#if defined(GM2)
      tem = gm2_type_for_size (precision, unsign);
#else
      tem = c_common_type_for_size (precision, unsign);
#endif
      if (tem == NULL)
        {
          warning (0, "enumeration values exceed range of largest integer");
          tem = long_long_integer_type_node;
        }
    }
  else
    tem = unsign ? unsigned_type_node : integer_type_node;

  TYPE_MIN_VALUE (enumtype) = TYPE_MIN_VALUE (tem);
  TYPE_MAX_VALUE (enumtype) = TYPE_MAX_VALUE (tem);
  TYPE_UNSIGNED (enumtype) = TYPE_UNSIGNED (tem);
  TYPE_SIZE (enumtype) = 0;

  /* If the precision of the type was specific with an attribute and it
     was too small, give an error.  Otherwise, use it.  */
  if (TYPE_PRECISION (enumtype))
    {
      if (precision > TYPE_PRECISION (enumtype))
        error ("specified mode too small for enumeral values");
    }
  else
    TYPE_PRECISION (enumtype) = TYPE_PRECISION (tem);

  layout_type (enumtype);

  if (values != error_mark_node)
    {
      /* Change the type of the enumerators to be the enum type.  We
         need to do this irrespective of the size of the enum, for
         proper type checking.  Replace the DECL_INITIALs of the
         enumerators, and the value slots of the list, with copies
         that have the enum type; they cannot be modified in place
         because they may be shared (e.g.  integer_zero_node) Finally,
         change the purpose slots to point to the names of the decls.  */
      for (pair = values; pair; pair = TREE_CHAIN (pair))
        {
          tree enu = TREE_PURPOSE (pair);
          tree ini = DECL_INITIAL (enu);

          TREE_TYPE (enu) = enumtype;

          /* The ISO C Standard mandates enumerators to have type int,
             even though the underlying type of an enum type is
             unspecified.  Here we convert any enumerators that fit in
             an int to type int, to avoid promotions to unsigned types
             when comparing integers with enumerators that fit in the
             int range.  When -pedantic is given, build_enumerator()
             would have already taken care of those that don't fit.  */
          if (int_fits_type_p (ini, integer_type_node))
            tem = integer_type_node;
          else
            tem = enumtype;
          ini = convert (tem, ini);

          DECL_INITIAL (enu) = ini;
          TREE_PURPOSE (pair) = DECL_NAME (enu);
          TREE_VALUE (pair) = ini;
        }

      TYPE_VALUES (enumtype) = values;
    }

#if !defined(GM2)
  /* Record the min/max values so that we can warn about bit-field
     enumerations that are too small for the values.  */
  lt = GGC_CNEW (struct lang_type);
  lt->enum_min = minnode;
  lt->enum_max = maxnode;
  TYPE_LANG_SPECIFIC (enumtype) = lt;
#endif

  /* Fix up all variant types of this enum type.  */
  for (tem = TYPE_MAIN_VARIANT (enumtype); tem; tem = TYPE_NEXT_VARIANT (tem))
    {
      if (tem == enumtype)
        continue;
      TYPE_VALUES (tem) = TYPE_VALUES (enumtype);
      TYPE_MIN_VALUE (tem) = TYPE_MIN_VALUE (enumtype);
      TYPE_MAX_VALUE (tem) = TYPE_MAX_VALUE (enumtype);
      TYPE_SIZE (tem) = TYPE_SIZE (enumtype);
      TYPE_SIZE_UNIT (tem) = TYPE_SIZE_UNIT (enumtype);
      TYPE_MODE (tem) = TYPE_MODE (enumtype);
      TYPE_PRECISION (tem) = TYPE_PRECISION (enumtype);
      TYPE_ALIGN (tem) = TYPE_ALIGN (enumtype);
      TYPE_USER_ALIGN (tem) = TYPE_USER_ALIGN (enumtype);
      TYPE_UNSIGNED (tem) = TYPE_UNSIGNED (enumtype);
      TYPE_LANG_SPECIFIC (tem) = TYPE_LANG_SPECIFIC (enumtype);
    }

  /* Finish debugging output for this type.  */
  rest_of_type_compilation (enumtype, toplevel);

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
  register tree decl, type;

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
      if (enum_overflow)
        error ("overflow in enumeration values");
    }

  if (pedantic && ! int_fits_type_p (value, integer_type_node))
    {
      pedwarn ("ISO C restricts enumerator values to range of `int'");
      value = convert (integer_type_node, value);
    }

  /* Set basis for default for next value.  */
  enum_next_value = build_binary_op (PLUS_EXPR, value, integer_one_node, 0);
  enum_overflow = tree_int_cst_lt (enum_next_value, value);

  /* Now create a declaration for the enum value name.  */

  type = TREE_TYPE (value);
  type = gm2_type_for_size (MAX (TYPE_PRECISION (type),
                                 TYPE_PRECISION (integer_type_node)),
                            ((flag_traditional
                              || TYPE_PRECISION (type) >= TYPE_PRECISION (integer_type_node))
                             && TYPE_UNSIGNED (type)));

  decl = build_decl (CONST_DECL, name, type);
  DECL_INITIAL (decl) = convert (type, value);
  pushdecl (decl);

  return tree_cons (decl, value, NULL_TREE);
}

/*
 *  ExpandExpressionStatement - maps onto expand_expr_stmt_value in stmt.c
 */

void
gccgm2_ExpandExpressionStatement (tree t)
{
  add_stmt (t);
}

/*
 *  InitGlobalContext - initializes a dummy function for the global scope.
 */

void gccgm2_InitGlobalContext (void)
{
  if (cfun == NULL)
    init_dummy_function_start ();
}

/*
 *  MarkFunctionReferenced - marks a function as referenced.
 */

void
gccgm2_MarkFunctionReferenced (tree f)
{
  if (f != NULL_TREE)
    if (TREE_CODE (f) == FUNCTION_DECL)
      mark_decl_referenced (f);
}

/*
 *  FinishBackend - flushes all outstanding functions held in the GCC backend
 *                  out to the assembly file.
 */

void gccgm2_FinishBackend (void)
{
  /* We're done parsing; proceed to optimize and emit assembly. */
  cgraph_finalize_compilation_unit ();
  cgraph_optimize ();
}

/*
 *  SetFlagUnitAtATime - sets GCC flag_unit_at_a_time to b.
 */

void gccgm2_SetFlagUnitAtATime (int b)
{
  flag_unit_at_a_time = b;
  flag_inline_trees = b;
}

/*
 *  GarbageCollect - force gcc to garbage collect.
 */

void
gccgm2_GarbageCollect (void)
{
  ggc_collect();
}

#include "gtype-gm2.h"
#include "gt-gm2-gccgm2.h"

/*
 * Local variables:
 *  compile-command: "gcc -c  -DIN_GCC    -g -W -Wall -Wwrite-strings -Wtraditional -Wstrict-prototypes -Wmissing-prototypes -pedantic -Wno-long-long  -W -Wall -DGM2 -I. -I.. -I. -I./.. -I./../config -I./../../include gccgm2.c"
 * End:
 */
