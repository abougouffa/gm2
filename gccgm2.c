/* Copyright (C) 2000, 2001, 2002, 2003, 2004
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
along with GNU Modula-2; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/*
 * IMPLEMENTATION MODULE gccgm2
 *     thus all external functions will be prefixed by gccgm2_
 *     which will allow us to construct a DEFINITION MODULE
 *     so that the Modula-2 source code front end can interface to gcc.
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
static int broken_set_debugging_info = TRUE;

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

#define GM2
#define GM2_BUG_REPORT "Please report this crash to the GNU Modula-2 mailing list <gm2@glam.ac.uk>\n"

#define ASSERT(X,Y)   { if (!(X)) { debug_tree(Y); internal_error("[%s:%d]:condition `%s' failed", \
                                                                   __FILE__, __LINE__, #X); } }

enum attrs {A_PACKED, A_NOCOMMON, A_COMMON, A_NORETURN, A_CONST, A_T_UNION,
            A_CONSTRUCTOR, A_DESTRUCTOR, A_MODE, A_SECTION, A_ALIGNED,
            A_UNUSED, A_FORMAT, A_FORMAT_ARG, A_WEAK, A_ALIAS};

/*
 *  and define some type sizes
 */

#ifndef SET_WORD_SIZE
#define SET_WORD_SIZE BITS_PER_WORD
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
static GTY(()) tree m2_iso_loc_type_node;
static GTY(()) tree m2_iso_byte_type_node;
static GTY(()) tree m2_iso_word_type_node;
static GTY(()) tree set_full_complement;



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

static GTY(()) tree c_global_trees[CTI_MAX];

extern tree current_function_decl;


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
GTY(()) tree param_type_list;
GTY(()) tree param_list = NULL_TREE;   /* ready for the next time we call/define a function */
GTY(()) tree last_function=NULL_TREE;
static int label_count = 0;

/* A list (chain of TREE_LIST nodes) of all LABEL_DECLs in the function
   that have names.  Here so we can clear out their names' definitions
   at the end of the function.  */

static GTY(()) tree named_labels;

/* A list of LABEL_DECLs from outer contexts that are currently shadowed.  */

static GTY(()) tree shadowed_labels;

/* constructor globals used by BuildStartSetConstructor
 * (constant set creation)
 */

static GTY(()) tree constructor_type = NULL_TREE;
static GTY(()) tree constructor_fields = NULL_TREE;
static GTY(()) tree constructor_element_list = NULL_TREE;

static tree qualify_type                PARAMS ((tree, tree));
static tree build_c_type_variant        PARAMS ((tree, int, int));

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

/* Warn about testing equality of floating point numbers.  */

int warn_float_equal = 0;

/* Nonzero means give `double' the same size as `float'.  */

int flag_short_double=0;

/* Nonzero means to allow single precision math even if we're generally
   being traditional.  */
int flag_allow_single_precision = 0;

/* Nonzero whenever Objective-C functionality is being used.  */
int flag_objc = 0;

/* Nonzero means to warn about compile-time division by zero.  */
int warn_div_by_zero = 1;

/* arguments given to compiler */

extern int save_argc ;
extern char **save_argv;

static int insideCppArgs = 0;

extern void gccgm2front                     PARAMS((int argc, char *argv[]));
extern void gm2builtins_init                PARAMS((void));
extern tree gm2builtins_BuiltInHugeVal      PARAMS((void));
extern tree gm2builtins_BuiltInHugeValShort PARAMS((void));
extern tree gm2builtins_BuiltInHugeValLong  PARAMS((void));

/* Global Variables Expected by gcc: */

int flag_traditional=FALSE;     /* Used by dwarfout.c.  */


/* function prototypes */
#define EXTERN 
#include "gm2-common.h"
#undef EXTERN

#define EXTERN extern
#include "gm2-lang.h"
#undef EXTERN

       tree                   finish_enum                                 PARAMS ((tree, tree, tree));
       void                   gccgm2_EndTemporaryAllocation               PARAMS ((void));
       void                   gccgm2_ResumeTemporaryAllocation  	  PARAMS ((void));
       void                   gccgm2_SetFileNameAndLineNo       	  PARAMS ((char *fn, int line));
       void                   gccgm2_EmitLineNote               	  PARAMS ((char *fn, int line));
static struct binding_level  *make_binding_level                	  PARAMS ((void));
//static void                   mark_binding_level                          PARAMS ((void *arg));
       int                    kept_level_p                      	  PARAMS ((void));
       int                    in_parm_level_p                   	  PARAMS ((void));
static void                   clear_limbo_values                	  PARAMS ((tree block));
       tree                   define_label                      	  PARAMS ((const char *filename, int line, tree name));
       void                   delete_block                      	  PARAMS ((tree block));
       tree                   lookup_name                       	  PARAMS ((tree name));
       tree                   lookup_name_current_level         	  PARAMS ((tree name));
       tree                   pushdecl_top_level                	  PARAMS ((tree x));
       void                   lang_mark_tree                    	  PARAMS ((tree t));
       void                   init_m2_builtins                  	  PARAMS ((void));
       void                   lang_finish                       	  PARAMS ((void));
       const char            *init_parse                        	  PARAMS ((const char *filename));
       void                   finish_parse                      	  PARAMS ((void));
       const char            *lang_identify                     	  PARAMS ((void));
       void                   print_lang_statistics             	  PARAMS ((void));
       void                   init_lex                          	  PARAMS ((void));
       void                   yyerror                           	  PARAMS ((char *str));
static tree                   build_c_type_variant              	  PARAMS ((tree type, int constp, int volatilep));
static tree                   qualify_type                      	  PARAMS ((tree type, tree like));
       tree                   common_type                       	  PARAMS ((tree t1, tree t2));
       tree                   base_type            		       	  PARAMS ((tree type));
       int                    comptypes      	        	       	  PARAMS ((tree type1, tree type2));
static void                   pedantic_lvalue_warning 	         	  PARAMS ((enum tree_code code));
       void                   readonly_warning 		        	  PARAMS ((tree arg, const char *string));
       int                    is_string_type 		        	  PARAMS ((tree string, int warn));
       int                    lvalue_p                		       	  PARAMS ((tree ref));
       int                    lvalue_or_else 		        	  PARAMS ((tree ref, const char *string));
static tree                   unary_complex_lvalue 	        	  PARAMS ((enum tree_code code, tree arg, int flag));
       tree                   default_conversion 	        	  PARAMS ((tree exp));
static int                    comp_target_types                           PARAMS ((tree, tree, int));
       tree                   convert_set    		        	  PARAMS ((tree type, tree expr));
       tree                   convert        		        	  PARAMS ((tree type, tree expr));
       tree                   gccgm2_BuildCoerce                          PARAMS ((tree op1, tree op2, tree op3));
#if 0
static tree                   build_m2_cast                               PARAMS ((tree, tree));
#endif
static void                   warn_for_assignment 	       	          PARAMS ((const char *msg, const char *opname,
									 	  tree function, int argnum));
       void                   constant_expression_warning       	  PARAMS ((tree value));
       void                   overflow_warning 		         	  PARAMS ((tree value));
       void                   unsigned_conversion_warning       	  PARAMS ((tree result, tree operand));
       tree                   convert_and_check 		       	  PARAMS ((tree type, tree expr));
       tree                   build_conditional_expr 	         	  PARAMS ((tree ifexp, tree op1, tree op2));
       tree                   c_size_in_bytes 		        	  PARAMS ((tree type));
       tree                   shorten_compare 		        	  PARAMS ((tree *op0_ptr, tree *op1,
									 	  tree *restype_ptr,
									 	  enum tree_code *rescode_ptr));
       tree                   require_complete_type 	       	 	  PARAMS ((tree value));
static tree                   convert_for_assignment                      PARAMS ((tree, tree, const char *, tree, tree, int));
       tree                   build_modify_expr 		       	  PARAMS ((tree lhs, enum tree_code modifycode,
									 	  tree rhs));
       tree                   c_build_qualified_type 	       	 	  PARAMS ((tree type, int type_quals));
       tree                   build_unary_op 		       	 	  PARAMS ((enum tree_code code, tree xarg, int noconvert));
       void                   binary_op_error 		       	 	  PARAMS ((enum tree_code code));
       tree                   build_binary_op 		       	 	  PARAMS ((enum tree_code code, tree orig_op0,
									 	  tree orig_op1, int convert_p));
       tree                   gccgm2_DeclareKnownType 	       	 	  PARAMS ((char *name, tree type));
       tree                   skip_type_decl 		       	 	  PARAMS ((tree type));
       tree                   skip_const_decl 		       	 	  PARAMS ((tree exp));
       tree                   gccgm2_DeclareKnownVariable                 PARAMS ((char *, tree, int, int, int, int, tree));

       tree                   gccgm2_GetMinFrom 		       	  PARAMS ((tree type));
       tree                   gccgm2_GetMaxFrom 		       	  PARAMS ((tree type));
       int                    gccgm2_GetBitsPerWord 	       	 	  PARAMS ((void));
       int                    gccgm2_GetBitsPerUnit 	       	 	  PARAMS ((void));
       int                    gccgm2_GetBitsPerInt 	       	 	  PARAMS ((void));
       int                    gccgm2_GetBitsPerBitset 	       	 	  PARAMS ((void));
       void                   trythis        		       	 	  PARAMS ((void));
       tree                   gccgm2_DeclareKnownConstant       	  PARAMS ((tree type, tree value));
       int                    complete_array_type 	       	 	  PARAMS ((tree type, tree initial_value,
									 	  int do_default));
       void                   finish_decl    		       	 	  PARAMS ((tree decl, tree init, tree asmspec_tree));
#if 0
static void                   add_attribute  		       	 	  PARAMS ((enum attrs id, const char *string, int min_len,
									 	  int max_len, int decl_req));
static void                   init_attributes 		       	 	  PARAMS ((void));
       void                   split_specs_attrs 		 	  PARAMS ((tree specs_attrs, tree *declspecs,
									 	  tree *prefix_attributes));
static int                    default_valid_lang_attribute                PARAMS ((tree attr_name, tree attr_args,
									 	  tree decl, tree type));
#endif
       tree                   lookup_label   		       	 	  PARAMS ((tree id));
       tree                   gccgm2_BuildStartEnumeration      	  PARAMS ((char *name));
       tree                   gccgm2_BuildEndEnumeration        	  PARAMS ((tree enumtype));
       tree                   gccgm2_BuildEnumerator 	       	 	  PARAMS ((char *name, tree value));
       tree                   gccgm2_BuildPointerType 	       	 	  PARAMS ((tree totype));
       tree                   gccgm2_BuildArrayType 	       	 	  PARAMS ((tree elementtype, tree indextype));
       tree                   build_set_type                              PARAMS ((tree domain, tree type, int allow_void));
       tree                   gccgm2_BuildSetTypeFromSubrange             PARAMS ((char *, tree, tree, tree));
       tree                   gccgm2_BuildSetType 	       	 	  PARAMS ((char *name, tree type, tree lowval,
									 	  tree highval));
       tree                   gccgm2_BuildSubrangeType 	       	 	  PARAMS ((char *name, tree type, tree lowval,
									 	  tree highval));
       tree                   gccgm2_BuildArrayIndexType        	  PARAMS ((tree low, tree high));
       tree                   gccgm2_BuildVariableArrayAndDeclare         PARAMS ((tree elementtype,
									 	  tree high, char *name,
									 	  tree scope));
       tree                   gccgm2_BuildStartFunctionType     	  PARAMS ((char *name));
       tree                   gccgm2_BuildEndFunctionType       	  PARAMS ((tree func, tree type));
static tree                   finish_build_pointer_type                   PARAMS ((tree t, tree to_type));

       tree                   gccgm2_BuildParameterDeclaration  	  PARAMS ((char *name, tree type,
									 	  int isreference));
       tree                   completeParameterDeclaration      	  PARAMS ((char *name, tree actual_type,
									 	  tree parm_type));
       tree                   gccgm2_BuildParameterDeclaration  	  PARAMS ((char *name, tree type,
									 	  int isreference));
       void                   gccgm2_BuildStartFunctionDeclaration 	  PARAMS ((int uses_varargs));
       tree                   gccgm2_BuildEndFunctionDeclaration  	  PARAMS ((char *name,
										   tree returntype,
										   int isexternal,
										   int isnested));
       void                   gccgm2_BuildStartFunctionCode     	  PARAMS ((tree fndecl, int isexported));
       void                   gccgm2_BuildEndFunctionCode       	  PARAMS ((tree fndecl));
       void                   iterative_factorial 	       	 	  PARAMS ((void));
       void                   gccgm2_BuildReturnValueCode       	  PARAMS ((tree fndecl, tree value));
       tree                   gccgm2_BuildAssignment 	       	 	  PARAMS ((tree des, tree expr));
       tree                   gccgm2_BuildAdd 		       	 	  PARAMS ((tree op1, tree op2, int needconvert));
       tree                   gccgm2_BuildSub 		       	 	  PARAMS ((tree op1, tree op2, int needconvert));
       tree                   gccgm2_BuildMult 		       	 	  PARAMS ((tree op1, tree op2, int needconvert));
       tree                   gccgm2_BuildDiv 		       	 	  PARAMS ((tree op1, tree op2, int needconvert));
       tree                   gccgm2_BuildMod 		       	 	  PARAMS ((tree op1, tree op2, int needconvert));
       tree                   gccgm2_BuildLSL 		       	 	  PARAMS ((tree op1, tree op2, int needconvert));
       tree                   gccgm2_BuildLSR                             PARAMS ((tree op1, tree op2, int needconvert));
       tree                   gccgm2_BuildConvert 	       	 	  PARAMS ((tree op1, tree op2));
       tree                   gccgm2_BuildTrunc 		       	  PARAMS ((tree op1));
       tree                   gccgm2_BuildNegate 	       	 	  PARAMS ((tree op1, int needconvert));
       tree                   gccgm2_BuildSetNegate 	       	 	  PARAMS ((tree op1, int needconvert));
       tree                   gccgm2_GetSizeOf 		       	 	  PARAMS ((tree type));
       tree                   gccgm2_GetSizeOfInBits                      PARAMS ((tree type));
       tree                   gccgm2_BuildAddr 		       	 	  PARAMS ((tree op1, int needconvert));
       tree                   gccgm2_BuildLogicalOrAddress     	 	  PARAMS ((tree op1, tree op2, int needconvert));
       tree                   gccgm2_BuildLogicalOr 	       	 	  PARAMS ((tree op1, tree op2, int needconvert));
       tree                   gccgm2_BuildLogicalAnd 	       	 	  PARAMS ((tree op1, tree op2, int needconvert));
       tree                   gccgm2_BuildSymmetricDifference   	  PARAMS ((tree op1, tree op2, int needconvert));
       tree                   gccgm2_BuildLogicalDifference     	  PARAMS ((tree op1, tree op2, int needconvert));
       void                   gccgm2_BuildLogicalShift                    PARAMS ((tree op1, tree op2, tree op3, tree nBits, int needconvert));
       tree                   gccgm2_BuildLRL                             PARAMS ((tree op1, tree op2, int needconvert));
       tree                   gccgm2_BuildLRR                             PARAMS ((tree op1, tree op2, int needconvert));
       tree                   gccgm2_BuildLRLn                            PARAMS ((tree op1, tree op2, tree nBits, int needconvert));
       tree                   gccgm2_BuildLRRn                            PARAMS ((tree op1, tree op2, tree nBits, int needconvert));
       tree                   gccgm2_BuildMask                            PARAMS ((tree nBits, int needconvert));
       void                   gccgm2_BuildLogicalRotate                   PARAMS ((tree op1, tree op2, tree op3, tree nBits, int needconvert));
       void                   gccgm2_BuildBinarySetDo                     PARAMS ((tree settype, tree op1, tree op2, tree op3, tree (*binop)(tree, tree, tree, tree, int), int is_op1lvalue, int is_op2lvalue, int is_op3lvalue, tree nBits, tree unbounded, tree varproc, tree leftproc, tree rightproc));
       tree                   buildUnboundedArrayOf                       PARAMS ((tree, tree, tree));
       tree                   create_label_from_name 	       	 	  PARAMS ((char *name));
       tree                   gccgm2_DeclareLabel 	       	 	  PARAMS ((char *name));
static char *                 createUniqueLabel                           PARAMS ((void));
       tree                   gccgm2_BuildLessThan 	       	 	  PARAMS ((tree op1, tree op2));
       tree                   gccgm2_BuildGreaterThan 	       	 	  PARAMS ((tree op1, tree op2));
       tree                   gccgm2_BuildLessThanOrEqual       	  PARAMS ((tree op1, tree op2));
       tree                   gccgm2_BuildGreaterThanOrEqual    	  PARAMS ((tree op1, tree op2));
       tree                   gccgm2_BuildEqualTo 	       	 	  PARAMS ((tree op1, tree op2));
       tree                   gccgm2_BuildNotEqualTo 	       	 	  PARAMS ((tree op1, tree op2));
       tree                   gccgm2_BuildIsSuperset                      PARAMS ((tree op1, tree op2));
       tree                   gccgm2_BuildIsNotSuperset                   PARAMS ((tree op1, tree op2));
       tree                   gccgm2_BuildIsSubset                        PARAMS ((tree op1, tree op2));
       tree                   gccgm2_BuildIsNotSubset                     PARAMS ((tree op1, tree op2));
       tree                   gccgm2_BuildIndirect 	       	 	  PARAMS ((tree target, tree type));
       void                   gccgm2_DoJump  		       	 	  PARAMS ((tree exp, char *falselabel, char *truelabel));
       void                   gccgm2_BuildParam 		       	  PARAMS ((tree param));
       tree                   gccgm2_BuildProcedureCall 	       	  PARAMS ((tree procedure, tree rettype));
       tree                   gccgm2_BuildIndirectProcedureCall 	  PARAMS ((tree procedure, tree rettype));
       void                   gccgm2_BuildFunctValue 	       	 	  PARAMS ((tree value));
       void                   gccgm2_BuildAsm 		       	 	  PARAMS ((tree instr, int IsVolatile, tree inputs,
									 	  tree outputs, tree trash));
       void                   gccgm2_AssignBooleanTrueFalse     	  PARAMS ((tree booleanid, tree trueid, tree falseid));
       tree                   gccgm2_GetIntegerType 	       	 	  PARAMS ((void));
       tree                   gccgm2_GetCardinalType 	       	 	  PARAMS ((void));
       tree                   gccgm2_GetCharType 	       	 	  PARAMS ((void));
       tree                   gccgm2_GetByteType 	       	 	  PARAMS ((void));
       tree                   gccgm2_GetLocType 	       	 	  PARAMS ((void));
       tree                   gccgm2_GetVoidType 	       	 	  PARAMS ((void));
       tree                   gccgm2_GetPointerType 	       	 	  PARAMS ((void));
       tree                   gccgm2_GetBitsetType 	       	 	  PARAMS ((void));
       tree                   gccgm2_GetBitnumType 	       	 	  PARAMS ((void));
       tree                   gccgm2_GetRealType 	       	 	  PARAMS ((void));
       tree                   gccgm2_GetLongRealType 	       	 	  PARAMS ((void));
       tree                   gccgm2_GetShortRealType 	       	 	  PARAMS ((void));
       tree                   gccgm2_GetLongIntType 	       	 	  PARAMS ((void));
       tree                   gccgm2_GetWordType 	       	 	  PARAMS ((void));
       tree                   gccgm2_GetProcType 	       	 	  PARAMS ((void));
       tree                   gccgm2_GetIntegerZero 	       	 	  PARAMS ((void));
       tree                   gccgm2_GetIntegerOne 	       	 	  PARAMS ((void));
       tree                   gccgm2_GetWordZero 	       	 	  PARAMS ((void));
       tree                   gccgm2_GetWordOne 	       	 	  PARAMS ((void));
       tree                   gccgm2_GetPointerZero	       	 	  PARAMS ((void));
       tree                   gccgm2_GetPointerOne 	       	 	  PARAMS ((void));
       tree                   gccgm2_ToWord                               PARAMS ((tree));
       tree                   gccgm2_GetCurrentFunction 	       	  PARAMS ((void));
       tree                   gccgm2_GetErrorNode 	       	 	  PARAMS ((void));
       tree                   build_int_2_type                            PARAMS ((int low, int hi, tree type));
       tree                   convertToPtr   		       	 	  PARAMS ((tree t));
       int                    gccgm2_AreConstantsEqual 	       	 	  PARAMS ((tree e1, tree e2));
       int                    gccgm2_DetermineSign 	       	 	  PARAMS ((tree e));
       tree                   gccgm2_BuildStringConstant        	  PARAMS ((char *string, int length));
       tree                   gccgm2_BuildCharConstant 	       	 	  PARAMS ((char *string));
       tree                   gccgm2_ConvertConstantAndCheck    	  PARAMS ((tree type, tree expr));
       tree                   gccgm2_RealToTree 		       	  PARAMS ((char *name));
       tree                   gccgm2_BuildStart 		       	  PARAMS ((char *name, int line, int inner_module));
       void                   gccgm2_BuildEnd 		       	 	  PARAMS ((tree fndecl));
       void                   gccgm2_BuildCallInnerInit 	       	  PARAMS ((tree fndecl));
       void                   gccgm2_BuildStartMainModule       	  PARAMS ((void));
       void                   gccgm2_BuildEndMainModule 	       	  PARAMS ((void));
       tree                   gccgm2_BuildCap                             PARAMS ((tree t));
       void                   gccgm2_DebugTree 		       	 	  PARAMS ((tree t));
       void                   gccgm2_DebugTreeChain                       PARAMS ((tree t));
       tree                   build_function_call 	       	 	  PARAMS ((tree function, tree params));
       tree                   start_enum     		       	 	  PARAMS ((tree name));
       void                   pushtag        		       	 	  PARAMS ((tree name, tree type));
       tree                   start_struct   		       	 	  PARAMS ((enum tree_code code, tree name));
       tree                   gccgm2_BuildStartRecord 	       	 	  PARAMS ((char *name));
       tree                   gccgm2_BuildStartVarientRecord    	  PARAMS ((char *name));
static void                   layout_array_type 		       	  PARAMS ((tree t));
       tree                   gccgm2_BuildFieldRecord 	       	 	  PARAMS ((char *name, tree type));
       tree                   gccgm2_ChainOn 		       	 	  PARAMS ((tree t1, tree t2));
       tree                   gccgm2_ChainOnParamValue 	       	 	  PARAMS ((tree list, tree parm, tree value));
       tree                   gccgm2_AddStringToTreeList        	  PARAMS ((tree list, tree string));
       tree                   gccgm2_BuildEndRecord 	       	 	  PARAMS ((tree t, tree fieldlist));
       tree                   start_enum     		       	 	  PARAMS ((tree name));
       unsigned int           min_precision          		       	  PARAMS ((tree value, int unsignedp));
       tree                   build_enumerator 		       	          PARAMS ((tree name, tree value));
       void                   gccgm2_ExpandExpressionStatement  	  PARAMS ((tree t));
       int                    is_of_string_type                           PARAMS ((tree type, int warn));
       tree                   gccgm2_BuildSize                            PARAMS ((tree op1, int  needconvert));
       tree                   gccgm2_BuildOffset                          PARAMS ((tree field, int needconvert));
       tree                   gccgm2_BuildIntegerConstant                 PARAMS ((int value));
       void                   gccgm2_BuildGoto                            PARAMS ((char *name));
       tree                   gccgm2_RememberConstant                     PARAMS ((tree t));
       tree                   global_constant                             PARAMS ((tree t));
       tree                   gccgm2_FoldAndStrip                         PARAMS ((tree t));
       void                   stop                                        PARAMS ((void));
static int                    is_a_constant                               PARAMS ((tree t));
static tree                   decl_constant_value_for_broken_optimization PARAMS ((tree decl));
       tree                   maybe_apply_renaming_pragma                 PARAMS ((tree decl, tree asmname));
static void                   internal_error_function                     PARAMS ((const char *msgid, va_list *ap));
static int                    function_types_compatible_p                 PARAMS ((tree f1, tree f2));
static int                    type_lists_compatible_p                     PARAMS ((tree args1, tree args2));
static tree                   build_bitset_type                           PARAMS ((void));
static tree                   build_m2_char_node                          PARAMS ((void));
static tree                   build_m2_integer_node                       PARAMS ((void));
static tree                   build_m2_cardinal_node                      PARAMS ((void));
static tree                   build_m2_short_real_node                    PARAMS ((void));
static tree                   build_m2_real_node                          PARAMS ((void));
static tree                   build_m2_long_real_node                     PARAMS ((void));
static tree                   build_m2_long_int_node                      PARAMS ((void));
static tree                   build_m2_long_card_node                     PARAMS ((void));
static tree                   build_m2_short_int_node                     PARAMS ((void));
static tree                   build_m2_short_card_node                     PARAMS ((void));
static tree                   build_m2_iso_loc_node                       PARAMS ((void));
static tree                   build_m2_iso_byte_node                      PARAMS ((void));
static tree                   build_m2_iso_word_node                      PARAMS ((void));
       tree                   convert_type_to_range                       PARAMS ((tree type));
       tree                   gccgm2_GetDefaultType                       PARAMS ((char *name, tree type));
       void                   gccgm2_BuildStartSetConstructor             PARAMS ((tree type));
       void                   gccgm2_BuildSetConstructorElement           PARAMS ((tree value));
       tree                   gccgm2_BuildEndSetConstructor               PARAMS ((void));
       tree                   gccgm2_GetM2CharType                        PARAMS ((void));
       tree                   gccgm2_GetM2IntegerType                     PARAMS ((void));
       tree                   gccgm2_GetM2ShortRealType                   PARAMS ((void));
       tree                   gccgm2_GetM2RealType                        PARAMS ((void));
       tree                   gccgm2_GetM2LongRealType                    PARAMS ((void));
       tree                   gccgm2_GetM2LongIntType                     PARAMS ((void));
       tree                   gccgm2_GetM2ShortIntType                    PARAMS ((void));
       tree                   gccgm2_GetShortIntType                      PARAMS ((void));
       tree                   gccgm2_GetM2ShortCardType                   PARAMS ((void));
       tree                   gccgm2_GetShortCardType                     PARAMS ((void));
       tree                   gccgm2_GetISOWordType                       PARAMS ((void));
       tree                   gccgm2_GetISOByteType                       PARAMS ((void));
       tree                   gccgm2_GetISOLocType                        PARAMS ((void));

       int                    gccgm2_CompareTrees                         PARAMS ((tree e1, tree e2));
static int                    is_type                                     PARAMS ((tree type));
       void                   gccgm2_BuildBinaryForeachWordDo             PARAMS ((tree, tree, tree, tree, tree (*binop)(tree, tree, int), int, int, int, int, int, int));
       void                   gccgm2_BuildUnaryForeachWordDo              PARAMS ((tree, tree, tree, tree (*unop)(tree, int), int, int, int, int));
static tree                   get_rvalue                                  PARAMS ((tree, tree, int));
static tree                   get_set_address                             PARAMS ((tree, int));
static tree                   get_set_field_lhs                           PARAMS ((tree, tree));
static tree                   get_set_field_rhs                           PARAMS ((tree, tree));
       void                   gccgm2_BuildExcludeVarConst                 PARAMS ((tree, tree, tree, int, int));
       void                   gccgm2_BuildIncludeVarConst                 PARAMS ((tree, tree, tree, int, int));
       void                   gccgm2_BuildExcludeVarVar                   PARAMS ((tree, tree, tree, int, tree));
       void                   gccgm2_BuildIncludeVarVar                   PARAMS ((tree, tree, tree, int, tree));
       void                   gccgm2_BuildIfConstInVar                    PARAMS ((tree, tree, tree, int, int, char *));
       void                   gccgm2_BuildIfNotConstInVar                 PARAMS ((tree, tree, tree, int, int, char *));
       void                   gccgm2_BuildIfVarInVar                      PARAMS ((tree, tree, tree, int, tree, tree, char *));
       void                   gccgm2_BuildIfNotVarInVar                   PARAMS ((tree, tree, tree, int, tree, tree, char *));
static void                   do_jump_if_bit                              PARAMS ((enum tree_code, tree, tree, char *));
#if 0
       void                   gccgm2_BuildIfConstSetEquVar                PARAMS ((tree, tree, tree, int, char *));
       void                   gccgm2_BuildIfNotConstSetEquVar             PARAMS ((tree, tree, tree, int, char *));
       void                   gccgm2_BuildIfVarEquVar                     PARAMS ((tree, tree, tree, int, int, char *));
       void                   gccgm2_BuildIfNotVarEquVar                  PARAMS ((tree, tree, tree, int, int, char *));
#endif
       void                   gccgm2_BuildIfInRangeGoto                   PARAMS ((tree, tree, tree, char *));
       void                   gccgm2_BuildIfNotInRangeGoto                PARAMS ((tree, tree, tree, char *));
       void                   gccgm2_BuildForeachWordInSetDoIfExpr        PARAMS ((tree, tree, tree, int, int, int, int, tree (*expr) (tree, tree), char *));
static tree                   get_set_address_if_var                      PARAMS ((tree, int, int));
static tree                   get_field_list                              PARAMS ((tree, tree, int));
static tree                   get_set_value                               PARAMS ((tree, tree, int));
       void                   gccgm2_InitFunctionTypeParameters           PARAMS ((int uses_varargs));
       tree                   gccgm2_GetM2CardinalType                    PARAMS ((void));
static tree                   get_tree_val                                PARAMS ((tree e));
static tree                   convert_arguments                           PARAMS ((tree, tree, tree, tree));
static tree                   default_function_array_conversion           PARAMS ((tree exp));
       void                   gccgm2_BuildPushFunctionContext             PARAMS ((void));
       void                   gccgm2_BuildPopFunctionContext              PARAMS ((void));
       void                   gccgm2_InitGlobalContext                    PARAMS ((void));
       tree                   gccgm2_GetM2LongCardType                    PARAMS ((void));
static bool                   flexible_array_type_p                       PARAMS ((tree type));
       int                    maybe_objc_comptypes                        PARAMS ((tree, tree, int));
// static void                   pop_binding_level                           PARAMS ((struct binding_level **lp));
       int                    gm2_tree_expr_nonnegative_p                 PARAMS ((tree));
       void                   gm2_incomplete_type_error                   PARAMS ((tree, tree));
       void                   declare_parm_level                          PARAMS ((int));
       tree                   c_type_promotes_to                          PARAMS ((tree));
       int                    objc_comptypes                              PARAMS ((tree, tree, int));
static tree                   build_set_full_complement                   PARAMS ((void));
       void                   gccgm2_GarbageCollect                       PARAMS ((void));
       tree                   gccgm2_BuildConstLiteralNumber              PARAMS ((const char *str, unsigned int base));
static int                    interpret_integer                           PARAMS ((const char *str, unsigned int base, unsigned HOST_WIDE_INT *low, HOST_WIDE_INT *high));
static int                    append_digit                                PARAMS ((unsigned HOST_WIDE_INT *low, HOST_WIDE_INT *high, unsigned int digit, unsigned int base));


#if defined(TRACE_DEBUG_GGC)
static void                   dump_binding_level                          PARAMS ((struct binding_level *level));
#endif

/* end of prototypes */

void stop (void) {}

tree
maybe_apply_renaming_pragma (decl, asmname)
     tree decl ATTRIBUTE_UNUSED, asmname;
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
gccgm2_SetFileNameAndLineNo (fn, line)
     char *fn;
     int   line;
{
  /* remember that both these variables are actually external to this file */
  lineno = line;
  if (cfun && fn)
    input_filename = ggc_strdup(fn);
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
  if (cfun && fn)
    emit_line_note (ggc_strdup (fn), line);
}

/* Routines Expected by gcc:  */

/* These are used to build types for various sizes.  The code below
   is a simplified version of that of GNAT.  */

#ifndef MAX_BITS_PER_WORD
#define MAX_BITS_PER_WORD  BITS_PER_WORD
#endif

/* Return an integer type with BITS bits of precision,
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed.  */

tree
gm2_type_for_size (bits, unsignedp)
     unsigned bits;
     int unsignedp;
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
gm2_type_for_mode (mode, unsignedp)
     enum machine_mode mode;
     int unsignedp;
{
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

  if (mode == TYPE_MODE (build_pointer_type (char_type_node)))
    return build_pointer_type (char_type_node);

  if (mode == TYPE_MODE (build_pointer_type (integer_type_node)))
    return build_pointer_type (integer_type_node);

  switch (mode)
    {
    case V16QImode:
      return unsignedp ? unsigned_V16QI_type_node : V16QI_type_node;
    case V8HImode:
      return unsignedp ? unsigned_V8HI_type_node : V8HI_type_node;
    case V4SImode:
      return unsignedp ? unsigned_V4SI_type_node : V4SI_type_node;
    case V2DImode:
      return unsignedp ? unsigned_V2DI_type_node : V2DI_type_node;
    case V2SImode:
      return unsignedp ? unsigned_V2SI_type_node : V2SI_type_node;
    case V2HImode:
      return unsignedp ? unsigned_V2HI_type_node : V2HI_type_node;
    case V4HImode:
      return unsignedp ? unsigned_V4HI_type_node : V4HI_type_node;
    case V8QImode:
      return unsignedp ? unsigned_V8QI_type_node : V8QI_type_node;
    case V1DImode:
      return unsignedp ? unsigned_V1DI_type_node : V1DI_type_node;
    case V16SFmode:
      return V16SF_type_node;
    case V4SFmode:
      return V4SF_type_node;
    case V2SFmode:
      return V2SF_type_node;
    case V2DFmode:
      return V2DF_type_node;
    default:
      break;
    }

  return 0;
}

/* Return an unsigned type the same as TYPE in other respects. */
tree
gm2_unsigned_type (type)
     tree type;
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
gm2_signed_type (type)
     tree type;
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
gm2_signed_or_unsigned_type (unsignedp, type)
     int unsignedp;
     tree type;
{
  if (! INTEGRAL_TYPE_P (type)
      || TREE_UNSIGNED (type) == unsignedp)
    return type;

  if (TYPE_PRECISION (type) == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (long_long_integer_type_node))
    return (unsignedp ? long_long_unsigned_type_node
	    : long_long_integer_type_node);
  if (TYPE_PRECISION (type) == TYPE_PRECISION (widest_integer_literal_type_node))
    return (unsignedp ? widest_unsigned_literal_type_node
	    : widest_integer_literal_type_node);

#if HOST_BITS_PER_WIDE_INT >= 64
  if (TYPE_PRECISION (type) == TYPE_PRECISION (intTI_type_node))
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;
#endif
  if (TYPE_PRECISION (type) == TYPE_PRECISION (intDI_type_node))
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (intSI_type_node))
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (intHI_type_node))
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (intQI_type_node))
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

  return type;
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

struct binding_level GTY(())
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

    /* List of decls in `names' that have incomplete structure or
       union types.  */
    tree incomplete_list;

    /* A list of constants (only kept in the global binding level).
       Constants need to be kept through the life of the compilation,
       as the same constants can be used in any scope. */
    tree constants;
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
  = {NULL, NULL, NULL, NULL, NULL, NULL_BINDING_LEVEL, 0, 0, 0, 0, 0, 0,
     NULL};

/* Nonzero means unconditionally make a BLOCK for the next level pushed.  */

static int keep_next_level_flag;

/* Nonzero means make a BLOCK for the next level pushed
   if it has subblocks.  */

static int keep_next_if_subblocks;

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
  struct c_lang_decl base;
  /* The return types and parameter types may have variable size.
     This is a list of any SAVE_EXPRs that need to be evaluated to
     compute those sizes.  */
  tree pending_sizes;
};

/* Macros for access to language-specific slots in an identifier.  */
/* Each of these slots contains a DECL node or null.  */

/* This represents the value which the identifier has in the
   file-scope namespace.  */
#define IDENTIFIER_GLOBAL_VALUE(NODE)	\
  (((struct lang_identifier *) (NODE))->global_value)
/* This represents the value which the identifier has in the current
   scope.  */
#define IDENTIFIER_LOCAL_VALUE(NODE)	\
  (((struct lang_identifier *) (NODE))->local_value)
/* This represents the value which the identifier has as a label in
   the current label scope.  */
#define IDENTIFIER_LABEL_VALUE(NODE)	\
  (((struct lang_identifier *) (NODE))->label_value)
/* This records the extern decl of this identifier, if it has had one
   at any point in this compilation.  */
#define IDENTIFIER_LIMBO_VALUE(NODE)	\
  (((struct lang_identifier *) (NODE))->limbo_value)
/* This records the implicit function decl of this identifier, if it
   has had one at any point in this compilation.  */
#define IDENTIFIER_IMPLICIT_DECL(NODE)	\
  (((struct lang_identifier *) (NODE))->implicit_decl)
/* This is the last function in which we printed an "undefined variable"
   message for this identifier.  Value is a FUNCTION_DECL or null.  */
#define IDENTIFIER_ERROR_LOCUS(NODE)	\
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

/* This function was declared inline.  This flag controls the linkage
   semantics of 'inline'; whether or not the function is inlined is
   controlled by DECL_INLINE.  */
#define DECL_DECLARED_INLINE_P(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->base.declared_inline)

/* In a RECORD_TYPE, a sorted array of the fields of the type.  */
struct lang_type GTY(())
{
  int len;
  tree GTY((length ("%h.len"))) elts[1];
};

/* Record whether a type or decl was written with nonconstant size.
   Note that TYPE_SIZE may have simplified to a constant.  */
#define C_TYPE_VARIABLE_SIZE(TYPE) TYPE_LANG_FLAG_1 (TYPE)
#define C_DECL_VARIABLE_SIZE(TYPE) DECL_LANG_FLAG_0 (TYPE)

#if 0 /* Not used.  */
/* Record whether a decl for a function or function pointer has
   already been mentioned (in a warning) because it was called
   but didn't have a prototype.  */
#define C_MISSING_PROTOTYPE_WARNED(DECL) DECL_LANG_FLAG_2 (DECL)
#endif

/* Store a value in that field.  */
#define C_SET_EXP_ORIGINAL_CODE(EXP, CODE) \
  (TREE_COMPLEXITY (EXP) = (int) (CODE))

/* Record whether a typedef for type `int' was actually `signed int'.  */
#define C_TYPEDEF_EXPLICITLY_SIGNED(EXP) DECL_LANG_FLAG_1 (EXP)

/* For a FUNCTION_DECL, nonzero if it was defined without an explicit
   return type.  */
#define C_FUNCTION_IMPLICIT_INT(EXP) DECL_LANG_FLAG_1 (EXP)

/* Nonzero for a declaration of a built in function if there has been no
   occasion that would declare the function in ordinary C.
   Using the function draws a pedantic warning in this case.  */
#define C_DECL_ANTICIPATED(EXP) DECL_LANG_FLAG_3 (EXP)

/* For FUNCTION_TYPE, a hidden list of types of arguments.  The same as
   TYPE_ARG_TYPES for functions with prototypes, but created for functions
   without prototypes.  */
#define TYPE_ACTUAL_ARG_TYPES(NODE) TYPE_BINFO (NODE)

/* FROM ../c-decl.c IMPORT */
/* Reuse or create a struct for this binding level.  */

static struct binding_level *
make_binding_level ()
{
  if (free_binding_level)
    {
      struct binding_level *result = free_binding_level;
      free_binding_level = result->level_chain;
      return result;
    }
  else
    return (struct binding_level *) ggc_alloc (sizeof (struct binding_level));
}

#define c_build_type_variant(TYPE, CONST_P, VOLATILE_P)		  \
  c_build_qualified_type ((TYPE),				  \
			  ((CONST_P) ? TYPE_QUAL_CONST : 0) |	  \
			  ((VOLATILE_P) ? TYPE_QUAL_VOLATILE : 0))

/* FROM ../c-decl.c IMPORT */
/* Remove a binding level from a list and add it to the level chain.  */

#if 0
static void
pop_binding_level (lp)
     struct binding_level **lp;
{
  struct binding_level *l = *lp;
  *lp = l->level_chain;
  
  memset (l, 0, sizeof (struct binding_level));
  l->level_chain = free_binding_level;
  free_binding_level = l;
}
#endif

/* FROM ../c-decl.c IMPORT */
/* Nonzero if we are currently in the global binding level.  */

int
global_bindings_p ()
{
  return current_binding_level == global_binding_level;
}

/* Return the list of declarations of the current level.
   Note that this list is in reverse order unless/until
   you nreverse it; and when you do nreverse it, you must
   store the result back using `storedecls' or you will lose.  */

tree
getdecls ()
{
  return current_binding_level->names;
}

/* Nonzero if the current level needs to have a BLOCK made.  */

int
kept_level_p ()
{
  return ((current_binding_level->keep_if_subblocks
	   && current_binding_level->blocks != 0)
	  || current_binding_level->keep
	  || current_binding_level->names != 0
	  || (current_binding_level->tags != 0
	      && !current_binding_level->tag_transparent));
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

/* Enter a new binding level.
   If TAG_TRANSPARENT is nonzero, do so only for the name space of variables,
   not for that of tags.  */

void
pushlevel (tag_transparent)
     int tag_transparent;
{
  struct binding_level *newlevel = NULL_BINDING_LEVEL;

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

/* Define a label, specifying the location in the source file.
   Return the LABEL_DECL node for the label, if the definition is valid.
   Otherwise return 0.  */

tree
define_label (filename, line, name)
     const char *filename;
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
#if !defined(GM2)
  if (warn_traditional && !in_system_header && lookup_name (name))
    warning_with_file_and_line (filename, line,
				"traditional C lacks a separate namespace for labels, identifier `%s' conflicts",
				IDENTIFIER_POINTER (name));
#endif

  if (DECL_INITIAL (decl) != 0)
    {
      error_with_file_and_line (filename, line, "duplicate label `%s'",
				IDENTIFIER_POINTER (name));
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
  tree link;
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
    if (!COMPLETE_TYPE_P (TREE_VALUE (link)))
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
      }

  /* We used to warn about unused variables in expand_end_bindings,
     i.e. while generating RTL.  But in function-at-a-time mode we may
     choose to never expand a function at all (e.g. auto inlining), so
     we do this explicitly now.  */
  warn_about_unused_variables (getdecls ());

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
	  tree label = TREE_VALUE (link);

	  if (DECL_INITIAL (label) == 0)
	    {
	      error_with_decl (label, "label `%s' used but not defined");
	      /* Avoid crashing later.  */
	      define_label (input_filename, lineno,
			    DECL_NAME (label));
	    }
	  else if (warn_unused_label && !TREE_USED (label))
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
    struct binding_level *level = current_binding_level;
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
     tagged types.  */

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

#if 0
/* Mark ARG for GC.  */

static void
mark_binding_level (arg)
     void *arg;
{
  struct binding_level *level = *(struct binding_level **) arg;

  for (; level != 0; level = level->level_chain)
    {
#if defined(TRACE_DEBUG_GGC)
      dump_binding_level (level);
#endif
      ggc_mark_tree (level->names);
      ggc_mark_tree (level->tags);
      ggc_mark_tree (level->shadowed);
      ggc_mark_tree (level->blocks);
      ggc_mark_tree (level->this_block);
      ggc_mark_tree (level->constants);
    }
}
#endif

#if defined(TRACE_DEBUG_GGC)
/*
 *  dump_binding_level - displays the contents of a binding level.
 */

static void
dump_binding_level (level)
     struct binding_level *level;
{
  if (level) {
    tree t;

    for (t = level->names; t; t = TREE_CHAIN (t))
      debug_tree (t);
    for (t = level->tags; t; t = TREE_CHAIN (t))
      debug_tree (t);
    for (t = level->shadowed; t; t = TREE_CHAIN (t))
      debug_tree (t);
    for (t = level->blocks; t; t = TREE_CHAIN (t))
      debug_tree (t);
    for (t = level->this_block; t; t = TREE_CHAIN (t))
      debug_tree (t);

  }
}
#endif

#if 1
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
  tree t;
  tree name = DECL_NAME (x);
  struct binding_level *b = current_binding_level;

  /* Functions need the lang_decl data.  */
  if (TREE_CODE (x) == FUNCTION_DECL && ! DECL_LANG_SPECIFIC (x))
    DECL_LANG_SPECIFIC (x) = (struct lang_decl *)
      ggc_alloc_cleared (sizeof (struct lang_decl));

  DECL_CONTEXT (x) = current_function_decl;
  /* A local extern declaration for a function doesn't constitute nesting.
     A local auto declaration does, since it's a forward decl
     for a nested function coming later.  */
  if ((TREE_CODE (x) == FUNCTION_DECL || TREE_CODE (x) == VAR_DECL)
      && DECL_INITIAL (x) == 0 && DECL_EXTERNAL (x))
    DECL_CONTEXT (x) = 0;

  if (name)
    {
      int different_binding_level = 0;

      if (warn_nested_externs
	  && DECL_EXTERNAL (x)
	  && b != global_binding_level
	  && x != IDENTIFIER_IMPLICIT_DECL (name)
	  /* No error messages for __FUNCTION__ and __PRETTY_FUNCTION__.  */
	  && !DECL_IN_SYSTEM_HEADER (x))
	warning ("nested extern declaration of `%s'",
		 IDENTIFIER_POINTER (name));

      t = lookup_name_current_level (name);
      if (! t && DECL_EXTERNAL (x) && TREE_PUBLIC (x))
	{
	  t = IDENTIFIER_GLOBAL_VALUE (name);
	  /* Type decls at global scope don't conflict with externs declared
	     inside lexical blocks.  */
	  if (! t || TREE_CODE (t) == TYPE_DECL)
	    /* If there's no visible global declaration, try for an
               invisible one.  */
	    t = IDENTIFIER_LIMBO_VALUE (name);
	  different_binding_level = 1;
	}
      if (t != 0 && t == error_mark_node)
	/* error_mark_node is 0 for a while during initialization!  */
	{
	  t = 0;
	  error_with_decl (x, "`%s' used prior to declaration");
	}

      /* If this decl is `static' and an implicit decl was seen previously,
	 warn.  */
      if (TREE_PUBLIC (name)
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

      /* testing removal of this if statement (gaius) */
#if 0
      
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
#endif

      /* If we are processing a typedef statement, generate a whole new
	 ..._TYPE node (which will be just a variant of the existing
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
	      TREE_USED (tt) = TREE_USED (x);
	      TREE_TYPE (x) = tt;
	    }
	}

      /* Multiple external decls of the same identifier ought to match.
	 We get warnings about inline functions where they are defined.
	 Avoid duplicate warnings where they are used.  */
      if (TREE_PUBLIC (x)
	  && ! (TREE_CODE (x) == FUNCTION_DECL && DECL_INLINE (x)))
	{
	  tree decl;

	  if (IDENTIFIER_LIMBO_VALUE (name) != 0)
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
	      && oldglobal != 0
	      && TREE_CODE (x) == FUNCTION_DECL
	      && TREE_CODE (oldglobal) == FUNCTION_DECL
	      && DECL_EXTERNAL (x)
	      && ! DECL_DECLARED_INLINE_P (x))
	    {
	      /* We have one.  Their types must agree.  */
	      if (! comptypes (TREE_TYPE (x),
			       TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (name))))
		pedwarn_with_decl (x, "extern declaration of `%s' doesn't match global one");
	      else
		{
		  /* Inner extern decl is inline if global one is.
		     Copy enough to really inline it.  */
		  if (DECL_DECLARED_INLINE_P (oldglobal))
		    {
		      DECL_DECLARED_INLINE_P (x)
		        = DECL_DECLARED_INLINE_P (oldglobal);
		      DECL_INLINE (x) = DECL_INLINE (oldglobal);
		      DECL_INITIAL (x) = (current_function_decl == oldglobal
					  ? 0 : DECL_INITIAL (oldglobal));
		      DECL_SAVED_INSNS (x) = DECL_SAVED_INSNS (oldglobal);
		      DECL_NUM_STMTS (x) = DECL_NUM_STMTS (oldglobal);
		      DECL_ARGUMENTS (x) = DECL_ARGUMENTS (oldglobal);
		      DECL_RESULT (x) = DECL_RESULT (oldglobal);
		      TREE_ASM_WRITTEN (x) = TREE_ASM_WRITTEN (oldglobal);
		      DECL_ABSTRACT_ORIGIN (x)
			= DECL_ABSTRACT_ORIGIN (oldglobal);
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

#if 0
	  /* This case is probably sometimes the right thing to do.  */
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
#if !defined(GM2)
	  warn_if_shadowing (x, oldlocal);
#endif
	  /* If storing a local value, there may already be one (inherited).
	     If so, record it for restoration when this binding level ends.  */
	  if (oldlocal != 0)
	    b->shadowed = tree_cons (name, oldlocal, b->shadowed);
	}

      /* Keep list of variables in this level with incomplete type.
	 If the input is erroneous, we can have error_mark in the type
	 slot (e.g. "f(void a, ...)") - that doesn't count as an
	 incomplete type.  */
      if (TREE_TYPE (x) != error_mark_node
	  && !COMPLETE_TYPE_P (TREE_TYPE (x)))
	{
	  tree element = TREE_TYPE (x);

	  while (TREE_CODE (element) == ARRAY_TYPE)
	    element = TREE_TYPE (element);
	  if (TREE_CODE (element) == RECORD_TYPE
	      || TREE_CODE (element) == UNION_TYPE)
	    b->incomplete_list = tree_cons (NULL_TREE, x, b->incomplete_list);
	}
    }

  /* Put decls on list in reverse order.
     We will reverse them later if necessary.  */
  TREE_CHAIN (x) = b->names;
  b->names = x;

  return x;
}
#endif

/* Like pushdecl, only it places X in GLOBAL_BINDING_LEVEL, if appropriate.  */

tree
pushdecl_top_level (x)
     tree x;
{
  tree t;
  struct binding_level *b = current_binding_level;

  current_binding_level = global_binding_level;
  t = pushdecl (x);
  current_binding_level = b;
  return t;
}

#if 0
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
#endif

/* Hook used by expand_expr to expand language-specific tree codes.  */

rtx
gm2_expand_expr (exp, target, tmode, modifier)
     tree exp;
     rtx target;
     enum machine_mode tmode ATTRIBUTE_UNUSED;
     int modifier ATTRIBUTE_UNUSED;  /* Actually enum_modifier.  */
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
      do_abort ();
      return target;
    }
}

/* init_m2_builtins - build tree nodes and builtin functions for GNU Modula-2
 */

void
init_m2_builtins ()
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
build_void_list_node ()
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
  TREE_UNSIGNED (c) = 0;

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
  TREE_UNSIGNED (c) = 1;

  return c;
}

static
tree
build_m2_char_node (void)
{
  tree c;

  /* Define `CHAR', which is like either `signed char' or `unsigned char'
     but not the same as either.  */

  /* Modula-2 CHAR type, borrowed from GNU Pascal */
  c = make_node (CHAR_TYPE);
  TYPE_PRECISION (c) = CHAR_TYPE_SIZE;
  TYPE_SIZE (c) = 0;

  if (flag_signed_char)
    {
      fixup_signed_type (c);
      TREE_UNSIGNED (c) = 0;
    }
  else
    {
      fixup_unsigned_type (c);
      TREE_UNSIGNED (c) = 1;
    }
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
  TREE_UNSIGNED (c) = 1;

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

  build_common_tree_nodes (flag_signed_char);

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

  set_sizetype (make_unsigned_type (POINTER_SIZE));

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

  ptrdiff_type_node
    = TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (get_identifier (PTRDIFF_TYPE)));
  unsigned_ptrdiff_type_node = gm2_unsigned_type (ptrdiff_type_node);

  pedantic_lvalues = pedantic;

  /* Record our roots.  */

#if 0
  ggc_add_tree_root (c_global_trees, CTI_MAX);
  ggc_add_tree_root (&named_labels, 1);
  ggc_add_tree_root (&shadowed_labels, 1);
  ggc_add_root (&current_binding_level, 1, sizeof current_binding_level,
		mark_binding_level);
#endif
  init_m2_builtins();
}

/*
 *  is_a_constant - returns TRUE if tree, t, is a constant.
 */

static int
is_a_constant (t)
     tree t;
{
  return((TREE_CODE (t) == INTEGER_CST) ||
	 (TREE_CODE (t) == REAL_CST) ||
	 (TREE_CODE (t) == REAL_CST) ||
	 (TREE_CODE (t) == COMPLEX_CST) ||   /* front end doesn't use COMPLEX_CST yet, here for completeness */
	 (TREE_CODE (t) == STRING_CST));
}

/*
 *  RememberConstant - adds a tree, t, onto the list of constants to be marked
 *                     whenever the ggc re-marks all used storage. Constants
 *                     live throughout the whole compilation - and they
 *                     can be used by many different functions if necessary.
 */

tree
gccgm2_RememberConstant (t)
     tree t;
{
  if ((t != NULL) && (is_a_constant(t))) {
    return global_constant(t);
  }
  return t;
}

/*
 *  global_constant - t is a constant, we keep a chain of all constants
 *                    in the global binding level.
 */

tree
global_constant (t)
  tree t;
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

tree
gccgm2_FoldAndStrip (t)
     tree t;
{
  if (t != NULL) {
    t = fold (t);
    STRIP_NOPS (t);
  }
  return t;
}

/* Decode all the language specific options that cannot be decoded by GCC. The
   option decoding phase of GCC calls this routine on the flags that it cannot
   decode.  Return 1 if successful, otherwise return 0. */

int
gm2_decode_option (argc, argv)
     int argc;
     char **argv ATTRIBUTE_UNUSED;
{
  if (argc == 0)
    return 0;

  if (insideCppArgs) {
    if (argv[0][0] == '-') {
      if (strcmp(argv[0], "-Wcppend") == 0)
	insideCppArgs = 0;
      return 1;
    } else
      return 0;
  } else if (strcmp(argv[0], "-Wreturn") == 0) {
    return 1;
  } else if (strcmp(argv[0], "-Wbounds") == 0) {
    return 1;
  } else if (strcmp(argv[0], "-Wnil") == 0) {
    return 1;
  } else if (strcmp(argv[0], "-Wcase") == 0) {
    return 1;
  } else if (strcmp(argv[0], "-Wcheck-all") == 0) {
    return 1;
  } else if (strcmp(argv[0], "-Wverbose-unbounded") == 0) {
    return 1;
  } else if (strcmp(argv[0], "-Wquiet") == 0) {
    return 1;
  } else if (strcmp(argv[0], "-Wcpp") == 0) {
    return 1;
  } else if (strcmp(argv[0], "-Wcppbegin") == 0) {
    insideCppArgs = 1;
    return 1;
  } else if (strcmp(argv[0], "-Wmakeall") == 0) {
    return 1;
  } else if (strcmp(argv[0], "-Wmakeall0") == 0) {
    return 1;
  } else if (strncmp(argv[0], "-Wmake-I=", 9) == 0) {
    return 1;
  } else if (strncmp(argv[0], "-gstabs", 7) == 0) {
    /* we just note that -gstabs is being used and unset this flag */
#if 0
    broken_set_debugging_info = FALSE;
#endif
    /* note we do not return 1, as this is not really a front end option */
  } else if (strcmp(argv[0], "-funbounded-by-reference") == 0) {
    return 1;
  }
  return 0;
}

/* Perform all the initialization steps that are language-specific.  */

/* Here we have the function to handle the compiler error processing in GCC.  */

static void
internal_error_function (msgid, ap)
     const char *msgid;
     va_list *ap;
{
  diagnostic_info diagnostic;

  diagnostic_set_info (&diagnostic, msgid, ap, input_filename, lineno,
                       DK_ICE);
  report_diagnostic (&diagnostic);

  fnotice (stderr, GM2_BUG_REPORT);
  exit (FATAL_EXIT_CODE);
}

/* Perform all the finalization steps that are language-specific.  */

const char *
gm2_init (filename)
     const char *filename;
{
  lineno = 0;
  gm2_init_decl_processing ();
  global_dc->internal_error = &internal_error_function;

  return filename;
}

#if 0
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
#endif

tree
gm2_truthvalue_conversion (expr)
     tree expr;
{ return expr;}

/* Print any language-specific compilation statistics.  */

void
print_lang_statistics ()
{}

int
maybe_objc_comptypes (lhs, rhs, reflexive)
     tree lhs ATTRIBUTE_UNUSED;
     tree rhs ATTRIBUTE_UNUSED;
     int reflexive ATTRIBUTE_UNUSED;
{
  return -1;
}

/* Return 1 if PARMS specifies a fixed number of parameters
   and none of their types is affected by default promotions.  */

int
self_promoting_args_p (parms)
     tree parms ATTRIBUTE_UNUSED;
{
  return 0;
}

/* Given a type, apply default promotions wrt unnamed function arguments
   and return the new type.  Return NULL_TREE if no change.  */
#if 0
tree
simple_type_promotes_to (type)
     tree type ATTRIBUTE_UNUSED;
{
  return NULL_TREE;
}

/* Since we don't use the DECL_LANG_SPECIFIC field, this is a no-op.  */

void
copy_lang_decl (node)
     tree node ATTRIBUTE_UNUSED;
{}
#endif

/* Performs whatever initialization steps are needed by the language-dependent
   lexical analyzer.  */

/* Routine to print parse error message.  */

void
yyerror (str)
     char *str;
{
  fprintf (stderr, "%s\n", str);
}

int
gm2_parse_file (void)
{
  gccgm2front(save_argc, save_argv);
  return 0;
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
    t1 = gm2_type_for_size (TYPE_PRECISION (t1), 1);
  if (TREE_CODE (t2) == ENUMERAL_TYPE)
    t2 = gm2_type_for_size (TYPE_PRECISION (t2), 1);

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
    t1 = gm2_type_for_size (TYPE_PRECISION (t1), TREE_UNSIGNED (t1));
  if (TREE_CODE (t2) == ENUMERAL_TYPE)
    t2 = gm2_type_for_size (TYPE_PRECISION (t2), TREE_UNSIGNED (t2));

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

/* FROM ../c-typeck.c IMPORT */

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
c_type_promotes_to (type)
     tree type;
{
  if (TYPE_MAIN_VARIANT (type) == float_type_node)
    return double_type_node;

  if (c_promoting_integer_type_p (type))
    {
      /* Preserve unsignedness if not really getting any wider.  */
      if (TREE_UNSIGNED (type)
          && (TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node)))
        return unsigned_type_node;
      return integer_type_node;
    }

  return type;
}

/* FROM ../c-typeck.c IMPORT */

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
readonly_warning (arg, string)
     tree arg;
     const char *string;
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

/* Mark EXP saying that we need to be able to take the
   address of it; it should not be allocated in a register.
   Returns true if successful.  */

bool
gm2_mark_addressable (exp)
     tree exp;
{
  tree x = exp;

  while (1)
    switch (TREE_CODE (x))
      {
      case COMPONENT_REF:
	if (DECL_C_BIT_FIELD (TREE_OPERAND (x, 1)))
	  {
	    error ("cannot take address of bit-field `%s'",
		   IDENTIFIER_POINTER (DECL_NAME (TREE_OPERAND (x, 1))));
	    return false;
	  }

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
		return false;
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
		return false;
	      }

	    /* If we are making this addressable due to its having
	       volatile components, give a different error message.  Also
	       handle the case of an unnamed parameter by not trying
	       to give the name.  */

	    else if (C_TYPE_FIELDS_VOLATILE (TREE_TYPE (x)))
	      {
		error ("cannot put object with volatile field into register");
		return false;
	      }

	    pedwarn ("address of register variable `%s' requested",
		     IDENTIFIER_POINTER (DECL_NAME (x)));
	  }
	put_var_into_stack (x, /*rescan=*/true);

	/* drops in */
      case FUNCTION_DECL:
	TREE_ADDRESSABLE (x) = 1;
#if 0  /* poplevel deals with this now.  */
	if (DECL_CONTEXT (x) == 0)
	  TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (x)) = 1;
#endif

      default:
	return true;
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

tree
decl_constant_value (decl)
     tree decl;
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
   Lvalues can have their address taken, unless they have DECL_REGISTER.  */

int
lvalue_p (ref)
     tree ref;
{
  enum tree_code code = TREE_CODE (ref);

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
   but which we can accept as lvalues.  If FLAG is nonzero, then
   non-lvalues are OK since we may be converting a non-lvalue array to
   a pointer in C99.

   If ARG is not a kind of expression we can handle, return zero.  */
   
static tree
unary_complex_lvalue (code, arg, flag)
     enum tree_code code;
     tree arg;
     int flag;
{
  /* Handle (a, b) used as an "lvalue".  */
  if (TREE_CODE (arg) == COMPOUND_EXPR)
    {
      tree real_result = build_unary_op (code, TREE_OPERAND (arg, 1), 0);

      /* If this returns a function type, it isn't really being used as
	 an lvalue, so don't issue a warning about it.  */
      if (TREE_CODE (TREE_TYPE (arg)) != FUNCTION_TYPE && !flag)
	pedantic_lvalue_warning (COMPOUND_EXPR);

      return build (COMPOUND_EXPR, TREE_TYPE (real_result),
		    TREE_OPERAND (arg, 0), real_result);
    }

  /* Handle (a ? b : c) used as an "lvalue".  */
  if (TREE_CODE (arg) == COND_EXPR)
    {
      if (!flag)
	pedantic_lvalue_warning (COND_EXPR);
      if (TREE_CODE (TREE_TYPE (arg)) != FUNCTION_TYPE && !flag)
	pedantic_lvalue_warning (COMPOUND_EXPR);

      return (build_conditional_expr
	      (TREE_OPERAND (arg, 0),
	       build_unary_op (code, TREE_OPERAND (arg, 1), flag),
	       build_unary_op (code, TREE_OPERAND (arg, 2), flag)));
    }

  return 0;
}

/* Nonzero if the type T promotes to int.  This is (nearly) the
   integral promotions defined in ISO C99 6.3.1.1/2.  */

bool
c_promoting_integer_type_p (t)
     tree t;
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
decl_constant_value_for_broken_optimization (decl)
     tree decl;
{
  if (pedantic || DECL_MODE (decl) == BLKmode)
    return decl;
  else
    return decl_constant_value (decl);
}

/* Perform default promotions for C data used in expressions.
   Arrays and functions are converted to pointers;
   enumeral types or short or char, to int.
   In addition, manifest constants symbols are replaced by their values.  */

tree
default_conversion (exp)
     tree exp;
{
  tree orig_exp;
  tree type = TREE_TYPE (exp);
  enum tree_code code = TREE_CODE (type);

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

  /* Strip NON_LVALUE_EXPRs and no-op conversions, since we aren't using as
     an lvalue. 

     Do not use STRIP_NOPS here!  It will remove conversions from pointer
     to integer and cause infinite recursion.  */
  orig_exp = exp;
  while (TREE_CODE (exp) == NON_LVALUE_EXPR
	 || (TREE_CODE (exp) == NOP_EXPR
	     && TREE_TYPE (TREE_OPERAND (exp, 0)) == TREE_TYPE (exp)))
    exp = TREE_OPERAND (exp, 0);

  /* Preserve the original expression code.  */
  if (IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (TREE_CODE (exp))))
    C_SET_EXP_ORIGINAL_CODE (exp, C_EXP_ORIGINAL_CODE (orig_exp));

  /* Normally convert enums to int,
     but convert wide enums to something wider.  */
  if (code == ENUMERAL_TYPE)
    {
      type = gm2_type_for_size (MAX (TYPE_PRECISION (type),
				     TYPE_PRECISION (integer_type_node)),
				((TYPE_PRECISION (type)
				  >= TYPE_PRECISION (integer_type_node))
				 && TREE_UNSIGNED (type)));

      return convert (type, exp);
    }

#if 0
  if (base_type (type) == m2_char_type_node)
    return convert (char_type_node, exp);
  if (base_type (type) == m2_integer_type_node)
    return convert (integer_type_node, exp);
  if (base_type (type) == m2_cardinal_type_node)
    return convert (unsigned_type_node, exp);
#endif

  if (TREE_CODE (exp) == COMPONENT_REF
      && DECL_C_BIT_FIELD (TREE_OPERAND (exp, 1))
      /* If it's thinner than an int, promote it like a
	 c_promoting_integer_type_p, otherwise leave it alone.  */
      && 0 > compare_tree_int (DECL_SIZE (TREE_OPERAND (exp, 1)),
			       TYPE_PRECISION (integer_type_node)))
    return convert (integer_type_node, exp);

  if (c_promoting_integer_type_p (type))
    {
      /* Preserve unsignedness if not really getting any wider.  */
      if (TREE_UNSIGNED (type)
	  && TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node))
	return convert (unsigned_type_node, exp);

      return convert (integer_type_node, exp);
    }

  if (code == VOID_TYPE)
    {
      error ("void value not ignored as it ought to be");
      return error_mark_node;
    }
  return exp;
}

/* Return 1 if TTL and TTR are pointers to types that are equivalent,
   ignoring their qualifiers.  REFLEXIVE is only used by ObjC - set it
   to 1 or 0 depending if the check of the pointer types is meant to
   be reflexive or not (typically, assignments are not reflexive,
   while comparisons are reflexive).
*/

static int
comp_target_types (ttl, ttr, reflexive)
     tree ttl, ttr;
     int reflexive;
{
  int val;

  /* Give objc_comptypes a crack at letting these types through.  */
  if ((val = objc_comptypes (ttl, ttr, reflexive)) >= 0)
    return val;

  val = comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (ttl)),
		   TYPE_MAIN_VARIANT (TREE_TYPE (ttr)));

  if (val == 2 && pedantic)
    pedwarn ("types are not quite compatible");
  return val;
}



/*
 *  convert_set - returns NULL if no set was found,
 *                otherwise convert set to type.
 */

tree
convert_set (type, expr)
     tree type, expr;
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
						   TREE_UNSIGNED (type)),
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
convert (type, expr)
     tree type, expr;
{
  tree e = expr;
  enum tree_code code = TREE_CODE (type);

  if (type == TREE_TYPE (expr)
      || TREE_CODE (expr) == ERROR_MARK
      || code == ERROR_MARK || TREE_CODE (TREE_TYPE (expr)) == ERROR_MARK)
    return expr;

  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (expr)))
    return fold (build1 (NOP_EXPR, type, expr));
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
	return fold (build1 (NOP_EXPR, type, TREE_OPERAND (t, 0)));
      else
	return fold (build1 (NOP_EXPR, type, t));
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

/* Print a warning using MSGID.
   It gets OPNAME as its one parameter.
   If OPNAME is null, it is replaced by "passing arg ARGNUM of `FUNCTION'".
   FUNCTION and ARGNUM are handled specially if we are building an
   Objective-C selector.  */

static void
warn_for_assignment (msgid, opname, function, argnum)
     const char *msgid;
     const char *opname;
     tree function;
     int argnum;
{
  if (opname == 0)
    {
#if NOT_NEEDED_FOR_M2
      tree selector = maybe_building_objc_message_expr ();
      char * new_opname;
      
      if (selector && argnum > 2)
	{
	  function = selector;
	  argnum -= 2;
	}
#else
      char * new_opname;
#endif
      if (function)
	{
	  /* Function name is known; supply it.  */
	  const char *argstring = _("passing arg %d of `%s'");
	  new_opname = (char *) alloca (IDENTIFIER_LENGTH (function)
					+ strlen (argstring) + 1 + 25
					/*%d*/ + 1);
	  sprintf (new_opname, argstring, argnum,
		   IDENTIFIER_POINTER (function));
	}
      else
	{
	  /* Function name unknown (call through ptr); just give arg number.*/
	  const char *argnofun = _("passing arg %d of pointer to function");
	  new_opname = (char *) alloca (strlen (argnofun) + 1 + 25 /*%d*/ + 1);
	  sprintf (new_opname, argnofun, argnum);
	}
      opname = new_opname;
    }
  pedwarn (msgid, opname);
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

/* FROM ../c-common.c IMPORT */

/* Print a warning if a large constant is truncated to unsigned,
   or if -Wconversion is used and a constant < 0 is converted to unsigned.
   Invoke this function on every expression that might be implicitly
   converted to an unsigned type.  */

void
unsigned_conversion_warning (result, operand)
     tree result, operand;
{
  tree type = TREE_TYPE (result);

  if (TREE_CODE (operand) == INTEGER_CST
      && TREE_CODE (type) == INTEGER_TYPE
      && TREE_UNSIGNED (type)
      && skip_evaluation == 0
      && !int_fits_type_p (operand, type))
    {
      if (!int_fits_type_p (operand, gm2_signed_type (type)))
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
                 || ! int_fits_type_p (expr, gm2_unsigned_type (type)))
                && skip_evaluation == 0)
                warning ("overflow in implicit constant conversion");
        }
      else
        unsigned_conversion_warning (t, expr);
    }
  return t;
}

/* FROM ../c-typeck.c IMPORT */
/* Return true if `t' is known to be non-negative.  */

int
gm2_tree_expr_nonnegative_p (t)
     tree t;
{
  if (TREE_CODE (t) == STMT_EXPR)
    {
      t=COMPOUND_BODY (STMT_EXPR_STMT (t));

      /* Find the last statement in the chain, ignoring the final
	     * scope statement */
      while (TREE_CHAIN (t) != NULL_TREE 
             && TREE_CODE (TREE_CHAIN (t)) != SCOPE_STMT)
        t=TREE_CHAIN (t);
      return tree_expr_nonnegative_p (TREE_OPERAND (t, 0));
    }
  return tree_expr_nonnegative_p (t);
}

/* FROM ../c-typeck.c IMPORT */

/* Build and return a conditional expression IFEXP ? OP1 : OP2.  */

tree
build_conditional_expr (ifexp, op1, op2)
     tree ifexp, op1, op2;
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
      if ((warn_sign_compare < 0 ? extra_warnings : warn_sign_compare)
	  && !skip_evaluation)
	{
	  int unsigned_op1 = TREE_UNSIGNED (TREE_TYPE (orig_op1));
	  int unsigned_op2 = TREE_UNSIGNED (TREE_TYPE (orig_op2));

	  if (unsigned_op1 ^ unsigned_op2)
	    {
	      /* Do not warn if the result type is signed, since the
		 signed type will only be chosen if it can represent
		 all the values of the unsigned type.  */
	      if (! TREE_UNSIGNED (result_type))
		/* OK */;
	      /* Do not warn if the signed quantity is an unsuffixed
		 integer literal (or some static constant expression
		 involving such literals) and it is non-negative.  */
	      else if ((unsigned_op2 && gm2_tree_expr_nonnegative_p (op1))
		       || (unsigned_op1 && gm2_tree_expr_nonnegative_p (op2)))
		/* OK */;
	      else
		warning ("signed and unsigned type in conditional expression");
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
      if (comp_target_types (type1, type2, 1))
	result_type = common_type (type1, type2);
      else if (integer_zerop (op1) && TREE_TYPE (type1) == void_type_node
	       && TREE_CODE (orig_op1) != NOP_EXPR)
	result_type = qualify_type (type2, type1);
      else if (integer_zerop (op2) && TREE_TYPE (type2) == void_type_node
	       && TREE_CODE (orig_op2) != NOP_EXPR)
	result_type = qualify_type (type1, type2);
      else if (VOID_TYPE_P (TREE_TYPE (type1)))
	{
	  if (pedantic && TREE_CODE (TREE_TYPE (type2)) == FUNCTION_TYPE)
	    pedwarn ("ISO C forbids conditional expr between `void *' and function pointer");
	  result_type = build_pointer_type (qualify_type (TREE_TYPE (type1),
							  TREE_TYPE (type2)));
	}
      else if (VOID_TYPE_P (TREE_TYPE (type2)))
	{
	  if (pedantic && TREE_CODE (TREE_TYPE (type1)) == FUNCTION_TYPE)
	    pedwarn ("ISO C forbids conditional expr between `void *' and function pointer");
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
      if (! integer_zerop (op2))
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
      int unsignedp = TREE_UNSIGNED (*restype_ptr);
      tree val;

      type = gm2_signed_or_unsigned_type (unsignedp0,
					  TREE_TYPE (primop0));

      /* If TYPE is an enumeration, then we need to get its min/max
	 values from it's underlying integral type, not the enumerated
	 type itself.  */
      if (TREE_CODE (type) == ENUMERAL_TYPE)
	type = gm2_type_for_size (TYPE_PRECISION (type), unsignedp0);

      maxval = TYPE_MAX_VALUE (type);
      minval = TYPE_MIN_VALUE (type);

      if (unsignedp && !unsignedp0)
	*restype_ptr = gm2_signed_type (*restype_ptr);

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
	  type = gm2_unsigned_type (type);
	}

      if (TREE_CODE (primop0) != INTEGER_CST)
	{
	  if (val == boolean_false_node)
	    warning ("comparison is always false due to limited range of data type");
	  if (val == boolean_true_node)
	    warning ("comparison is always true due to limited range of data type");
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
      type = gm2_signed_or_unsigned_type (unsignedp0
					  || TREE_UNSIGNED (*restype_ptr),
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
	      if (extra_warnings && !in_system_header
		  && ! (TREE_CODE (primop0) == INTEGER_CST
			&& ! TREE_OVERFLOW (convert (gm2_signed_type (type),
						     primop0))))
		warning ("comparison of unsigned expression >= 0 is always true");
	      value = boolean_true_node;
	      break;

	    case LT_EXPR:
	      if (extra_warnings && !in_system_header
		  && ! (TREE_CODE (primop0) == INTEGER_CST
			&& ! TREE_OVERFLOW (convert (gm2_signed_type (type),
						     primop0))))
		warning ("comparison of unsigned expression < 0 is always false");
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

/* Print an error message for invalid use of an incomplete type.
   VALUE is the expression that was used (or 0 if that isn't known)
   and TYPE is the type that was invalid.  */

void
gm2_incomplete_type_error (value, type)
     tree value;
     tree type;
{
  const char *type_code_string;

  /* Avoid duplicate error message.  */
  if (TREE_CODE (type) == ERROR_MARK)
    return;

  if (value != 0 && (TREE_CODE (value) == VAR_DECL
		     || TREE_CODE (value) == PARM_DECL))
    error ("`%s' has an incomplete type",
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
require_complete_type (value)
     tree value;
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
   The real work of conversion is done by `convert'.
   The purpose of this function is to generate error messages
   for assignments that are not allowed in C.
   ERRTYPE is a string to use in error messages:
   "assignment", "return", etc.  If it is null, this is parameter passing
   for a function call (and different error messages are output).

   FUNNAME is the name of the function being called,
   as an IDENTIFIER_NODE, or null.
   PARMNUM is the number of the argument, for printing in error messages.  */

static tree
convert_for_assignment (type, rhs, errtype, fundecl, funname, parmnum)
     tree type, rhs;
     const char *errtype;
     tree fundecl, funname;
     int parmnum;
{
  enum tree_code codel = TREE_CODE (type);
  tree rhstype;
  enum tree_code coder;

  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
  /* Do not use STRIP_NOPS here.  We do not want an enumerator
     whose value is 0 to count as a null pointer constant.  */
  if (TREE_CODE (rhs) == NON_LVALUE_EXPR)
    rhs = TREE_OPERAND (rhs, 0);

  if (TREE_CODE (TREE_TYPE (rhs)) == ARRAY_TYPE
      || TREE_CODE (TREE_TYPE (rhs)) == FUNCTION_TYPE)
    rhs = default_conversion (rhs);
  else if (optimize && TREE_CODE (rhs) == VAR_DECL)
    rhs = decl_constant_value_for_broken_optimization (rhs);

  rhstype = TREE_TYPE (rhs);
  coder = TREE_CODE (rhstype);

  if (coder == ERROR_MARK)
    return error_mark_node;

  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (rhstype))
    {
      overflow_warning (rhs);
      /* Check for Objective-C protocols.  This will automatically
	 issue a warning if there are protocol violations.  No need to
	 use the return value.  */
      if (flag_objc)
	objc_comptypes (type, rhstype, 0);
      return rhs;
    }

  if (coder == VOID_TYPE)
    {
      error ("void value not ignored as it ought to be");
      return error_mark_node;
    }
  /* A type converts to a reference to it.  
     This code doesn't fully support references, it's just for the
     special case of va_start and va_copy.  */
  if (codel == REFERENCE_TYPE
      && comptypes (TREE_TYPE (type), TREE_TYPE (rhs)) == 1)
    {
      if (!lvalue_p (rhs))
	{
	  error ("cannot pass rvalue to reference parameter");
	  return error_mark_node;
	}
      if (!gm2_mark_addressable (rhs))
	return error_mark_node;
      rhs = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (rhs)), rhs);

      /* We already know that these two types are compatible, but they
	 may not be exactly identical.  In fact, `TREE_TYPE (type)' is
	 likely to be __builtin_va_list and `TREE_TYPE (rhs)' is
	 likely to be va_list, a typedef to __builtin_va_list, which
	 is different enough that it will cause problems later.  */
      if (TREE_TYPE (TREE_TYPE (rhs)) != TREE_TYPE (type))
	rhs = build1 (NOP_EXPR, build_pointer_type (TREE_TYPE (type)), rhs);

      rhs = build1 (NOP_EXPR, type, rhs);
      return rhs;
    }
  /* Arithmetic types all interconvert, and enum is treated like int.  */
  else if ((codel == INTEGER_TYPE || codel == REAL_TYPE 
	    || codel == ENUMERAL_TYPE || codel == COMPLEX_TYPE
	    || codel == BOOLEAN_TYPE)
	   && (coder == INTEGER_TYPE || coder == REAL_TYPE
	       || coder == ENUMERAL_TYPE || coder == COMPLEX_TYPE
	       || coder == BOOLEAN_TYPE))
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
	      tree ttl = TREE_TYPE (memb_type);
	      tree ttr = TREE_TYPE (rhstype);

	      /* Any non-function converts to a [const][volatile] void *
		 and vice versa; otherwise, targets must be the same.
		 Meanwhile, the lhs target must have all the qualifiers of
		 the rhs.  */
	      if (VOID_TYPE_P (ttl) || VOID_TYPE_P (ttr)
		  || comp_target_types (memb_type, rhstype, 0))
		{
		  /* If this type won't generate any warnings, use it.  */
		  if (TYPE_QUALS (ttl) == TYPE_QUALS (ttr)
		      || ((TREE_CODE (ttr) == FUNCTION_TYPE
			   && TREE_CODE (ttl) == FUNCTION_TYPE)
			  ? ((TYPE_QUALS (ttl) | TYPE_QUALS (ttr))
			     == TYPE_QUALS (ttr))
			  : ((TYPE_QUALS (ttl) | TYPE_QUALS (ttr))
			     == TYPE_QUALS (ttl))))
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
	      tree ttl = TREE_TYPE (marginal_memb_type);
	      tree ttr = TREE_TYPE (rhstype);

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
		  if (TYPE_QUALS (ttl) & ~TYPE_QUALS (ttr))
		    warn_for_assignment ("%s makes qualified function pointer from unqualified",
					 errtype, funname, parmnum);
		}
	      else if (TYPE_QUALS (ttr) & ~TYPE_QUALS (ttl))
		warn_for_assignment ("%s discards qualifiers from pointer target type",
				     errtype, funname,
				     parmnum);
	    }
	  
	  if (pedantic && ! DECL_IN_SYSTEM_HEADER (fundecl))
	    pedwarn ("ISO C prohibits argument conversion to union type");

	  return build1 (NOP_EXPR, type, rhs);
	}
    }

  /* Conversions among pointers */
  else if ((codel == POINTER_TYPE || codel == REFERENCE_TYPE)
	   && (coder == codel))
    {
      tree ttl = TREE_TYPE (type);
      tree ttr = TREE_TYPE (rhstype);

      /* Any non-function converts to a [const][volatile] void *
	 and vice versa; otherwise, targets must be the same.
	 Meanwhile, the lhs target must have all the qualifiers of the rhs.  */
      if (VOID_TYPE_P (ttl) || VOID_TYPE_P (ttr)
	  || comp_target_types (type, rhstype, 0)
	  || (gm2_unsigned_type (TYPE_MAIN_VARIANT (ttl))
	      == gm2_unsigned_type (TYPE_MAIN_VARIANT (ttr))))
	{
	  if (pedantic
	      && ((VOID_TYPE_P (ttl) && TREE_CODE (ttr) == FUNCTION_TYPE)
		  ||
		  (VOID_TYPE_P (ttr)
		   /* Check TREE_CODE to catch cases like (void *) (char *) 0
		      which are not ANSI null ptr constants.  */
		   && (!integer_zerop (rhs) || TREE_CODE (rhs) == NOP_EXPR)
		   && TREE_CODE (ttl) == FUNCTION_TYPE)))
	    warn_for_assignment ("ISO C forbids %s between function pointer and `void *'",
				 errtype, funname, parmnum);
	  /* Const and volatile mean something different for function types,
	     so the usual warnings are not appropriate.  */
	  else if (TREE_CODE (ttr) != FUNCTION_TYPE
		   && TREE_CODE (ttl) != FUNCTION_TYPE)
	    {
	      if (TYPE_QUALS (ttr) & ~TYPE_QUALS (ttl))
		warn_for_assignment ("%s discards qualifiers from pointer target type",
				     errtype, funname, parmnum);
	      /* If this is not a case of ignoring a mismatch in signedness,
		 no warning.  */
	      else if (VOID_TYPE_P (ttl) || VOID_TYPE_P (ttr)
		       || comp_target_types (type, rhstype, 0))
		;
	      /* If there is a mismatch, do warn.  */
	      else if (pedantic)
		warn_for_assignment ("pointer targets in %s differ in signedness",
				     errtype, funname, parmnum);
	    }
	  else if (TREE_CODE (ttl) == FUNCTION_TYPE
		   && TREE_CODE (ttr) == FUNCTION_TYPE)
	    {
	      /* Because const and volatile on functions are restrictions
		 that say the function will not do certain things,
		 it is okay to use a const or volatile function
		 where an ordinary one is wanted, but not vice-versa.  */
	      if (TYPE_QUALS (ttl) & ~TYPE_QUALS (ttr))
		warn_for_assignment ("%s makes qualified function pointer from unqualified",
				     errtype, funname, parmnum);
	    }
	}
      else
	warn_for_assignment ("%s from incompatible pointer type",
			     errtype, funname, parmnum);
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
			       errtype, funname, parmnum);
	  return convert (type, rhs);
	}
      return null_pointer_node;
    }
  else if (codel == INTEGER_TYPE && coder == POINTER_TYPE)
    {
      warn_for_assignment ("%s makes integer from pointer without a cast",
			   errtype, funname, parmnum);
      return convert (type, rhs);
    }
  else if (codel == BOOLEAN_TYPE && coder == POINTER_TYPE)
    return convert (type, rhs);

  if (!errtype)
    {
      if (funname)
 	{
#if !defined(GM2)
 	  tree selector = objc_message_selector ();
 
 	  if (selector && parmnum > 2)
 	    error ("incompatible type for argument %d of `%s'",
		   parmnum - 2, IDENTIFIER_POINTER (selector));
 	  else
	    error ("incompatible type for argument %d of `%s'",
		   parmnum, IDENTIFIER_POINTER (funname));
#endif
	}
      else
	error ("incompatible type for argument %d of indirect function call",
	       parmnum);
    }
  else
    error ("incompatible types in %s", errtype);

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

/* Make a variant type in the proper way for C/C++, propagating qualifiers
   down to the element type of an array.  */

tree
c_build_qualified_type (type, type_quals)
     tree type;
     int type_quals;
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
boolean_increment (code, arg)
     enum tree_code code;
     tree arg;
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

/* Prepare expr to be an argument of a TRUTH_NOT_EXPR,
   or validate its data type for an `if' or `while' statement or ?..: exp.

   This preparation consists of taking the ordinary
   representation of an expression expr and producing a valid tree
   boolean expression describing whether expr is nonzero.  We could
   simply always do build_binary_op (NE_EXPR, expr, boolean_false_node, 1),
   but we optimize comparisons, &&, ||, and !.

   The resulting type should always be `boolean_type_node'.  */

tree
c_common_truthvalue_conversion (expr)
     tree expr;
{
  if (TREE_CODE (expr) == ERROR_MARK)
    return expr;

#if 0 /* This appears to be wrong for C++.  */
  /* These really should return error_mark_node after 2.4 is stable.
     But not all callers handle ERROR_MARK properly.  */
  switch (TREE_CODE (TREE_TYPE (expr)))
    {
    case RECORD_TYPE:
      error ("struct type value used where scalar is required");
      return boolean_false_node;

    case UNION_TYPE:
      error ("union type value used where scalar is required");
      return boolean_false_node;

    case ARRAY_TYPE:
      error ("array type value used where scalar is required");
      return boolean_false_node;

    default:
      break;
    }
#endif /* 0 */

  switch (TREE_CODE (expr))
    {
    case EQ_EXPR:
    case NE_EXPR: case LE_EXPR: case GE_EXPR: case LT_EXPR: case GT_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case TRUTH_NOT_EXPR:
      TREE_TYPE (expr) = boolean_type_node;
      return expr;

    case ERROR_MARK:
      return expr;

    case INTEGER_CST:
      return integer_zerop (expr) ? boolean_false_node : boolean_true_node;

    case REAL_CST:
      return real_zerop (expr) ? boolean_false_node : boolean_true_node;

    case ADDR_EXPR:
      /* If we are taking the address of an external decl, it might be zero
	 if it is weak, so we cannot optimize.  */
      if (DECL_P (TREE_OPERAND (expr, 0))
	  && DECL_EXTERNAL (TREE_OPERAND (expr, 0)))
	break;

      if (TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 0)))
	return build (COMPOUND_EXPR, boolean_type_node,
		      TREE_OPERAND (expr, 0), boolean_true_node);
      else
	return boolean_true_node;

    case COMPLEX_EXPR:
      return build_binary_op ((TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 1))
			       ? TRUTH_OR_EXPR : TRUTH_ORIF_EXPR),
		c_common_truthvalue_conversion (TREE_OPERAND (expr, 0)),
		c_common_truthvalue_conversion (TREE_OPERAND (expr, 1)),
			      0);

    case NEGATE_EXPR:
    case ABS_EXPR:
    case FLOAT_EXPR:
    case FFS_EXPR:
      /* These don't change whether an object is nonzero or zero.  */
      return c_common_truthvalue_conversion (TREE_OPERAND (expr, 0));

    case LROTATE_EXPR:
    case RROTATE_EXPR:
      /* These don't change whether an object is zero or nonzero, but
	 we can't ignore them if their second arg has side-effects.  */
      if (TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 1)))
	return build (COMPOUND_EXPR, boolean_type_node, TREE_OPERAND (expr, 1),
		      c_common_truthvalue_conversion (TREE_OPERAND (expr, 0)));
      else
	return c_common_truthvalue_conversion (TREE_OPERAND (expr, 0));

    case COND_EXPR:
      /* Distribute the conversion into the arms of a COND_EXPR.  */
      return fold (build (COND_EXPR, boolean_type_node, TREE_OPERAND (expr, 0),
		c_common_truthvalue_conversion (TREE_OPERAND (expr, 1)),
		c_common_truthvalue_conversion (TREE_OPERAND (expr, 2))));

    case CONVERT_EXPR:
      /* Don't cancel the effect of a CONVERT_EXPR from a REFERENCE_TYPE,
	 since that affects how `default_conversion' will behave.  */
      if (TREE_CODE (TREE_TYPE (expr)) == REFERENCE_TYPE
	  || TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0))) == REFERENCE_TYPE)
	break;
      /* fall through...  */
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
      /* fall through...  */
    case BIT_XOR_EXPR:
      /* This and MINUS_EXPR can be changed into a comparison of the
	 two objects.  */
      if (TREE_TYPE (TREE_OPERAND (expr, 0))
	  == TREE_TYPE (TREE_OPERAND (expr, 1)))
	return build_binary_op (NE_EXPR, TREE_OPERAND (expr, 0),
				TREE_OPERAND (expr, 1), 1);
      return build_binary_op (NE_EXPR, TREE_OPERAND (expr, 0),
			      fold (build1 (NOP_EXPR,
					    TREE_TYPE (TREE_OPERAND (expr, 0)),
					    TREE_OPERAND (expr, 1))), 1);

    case BIT_AND_EXPR:
      if (integer_onep (TREE_OPERAND (expr, 1))
	  && TREE_TYPE (expr) != boolean_type_node)
	/* Using convert here would cause infinite recursion.  */
	return build1 (NOP_EXPR, boolean_type_node, expr);
      break;

    case MODIFY_EXPR:
      if ( /* warn_parentheses && */ C_EXP_ORIGINAL_CODE (expr) == MODIFY_EXPR)
	warning ("suggest parentheses around assignment used as truth value");
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

/* Construct and perhaps optimize a tree representation
   for a unary operation.  CODE, a tree_code, specifies the operation
   and XARG is the operand.
   For any CODE other than ADDR_EXPR, FLAG nonzero suppresses
   the default promotions (such as from short to int).
   For ADDR_EXPR, the default promotions are not applied; FLAG nonzero
   allows non-lvalues; this is only used to handle conversion of non-lvalue
   arrays to pointers in C99.  */

tree
build_unary_op (code, xarg, flag)
     enum tree_code code;
     tree xarg;
     int flag;
{
  /* No default_conversion here.  It causes trouble for ADDR_EXPR.  */
  tree arg = xarg;
  tree argtype = 0;
  enum tree_code typecode = TREE_CODE (TREE_TYPE (arg));
  tree val;
  int noconvert = flag;

  if (typecode == ERROR_MARK)
    return error_mark_node;
  if (typecode == ENUMERAL_TYPE || typecode == BOOLEAN_TYPE)
    typecode = INTEGER_TYPE;

  switch (code)
    {
    case CONVERT_EXPR:
      /* This is used for unary plus, because a CONVERT_EXPR
	 is enough to prevent anybody from looking inside for
	 associativity, but won't generate any code.  */
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE
	    || typecode == COMPLEX_TYPE))
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
	    pedwarn ("ISO C does not support `~' for complex conjugation");
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
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE
	    || typecode == COMPLEX_TYPE))
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
	  && typecode != COMPLEX_TYPE
	  /* These will convert to a pointer.  */
	  && typecode != ARRAY_TYPE && typecode != FUNCTION_TYPE)
	{
	  error ("wrong type argument to unary exclamation mark");
	  return error_mark_node;
	}
      arg = c_common_truthvalue_conversion (arg);
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

      val = unary_complex_lvalue (code, arg, 0);
      if (val != 0)
	return val;

      /* Increment or decrement the real part of the value,
	 and don't change the imaginary part.  */
      if (typecode == COMPLEX_TYPE)
	{
	  tree real, imag;

	  if (pedantic)
	    pedwarn ("ISO C does not support `++' and `--' on complex types");

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
		  if (TREE_CODE (TREE_TYPE (arg)) == BOOLEAN_TYPE)
		    value = boolean_increment (code, arg);
		  else
		    {
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
		    }
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
				   ? "invalid lvalue in increment"
				   : "invalid lvalue in decrement")))
	  return error_mark_node;

	/* Report a read-only lvalue.  */
	if (TREE_READONLY (arg))
	  readonly_warning (arg, 
			    ((code == PREINCREMENT_EXPR
			      || code == POSTINCREMENT_EXPR)
			     ? "increment" : "decrement"));

	if (TREE_CODE (TREE_TYPE (arg)) == BOOLEAN_TYPE)
	  val = boolean_increment (code, arg);
	else
	  val = build (code, TREE_TYPE (arg), arg, inc);
	TREE_SIDE_EFFECTS (val) = 1;
	val = convert (result_type, val);
	if (TREE_CODE (val) != code)
	  TREE_NO_UNUSED_WARNING (val) = 1;
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
	  if (!gm2_mark_addressable (TREE_OPERAND (arg, 0)))
	    return error_mark_node;
	  return build_binary_op (PLUS_EXPR, TREE_OPERAND (arg, 0),
				  TREE_OPERAND (arg, 1), 1);
	}

      /* Handle complex lvalues (when permitted)
	 by reduction to simpler cases.  */
      val = unary_complex_lvalue (code, arg, flag);
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
	    pedwarn ("ISO C forbids the address of a cast expression");
	  return convert (build_pointer_type (TREE_TYPE (arg)),
			  build_unary_op (ADDR_EXPR, TREE_OPERAND (arg, 0),
					  0));
	}
#endif

      /* Anything not already handled and not a true memory reference
	 or a non-lvalue array is an error.  */
      else if (typecode != FUNCTION_TYPE && !flag
	       && !lvalue_or_else (arg, "invalid lvalue in unary `&'"))
	return error_mark_node;

      /* Ordinary case; arg is a COMPONENT_REF or a decl.  */
      argtype = TREE_TYPE (arg);

      /* If the lvalue is const or volatile, merge that into the type
         to which the address will point.  Note that you can't get a
	 restricted pointer by taking the address of something, so we
	 only have to deal with `const' and `volatile' here.  */
      if ((DECL_P (arg) || TREE_CODE_CLASS (TREE_CODE (arg)) == 'r')
	  && (TREE_READONLY (arg) || TREE_THIS_VOLATILE (arg)))
	  argtype = c_build_type_variant (argtype,
					  TREE_READONLY (arg),
					  TREE_THIS_VOLATILE (arg));

      argtype = build_pointer_type (argtype);

      if (!gm2_mark_addressable (arg))
	return error_mark_node;

      {
	tree addr;

	if (TREE_CODE (arg) == COMPONENT_REF)
	  {
	    tree field = TREE_OPERAND (arg, 1);

	    addr = build_unary_op (ADDR_EXPR, TREE_OPERAND (arg, 0), flag);

	    if (DECL_C_BIT_FIELD (field))
	      {
		error ("attempt to take address of bit-field structure member `%s'",
		       IDENTIFIER_POINTER (DECL_NAME (field)));
		return error_mark_node;
	      }

	    addr = fold (build (PLUS_EXPR, argtype,
				convert (argtype, addr),
				convert (argtype, byte_position (field))));
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

  if (argtype == 0)
    argtype = TREE_TYPE (arg);
  return fold (build1 (code, argtype, arg));
}

/* Print an error message for invalid operands to arith operation CODE.
   NOP_EXPR is used as a special case (see gm2_truthvalue_conversion).  */

void
binary_op_error (code)
     enum tree_code code;
{
  register const char *opname;

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
    case TRUTH_AND_EXPR:
      opname = "&"; break;
    case TRUTH_OR_EXPR:
      opname = "|"; break;
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
      if ((code0 == POINTER_TYPE && code1 == INTEGER_TYPE) ||
          (code1 == POINTER_TYPE && code0 == INTEGER_TYPE) ||
	  (code0 == POINTER_TYPE && code1 == POINTER_TYPE))
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
      if ((code0 == POINTER_TYPE && code1 == INTEGER_TYPE) ||
          (code1 == POINTER_TYPE && code0 == INTEGER_TYPE) ||
	  (code0 == POINTER_TYPE && code1 == POINTER_TYPE))
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
      if (warn_div_by_zero && skip_evaluation == 0 && integer_zerop (op1))
	warning ("division by zero");

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
	  if (!(code0 == INTEGER_TYPE && code1 == INTEGER_TYPE))
	    resultcode = RDIV_EXPR;
	  else
	    /* Although it would be tempting to shorten always here, that
	       loses on some targets, since the modulo instruction is
	       undefined if the quotient can't be represented in the
	       computation mode.  We shorten only if unsigned or if
	       dividing by something we know != -1.  */
	    shorten = (TREE_UNSIGNED (TREE_TYPE (orig_op0))
		       || (TREE_CODE (op1) == INTEGER_CST
			   && ! integer_all_onesp (op1)));
	  common = 1;
	}
      break;

    case BIT_AND_EXPR:
    case BIT_ANDTC_EXPR:
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
      if (warn_div_by_zero && skip_evaluation == 0 && integer_zerop (op1))
	warning ("division by zero");

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
	  shorten = (TREE_UNSIGNED (TREE_TYPE (orig_op0))
		     || (TREE_CODE (op1) == INTEGER_CST
			 && ! integer_all_onesp (op1)));
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
                warning ("right shift count is negative");
              else
                {
                  if (! integer_zerop (op1))
                    short_shift = 1;

                  if (compare_tree_int (op1, TYPE_PRECISION (type0)) >= 0)
                    warning ("right shift count >= width of type");
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
                warning ("left shift count is negative");

              else if (compare_tree_int (op1, TYPE_PRECISION (type0)) >= 0)
                warning ("left shift count >= width of type");
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
                warning ("shift count is negative");
              else if (compare_tree_int (op1, TYPE_PRECISION (type0)) >= 0)
                warning ("shift count >= width of type");
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
	warning ("comparing floating point with == or != is unsafe");
      /* Result of comparison is always int,
	 but don't convert the args to int!  */
      build_type = integer_type_node;
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE
	   || code0 == COMPLEX_TYPE
	   || code0 == VECTOR_TYPE)
	  && (code1 == INTEGER_TYPE || code1 == REAL_TYPE
	      || code1 == COMPLEX_TYPE
	      || code1 == VECTOR_TYPE))
	short_compare = 1;
      else if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
	{
	  tree tt0 = TREE_TYPE (type0);
	  tree tt1 = TREE_TYPE (type1);
	  /* Anything compares with void *.  void * compares with anything.
	     Otherwise, the targets must be compatible
	     and both must be object or both incomplete.  */
	  if (comp_target_types (type0, type1, 1))
	    result_type = common_type (type0, type1);
	  else if (VOID_TYPE_P (tt0))
	    {
	      /* op0 != orig_op0 detects the case of something
		 whose value is 0 but which isn't a valid null ptr const.  */
	      if (pedantic && (!integer_zerop (op0) || op0 != orig_op0)
		  && TREE_CODE (tt1) == FUNCTION_TYPE)
		pedwarn ("ISO C forbids comparison of `void *' with function pointer");
	    }
	  else if (VOID_TYPE_P (tt1))
	    {
	      if (pedantic && (!integer_zerop (op1) || op1 != orig_op1)
		  && TREE_CODE (tt0) == FUNCTION_TYPE)
		pedwarn ("ISO C forbids comparison of `void *' with function pointer");
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
	  if (comp_target_types (type0, type1, 1))
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
	  if (comp_target_types (type0, type1, 1))
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
	    unsigned_arg = TREE_UNSIGNED (TREE_TYPE (op0));

	  if (TYPE_PRECISION (TREE_TYPE (arg0)) < TYPE_PRECISION (result_type)
	      /* We can shorten only if the shift count is less than the
		 number of bits in the smaller type size.  */
	      && compare_tree_int (op1, TYPE_PRECISION (TREE_TYPE (arg0))) < 0
	      /* We cannot drop an unsigned shift after sign-extension.  */
	      && (!TREE_UNSIGNED (final_type) || unsigned_arg))
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
	      int op0_signed = ! TREE_UNSIGNED (TREE_TYPE (orig_op0));
	      int op1_signed = ! TREE_UNSIGNED (TREE_TYPE (orig_op1));
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
	      if (! TREE_UNSIGNED (result_type))
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
		  if (gm2_tree_expr_nonnegative_p (sop))
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
		    warning ("comparison between signed and unsigned");
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
    tree result = build (resultcode, build_type, op0, op1);
    tree folded;

    folded = fold (result);
    if (folded == result)
      TREE_CONSTANT (folded) = TREE_CONSTANT (op0) & TREE_CONSTANT (op1);
    if (final_type != 0)
      return convert (final_type, folded);
    return folded;
  }
}

/*
 *  routines which interface with the Modula-2 source
 */

tree
skip_type_decl (type)
     tree type;
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
skip_const_decl (exp)
     tree exp;
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
gccgm2_DeclareKnownType (name, type)
     char *name;
     tree type;
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
gccgm2_GetDefaultType (name, type)
     char *name;
     tree type;
{
  tree id = maybe_get_identifier (name);

  if (id == NULL)
    return gccgm2_DeclareKnownType(name, type);
  else
    return id;
}

/*
 *  GetMinFrom - given a, type, return a constant representing the minimum
 *               legal value.
 */

tree
gccgm2_GetMinFrom (type)
     tree type;
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

  return TYPE_MIN_VALUE (skip_type_decl (type));
}

/*
 *  GetMaxFrom - given a, type, return a constant representing the maximum
 *               legal value.
 */

tree
gccgm2_GetMaxFrom (type)
     tree type;
{
  if (type == m2_real_type_node || type == gccgm2_GetRealType ())
    return fold (gm2builtins_BuiltInHugeVal ());
  if (type == m2_long_real_type_node || type == gccgm2_GetLongRealType())
    return fold (gm2builtins_BuiltInHugeValLong ());
  if (type == m2_short_real_type_node || type == gccgm2_GetShortRealType())
    return fold (gm2builtins_BuiltInHugeValShort ());

  return TYPE_MAX_VALUE (skip_type_decl (type));
}

int
gccgm2_GetBitsPerWord ()
{
  return BITS_PER_WORD;
}

int
gccgm2_GetBitsPerInt ()
{
  return INT_TYPE_SIZE;
}

int
gccgm2_GetBitsPerBitset ()
{
  return SET_WORD_SIZE;
}

int
gccgm2_GetBitsPerUnit ()
{
  return BITS_PER_UNIT;
}

#if defined(DEBUGGING)
static tree watch;

static int                    is_var                                      PARAMS ((tree var));
static void                   debug_watch                                 PARAMS ((tree));
static int                    is_array                                    PARAMS ((tree));

static void debug_watch (t)
     tree t;
{
  watch = t;
}

static int
is_var (var)
     tree var;
{
  return TREE_CODE(var) == VAR_DECL;
}

static int
is_array (array)
     tree array;
{
  return TREE_CODE(array) == ARRAY_TYPE;
}
#endif

static int
is_type (type)
     tree type;
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

  if (strcmp(name, "varin_var") == 0)
    stop();

  ASSERT(is_type(type), type);

  id   = get_identifier (name);
  type = skip_type_decl (type);
  decl = build_decl (VAR_DECL, id, type);

  DECL_SOURCE_LINE (decl) = lineno;

  DECL_EXTERNAL (decl)    = imported;
  if (isglobal && (scope == NULL_TREE)) {
    TREE_PUBLIC   (decl)  = exported;
    TREE_STATIC   (decl)  = isglobal;           /* declaration and definition */
  } 
  else
    TREE_PUBLIC   (decl)  = 1;

  DECL_CONTEXT  (decl)    = scope;
  TREE_USED     (type)    = 1;
  TREE_USED     (decl)    = 1;

  /* now for the id */

  if (imported)
    TREE_STATIC   (id)    = 0;           /* declaration and definition */
  else
    TREE_STATIC   (id)    = 1;           /* declaration and definition */

  if (isglobal)
    TREE_PUBLIC   (id)    = exported;

  TREE_USED     (id)      = 1;

  pushdecl (decl);

  if (DECL_SIZE(decl) == 0) {
    error_with_decl (decl, "storage size of `%s' hasn't been resolved");
  }
  expand_decl (decl);
  if (isglobal) {
    layout_decl (decl, 0);
    rest_of_decl_compilation (decl, 0, 1, 0);
  }

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
gccgm2_DeclareKnownConstant (type, value)
     tree type, value;
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

/* FROM ../c-semantics.c IMPORT */
/* Create a declaration statement for the declaration given by the
   DECL.  */

void
add_decl_stmt (decl)
     tree decl ATTRIBUTE_UNUSED;
{
#if defined(GM2)
  abort();
#else
  tree decl_stmt;

  /* We need the type to last until instantiation time.  */
  decl_stmt = build_stmt (DECL_STMT, decl);
  add_stmt (decl_stmt); 
#endif
}

/* FROM ../c-decl.c IMPORT */
/* Finish processing of a declaration;
   install its initial value.
   If the length of an array type is not known before,
   it must be determined now, from the initial value, or it is an error.  */

void
finish_decl (decl, init, asmspec_tree)
     tree decl, init;
     tree asmspec_tree;
{
  tree type = TREE_TYPE (decl);
  int was_incomplete = (DECL_SIZE (decl) == 0);
  const char *asmspec = 0;

  /* If a name was specified, get the string.  */
  if (current_binding_level == global_binding_level)
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

      else if (failure == 2)
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
      else if (pedantic && TYPE_DOMAIN (type) != 0
	      && tree_int_cst_sgn (TYPE_MAX_VALUE (TYPE_DOMAIN (type))) < 0)
	error_with_decl (decl, "zero or negative size array `%s'");

      layout_decl (decl, 0);
    }

  if (TREE_CODE (decl) == VAR_DECL)
    {
      if (DECL_SIZE (decl) == 0 && TREE_TYPE (decl) != error_mark_node
	  && COMPLETE_TYPE_P (TREE_TYPE (decl)))
	layout_decl (decl, 0);

      if (DECL_SIZE (decl) == 0
	  /* Don't give an error if we already gave one earlier.  */
	  && TREE_TYPE (decl) != error_mark_node
	  && (TREE_STATIC (decl)
	      ?
		/* A static variable with an incomplete type
		   is an error if it is initialized.
		   Also if it is not file scope.
		   Otherwise, let it through, but if it is not `extern'
		   then it may cause an error message later.  */
		(DECL_INITIAL (decl) != 0
		 || DECL_CONTEXT (decl) != 0)
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

      if (TREE_USED (type))
	TREE_USED (decl) = 1;
    }

  /* If this is a function and an assembler name is specified, it isn't
     builtin any more.  Also reset DECL_RTL so we can give it its new
     name.  */
  if (TREE_CODE (decl) == FUNCTION_DECL && asmspec)
    {
      DECL_BUILT_IN_CLASS (decl) = NOT_BUILT_IN;
      SET_DECL_RTL (decl, NULL_RTX);
      SET_DECL_ASSEMBLER_NAME (decl, get_identifier (asmspec));
    }

  /* Output the assembler code and/or RTL code for variables and functions,
     unless the type is an undefined structure or union.
     If not, it will get done when the type is completed.  */

  if (TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == FUNCTION_DECL)
    {
      /* This is a no-op in c-lang.c or something real in objc-act.c.  */
      if (flag_objc)
	objc_check_decl (decl);

      if (!DECL_CONTEXT (decl))
	{
	  if (DECL_INITIAL (decl) == NULL_TREE
	      || DECL_INITIAL (decl) == error_mark_node)
	    /* Don't output anything
	       when a tentative file-scope definition is seen.
	       But at end of compilation, do output code for them.  */
	    DECL_DEFER_OUTPUT (decl) = 1;
	  rest_of_decl_compilation (decl, asmspec,
				    (DECL_CONTEXT (decl) == 0
				     || TREE_ASM_WRITTEN (decl)), 0);
	}
      else
	{
	  /* This is a local variable.  If there is an ASMSPEC, the
	     user has requested that we handle it specially.  */
	  if (asmspec)
	    {
	      /* In conjunction with an ASMSPEC, the `register'
		 keyword indicates that we should place the variable
		 in a particular register.  */
	      if (DECL_REGISTER (decl))
		DECL_C_HARD_REGISTER (decl) = 1;

	      /* If this is not a static variable, issue a warning.
		 It doesn't make any sense to give an ASMSPEC for an
		 ordinary, non-register local variable.  Historically,
		 GCC has accepted -- but ignored -- the ASMSPEC in
		 this case.  */
	      if (TREE_CODE (decl) == VAR_DECL 
		  && !DECL_REGISTER (decl)
		  && !TREE_STATIC (decl))
		warning_with_decl (decl,
				   "ignoring asm-specifier for non-static local variable `%s'");
	      else
		SET_DECL_ASSEMBLER_NAME (decl, get_identifier (asmspec));
	    }

	  if (TREE_CODE (decl) != FUNCTION_DECL)
	    add_decl_stmt (decl);
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
	    }
	}
    }

  if (TREE_CODE (decl) == TYPE_DECL)
    {
      /* This is a no-op in c-lang.c or something real in objc-act.c.  */
      if (flag_objc)
	objc_check_decl (decl);
      rest_of_decl_compilation (decl, NULL, DECL_CONTEXT (decl) == 0, 0);
    }

  /* At the end of a declaration, throw away any variable type sizes
     of types defined inside that declaration.  There is no use
     computing them in the following function definition.  */
  if (current_binding_level == global_binding_level)
    get_pending_sizes ();

#if !defined(GM2)
  /* Install a cleanup (aka destructor) if one was given.  */
  if (TREE_CODE (decl) == VAR_DECL && !TREE_STATIC (decl))
    {
      tree attr = lookup_attribute ("cleanup", DECL_ATTRIBUTES (decl));
      if (attr)
	{
	  static bool eh_initialized_p;

	  tree cleanup_id = TREE_VALUE (TREE_VALUE (attr));
	  tree cleanup_decl = lookup_name (cleanup_id);
	  tree cleanup;

	  /* Build "cleanup(&decl)" for the destructor.  */
	  cleanup = build_unary_op (ADDR_EXPR, decl, 0);
	  cleanup = build_tree_list (NULL_TREE, cleanup);
	  cleanup = build_function_call (cleanup_decl, cleanup);

	  /* Don't warn about decl unused; the cleanup uses it.  */
	  TREE_USED (decl) = 1;

	  /* Initialize EH, if we've been told to do so.  */
	  if (flag_exceptions && !eh_initialized_p)
	    {
	      eh_initialized_p = true;
	      eh_personality_libfunc
		= init_one_libfunc (USING_SJLJ_EXCEPTIONS
				    ? "__gcc_personality_sj0"
				    : "__gcc_personality_v0");
	      using_eh_for_cleanups ();
	    }

	  add_stmt (build_stmt (CLEANUP_STMT, decl, cleanup));
	}
    }
#endif
}

#if 0
/* To speed up processing of attributes, we maintain an array of
   IDENTIFIER_NODES and the corresponding attribute types.  */

/* Array to hold attribute information.  */

static struct {enum attrs id; tree name; int min, max, decl_req;} attrtab[50];

static int attrtab_idx = 0;

/* Add an entry to the attribute table above.  */

static void
add_attribute (id, string, min_len, max_len, decl_req)
     enum attrs id;
     const char *string;
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

int (*valid_lang_attribute) PARAMS ((tree, tree, tree, tree))
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
#endif

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
  return enumtype;
}

tree
gccgm2_BuildEnumerator (name, value)
     char *name;
     tree value;
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
gccgm2_BuildPointerType (totype)
     tree totype;
{
  return build_pointer_type (skip_type_decl (totype));
}


/*
 *  BuildArrayType - returns a type which is an array indexed by IndexType
 *                   and which has ElementType elements.
 */

tree
gccgm2_BuildArrayType (elementtype, indextype)
     tree elementtype, indextype;
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
build_set_type (domain, range_type, allow_void)
     tree domain;
     tree range_type;
     int allow_void;
{
  tree type;

  if (! ORDINAL_TYPE (TREE_CODE (domain))
      && !(allow_void && TREE_CODE (domain) == VOID_TYPE))
    {
      error ("set base type must be an ordinal type");
      return NULL;
    }

  type = make_node (SET_TYPE);
  TREE_TYPE (type) = range_type;
  TYPE_DOMAIN (type) = domain;

  if (TYPE_SIZE (range_type) == 0)
    layout_type (range_type);

  return type;
}

/*
 *  convert_type_to_range - does the conversion and copies the range type
 */

tree
convert_type_to_range (type)
     tree type;
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

  itype = build_type_copy (build_range_type (TREE_TYPE (min), min, max));

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

  return gccgm2_BuildSetTypeFromSubrange (NULL, bitnum_type_node,
					  gccgm2_BuildIntegerConstant (0),
					  gccgm2_BuildIntegerConstant (gccgm2_GetBitsPerBitset()-1));
}

/*
 *  BuildSetTypeFromSubrange - constructs a set type from a subrangeType.
 */

tree
gccgm2_BuildSetTypeFromSubrange (name, subrangeType, lowval, highval)
     char *name;
     tree subrangeType;
     tree lowval;
     tree highval;
{
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
      TYPE_MAX_VALUE (settype) = gccgm2_BuildSub (gccgm2_BuildLSL (gccgm2_GetWordOne(), noelements, FALSE),
						  integer_one_node,
						  FALSE);
    TYPE_MIN_VALUE (settype) = integer_zero_node;
    
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
gccgm2_BuildSetType (name, type, lowval, highval)
     char *name;
     tree type, lowval, highval;
{
  tree range = build_range_type (skip_type_decl (type), lowval, highval);

  return gccgm2_BuildSetTypeFromSubrange (name, range, lowval, highval);
}

/*
 *  BuildStartSetConstructor - starts to create a set constant.
 *                             Remember that type is really a record type.
 */

void
gccgm2_BuildStartSetConstructor (type)
     tree type;
{
  constructor_type = type;
  constructor_fields = TYPE_FIELDS (type);
  constructor_element_list = NULL_TREE;
}

/*
 *  BuildSetConstructorElement - adds, value, to the constructor_element_list.
 */

void
gccgm2_BuildSetConstructorElement (value)
     tree value;
{
  if (value == NULL_TREE) {
    error_with_decl(constructor_type, "internal error `%s' set type cannot be initialized with a NULL_TREE");
    return;
  }

  if (constructor_fields == NULL) {
    error_with_decl(constructor_type, "internal error `%s' set type does not take another integer value");
    error_with_decl(value, "internal error `%s' value initializing the set");
    return;
  }
  
  constructor_element_list = tree_cons (constructor_fields, value, constructor_element_list);
  constructor_fields = TREE_CHAIN (constructor_fields);
}

/*
 *  BuildEndSetConstructor - finishes building a set constant.
 */

tree
gccgm2_BuildEndSetConstructor ()
{
  tree constructor = build (CONSTRUCTOR, constructor_type, NULL_TREE,
			    nreverse (constructor_element_list));
  TREE_CONSTANT (constructor) = 1;
  TREE_STATIC (constructor) = 1;
  return constructor;
}

/*
 *  BuildSubrangeType - creates a subrange of, type, with, lowval, highval.
 */

tree
gccgm2_BuildSubrangeType (name, type, lowval, highval)
     char *name;
     tree type, lowval, highval;
{
  tree id = build_range_type( skip_type_decl (type), lowval, highval);

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
gccgm2_BuildArrayIndexType (low, high)
     tree low, high;
{
  return build_index_2_type (default_conversion(low), default_conversion(high));
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
  tree indextype = build_index_type (variable_size(high));
  tree arraytype = build_array_type (elementtype, indextype);
  tree id        = get_identifier (name);
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
  pushdecl (decl);
  expand_decl (decl);
  layout_decl (decl, 0);
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
 *  BuildStartFunctionType - initializes global variables ready for building
 *                           a function.
 */

tree
gccgm2_BuildStartFunctionType (name)
     char *name ATTRIBUTE_UNUSED;
{
  tree n = make_node (POINTER_TYPE);
  TYPE_SIZE (n) = 0;

  return n;
}

/*
 *  finish_build_pointer_type - finish building a POINTER_TYPE node.
 *                              necessary to solve self references in
 *                              procedure types.
 */

static
tree
finish_build_pointer_type (t, to_type)
     tree t, to_type;
{
  TREE_TYPE (t) = to_type;

  /* Record this type as the pointer to TO_TYPE.  */
  TYPE_POINTER_TO (to_type) = t;

  /* Lay out the type.  This function has many callers that are concerned
     with expression-construction, and this simplifies them all.
     Also, it guarantees the TYPE_SIZE is in the same obstack as the type.  */
  layout_type (t);

  return t;
}

/*
 *
 *  BuildEndFunctionType - build a function type which would return a, value.
 *                         The arguments have been created by BuildParameterDeclaration.
 */

tree
gccgm2_BuildEndFunctionType (func, type)
     tree func, type;
{
  tree t;

  if (type == NULL_TREE)
    type = void_type_node;

  type = skip_type_decl (type);
  t = finish_build_pointer_type (func, build_function_type (type, param_type_list));

  return t;
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

  type = skip_type_decl(type);
  layout_type (type);
  if (isreference) {
    type = build_reference_type (type);
  }
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
gccgm2_BuildEndFunctionDeclaration (name, returntype, isexternal, isnested)
     char *name;
     tree  returntype;
     int   isexternal;
     int   isnested;
{
  tree fntype;
  tree fndecl;

  returntype = skip_type_decl (returntype);
  /*
   *  the function type depends on the return type and type of args,
   *  both of which we have created in BuildParameterDeclaration
   */
  if (returntype == NULL_TREE)
    returntype = void_type_node;
  else if (TREE_CODE(returntype) == FUNCTION_TYPE)
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
  DECL_SOURCE_LINE (fndecl) = lineno;

  pushdecl (fndecl);

  rest_of_decl_compilation (fndecl, NULL, 1, 0);
  param_list = NULL_TREE;   /* ready for the next time we call/define a function */
  return fndecl;
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

  /* Announce we are compiling this function.  */
  announce_function (fndecl);

  /* Set up to compile the function and enter it.  */

  /* Make the init_value nonzero so pushdecl knows this is not tentative.
     error_mark_node is replaced below (in poplevel) with the BLOCK.  */
  DECL_INITIAL (fndecl) = error_mark_node;

  current_function_decl = fndecl;
  pushlevel(0);

  declare_parm_level (1);
  current_binding_level->subblocks_tag_transparent = 1;
  make_decl_rtl (current_function_decl, NULL);

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
  TREE_ADDRESSABLE(fndecl) = 1;   /* could be improved, if we want to inline it make this a 0 */

  init_function_start (fndecl, input_filename, lineno);
  expand_function_start (fndecl, 0);

  /* open a new nesting level */
  pushlevel (0);   /* outer nesting level contains parameters and inner contains local variables */
  expand_start_bindings (0);
  // printf("starting scope %s\n", IDENTIFIER_POINTER(DECL_NAME (fndecl)));
}

/*
 *  BuildEndFunctionCode - generates the function epilogue.
 */

void
gccgm2_BuildEndFunctionCode (fndecl)
     tree fndecl;
{
  expand_end_bindings (getdecls (), 1, 0);
  poplevel (1, 1, 0);   /* shutdown local variables */
  poplevel (1, 0, 1);   /* shutdown parameters and function */
  BLOCK_SUPERCONTEXT (DECL_INITIAL (fndecl)) = fndecl;

  /* Must mark the RESULT_DECL as being in this function.  */
  DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;

  expand_function_end (input_filename, lineno, 0);

  rest_of_compilation (fndecl);
  current_function_decl = NULL;
  // printf("ending scope %s\n", IDENTIFIER_POINTER(DECL_NAME (fndecl)));
}

/*
 *  BuildReturnValueCode - generates the code associated with: RETURN( value )
 */

void
gccgm2_BuildReturnValueCode (fndecl, value)
     tree fndecl, value;
{
  DECL_SOURCE_LINE (DECL_RESULT (fndecl)) = lineno;
  DECL_SOURCE_FILE (DECL_RESULT (fndecl)) = input_filename;

  if (TREE_CODE (TREE_TYPE (value)) == FUNCTION_TYPE)
    expand_return( build (MODIFY_EXPR, void_type_node,
                          DECL_RESULT (fndecl), build1 (CONVERT_EXPR, ptr_type_node, value)) );

  else
    expand_return( build (MODIFY_EXPR, void_type_node,
                          DECL_RESULT (fndecl),
                          gccgm2_BuildConvert (TREE_TYPE (DECL_RESULT (fndecl)), value)) );
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

/*
 *  BuildAssignment - builds the assignment of, des, and, expr.
 *                    It returns, des.
 */

tree
gccgm2_BuildAssignment (des, expr)
     tree des, expr;
{
  if (TREE_CODE (expr) == FUNCTION_DECL)
    expr = build_unary_op (ADDR_EXPR, expr, 0);
  
  expand_assignment (des, expr, 0, 0);
  return des;
}


/*
 *  BuildAdd - builds an addition tree.
 */

tree
gccgm2_BuildAdd (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return build_binary_op (PLUS_EXPR, op1, op2, needconvert);
}


/*
 *  BuildSub - builds a subtraction tree.
 */

tree
gccgm2_BuildSub (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return build_binary_op (MINUS_EXPR, op1, op2, needconvert);
}


/*
 *  BuildMult - builds a multiplication tree.
 */

tree
gccgm2_BuildMult (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return build_binary_op (MULT_EXPR, op1, op2, needconvert);
}


/*
 *  BuildDiv - builds a division tree.
 */

tree
gccgm2_BuildDiv (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return build_binary_op (TRUNC_DIV_EXPR, op1, op2, needconvert);
}


/*
 *  BuildMod - builds a modulus tree.
 */

tree
gccgm2_BuildMod (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return build_binary_op (TRUNC_MOD_EXPR, op1, op2, needconvert);
}


/*
 *  BuildLSL - builds and returns tree (op1 << op2)
 */

tree
gccgm2_BuildLSL (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return build_binary_op (LSHIFT_EXPR, op1, op2, needconvert);
}

/*
 *  BuildLSR - builds and returns tree (op1 >> op2)
 */

tree
gccgm2_BuildLSR (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return build_binary_op (RSHIFT_EXPR, op1, op2, needconvert);
}

/*
 *  createUniqueLabel - returns a unique label which has been
 *                      xmalloc'ed.
 */

static char *
createUniqueLabel ()
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
gccgm2_BuildLogicalShift (op1, op2, op3, nBits, needconvert)
     tree op1, op2, op3;
     tree nBits ATTRIBUTE_UNUSED;
     int needconvert;
{
  tree res;

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
gccgm2_BuildLRL (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return build_binary_op (LROTATE_EXPR, op1, op2, needconvert);
}

/*
 *  BuildLRR - builds and returns tree (op1 rotate right by op2 bits)
 */

tree
gccgm2_BuildLRR (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return build_binary_op (RROTATE_EXPR, op1, op2, needconvert);
}

/*
 *  gccgm2_BuildMask - returns a tree for the mask of a set
 *                     of nBits. It assumes nBits is <= TSIZE(WORD)
 */

tree
gccgm2_BuildMask (nBits, needconvert)
     tree nBits;
     int needconvert;
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
gccgm2_BuildLRLn (op1, op2, nBits, needconvert)
     tree op1, op2, nBits;
     int  needconvert;
{
  /*
   *  ensure we wrap the rotate
   */
  op2 = gccgm2_BuildMod (op2, nBits, needconvert);
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
gccgm2_BuildLRRn (op1, op2, nBits, needconvert)
     tree op1, op2, nBits;
     int  needconvert;
{
  /*
   *  ensure we wrap the rotate
   */
  op2 = gccgm2_BuildMod (op2, nBits, needconvert);
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
gccgm2_BuildLogicalRotate (op1, op2, op3, nBits, needconvert)
     tree op1, op2, op3;
     tree nBits;
     int needconvert;
{
  tree res;

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
buildUnboundedArrayOf (unbounded, contentsPtr, high)
     tree unbounded, contentsPtr, high;
{
  tree fields     = TYPE_FIELDS (unbounded);
  tree field_list = NULL_TREE;
  tree constructor;

  field_list = tree_cons (fields, contentsPtr, field_list);
  fields = TREE_CHAIN (fields);

  field_list = tree_cons (fields, high, field_list);

  constructor = build (CONSTRUCTOR, unbounded, NULL_TREE,
		       nreverse (field_list));
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
gccgm2_BuildBinarySetDo (settype, op1, op2, op3,
			 binop,
			 is_op1lvalue, is_op2lvalue, is_op3lvalue,
			 nBits, unbounded,
			 varproc, leftproc, rightproc)
     tree settype, op1, op2, op3;
     tree (*binop)(tree, tree, tree, tree, int);
     int is_op1lvalue, is_op2lvalue, is_op3lvalue;
     tree nBits;
     tree unbounded;
     tree varproc, leftproc, rightproc;
{
  tree size     = gccgm2_GetSizeOf (settype);
  int  is_const = FALSE;
  int  is_left  = FALSE;

  if (gccgm2_CompareTrees (size, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    (*binop) (get_rvalue (op1, settype, is_op1lvalue),
	      get_rvalue (op2, settype, is_op2lvalue),
	      get_rvalue (op3, settype, is_op3lvalue),
	      nBits,
	      FALSE);
  else {
    tree result;
    tree high = gccgm2_BuildSub (gccgm2_BuildDiv (size,
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
  }
}

/*
 *  BuildConvert - build and return tree VAL(op1, op2)
 *                 where op1 is the type to which op2
 *                 is to be converted.
 */

tree
gccgm2_BuildConvert (op1, op2)
     tree op1, op2;
{
  return convert_and_check (skip_type_decl (op1), op2);
}

#if 0
/*
 *  Build an expression representing a cast to type TYPE of expression EXPR.
 *  (taken from build_c_cast)
 */

static
tree
build_m2_cast (type, expr)
     tree type;
     tree expr;
{
  tree value = expr;
  
  if (type == error_mark_node || expr == error_mark_node)
    return error_mark_node;
#if !defined(GM2)
  type = TYPE_MAIN_VARIANT (type);
#endif

#if 0
  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
  if (TREE_CODE (value) == NON_LVALUE_EXPR)
    value = TREE_OPERAND (value, 0);
#endif

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      error ("cast specifies array type");
      return error_mark_node;
    }

  if (TREE_CODE (type) == FUNCTION_TYPE)
    {
      error ("cast specifies function type");
      return error_mark_node;
    }

#if !defined(GM2)
  if (type == TYPE_MAIN_VARIANT (TREE_TYPE (value)))
    {
      if (pedantic)
	{
	  if (TREE_CODE (type) == RECORD_TYPE
	      || TREE_CODE (type) == UNION_TYPE)
	    pedwarn ("ISO C forbids casting nonscalar to the same type");
	}
    }
  else if (TREE_CODE (type) == UNION_TYPE)
    {
      tree field;
      value = default_function_array_conversion (value);

      for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	if (comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (field)),
		       TYPE_MAIN_VARIANT (TREE_TYPE (value))))
	  break;

      if (field)
	{
	  const char *name;
	  tree t;

	  if (pedantic)
	    pedwarn ("ISO C forbids casts to union type");
	  if (TYPE_NAME (type) != 0)
	    {
	      if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
		name = IDENTIFIER_POINTER (TYPE_NAME (type));
	      else
		name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
	    }
	  else
	    name = "";
	  t = digest_init (type, build (CONSTRUCTOR, type, NULL_TREE,
					build_tree_list (field, value)),
			   0, 0);
	  TREE_CONSTANT (t) = TREE_CONSTANT (value);
	  return t;
	}
      error ("cast to union type from type not present in union");
      return error_mark_node;
    }
  else
#endif
    {
      tree otype, ovalue;

      /* If casting to void, avoid the error that would come
	 from default_conversion in the case of a non-lvalue array.  */
      if (type == void_type_node)
	return build1 (CONVERT_EXPR, type, value);

      /* Convert functions and arrays to pointers,
	 but don't convert any other types.  */
      value = default_function_array_conversion (value);
      otype = TREE_TYPE (value);

#if !defined(GM2)
      /* Optionally warn about potentially worrisome casts.  */

      if (warn_cast_qual
	  && TREE_CODE (type) == POINTER_TYPE
	  && TREE_CODE (otype) == POINTER_TYPE)
	{
	  tree in_type = type;
	  tree in_otype = otype;
	  int added = 0;
	  int discarded = 0;

	  /* Check that the qualifiers on IN_TYPE are a superset of
	     the qualifiers of IN_OTYPE.  The outermost level of
	     POINTER_TYPE nodes is uninteresting and we stop as soon
	     as we hit a non-POINTER_TYPE node on either type.  */
	  do
	    {
	      in_otype = TREE_TYPE (in_otype);
	      in_type = TREE_TYPE (in_type);

	      /* GNU C allows cv-qualified function types.  'const'
		 means the function is very pure, 'volatile' means it
		 can't return.  We need to warn when such qualifiers
		 are added, not when they're taken away.  */
	      if (TREE_CODE (in_otype) == FUNCTION_TYPE
		  && TREE_CODE (in_type) == FUNCTION_TYPE)
		added |= (TYPE_QUALS (in_type) & ~TYPE_QUALS (in_otype));
	      else
		discarded |= (TYPE_QUALS (in_otype) & ~TYPE_QUALS (in_type));
	    }
	  while (TREE_CODE (in_type) == POINTER_TYPE
		 && TREE_CODE (in_otype) == POINTER_TYPE);

	  if (added)
	    warning ("cast adds new qualifiers to function type");

	  if (discarded)
	    /* There are qualifiers present in IN_OTYPE that are not
	       present in IN_TYPE.  */
	    warning ("cast discards qualifiers from pointer target type");
	}

      /* Warn about possible alignment problems.  */
      if (STRICT_ALIGNMENT && warn_cast_align
	  && TREE_CODE (type) == POINTER_TYPE
	  && TREE_CODE (otype) == POINTER_TYPE
	  && TREE_CODE (TREE_TYPE (otype)) != VOID_TYPE
	  && TREE_CODE (TREE_TYPE (otype)) != FUNCTION_TYPE
	  /* Don't warn about opaque types, where the actual alignment
	     restriction is unknown.  */
	  && !((TREE_CODE (TREE_TYPE (otype)) == UNION_TYPE
		|| TREE_CODE (TREE_TYPE (otype)) == RECORD_TYPE)
	       && TYPE_MODE (TREE_TYPE (otype)) == VOIDmode)
	  && TYPE_ALIGN (TREE_TYPE (type)) > TYPE_ALIGN (TREE_TYPE (otype)))
	warning ("cast increases required alignment of target type");
#endif
      if (TREE_CODE (type) == INTEGER_TYPE
	  && TREE_CODE (otype) == POINTER_TYPE
	  && TYPE_PRECISION (type) != TYPE_PRECISION (otype)
	  && !TREE_CONSTANT (value))
	warning ("cast from pointer to integer of different size");

#if !defined(GM2)
      if (warn_bad_function_cast
	  && TREE_CODE (value) == CALL_EXPR
	  && TREE_CODE (type) != TREE_CODE (otype))
	warning ("cast does not match function type");
#endif

      if (TREE_CODE (type) == POINTER_TYPE
	  && TREE_CODE (otype) == INTEGER_TYPE
	  && TYPE_PRECISION (type) != TYPE_PRECISION (otype)
	  /* Don't warn about converting any constant.  */
	  && !TREE_CONSTANT (value))
	warning ("cast to pointer from integer of different size");

      ovalue = value;
      value = convert (type, value);

      /* Ignore any integer overflow caused by the cast.  */
      if (TREE_CODE (value) == INTEGER_CST)
	{
	  TREE_OVERFLOW (value) = TREE_OVERFLOW (ovalue);
	  TREE_CONSTANT_OVERFLOW (value) = TREE_CONSTANT_OVERFLOW (ovalue);
	}
    }

  /* Pedantically, don't let (void *) (FOO *) 0 be a null pointer constant.  */
  if (pedantic && TREE_CODE (value) == INTEGER_CST
      && TREE_CODE (expr) == INTEGER_CST
      && TREE_CODE (TREE_TYPE (expr)) != INTEGER_TYPE)
    value = non_lvalue (value);

  /* If pedantic, don't let a cast be an lvalue.  */
  if (value == expr && pedantic)
    value = non_lvalue (value);

  return value;
}
#endif

/*
 *  BuildCoerce - returns a tree containing the expression, expr, after
 *                it has been coersed to, type.
 */

tree
gccgm2_BuildCoerce (des, type, expr)
     tree des, type, expr;
{
  tree copy = copy_node (expr);
  TREE_TYPE (copy) = type;
  
  return build_modify_expr (des, NOP_EXPR, copy);
  /* return copy; */
  /*  return build_m2_cast (type, expr); */
}

/*
 *  BuildTrunc - returns an integer expression from a REAL or LONGREAL op1.
 */

tree
gccgm2_BuildTrunc (op1)
     tree op1;
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
#if 1
  if (code == ENUMERAL_TYPE)
    error ("not expecting to negate an enumerated value");
#endif

  return build_unary_op (NEGATE_EXPR, op1, needconvert);
}

/*
 *  BuildSetNegate - builds a set negate expression and returns the tree.
 */

tree
gccgm2_BuildSetNegate (op1, needconvert)
     tree op1;
     int  needconvert;
{
  return build_binary_op (BIT_XOR_EXPR,
			  gccgm2_BuildConvert(gccgm2_GetWordType (), op1),
			  set_full_complement,
			  needconvert);
}

/*
 *  gccgm2_GetSizeOfInBits - returns the number of bits used to contain, type.
 */

tree
gccgm2_GetSizeOfInBits (type)
     tree type;
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
gccgm2_GetSizeOf (type)
     tree type;
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
gccgm2_BuildSize (op1, needconvert)
     tree op1;
     int  needconvert ATTRIBUTE_UNUSED;
{
  return gccgm2_GetSizeOf(op1);
}


/*
 *  BuildAddr - builds an expression which calculates the address of op1 and returns the tree.
 */

tree
gccgm2_BuildAddr (op1, needconvert)
     tree op1;
     int  needconvert;
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
 *  BuildOffset - builds an expression containing the number of bytes the field
 *                is offset from the start of the record structure.
 *                The expression is returned.
 */

tree
gccgm2_BuildOffset (field, needconvert)
     tree field;
     int  needconvert ATTRIBUTE_UNUSED;
{
  return gccgm2_BuildConvert (gccgm2_GetIntegerType (),
			      gccgm2_BuildAdd (DECL_FIELD_OFFSET (field),
					       gccgm2_BuildDiv (DECL_FIELD_BIT_OFFSET (field),
								gccgm2_BuildIntegerConstant (BITS_PER_UNIT),
								FALSE),
					       FALSE));
}

/*
 *  BuildLogicalOrAddress - build a logical or expressions and return the tree.
 */

tree
gccgm2_BuildLogicalOrAddress (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return build_binary_op (BIT_IOR_EXPR, op1, op2, needconvert);
}

/*
 *  BuildLogicalOr - build a logical or expressions and return the tree.
 */

tree
gccgm2_BuildLogicalOr (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return build_binary_op (BIT_IOR_EXPR,
			  gccgm2_BuildConvert (gccgm2_GetWordType (), op1),
			  gccgm2_BuildConvert (gccgm2_GetWordType (), op2), needconvert);
}

/*
 *  BuildLogicalAnd - build a logical and expression and return the tree.
 */

tree
gccgm2_BuildLogicalAnd (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return build_binary_op (BIT_AND_EXPR,
			  gccgm2_BuildConvert (gccgm2_GetWordType (), op1),
			  gccgm2_BuildConvert (gccgm2_GetWordType (), op2), needconvert);
}

/*
 *  BuildSymmetricalDifference - build a logical xor expression and return the tree.
 */

tree
gccgm2_BuildSymmetricDifference (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return build_binary_op (BIT_XOR_EXPR,
			  gccgm2_BuildConvert (gccgm2_GetWordType (), op1),
			  gccgm2_BuildConvert (gccgm2_GetWordType (), op2), needconvert);
}


/*
 *  BuildLogicalDifference - build a logical difference expression and
 *                           return the tree.
 *                           (op1 and (not op2))
 */

tree
gccgm2_BuildLogicalDifference (op1, op2, needconvert)
     tree op1, op2;
     int  needconvert;
{
  return build_binary_op (BIT_AND_EXPR,
			  gccgm2_BuildConvert (gccgm2_GetWordType (), op1),
			  gccgm2_BuildSetNegate (op2, needconvert),
			  needconvert);
}

/*
 *  create_label_from_name - returns a tree label.
 */

tree
create_label_from_name (name)
     char *name;
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
  tree id = get_identifier (name);
  tree decl = lookup_label (id);

  if (decl == 0)
    error ("problems trying to declare a label");
  else {
    TREE_USED (decl) = 1;
    DECL_SOURCE_FILE (decl) = input_filename;
    DECL_SOURCE_LINE (decl) = lineno;
  }
  expand_label (decl);
  return decl;
}

/*
 *  BuildLessThan - return a tree which computes <
 */

tree
gccgm2_BuildLessThan (op1, op2)
     tree op1, op2;
{
  return build_binary_op (LT_EXPR, op1, op2, 0);
}

/*
 *  BuildGreaterThan - return a tree which computes >
 */

tree
gccgm2_BuildGreaterThan (op1, op2)
     tree op1, op2;
{
  return build_binary_op (GT_EXPR, op1, op2, 0);
}


/*
 *  BuildLessThanOrEqual - return a tree which computes <
 */

tree
gccgm2_BuildLessThanOrEqual (op1, op2)
     tree op1, op2;
{
  return build_binary_op (LE_EXPR, op1, op2, TRUE);
}


/*
 *  BuildGreaterThanOrEqual - return a tree which computes >=
 */

tree
gccgm2_BuildGreaterThanOrEqual (op1, op2)
     tree op1, op2;
{
  return build_binary_op (GE_EXPR, op1, op2, 0);
}


/*
 *  BuildEqualTo - return a tree which computes =
 */

tree
gccgm2_BuildEqualTo (op1, op2)
     tree op1, op2;
{
  return build_binary_op (EQ_EXPR, op1, op2, 0);
}


/*
 *  BuildEqualNotTo - return a tree which computes #
 */

tree
gccgm2_BuildNotEqualTo (op1, op2)
     tree op1, op2;
{
  return build_binary_op (NE_EXPR, op1, op2, 0);
}

/*
 *  BuildIsSuperset - return a tree which computes:  op1 & op2 == op2
 */

tree
gccgm2_BuildIsSuperset (op1, op2)
     tree op1, op2;
{
  return gccgm2_BuildEqualTo (op2,
			      gccgm2_BuildLogicalAnd (op1, op2, FALSE));
}

/*
 *  BuildIsNotSuperset - return a tree which computes: op1 & op2 != op2
 */

tree
gccgm2_BuildIsNotSuperset (op1, op2)
     tree op1, op2;
{
  return gccgm2_BuildNotEqualTo (op2,
				 gccgm2_BuildLogicalAnd (op1, op2, FALSE));
}

/*
 *  BuildIsSubset - return a tree which computes:  op1 & op2 == op1
 */

tree
gccgm2_BuildIsSubset (op1, op2)
     tree op1, op2;
{
  return gccgm2_BuildEqualTo (op1,
			      gccgm2_BuildLogicalAnd (op1, op2, FALSE));
}

/*
 *  BuildIsNotSubset - return a tree which computes: op1 & op2 != op1
 */

tree
gccgm2_BuildIsNotSubset (op1, op2)
     tree op1, op2;
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
do_jump_if_bit (code, word, bit, label)
     enum tree_code code;
     tree word, bit;
     char *label;
{
  gccgm2_DoJump (build_binary_op (code,
				  build_binary_op (BIT_AND_EXPR,
						   gccgm2_BuildConvert (gccgm2_GetWordType (), word),
						   gccgm2_BuildConvert (gccgm2_GetWordType (), gccgm2_BuildLSL (gccgm2_GetWordOne(),
														gccgm2_BuildConvert (gccgm2_GetWordType (), bit),
														FALSE)),
						   FALSE),
				  integer_zero_node, FALSE),
		 NULL, label);
}

/*
 *  BuildIfConstInVar - generates: if constel in varset then goto label.
 */

void
gccgm2_BuildIfConstInVar (type, varset, constel, is_lvalue, fieldno, label)
     tree type, varset, constel;
     int is_lvalue, fieldno;
     char *label;
{
  tree size = gccgm2_GetSizeOf (type);

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
gccgm2_BuildIfNotConstInVar (type, varset, constel, is_lvalue, fieldno, label)
     tree type, varset, constel;
     int is_lvalue, fieldno;
     char *label;
{
  tree size = gccgm2_GetSizeOf (type);

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
gccgm2_BuildIfVarInVar (type, varset, varel, is_lvalue, low, high, label)
     tree  type, varset, varel;
     int   is_lvalue;
     tree  low;
     tree  high ATTRIBUTE_UNUSED;
     char *label;
{
  tree size = gccgm2_GetSizeOf (type);

  if (gccgm2_CompareTrees (size, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    do_jump_if_bit (NE_EXPR, get_rvalue (varset, type, is_lvalue), varel, label);
  else {
    tree p1               = get_set_address (varset, is_lvalue);
    /* calculate the index from the first bit */
    tree index_0          = gccgm2_BuildSub (gccgm2_BuildConvert (gccgm2_GetIntegerType(), varel),
					     gccgm2_BuildConvert (gccgm2_GetIntegerType(), low), FALSE);
    /* which word do we need to fetch? */
    tree word_index       = gccgm2_BuildDiv (index_0, gccgm2_BuildIntegerConstant (SET_WORD_SIZE), FALSE);
    /* calculate the bit in this word */
    tree offset_into_word = gccgm2_BuildMod (index_0, gccgm2_BuildIntegerConstant (SET_WORD_SIZE), FALSE);

    /* calculate the address of the word we are interested in */
    p1 = gccgm2_BuildAdd (p1, gccgm2_BuildMult (word_index, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT),
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
gccgm2_BuildIfNotVarInVar (type, varset, varel, is_lvalue, low, high, label)
     tree  type, varset, varel;
     int   is_lvalue;
     tree  low;
     tree  high ATTRIBUTE_UNUSED;
     char *label;
{
  tree size = gccgm2_GetSizeOf (type);

  if (gccgm2_CompareTrees (size, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    do_jump_if_bit (EQ_EXPR, get_rvalue (varset, type, is_lvalue), varel, label);
  else {
    tree p1               = get_set_address (varset, is_lvalue);
    /* calculate the index from the first bit */
    tree index_0          = gccgm2_BuildSub (gccgm2_BuildConvert (gccgm2_GetIntegerType (), varel),
					     gccgm2_BuildConvert (gccgm2_GetIntegerType (), low), FALSE);
    /* which word do we need to fetch? */
    tree word_index       = gccgm2_BuildDiv (index_0, gccgm2_BuildIntegerConstant (SET_WORD_SIZE), FALSE);
    /* calculate the bit in this word */
    tree offset_into_word = gccgm2_BuildMod (index_0, gccgm2_BuildIntegerConstant (SET_WORD_SIZE), FALSE);

    /* calculate the address of the word we are interested in */
    p1 = gccgm2_BuildAdd (p1, gccgm2_BuildMult (word_index, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT),
						FALSE),
			  FALSE);

    /* fetch the word, extract the bit and test for == 0 */
    do_jump_if_bit (EQ_EXPR, gccgm2_BuildIndirect (p1, bitset_type_node), offset_into_word, label);
  }
}

#if 0
/*
 *  BuildIfConstSetEquVar - builds the if var == constset then label
 */

void
gccgm2_BuildIfConstSetEquVar (type, constset, varset, is_lvalue, label)
     tree type, constset, varset;
     int  is_lvalue;
     char *label;
{
  tree p1 = get_set_address (varset, is_lvalue);
  tree fieldlist = TYPE_FIELDS (type);
  tree constlist = CONSTRUCTOR_ELTS (constset);
  tree field;
  tree constfield= constlist;

  for (field = fieldlist; (field != NULL) && (constfield != NULL); field = TREE_CHAIN (field)) {
    tree constvalue = TREE_VALUE (constfield);
    tree fieldvalue = gccgm2_BuildIndirect (get_set_field_lhs (p1, field), gccgm2_GetWordType ());

    if (constvalue == NULL) {
      error ("constant set has a different number of fields than the set variable");
      return;
    }
    gccgm2_DoJump (gccgm2_BuildEqualTo (gccgm2_BuildConvert (gccgm2_GetWordType (), fieldvalue),
					gccgm2_BuildConvert (gccgm2_GetWordType (), constvalue)),
		   NULL, label);
    constfield = TREE_CHAIN (constfield);
  }
}

/*
 *  BuildIfNotConstSetEquVar - builds the if varset != constset then label
 */

void
gccgm2_BuildIfNotConstSetEquVar (type, constset, varset, is_lvalue, label)
     tree type, constset, varset;
     int  is_lvalue;
     char *label;
{
  tree p1 = get_set_address (varset, is_lvalue);
  tree fieldlist = TYPE_FIELDS (type);
  tree constlist = CONSTRUCTOR_ELTS (constset);
  tree field;
  tree constfield= constlist;

  for (field = fieldlist; (field != NULL) && (constfield != NULL); field = TREE_CHAIN (field)) {
    tree constvalue = TREE_VALUE (constfield);
    tree fieldvalue = gccgm2_BuildIndirect (get_set_field_lhs (p1, field), gccgm2_GetWordType ());

    if (constvalue == NULL) {
      error ("constant set has a different number of fields than the set variable");
      return;
    }
    gccgm2_DoJump (gccgm2_BuildNotEqualTo (gccgm2_BuildConvert (gccgm2_GetWordType (), fieldvalue),
					   gccgm2_BuildConvert (gccgm2_GetWordType (), constvalue)),
		   NULL, label);
    constfield = TREE_CHAIN (constfield);
  }
}

void
gccgm2_BuildIfVarEquVar (type, op1, op2, is_op1lvalue, is_op2lvalue, label)
     tree type, op1, op2;
     int is_op1lvalue, is_op2lvalue;
     char *label;
{
  tree p1 = get_set_address (op1, is_op1lvalue);
  tree p2 = get_set_address (op2, is_op2lvalue);
  tree fieldlist = TYPE_FIELDS (type);
  tree field;

  for (field = fieldlist; field != NULL; field = TREE_CHAIN (field)) {
    tree fieldvalue1 = gccgm2_BuildIndirect (get_set_field_lhs (p1, field), gccgm2_GetWordType ());
    tree fieldvalue2 = gccgm2_BuildIndirect (get_set_field_lhs (p2, field), gccgm2_GetWordType ());

    gccgm2_DoJump (gccgm2_BuildEqualTo (fieldvalue1, fieldvalue2), NULL, label);
  }
}

void
gccgm2_BuildIfNotVarEquVar (type, op1, op2, is_op1lvalue, is_op2lvalue, label)
     tree type, op1, op2;
     int is_op1lvalue, is_op2lvalue;
     char *label;
{
  tree p1 = get_set_address (op1, is_op1lvalue);
  tree p2 = get_set_address (op2, is_op2lvalue);
  tree fieldlist = TYPE_FIELDS (type);
  tree field;

  for (field = fieldlist; field != NULL; field = TREE_CHAIN (field)) {
    tree fieldvalue1 = gccgm2_BuildIndirect (get_set_field_lhs (p1, field), gccgm2_GetWordType ());
    tree fieldvalue2 = gccgm2_BuildIndirect (get_set_field_lhs (p2, field), gccgm2_GetWordType ());

    gccgm2_DoJump (gccgm2_BuildNotEqualTo (fieldvalue1, fieldvalue2), NULL, label);
  }
}
#endif

/*
 *  get_set_address_if_var - returns the address of, op, providing
 *                           it is not a constant.
 *                           NULL is returned if, op, is a constant.
 */

static
tree
get_set_address_if_var (op, is_lvalue, is_const)
     tree op;
     int  is_lvalue, is_const;
{
  if (is_const)
    return NULL;
  else
    return get_set_address (op, is_lvalue);
}

/*
 *  get_field_list - returns the field list for, op. The struct field
 *                   list of the type is returned if, op, is a variable.
 *                   This allows the calling function to dereference
 *                   the field using a pointer to the variable.
 *                   Alternatively if, op, is a constant then the first
 *                   member of the constant structure is returned.
 */

static
tree
get_field_list (type, op, is_const)
     tree type, op;
     int is_const;
{
  if (is_const)
    return CONSTRUCTOR_ELTS (op);
  else
    return TYPE_FIELDS (type);
}

/*
 *  get_set_value - returns the value indicated by, field, in the set.
 *                  Either p->field or the constant(field) is returned.
 */

static
tree
get_set_value (p, field, is_const)
     tree p, field;
     int  is_const;
{
  if (is_const)
    return TREE_VALUE (field);
  else
    return gccgm2_BuildIndirect (get_set_field_lhs (p, field), gccgm2_GetWordType ());
}

/*
 *  BuildForeachWordInSetDoIfExpr - foreach word in set, type, compute the expression, expr, and if true
 *                                  goto label.
 */

void
gccgm2_BuildForeachWordInSetDoIfExpr (type, op1, op2,
				      is_op1lvalue, is_op2lvalue,
				      is_op1const, is_op2const,
				      expr, label)
     tree type, op1, op2;
     int  is_op1lvalue, is_op2lvalue;
     int  is_op1const, is_op2const;
     tree (*expr) (tree, tree);
     char *label;
{
  tree p1 = get_set_address_if_var (op1, is_op1lvalue, is_op1const);
  tree p2 = get_set_address_if_var (op2, is_op2lvalue, is_op2const);
  tree field1 = get_field_list (type, op1, is_op1const);
  tree field2 = get_field_list (type, op2, is_op2const);

  while (field1 != NULL && field2 != NULL) {
    gccgm2_DoJump ((*expr) (get_set_value (p1, field1, is_op1const),
			    get_set_value (p2, field2, is_op2const)),
		   NULL, label);
    field1 = TREE_CHAIN (field1);
    field2 = TREE_CHAIN (field2);
  }
  if (field1 != NULL || field2 != NULL)
    error ("the two sets have a different number of fields");
}

/*
 *  BuildIfInRangeGoto - if var is in the range low..high then goto label
 */

void
gccgm2_BuildIfInRangeGoto (var, low, high, label)
     tree var, low, high;
     char *label;
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
gccgm2_BuildIfNotInRangeGoto (var, low, high, label)
     tree var, low, high;
     char *label;
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

  return build1 (INDIRECT_REF, skip_type_decl(type), target);
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
  if ((falselabel != NULL) && (truelabel == NULL))
    do_jump(exp, label_rtx(create_label_from_name(falselabel)), NULL_RTX);
  else if ((falselabel == NULL) && (truelabel != NULL))
    do_jump(exp, NULL_RTX, label_rtx(create_label_from_name(truelabel)));
  else
    error("expecting one and only one label to be declared");
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
gccgm2_BuildProcedureCall (procedure, rettype)
     tree procedure, rettype;
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

#if 0
    fprintf(stderr, "built the modula-2 call, here are the params\n"); fflush(stderr);
    debug_tree (param_list);
#endif
#if defined(DEBUG_PROCEDURE_CALLS)
    fprintf(stderr, "built the modula-2 call, here is the tree\n"); fflush(stderr);
    debug_tree (call);
#endif
    expand_expr_stmt (call);
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
gccgm2_BuildIndirectProcedureCall (procedure, rettype)
     tree procedure, rettype;
{
  tree call;

  TREE_USED(procedure) = TRUE;

  if (rettype == NULL_TREE) {
    rettype = void_type_node;
    call = build (CALL_EXPR, rettype, procedure, param_list, NULL_TREE);
    TREE_USED (call)         = TRUE;
    TREE_SIDE_EFFECTS (call) = TRUE ;

#if 0
    fprintf(stderr, "built the modula-2 call, here are the params\n"); fflush(stderr);
    debug_tree(param_list);
#endif
#if defined(DEBUG_PROCEDURE_CALLS)
    fprintf(stderr, "built the modula-2 call, here is the tree\n"); fflush(stderr);
    debug_tree(call);
#endif
    expand_expr_stmt (call);
    last_function   = NULL_TREE;
  } else
    last_function   = build (CALL_EXPR, skip_type_decl (rettype), procedure, param_list, NULL_TREE);

  param_list = NULL_TREE;   /* ready for the next time we call a procedure */
  return last_function;
}

/*
 *  BuildFunctValue - generates code for value := last_function(foobar);
 */

void
gccgm2_BuildFunctValue (value)
     tree value;
{
  tree assign = build_modify_expr (value, NOP_EXPR, last_function);

  TREE_SIDE_EFFECTS (assign) = TRUE;
  TREE_USED (assign) = TRUE;
  expand_expr_stmt (assign);
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
  TREE_TYPE (boolean_false_node) = booleanid;
  TREE_TYPE (boolean_true_node)  = booleanid;
}

tree
gccgm2_GetIntegerType ()
{
  return integer_type_node;
}

tree
gccgm2_GetCharType ()
{
  return char_type_node;
}

tree
gccgm2_GetByteType ()
{
  return unsigned_char_type_node;
}

tree
gccgm2_GetVoidType ()
{
  return void_type_node;
}

tree
gccgm2_GetPointerType ()
{
  return ptr_type_node;
}

tree
gccgm2_GetCardinalType ()
{
  return unsigned_type_node;
}

tree
gccgm2_GetBitsetType ()
{
  return bitset_type_node;
}

tree
gccgm2_GetBitnumType ()
{
  return bitnum_type_node;
}

tree
gccgm2_GetRealType ()
{
  return double_type_node;
}

tree
gccgm2_GetLongRealType ()
{
  return long_double_type_node;
}

tree
gccgm2_GetShortRealType ()
{
  return float_type_node;
}

tree
gccgm2_GetLongIntType ()
{
  return long_integer_type_node;
}

tree
gccgm2_GetWordType ()
{
  return unsigned_type_node;
}

tree
gccgm2_GetISOLocType ()
{
  return m2_iso_loc_type_node;
}

tree
gccgm2_GetISOByteType ()
{
  return m2_iso_byte_type_node;
}

tree
gccgm2_GetISOWordType ()
{
  return m2_iso_word_type_node;
}

tree
gccgm2_GetProcType ()
{
  return proc_type_node;
}

tree
gccgm2_GetM2CharType ()
{
  return m2_char_type_node;
}

tree
gccgm2_GetM2IntegerType ()
{
  return m2_integer_type_node;
}

tree
gccgm2_GetM2CardinalType ()
{
  return m2_cardinal_type_node;
}

tree
gccgm2_GetM2ShortRealType ()
{
  return m2_short_real_type_node;
}

tree
gccgm2_GetM2RealType ()
{
  return m2_real_type_node;
}

tree
gccgm2_GetM2LongRealType ()
{
  return m2_long_real_type_node;
}

tree
gccgm2_GetM2LongIntType ()
{
  return m2_long_int_type_node;
}

tree
gccgm2_GetM2LongCardType ()
{
  return m2_long_card_type_node;
}

tree
gccgm2_GetM2ShortIntType ()
{
  return m2_short_int_type_node;
}

tree
gccgm2_GetShortIntType ()
{
  return short_integer_type_node;
}

tree
gccgm2_GetM2ShortCardType ()
{
  return m2_short_card_type_node;
}

tree
gccgm2_GetShortCardType ()
{
  return short_unsigned_type_node;
}

tree
gccgm2_GetIntegerZero ()
{
  return integer_zero_node;
}

tree
gccgm2_GetIntegerOne ()
{
  return integer_one_node;
}

tree
gccgm2_GetWordZero ()
{
  return gccgm2_ToWord(integer_zero_node);
}

tree
gccgm2_GetWordOne ()
{
  return gccgm2_ToWord(integer_one_node);
}

tree
gccgm2_GetPointerZero ()
{
  return convertToPtr (integer_zero_node);
}

tree
gccgm2_GetPointerOne ()
{
  return convertToPtr (integer_one_node);
}

tree
gccgm2_GetCurrentFunction ()
{
  return current_function_decl;
}

tree
gccgm2_GetErrorNode ()
{
  return error_mark_node;
}

/*
 *  convertToPtr - if the type of tree, t, is not a ptr_type_node then convert it.
 */

tree
convertToPtr (t)
     tree t;
{
  if (TREE_CODE (TREE_TYPE (t)) == POINTER_TYPE)
    return t;
  else
    return gccgm2_BuildConvert (ptr_type_node, t);
}

/*
 *  AreConstantsEqual - maps onto tree.c (tree_int_cst_equal). It returns
 *                      TRUE if the value of e1 is the same as e2.
 */

int
gccgm2_AreConstantsEqual (e1, e2)
  tree e1, e2;
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
gccgm2_DetermineSign (e)
     tree e;
{
  return tree_int_cst_sgn(e);
}

static
tree
get_tree_val (e)
     tree e;
{
  switch (TREE_CODE (e)) {

  case CONST_DECL:
    return DECL_INITIAL (e);
    break;
  case INTEGER_CST:
  case REAL_CST:
    return e;

  default:
    error ("not expecting this type when evaluating constant");
    return ERROR_MARK;
  }
}  

/*
 *  CompareTrees - returns -1 if e1 < e2, 0 if e1 == e2, and 1 if e1 > e2.
 */

int gccgm2_CompareTrees (e1, e2)
     tree e1, e2;
{
  return tree_int_cst_compare (get_tree_val (e1), get_tree_val (e2));
}

/*
 *  get_rvalue - returns the rvalue of t. The, type, is the object type to be
 *               copied upon indirection.
 */

static
tree
get_rvalue (t, type, is_lvalue)
     tree t, type;
     int  is_lvalue;
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
get_set_address (op1, is_lvalue)
     tree op1;
     int  is_lvalue;
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
get_set_field_lhs (p, field)
     tree p, field;
{
  return gccgm2_BuildAdd (p, gccgm2_BuildOffset (field, FALSE), FALSE);
}

/*
 *  get_set_field_rhs - returns the value of p->field.
 */

static
tree
get_set_field_rhs (p, field)
     tree p, field;
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
gccgm2_BuildBinaryForeachWordDo (type, op1, op2, op3, binop,
				 is_op1lvalue, is_op2lvalue, is_op3lvalue,
				 is_op1const, is_op2const, is_op3const)
     tree  type, op1, op2, op3;
     tree  (*binop)(tree, tree, int);
     int   is_op1lvalue, is_op2lvalue, is_op3lvalue;
     int   is_op1const, is_op2const, is_op3const;
{
  tree size = gccgm2_GetSizeOf (type);

  if (gccgm2_CompareTrees (size, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    gccgm2_BuildAssignment (get_rvalue (op1, type, is_op1lvalue),
			    (*binop) (get_rvalue (op2, type, is_op2lvalue),
				      get_rvalue (op3, type, is_op3lvalue), FALSE));
  else {
    /* large set size > TSIZE(WORD) */

    tree p1         = get_set_address (op1, is_op1lvalue);
    tree p2         = get_set_address_if_var (op2, is_op2lvalue, is_op2const);
    tree p3         = get_set_address_if_var (op3, is_op3lvalue, is_op3const);
    tree field1     = get_field_list (type, op1, is_op1const);
    tree field2     = get_field_list (type, op2, is_op2const);
    tree field3     = get_field_list (type, op3, is_op3const);

    if (is_op1const)
      error("internal error: not expecting operand1 to be a constant set");

    while (field1 != NULL && field2 != NULL && field3 != NULL) {
      gccgm2_BuildAssignment (get_set_field_rhs (p1, field1),
			      (*binop) (get_set_value (p2, field2,
						       is_op2const),
					get_set_value (p3, field3,
						       is_op3const), FALSE));
      field1 = TREE_CHAIN (field1);
      field2 = TREE_CHAIN (field2);
      field3 = TREE_CHAIN (field3);
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
gccgm2_BuildUnaryForeachWordDo (type, op1, op2, unop,
				is_op1lvalue, is_op2lvalue,
				is_op1const, is_op2const)
     tree  type, op1, op2;
     tree  (*unop)(tree, int);
     int   is_op1lvalue, is_op2lvalue;
     int   is_op1const, is_op2const;
{
  tree size = gccgm2_GetSizeOf (type);

  if (gccgm2_CompareTrees (size, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    gccgm2_BuildAssignment (get_rvalue (op1, type, is_op1lvalue),
			    (*unop) (get_rvalue (op2, type, is_op2lvalue), FALSE));
  else {
    /* large set size > TSIZE(WORD) */

    tree p1        = get_set_address (op1, is_op1lvalue);
    tree p2        = get_set_address_if_var (op2, is_op2lvalue, is_op2const);
    tree field1    = get_field_list (type, op1, is_op1const);
    tree field2    = get_field_list (type, op2, is_op2const);

    if (is_op1const)
      error("internal error: not expecting operand1 to be a constant set");

    while (field1 != NULL && field2 != NULL) {
      gccgm2_BuildAssignment (get_set_field_rhs (p1, field1),
			      (*unop) (get_set_value (p2, field2,
						      is_op2const),
				       FALSE));
      field1 = TREE_CHAIN (field1);
      field2 = TREE_CHAIN (field2);
    }
  }
}

/*
 *  BuildExcludeVarConst - builds the EXCL(op1, 1<<op2) operation for a small sets. Large
 *                         sets call this routine to exclude the bit in the particular word.
 *                         op2 is a constant.
 */

void
gccgm2_BuildExcludeVarConst (type, op1, op2, is_lvalue, fieldno)
     tree type, op1, op2;
     int  is_lvalue, fieldno;
{
  tree size = gccgm2_GetSizeOf (type);

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
gccgm2_BuildExcludeVarVar (type, varset, varel, is_lvalue, low)
     tree  type, varset, varel;
     int   is_lvalue;
     tree  low;
{
  tree size = gccgm2_GetSizeOf (type);

  if (gccgm2_CompareTrees (size, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    gccgm2_BuildAssignment (get_rvalue (varset, type, is_lvalue),
			    gccgm2_BuildLogicalAnd (get_rvalue (varset, type, is_lvalue),
						    gccgm2_BuildSetNegate (gccgm2_BuildLSL (gccgm2_GetWordOne(), varel, FALSE),
									   FALSE),
						    FALSE));
  else {
    tree p1               = get_set_address (varset, is_lvalue);
    /* calculate the index from the first bit */
    tree index_0          = gccgm2_BuildSub (gccgm2_BuildConvert (gccgm2_GetIntegerType(), varel),
					     gccgm2_BuildConvert (gccgm2_GetIntegerType(), low), FALSE);
    /* which word do we need to fetch? */
    tree word_index       = gccgm2_BuildDiv (index_0, gccgm2_BuildIntegerConstant (SET_WORD_SIZE), FALSE);
    /* calculate the bit in this word */
    tree offset_into_word = gccgm2_BuildMod (index_0, gccgm2_BuildIntegerConstant (SET_WORD_SIZE), FALSE);

    /* calculate the address of the word we are interested in */
    p1 = gccgm2_BuildAdd (p1, gccgm2_BuildMult (word_index, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT),
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
gccgm2_BuildIncludeVarConst (type, op1, op2, is_lvalue, fieldno)
     tree type, op1, op2;
     int  is_lvalue, fieldno;
{
  tree size = gccgm2_GetSizeOf (type);

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
gccgm2_BuildIncludeVarVar (type, varset, varel, is_lvalue, low)
     tree  type, varset, varel;
     int   is_lvalue;
     tree  low;
{
  tree size = gccgm2_GetSizeOf (type);

  if (gccgm2_CompareTrees (size, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT)) <= 0)
    /* small set size <= TSIZE(WORD) */
    gccgm2_BuildAssignment (get_rvalue (varset, type, is_lvalue),
			    gccgm2_BuildLogicalOr (get_rvalue (varset, type, is_lvalue),
						   gccgm2_BuildLSL (gccgm2_GetWordOne(), varel, FALSE),
						   FALSE));
  else {
    tree p1               = get_set_address (varset, is_lvalue);
    /* calculate the index from the first bit */
    tree index_0          = gccgm2_BuildSub (gccgm2_BuildConvert (gccgm2_GetIntegerType(), varel),
					     gccgm2_BuildConvert (gccgm2_GetIntegerType(), low), FALSE);
    /* which word do we need to fetch? */
    tree word_index       = gccgm2_BuildDiv (index_0, gccgm2_BuildIntegerConstant (SET_WORD_SIZE), FALSE);
    /* calculate the bit in this word */
    tree offset_into_word = gccgm2_BuildMod (index_0, gccgm2_BuildIntegerConstant (SET_WORD_SIZE), FALSE);

    /* calculate the address of the word we are interested in */
    p1 = gccgm2_BuildAdd (p1, gccgm2_BuildMult (word_index, gccgm2_BuildIntegerConstant (SET_WORD_SIZE/BITS_PER_UNIT),
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
  tree id;

  switch (value) {

  case 0:  return integer_zero_node;
  case 1:  return integer_one_node;

  default:
      id = build_int_2 (value, 0);
      return id;
  }
}

/*
 *  BuildConstLiteralNumber - returns a GCC TREE built from the string, str.
 *                            It assumes that, str, represents a legal
 *                            number in Modula-2. It always returns a
 *                            positive value.
 */

tree
gccgm2_BuildConstLiteralNumber (str, base)
     const char *str;
     unsigned int base;
{
  tree value;
  unsigned HOST_WIDE_INT low;
  HOST_WIDE_INT high;
  int overflow;

  overflow = interpret_integer (str, base,
				&low, (HOST_WIDE_INT *) &high);
  value = build_int_2_wide (low, high);
#if 1
  if (high < 0)
    TREE_UNSIGNED (value) = TRUE;
#endif

  return value;
}

/*
 * interpret_integer - converts an integer constant into two integer
 *                     constants. Heavily borrowed from gcc/cppexp.c.
 */

static int
interpret_integer (str, base, low, high)
     const char *str;
     unsigned int base;
     unsigned HOST_WIDE_INT *low;
     HOST_WIDE_INT *high;
{
  const unsigned char *p, *end;
  int overflow = FALSE;
  int len;

  *low = 0;
  *high = 0;
  p = str;
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

      if (overflow)
	error ("integer constant is too large");
    }
  return overflow;
}

/* Append DIGIT to NUM, a number of PRECISION bits being read in base
   BASE.  */
static int
append_digit (low, high, digit, base)
     unsigned HOST_WIDE_INT *low;
     HOST_WIDE_INT *high;
     unsigned int digit;
     unsigned int base;
{
  unsigned int shift;
  int overflow;
  HOST_WIDE_INT add_high, res_high;
  unsigned HOST_WIDE_INT int add_low, res_low;

  switch (base) {

  case 2:  shift = 1; break;
  case 8:  shift = 3; break;
  case 10: shift = 3; break;
  case 16:  shift = 4; break;

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

tree mystr;

/*
 *  BuildStringConstant - creates a string constant given a, string,
 *                        and, length.
 */

tree
gccgm2_BuildStringConstant (string, length)
     char *string;
     int   length;
{
  tree id;

  id = build_string (length+1, string);  /* +1 ensures that we always nul terminate our strings */
  TREE_TYPE (id) = char_array_type_node;
  mystr = id;
  return id;
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
  return id;
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
  expr = fold (expr);
  STRIP_NOPS (expr);
  return convert_and_check (skip_type_decl (type), get_tree_val (expr));
}

/*
 *  ToWord - converts an expression (Integer or Ordinal type) into
 *           a WORD.
 */

tree
gccgm2_ToWord (expr)
     tree expr;
{
  return gccgm2_BuildConvert (gccgm2_GetWordType(), expr);
}

/*
 *  RealToTree - convert a real number into a Tree.
 */

tree
gccgm2_RealToTree (name)
     char *name;
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
gccgm2_BuildStart (name, line, inner_module)
     char *name;
     int   line;
     int   inner_module;
{
  tree fntype;
  tree fndecl;

  /* The function type depends on the return type and type of args.  */
  fntype = build_function_type (integer_type_node, NULL_TREE);
  fndecl = build_decl (FUNCTION_DECL, get_identifier (name), fntype);

  DECL_EXTERNAL (fndecl) = 0;
  if (! inner_module)
    TREE_PUBLIC (fndecl) = 1;

  TREE_STATIC (fndecl)   = 1;
  DECL_RESULT (fndecl)   = build_decl (RESULT_DECL, NULL_TREE, integer_type_node);
  DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;

  rest_of_decl_compilation (fndecl, NULL, 1, 0);

  /* Announce we are compiling this function.  */
  announce_function (fndecl);

  /* Set up to compile the function and enter it.  */
  current_function_decl = fndecl;
  DECL_INITIAL (fndecl) = error_mark_node;

  pushlevel (0);
  make_decl_rtl (fndecl, NULL);

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

  init_function_start (fndecl, input_filename, line);
  expand_function_start (fndecl, 0);
  expand_start_bindings (0);
  return fndecl;
}

void
gccgm2_BuildEnd (fndecl)
     tree fndecl;
{
  /* Now get back out of the function and compile it.  */
  /* pop the block level */
  tree block = poplevel(1, 0, 1);
  expand_end_bindings (block, 0, 1);
  expand_function_end (input_filename, lineno, 0);
  rest_of_compilation (fndecl);
  current_function_decl = 0;
}

void
gccgm2_BuildCallInnerInit (fndecl)
     tree fndecl;
{
  expand_expr_stmt (build_function_call (fndecl, NULL_TREE));
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


/*
 *  DebugTree - display the tree, t.
 */

void
gccgm2_DebugTree (t)
     tree t;
{
  debug_tree(t);
}

void
gccgm2_DebugTreeChain (t)
     tree t;
{
  for (; t; t = TREE_CHAIN (t))
    debug_tree(t);
}

/*
 * Similar to build_int_2() but allows you to specify the type of the
 * integer constant that you are creating.
 */

tree
build_int_2_type(low, hi, type)
int low, hi;
tree type;
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
gccgm2_BuildCap (t)
     tree t;
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
    less_than = build_binary_op( LT_EXPR, t,
				 build_int_2_type( 'a', 0,
						   char_type_node), 0);
    greater_than = build_binary_op( GT_EXPR, t,
				    build_int_2_type( 'z', 0,
						      char_type_node), 0);
    out_of_range = build_binary_op( TRUTH_ORIF_EXPR,
				    less_than, greater_than, 0);
    
    translated = fold( build( MINUS_EXPR, char_type_node, t,
			      build_int_2_type( 'a'-'A', 0,
						char_type_node)));
    
    return fold( build_conditional_expr (out_of_range, t, translated));
  }

  error ("argument to CAP is not a constant or variable of type CHAR");
  return error_mark_node;
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
expand_tree_builtin (function, params, coerced_params)
     tree function, params ATTRIBUTE_UNUSED, coerced_params;
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
default_function_array_conversion (exp)
     tree exp;
{
  /* not needed in Modula-2 */
  return exp;
}

/* Convert the argument expressions in the list VALUES
   to the types in the list TYPELIST.  The result is a list of converted
   argument expressions.

   If TYPELIST is exhausted, or when an element has NULL as its type,
   perform the default conversions.

   PARMLIST is the chain of parm decls for the function being called.
   It may be 0, if that info is not available.
   It is used only for generating error messages.

   NAME is an IDENTIFIER_NODE or 0.  It is used only for error messages.

   This is also where warnings about wrong number of args are generated.

   Both VALUES and the returned value are chains of TREE_LIST nodes
   with the elements of the list in the TREE_VALUE slots of those nodes.  */

static tree
convert_arguments (typelist, values, name, fundecl)
     tree typelist, values, name, fundecl;
{
  tree typetail, valtail;
  tree result = NULL;
  int parmnum;

  /* Scan the given expressions and types, producing individual
     converted arguments and pushing them on RESULT in reverse order.  */

  for (valtail = values, typetail = typelist, parmnum = 0;
       valtail;
       valtail = TREE_CHAIN (valtail), parmnum++)
    {
      tree type = typetail ? TREE_VALUE (typetail) : 0;
      tree val = TREE_VALUE (valtail);

      if (type == void_type_node)
	{
	  if (name)
	    error ("too many arguments to function `%s'",
		   IDENTIFIER_POINTER (name));
	  else
	    error ("too many arguments to function");
	  break;
	}

      /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
      /* Do not use STRIP_NOPS here!  We do not want an enumerator with value 0
	 to convert automatically to a pointer.  */
      if (TREE_CODE (val) == NON_LVALUE_EXPR)
	val = TREE_OPERAND (val, 0);

      val = default_function_array_conversion (val);

      val = require_complete_type (val);

      if (type != 0)
	{
	  /* Formal parm type is specified by a function prototype.  */
	  tree parmval;

	  if (!COMPLETE_TYPE_P (type))
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
		  int formal_prec = TYPE_PRECISION (type);

		  if (INTEGRAL_TYPE_P (type)
		      && TREE_CODE (TREE_TYPE (val)) == REAL_TYPE)
		    warn_for_assignment ("%s as integer rather than floating due to prototype", (char *) 0, name, parmnum + 1);
		  if (INTEGRAL_TYPE_P (type)
		      && TREE_CODE (TREE_TYPE (val)) == COMPLEX_TYPE)
		    warn_for_assignment ("%s as integer rather than complex due to prototype", (char *) 0, name, parmnum + 1);
		  else if (TREE_CODE (type) == COMPLEX_TYPE
			   && TREE_CODE (TREE_TYPE (val)) == REAL_TYPE)
		    warn_for_assignment ("%s as complex rather than floating due to prototype", (char *) 0, name, parmnum + 1);
		  else if (TREE_CODE (type) == REAL_TYPE
			   && INTEGRAL_TYPE_P (TREE_TYPE (val)))
		    warn_for_assignment ("%s as floating rather than integer due to prototype", (char *) 0, name, parmnum + 1);
		  else if (TREE_CODE (type) == COMPLEX_TYPE
			   && INTEGRAL_TYPE_P (TREE_TYPE (val)))
		    warn_for_assignment ("%s as complex rather than integer due to prototype", (char *) 0, name, parmnum + 1);
		  else if (TREE_CODE (type) == REAL_TYPE
			   && TREE_CODE (TREE_TYPE (val)) == COMPLEX_TYPE)
		    warn_for_assignment ("%s as floating rather than complex due to prototype", (char *) 0, name, parmnum + 1);
		  /* ??? At some point, messages should be written about
		     conversions between complex types, but that's too messy
		     to do now.  */
		  else if (TREE_CODE (type) == REAL_TYPE
			   && TREE_CODE (TREE_TYPE (val)) == REAL_TYPE)
		    {
		      /* Warn if any argument is passed as `float',
			 since without a prototype it would be `double'.  */
		      if (formal_prec == TYPE_PRECISION (float_type_node))
			warn_for_assignment ("%s as `float' rather than `double' due to prototype", (char *) 0, name, parmnum + 1);
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
			warn_for_assignment ("%s with different width due to prototype", (char *) 0, name, parmnum + 1);
		      else if (TREE_UNSIGNED (type) == TREE_UNSIGNED (type1))
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
		      /* Likewise for a constant in a NOP_EXPR.  */
		      else if (TREE_CODE (val) == NOP_EXPR
			       && TREE_CODE (TREE_OPERAND (val, 0)) == INTEGER_CST
			       && int_fits_type_p (TREE_OPERAND (val, 0), type))
			;
#if 0 /* We never get such tree structure here.  */
		      else if (TREE_CODE (TREE_TYPE (val)) == ENUMERAL_TYPE
			       && int_fits_type_p (TYPE_MIN_VALUE (TREE_TYPE (val)), type)
			       && int_fits_type_p (TYPE_MAX_VALUE (TREE_TYPE (val)), type))
			/* Change in signedness doesn't matter
			   if an enum value is unaffected.  */
			;
#endif
		      /* If the value is extended from a narrower
			 unsigned type, it doesn't matter whether we
			 pass it as signed or unsigned; the value
			 certainly is the same either way.  */
		      else if (TYPE_PRECISION (TREE_TYPE (val)) < TYPE_PRECISION (type)
			       && TREE_UNSIGNED (TREE_TYPE (val)))
			;
		      else if (TREE_UNSIGNED (type))
			warn_for_assignment ("%s as unsigned due to prototype", (char *) 0, name, parmnum + 1);
		      else
			warn_for_assignment ("%s as signed due to prototype", (char *) 0, name, parmnum + 1);
		    }
		}
#endif

	      parmval = convert_for_assignment (type, val, 
					        (char *) 0, /* arg passing  */
						fundecl, name, parmnum + 1);
	      
	      if (PROMOTE_PROTOTYPES
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
      else
	/* Convert `short' and `char' to full-size `int'.  */
	result = tree_cons (NULL_TREE, default_conversion (val), result);

      if (typetail)
	typetail = TREE_CHAIN (typetail);
    }

  if (typetail != 0 && TREE_VALUE (typetail) != void_type_node)
    {
      if (name)
	error ("too few arguments to function `%s'",
	       IDENTIFIER_POINTER (name));
      else
	error ("too few arguments to function");
    }

  return nreverse (result);
}

/* Build a function call to function FUNCTION with parameters PARAMS.
   PARAMS is a list--a chain of TREE_LIST nodes--in which the
   TREE_VALUE of each node is a parameter-expression.
   FUNCTION's data type may be a function type or a pointer-to-function.  */

tree
build_function_call (function, params)
     tree function, params;
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

  b->tags = tree_cons (name, type, b->tags);

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
   CODE says which kind of tag NAME ought to be.  */

tree
start_struct (code, name)
     enum tree_code code;
     tree name;
{
  /* If there is already a tag defined at this binding level
     (as a forward reference), just return it.  */

  register tree ref = 0;

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

  if ((name == NULL) || (strcmp(name, "") == 0))
    id = NULL_TREE;
  else
    id = get_identifier (name);

  return start_struct (RECORD_TYPE, id);
}


tree
gccgm2_BuildStartVarientRecord (name)
     char *name;
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
layout_array_type (t)
     tree t;
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
gccgm2_BuildFieldRecord (name, type)
     char *name;
     tree type;
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
gccgm2_ChainOn (t1, t2)
     tree t1, t2;
{
  return chainon (t1, t2);
}

/*
 *  ChainOnParamValue - adds a list node {parm, value} into the tree list.
 */

tree
gccgm2_ChainOnParamValue (list, parm, value)
     tree list, parm, value;
{
  return chainon (list, build_tree_list(parm, value));
}

/*
 *  AddStringToTreeList - adds, string, to list.
 */

tree
gccgm2_AddStringToTreeList (list, string)
     tree list, string;
{
  return tree_cons (NULL_TREE, string, list);
}

/* Determine whether TYPE is a structure with a flexible array member,
   or a union containing such a structure (possibly recursively).  */

static bool
flexible_array_type_p (type)
     tree type;
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

/* Fill in the fields of a RECORD_TYPE or UNION_TYPE node, T.
   FIELDLIST is a chain of FIELD_DECL nodes for the fields.
   ATTRIBUTES are attributes to be applied to the structure.  */

tree
gccgm2_BuildEndRecord (t, fieldlist /* , attributes */ )
     tree t;
     tree fieldlist;
{
  tree x;
  int toplevel = global_binding_level == current_binding_level;
  int saw_named_field;

  /* If this type was previously laid out as a forward reference,
     make sure we lay it out again.  */

  TYPE_SIZE (t) = 0;

#if !defined(GM2)
  decl_attributes (&t, attributes, (int) ATTR_FLAG_TYPE_IN_PLACE);

  /* Nameless union parm types are useful as GCC extension.  */
  if (! (TREE_CODE (t) == UNION_TYPE && TYPE_NAME (t) == 0) && !pedantic)
    /* Otherwise, warn about any struct or union def. in parmlist.  */
    if (in_parm_level_p ())
      {
	if (pedantic)
	  pedwarn ("%s defined inside parms",
		   TREE_CODE (t) == UNION_TYPE ? _("union") : _("structure"));
	else
	  warning ("%s defined inside parms",
		   TREE_CODE (t) == UNION_TYPE ? _("union") : _("structure"));
      }
#endif

  if (pedantic)
    {
      for (x = fieldlist; x; x = TREE_CHAIN (x))
	if (DECL_NAME (x) != 0)
	  break;

      if (x == 0)
	pedwarn ("%s has no %s",
		 TREE_CODE (t) == UNION_TYPE ? _("union") : _("struct"),
		 fieldlist ? _("named members") : _("members"));
    }

  /* Install struct as DECL_CONTEXT of each field decl.
     Also process specified field sizes,m which is found in the DECL_INITIAL.
     Store 0 there, except for ": 0" fields (so we can find them
     and delete them, below).  */

  saw_named_field = 0;
  for (x = fieldlist; x; x = TREE_CHAIN (x))
    {
      DECL_CONTEXT (x) = t;
      DECL_PACKED (x) |= TYPE_PACKED (t);

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
	      error_with_decl (x,
			       "bit-field `%s' width not an integer constant");
	      DECL_INITIAL (x) = NULL;
	    }
	}

      /* Detect invalid bit-field type.  */
      if (DECL_INITIAL (x)
	  && TREE_CODE (TREE_TYPE (x)) != INTEGER_TYPE
	  && TREE_CODE (TREE_TYPE (x)) != BOOLEAN_TYPE
	  && TREE_CODE (TREE_TYPE (x)) != ENUMERAL_TYPE)
	{
	  error_with_decl (x, "bit-field `%s' has invalid type");
	  DECL_INITIAL (x) = NULL;
	}

      if (DECL_INITIAL (x) && pedantic
	  && TYPE_MAIN_VARIANT (TREE_TYPE (x)) != integer_type_node
	  && TYPE_MAIN_VARIANT (TREE_TYPE (x)) != unsigned_type_node
	  && TYPE_MAIN_VARIANT (TREE_TYPE (x)) != c_bool_type_node
	  /* Accept an enum that's equivalent to int or unsigned int.  */
	  && !(TREE_CODE (TREE_TYPE (x)) == ENUMERAL_TYPE
	       && (TYPE_PRECISION (TREE_TYPE (x))
		   == TYPE_PRECISION (integer_type_node))))
	pedwarn_with_decl (x, "bit-field `%s' type invalid in ISO C");

      /* Detect and ignore out of range field width and process valid
	 field widths.  */
      if (DECL_INITIAL (x))
	{
	  int max_width
	    = (TYPE_MAIN_VARIANT (TREE_TYPE (x)) == c_bool_type_node
	       ? CHAR_TYPE_SIZE : TYPE_PRECISION (TREE_TYPE (x)));

	  if (tree_int_cst_sgn (DECL_INITIAL (x)) < 0)
	    error_with_decl (x, "negative width in bit-field `%s'");
	  else if (0 < compare_tree_int (DECL_INITIAL (x), max_width))
	    pedwarn_with_decl (x, "width of `%s' exceeds its type");
	  else if (integer_zerop (DECL_INITIAL (x)) && DECL_NAME (x) != 0)
	    error_with_decl (x, "zero width for bit-field `%s'");
	  else
	    {
	      /* The test above has assured us that TREE_INT_CST_HIGH is 0.  */
	      unsigned HOST_WIDE_INT width
		= tree_low_cst (DECL_INITIAL (x), 1);

	      if (TREE_CODE (TREE_TYPE (x)) == ENUMERAL_TYPE
		  && (width < min_precision (TYPE_MIN_VALUE (TREE_TYPE (x)),
					     TREE_UNSIGNED (TREE_TYPE (x)))
		      || (width
			  < min_precision (TYPE_MAX_VALUE (TREE_TYPE (x)),
					   TREE_UNSIGNED (TREE_TYPE (x))))))
		warning_with_decl (x,
				   "`%s' is narrower than values of its type");

	      DECL_SIZE (x) = bitsize_int (width);
	      DECL_BIT_FIELD (x) = 1;
	      SET_DECL_C_BIT_FIELD (x);

	      if (width == 0
		  && ! (* targetm.ms_bitfield_layout_p) (t))
		{
		  /* field size 0 => force desired amount of alignment.  */
#ifdef EMPTY_FIELD_BOUNDARY
		  DECL_ALIGN (x) = MAX (DECL_ALIGN (x), EMPTY_FIELD_BOUNDARY);
#endif
#ifdef PCC_BITFIELD_TYPE_MATTERS
		  if (PCC_BITFIELD_TYPE_MATTERS)
		    {
		      DECL_ALIGN (x) = MAX (DECL_ALIGN (x),
					    TYPE_ALIGN (TREE_TYPE (x)));
		      DECL_USER_ALIGN (x) |= TYPE_USER_ALIGN (TREE_TYPE (x));
		    }
#endif
		}
	    }
	}

      DECL_INITIAL (x) = 0;

      /* Detect flexible array member in an invalid context.  */
      if (TREE_CODE (TREE_TYPE (x)) == ARRAY_TYPE
	  && TYPE_SIZE (TREE_TYPE (x)) == NULL_TREE
	  && TYPE_DOMAIN (TREE_TYPE (x)) != NULL_TREE
	  && TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (x))) == NULL_TREE)
	{
	  if (TREE_CODE (t) == UNION_TYPE)
	    error_with_decl (x, "flexible array member in union");
	  else if (TREE_CHAIN (x) != NULL_TREE)
	    error_with_decl (x, "flexible array member not at end of struct");
	  else if (! saw_named_field)
	    error_with_decl (x, "flexible array member in otherwise empty struct");
	}

      if (pedantic && TREE_CODE (t) == RECORD_TYPE
	  && flexible_array_type_p (TREE_TYPE (x)))
	pedwarn_with_decl (x, "invalid use of structure with flexible array member");

      if (DECL_NAME (x))
	saw_named_field = 1;
    }

  /* Delete all duplicate fields from the fieldlist */
  for (x = fieldlist; x && TREE_CHAIN (x);)
    /* Anonymous fields aren't duplicates.  */
    if (DECL_NAME (TREE_CHAIN (x)) == 0)
      x = TREE_CHAIN (x);
    else
      {
	tree y = fieldlist;

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
	else
	  x = TREE_CHAIN (x);
      }

  /* Now we have the nearly final fieldlist.  Record it,
     then lay out the structure or union (including the fields).  */

  TYPE_FIELDS (t) = fieldlist;

  layout_type (t);

  /* Delete all zero-width bit-fields from the fieldlist */
  {
    tree *fieldlistp = &fieldlist;
    while (*fieldlistp)
      if (TREE_CODE (*fieldlistp) == FIELD_DECL && DECL_INITIAL (*fieldlistp))
	*fieldlistp = TREE_CHAIN (*fieldlistp);
      else
	fieldlistp = &TREE_CHAIN (*fieldlistp);
  }

  /* Now we have the truly final field list.
     Store it in this type and in the variants.  */

  TYPE_FIELDS (t) = fieldlist;

  for (x = TYPE_MAIN_VARIANT (t); x; x = TYPE_NEXT_VARIANT (x))
    {
      TYPE_FIELDS (x) = TYPE_FIELDS (t);
      TYPE_LANG_SPECIFIC (x) = TYPE_LANG_SPECIFIC (t);
      TYPE_ALIGN (x) = TYPE_ALIGN (t);
      TYPE_USER_ALIGN (x) = TYPE_USER_ALIGN (t);
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

  if (current_binding_level->incomplete_list != NULL_TREE)
    {
      tree prev = NULL_TREE;

      for (x = current_binding_level->incomplete_list; x; x = TREE_CHAIN (x))
        {
	  tree decl = TREE_VALUE (x);

	  if (TYPE_MAIN_VARIANT (TREE_TYPE (decl)) == TYPE_MAIN_VARIANT (t)
	      && TREE_CODE (decl) != TYPE_DECL)
	    {
	      layout_decl (decl, 0);
	      /* This is a no-op in c-lang.c or something real in objc-act.c.  */
	      if (flag_objc)
		objc_check_decl (decl);
	      rest_of_decl_compilation (decl, NULL, toplevel, 0);
	      if (! toplevel)
		expand_decl (decl);
	      /* Unlink X from the incomplete list.  */
	      if (prev)
		TREE_CHAIN (prev) = TREE_CHAIN (x);
	      else
	        current_binding_level->incomplete_list = TREE_CHAIN (x);
	    }
	  else if (!COMPLETE_TYPE_P (TREE_TYPE (decl))
		   && TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
	    {
	      tree element = TREE_TYPE (decl);
	      while (TREE_CODE (element) == ARRAY_TYPE)
		element = TREE_TYPE (element);
	      if (element == t)
		{
		  layout_array_type (TREE_TYPE (decl));
		  if (TREE_CODE (decl) != TYPE_DECL)
		    {
		      layout_decl (decl, 0);
		      if (flag_objc)
			objc_check_decl (decl);
		      rest_of_decl_compilation (decl, NULL, toplevel, 0);
		      if (! toplevel)
			expand_decl (decl);
		    }
		  /* Unlink X from the incomplete list.  */
		  if (prev)
		    TREE_CHAIN (prev) = TREE_CHAIN (x);
		  else
		    current_binding_level->incomplete_list = TREE_CHAIN (x);
		}
	    }
	}
    }

  /* Finish debugging output for this type.  */
  rest_of_type_compilation (t, toplevel);

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
  register tree enumtype = 0;

  /* If this is the real definition for a previous forward reference,
     fill in the contents in the same object that used to be the
     forward reference.  */

#if !defined(GM2)
  if (name != 0)
    enumtype = lookup_tag (ENUMERAL_TYPE, name, current_binding_level, 1);
#endif

  if (enumtype == 0 || TREE_CODE (enumtype) != ENUMERAL_TYPE)
    {
      enumtype = make_node (ENUMERAL_TYPE);
      pushtag (name, enumtype);
    }
#if !defined(GM2)
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
  register tree pair, tem;
  tree minnode = 0, maxnode = 0, enum_value_type;
  int precision, unsign;
  int toplevel = (global_binding_level == current_binding_level);

#if !defined(GM2)
  if (in_parm_level_p ())
    warning ("enum defined inside parms");

  decl_attributes (enumtype, attributes, NULL_TREE);
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
  if (!TYPE_PACKED (enumtype))
    precision = MAX (precision, TYPE_PRECISION (integer_type_node));
  if (gm2_type_for_size (precision, unsign) == 0)
    {
      warning ("enumeration values exceed range of largest integer");
      precision = TYPE_PRECISION (long_long_integer_type_node);
    }

  if (precision == TYPE_PRECISION (integer_type_node))
    enum_value_type = gm2_type_for_size (precision, 0);
  else
    enum_value_type = enumtype;

  TYPE_MIN_VALUE (enumtype) = minnode;
  TYPE_MAX_VALUE (enumtype) = maxnode;
  TYPE_PRECISION (enumtype) = precision;
  TREE_UNSIGNED (enumtype) = unsign;
  TYPE_SIZE (enumtype) = 0;
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

	  TREE_TYPE (enu) = enumtype;
	  DECL_SIZE (enu) = TYPE_SIZE (enumtype);
	  DECL_SIZE_UNIT (enu) = TYPE_SIZE_UNIT (enumtype);
	  DECL_ALIGN (enu) = TYPE_ALIGN (enumtype);
	  DECL_USER_ALIGN (enu) = TYPE_USER_ALIGN (enumtype);
	  DECL_MODE (enu) = TYPE_MODE (enumtype);

	  /* The ISO C Standard mandates enumerators to have type int,
	     even though the underlying type of an enum type is
	     unspecified.  Here we convert any enumerators that fit in
	     an int to type int, to avoid promotions to unsigned types
	     when comparing integers with enumerators that fit in the
	     int range.  When -pedantic is given, build_enumerator()
	     would have already taken care of those that don't fit.  */
	  if (int_fits_type_p (DECL_INITIAL (enu), enum_value_type))
	    DECL_INITIAL (enu) = convert (enum_value_type, DECL_INITIAL (enu));
	  else
	    DECL_INITIAL (enu) = convert (enumtype, DECL_INITIAL (enu));

	  TREE_PURPOSE (pair) = DECL_NAME (enu);
	  TREE_VALUE (pair) = DECL_INITIAL (enu);
	}

      TYPE_VALUES (enumtype) = values;
    }

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
      TREE_UNSIGNED (tem) = TREE_UNSIGNED (enumtype);
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
			     && TREE_UNSIGNED (type)));

  decl = build_decl (CONST_DECL, name, type);
  DECL_INITIAL (decl) = convert (type, value);
  pushdecl (decl);

  return tree_cons (decl, value, NULL_TREE);
}

/*
 *  ExpandExpressionStatement - maps onto expand_expr_stmt_value in stmt.c
 */

void
gccgm2_ExpandExpressionStatement (t)
     tree t;
{
  expand_expr_stmt_value (t, 1, 1);
}

/*
 *  InitGlobalContext - initializes a dummy function for the global scope.
 */

void gccgm2_InitGlobalContext (void)
{
  if (cfun == 0)
    init_dummy_function_start ();
}

/*
 *  GarbageCollect - force gcc to garbage collect.
 */

void
gccgm2_GarbageCollect (void)
{
  ggc_collect();
}

#include "gt-gm2-gm2-lang.h"
#include "gtype-gm2.h"
#include "gt-gm2-gccgm2.h"

/*
 * Local variables:
 *  compile-command: "gcc -c  -DIN_GCC    -g -W -Wall -Wwrite-strings -Wtraditional -Wstrict-prototypes -Wmissing-prototypes -pedantic -Wno-long-long  -W -Wall -DGM2 -I. -I.. -I. -I./.. -I./../config -I./../../include gccgm2.c"
 * End:
 */
