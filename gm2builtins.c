/* Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
 * Free Software Foundation, Inc.  */
/* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. */

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
#include "real.h"
#include "float.h"

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

extern tree builtin_function (const char *, tree, int, enum built_in_class,
			      const char *, tree);
extern tree pushdecl (tree);


#define GM2
#define GM2_BUG_REPORT "Please report this crash to the GNU Modula-2 mailing list <gm2@glam.ac.uk>\n"

#define ASSERT(X,Y)   { if (!(X)) { debug_tree(Y); internal_error("[%s:%d]:condition `%s' failed", \
                                                                   __FILE__, __LINE__, #X); } }
#define ERROR(X)      { internal_error("[%s:%d]:%s",               __FILE__, __LINE__, X); }


typedef enum {
  BT_FN_PTR_SIZE,
  BT_FN_TRAD_PTR_PTR_CONST_PTR_SIZE,
  BT_FN_FLOAT,
  BT_FN_DOUBLE,
  BT_FN_LONG_DOUBLE,
  BT_FN_FLOAT_FLOAT,
  BT_FN_DOUBLE_DOUBLE,
  BT_FN_LONG_DOUBLE_LONG_DOUBLE,
  BT_FN_STRING_CONST_STRING_INT,
  BT_FN_INT_CONST_PTR_CONST_PTR_SIZE,
  BT_FN_TRAD_PTR_PTR_INT_SIZE,
  BT_FN_STRING_STRING_CONST_STRING,
  BT_FN_STRING_STRING_CONST_STRING_SIZE,
  BT_FN_INT_CONST_STRING_CONST_STRING,
  BT_FN_INT_CONST_STRING_CONST_STRING_SIZE,
  BT_FN_INT_CONST_STRING,
  BT_FN_STRING_CONST_STRING_CONST_STRING,
  BT_FN_SIZE_CONST_STRING_CONST_STRING,
  BT_FN_PTR_UNSIGNED,
  BT_FN_VOID_PTR_INT,
  BT_FN_INT_PTR,
  BT_FN_INT_FLOAT,
  BT_FN_INT_DOUBLE,
  BT_FN_INT_LONG_DOUBLE,
  BT_FN_FLOAT_FCOMPLEX,
  BT_FN_DOUBLE_DCOMPLEX,
  BT_FN_LONG_DOUBLE_LDCOMPLEX,

  BT_FN_FCOMPLEX_FCOMPLEX,
  BT_FN_DCOMPLEX_DCOMPLEX,
  BT_FN_LDCOMPLEX_LDCOMPLEX,

  BT_FN_DCOMPLEX_DOUBLE_DCOMPLEX,
  BT_FN_FCOMPLEX_FLOAT_FCOMPLEX,
  BT_FN_LDCOMPLEX_LONG_DOUBLE_LDCOMPLEX,

  BT_FN_FLOAT_FLOAT_FLOATPTR,
  BT_FN_DOUBLE_DOUBLE_DOUBLEPTR,
  BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG_DOUBLEPTR,

  BT_FN_FLOAT_FLOAT_LONG_DOUBLE,
  BT_FN_DOUBLE_DOUBLE_LONG_DOUBLE,
  BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG_DOUBLE,

  BT_FN_FLOAT_FLOAT_LONG,
  BT_FN_DOUBLE_DOUBLE_LONG,
  BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG,

  BT_FN_FLOAT_FLOAT_INT,
  BT_FN_DOUBLE_DOUBLE_INT,
  BT_FN_LONG_DOUBLE_LONG_DOUBLE_INT,

  BT_FN_FLOAT_FLOAT_FLOAT,
  BT_FN_DOUBLE_DOUBLE_DOUBLE,
} builtin_prototype;

struct builtin_function_entry {
  const char *name;
  builtin_prototype defn;
  int function_code;
  int class;
  const char *library_name;
  tree function_node;
  tree return_node;
};

/*
 *  entries are added by examining gcc/builtins.def and copying those
 *  functions which can be applied to Modula-2
 */

static struct builtin_function_entry list_of_builtins[] = {
{ "__builtin_alloca",  BT_FN_PTR_SIZE, BUILT_IN_ALLOCA, BUILT_IN_NORMAL, "alloca", NULL, NULL},
{ "__builtin_memcpy",  BT_FN_TRAD_PTR_PTR_CONST_PTR_SIZE, BUILT_IN_MEMCPY, BUILT_IN_NORMAL, "memcpy", NULL, NULL},
{ "__builtin_sinf",    BT_FN_FLOAT_FLOAT, BUILT_IN_SINF, BUILT_IN_NORMAL, "sinf", NULL, NULL},
{ "__builtin_sin",     BT_FN_DOUBLE_DOUBLE, BUILT_IN_SIN, BUILT_IN_NORMAL, "sin", NULL, NULL},
{ "__builtin_sinl",    BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_SINL, BUILT_IN_NORMAL, "sinl", NULL, NULL},
{ "__builtin_cosf",    BT_FN_FLOAT_FLOAT, BUILT_IN_SINF, BUILT_IN_NORMAL, "cosf", NULL, NULL},
{ "__builtin_cos",     BT_FN_DOUBLE_DOUBLE, BUILT_IN_COS, BUILT_IN_NORMAL, "cos", NULL, NULL},
{ "__builtin_cosl",    BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_COSL, BUILT_IN_NORMAL, "cosl", NULL, NULL},
{ "__builtin_sqrtf",   BT_FN_FLOAT_FLOAT, BUILT_IN_SQRTF, BUILT_IN_NORMAL, "sqrtf", NULL, NULL},
{ "__builtin_sqrt",    BT_FN_DOUBLE_DOUBLE, BUILT_IN_SQRT, BUILT_IN_NORMAL, "sqrt", NULL, NULL},
{ "__builtin_sqrtl",   BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_SQRTL, BUILT_IN_NORMAL, "sqrtl", NULL, NULL},
{ "__builtin_fabsf",   BT_FN_FLOAT_FLOAT, BUILT_IN_FABSF, BUILT_IN_NORMAL, "fabsf", NULL, NULL},
{ "__builtin_fabs",    BT_FN_DOUBLE_DOUBLE, BUILT_IN_FABS, BUILT_IN_NORMAL, "fabs", NULL, NULL},
{ "__builtin_fabsl",   BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_FABSL, BUILT_IN_NORMAL, "fabsl", NULL, NULL},
{ "__builtin_logf",    BT_FN_FLOAT_FLOAT, BUILT_IN_LOGF, BUILT_IN_NORMAL, "logf", NULL, NULL},
{ "__builtin_log",     BT_FN_DOUBLE_DOUBLE, BUILT_IN_LOG, BUILT_IN_NORMAL, "log", NULL, NULL},
{ "__builtin_logl",    BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_LOGL, BUILT_IN_NORMAL, "logl", NULL, NULL},
{ "__builtin_expf",    BT_FN_FLOAT_FLOAT, BUILT_IN_EXPF, BUILT_IN_NORMAL, "expf", NULL, NULL},
{ "__builtin_exp",     BT_FN_DOUBLE_DOUBLE, BUILT_IN_EXP, BUILT_IN_NORMAL, "exp", NULL, NULL},
{ "__builtin_expl",    BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_EXPL, BUILT_IN_NORMAL, "expl", NULL, NULL},
{ "__builtin_log10f",  BT_FN_FLOAT_FLOAT, BUILT_IN_LOG10F, BUILT_IN_NORMAL, "log10f", NULL, NULL},
{ "__builtin_log10",   BT_FN_DOUBLE_DOUBLE, BUILT_IN_LOG10, BUILT_IN_NORMAL, "log10", NULL, NULL},
{ "__builtin_log10l",  BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_LOG10L, BUILT_IN_NORMAL, "log10l", NULL, NULL},
{ "__builtin_ilogbf",  BT_FN_INT_FLOAT, BUILT_IN_ILOGBF, BUILT_IN_NORMAL, "ilogbf", NULL, NULL},
{ "__builtin_ilogb",   BT_FN_INT_DOUBLE, BUILT_IN_ILOGB, BUILT_IN_NORMAL, "ilogb", NULL, NULL},
{ "__builtin_ilogbl",  BT_FN_INT_LONG_DOUBLE, BUILT_IN_ILOGBL, BUILT_IN_NORMAL, "ilogbl", NULL, NULL},

{ "__builtin_atan2f",  BT_FN_FLOAT_FLOAT_FLOAT, BUILT_IN_ATAN2F, BUILT_IN_NORMAL, "atan2f", NULL, NULL},
{ "__builtin_atan2",   BT_FN_DOUBLE_DOUBLE_DOUBLE, BUILT_IN_ATAN2, BUILT_IN_NORMAL, "atan2", NULL, NULL},
{ "__builtin_atan2l",  BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_ATAN2L, BUILT_IN_NORMAL, "atan2l", NULL, NULL},

{ "__builtin_signbit", BT_FN_INT_DOUBLE, BUILT_IN_SIGNBIT, BUILT_IN_NORMAL, "signbit", NULL, NULL},
{ "__builtin_signbitf", BT_FN_INT_FLOAT, BUILT_IN_SIGNBITF, BUILT_IN_NORMAL, "signbitf", NULL, NULL},
{ "__builtin_signbitl", BT_FN_INT_LONG_DOUBLE, BUILT_IN_SIGNBITL, BUILT_IN_NORMAL, "signbitl", NULL, NULL},
{ "__builtin_significand", BT_FN_DOUBLE_DOUBLE, BUILT_IN_SIGNIFICAND, BUILT_IN_NORMAL, "significand", NULL, NULL},
{ "__builtin_significandf", BT_FN_FLOAT_FLOAT, BUILT_IN_SIGNIFICANDF, BUILT_IN_NORMAL, "significandf", NULL, NULL},
{ "__builtin_significandl", BT_FN_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_SIGNIFICANDL, BUILT_IN_NORMAL, "significandl", NULL, NULL},
{ "__builtin_modf", BT_FN_DOUBLE_DOUBLE_DOUBLEPTR, BUILT_IN_MODF, BUILT_IN_NORMAL, "modf", NULL, NULL},
{ "__builtin_modff", BT_FN_FLOAT_FLOAT_FLOATPTR, BUILT_IN_MODFF, BUILT_IN_NORMAL, "modff", NULL, NULL},
{ "__builtin_modfl", BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG_DOUBLEPTR, BUILT_IN_MODFL, BUILT_IN_NORMAL, "modfl", NULL, NULL},
{ "__builtin_nextafter", BT_FN_DOUBLE_DOUBLE_DOUBLE, BUILT_IN_NEXTAFTER, BUILT_IN_NORMAL, "nextafter", NULL, NULL},
{ "__builtin_nextafterf", BT_FN_FLOAT_FLOAT_FLOAT, BUILT_IN_NEXTAFTERF, BUILT_IN_NORMAL, "nextafterf", NULL, NULL},
{ "__builtin_nextafterl", BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_NEXTAFTERL, BUILT_IN_NORMAL, "nextafterl", NULL, NULL},
{ "__builtin_nexttoward", BT_FN_DOUBLE_DOUBLE_LONG_DOUBLE, BUILT_IN_NEXTTOWARD, BUILT_IN_NORMAL, "nexttoward", NULL, NULL},
{ "__builtin_nexttowardf", BT_FN_FLOAT_FLOAT_LONG_DOUBLE, BUILT_IN_NEXTTOWARDF, BUILT_IN_NORMAL, "nexttowardf", NULL, NULL},
{ "__builtin_nexttowardl", BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_NEXTTOWARDL, BUILT_IN_NORMAL, "nexttowardl", NULL, NULL},
{ "__builtin_scalb", BT_FN_DOUBLE_DOUBLE_DOUBLE, BUILT_IN_SCALB, BUILT_IN_NORMAL, "scalb", NULL, NULL},
{ "__builtin_scalbf", BT_FN_FLOAT_FLOAT_FLOAT, BUILT_IN_SCALBF, BUILT_IN_NORMAL, "scalbf", NULL, NULL},
{ "__builtin_scalbl", BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG_DOUBLE, BUILT_IN_SCALBL, BUILT_IN_NORMAL, "scalbl", NULL, NULL},
{ "__builtin_scalbln", BT_FN_DOUBLE_DOUBLE_LONG, BUILT_IN_SCALBLN, BUILT_IN_NORMAL, "scalbln", NULL, NULL},
{ "__builtin_scalblnf", BT_FN_FLOAT_FLOAT_LONG, BUILT_IN_SCALBLNF, BUILT_IN_NORMAL, "scalblnf", NULL, NULL},
{ "__builtin_scalblnl", BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG, BUILT_IN_SCALBLNL, BUILT_IN_NORMAL, "scalblnl", NULL, NULL},
{ "__builtin_scalbn", BT_FN_DOUBLE_DOUBLE_INT, BUILT_IN_SCALBN, BUILT_IN_NORMAL, "scalbln", NULL, NULL},
{ "__builtin_scalbnf", BT_FN_FLOAT_FLOAT_INT, BUILT_IN_SCALBNF, BUILT_IN_NORMAL, "scalblnf", NULL, NULL},
{ "__builtin_scalbnl", BT_FN_LONG_DOUBLE_LONG_DOUBLE_INT, BUILT_IN_SCALBNL, BUILT_IN_NORMAL, "scalblnl", NULL, NULL},

/* complex intrinsic functions */
{ "__builtin_cabs", BT_FN_DOUBLE_DCOMPLEX, BUILT_IN_CABS, BUILT_IN_NORMAL, "cabs", NULL, NULL},
{ "__builtin_cabsf", BT_FN_FLOAT_FCOMPLEX, BUILT_IN_CABSF, BUILT_IN_NORMAL, "cabsf", NULL, NULL},
{ "__builtin_cabsl", BT_FN_LONG_DOUBLE_LDCOMPLEX, BUILT_IN_CABSL, BUILT_IN_NORMAL, "cabsl", NULL, NULL},

{ "__builtin_carg", BT_FN_DOUBLE_DCOMPLEX, BUILT_IN_CABS, BUILT_IN_NORMAL, "carg", NULL, NULL},
{ "__builtin_cargf", BT_FN_FLOAT_FCOMPLEX, BUILT_IN_CABSF, BUILT_IN_NORMAL, "cargf", NULL, NULL},
{ "__builtin_cargl", BT_FN_LONG_DOUBLE_LDCOMPLEX, BUILT_IN_CABSL, BUILT_IN_NORMAL, "cargl", NULL, NULL},

{ "__builtin_conj", BT_FN_DCOMPLEX_DCOMPLEX, BUILT_IN_CONJ, BUILT_IN_NORMAL, "carg", NULL, NULL},
{ "__builtin_conjf", BT_FN_FCOMPLEX_FCOMPLEX, BUILT_IN_CONJF, BUILT_IN_NORMAL, "conjf", NULL, NULL},
{ "__builtin_conjl", BT_FN_LDCOMPLEX_LDCOMPLEX, BUILT_IN_CONJL, BUILT_IN_NORMAL, "conjl", NULL, NULL},

{ "__builtin_cpow", BT_FN_DCOMPLEX_DOUBLE_DCOMPLEX, BUILT_IN_CPOW, BUILT_IN_NORMAL, "cpow", NULL, NULL},
{ "__builtin_cpowf", BT_FN_FCOMPLEX_FLOAT_FCOMPLEX, BUILT_IN_CPOWF, BUILT_IN_NORMAL, "cpowf", NULL, NULL},
{ "__builtin_cpowl", BT_FN_LDCOMPLEX_LONG_DOUBLE_LDCOMPLEX, BUILT_IN_CPOWL, BUILT_IN_NORMAL, "cpowl", NULL, NULL},

{ "__builtin_csqrt", BT_FN_DCOMPLEX_DCOMPLEX, BUILT_IN_CSQRT, BUILT_IN_NORMAL, "csqrt", NULL, NULL},
{ "__builtin_csqrtf", BT_FN_FCOMPLEX_FCOMPLEX, BUILT_IN_CSQRTF, BUILT_IN_NORMAL, "csqrtf", NULL, NULL},
{ "__builtin_csqrtl", BT_FN_LDCOMPLEX_LDCOMPLEX, BUILT_IN_CSQRTL, BUILT_IN_NORMAL, "csqrtl", NULL, NULL},

{ "__builtin_cexp", BT_FN_DCOMPLEX_DCOMPLEX, BUILT_IN_CEXP, BUILT_IN_NORMAL, "cexp", NULL, NULL},
{ "__builtin_cexpf", BT_FN_FCOMPLEX_FCOMPLEX, BUILT_IN_CEXPF, BUILT_IN_NORMAL, "cexpf", NULL, NULL},
{ "__builtin_cexpl", BT_FN_LDCOMPLEX_LDCOMPLEX, BUILT_IN_CEXPL, BUILT_IN_NORMAL, "cexpl", NULL, NULL},

{ "__builtin_cln", BT_FN_DCOMPLEX_DCOMPLEX, BUILT_IN_CLOG, BUILT_IN_NORMAL, "cln", NULL, NULL},
{ "__builtin_clnf", BT_FN_FCOMPLEX_FCOMPLEX, BUILT_IN_CLOGF, BUILT_IN_NORMAL, "clnf", NULL, NULL},
{ "__builtin_clnl", BT_FN_LDCOMPLEX_LDCOMPLEX, BUILT_IN_CLOGL, BUILT_IN_NORMAL, "clnl", NULL, NULL},

{ "__builtin_csin", BT_FN_DCOMPLEX_DCOMPLEX, BUILT_IN_CSIN, BUILT_IN_NORMAL, "csin", NULL, NULL},
{ "__builtin_csinf", BT_FN_FCOMPLEX_FCOMPLEX, BUILT_IN_CSINF, BUILT_IN_NORMAL, "csinf", NULL, NULL},
{ "__builtin_csinl", BT_FN_LDCOMPLEX_LDCOMPLEX, BUILT_IN_CSINL, BUILT_IN_NORMAL, "csinl", NULL, NULL},

{ "__builtin_ccos", BT_FN_DCOMPLEX_DCOMPLEX, BUILT_IN_CCOS, BUILT_IN_NORMAL, "ccos", NULL, NULL},
{ "__builtin_ccosf", BT_FN_FCOMPLEX_FCOMPLEX, BUILT_IN_CCOSF, BUILT_IN_NORMAL, "ccosf", NULL, NULL},
{ "__builtin_ccosl", BT_FN_LDCOMPLEX_LDCOMPLEX, BUILT_IN_CCOSL, BUILT_IN_NORMAL, "ccosl", NULL, NULL},

{ "__builtin_ctan", BT_FN_DCOMPLEX_DCOMPLEX, BUILT_IN_CTAN, BUILT_IN_NORMAL, "ctan", NULL, NULL},
{ "__builtin_ctanf", BT_FN_FCOMPLEX_FCOMPLEX, BUILT_IN_CTANF, BUILT_IN_NORMAL, "ctanf", NULL, NULL},
{ "__builtin_ctanl", BT_FN_LDCOMPLEX_LDCOMPLEX, BUILT_IN_CTANL, BUILT_IN_NORMAL, "ctanl", NULL, NULL},

{ "__builtin_casin", BT_FN_DCOMPLEX_DCOMPLEX, BUILT_IN_CASIN, BUILT_IN_NORMAL, "casin", NULL, NULL},
{ "__builtin_casinf", BT_FN_FCOMPLEX_FCOMPLEX, BUILT_IN_CASINF, BUILT_IN_NORMAL, "casinf", NULL, NULL},
{ "__builtin_casinl", BT_FN_LDCOMPLEX_LDCOMPLEX, BUILT_IN_CASINL, BUILT_IN_NORMAL, "casinl", NULL, NULL},

{ "__builtin_cacos", BT_FN_DCOMPLEX_DCOMPLEX, BUILT_IN_CACOS, BUILT_IN_NORMAL, "cacos", NULL, NULL},
{ "__builtin_cacosf", BT_FN_FCOMPLEX_FCOMPLEX, BUILT_IN_CACOSF, BUILT_IN_NORMAL, "cacosf", NULL, NULL},
{ "__builtin_cacosl", BT_FN_LDCOMPLEX_LDCOMPLEX, BUILT_IN_CACOSL, BUILT_IN_NORMAL, "cacosl", NULL, NULL},

{ "__builtin_catan", BT_FN_DCOMPLEX_DCOMPLEX, BUILT_IN_CATAN, BUILT_IN_NORMAL, "catan", NULL, NULL},
{ "__builtin_catanf", BT_FN_FCOMPLEX_FCOMPLEX, BUILT_IN_CATANF, BUILT_IN_NORMAL, "catanf", NULL, NULL},
{ "__builtin_catanl", BT_FN_LDCOMPLEX_LDCOMPLEX, BUILT_IN_CATANL, BUILT_IN_NORMAL, "catanl", NULL, NULL},

{ "__builtin_huge_val",   BT_FN_DOUBLE, BUILT_IN_HUGE_VAL, BUILT_IN_NORMAL, "hughe_val", NULL, NULL},
{ "__builtin_huge_valf",  BT_FN_FLOAT , BUILT_IN_HUGE_VALF, BUILT_IN_NORMAL, "hughe_valf", NULL, NULL},
{ "__builtin_huge_vall",  BT_FN_LONG_DOUBLE, BUILT_IN_HUGE_VALL, BUILT_IN_NORMAL, "hughe_vall", NULL, NULL},

{ "__builtin_index",   BT_FN_STRING_CONST_STRING_INT, BUILT_IN_INDEX, BUILT_IN_NORMAL, "index", NULL, NULL},
{ "__builtin_rindex",  BT_FN_STRING_CONST_STRING_INT, BUILT_IN_RINDEX, BUILT_IN_NORMAL, "rindex", NULL, NULL},
{ "__builtin_memcmp",  BT_FN_INT_CONST_PTR_CONST_PTR_SIZE, BUILT_IN_MEMCMP, BUILT_IN_NORMAL, "memcmp", NULL, NULL},
{ "__builtin_memmove", BT_FN_TRAD_PTR_PTR_CONST_PTR_SIZE, BUILT_IN_MEMMOVE, BUILT_IN_NORMAL, "memmove", NULL, NULL},
{ "__builtin_memset",  BT_FN_TRAD_PTR_PTR_INT_SIZE, BUILT_IN_MEMSET, BUILT_IN_NORMAL, "memset", NULL, NULL},
{ "__builtin_strcat",  BT_FN_STRING_STRING_CONST_STRING, BUILT_IN_STRCAT, BUILT_IN_NORMAL, "strcat", NULL, NULL},
{ "__builtin_strncat", BT_FN_STRING_STRING_CONST_STRING_SIZE, BUILT_IN_STRNCAT, BUILT_IN_NORMAL, "strncat", NULL, NULL},
{ "__builtin_strcpy",  BT_FN_STRING_STRING_CONST_STRING, BUILT_IN_STRCPY, BUILT_IN_NORMAL, "strcpy", NULL, NULL},
{ "__builtin_strncpy", BT_FN_STRING_STRING_CONST_STRING_SIZE, BUILT_IN_STRNCPY, BUILT_IN_NORMAL, "strncpy", NULL, NULL},
{ "__builtin_strcmp",  BT_FN_INT_CONST_STRING_CONST_STRING, BUILT_IN_STRCMP, BUILT_IN_NORMAL, "strcmp", NULL, NULL},
{ "__builtin_strncmp", BT_FN_INT_CONST_STRING_CONST_STRING_SIZE, BUILT_IN_STRNCMP, BUILT_IN_NORMAL, "strncmp", NULL, NULL},
{ "__builtin_strlen",  BT_FN_INT_CONST_STRING, BUILT_IN_STRLEN, BUILT_IN_NORMAL, "strlen", NULL, NULL},
{ "__builtin_strstr",  BT_FN_STRING_CONST_STRING_CONST_STRING, BUILT_IN_STRSTR, BUILT_IN_NORMAL, "strstr", NULL, NULL},
{ "__builtin_strpbrk", BT_FN_STRING_CONST_STRING_CONST_STRING, BUILT_IN_STRPBRK, BUILT_IN_NORMAL, "strpbrk", NULL, NULL},
{ "__builtin_strspn",  BT_FN_SIZE_CONST_STRING_CONST_STRING, BUILT_IN_STRSPN, BUILT_IN_NORMAL, "strspn", NULL, NULL},
{ "__builtin_strcspn", BT_FN_SIZE_CONST_STRING_CONST_STRING, BUILT_IN_STRCSPN, BUILT_IN_NORMAL, "strcspn", NULL, NULL},
{ "__builtin_strchr",  BT_FN_STRING_CONST_STRING_INT, BUILT_IN_STRCHR, BUILT_IN_NORMAL, "strchr", NULL, NULL},
{ "__builtin_strrchr", BT_FN_STRING_CONST_STRING_INT, BUILT_IN_STRCHR, BUILT_IN_NORMAL, "strrchr", NULL, NULL},
  //{ "__builtin_constant_p", BT_FN_INT_VAR, BUILT_IN_CONSTANT_P, BUILT_IN_NORMAL, "constant_p", NULL, NULL},
{ "__builtin_frame_address", BT_FN_PTR_UNSIGNED, BUILT_IN_FRAME_ADDRESS, BUILT_IN_NORMAL, "frame_address", NULL, NULL},
{ "__builtin_return_address", BT_FN_PTR_UNSIGNED, BUILT_IN_RETURN_ADDRESS, BUILT_IN_NORMAL, "return_address", NULL, NULL},
  //{ "__builtin_aggregate_incoming_address", BT_FN_PTR_VAR, BUILT_IN_AGGREGATE_INCOMING_ADDRESS, BUILT_IN_NORMAL, "aggregate_incoming_address", NULL, NULL},
{ "__builtin_longjmp", BT_FN_VOID_PTR_INT, BUILT_IN_LONGJMP, BUILT_IN_NORMAL, "longjmp", NULL, NULL},
{ "__builtin_setjmp", BT_FN_INT_PTR, BUILT_IN_SETJMP, BUILT_IN_NORMAL, "setjmp", NULL, NULL},
{ NULL, 0, 0, 0, "", NULL, NULL} };


struct builtin_type_info {
  const char *name;
  unsigned int returnType;
  tree (*functionHandler)(tree);
};

static GTY(()) tree sizetype_endlink;
static GTY(()) tree unsigned_endlink;
static GTY(()) tree endlink;
static GTY(()) tree math_endlink;
static GTY(()) tree int_endlink;
static GTY(()) tree ptr_endlink;
static GTY(()) tree const_ptr_endlink;
static GTY(()) tree double_ftype_void;
static GTY(()) tree float_ftype_void;
static GTY(()) tree ldouble_ftype_void;
static GTY(()) tree float_ftype_float;
static GTY(()) tree double_ftype_double;
static GTY(()) tree ldouble_ftype_ldouble;
static GTY(()) tree gm2_alloca_node;
static GTY(()) tree gm2_memcpy_node;
static GTY(()) tree gm2_huge_valf_node;
static GTY(()) tree gm2_huge_val_node;
static GTY(()) tree gm2_huge_vall_node;
static GTY(()) tree long_doubleptr_type_node;
static GTY(()) tree doubleptr_type_node;
static GTY(()) tree floatptr_type_node;


/* prototypes go here */
/* imports */
extern tree                   convertToPtr   		       	 	  (tree t);
extern tree                   gccgm2_BuildIntegerConstant                 (int value);
extern void                   gccgm2_SetLastFunction                      (tree t);
extern tree                   gccgm2_GetLastFunction                      (void);
extern void                   gccgm2_SetParamList                         (tree t);
extern tree                   gccgm2_GetParamList                         (void);
extern tree                   gccgm2_GetM2ShortRealType                   (void);
extern tree                   gccgm2_GetM2RealType                        (void);
extern tree                   gccgm2_GetM2LongRealType                    (void);
extern tree                   gccgm2_GetM2RType                           (void);
extern tree                   gccgm2_GetM2ZType                           (void);
extern tree                   gccgm2_GetBooleanTrue                       (void);
extern tree                   gccgm2_GetBooleanFalse                      (void);
extern tree                   gccgm2_IsTrue                               (tree t);
extern tree                   gccgm2_IsFalse                              (tree t);
extern tree                   gccgm2_BuildEqualTo                         (tree des, tree exp);
extern tree                   gccgm2_GetSizeOfInBits                      (tree t);
extern tree                   gccgm2_BuildLessThan                        (tree op1, tree op2);
extern tree                   skip_type_decl                              (tree type);

/* locally defined functions */
static tree                   DoBuiltinAlloca                             (tree params);
static tree                   DoBuiltinMemCopy                            (tree params);
       tree                   gm2builtins_BuildBuiltinTree                (char *name);
       tree                   gm2builtins_GetBuiltinConst                 (char *name);
       int                    gm2builtins_GetBuiltinConstType             (char *name);
       int                    gm2builtins_BuiltinExists                   (char *name);
       tree                   gm2builtins_BuildBuiltinTree                (char *name);
       tree                   gm2builtins_BuiltInMemCopy                  (tree, tree, tree);
       tree                   gm2builtins_BuiltInAlloca                   (tree);
       tree                   gm2builtins_BuiltInHugeVal                  (void);
static void                   create_function_prototype                   (struct builtin_function_entry *fe);
       void                   gm2builtins_init                            (void);
       tree                   gm2builtins_BuiltInHugeValShort             (void);
       tree                   gm2builtins_BuiltInHugeValLong              (void);
static tree                   doradix                                     (tree type);
static tree                   doplaces                                    (tree type);
static tree                   doexponentmin                               (tree type);
static tree                   doexponentmax                               (tree type);
static tree                   dolarge                                     (tree type);
static tree                   dosmall                                     (tree type);
static tree                   doiec559                                    (tree type);
static tree                   dolia1                                      (tree type);
static tree                   doiso                                       (tree type);
static tree                   doieee                                      (tree type);
static tree                   dorounds                                    (tree type);
static tree                   dogUnderflow                                (tree type);
static tree                   doexception                                 (tree type);
static tree                   doextend                                    (tree type);
static tree                   donModes                                    (tree type);
       unsigned int           gm2builtins_GetBuiltinTypeInfoType          (const char *ident);
       tree                   gm2builtins_GetBuiltinTypeInfo              (tree type, const char *ident);
/* prototypes finish here */

static struct builtin_type_info m2_type_info[] = {
  { "radix", 2, doradix },
  { "places", 2, doplaces },
  { "expoMin", 2, doexponentmin },
  { "expoMax", 2, doexponentmax },
  { "large", 3, dolarge },
  { "small", 3, dosmall },
  { "IEC559", 1, doiec559 },
  { "LIA1", 1, dolia1 },
  { "ISO", 1, doiso },
  { "IEEE", 1, doieee },
  { "rounds", 1, dorounds },
  { "gUnderflow", 1, dogUnderflow },
  { "exception", 1, doexception },
  { "extend", 1, doextend },
  { "nModes", 2, donModes },
  { NULL, 0, NULL },
};



/* Given a chain CHAIN of tree nodes,
   construct and return a list of those nodes.  */

static tree
listify (tree chain)
{
  tree result = NULL_TREE;
  tree in_tail = chain;
  tree out_tail = NULL_TREE;

  while (in_tail)
    {
      tree next = tree_cons (NULL_TREE, in_tail, NULL_TREE);
      if (out_tail)
	TREE_CHAIN (out_tail) = next;
      else
	result = next;
      out_tail = next;
      in_tail = TREE_CHAIN (in_tail);
    }

  return result;
}

/* Return a definition for a builtin function named NAME and whose data type
   is TYPE.  TYPE should be a function type with argument types.
   FUNCTION_CODE tells later passes how to compile calls to this function.
   See tree.h for its possible values.

   If LIBRARY_NAME is nonzero, use that for DECL_ASSEMBLER_NAME,
   the name to be called if we can't opencode the function.  */

tree
builtin_function (const char *name, tree type, int function_code,
		  enum built_in_class class, const char *library_name,
		  tree attrs ATTRIBUTE_UNUSED)
{
  tree decl = build_decl (FUNCTION_DECL, get_identifier (name), type);

  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  if (library_name)
    SET_DECL_ASSEMBLER_NAME (decl, get_identifier (library_name));

  pushdecl (decl);
  DECL_BUILT_IN_CLASS (decl) = class;
  DECL_FUNCTION_CODE (decl) = function_code;

#if 0
  if (attrs)
    decl_attributes (&decl, attrs, ATTR_FLAG_BUILT_IN);
#endif

  return decl;
}

/*
 *  GetBuiltinConst - returns the gcc tree of a builtin constant, name.
 *                    NIL is returned if the constant is unknown.
 */

tree
gm2builtins_GetBuiltinConst (char *name)
{
  if (strcmp(name, "BITS_PER_UNIT") == 0)
    return gccgm2_BuildIntegerConstant (BITS_PER_UNIT);
  if (strcmp(name, "BITS_PER_WORD") == 0)
    return gccgm2_BuildIntegerConstant (BITS_PER_WORD);
  if (strcmp (name, "BITS_PER_CHAR") == 0)
    return gccgm2_BuildIntegerConstant (CHAR_TYPE_SIZE);
  if (strcmp (name, "UNITS_PER_WORD") == 0)
    return gccgm2_BuildIntegerConstant (UNITS_PER_WORD);
  
  return NULL_TREE;
}

/*
 *  GetBuiltinConstType - returns the type of a builtin constant, name.
 *
 *                        0 = unknown constant name
 *                        1 = integer
 *                        2 = real
 */

int
gm2builtins_GetBuiltinConstType (char *name)
{
  if (strcmp(name, "BITS_PER_UNIT") == 0)
    return 1;
  if (strcmp(name, "BITS_PER_WORD") == 0)
    return 1;
  if (strcmp (name, "BITS_PER_CHAR") == 0)
    return 1;
  if (strcmp (name, "UNITS_PER_WORD") == 0)
    return 1;
  
  return 0;
}

/*
 *  GetBuiltinTypeInfoType - returns value:
 *                           0    is ident is unknown.
 *                           1    if ident is IEC559, LIA1, ISO, IEEE, rounds, underflow,
 *                                            exception, extend.
 *                           2    if ident is radix, places, exponentmin, exponentmax,
 *                                            noofmodes.
 *                           3    if ident is large, small.
 */

unsigned int gm2builtins_GetBuiltinTypeInfoType (const char *ident)
{
  int i=0;

  while (m2_type_info[i].name != NULL)
    if (strcmp (m2_type_info[i].name, ident) == 0)
      return m2_type_info[i].returnType;
    else
      i++;
  return 0;
}

/*
 *  GetBuiltinTypeInfo - returns value:
 *
 *                       NULL_TREE        if ident is unknown.
 *                       boolean Tree     if ident is IEC559, LIA1, ISO, IEEE, rounds, underflow,
 *                                        exception, extend.
 *                       ZType Tree       if ident is radix, places, exponentmin, exponentmax,
 *                                        noofmodes.
 *                       RType Tree       if ident is large, small.
 */

tree gm2builtins_GetBuiltinTypeInfo (tree type, const char *ident)
{
  int i=0;

  type = skip_type_decl (type);
  while (m2_type_info[i].name != NULL)
    if (strcmp (m2_type_info[i].name, ident) == 0)
      return (*m2_type_info[i].functionHandler)(type);
    else
      i++;
  return NULL_TREE;
}

/*
 *  doradix - returns the radix of the floating point, type.
 */

static tree doradix (tree type)
{
  if (TREE_CODE (type) == REAL_TYPE)
    {
      enum machine_mode mode = TYPE_MODE (type);
      int radix = REAL_MODE_FORMAT (mode)->b;
      return gccgm2_BuildIntegerConstant (radix);
    }
  else
    return NULL_TREE;
}

/*
 *  doplaces - returns the whole number value of the number of radix
 *             places used to store values of the corresponding real
 *             number type.
 */

static tree doplaces (tree type)
{
  if (TREE_CODE (type) == REAL_TYPE)
    {
      /*  taken from c-cppbuiltin.c */
      /*  The number of decimal digits, q, such that any floating-point number
       *  with q decimal digits can be rounded into a floating-point number with
       *  p radix b digits and back again without change to the q decimal digits,
       *
       *  p log10 b			if b is a power of 10
       *  floor((p - 1) log10 b)		otherwise
      */
      enum machine_mode mode = TYPE_MODE (type);
      const double log10_2 = .30102999566398119521;
      double log10_b = log10_2 * REAL_MODE_FORMAT (mode)->log2_b;
      int digits = (REAL_MODE_FORMAT (mode)->p -1) * log10_b;
      return gccgm2_BuildIntegerConstant (digits);
    }
  else
    return NULL_TREE;
}

/*
 *  doexponentmin - returns the whole number of the exponent minimum.
 */

static tree doexponentmin (tree type)
{
  if (TREE_CODE (type) == REAL_TYPE)
    {
      enum machine_mode mode = TYPE_MODE (type);
      int emin = REAL_MODE_FORMAT (mode)->emin;
      return gccgm2_BuildIntegerConstant (emin);
    }
  else
    return NULL_TREE;
}

/*
 *  doexponentmax - returns the whole number of the exponent maximum.
 */

static tree doexponentmax (tree type)
{
  if (TREE_CODE (type) == REAL_TYPE)
    {
      enum machine_mode mode = TYPE_MODE (type);
      int emax = REAL_MODE_FORMAT (mode)->emax;
      return gccgm2_BuildIntegerConstant (emax);
    }
  else
    return NULL_TREE;
}

static tree computeLarge (tree type)
{
  enum machine_mode mode = TYPE_MODE (type);
  const struct real_format *fmt = REAL_MODE_FORMAT (mode);
  REAL_VALUE_TYPE real;
  char buf[128];

  /* shamelessly taken from c-cppbuiltin.c:builtin_define_float_constants */

  /* Since, for the supported formats, B is always a power of 2, we
     construct the following numbers directly as a hexadecimal
     constants.  */

  /* The maximum representable finite floating-point number,
     (1 - b**-p) * b**emax  */
  {
    int i, n;
    char *p;

    strcpy (buf, "0x0.");
    n = fmt->p * fmt->log2_b;
    for (i = 0, p = buf + 4; i + 3 < n; i += 4)
      *p++ = 'f';
    if (i < n)
      *p++ = "08ce"[n - i];
    sprintf (p, "p%d", fmt->emax * fmt->log2_b);
    if (fmt->pnan < fmt->p)
      {
	/* This is an IBM extended double format made up of two IEEE
	   doubles.  The value of the long double is the sum of the
	   values of the two parts.  The most significant part is
	   required to be the value of the long double rounded to the
	   nearest double.  Rounding means we need a slightly smaller
	   value for LDBL_MAX.  */
	buf[4 + fmt->pnan / 4] = "7bde"[fmt->pnan % 4];
      }
  }
  real_from_string (&real, buf);
  return build_real (type, real);
}

/*
 *  dolarge - return the largest value of the corresponding real type.
 */

static tree dolarge (tree type)
{
  if (TREE_CODE (type) == REAL_TYPE)
    return computeLarge (type);
  return NULL_TREE;
}

static tree computeSmall (tree type)
{
  enum machine_mode mode = TYPE_MODE (type);
  const struct real_format *fmt = REAL_MODE_FORMAT (mode);
  REAL_VALUE_TYPE real;
  char buf[128];

  /* The minimum normalized positive floating-point number,
     b**(emin-1).  */

  sprintf (buf, "0x1p%d", (fmt->emin - 1) * fmt->log2_b);
  real_from_string (&real, buf);
  return build_real (type, real);
}

/*
 *  dosmall - return the smallest positive value of the
 *            corresponding real type.
 */

static tree dosmall (tree type)
{
  if (TREE_CODE (type) == REAL_TYPE)
    return computeSmall (type);
  return NULL_TREE;
}

/*
 *  doiec559 - a boolean value that is true if and only if the
 *             implementation of the corresponding real number
 *             type conforms to IEC 559:1989 (also known as
 *             IEEE 754:1987) in all regards.
 */

static tree doiec559 (tree type)
{
  if (gccgm2_IsTrue (gccgm2_BuildEqualTo (gccgm2_BuildIntegerConstant (32),
					  gccgm2_GetSizeOfInBits (type))))
    return gccgm2_GetBooleanTrue ();
  if (gccgm2_IsTrue (gccgm2_BuildEqualTo (gccgm2_BuildIntegerConstant (64),
					  gccgm2_GetSizeOfInBits (type))))
    return gccgm2_GetBooleanTrue ();
  return gccgm2_GetBooleanFalse ();
}

/*
 *  dolia1 - returns TRUE if 
 */

static tree dolia1 (tree type)
{
  return doieee (type);
}

/*
 *
 */

static tree doiso (tree type)
{
  return doieee (type);
}

/*
 *
 */

static tree doieee (tree type ATTRIBUTE_UNUSED)
{
  if (IEEE_FLOAT_FORMAT)
    return gccgm2_GetBooleanTrue ();
  else
    return gccgm2_GetBooleanFalse ();
}

/*
 *  dorounds - returns TRUE if and only if each operation produces
 *             a result that is one of the values of the
 *             corresponding real number type nearest to the
 *             mathematical result.
 */

static tree dorounds (tree type ATTRIBUTE_UNUSED)
{
  if (FLT_ROUNDS)
    return gccgm2_GetBooleanTrue ();
  else
    return gccgm2_GetBooleanFalse ();
}

/*
 *  dogUnderflow - returns TRUE if and only if there are
 *                 values of the corresponding real number
 *                 type between 0.0 and small.
 */

static tree dogUnderflow (tree type)
{
  if (TREE_CODE (type) == REAL_TYPE)
    {
      enum machine_mode mode = TYPE_MODE (type);
      const struct real_format *fmt = REAL_MODE_FORMAT (mode);
      if (fmt->has_denorm)
	return gccgm2_GetBooleanTrue ();
      else
	return gccgm2_GetBooleanFalse ();
    }
  return NULL_TREE;
}

/*
 *  doexception -
 */

static tree doexception (tree type ATTRIBUTE_UNUSED)
{
  return gccgm2_GetBooleanTrue ();
}

/*
 *  doextend -
 */

static tree doextend (tree type ATTRIBUTE_UNUSED)
{
  return gccgm2_GetBooleanTrue ();
}

/*
 *  donModes - 
 */

static tree donModes (tree type ATTRIBUTE_UNUSED)
{
  return gccgm2_BuildIntegerConstant (1);
}


/*
 *  BuiltInMemCopy - copy n bytes of memory efficiently from address
 *                   src to dest.
 */

tree
gm2builtins_BuiltInMemCopy (tree dest, tree src, tree n)
{
  tree params = chainon (chainon (build_tree_list (NULL_TREE, convertToPtr (dest)),
				  build_tree_list (NULL_TREE, convertToPtr (src))),
			 build_tree_list (NULL_TREE, n));
  return DoBuiltinMemCopy (params);
}

/*
 *  BuiltInAlloca - given an expression, n, allocate, n, bytes on
 *                  the stack for the life of the current function.
 */

tree
gm2builtins_BuiltInAlloca (tree n)
{
  return DoBuiltinAlloca (listify (n));
}

/*
 *  BuiltinExists - returns TRUE if the builtin function, name, exists
 *                  for this target architecture.
 */

int
gm2builtins_BuiltinExists (char *name)
{
  struct builtin_function_entry *fe;

  for (fe=&list_of_builtins[0]; fe->name != NULL; fe++)
    if (strcmp(name, fe->name) == 0)
      return TRUE;

  return FALSE;
}

/*
 *  BuildBuiltinTree - returns a Tree containing the builtin function, name.
 */

tree
gm2builtins_BuildBuiltinTree (char *name)
{
  struct builtin_function_entry *fe;
  tree t;

  gccgm2_SetLastFunction(NULL_TREE);
  for (fe=&list_of_builtins[0]; fe->name != NULL; fe++)
    if (strcmp(name, fe->name) == 0) {
      tree functype = TREE_TYPE (fe->function_node);
      tree funcptr  = build1 (ADDR_EXPR, build_pointer_type (functype), fe->function_node);
      gccgm2_SetLastFunction(build (CALL_EXPR, fe->return_node,
				    funcptr, gccgm2_GetParamList (), NULL_TREE));

      gccgm2_SetParamList (NULL_TREE);
      t = gccgm2_GetLastFunction ();
      if (fe->return_node == void_type_node)
	gccgm2_SetLastFunction(NULL_TREE);
      return t;
    }
  
  gccgm2_SetParamList(NULL_TREE);
  return gccgm2_GetLastFunction ();
}

static tree
DoBuiltinMemCopy (tree params)
{
  tree functype = TREE_TYPE (gm2_memcpy_node);
  tree funcptr  = build1 (ADDR_EXPR, build_pointer_type (functype), gm2_memcpy_node);
  tree call     = build (CALL_EXPR, ptr_type_node, funcptr, params, NULL_TREE);

#if 0
  fprintf(stderr, "built the modula-2 call, here are the params\n"); fflush(stderr);
  debug_tree (params);
  fprintf(stderr, "built the modula-2 call, here is the tree\n"); fflush(stderr);
  debug_tree (call);
#endif
  return call;
}

static tree
DoBuiltinAlloca (tree params)
{
  tree functype = TREE_TYPE (gm2_alloca_node);
  tree funcptr  = build1 (ADDR_EXPR, build_pointer_type (functype), gm2_alloca_node);
  tree call     = build (CALL_EXPR, ptr_type_node, funcptr, params, NULL_TREE);

  return call;
}

tree
gm2builtins_BuiltInHugeVal (void)
{
  tree functype = TREE_TYPE (gm2_huge_val_node);
  tree funcptr  = build1 (ADDR_EXPR, build_pointer_type (functype), gm2_huge_val_node);
  tree call     = build (CALL_EXPR, ptr_type_node, funcptr, NULL_TREE, NULL_TREE);
  return call;
}

tree
gm2builtins_BuiltInHugeValShort (void)
{
  tree functype = TREE_TYPE (gm2_huge_valf_node);
  tree funcptr  = build1 (ADDR_EXPR, build_pointer_type (functype), gm2_huge_valf_node);
  tree call     = build (CALL_EXPR, ptr_type_node, funcptr, NULL_TREE, NULL_TREE);
  return call;
}

tree
gm2builtins_BuiltInHugeValLong (void)
{
  tree functype = TREE_TYPE (gm2_huge_vall_node);
  tree funcptr  = build1 (ADDR_EXPR, build_pointer_type (functype), gm2_huge_vall_node);
  tree call     = build (CALL_EXPR, ptr_type_node, funcptr, NULL_TREE, NULL_TREE);
  return call;
}

static void
create_function_prototype (struct builtin_function_entry *fe)
{
  tree ftype;

  switch (fe->defn) {

  case BT_FN_PTR_SIZE:
    ftype = build_function_type (ptr_type_node, sizetype_endlink);
    fe->return_node = ptr_type_node;
    break;

  case BT_FN_STRING_STRING_CONST_STRING_SIZE:
  case BT_FN_TRAD_PTR_PTR_CONST_PTR_SIZE:
    ftype = build_function_type (ptr_type_node,
				 tree_cons (NULL_TREE, ptr_type_node,
					    tree_cons (NULL_TREE, const_ptr_type_node,
						       sizetype_endlink)));
    fe->return_node = ptr_type_node;
    break;
  case BT_FN_FLOAT:
    ftype = float_ftype_void;
    fe->return_node = float_type_node;
    break;
  case BT_FN_DOUBLE:
    ftype = double_ftype_void;
    fe->return_node = double_type_node;
    break;
  case BT_FN_LONG_DOUBLE:
    ftype = ldouble_ftype_void;
    fe->return_node = long_double_type_node;
    break;
  case BT_FN_FLOAT_FLOAT:
    ftype = float_ftype_float;
    fe->return_node = float_type_node;
    break;
  case BT_FN_DOUBLE_DOUBLE:
    ftype = double_ftype_double;
    fe->return_node = double_type_node;
    break;
  case BT_FN_LONG_DOUBLE_LONG_DOUBLE:
    ftype = ldouble_ftype_ldouble;
    fe->return_node = long_double_type_node;
    break;
  case BT_FN_STRING_CONST_STRING_INT:
    ftype = build_function_type (ptr_type_node,
				 tree_cons (NULL_TREE, ptr_type_node,
					    int_endlink));
    fe->return_node = ptr_type_node;
    break;
  case BT_FN_INT_CONST_PTR_CONST_PTR_SIZE:
    ftype = build_function_type (integer_type_node,
				 tree_cons (NULL_TREE, const_ptr_type_node,
					    tree_cons (NULL_TREE, const_ptr_type_node,
						       int_endlink)));
    fe->return_node = integer_type_node;
    break;
  case BT_FN_TRAD_PTR_PTR_INT_SIZE:
    ftype = build_function_type (ptr_type_node,
				 tree_cons (NULL_TREE, ptr_type_node,
					    tree_cons (NULL_TREE, integer_type_node,
						       sizetype_endlink)));
    fe->return_node = ptr_type_node;
    break;
  case BT_FN_STRING_STRING_CONST_STRING:
    ftype = build_function_type (ptr_type_node,
				 tree_cons (NULL_TREE, ptr_type_node,
					    ptr_endlink));
    fe->return_node = ptr_type_node;
    break;
  case BT_FN_INT_CONST_STRING_CONST_STRING:
    ftype = build_function_type (integer_type_node,
				 tree_cons (NULL_TREE, const_ptr_type_node,
					    ptr_endlink));
    fe->return_node = integer_type_node;
    break;
  case BT_FN_INT_CONST_STRING_CONST_STRING_SIZE:
    ftype = build_function_type (integer_type_node,
				 tree_cons (NULL_TREE, const_ptr_type_node,
					    tree_cons (NULL_TREE, const_ptr_type_node,
						       sizetype_endlink)));
    fe->return_node = integer_type_node;
    break;
  case BT_FN_INT_CONST_STRING:
    ftype = build_function_type (integer_type_node, ptr_endlink);
    fe->return_node = integer_type_node;
    break;
  case BT_FN_STRING_CONST_STRING_CONST_STRING:
    ftype = build_function_type (ptr_type_node,
				 tree_cons (NULL_TREE, const_ptr_type_node,
					    const_ptr_endlink));
    fe->return_node = ptr_type_node;
    break;
  case BT_FN_SIZE_CONST_STRING_CONST_STRING:
    ftype = build_function_type (sizetype,
				 tree_cons (NULL_TREE, const_ptr_type_node,
					    const_ptr_endlink));
    fe->return_node = sizetype;
    break;
  case BT_FN_PTR_UNSIGNED:
    ftype = build_function_type (ptr_type_node, unsigned_endlink);
    fe->return_node = ptr_type_node;
    break;
  case BT_FN_VOID_PTR_INT:
    ftype = build_function_type (void_type_node,
				 tree_cons (NULL_TREE, ptr_type_node,
					    int_endlink));
    fe->return_node = void_type_node;
    break;
  case BT_FN_INT_PTR:
    ftype = build_function_type (integer_type_node, ptr_endlink);
    fe->return_node = integer_type_node;
    break;
  case BT_FN_INT_FLOAT:
    ftype = build_function_type (integer_type_node,
				 tree_cons (NULL_TREE, float_type_node,
					    endlink));
    fe->return_node = integer_type_node;
    break;
  case BT_FN_INT_DOUBLE:
    ftype = build_function_type (integer_type_node,
				 tree_cons (NULL_TREE, double_type_node,
					    endlink));
    fe->return_node = integer_type_node;
    break;
  case BT_FN_INT_LONG_DOUBLE:
    ftype = build_function_type (integer_type_node,
				 tree_cons (NULL_TREE, long_double_type_node,
					    endlink));
    fe->return_node = integer_type_node;
    break;
  case BT_FN_FLOAT_FCOMPLEX:
    ftype = build_function_type (float_type_node,
				 tree_cons (NULL_TREE, complex_float_type_node,
					    endlink));
    fe->return_node = float_type_node;
    break;
  case BT_FN_DOUBLE_DCOMPLEX:
    ftype = build_function_type (double_type_node,
				 tree_cons (NULL_TREE, complex_double_type_node,
					    endlink));
    fe->return_node = double_type_node;
    break;
  case BT_FN_LONG_DOUBLE_LDCOMPLEX:
    ftype = build_function_type (long_double_type_node,
				 tree_cons (NULL_TREE, complex_long_double_type_node,
					    endlink));
    fe->return_node = long_double_type_node;
    break;
  case BT_FN_FCOMPLEX_FCOMPLEX:
    ftype = build_function_type (complex_float_type_node,
				 tree_cons (NULL_TREE, complex_float_type_node,
					    endlink));
    fe->return_node = complex_float_type_node;
    break;
  case BT_FN_DCOMPLEX_DCOMPLEX:
    ftype = build_function_type (complex_double_type_node,
				 tree_cons (NULL_TREE, complex_double_type_node,
					    endlink));
    fe->return_node = complex_double_type_node;
    break;
  case BT_FN_LDCOMPLEX_LDCOMPLEX:
    ftype = build_function_type (complex_long_double_type_node,
				 tree_cons (NULL_TREE, complex_long_double_type_node,
					    endlink));
    fe->return_node = complex_long_double_type_node;
    break;
  case BT_FN_DCOMPLEX_DOUBLE_DCOMPLEX:
    ftype = build_function_type (complex_double_type_node,
				 tree_cons (NULL_TREE, complex_double_type_node,
					    tree_cons (NULL_TREE, double_type_node,
						       endlink)));
    fe->return_node = complex_double_type_node;
    break;
  case BT_FN_FCOMPLEX_FLOAT_FCOMPLEX:
    ftype = build_function_type (complex_float_type_node,
				 tree_cons (NULL_TREE, complex_float_type_node,
					    tree_cons (NULL_TREE, float_type_node,
						       endlink)));
    fe->return_node = complex_float_type_node;
    break;
  case BT_FN_LDCOMPLEX_LONG_DOUBLE_LDCOMPLEX:
    ftype = build_function_type (complex_long_double_type_node,
				 tree_cons (NULL_TREE, complex_long_double_type_node,
					    tree_cons (NULL_TREE, long_double_type_node,
						       endlink)));
    fe->return_node = complex_long_double_type_node;
    break;
  case BT_FN_FLOAT_FLOAT_FLOATPTR:
    ftype = build_function_type (float_type_node,
				 tree_cons (NULL_TREE, float_type_node,
					    tree_cons (NULL_TREE, floatptr_type_node,
						       endlink)));
    fe->return_node = float_type_node;
    break;
  case BT_FN_DOUBLE_DOUBLE_DOUBLEPTR:
    ftype = build_function_type (double_type_node,
				 tree_cons (NULL_TREE, double_type_node,
					    tree_cons (NULL_TREE, doubleptr_type_node,
						       endlink)));
    fe->return_node = double_type_node;
    break;
  case BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG_DOUBLEPTR:
    ftype = build_function_type (long_double_type_node,
				 tree_cons (NULL_TREE, long_double_type_node,
					    tree_cons (NULL_TREE, long_doubleptr_type_node,
						       endlink)));
    fe->return_node = long_double_type_node;
    break;
  case BT_FN_FLOAT_FLOAT_LONG_DOUBLE:
    ftype = build_function_type (float_type_node,
				 tree_cons (NULL_TREE, float_type_node,
					    tree_cons (NULL_TREE, long_double_type_node,
						       endlink)));
    fe->return_node = float_type_node;
    break;
  case BT_FN_DOUBLE_DOUBLE_LONG_DOUBLE:
    ftype = build_function_type (double_type_node,
				 tree_cons (NULL_TREE, double_type_node,
					    tree_cons (NULL_TREE, long_double_type_node,
						       endlink)));
    fe->return_node = double_type_node;
    break;
  case BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG_DOUBLE:
    ftype = build_function_type (long_double_type_node,
				 tree_cons (NULL_TREE, long_double_type_node,
					    tree_cons (NULL_TREE, long_double_type_node,
						       endlink)));
    fe->return_node = long_double_type_node;
    break;
  case BT_FN_FLOAT_FLOAT_LONG:
    ftype = build_function_type (float_type_node,
				 tree_cons (NULL_TREE, float_type_node,
					    tree_cons (NULL_TREE, long_integer_type_node,
						       endlink)));
    fe->return_node = float_type_node;
    break;
  case BT_FN_DOUBLE_DOUBLE_LONG:
    ftype = build_function_type (double_type_node,
				 tree_cons (NULL_TREE, double_type_node,
					    tree_cons (NULL_TREE, long_integer_type_node,
						       endlink)));
    fe->return_node = double_type_node;
    break;
  case BT_FN_LONG_DOUBLE_LONG_DOUBLE_LONG:
    ftype = build_function_type (long_double_type_node,
				 tree_cons (NULL_TREE, long_double_type_node,
					    tree_cons (NULL_TREE, long_integer_type_node,
						       endlink)));
    fe->return_node = long_double_type_node;
    break;
  case BT_FN_FLOAT_FLOAT_INT:
    ftype = build_function_type (float_type_node,
				 tree_cons (NULL_TREE, float_type_node,
					    tree_cons (NULL_TREE, integer_type_node,
						       endlink)));
    fe->return_node = float_type_node;
    break;
  case BT_FN_DOUBLE_DOUBLE_INT:
    ftype = build_function_type (double_type_node,
				 tree_cons (NULL_TREE, double_type_node,
					    tree_cons (NULL_TREE, integer_type_node,
						       endlink)));
    fe->return_node = double_type_node;
    break;
  case BT_FN_LONG_DOUBLE_LONG_DOUBLE_INT:
    ftype = build_function_type (long_double_type_node,
				 tree_cons (NULL_TREE, long_double_type_node,
					    tree_cons (NULL_TREE, integer_type_node,
						       endlink)));
    fe->return_node = long_double_type_node;
    break;
  case BT_FN_FLOAT_FLOAT_FLOAT:
    ftype = build_function_type (float_type_node,
				 tree_cons (NULL_TREE, float_type_node,
					    tree_cons (NULL_TREE, float_type_node,
						       endlink)));
    fe->return_node = float_type_node;
    break;
  case BT_FN_DOUBLE_DOUBLE_DOUBLE:
    ftype = build_function_type (double_type_node,
				 tree_cons (NULL_TREE, double_type_node,
					    tree_cons (NULL_TREE, double_type_node,
						       endlink)));
    fe->return_node = double_type_node;
    break;
  default:
    ERROR("enum has no case");
  }
  fe->function_node = builtin_function (fe->name, ftype, fe->function_code, fe->class, fe->library_name, NULL);
}

static tree
find_builtin_tree (const char *name)
{
  struct builtin_function_entry *fe;

  for (fe=&list_of_builtins[0]; fe->name != NULL; fe++)
    if (strcmp(name, fe->name) == 0)
      return fe->function_node;

  ERROR ("cannot find builtin function");
  return NULL_TREE;
}

void
gm2builtins_init (void)
{
  int i;

  endlink = void_list_node;
  sizetype_endlink = tree_cons (NULL_TREE, sizetype, endlink);
  math_endlink = tree_cons (NULL_TREE, void_type_node, NULL_TREE);
  int_endlink =  tree_cons (NULL_TREE, integer_type_node, NULL_TREE);
  ptr_endlink =  tree_cons (NULL_TREE, ptr_type_node, NULL_TREE);
  const_ptr_endlink =  tree_cons (NULL_TREE, const_ptr_type_node, NULL_TREE);
  unsigned_endlink = tree_cons (NULL_TREE, unsigned_type_node, NULL_TREE);

  float_ftype_void = build_function_type (float_type_node, math_endlink);
  double_ftype_void = build_function_type (double_type_node, math_endlink);
  ldouble_ftype_void = build_function_type (long_double_type_node,
					    math_endlink);

  long_doubleptr_type_node = build_pointer_type (long_double_type_node);
  doubleptr_type_node = build_pointer_type (double_type_node);
  floatptr_type_node = build_pointer_type (float_type_node);

  float_ftype_float
    = build_function_type (float_type_node,
 			   tree_cons (NULL_TREE, float_type_node, math_endlink));

  double_ftype_double
    = build_function_type (double_type_node,
			   tree_cons (NULL_TREE, double_type_node, math_endlink));
			   
  ldouble_ftype_ldouble
    = build_function_type (long_double_type_node,
			   tree_cons (NULL_TREE, long_double_type_node,
				      endlink));
  
  for (i=0; list_of_builtins[i].name != NULL; i++)
    create_function_prototype (&list_of_builtins[i]);

  gm2_alloca_node = find_builtin_tree ("__builtin_alloca");
  gm2_memcpy_node = find_builtin_tree ("__builtin_memcpy");
  gm2_huge_valf_node = find_builtin_tree ("__builtin_huge_valf");
  gm2_huge_val_node = find_builtin_tree ("__builtin_huge_val");
  gm2_huge_vall_node = find_builtin_tree ("__builtin_huge_vall");
}

#include "gt-gm2-gm2builtins.h"

/* END gm2builtins. */
