/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/varargs.def.  */


#if !defined (_varargs_H)
#   define _varargs_H

#ifdef __cplusplus
extern "C" {
#endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_varargs_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

#if !defined (varargs_vararg_D)
#  define varargs_vararg_D
   typedef void *varargs_vararg;
#endif


/*
   nargs - returns the number of arguments wrapped in, v.
*/

EXTERN unsigned int varargs_nargs (varargs_vararg v);

/*
   arg - fills in, a, with the next argument.  The size of, a, must
         be an exact match with the original vararg parameter.
*/

EXTERN void varargs_arg (varargs_vararg v, unsigned char *a, unsigned int _a_high);

/*
   next - assigns the next arg to be collected as, i.
*/

EXTERN void varargs_next (varargs_vararg v, unsigned int i);

/*
   copy - returns a copy of, v.
*/

EXTERN varargs_vararg varargs_copy (varargs_vararg v);

/*
   replace - fills the next argument with, a.  The size of, a,
             must be an exact match with the original vararg
             parameter.
*/

EXTERN void varargs_replace (varargs_vararg v, unsigned char *a, unsigned int _a_high);

/*
   end - destructor for vararg, v.
*/

EXTERN void varargs_end (varargs_vararg *v);

/*
   start1 - wraps up argument, a, into a vararg.
*/

EXTERN varargs_vararg varargs_start1 (unsigned char *a_, unsigned int _a_high);

/*
   start2 - wraps up arguments, a, b, into a vararg.
*/

EXTERN varargs_vararg varargs_start2 (unsigned char *a_, unsigned int _a_high, unsigned char *b_, unsigned int _b_high);

/*
   start3 - wraps up arguments, a, b, c, into a vararg.
*/

EXTERN varargs_vararg varargs_start3 (unsigned char *a_, unsigned int _a_high, unsigned char *b_, unsigned int _b_high, unsigned char *c_, unsigned int _c_high);

/*
   start4 - wraps up arguments, a, b, c, d, into a vararg.
*/

EXTERN varargs_vararg varargs_start4 (unsigned char *a_, unsigned int _a_high, unsigned char *b_, unsigned int _b_high, unsigned char *c_, unsigned int _c_high, unsigned char *d_, unsigned int _d_high);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
