/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcError.def.  */


#if !defined (_mcError_H)
#   define _mcError_H

#ifdef __cplusplus
extern "C" {
#endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"
#   include "GDynamicStrings.h"

#   if defined (_mcError_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

#if !defined (mcError_error_D)
#  define mcError_error_D
   typedef void *mcError_error;
#endif


/*
   internalError - displays an internal error message together with the compiler source
                   file and line number.
                   This function is not buffered and is used when the compiler is about
                   to give up.
*/

EXTERN void mcError_internalError (char *a_, unsigned int _a_high, char *file_, unsigned int _file_high, unsigned int line);

/*
   writeFormat0 - displays the source module and line together
                  with the encapsulated format string.
                  Used for simple error messages tied to the current token.
*/

EXTERN void mcError_writeFormat0 (char *a_, unsigned int _a_high);

/*
   writeFormat1 - displays the source module and line together
                  with the encapsulated format string.
                  Used for simple error messages tied to the current token.
*/

EXTERN void mcError_writeFormat1 (char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);

/*
   writeFormat2 - displays the module and line together with the encapsulated
                  format strings.
                  Used for simple error messages tied to the current token.
*/

EXTERN void mcError_writeFormat2 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);

/*
   writeFormat3 - displays the module and line together with the encapsulated
                  format strings.
                  Used for simple error messages tied to the current token.
*/

EXTERN void mcError_writeFormat3 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);

/*
   newError - creates and returns a new error handle.
*/

EXTERN mcError_error mcError_newError (unsigned int atTokenNo);

/*
   newWarning - creates and returns a new error handle suitable for a warning.
                A warning will not stop compilation.
*/

EXTERN mcError_error mcError_newWarning (unsigned int atTokenNo);

/*
   chainError - creates and returns a new error handle, this new error
                is associated with, e, and is chained onto the end of, e.
*/

EXTERN mcError_error mcError_chainError (unsigned int atTokenNo, mcError_error e);
EXTERN void mcError_errorFormat0 (mcError_error e, char *a_, unsigned int _a_high);
EXTERN void mcError_errorFormat1 (mcError_error e, char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);
EXTERN void mcError_errorFormat2 (mcError_error e, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);
EXTERN void mcError_errorFormat3 (mcError_error e, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);
EXTERN void mcError_errorString (mcError_error e, DynamicStrings_String str);
EXTERN void mcError_errorStringAt (DynamicStrings_String s, unsigned int tok);
EXTERN void mcError_errorStringAt2 (DynamicStrings_String s, unsigned int tok1, unsigned int tok2);
EXTERN void mcError_errorStringsAt2 (DynamicStrings_String s1, DynamicStrings_String s2, unsigned int tok1, unsigned int tok2);
EXTERN void mcError_warnStringAt (DynamicStrings_String s, unsigned int tok);
EXTERN void mcError_warnStringAt2 (DynamicStrings_String s, unsigned int tok1, unsigned int tok2);
EXTERN void mcError_warnStringsAt2 (DynamicStrings_String s1, DynamicStrings_String s2, unsigned int tok1, unsigned int tok2);

/*
   warnFormat0 - displays the source module and line together
                 with the encapsulated format string.
                 Used for simple warning messages tied to the current token.
*/

EXTERN void mcError_warnFormat0 (char *a_, unsigned int _a_high);

/*
   warnFormat1 - displays the source module and line together
                 with the encapsulated format string.
                 Used for simple warning messages tied to the current token.
*/

EXTERN void mcError_warnFormat1 (char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);

/*
   flushErrors - switches the output channel to the error channel
                 and then writes out all errors.
                 If an error is present the compilation is terminated.
                 All warnings are ignored.
*/

EXTERN void mcError_flushErrors (void);

/*
   flushWarnings - switches the output channel to the error channel
                   and then writes out all warnings.
                   If an error is present the compilation is terminated,
                   if warnings only were emitted then compilation will
                   continue.
*/

EXTERN void mcError_flushWarnings (void);

/*
   errorAbort0 - aborts compiling, it flushes all warnings and errors before aborting.
*/

EXTERN void mcError_errorAbort0 (char *a_, unsigned int _a_high);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
