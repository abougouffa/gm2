/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcError.def.  */


#if !defined (_mcError_H)
#   define _mcError_H

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
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

#if !defined (mcError_error_D)
#  define mcError_error_D
   typedef void *mcError_error;
#endif

EXTERN void mcError_internalError (char *a_, unsigned int _a_high, char *file_, unsigned int _file_high, unsigned int line);
EXTERN void mcError_writeFormat0 (char *a_, unsigned int _a_high);
EXTERN void mcError_writeFormat1 (char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);
EXTERN void mcError_writeFormat2 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);
EXTERN void mcError_writeFormat3 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);
EXTERN mcError_error mcError_newError (unsigned int atTokenNo);
EXTERN mcError_error mcError_newWarning (unsigned int atTokenNo);
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
EXTERN void mcError_warnFormat0 (char *a_, unsigned int _a_high);
EXTERN void mcError_warnFormat1 (char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);
EXTERN void mcError_flushErrors (void);
EXTERN void mcError_flushWarnings (void);
EXTERN void mcError_errorAbort0 (char *a_, unsigned int _a_high);

#   undef EXTERN
#endif
