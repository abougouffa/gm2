/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/dtoa.def.  */


#if !defined (_dtoa_H)
#   define _dtoa_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_dtoa_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

typedef enum {dtoa_maxsignificant, dtoa_decimaldigits} dtoa_Mode;

EXTERN double dtoa_strtod (void * s, unsigned int *error);
EXTERN void * dtoa_dtoa (double d, dtoa_Mode mode, int ndigits, int *decpt, unsigned int *sign);

#   undef EXTERN
#endif
