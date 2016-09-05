/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/ldtoa.def.  */


#if !defined (_ldtoa_H)
#   define _ldtoa_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_ldtoa_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

typedef enum {ldtoa_maxsignificant, ldtoa_decimaldigits} ldtoa_Mode;

EXTERN long double ldtoa_strtold (void * s, unsigned int *error);
EXTERN void * ldtoa_ldtoa (long double d, ldtoa_Mode mode, int ndigits, int *decpt, unsigned int *sign);

#   undef EXTERN
#endif
