/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcComp.def.  */


#if !defined (_mcComp_H)
#   define _mcComp_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GDynamicStrings.h"

#   if defined (_mcComp_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN void mcComp_compile (DynamicStrings_String s);

#   undef EXTERN
#endif
