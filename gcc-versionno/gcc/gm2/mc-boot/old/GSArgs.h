/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/SArgs.def.  */


#if !defined (_SArgs_H)
#   define _SArgs_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GDynamicStrings.h"

#   if defined (_SArgs_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN unsigned int SArgs_GetArg (DynamicStrings_String *s, unsigned int i);
EXTERN unsigned int SArgs_Narg (void);

#   undef EXTERN
#endif
