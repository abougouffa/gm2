/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcPreprocess.def.  */


#if !defined (_mcPreprocess_H)
#   define _mcPreprocess_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GDynamicStrings.h"

#   if defined (_mcPreprocess_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN DynamicStrings_String mcPreprocess_preprocessModule (DynamicStrings_String filename);

#   undef EXTERN
#endif
