/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcPreprocess.def.  */


#if !defined (_mcPreprocess_H)
#   define _mcPreprocess_H

#ifdef __cplusplus
extern "C" {
#endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GDynamicStrings.h"

#   if defined (_mcPreprocess_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

EXTERN DynamicStrings_String mcPreprocess_preprocessModule (DynamicStrings_String filename);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
