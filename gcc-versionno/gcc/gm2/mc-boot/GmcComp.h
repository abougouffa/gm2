/* automatically created by mc from ../../gcc-versionno/gcc/gm2/mc/mcComp.def.  */


#if !defined (_mcComp_H)
#   define _mcComp_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GDynamicStrings.h"

#   if defined (_mcComp_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   compile - translate file, s, using a 6 pass technique.
*/

EXTERN void mcComp_compile (DynamicStrings_String s);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
