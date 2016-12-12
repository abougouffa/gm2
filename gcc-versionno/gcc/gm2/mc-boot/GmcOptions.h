/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcOptions.def.  */


#if !defined (_mcOptions_H)
#   define _mcOptions_H

#ifdef __cplusplus
extern "C" {
#endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GDynamicStrings.h"

#   if defined (_mcOptions_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

EXTERN DynamicStrings_String mcOptions_handleOptions (void);
EXTERN unsigned int mcOptions_getQuiet (void);
EXTERN unsigned int mcOptions_getVerbose (void);
EXTERN unsigned int mcOptions_getInternalDebugging (void);
EXTERN DynamicStrings_String mcOptions_getCppCommandLine (void);
EXTERN DynamicStrings_String mcOptions_getOutputFile (void);
EXTERN unsigned int mcOptions_getExtendedOpaque (void);
EXTERN void mcOptions_setDebugTopological (unsigned int value);
EXTERN unsigned int mcOptions_getDebugTopological (void);
EXTERN DynamicStrings_String mcOptions_getHPrefix (void);
EXTERN unsigned int mcOptions_getIgnoreFQ (void);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
