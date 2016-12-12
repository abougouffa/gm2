/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcSearch.def.  */


#if !defined (_mcSearch_H)
#   define _mcSearch_H

#ifdef __cplusplus
extern "C" {
#endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GDynamicStrings.h"

#   if defined (_mcSearch_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

EXTERN void mcSearch_initSearchPath (DynamicStrings_String path);
EXTERN void mcSearch_prependSearchPath (DynamicStrings_String path);
EXTERN unsigned int mcSearch_findSourceFile (DynamicStrings_String FileName, DynamicStrings_String *fullPath);
EXTERN unsigned int mcSearch_findSourceDefFile (DynamicStrings_String stem, DynamicStrings_String *fullPath);
EXTERN unsigned int mcSearch_findSourceModFile (DynamicStrings_String stem, DynamicStrings_String *fullPath);
EXTERN void mcSearch_setDefExtension (DynamicStrings_String ext);
EXTERN void mcSearch_setModExtension (DynamicStrings_String ext);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
