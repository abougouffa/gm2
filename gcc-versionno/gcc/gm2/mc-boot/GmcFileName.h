/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcFileName.def.  */


#if !defined (_mcFileName_H)
#   define _mcFileName_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GDynamicStrings.h"

#   if defined (_mcFileName_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN DynamicStrings_String mcFileName_calculateFileName (DynamicStrings_String module, DynamicStrings_String extension);
EXTERN DynamicStrings_String mcFileName_calculateStemName (DynamicStrings_String module);
EXTERN DynamicStrings_String mcFileName_extractExtension (DynamicStrings_String filename, DynamicStrings_String ext);
EXTERN DynamicStrings_String mcFileName_extractModule (DynamicStrings_String filename);

#   undef EXTERN
#endif
