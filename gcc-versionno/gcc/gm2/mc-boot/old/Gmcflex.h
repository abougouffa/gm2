/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcflex.def.  */


#if !defined (_mcflex_H)
#   define _mcflex_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_mcflex_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN void mcflex_closeSource (void);
EXTERN unsigned int mcflex_openSource (void * s);
EXTERN void * mcflex_getToken (void);
EXTERN unsigned int mcflex_getLineNo (void);
EXTERN unsigned int mcflex_getColumnNo (void);
EXTERN void mcflex_mcError (void * s);
EXTERN unsigned int mcflex_getTotalLines (void);

#   undef EXTERN
#endif
