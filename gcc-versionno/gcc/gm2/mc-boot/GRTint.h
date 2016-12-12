/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/RTint.def.  */


#if !defined (_RTint_H)
#   define _RTint_H

#ifdef __cplusplus
extern "C" {
#endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_RTint_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

typedef struct RTint_DespatchVector_p RTint_DespatchVector;

typedef void (*RTint_DespatchVector_t) (unsigned int, unsigned int, void *);
struct RTint_DespatchVector_p { RTint_DespatchVector_t proc; };

EXTERN unsigned int RTint_InitInputVector (int fd, unsigned int pri);
EXTERN unsigned int RTint_InitOutputVector (int fd, unsigned int pri);
EXTERN unsigned int RTint_InitTimeVector (unsigned int micro, unsigned int secs, unsigned int pri);
EXTERN void RTint_ReArmTimeVector (unsigned int vec, unsigned int micro, unsigned int secs);
EXTERN void RTint_GetTimeVector (unsigned int vec, unsigned int *micro, unsigned int *secs);
EXTERN void * RTint_AttachVector (unsigned int vec, void * p);
EXTERN void RTint_IncludeVector (unsigned int vec);
EXTERN void RTint_ExcludeVector (unsigned int vec);
EXTERN void RTint_Listen (unsigned int untilInterrupt, RTint_DespatchVector call, unsigned int pri);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
