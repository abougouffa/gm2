/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/SysStorage.def.  */


#if !defined (_SysStorage_H)
#   define _SysStorage_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_SysStorage_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN void SysStorage_ALLOCATE (void * *a, unsigned int Size);
EXTERN void SysStorage_DEALLOCATE (void * *a, unsigned int Size);
EXTERN void SysStorage_REALLOCATE (void * *a, unsigned int Size);
EXTERN unsigned int SysStorage_Available (unsigned int Size);
EXTERN void SysStorage_Init (void);

#   undef EXTERN
#endif
