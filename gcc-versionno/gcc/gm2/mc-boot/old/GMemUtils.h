/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/MemUtils.def.  */


#if !defined (_MemUtils_H)
#   define _MemUtils_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_MemUtils_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN void MemUtils_MemCopy (void * from, unsigned int length, void * to);
EXTERN void MemUtils_MemZero (void * a, unsigned int length);

#   undef EXTERN
#endif
