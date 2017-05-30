/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/Storage.def.  */


#if !defined (_Storage_H)
#   define _Storage_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_Storage_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN void Storage_ALLOCATE (void * *a, unsigned int Size);
EXTERN void Storage_DEALLOCATE (void * *a, unsigned int Size);
EXTERN void Storage_REALLOCATE (void * *a, unsigned int Size);
EXTERN unsigned int Storage_Available (unsigned int Size);

#   undef EXTERN
#endif
