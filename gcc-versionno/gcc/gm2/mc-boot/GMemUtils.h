
#if !defined (_MemUtils_H)
#   define _MemUtils_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_MemUtils_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   MemCopy - copys a region of memory to the required destination.
*/

EXTERN void MemUtils_MemCopy (void * from, unsigned int length, void * to);

/*
   MemZero - sets a region of memory: a..a+length to zero.
*/

EXTERN void MemUtils_MemZero (void * a, unsigned int length);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
