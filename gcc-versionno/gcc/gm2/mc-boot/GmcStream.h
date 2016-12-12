/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcStream.def.  */


#if !defined (_mcStream_H)
#   define _mcStream_H

#ifdef __cplusplus
extern "C" {
#endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GFIO.h"

#   if defined (_mcStream_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

EXTERN FIO_File mcStream_openFrag (unsigned int id);
EXTERN void mcStream_setDest (FIO_File f);
EXTERN FIO_File mcStream_combine (void);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
