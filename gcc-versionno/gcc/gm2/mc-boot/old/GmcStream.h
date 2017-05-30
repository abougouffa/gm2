/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcStream.def.  */


#if !defined (_mcStream_H)
#   define _mcStream_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GFIO.h"

#   if defined (_mcStream_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN FIO_File mcStream_openFrag (unsigned int id);
EXTERN void mcStream_setDest (FIO_File f);
EXTERN FIO_File mcStream_combine (void);

#   undef EXTERN
#endif
