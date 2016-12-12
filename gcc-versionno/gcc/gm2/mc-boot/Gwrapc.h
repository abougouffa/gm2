/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/wrapc.def.  */


#if !defined (_wrapc_H)
#   define _wrapc_H

#ifdef __cplusplus
extern "C" {
#endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_wrapc_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

EXTERN void * wrapc_strtime (void);
EXTERN int wrapc_filesize (int f, unsigned int *low, unsigned int *high);
EXTERN int wrapc_filemtime (int f);
EXTERN int wrapc_getrand (int n);
EXTERN void * wrapc_getusername (void);
EXTERN void wrapc_getnameuidgid (void * name, int *uid, int *gid);
EXTERN int wrapc_signbit (double r);
EXTERN int wrapc_signbitf (float s);
EXTERN int wrapc_signbitl (long double l);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
