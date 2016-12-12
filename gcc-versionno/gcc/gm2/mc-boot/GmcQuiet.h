/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcQuiet.def.  */


#if !defined (_mcQuiet_H)
#   define _mcQuiet_H

#ifdef __cplusplus
extern "C" {
#endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_mcQuiet_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

EXTERN void mcQuiet_qprintf0 (char *a_, unsigned int _a_high);
EXTERN void mcQuiet_qprintf1 (char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);
EXTERN void mcQuiet_qprintf2 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);
EXTERN void mcQuiet_qprintf3 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);
EXTERN void mcQuiet_qprintf4 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high, unsigned char *w4_, unsigned int _w4_high);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
