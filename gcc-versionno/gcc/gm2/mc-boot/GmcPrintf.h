/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcPrintf.def.  */


#if !defined (_mcPrintf_H)
#   define _mcPrintf_H

#ifdef __cplusplus
extern "C" {
#endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"
#   include "GFIO.h"

#   if defined (_mcPrintf_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

EXTERN void mcPrintf_printf0 (char *a_, unsigned int _a_high);
EXTERN void mcPrintf_printf1 (char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);
EXTERN void mcPrintf_printf2 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);
EXTERN void mcPrintf_printf3 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);
EXTERN void mcPrintf_printf4 (char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high, unsigned char *w4_, unsigned int _w4_high);
EXTERN void mcPrintf_fprintf0 (FIO_File file, char *a_, unsigned int _a_high);
EXTERN void mcPrintf_fprintf1 (FIO_File file, char *a_, unsigned int _a_high, unsigned char *w_, unsigned int _w_high);
EXTERN void mcPrintf_fprintf2 (FIO_File file, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);
EXTERN void mcPrintf_fprintf3 (FIO_File file, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);
EXTERN void mcPrintf_fprintf4 (FIO_File file, char *a_, unsigned int _a_high, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high, unsigned char *w4_, unsigned int _w4_high);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
