/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/FormatStrings.def.  */


#if !defined (_FormatStrings_H)
#   define _FormatStrings_H

#ifdef __cplusplus
extern "C" {
#endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"
#   include "GDynamicStrings.h"

#   if defined (_FormatStrings_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

EXTERN DynamicStrings_String FormatStrings_Sprintf0 (DynamicStrings_String s);
EXTERN DynamicStrings_String FormatStrings_Sprintf1 (DynamicStrings_String s, unsigned char *w_, unsigned int _w_high);
EXTERN DynamicStrings_String FormatStrings_Sprintf2 (DynamicStrings_String s, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high);
EXTERN DynamicStrings_String FormatStrings_Sprintf3 (DynamicStrings_String s, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high);
EXTERN DynamicStrings_String FormatStrings_Sprintf4 (DynamicStrings_String s, unsigned char *w1_, unsigned int _w1_high, unsigned char *w2_, unsigned int _w2_high, unsigned char *w3_, unsigned int _w3_high, unsigned char *w4_, unsigned int _w4_high);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
