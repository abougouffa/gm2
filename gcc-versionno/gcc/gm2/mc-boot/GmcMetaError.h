/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcMetaError.def.  */


#if !defined (_mcMetaError_H)
#   define _mcMetaError_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"
#   include "GDynamicStrings.h"

#   if defined (_mcMetaError_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

EXTERN void mcMetaError_metaError1 (char *m_, unsigned int _m_high, unsigned char *s_, unsigned int _s_high);
EXTERN void mcMetaError_metaError2 (char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high);
EXTERN void mcMetaError_metaError3 (char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high);
EXTERN void mcMetaError_metaError4 (char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high);
EXTERN void mcMetaError_metaErrors1 (char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s_, unsigned int _s_high);
EXTERN void mcMetaError_metaErrors2 (char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high);
EXTERN void mcMetaError_metaErrors3 (char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high);
EXTERN void mcMetaError_metaErrors4 (char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high);
EXTERN void mcMetaError_metaErrorT1 (unsigned int tok, char *m_, unsigned int _m_high, unsigned char *s_, unsigned int _s_high);
EXTERN void mcMetaError_metaErrorT2 (unsigned int tok, char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high);
EXTERN void mcMetaError_metaErrorT3 (unsigned int tok, char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high);
EXTERN void mcMetaError_metaErrorT4 (unsigned int tok, char *m_, unsigned int _m_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high);
EXTERN void mcMetaError_metaErrorsT1 (unsigned int tok, char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s_, unsigned int _s_high);
EXTERN void mcMetaError_metaErrorsT2 (unsigned int tok, char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high);
EXTERN void mcMetaError_metaErrorsT3 (unsigned int tok, char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high);
EXTERN void mcMetaError_metaErrorsT4 (unsigned int tok, char *m1_, unsigned int _m1_high, char *m2_, unsigned int _m2_high, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high);
EXTERN void mcMetaError_metaErrorString1 (DynamicStrings_String m, unsigned char *s_, unsigned int _s_high);
EXTERN void mcMetaError_metaErrorString2 (DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high);
EXTERN void mcMetaError_metaErrorString3 (DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high);
EXTERN void mcMetaError_metaErrorString4 (DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high);
EXTERN void mcMetaError_metaErrorStringT1 (unsigned int tok, DynamicStrings_String m, unsigned char *s_, unsigned int _s_high);
EXTERN void mcMetaError_metaErrorStringT2 (unsigned int tok, DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high);
EXTERN void mcMetaError_metaErrorStringT3 (unsigned int tok, DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high);
EXTERN void mcMetaError_metaErrorStringT4 (unsigned int tok, DynamicStrings_String m, unsigned char *s1_, unsigned int _s1_high, unsigned char *s2_, unsigned int _s2_high, unsigned char *s3_, unsigned int _s3_high, unsigned char *s4_, unsigned int _s4_high);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
