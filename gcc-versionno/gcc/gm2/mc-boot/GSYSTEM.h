/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/SYSTEM.def.  */


#if !defined (_SYSTEM_H)
#   define _SYSTEM_H

#ifdef __cplusplus
extern "C" {
#endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_SYSTEM_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

#   define SYSTEM_BITSPERBYTE 8
#   define SYSTEM_BYTESPERWORD 4
EXTERN void SYSTEM_ShiftVal (unsigned int *s, unsigned int _s_high, unsigned int *d, unsigned int _d_high, unsigned int SetSizeInBits, int ShiftCount);
EXTERN void SYSTEM_ShiftLeft (unsigned int *s, unsigned int _s_high, unsigned int *d, unsigned int _d_high, unsigned int SetSizeInBits, unsigned int ShiftCount);
EXTERN void SYSTEM_ShiftRight (unsigned int *s, unsigned int _s_high, unsigned int *d, unsigned int _d_high, unsigned int SetSizeInBits, unsigned int ShiftCount);
EXTERN void SYSTEM_RotateVal (unsigned int *s, unsigned int _s_high, unsigned int *d, unsigned int _d_high, unsigned int SetSizeInBits, int RotateCount);
EXTERN void SYSTEM_RotateLeft (unsigned int *s, unsigned int _s_high, unsigned int *d, unsigned int _d_high, unsigned int SetSizeInBits, unsigned int RotateCount);
EXTERN void SYSTEM_RotateRight (unsigned int *s, unsigned int _s_high, unsigned int *d, unsigned int _d_high, unsigned int SetSizeInBits, unsigned int RotateCount);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
