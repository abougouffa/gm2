/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/FpuIO.def.  */


#if !defined (_FpuIO_H)
#   define _FpuIO_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_FpuIO_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN void FpuIO_ReadReal (double *x);
EXTERN void FpuIO_WriteReal (double x, unsigned int TotalWidth, unsigned int FractionWidth);
EXTERN void FpuIO_StrToReal (char *a_, unsigned int _a_high, double *x);
EXTERN void FpuIO_RealToStr (double x, unsigned int TotalWidth, unsigned int FractionWidth, char *a, unsigned int _a_high);
EXTERN void FpuIO_ReadLongReal (long double *x);
EXTERN void FpuIO_WriteLongReal (long double x, unsigned int TotalWidth, unsigned int FractionWidth);
EXTERN void FpuIO_StrToLongReal (char *a_, unsigned int _a_high, long double *x);
EXTERN void FpuIO_LongRealToStr (long double x, unsigned int TotalWidth, unsigned int FractionWidth, char *a, unsigned int _a_high);
EXTERN void FpuIO_ReadLongInt (long int *x);
EXTERN void FpuIO_WriteLongInt (long int x, unsigned int n);
EXTERN void FpuIO_StrToLongInt (char *a_, unsigned int _a_high, long int *x);
EXTERN void FpuIO_LongIntToStr (long int x, unsigned int n, char *a, unsigned int _a_high);

#   undef EXTERN
#endif
