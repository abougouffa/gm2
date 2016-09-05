/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/StrIO.def.  */


#if !defined (_StrIO_H)
#   define _StrIO_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_StrIO_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN void StrIO_WriteLn (void);
EXTERN void StrIO_ReadString (char *a, unsigned int _a_high);
EXTERN void StrIO_WriteString (char *a_, unsigned int _a_high);

#   undef EXTERN
#endif
