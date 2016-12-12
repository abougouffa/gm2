/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/StrCase.def.  */


#if !defined (_StrCase_H)
#   define _StrCase_H

#ifdef __cplusplus
extern "C" {
#endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_StrCase_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

EXTERN void StrCase_StrToUpperCase (char *a_, unsigned int _a_high, char *b, unsigned int _b_high);
EXTERN void StrCase_StrToLowerCase (char *a_, unsigned int _a_high, char *b, unsigned int _b_high);
EXTERN char StrCase_Cap (char ch);
EXTERN char StrCase_Lower (char ch);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
