/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/Assertion.def.  */


#if !defined (_Assertion_H)
#   define _Assertion_H

#ifdef __cplusplus
extern "C" {
#endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_Assertion_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

EXTERN void Assertion_Assert (unsigned int Condition);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
