/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/Environment.def.  */


#if !defined (_Environment_H)
#   define _Environment_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_Environment_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN unsigned int Environment_GetEnvironment (char *Env_, unsigned int _Env_high, char *a, unsigned int _a_high);

#   undef EXTERN
#endif