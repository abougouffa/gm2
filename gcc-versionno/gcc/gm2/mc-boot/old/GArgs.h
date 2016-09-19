/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/Args.def.  */


#if !defined (_Args_H)
#   define _Args_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_Args_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN unsigned int Args_GetArg (char *a, unsigned int _a_high, unsigned int i);
EXTERN unsigned int Args_Narg (void);

#   undef EXTERN
#endif
