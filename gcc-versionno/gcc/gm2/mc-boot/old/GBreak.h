/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/Break.def.  */


#if !defined (_Break_H)
#   define _Break_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_Break_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif


#   undef EXTERN
#endif
