/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcDebug.def.  */


#if !defined (_mcDebug_H)
#   define _mcDebug_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_mcDebug_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN void mcDebug_assert (unsigned int q);
EXTERN void mcDebug_writeDebug (char *a_, unsigned int _a_high);

#   undef EXTERN
#endif