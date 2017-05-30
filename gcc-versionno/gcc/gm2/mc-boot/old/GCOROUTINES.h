/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/COROUTINES.def.  */


#if !defined (_COROUTINES_H)
#   define _COROUTINES_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_COROUTINES_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

#   define COROUTINES_UnassignedPriority 0
typedef unsigned int COROUTINES_INTERRUPTSOURCE;

typedef unsigned int COROUTINES_PROTECTION;


#   undef EXTERN
#endif
