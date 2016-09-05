/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/TimeString.def.  */


#if !defined (_TimeString_H)
#   define _TimeString_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_TimeString_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN void TimeString_GetTimeString (char *a, unsigned int _a_high);

#   undef EXTERN
#endif
