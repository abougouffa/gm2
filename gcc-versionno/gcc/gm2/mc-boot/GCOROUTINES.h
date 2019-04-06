

#if !defined (_COROUTINES_H)
#   define _COROUTINES_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_COROUTINES_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

#   define COROUTINES_UnassignedPriority 0
typedef unsigned int COROUTINES_INTERRUPTSOURCE;

typedef unsigned int COROUTINES_PROTECTION;

#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
