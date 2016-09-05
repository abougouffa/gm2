/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcStack.def.  */


#if !defined (_mcStack_H)
#   define _mcStack_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_mcStack_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

#if !defined (mcStack_stack_D)
#  define mcStack_stack_D
   typedef void *mcStack_stack;
#endif

EXTERN mcStack_stack mcStack_init (void);
EXTERN void mcStack_kill (mcStack_stack *s);
EXTERN void * mcStack_push (mcStack_stack s, void * a);
EXTERN void * mcStack_pop (mcStack_stack s);
EXTERN void * mcStack_replace (mcStack_stack s, void * a);
EXTERN unsigned int mcStack_depth (mcStack_stack s);
EXTERN void * mcStack_access (mcStack_stack s, unsigned int i);

#   undef EXTERN
#endif
