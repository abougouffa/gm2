/* automatically created by mc from ../../gcc-versionno/gcc/gm2/mc/mcStack.def.  */


#if !defined (_mcStack_H)
#   define _mcStack_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_mcStack_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

#if !defined (mcStack_stack_D)
#  define mcStack_stack_D
   typedef void *mcStack_stack;
#endif


/*
   init - create and return a stack.
*/

EXTERN mcStack_stack mcStack_init (void);

/*
   kill - deletes stack, s.
*/

EXTERN void mcStack_kill (mcStack_stack *s);

/*
   push - an address, a, onto the stack, s.
          It returns, a.
*/

EXTERN void * mcStack_push (mcStack_stack s, void * a);

/*
   pop - and return the top element from stack, s.
*/

EXTERN void * mcStack_pop (mcStack_stack s);

/*
   replace - performs a pop; push (a); return a.
*/

EXTERN void * mcStack_replace (mcStack_stack s, void * a);

/*
   depth - returns the depth of the stack.
*/

EXTERN unsigned int mcStack_depth (mcStack_stack s);

/*
   access - returns the, i, th stack element.
            The top of stack is defined by:

            access (s, depth (s)).
*/

EXTERN void * mcStack_access (mcStack_stack s, unsigned int i);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
