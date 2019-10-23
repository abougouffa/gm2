

#if !defined (_RTco_H)
#   define _RTco_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_RTco_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
 init initializes the module and allows the application to lazily invoke threads.  */

EXTERN int RTco_init (void);

/*
 init initializes the module and allows the application to lazily invoke threads.  */

EXTERN int RTco_initThread (PROC p, unsigned int stackSize, unsigned int interruptLevel);

/*
 init initializes the module and allows the application to lazily invoke threads.  */

EXTERN int RTco_initSemaphore (unsigned int value);

/*
 init initializes the module and allows the application to lazily invoke threads.  */

EXTERN void RTco_wait (int semaphore);

/*
 init initializes the module and allows the application to lazily invoke threads.  */

EXTERN void RTco_signal (int semaphore);

/*
 init initializes the module and allows the application to lazily invoke threads.  */

EXTERN void RTco_transfer (int *p1, int p2);

/*
 init initializes the module and allows the application to lazily invoke threads.  */

EXTERN void RTco_waitThread (int tid);

/*
 init initializes the module and allows the application to lazily invoke threads.  */

EXTERN void RTco_signalThread (int tid);

/*
 init initializes the module and allows the application to lazily invoke threads.  */

EXTERN int RTco_currentThread (void);

/*
 currentInterruptLevel returns the interrupt level of the current thread.  */

EXTERN unsigned int RTco_currentInterruptLevel (void);
EXTERN unsigned int RTco_turnInterrupts (unsigned int newLevel);

/*
   select access to the select system call which will be thread safe.
   This is typically called from the idle process to wait for an interrupt.
*/

EXTERN int RTco_select (int p1, void * p2, void * p3, void * p4, void * p5);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
