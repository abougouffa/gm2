/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/RTExceptions.def.  */


#if !defined (_RTExceptions_H)
#   define _RTExceptions_H

#ifdef __cplusplus
extern "C" {
#endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_RTExceptions_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

#if !defined (RTExceptions_EHBlock_D)
#  define RTExceptions_EHBlock_D
   typedef void *RTExceptions_EHBlock;
#endif

typedef struct RTExceptions_ProcedureHandler_p RTExceptions_ProcedureHandler;

typedef void (*RTExceptions_ProcedureHandler_t) (void);
struct RTExceptions_ProcedureHandler_p { RTExceptions_ProcedureHandler_t proc; };

EXTERN void RTExceptions_Raise (unsigned int number, void * file, unsigned int line, unsigned int column, void * function, void * message);
EXTERN void RTExceptions_SetExceptionBlock (RTExceptions_EHBlock source);
EXTERN RTExceptions_EHBlock RTExceptions_GetExceptionBlock (void);
EXTERN void * RTExceptions_GetTextBuffer (RTExceptions_EHBlock e);
EXTERN unsigned int RTExceptions_GetTextBufferSize (RTExceptions_EHBlock e);
EXTERN unsigned int RTExceptions_GetNumber (RTExceptions_EHBlock source);
EXTERN RTExceptions_EHBlock RTExceptions_InitExceptionBlock (void);
EXTERN RTExceptions_EHBlock RTExceptions_KillExceptionBlock (RTExceptions_EHBlock e);
EXTERN void RTExceptions_PushHandler (RTExceptions_EHBlock e, unsigned int number, RTExceptions_ProcedureHandler p);
EXTERN void RTExceptions_PopHandler (RTExceptions_EHBlock e, unsigned int number);
EXTERN void RTExceptions_DefaultErrorCatch (void);
EXTERN void RTExceptions_BaseExceptionsThrow (void);
EXTERN unsigned int RTExceptions_IsInExceptionState (void);
EXTERN unsigned int RTExceptions_SetExceptionState (unsigned int to);
EXTERN void RTExceptions_SwitchExceptionState (unsigned int *from, unsigned int to);
EXTERN RTExceptions_EHBlock RTExceptions_GetBaseExceptionBlock (void);
EXTERN void RTExceptions_SetExceptionSource (void * source);
EXTERN void * RTExceptions_GetExceptionSource (void);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
