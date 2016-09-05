/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/M2RTS.def.  */


#if !defined (_M2RTS_H)
#   define _M2RTS_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_M2RTS_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

EXTERN void M2RTS_ExecuteTerminationProcedures (void);
EXTERN unsigned int M2RTS_InstallTerminationProcedure (PROC p);
EXTERN void M2RTS_ExecuteInitialProcedures (void);
EXTERN unsigned int M2RTS_InstallInitialProcedure (PROC p);
EXTERN void M2RTS_Terminate (void);
EXTERN void M2RTS_HALT (int exitcode);
EXTERN void M2RTS_Halt (char *file_, unsigned int _file_high, unsigned int line, char *function_, unsigned int _function_high, char *description_, unsigned int _description_high);
EXTERN void M2RTS_ExitOnHalt (int e);
EXTERN void M2RTS_ErrorMessage (char *message_, unsigned int _message_high, char *file_, unsigned int _file_high, unsigned int line, char *function_, unsigned int _function_high);
EXTERN unsigned int M2RTS_Length (char *a_, unsigned int _a_high);
EXTERN void M2RTS_AssignmentException (void * filename, unsigned int line, unsigned int column, void * scope);
EXTERN void M2RTS_IncException (void * filename, unsigned int line, unsigned int column, void * scope);
EXTERN void M2RTS_DecException (void * filename, unsigned int line, unsigned int column, void * scope);
EXTERN void M2RTS_InclException (void * filename, unsigned int line, unsigned int column, void * scope);
EXTERN void M2RTS_ExclException (void * filename, unsigned int line, unsigned int column, void * scope);
EXTERN void M2RTS_ShiftException (void * filename, unsigned int line, unsigned int column, void * scope);
EXTERN void M2RTS_RotateException (void * filename, unsigned int line, unsigned int column, void * scope);
EXTERN void M2RTS_StaticArraySubscriptException (void * filename, unsigned int line, unsigned int column, void * scope);
EXTERN void M2RTS_DynamicArraySubscriptException (void * filename, unsigned int line, unsigned int column, void * scope);
EXTERN void M2RTS_ForLoopBeginException (void * filename, unsigned int line, unsigned int column, void * scope);
EXTERN void M2RTS_ForLoopToException (void * filename, unsigned int line, unsigned int column, void * scope);
EXTERN void M2RTS_ForLoopEndException (void * filename, unsigned int line, unsigned int column, void * scope);
EXTERN void M2RTS_PointerNilException (void * filename, unsigned int line, unsigned int column, void * scope);
EXTERN void M2RTS_NoReturnException (void * filename, unsigned int line, unsigned int column, void * scope);
EXTERN void M2RTS_CaseException (void * filename, unsigned int line, unsigned int column, void * scope);
EXTERN void M2RTS_WholeNonPosDivException (void * filename, unsigned int line, unsigned int column, void * scope);
EXTERN void M2RTS_WholeNonPosModException (void * filename, unsigned int line, unsigned int column, void * scope);
EXTERN void M2RTS_WholeZeroDivException (void * filename, unsigned int line, unsigned int column, void * scope);
EXTERN void M2RTS_WholeZeroRemException (void * filename, unsigned int line, unsigned int column, void * scope);
EXTERN void M2RTS_NoException (void * filename, unsigned int line, unsigned int column, void * scope);

#   undef EXTERN
#endif
