/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/PushBackInput.def.  */


#if !defined (_PushBackInput_H)
#   define _PushBackInput_H

#ifdef __cplusplus
extern "C" {
#endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GFIO.h"
#   include "GDynamicStrings.h"

#   if defined (_PushBackInput_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

EXTERN FIO_File PushBackInput_Open (char *a_, unsigned int _a_high);
EXTERN char PushBackInput_GetCh (FIO_File f);
EXTERN char PushBackInput_PutCh (FIO_File f, char ch);
EXTERN void PushBackInput_PutString (FIO_File f, char *a_, unsigned int _a_high);
EXTERN void PushBackInput_Error (char *a_, unsigned int _a_high);
EXTERN void PushBackInput_WarnError (char *a_, unsigned int _a_high);
EXTERN void PushBackInput_WarnString (DynamicStrings_String s);
EXTERN void PushBackInput_Close (FIO_File f);
EXTERN unsigned int PushBackInput_GetExitStatus (void);
EXTERN void PushBackInput_SetDebug (unsigned int d);
EXTERN unsigned int PushBackInput_GetColumnPosition (void);
EXTERN unsigned int PushBackInput_GetCurrentLine (void);
#ifdef __cplusplus
}
#endif

#   undef EXTERN
#endif
