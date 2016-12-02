/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/M2EXCEPTION.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#include <limits.h>
#include "Gmcrts.h"
#define _M2EXCEPTION_H
#define _M2EXCEPTION_C

#   include "GSYSTEM.h"
#   include "GRTExceptions.h"

typedef enum {M2EXCEPTION_indexException, M2EXCEPTION_rangeException, M2EXCEPTION_caseSelectException, M2EXCEPTION_invalidLocation, M2EXCEPTION_functionException, M2EXCEPTION_wholeValueException, M2EXCEPTION_wholeDivException, M2EXCEPTION_realValueException, M2EXCEPTION_realDivException, M2EXCEPTION_complexValueException, M2EXCEPTION_complexDivException, M2EXCEPTION_protException, M2EXCEPTION_sysException, M2EXCEPTION_coException, M2EXCEPTION_exException} M2EXCEPTION_M2Exceptions;

M2EXCEPTION_M2Exceptions M2EXCEPTION_M2Exception (void);
unsigned int M2EXCEPTION_IsM2Exception (void);

M2EXCEPTION_M2Exceptions M2EXCEPTION_M2Exception (void)
{
  RTExceptions_EHBlock e;
  unsigned int n;

  e = RTExceptions_GetExceptionBlock ();
  n = RTExceptions_GetNumber (e);
  if (n == (UINT_MAX))
    RTExceptions_Raise ((unsigned int) (M2EXCEPTION_exException), "../../gcc-5.2.0/gcc/gm2/gm2-libs/M2EXCEPTION.mod", 39, 6, "M2Exception", "current coroutine is not in the exceptional execution state");
  else
    return (M2EXCEPTION_M2Exceptions) (n);
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/M2EXCEPTION.def", 10, 0);
}

unsigned int M2EXCEPTION_IsM2Exception (void)
{
  RTExceptions_EHBlock e;

  e = RTExceptions_GetExceptionBlock ();
  return (RTExceptions_GetNumber (e)) != (UINT_MAX);
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/M2EXCEPTION.def", 10, 0);
}

void _M2_M2EXCEPTION_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
  RTExceptions_SetExceptionBlock (RTExceptions_InitExceptionBlock ());
}

void _M2_M2EXCEPTION_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
