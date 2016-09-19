/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/mc/mcDebug.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#include <string.h>
#include <limits.h>
#define _mcDebug_H
#define _mcDebug_C

#   include "GStrIO.h"
#   include "GmcOptions.h"
#   include "GmcError.h"

void mcDebug_assert (unsigned int q);
void mcDebug_writeDebug (char *a_, unsigned int _a_high);

void mcDebug_assert (unsigned int q)
{
  if (! q)
    mcError_internalError ((char *) "assert failed", 13, (char *) "../../gcc-5.2.0/gcc/gm2/mc/mcDebug.mod", 38, 36);
}

void mcDebug_writeDebug (char *a_, unsigned int _a_high)
{
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  if (mcOptions_getInternalDebugging ())
    {
      StrIO_WriteString ((char *) a, _a_high);
      StrIO_WriteLn ();
    }
}

void _M2_mcDebug_init (int argc, char *argv[])
{
}

void _M2_mcDebug_finish (int argc, char *argv[])
{
}
