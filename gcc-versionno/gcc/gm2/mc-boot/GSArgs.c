/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/SArgs.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   if !defined (TRUE)
#      define TRUE (1==1)
#   endif

#   if !defined (FALSE)
#      define FALSE (1==0)
#   endif

#include <stddef.h>
#define _SArgs_H
#define _SArgs_C

#   include "GSYSTEM.h"
#   include "GUnixArgs.h"
#   include "GDynamicStrings.h"

typedef char *PtrToChar;

typedef PtrToChar *PtrToPtrToChar;

unsigned int SArgs_GetArg (DynamicStrings_String *s, unsigned int i);
unsigned int SArgs_Narg (void);

unsigned int SArgs_GetArg (DynamicStrings_String *s, unsigned int i)
{
  PtrToPtrToChar ppc;

  if (i < UnixArgs_ArgC)
    {
      ppc = (void *) (UnixArgs_ArgV+(i*(sizeof (PtrToChar))));
      (*s) = DynamicStrings_InitStringCharStar ((void *) (*ppc));
      return TRUE;
    }
  else
    {
      (*s) = NULL;
      return FALSE;
    }
}

unsigned int SArgs_Narg (void)
{
  return UnixArgs_ArgC;
}

void _M2_SArgs_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_SArgs_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
