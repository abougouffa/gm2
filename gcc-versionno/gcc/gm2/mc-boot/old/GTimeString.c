/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/TimeString.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#include <stddef.h>
#define _TimeString_H
#define _TimeString_C

#   include "Gwrapc.h"
#   include "GASCII.h"
#   include "GSYSTEM.h"

void TimeString_GetTimeString (char *a, unsigned int _a_high);

void TimeString_GetTimeString (char *a, unsigned int _a_high)
{
  unsigned int i;
  char * Addr;

  Addr = wrapc_strtime ();
  i = 0;
  if (Addr != NULL)
    while ((i < (_a_high)) && ((*Addr) != ASCII_nul))
      {
        a[i] = (*Addr);
        i += 1;
        Addr += 1;
      }
  if (i < (_a_high))
    a[i] = ASCII_nul;
}

void _M2_TimeString_init (int argc, char *argv[])
{
}
