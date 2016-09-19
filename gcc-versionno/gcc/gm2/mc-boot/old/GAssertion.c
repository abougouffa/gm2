/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/Assertion.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#define _Assertion_H
#define _Assertion_C

#   include "GStrIO.h"

void Assertion_Assert (unsigned int Condition);

void Assertion_Assert (unsigned int Condition)
{
  if (Condition)
    {
      StrIO_WriteString ((char *) "assert failed - halting system", 30);
      StrIO_WriteLn ();
      M2RTS_HALT (0);
    }
}

void _M2_Assertion_init (int argc, char *argv[])
{
}
