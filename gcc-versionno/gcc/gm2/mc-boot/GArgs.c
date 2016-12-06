/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/Args.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#define _Args_H
#define _Args_C

#   include "GUnixArgs.h"
#   include "GASCII.h"

#   define MaxArgs 255
#   define MaxString 4096
typedef struct _T1_a _T1;

typedef struct _T2_a _T2;

struct _T1_a { _T2 * array[MaxArgs+1]; };
struct _T2_a { char array[MaxString+1]; };
static _T1 * Source;
unsigned int Args_GetArg (char *a, unsigned int _a_high, unsigned int i);
unsigned int Args_Narg (void);

unsigned int Args_GetArg (char *a, unsigned int _a_high, unsigned int i)
{
  unsigned int High;
  unsigned int j;

  j = 0;
  High = _a_high;
  if (i < UnixArgs_ArgC)
    {
      Source = UnixArgs_ArgV;
      while (((*(*Source).array[i]).array[j] != ASCII_nul) && (j < High))
        {
          a[j] = (*(*Source).array[i]).array[j];
          j += 1;
        }
    }
  if (j <= High)
    a[j] = ASCII_nul;
  return i < UnixArgs_ArgC;
}

unsigned int Args_Narg (void)
{
  return UnixArgs_ArgC;
}

void _M2_Args_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_Args_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
