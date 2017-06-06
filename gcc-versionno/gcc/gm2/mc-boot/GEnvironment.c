/* automatically created by mc from ../../gcc-versionno/gcc/gm2/gm2-libs/Environment.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#include <stddef.h>
#include <string.h>
#include <limits.h>
#define _Environment_H
#define _Environment_C

#   include "GSYSTEM.h"
#   include "Glibc.h"
#   include "GASCII.h"
#   include "GStrLib.h"


/*
   GetEnvironment - gets the environment variable, Env, and places
      	       	    a copy of its value into string, a.
*/

unsigned int Environment_GetEnvironment (char *Env_, unsigned int _Env_high, char *a, unsigned int _a_high);


/*
   GetEnvironment - gets the environment variable, Env, and places
      	       	    a copy of its value into string, a.
*/

unsigned int Environment_GetEnvironment (char *Env_, unsigned int _Env_high, char *a, unsigned int _a_high)
{
  unsigned int High;
  unsigned int i;
  char * Addr;
  char Env[_Env_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (Env, Env_, _Env_high+1);

  i = 0;
  High = _a_high;
  Addr = libc_getenv (&Env);
  while (((i < High) && (Addr != NULL)) && ((*Addr) != ASCII_nul))
    {
      a[i] = (*Addr);
      Addr += 1;
      i += 1;
    }
  if (i < High)
    a[i] = ASCII_nul;
  return Addr != NULL;
}

void _M2_Environment_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_Environment_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
