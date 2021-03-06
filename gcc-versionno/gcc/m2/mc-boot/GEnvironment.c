/* do not edit automatically generated by mc from Environment.  */
/* Environment.mod provides access to the environment settings of a process.

Copyright (C) 2001-2020 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

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
    {
      a[i] = ASCII_nul;
    }
  return Addr != NULL;
  /* static analysis guarentees a RETURN statement will be used before here.  */
  __builtin_unreachable ();
}

void _M2_Environment_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_Environment_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
