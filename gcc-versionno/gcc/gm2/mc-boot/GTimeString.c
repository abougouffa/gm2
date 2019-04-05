/* Copyright (C) 2019 Free Software Foundation, Inc.

This file is part of GNU Modula-2.

GNU Modula-2 is software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING.  If not,
see <https://www.gnu.org/licenses/>.  */

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


/*
   GetTimeString - places the time in ascii format into array, a.

*/

void TimeString_GetTimeString (char *a, unsigned int _a_high);


/*
   GetTimeString - places the time in ascii format into array, a.

*/

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

void _M2_TimeString_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_TimeString_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
