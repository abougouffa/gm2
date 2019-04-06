/* Args.mod provide access to command line arguments.

Copyright (C) 2001-2018 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
MA  02110-1301  USA.  */

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

/*
   GetArg - returns the nth argument from the command line.
            The success of the operation is returned.
*/

unsigned int Args_GetArg (char *a, unsigned int _a_high, unsigned int i);

/*
   Narg - returns the number of arguments available from
          command line.
*/

unsigned int Args_Narg (void);


/*
   GetArg - returns the nth argument from the command line.
            The success of the operation is returned.
*/

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


/*
   Narg - returns the number of arguments available from
          command line.
*/

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
