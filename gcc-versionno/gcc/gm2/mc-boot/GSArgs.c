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


/*
   GetArg - returns the nth argument from the command line.
            The success of the operation is returned.
            If TRUE is returned then the string, s, contains a
            new string, otherwise s is set to NIL.
*/

unsigned int SArgs_GetArg (DynamicStrings_String *s, unsigned int i);

/*
   Narg - returns the number of arguments available from
          command line.
*/

unsigned int SArgs_Narg (void);


/*
   GetArg - returns the nth argument from the command line.
            The success of the operation is returned.
            If TRUE is returned then the string, s, contains a
            new string, otherwise s is set to NIL.
*/

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


/*
   Narg - returns the number of arguments available from
          command line.
*/

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
