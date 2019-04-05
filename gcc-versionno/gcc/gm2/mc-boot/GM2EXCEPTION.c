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

#include <limits.h>
#   include "Gmcrts.h"
#define _M2EXCEPTION_H
#define _M2EXCEPTION_C

#   include "GSYSTEM.h"
#   include "GRTExceptions.h"

typedef enum {M2EXCEPTION_indexException, M2EXCEPTION_rangeException, M2EXCEPTION_caseSelectException, M2EXCEPTION_invalidLocation, M2EXCEPTION_functionException, M2EXCEPTION_wholeValueException, M2EXCEPTION_wholeDivException, M2EXCEPTION_realValueException, M2EXCEPTION_realDivException, M2EXCEPTION_complexValueException, M2EXCEPTION_complexDivException, M2EXCEPTION_protException, M2EXCEPTION_sysException, M2EXCEPTION_coException, M2EXCEPTION_exException} M2EXCEPTION_M2Exceptions;

M2EXCEPTION_M2Exceptions M2EXCEPTION_M2Exception (void);
unsigned int M2EXCEPTION_IsM2Exception (void);

M2EXCEPTION_M2Exceptions M2EXCEPTION_M2Exception (void)
{
  RTExceptions_EHBlock e;
  unsigned int n;

  /* If the current coroutine is in the exceptional execution state because of the raising
     of a language exception, returns the corresponding enumeration value, and otherwise
     raises an exception.
  */
  e = RTExceptions_GetExceptionBlock ();
  n = RTExceptions_GetNumber (e);
  if (n == (UINT_MAX))
    RTExceptions_Raise ((unsigned int) (M2EXCEPTION_exException), "../../gcc-versionno/gcc/gm2/gm2-libs/M2EXCEPTION.mod", 39, 6, "M2Exception", "current coroutine is not in the exceptional execution state");
  else
    return (M2EXCEPTION_M2Exceptions) (n);
  ReturnException ("../../gcc-versionno/gcc/gm2/gm2-libs/M2EXCEPTION.def", 8, 1);
}

unsigned int M2EXCEPTION_IsM2Exception (void)
{
  RTExceptions_EHBlock e;

  /* If the current coroutine is in the exceptional execution state because of the raising
     of a language exception, returns TRUE, and otherwise returns FALSE.
  */
  e = RTExceptions_GetExceptionBlock ();
  return (RTExceptions_GetNumber (e)) != (UINT_MAX);
}

void _M2_M2EXCEPTION_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
  RTExceptions_SetExceptionBlock (RTExceptions_InitExceptionBlock ());
}

void _M2_M2EXCEPTION_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
