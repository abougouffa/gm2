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


#if !defined (_SysExceptions_H)
#   define _SysExceptions_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_SysExceptions_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

typedef struct SysExceptions_PROCEXCEPTION_p SysExceptions_PROCEXCEPTION;

typedef void (*SysExceptions_PROCEXCEPTION_t) (void *);
struct SysExceptions_PROCEXCEPTION_p { SysExceptions_PROCEXCEPTION_t proc; };

EXTERN void SysExceptions_InitExceptionHandlers (SysExceptions_PROCEXCEPTION indexf, SysExceptions_PROCEXCEPTION range, SysExceptions_PROCEXCEPTION casef, SysExceptions_PROCEXCEPTION invalidloc, SysExceptions_PROCEXCEPTION function, SysExceptions_PROCEXCEPTION wholevalue, SysExceptions_PROCEXCEPTION wholediv, SysExceptions_PROCEXCEPTION realvalue, SysExceptions_PROCEXCEPTION realdiv, SysExceptions_PROCEXCEPTION complexvalue, SysExceptions_PROCEXCEPTION complexdiv, SysExceptions_PROCEXCEPTION protection, SysExceptions_PROCEXCEPTION systemf, SysExceptions_PROCEXCEPTION coroutine, SysExceptions_PROCEXCEPTION exception);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
