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


#if !defined (_M2EXCEPTION_H)
#   define _M2EXCEPTION_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_M2EXCEPTION_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

typedef enum {M2EXCEPTION_indexException, M2EXCEPTION_rangeException, M2EXCEPTION_caseSelectException, M2EXCEPTION_invalidLocation, M2EXCEPTION_functionException, M2EXCEPTION_wholeValueException, M2EXCEPTION_wholeDivException, M2EXCEPTION_realValueException, M2EXCEPTION_realDivException, M2EXCEPTION_complexValueException, M2EXCEPTION_complexDivException, M2EXCEPTION_protException, M2EXCEPTION_sysException, M2EXCEPTION_coException, M2EXCEPTION_exException} M2EXCEPTION_M2Exceptions;

EXTERN M2EXCEPTION_M2Exceptions M2EXCEPTION_M2Exception (void);
EXTERN unsigned int M2EXCEPTION_IsM2Exception (void);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
