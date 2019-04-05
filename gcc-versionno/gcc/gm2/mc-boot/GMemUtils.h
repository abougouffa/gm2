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


#if !defined (_MemUtils_H)
#   define _MemUtils_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_MemUtils_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   MemCopy - copys a region of memory to the required destination.
*/

EXTERN void MemUtils_MemCopy (void * from, unsigned int length, void * to);

/*
   MemZero - sets a region of memory: a..a+length to zero.
*/

EXTERN void MemUtils_MemZero (void * a, unsigned int length);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
