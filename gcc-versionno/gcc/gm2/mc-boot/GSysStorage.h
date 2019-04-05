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


#if !defined (_SysStorage_H)
#   define _SysStorage_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_SysStorage_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   ALLOCATE - attempt to allocate memory from the heap.
              NIL is returned in, a, if ALLOCATE fails.
*/

EXTERN void SysStorage_ALLOCATE (void * *a, unsigned int Size);

/*
   DEALLOCATE - return, Size, bytes to the heap.
                The variable, a, is set to NIL.
*/

EXTERN void SysStorage_DEALLOCATE (void * *a, unsigned int Size);

/*
   REALLOCATE - attempts to reallocate storage. The address,
                a, should either be NIL in which case ALLOCATE
                is called, or alternatively it should have already
                been initialized by ALLOCATE. The allocated storage
                is resized accordingly.
*/

EXTERN void SysStorage_REALLOCATE (void * *a, unsigned int Size);

/*
   Available - returns TRUE if, Size, bytes can be allocated.
*/

EXTERN unsigned int SysStorage_Available (unsigned int Size);

/*
   Init - initializes the heap.
*/

EXTERN void SysStorage_Init (void);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
