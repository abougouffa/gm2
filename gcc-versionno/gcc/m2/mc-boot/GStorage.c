/* do not edit automatically generated by mc from Storage.  */
/* Storage.def provides access to the dynamic Storage handler.

Copyright (C) 2001-2019 Free Software Foundation, Inc.
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
along with GNU Modula-2; see the file COPYING.  If not,
see <https://www.gnu.org/licenses/>.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#define _Storage_H
#define _Storage_C

#   include "GSysStorage.h"

void Storage_ALLOCATE (void * *a, unsigned int Size);
void Storage_DEALLOCATE (void * *a, unsigned int Size);
void Storage_REALLOCATE (void * *a, unsigned int Size);
unsigned int Storage_Available (unsigned int Size);

void Storage_ALLOCATE (void * *a, unsigned int Size)
{
  SysStorage_ALLOCATE (a, Size);
}

void Storage_DEALLOCATE (void * *a, unsigned int Size)
{
  SysStorage_DEALLOCATE (a, Size);
}

void Storage_REALLOCATE (void * *a, unsigned int Size)
{
  SysStorage_REALLOCATE (a, Size);
}

unsigned int Storage_Available (unsigned int Size)
{
  return SysStorage_Available (Size);
}

void _M2_Storage_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_Storage_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}