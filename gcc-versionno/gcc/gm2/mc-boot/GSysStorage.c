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
#include <stdlib.h>
#define _SysStorage_H
#define _SysStorage_C

#   include "Glibc.h"
#   include "GDebug.h"

void SysStorage_ALLOCATE (void * *a, unsigned int Size);
void SysStorage_DEALLOCATE (void * *a, unsigned int Size);

/*
   REALLOCATE - attempts to reallocate storage. The address,
                a, should either be NIL in which case ALLOCATE
                is called, or alternatively it should have already
                been initialized by ALLOCATE. The allocated storage
                is resized accordingly.
*/

void SysStorage_REALLOCATE (void * *a, unsigned int Size);

/*
   REALLOCATE - attempts to reallocate storage. The address,
                a, should either be NIL in which case ALLOCATE
                is called, or alternatively it should have already
                been initialized by ALLOCATE. The allocated storage
                is resized accordingly.
*/

unsigned int SysStorage_Available (unsigned int Size);

/*
   Init - 
*/

void SysStorage_Init (void);

void SysStorage_ALLOCATE (void * *a, unsigned int Size)
{
  /* This file is part of GNU Modula-2.

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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA  */
  (*a) = libc_malloc ((size_t) Size);
  if ((*a) == NULL)
    Debug_Halt ((char *) "out of memory error", 19, 31, (char *) "../../gcc-versionno/gcc/gm2/gm2-libs/SysStorage.mod", 51);
}

void SysStorage_DEALLOCATE (void * *a, unsigned int Size)
{
  libc_free ((*a));
  (*a) = NULL;
}


/*
   REALLOCATE - attempts to reallocate storage. The address,
                a, should either be NIL in which case ALLOCATE
                is called, or alternatively it should have already
                been initialized by ALLOCATE. The allocated storage
                is resized accordingly.
*/

void SysStorage_REALLOCATE (void * *a, unsigned int Size)
{
  if ((*a) == NULL)
    SysStorage_ALLOCATE (a, Size);
  else
    {
      (*a) = libc_realloc ((*a), (size_t) Size);
      if ((*a) == NULL)
        Debug_Halt ((char *) "out of memory error", 19, 60, (char *) "../../gcc-versionno/gcc/gm2/gm2-libs/SysStorage.mod", 51);
    }
}


/*
   REALLOCATE - attempts to reallocate storage. The address,
                a, should either be NIL in which case ALLOCATE
                is called, or alternatively it should have already
                been initialized by ALLOCATE. The allocated storage
                is resized accordingly.
*/

unsigned int SysStorage_Available (unsigned int Size)
{
  void * a;

  a = libc_malloc ((size_t) Size);
  if (a == NULL)
    return FALSE;
  else
    {
      libc_free (a);
      return TRUE;
    }
}


/*
   Init - 
*/

void SysStorage_Init (void)
{
}

void _M2_SysStorage_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_SysStorage_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
