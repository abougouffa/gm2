/* Copyright (C) 2005 Free Software Foundation, Inc. */
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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA */


#include <p2c/p2c.h>


#define StorageG
#include "GStorage.h"


#ifndef SysStorageH
#include "GSysStorage.h"
#endif


void Storage_ALLOCATE(void **a, unsigned int Size)
{
  SysStorage_ALLOCATE(a, Size);
}


void Storage_DEALLOCATE(void **a, unsigned int Size)
{
  SysStorage_DEALLOCATE(a, Size);
}


void Storage_REALLOCATE(void **a, unsigned int Size)
{
  SysStorage_REALLOCATE(a, Size);
}

BOOLEAN Storage_Available(unsigned int Size)
{
  return (SysStorage_Available(Size));
}


void _M2_Storage_init(void)
{
}