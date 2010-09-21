/* Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc. */
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

#include <libiberty.h>
#include "../gm2-libiberty/Gchoosetemp.h"

/* Return a temporary file name (as a string) or NIL if unable to create
   one.  */

void *choosetemp_make_temp_file(void *suffix)
{
  return (void *)make_temp_file((const char *)suffix);
}

/* to satisfy the GM2 linker */
void
_M2_choosetemp_init (void) {}


/* to satisfy the GM2 linker */
void
_M2_choosetemp_finish (void) {}
