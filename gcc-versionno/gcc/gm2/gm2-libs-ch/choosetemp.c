/* choosetemp.c provide access to temporary file creation.

Copyright (C) 2005-2019 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

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

#include <libiberty.h>
#include "../gm2-libiberty/Gchoosetemp.h"

/* Return a temporary file name (as a string) or NIL if unable to
create one.  */

void *
choosetemp_make_temp_file (void *suffix)
{
  return (void *)make_temp_file ((const char *)suffix);
}

/* to satisfy the GM2 linker.  */
void
_M2_choosetemp_init (void)
{
}

/* to satisfy the GM2 linker.  */
void
_M2_choosetemp_finish (void)
{
}
