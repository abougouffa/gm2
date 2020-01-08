/* xlibc.c allow access to some poisoned functions.

Copyright (C) 2010-2020 Free Software Foundation, Inc.
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
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "gm2-libs-host.h"

#if defined(HAVE_STDIO_H)
#include <stdio.h>
#endif

#if defined(HAVE_STDDEF_H)
/* to obtain a definition for NULL */
# include <stddef.h>
#endif

#if defined(HAVE_STDIO_H)
/* to obtain a definition for NULL */
# include <stdio.h>
#endif

#if defined(HAVE_STRING_H)
/* to obtain a definition for NULL */
# include <string.h>
#endif

#if defined(HAVE_STDLIB_H)
/* to obtain a prototype for free and malloc */
# include <stdlib.h>
#endif

#if !defined(NULL)
# define NULL (void *)0
#endif


char *
xstrdup (char *src)
{
  char *dst = (char *) malloc (strlen (src) + 1);

  if (dst == NULL)
    return NULL;
  strcpy (dst, src);
  return dst;
}
