/* Copyright (C) 2009, 2010
 *               Free Software Foundation, Inc. */
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

#include "gm2-libs-host.h"

#if defined(HAVE_ERRNO_H)
#  include <errno.h>
#endif

#if defined(HAVE_MATH_H)
#  include <math.h>
#endif

#if defined(HAVE_STDIO_H)
#  include <stdio.h>
#endif

#if defined(HAVE_STRINGS_H)
#  include <strings.h>
#endif

#if defined(HAVE_STRING_H)
#  include <string.h>
#endif

#if defined(HAVE_SYS_TIME_H)
#  include <sys/time.h>
#endif

#if defined(HAVE_TIME_H)
#  include <time.h>
#endif

#if defined(HAVE_SYS_STAT_H)
#  include <sys/stat.h>
#endif

#if defined(HAVE_SYS_ERRNO_H)
#  include <sys/errno.h>
#endif

#if defined(HAVE_ERRNO_H)
#  include <errno.h>
#endif

int errno_geterrno (void)
{
#if defined(HAVE_ERRNO_H) || defined(HAVE_SYS_ERRNO_H)
  return errno;
#else
  return -1;
#endif
}

void _M2_errno_init (void)
{
}

void _M2_errno_finish (void)
{
}
