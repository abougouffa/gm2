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

#include <config.h>

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
