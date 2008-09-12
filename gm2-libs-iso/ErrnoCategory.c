/* Copyright (C) 2008 Free Software Foundation, Inc. */
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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
MA  02110-1301  USA */

#include <stdio.h>

#include "gm2-libs/gm2-libs-host.h"

#if defined(HAVE_ERRNO_H)
#   include <errno.h>
#endif

#if defined(HAVE_SYS_ERRNO_H)
#   include <sys/errno.h>
#endif


/*
 *  IsErrnoHard - returns TRUE if the value of errno is associated with
 *                a hard device error.
 */

int ErrnoCategory_IsErrnoHard (int e)
{
#if defined(HAVE_ERRNO_H) || defined(HAVE_SYS_ERRNO_H)
  return ((e == EPERM) || (e == ENOENT) || (e == EIO) ||
	  (e == ENXIO) || (e == EACCES) || (e == ENOTBLK) ||
	  (e == ENODEV) || (e == EINVAL) || (e == ENFILE) ||
	  (e == EROFS) || (e == EMLINK));
#else
  return FALSE;
#endif
}


/*
 *  IsErrnoSoft - returns TRUE if the value of errno is associated with
 *                a soft device error.
 */

int ErrnoCategory_IsErrnoSoft (int e)
{
#if defined(HAVE_ERRNO_H) || defined(HAVE_SYS_ERRNO_H)
  return ((e == ESRCH) || (e == EINTR) || (e == E2BIG) ||
	  (e == ENOEXEC) || (e == EBADF) || (e == ECHILD) ||
	  (e == EAGAIN) || (e == ENOMEM) || (e == EFAULT) ||
	  (e == EBUSY) || (e == EEXIST) ||
	  (e == EXDEV) || (e == ENOTDIR) || (e == EISDIR) ||
	  (e == EMFILE) || (e == ENOTTY) || (e == ETXTBSY) ||
	  (e == EFBIG) || (e == ENOSPC) || (e == EPIPE));
#else
  return FALSE;
#endif
}


int ErrnoCategory_UnAvailable (int e)
{
#if defined(HAVE_ERRNO_H) || defined(HAVE_SYS_ERRNO_H)
  return ((e == ENOENT) || (e == ESRCH) || (e == ENXIO) ||
	  (e == ECHILD) || (e == ENOTBLK) || (e == ENODEV) ||
	  (e == ENOTDIR));
#else
  return FALSE;
#endif
}

/*
 *  GNU Modula-2 linking fodder.
 */

void _M2_ErrnoCategory_init (void)
{
}

void _M2_ErrnoCategory_finish (void)
{
}
