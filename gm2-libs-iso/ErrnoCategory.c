/* Copyright (C) 2008, 2009, 2010 Free Software Foundation, Inc. */
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


#include <p2c/p2c.h>
#include "ChanConsts.h"

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
 *  GetOpenResults - maps errno onto the ISO Modula-2 enumerated
 *                   type, OpenResults.
 */

openResults
ErrnoCategory_GetOpenResults (int e)
{
  if (e == 0)
    return opened;
#if defined(HAVE_ERRNO_H) || defined(HAVE_SYS_ERRNO_H)
  switch (e) {
  case EPERM:   return wrongPermissions; break;
  case ENOENT:  return noSuchFile; break;
  case ENXIO:   return noSuchFile; break;
  case EACCES:  return wrongPermissions; break;
  case ENOTBLK: return wrongFileType; break;
  case EEXIST:  return fileExists; break;
  case ENODEV:  return noSuchFile; break;
  case ENOTDIR: return wrongFileType; break;
  case EISDIR:  return wrongFileType; break;
  case EINVAL:  return wrongFlags; break;
  case ENFILE:  return tooManyOpen; break;
  case EMFILE:  return tooManyOpen; break;
  case ENOTTY:  return wrongFileType; break;
  case ENOSPC:  return noRoomOnDevice; break;
  case EROFS:   return wrongPermissions; break;

  default:
    return otherProblem;
  }
#else
  return otherProblem;
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
