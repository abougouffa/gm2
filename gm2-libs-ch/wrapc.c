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

/*
   strtime - returns the address of a string which describes the
             local time.
*/

char *wrapc_strtime (void)
{
#if defined(HAVE_CTIME)
  void *clock = time((void *)0) ;
  char *string = ctime(&clock);

  string[24] = (char) 0;

  return string;
#else
  return "";
#endif
}


int wrapc_filesize (int f)
{
  struct stat s;

  if (fstat(f, (struct stat *) &s) == 0)
    return s.st_size;
  else
    return -1;
}


/*
 *   filemtime - returns the mtime of a file, f.
 */

int wrapc_filemtime (int f)
{
  struct stat s;

  if (fstat(f, (struct stat *) &s) == 0)
    return s.st_mtime;
  else
    return -1;
}


/*
   getrand - returns a random number between 0..n-1
*/

int wrapc_getrand (int n)
{
  return rand() % n;
}

#if defined(HAVE_PWD_H)
#include <pwd.h>

char *wrapc_getusername (void)
{
  return getpwuid(getuid()) -> pw_gecos;
}

/*
   getnameuidgid - fills in the, uid, and, gid, which represents
                   user, name.
*/

void wrapc_getnameuidgid (char *name, int *uid, int *gid)
{
  struct passwd *p=getpwnam(name);

  if (p == NULL) {
    *uid = -1;
    *gid = -1;
  } else {
    *uid = p->pw_uid;
    *gid = p->pw_gid;
  }
}
#else
char *wrapc_getusername (void)
{
  return "unknown";
}

void wrapc_getnameuidgid (char *name, int *uid, int *gid)
{
  *uid = -1;
  *gid = -1;
}
#endif


/*
   init - init function for the module
*/

void _M2_wrapc_init() {}