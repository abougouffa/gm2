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


/* --fixme-- put the HAVE_UIO definition into configure.in in gm2 directory */
#define HAVE_UIO

#if defined(HAVE_UIO)
#include <sys/uio.h>
#endif

#define libcG
#include "Glibc.h"


/*
   getenv - returns the value of an environment string.
*/

void *libc_getenv (void *p)
{
	return( getenv(p) );
}

/*
   isatty - returns true if the default input channel is the keyboard.
*/

int libc_isatty()
{
	return( !isatty(STDOUT_FILENO) );
}


/*
   exit - returns an exit status to the process callee.
*/

void libc_exit (r)
long r;
{
	exit(r);
}


long libc_write(d, buf, nbytes)
long nbytes;
Anyptr buf;
long d;
{
	return( write(d, buf, nbytes) );
}


long libc_read(d, buf, nbytes)
long nbytes;
Anyptr buf;
long d;
{
	return( read(d, buf, nbytes) );
}


long libc_system (string)
void *string;
{
    return system(string);
}


/*
     abort - generate a fault

     abort() first closes all open files if possible, then sends
     an IOT signal to the process.  This signal usually results
     in termination with a core dump, which may be used for
     debugging.

     It is possible for abort() to return control if is caught or
     ignored, in which case the value returned is that of the
     kill(2V) system call.
*/

void libc_abort ()
{
	abort();
}


/*
   malloc and free.

DESCRIPTION
     These routines provide a general-purpose memory allocation
     package.  They maintain a table of free blocks for efficient
     allocation and coalescing of free storage.  When there is no
     suitable space already free, the allocation routines call
     sbrk() (see brk(2)) to get more memory from the system.
 
     Each of the allocation routines returns a pointer to space
     suitably aligned for storage of any type of object. Each
     returns a NULL pointer if the request cannot be completed.
*/


/*
     malloc - memory allocator.

     char *malloc(size)
     unsigned size;

     malloc() returns a pointer to a block of at least size
     bytes, which is appropriately aligned.  If size is zero,
     malloc() returns a non-NULL pointer, but this pointer should
     not be dereferenced.
*/

Anyptr libc_malloc (size)
unsigned long size;
{
	return( malloc(size) );
}


/*
     free - memory deallocator.
 
     free(ptr)
     char *ptr;

     free() releases a previously allocated block.  Its argument
     is a pointer to a block previously allocated by malloc, cal-
     loc, realloc, malloc, or memalign.
*/

void libc_free (ptr)
Anyptr ptr;
{
	free(ptr);
}


/*
   getpid - returns the process identification number.
*/

long libc_getpid (void)
{
  return( getpid() );
}


/*
   dup - duplicates the file descriptor, d.
*/

long libc_dup (long d)
{
  return( dup(d) );
}


/*
   close - closes the file descriptor, d.
*/

long libc_close (long d)
{
  return( close(d) );
}


/*
   open - opens the filename with flag and mode.
*/

long libc_open (void *filename, unsigned long flag, unsigned long mode)
{
  return( open(filename, flag, mode) );
}


/*
   creat - creates a new file.
*/

long libc_creat (void *filename, unsigned long mode)
{
  return( creat(filename, mode) );
}


/*
   lseek - seeks to a new position in a file.
*/

long libc_lseek (long fd, long offset, long whence)
{
  return( lseek(fd, offset, whence) );
}


/*
  perror - display errno along with str
*/

void libc_perror (void *s)
{
  perror(s);
}


/*
   readv - reads in an io vector of bytes.
*/

long libc_readv (long fd, void *v, long n)
{
  return( readv(fd, v, n) );
}


/*
   writev - writes an io vector of bytes.
*/

long libc_writev (long fd, void *v, long n)
{
  return( writev(fd, v, n) );
}


/*
   environ - returns the libc variable environ.
*/

void *libc_environ (void)
{
#if ! (defined (VMS) || defined (OS2))
extern char **environ;
 return( (void *)environ );
#else
 return( NULL );
#endif
}


/*
  setenv - adds the variable name to the environment with the
           value value, if name does not already exist.
           If name does exist in the environment, then its
           value is changed to value if overwrite is non-zero;
           if overwrite is zero, then the value of name is not changed.
*/

#if !defined(HAVE_SETENV) && defined(HAVE_PUTENV)

long setenv(void *var, void *string, long overwrite)
{
  int  exists, varLen, strLen, result;
  char *buf;

  buf    = getenv(var);
  exists = buf != NULL;
  if (exists && !overwrite)
      return -1;

  varLen = strlen(var);
  strLen = strlen(string);
  buf    = malloc(varLen+strLen+2);
  if (buf != NULL) {
      strcpy(buf, var);
      strcpy(buf+varLen, "=");
      strcpy(buf+varLen+1, string);
      result = putenv(buf);
      free(buf);
      return result;
  }

  return -1;
}

#endif

long libc_setenv (void *name, void *value, long overwrite)
{
  return( setenv(name, value, overwrite) );
}


/*
 *  getcwd - copies the absolute pathname of  the
 *           current  working directory to the array pointed to by buf,
 *           which is of length size.
 *
 *           If the current absolute path name would require  a  buffer
 *           longer  than size elements, NULL is returned, and errno is
 *           set to ERANGE; an application should check for this error,
 *           and allocate a larger buffer if necessary.
 */

void *libc_getcwd (void *buf, long size)
{
  return getcwd(buf, size);
}


/*
   chown - The  owner  of  the  file  specified  by  path or by fd is
           changed.  Only the super-user may change the  owner  of  a
           file.   The  owner  of  a file may change the group of the
           file to any group of which that owner is  a  member.   The
           super-user may change the group arbitrarily.

           If  the owner or group is specified as -1, then that ID is
           not changed.

           On success, zero is returned.  On error, -1  is  returned,
           and errno is set appropriately.
*/

long libc_chown (void *filename, long uid, long gid)
{
  return chown(filename, uid, gid);
}


/*
 *  strlen - return length of string, a.
 */

long libc_strlen (void *a)
{
  return( strlen(a) );
}

/*
 *  strcpy - copies string, src, longo, dest.
 */

void libc_strcpy (void *dest, void *src)
{
  strcpy(dest, src);
}


/*
 *  strncpy - copies string, src, into, dest, copying at most, n, bytes.
 */

void libc_strncpy (void *dest, void *src, unsigned long n)
{
  strncpy(dest, src, n);
}

/*
 *  unlink - removes file and returns 0 if successful.
 */

long libc_unlink (void *file)
{
  return( unlink(file) );
}

/*
 *  memcpy - maps onto equivalent libc call
 */

void libc_memcpy (void *dest, void *src, unsigned long size)
{
  memcpy(dest, src, size);
}

/*
 *  memset - maps onto equivalent libc call
 */

void libc_memset (void *s, long c, unsigned long size)
{
  memset(s, c, size);
}


void _M2_libc_init (void)
{
}
