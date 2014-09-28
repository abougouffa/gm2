/* Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012,
 *               2013, 2014
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
/*
 *
 * Implementation module in C.
 *
 */

#include <config.h>

#if defined(HAVE_STDDEF_H)
/* to obtain a definition for NULL */
# include <stddef.h>
#endif

#if defined(HAVE_STDIO_H)
/* to obtain a definition for NULL */
# include <stdio.h>
#endif

#if defined(HAVE_SYS_TIME_H)
# include <sys/time.h>
#endif

#if defined(HAVE_TIME_H)
/* to obtain a definition for NULL */
# include <time.h>
#endif

#if defined(HAVE_STRING_H)
/* to obtain a definition for NULL */
# include <string.h>
#endif

#if defined(HAVE_WCHAR_H)
/* to obtain a definition for NULL */
# include <wchar.h>
#endif

#if defined(HAVE_STDLIB_H)
/* to obtain a prototype for free and malloc */
# include <stdlib.h>
#endif

#if !defined(NULL)
# define NULL (void *)0
#endif

#if defined(HAVE_SELECT)
# define FDSET_T fd_set
#else
# define FDSET_T void
#endif


/*
   PROCEDURE Select (nooffds: CARDINAL;
                     readfds, writefds, exceptfds: SetOfFd;
                     timeout: Timeval) : INTEGER ;
*/

#if defined(HAVE_STRUCT_TIMEVAL)
int Selective_Select (int nooffds,
		      fd_set *readfds,
		      fd_set *writefds,
		      fd_set *exceptfds,
		      struct timeval *timeout)
{
  return select(nooffds, readfds, writefds, exceptfds, timeout);
}
#else
int Selective_Select (int nooffds,
		      void *readfds,
		      void *writefds,
		      void *exceptfds,
		      void *timeout)
{
  return 0;
}
#endif

/*
   PROCEDURE InitTime (sec, usec) : Timeval ;
*/

#if defined(HAVE_STRUCT_TIMEVAL)
struct timeval *Selective_InitTime (unsigned int sec, unsigned int usec)
{
  struct timeval *t=(struct timeval *)malloc(sizeof(struct timeval));

  t->tv_sec = (long int)sec;
  t->tv_usec = (long int)usec;
  return t;
}

void Selective_GetTime (struct timeval *t,
			unsigned int *sec, unsigned int *usec)
{
  *sec = (unsigned int)t->tv_sec;
  *usec = (unsigned int)t->tv_usec;
}

void Selective_SetTime (struct timeval *t,
			unsigned int sec, unsigned int usec)
{
  t->tv_sec = sec;
  t->tv_usec = usec;
}

/*
   PROCEDURE KillTime (t: Timeval) : Timeval ;
*/

struct timeval *Selective_KillTime (struct timeval *t)
{
#if defined(HAVE_STDLIB_H)
  free (t);
#endif
  return NULL;
}

/*
   PROCEDURE InitSet () : SetOfFd ;
*/

FDSET_T *Selective_InitSet (void)
{
#if defined(HAVE_STDLIB_H)
  FDSET_T *s = (FDSET_T *) malloc (sizeof (FDSET_T));

  return s;
#else
  return NULL
#endif
}

/*
   PROCEDURE KillSet (s: SetOfFd) : SetOfFd ;
*/

FDSET_T *Selective_KillSet (FDSET_T *s)
{
#if defined(HAVE_STDLIB_H)
  free (s);
#endif
  return NULL;
}

/*
   PROCEDURE FdZero (s: SetOfFd) ;
*/

void Selective_FdZero (FDSET_T *s)
{
  FD_ZERO (s);
}

/*
   PROCEDURE Fd_Set (fd: INTEGER; SetOfFd) ;
*/

void Selective_FdSet (int fd, FDSET_T *s)
{
  FD_SET (fd, s);
}


/*
   PROCEDURE FdClr (fd: INTEGER; SetOfFd) ;
*/

void Selective_FdClr (int fd, FDSET_T *s)
{
  FD_CLR (fd, s);
}


/*
   PROCEDURE FdIsSet (fd: INTEGER; SetOfFd) : BOOLEAN ;
*/

int Selective_FdIsSet (int fd, FDSET_T *s)
{
  return FD_ISSET (fd, s);
}

/*
   GetTimeOfDay - fills in a record, Timeval, filled in with the
                  current system time in seconds and microseconds.
                  It returns zero (see man 3p gettimeofday)
*/

int Selective_GetTimeOfDay (struct timeval *t)
{
    return gettimeofday (t, NULL);
}
#else

void *Selective_InitTime (unsigned int sec, unsigned int usec)
{
  return NULL;
}

void *Selective_KillTime (void *t)
{
  return NULL;
}

void Selective_GetTime (void *t,
			unsigned int *sec, unsigned int *usec)
{
}

void Selective_SetTime (void *t,
			unsigned int sec, unsigned int usec)
{
}

FDSET_T *Selective_InitSet (void)
{
  return NULL;
}

FDSET_T *Selective_KillSet (void)
{
  return NULL;
}

void Selective_FdZero (void *s)
{
}

void Selective_FdSet (int fd, void *s)
{
}

void Selective_FdClr (int fd, void *s)
{
}

int Selective_FdIsSet (int fd, void *s)
{
  return 0;
}

int Selective_GetTimeOfDay (void *t)
{
    return -1;
}
#endif


/*
   PROCEDURE MaxFdsPlusOne (a, b: File) : File ;
*/

int Selective_MaxFdsPlusOne (int a, int b)
{
  if (a>b)
    return a+1;
  else
    return b+1;
}


/*
   PROCEDURE WriteCharRaw (fd: INTEGER; ch: CHAR) ;
*/

void Selective_WriteCharRaw (int fd, char ch)
{
  write (fd, &ch, 1);
}


/*
   PROCEDURE ReadCharRaw (fd: INTEGER) : CHAR ;
*/

char Selective_ReadCharRaw (int fd)
{
  char ch;

  read (fd, &ch, 1);
  return ch;
}

void _M2_Selective_init () {}
void _M2_Selective_finish () {}
