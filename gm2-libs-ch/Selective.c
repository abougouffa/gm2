/*
 *
 * Implementation module in C.
 *
 */

#include <p2c/p2c.h>

/*
   PROCEDURE Select (nooffds: CARDINAL;
                     readfds, writefds, exceptfds: SetOfFd;
                     timeout: Timeval) : INTEGER ;
*/

#if defined(HAVE_SELECT)
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

#if defined(HAVE_TIMEVAL)
struct timeval *Selective_InitTime (long sec, long usec)
{
  struct timeval *t=(struct timeval *)malloc(sizeof(struct timeval));

  t->tv_sec = sec;
  t->tv_usec = usec;
  return t;
}

void Selective_GetTime (struct timeval *t,
			long *sec, long *usec)
{
  *sec = t->tv_sec;
  *usec = t->tv_usec;
}

/*
   PROCEDURE KillTime (t: Timeval) : Timeval ;
*/

struct timeval *Selective_KillTime (struct timeval *t)
{
  free(t);
  return NULL;
}

/*
   PROCEDURE InitSet () : SetOfFd ;
*/

fd_set *Selective_InitSet (void)
{
  fd_set *s=(fd_set *)malloc(sizeof(fd_set));

  return s;
}

/*
   PROCEDURE KillSet (s: SetOfFd) : SetOfFd ;
*/

fd_set *Selective_KillSet (fd_set *s)
{
  free(s);
  return NULL;
}

/*
   PROCEDURE FdZero (s: SetOfFd) ;
*/

void Selective_FdZero (fd_set *s)
{
  FD_ZERO(s);
}


/*
   PROCEDURE Fd_Set (fd: INTEGER; SetOfFd) ;
*/

void Selective_FdSet (int fd, fd_set *s)
{
  FD_SET(fd, s);
}


/*
   PROCEDURE FdClr (fd: INTEGER; SetOfFd) ;
*/

void Selective_FdClr (int fd, fd_set *s)
{
  FD_CLR(fd, s);
}


/*
   PROCEDURE FdIsSet (fd: INTEGER; SetOfFd) : BOOLEAN ;
*/

int Selective_FdIsSet (int fd, fd_set *s)
{
  return FD_ISSET(fd, s);
}

#else

void *Selective_InitTime (long sec, long usec)
{
  return NULL;
}

void *Selective_KillTime (void *t)
{
  return NULL;
}

void Selective_GetTime (struct timeval *t,
			long *sec, long *usec)
{
}

fd_set *Selective_InitSet (void)
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
  write(fd, &ch, 1);
}


/*
   PROCEDURE ReadCharRaw (fd: INTEGER) : CHAR ;
*/

char Selective_ReadCharRaw (int fd)
{
  char ch;

  read(fd, &ch, 1);
  return ch;
}

void _M2_Selective_init () {}

