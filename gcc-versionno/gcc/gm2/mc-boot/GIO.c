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
MA  02110-1301  USA  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   if !defined (TRUE)
#      define TRUE (1==1)
#   endif

#   if !defined (FALSE)
#      define FALSE (1==0)
#   endif

#include <stdlib.h>
#define _IO_H
#define _IO_C

#   include "GStrLib.h"
#   include "GSYSTEM.h"
#   include "Glibc.h"
#   include "GFIO.h"
#   include "Gerrno.h"
#   include "GASCII.h"
#   include "Gtermios.h"

#   define MaxDefaultFd 2
typedef struct BasicFds_r BasicFds;

typedef struct _T1_a _T1;

struct BasicFds_r {
                    unsigned int IsEof;
                    unsigned int IsRaw;
                  };

struct _T1_a { BasicFds array[MaxDefaultFd+1]; };
static _T1 fdState;

/*
   IsDefaultFd - returns TRUE if, fd, is 0, 1 or 2.
*/

void IO_Read (char *ch);

/*
   doWrite - performs the write of a single character, ch,
             onto fd or f.
*/

void IO_Write (char ch);

/*
   doWrite - performs the write of a single character, ch,
             onto fd or f.
*/

void IO_Error (char ch);
void IO_UnBufferedMode (int fd, unsigned int input);
void IO_BufferedMode (int fd, unsigned int input);

/*
   EchoOn - turns on echoing for file descriptor, fd.  This
            only really makes sence for a file descriptor opened
            for terminal input or maybe some specific file descriptor
            which is attached to a particular piece of hardware.
*/

void IO_EchoOn (int fd, unsigned int input);

/*
   EchoOff - turns off echoing for file descriptor, fd.  This
             only really makes sence for a file descriptor opened
             for terminal input or maybe some specific file descriptor
             which is attached to a particular piece of hardware.
*/

void IO_EchoOff (int fd, unsigned int input);

/*
   IsDefaultFd - returns TRUE if, fd, is 0, 1 or 2.
*/

static unsigned int IsDefaultFd (int fd);

/*
   doWrite - performs the write of a single character, ch,
             onto fd or f.
*/

static void doWrite (int fd, FIO_File f, char ch);

/*
   setFlag - sets or unsets the appropriate flag in, t.
*/

static void setFlag (termios_TERMIOS t, termios_Flag f, unsigned int b);

/*
   doraw - sets all the flags associated with making this
           file descriptor into raw input/output.
*/

static void doraw (termios_TERMIOS term);

/*
   dononraw - sets all the flags associated with making this
              file descriptor into non raw input/output.
*/

static void dononraw (termios_TERMIOS term);

/*
   Init - 
*/

static void Init (void);


/*
   IsDefaultFd - returns TRUE if, fd, is 0, 1 or 2.
*/

static unsigned int IsDefaultFd (int fd)
{
  return (fd <= MaxDefaultFd) && (fd >= 0);
}


/*
   doWrite - performs the write of a single character, ch,
             onto fd or f.
*/

static void doWrite (int fd, FIO_File f, char ch)
{
  int r;

  if (fdState.array[fd].IsRaw)
    {
      /* avoid dangling else.  */
      if (! fdState.array[fd].IsEof)
        for (;;)
        {
          r = libc_write (FIO_GetUnixFileDescriptor (f), &ch, (size_t) 1);
          if (r == 1)
            return;
          else if (r == -1)
            {
              r = errno_geterrno ();
              if ((r != errno_EAGAIN) && (r != errno_EINTR))
                {
                  fdState.array[fd].IsEof = TRUE;
                  return;
                }
            }
        }
    }
  else
    FIO_WriteChar (f, ch);
}


/*
   setFlag - sets or unsets the appropriate flag in, t.
*/

static void setFlag (termios_TERMIOS t, termios_Flag f, unsigned int b)
{
  if (termios_SetFlag (t, f, b))
    {}  /* empty.  */
}


/*
   doraw - sets all the flags associated with making this
           file descriptor into raw input/output.
*/

static void doraw (termios_TERMIOS term)
{
  /* 
    * from man 3 termios
    *           termios_p->c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP
    *                                   | INLCR | IGNCR | ICRNL | IXON);
    *           termios_p->c_oflag &= ~OPOST;
    *           termios_p->c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
    *           termios_p->c_cflag &= ~(CSIZE | PARENB);
    *           termios_p->c_cflag |= CS8;
  */
  setFlag (term, (termios_Flag) termios_ignbrk, FALSE);
  setFlag (term, (termios_Flag) termios_ibrkint, FALSE);
  setFlag (term, (termios_Flag) termios_iparmrk, FALSE);
  setFlag (term, (termios_Flag) termios_istrip, FALSE);
  setFlag (term, (termios_Flag) termios_inlcr, FALSE);
  setFlag (term, (termios_Flag) termios_igncr, FALSE);
  setFlag (term, (termios_Flag) termios_icrnl, FALSE);
  setFlag (term, (termios_Flag) termios_ixon, FALSE);
  setFlag (term, (termios_Flag) termios_opost, FALSE);
  setFlag (term, (termios_Flag) termios_lecho, FALSE);
  setFlag (term, (termios_Flag) termios_lechonl, FALSE);
  setFlag (term, (termios_Flag) termios_licanon, FALSE);
  setFlag (term, (termios_Flag) termios_lisig, FALSE);
  setFlag (term, (termios_Flag) termios_liexten, FALSE);
  setFlag (term, (termios_Flag) termios_parenb, FALSE);
  setFlag (term, (termios_Flag) termios_cs8, TRUE);
}


/*
   dononraw - sets all the flags associated with making this
              file descriptor into non raw input/output.
*/

static void dononraw (termios_TERMIOS term)
{
  /* 
    * we undo these settings, (although we leave the character size alone)
    *
    * from man 3 termios
    *           termios_p->c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP
    *                                   | INLCR | IGNCR | ICRNL | IXON);
    *           termios_p->c_oflag &= ~OPOST;
    *           termios_p->c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
    *           termios_p->c_cflag &= ~(CSIZE | PARENB);
    *           termios_p->c_cflag |= CS8;
  */
  setFlag (term, (termios_Flag) termios_ignbrk, TRUE);
  setFlag (term, (termios_Flag) termios_ibrkint, TRUE);
  setFlag (term, (termios_Flag) termios_iparmrk, TRUE);
  setFlag (term, (termios_Flag) termios_istrip, TRUE);
  setFlag (term, (termios_Flag) termios_inlcr, TRUE);
  setFlag (term, (termios_Flag) termios_igncr, TRUE);
  setFlag (term, (termios_Flag) termios_icrnl, TRUE);
  setFlag (term, (termios_Flag) termios_ixon, TRUE);
  setFlag (term, (termios_Flag) termios_opost, TRUE);
  setFlag (term, (termios_Flag) termios_lecho, TRUE);
  setFlag (term, (termios_Flag) termios_lechonl, TRUE);
  setFlag (term, (termios_Flag) termios_licanon, TRUE);
  setFlag (term, (termios_Flag) termios_lisig, TRUE);
  setFlag (term, (termios_Flag) termios_liexten, TRUE);
}


/*
   Init - 
*/

static void Init (void)
{
  fdState.array[0].IsEof = FALSE;
  fdState.array[0].IsRaw = FALSE;
  fdState.array[1].IsEof = FALSE;
  fdState.array[1].IsRaw = FALSE;
  fdState.array[2].IsEof = FALSE;
  fdState.array[2].IsRaw = FALSE;
}


/*
   IsDefaultFd - returns TRUE if, fd, is 0, 1 or 2.
*/

void IO_Read (char *ch)
{
  int r;

  FIO_FlushBuffer (FIO_StdOut);
  FIO_FlushBuffer (FIO_StdErr);
  if (fdState.array[0].IsRaw)
    if (fdState.array[0].IsEof)
      (*ch) = ASCII_eof;
    else
      for (;;)
      {
        r = libc_read (FIO_GetUnixFileDescriptor (FIO_StdIn), ch, (size_t) 1);
        if (r == 1)
          return;
        else if (r == -1)
          {
            r = errno_geterrno ();
            if (r != errno_EAGAIN)
              {
                fdState.array[0].IsEof = TRUE;
                (*ch) = ASCII_eof;
                return;
              }
          }
      }
  else
    (*ch) = FIO_ReadChar (FIO_StdIn);
}


/*
   doWrite - performs the write of a single character, ch,
             onto fd or f.
*/

void IO_Write (char ch)
{
  doWrite (1, FIO_StdOut, ch);
}


/*
   doWrite - performs the write of a single character, ch,
             onto fd or f.
*/

void IO_Error (char ch)
{
  doWrite (2, FIO_StdErr, ch);
}

void IO_UnBufferedMode (int fd, unsigned int input)
{
  termios_TERMIOS term;
  int r;

  if (IsDefaultFd (fd))
    fdState.array[fd].IsRaw = TRUE;
  term = termios_InitTermios ();
  if ((termios_tcgetattr (fd, term)) == 0)
    {
      doraw (term);
      if (input)
        r = termios_tcsetattr (fd, termios_tcsflush (), term);
      else
        r = termios_tcsetattr (fd, termios_tcsdrain (), term);
    }
  term = termios_KillTermios (term);
}

void IO_BufferedMode (int fd, unsigned int input)
{
  termios_TERMIOS term;
  int r;

  if (IsDefaultFd (fd))
    fdState.array[fd].IsRaw = FALSE;
  term = termios_InitTermios ();
  if ((termios_tcgetattr (fd, term)) == 0)
    {
      dononraw (term);
      if (input)
        r = termios_tcsetattr (fd, termios_tcsflush (), term);
      else
        r = termios_tcsetattr (fd, termios_tcsdrain (), term);
    }
  term = termios_KillTermios (term);
}


/*
   EchoOn - turns on echoing for file descriptor, fd.  This
            only really makes sence for a file descriptor opened
            for terminal input or maybe some specific file descriptor
            which is attached to a particular piece of hardware.
*/

void IO_EchoOn (int fd, unsigned int input)
{
  termios_TERMIOS term;
  int res;
  int r;

  term = termios_InitTermios ();
  if ((termios_tcgetattr (fd, term)) == 0)
    {
      setFlag (term, (termios_Flag) termios_lecho, TRUE);
      if (input)
        r = termios_tcsetattr (fd, termios_tcsflush (), term);
      else
        r = termios_tcsetattr (fd, termios_tcsdrain (), term);
    }
  term = termios_KillTermios (term);
}


/*
   EchoOff - turns off echoing for file descriptor, fd.  This
             only really makes sence for a file descriptor opened
             for terminal input or maybe some specific file descriptor
             which is attached to a particular piece of hardware.
*/

void IO_EchoOff (int fd, unsigned int input)
{
  termios_TERMIOS term;
  int res;
  int r;

  term = termios_InitTermios ();
  if ((termios_tcgetattr (fd, term)) == 0)
    {
      setFlag (term, (termios_Flag) termios_lecho, FALSE);
      if (input)
        r = termios_tcsetattr (fd, termios_tcsflush (), term);
      else
        r = termios_tcsetattr (fd, termios_tcsdrain (), term);
    }
  term = termios_KillTermios (term);
}

void _M2_IO_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
  Init ();
}

void _M2_IO_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
