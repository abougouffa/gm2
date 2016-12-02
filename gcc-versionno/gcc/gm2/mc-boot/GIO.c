/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/IO.mod.  */

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

#include "Gmcrts.h"
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
void IO_Read (char *ch);
void IO_Write (char ch);
void IO_Error (char ch);
void IO_UnBufferedMode (int fd, unsigned int input);
void IO_BufferedMode (int fd, unsigned int input);
void IO_EchoOn (int fd, unsigned int input);
void IO_EchoOff (int fd, unsigned int input);
static unsigned int IsDefaultFd (int fd);
static void doWrite (int fd, FIO_File f, char ch);
static void setFlag (termios_TERMIOS t, termios_Flag f, unsigned int b);
static void doraw (termios_TERMIOS term);
static void dononraw (termios_TERMIOS term);
static void Init (void);

static unsigned int IsDefaultFd (int fd)
{
  return (fd <= MaxDefaultFd) && (fd >= 0);
  ReturnException ("../../gcc-5.2.0/gcc/gm2/gm2-libs/IO.def", 20, 0);
}

static void doWrite (int fd, FIO_File f, char ch)
{
  int r;

  if (fdState.array[fd].IsRaw)
  {
    /* avoid dangling else.  */
    if (! fdState.array[fd].IsEof)
      for (;;)
      {
        r = libc_write (FIO_GetUnixFileDescriptor (f), &ch, 1);
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

static void setFlag (termios_TERMIOS t, termios_Flag f, unsigned int b)
{
  if (termios_SetFlag (t, f, b))
    {}  /* empty.  */
}

static void doraw (termios_TERMIOS term)
{
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

static void dononraw (termios_TERMIOS term)
{
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

static void Init (void)
{
  fdState.array[0].IsEof = FALSE;
  fdState.array[0].IsRaw = FALSE;
  fdState.array[1].IsEof = FALSE;
  fdState.array[1].IsRaw = FALSE;
  fdState.array[2].IsEof = FALSE;
  fdState.array[2].IsRaw = FALSE;
}

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
        r = libc_read (FIO_GetUnixFileDescriptor (FIO_StdIn), ch, 1);
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

void IO_Write (char ch)
{
  doWrite (1, FIO_StdOut, ch);
}

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
