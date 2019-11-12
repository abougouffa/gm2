/* 
   Author     : Gaius Mulley
   Title      : IO
   Date       : 3/4/86  [$Date: 2010/10/03 19:01:10 $]
   SYSTEM     : GNU Modula-2
   Description: provides Read, Write, Errors procedures that map onto UNIX
                file descriptors 0, 1 and 2. This is achieved by using
                FIO if we are in buffered mode and using libc.write
                if not.
   Version    : $Revision: 1.6 $
  */


#if !defined (_IO_H)
#   define _IO_H

#   ifdef __cplusplus
extern "C" {
#   endif
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif


#   if defined (_IO_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif

EXTERN void IO_Read (char *ch);
EXTERN void IO_Write (char ch);
EXTERN void IO_Error (char ch);

/*
   UnBufferedMode - places file descriptor, fd, into an unbuffered mode.
*/

EXTERN void IO_UnBufferedMode (int fd, unsigned int input);

/*
   BufferedMode - places file descriptor, fd, into a buffered mode.
*/

EXTERN void IO_BufferedMode (int fd, unsigned int input);

/*
   EchoOn - turns on echoing for file descriptor, fd.  This
            only really makes sence for a file descriptor opened
            for terminal input or maybe some specific file descriptor
            which is attached to a particular piece of hardware.
*/

EXTERN void IO_EchoOn (int fd, unsigned int input);

/*
   EchoOff - turns off echoing for file descriptor, fd.  This
             only really makes sence for a file descriptor opened
             for terminal input or maybe some specific file descriptor
             which is attached to a particular piece of hardware.
*/

EXTERN void IO_EchoOff (int fd, unsigned int input);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
