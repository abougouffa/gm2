/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/Selective.def.  */


#if !defined (_Selective_H)
#   define _Selective_H

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GSYSTEM.h"

#   if defined (_Selective_C)
#      define EXTERN
#   else
#      if defined(__GNUG__)
#         define EXTERN extern "C"
#      else
#         define EXTERN extern
#      endif
#   endif

typedef void *Selective_SetOfFd;

typedef void *Selective_Timeval;

EXTERN int Selective_Select (unsigned int nooffds, Selective_SetOfFd readfds, Selective_SetOfFd writefds, Selective_SetOfFd exceptfds, Selective_Timeval timeout);
EXTERN Selective_Timeval Selective_InitTime (unsigned int sec, unsigned int usec);
EXTERN Selective_Timeval Selective_KillTime (Selective_Timeval t);
EXTERN void Selective_GetTime (Selective_Timeval t, unsigned int *sec, unsigned int *usec);
EXTERN void Selective_SetTime (Selective_Timeval t, unsigned int sec, unsigned int usec);
EXTERN Selective_SetOfFd Selective_InitSet (void);
EXTERN Selective_SetOfFd Selective_KillSet (Selective_SetOfFd s);
EXTERN void Selective_FdZero (Selective_SetOfFd s);
EXTERN void Selective_FdSet (int fd, Selective_SetOfFd s);
EXTERN void Selective_FdClr (int fd, Selective_SetOfFd s);
EXTERN unsigned int Selective_FdIsSet (int fd, Selective_SetOfFd s);
EXTERN int Selective_MaxFdsPlusOne (int a, int b);
EXTERN void Selective_WriteCharRaw (int fd, char ch);
EXTERN char Selective_ReadCharRaw (int fd);
EXTERN int Selective_GetTimeOfDay (Selective_Timeval tv);

#   undef EXTERN
#endif