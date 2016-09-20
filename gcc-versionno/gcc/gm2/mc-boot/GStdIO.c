/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/StdIO.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#define _StdIO_H
#define _StdIO_C

#   include "GIO.h"
#   include "GM2RTS.h"

typedef struct StdIO_ProcWrite_p StdIO_ProcWrite;

typedef struct StdIO_ProcRead_p StdIO_ProcRead;

#   define MaxStack 40
typedef struct _T1_a _T1;

typedef struct _T2_a _T2;

typedef void (*StdIO_ProcWrite_t) (char);
struct StdIO_ProcWrite_p { StdIO_ProcWrite_t proc; };

typedef void (*StdIO_ProcRead_t) (char *);
struct StdIO_ProcRead_p { StdIO_ProcRead_t proc; };

struct _T1_a { StdIO_ProcWrite array[MaxStack+1]; };
struct _T2_a { StdIO_ProcRead array[MaxStack+1]; };
static _T1 StackW;
static unsigned int StackWPtr;
static _T2 StackR;
static unsigned int StackRPtr;
void StdIO_Read (char *ch);
void StdIO_Write (char ch);
void StdIO_PushOutput (StdIO_ProcWrite p);
void StdIO_PopOutput (void);
StdIO_ProcWrite StdIO_GetCurrentOutput (void);
void StdIO_PushInput (StdIO_ProcRead p);
void StdIO_PopInput (void);
StdIO_ProcRead StdIO_GetCurrentInput (void);

void StdIO_Read (char *ch)
{
  (*StackR.array[StackRPtr].proc) (ch);
}

void StdIO_Write (char ch)
{
  (*StackW.array[StackWPtr].proc) (ch);
}

void StdIO_PushOutput (StdIO_ProcWrite p)
{
  if (StackWPtr == MaxStack)
    M2RTS_HALT (0);
  else
    {
      StackWPtr += 1;
      StackW.array[StackWPtr] = p;
    }
}

void StdIO_PopOutput (void)
{
  if (StackWPtr == 1)
    M2RTS_HALT (0);
  else
    StackWPtr -= 1;
}

StdIO_ProcWrite StdIO_GetCurrentOutput (void)
{
  if (StackWPtr > 0)
    return StackW.array[StackWPtr];
  else
    M2RTS_HALT (0);
}

void StdIO_PushInput (StdIO_ProcRead p)
{
  if (StackRPtr == MaxStack)
    M2RTS_HALT (0);
  else
    {
      StackRPtr += 1;
      StackR.array[StackRPtr] = p;
    }
}

void StdIO_PopInput (void)
{
  if (StackRPtr == 1)
    M2RTS_HALT (0);
  else
    StackRPtr -= 1;
}

StdIO_ProcRead StdIO_GetCurrentInput (void)
{
  if (StackRPtr > 0)
    return StackR.array[StackRPtr];
  else
    M2RTS_HALT (0);
}

void _M2_StdIO_init (int argc, char *argv[])
{
  StackWPtr = 0;
  StackWPtr = 0;
  StdIO_PushOutput ((StdIO_ProcWrite) {(StdIO_ProcWrite_t) IO_Write});
  StdIO_PushInput ((StdIO_ProcRead) {(StdIO_ProcRead_t) IO_Read});
}

void _M2_StdIO_finish (int argc, char *argv[])
{
}
