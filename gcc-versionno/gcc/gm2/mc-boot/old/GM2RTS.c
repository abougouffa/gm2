/* automatically created by mc from ../../gcc-5.2.0/gcc/gm2/gm2-libs/M2RTS.mod.  */

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

#include <string.h>
#include <limits.h>
#define _M2RTS_H
#define _M2RTS_C

#   include "Glibc.h"
#   include "GNumberIO.h"
#   include "GStrLib.h"
#   include "GSYSTEM.h"
#   include "GASCII.h"
#   include "GRTExceptions.h"
#   include "GM2EXCEPTION.h"

#   define MaxProcedures 1024
typedef struct _T1_a _T1;

struct _T1_a { PROC array[MaxProcedures+1]; };
static unsigned int iPtr;
static unsigned int tPtr;
static _T1 InitialProc;
static _T1 TerminateProc;
static int ExitValue;
static unsigned int isHalting;
static unsigned int CallExit;
void M2RTS_ExecuteTerminationProcedures (void);
unsigned int M2RTS_InstallTerminationProcedure (PROC p);
void M2RTS_ExecuteInitialProcedures (void);
unsigned int M2RTS_InstallInitialProcedure (PROC p);
void M2RTS_Terminate (void);
void M2RTS_HALT (int exitcode);
void M2RTS_Halt (char *file_, unsigned int _file_high, unsigned int line, char *function_, unsigned int _function_high, char *description_, unsigned int _description_high);
void M2RTS_ExitOnHalt (int e);
void M2RTS_ErrorMessage (char *message_, unsigned int _message_high, char *file_, unsigned int _file_high, unsigned int line, char *function_, unsigned int _function_high);
unsigned int M2RTS_Length (char *a_, unsigned int _a_high);
void M2RTS_AssignmentException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_IncException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_DecException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_InclException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_ExclException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_ShiftException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_RotateException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_StaticArraySubscriptException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_DynamicArraySubscriptException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_ForLoopBeginException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_ForLoopToException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_ForLoopEndException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_PointerNilException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_NoReturnException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_CaseException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_WholeNonPosDivException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_WholeNonPosModException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_WholeZeroDivException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_WholeZeroRemException (void * filename, unsigned int line, unsigned int column, void * scope);
void M2RTS_NoException (void * filename, unsigned int line, unsigned int column, void * scope);
static void ErrorString (char *a_, unsigned int _a_high);

static void ErrorString (char *a_, unsigned int _a_high)
{
  int n;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high);

  n = libc_write (2, &a, (int ) StrLib_StrLen ((char *) a, _a_high));
}

void M2RTS_ExecuteTerminationProcedures (void)
{
  unsigned int i;

  i = tPtr;
  while (i > 0)
    {
      i -= 1;
      (*TerminateProc.array[i].proc) ();
    }
}

unsigned int M2RTS_InstallTerminationProcedure (PROC p)
{
  if (tPtr > MaxProcedures)
    return FALSE;
  else
    {
      TerminateProc.array[tPtr] = p;
      tPtr += 1;
      return TRUE;
    }
}

void M2RTS_ExecuteInitialProcedures (void)
{
  unsigned int i;

  i = iPtr;
  while (i > 0)
    {
      i -= 1;
      (*InitialProc.array[i].proc) ();
    }
}

unsigned int M2RTS_InstallInitialProcedure (PROC p)
{
  if (iPtr > MaxProcedures)
    return FALSE;
  else
    {
      InitialProc.array[iPtr] = p;
      iPtr += 1;
      return TRUE;
    }
}

void M2RTS_Terminate (void)
{
  libc_exit (ExitValue);
}

void M2RTS_HALT (int exitcode)
{
  if (exitcode != -1)
    {
      CallExit = TRUE;
      ExitValue = exitcode;
    }
  if (isHalting)
    libc_exit (-1);
  else
    {
      isHalting = TRUE;
      M2RTS_ExecuteTerminationProcedures ();
    }
  if (CallExit)
    libc_exit (ExitValue);
  else
    libc_abort ();
}

void M2RTS_Halt (char *file_, unsigned int _file_high, unsigned int line, char *function_, unsigned int _function_high, char *description_, unsigned int _description_high)
{
  char file[_file_high+1];
  char function[_function_high+1];
  char description[_description_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (file, file_, _file_high);
  memcpy (function, function_, _function_high);
  memcpy (description, description_, _description_high);

  M2RTS_ErrorMessage ((char *) description, _description_high, (char *) file, _file_high, line, (char *) function, _function_high);
  M2RTS_HALT (0);
}

void M2RTS_ExitOnHalt (int e)
{
  ExitValue = e;
  CallExit = TRUE;
}

void M2RTS_ErrorMessage (char *message_, unsigned int _message_high, char *file_, unsigned int _file_high, unsigned int line, char *function_, unsigned int _function_high)
{
  typedef struct _T2_a _T2;

  struct _T2_a { char array[10+1]; };
  _T2 LineNo;
  char message[_message_high+1];
  char file[_file_high+1];
  char function[_function_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (message, message_, _message_high);
  memcpy (file, file_, _file_high);
  memcpy (function, function_, _function_high);

  ErrorString ((char *) file, _file_high);
  ErrorString ((char *) ":", 1);
  NumberIO_CardToStr (line, 0, (char *) &LineNo.array[0], 10);
  ErrorString ((char *) &LineNo.array[0], 10);
  ErrorString ((char *) ":", 1);
  if (StrLib_StrEqual ((char *) function, _function_high, (char *) "", 0))
    {
      ErrorString ((char *) "in ", 3);
      ErrorString ((char *) function, _function_high);
      ErrorString ((char *) " has caused ", 12);
    }
  ErrorString ((char *) message, _message_high);
  LineNo.array[0] = ASCII_nl;
  LineNo.array[1] = ASCII_nul;
  ErrorString ((char *) &LineNo.array[0], 10);
  libc_exit (1);
}

unsigned int M2RTS_Length (char *a_, unsigned int _a_high)
{
  unsigned int l;
  unsigned int h;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high);

  l = 0;
  h = _a_high;
  while ((l <= h) && (a[l] != ASCII_nul))
    l += 1;
  return l;
}

void M2RTS_AssignmentException (void * filename, unsigned int line, unsigned int column, void * scope)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_rangeException), filename, line, column, scope, "variable exceeds range during assignment");
}

void M2RTS_IncException (void * filename, unsigned int line, unsigned int column, void * scope)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_rangeException), filename, line, column, scope, "variable exceeds range during INC statement");
}

void M2RTS_DecException (void * filename, unsigned int line, unsigned int column, void * scope)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_rangeException), filename, line, column, scope, "variable exceeds range during DEC statement");
}

void M2RTS_InclException (void * filename, unsigned int line, unsigned int column, void * scope)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_rangeException), filename, line, column, scope, "bit exceeds set range during INCL statement");
}

void M2RTS_ExclException (void * filename, unsigned int line, unsigned int column, void * scope)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_rangeException), filename, line, column, scope, "bit exceeds set range during EXCL statement");
}

void M2RTS_ShiftException (void * filename, unsigned int line, unsigned int column, void * scope)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_rangeException), filename, line, column, scope, "bit exceeds set range during SHIFT statement");
}

void M2RTS_RotateException (void * filename, unsigned int line, unsigned int column, void * scope)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_rangeException), filename, line, column, scope, "bit exceeds set range during ROTATE statement");
}

void M2RTS_StaticArraySubscriptException (void * filename, unsigned int line, unsigned int column, void * scope)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_indexException), filename, line, column, scope, "array index out of bounds during static array access");
}

void M2RTS_DynamicArraySubscriptException (void * filename, unsigned int line, unsigned int column, void * scope)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_indexException), filename, line, column, scope, "array index out of bounds during dynamic array access");
}

void M2RTS_ForLoopBeginException (void * filename, unsigned int line, unsigned int column, void * scope)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_rangeException), filename, line, column, scope, "iterator variable exceeds range during FOR loop initial assignment");
}

void M2RTS_ForLoopToException (void * filename, unsigned int line, unsigned int column, void * scope)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_rangeException), filename, line, column, scope, "iterator variable will exceed range when calculating final value in FOR loop");
}

void M2RTS_ForLoopEndException (void * filename, unsigned int line, unsigned int column, void * scope)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_rangeException), filename, line, column, scope, "iterator variable exceeds range during increment at the end of a FOR loop");
}

void M2RTS_PointerNilException (void * filename, unsigned int line, unsigned int column, void * scope)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_invalidLocation), filename, line, column, scope, "attempting to dereference a NIL valued pointer");
}

void M2RTS_NoReturnException (void * filename, unsigned int line, unsigned int column, void * scope)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_functionException), filename, line, column, scope, "about to finish a PROCEDURE without executing a RETURN statement");
}

void M2RTS_CaseException (void * filename, unsigned int line, unsigned int column, void * scope)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_caseSelectException), filename, line, column, scope, "the expression in the CASE statement cannot be selected");
}

void M2RTS_WholeNonPosDivException (void * filename, unsigned int line, unsigned int column, void * scope)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_wholeDivException), filename, line, column, scope, "the division expression has a divisor which is less than or equal to zero");
}

void M2RTS_WholeNonPosModException (void * filename, unsigned int line, unsigned int column, void * scope)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_wholeDivException), filename, line, column, scope, "the modulus expression has a divisor which is less than or equal to zero");
}

void M2RTS_WholeZeroDivException (void * filename, unsigned int line, unsigned int column, void * scope)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_wholeDivException), filename, line, column, scope, "the division expression has a divisor which is equal to zero");
}

void M2RTS_WholeZeroRemException (void * filename, unsigned int line, unsigned int column, void * scope)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_wholeDivException), filename, line, column, scope, "the remainder expression has a divisor which is equal to zero");
}

void M2RTS_NoException (void * filename, unsigned int line, unsigned int column, void * scope)
{
  RTExceptions_Raise ((unsigned int) (M2EXCEPTION_exException), filename, line, column, scope, "M2Expection was called when no there was no outstanding exception to be returned");
}

void _M2_M2RTS_init (int argc, char *argv[])
{
  iPtr = 0;
  tPtr = 0;
  ExitValue = 0;
  isHalting = FALSE;
  CallExit = FALSE;
}
