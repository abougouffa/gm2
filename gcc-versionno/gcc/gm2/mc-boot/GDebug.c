/* automatically created by mc from ../../gcc-versionno/gcc/gm2/gm2-libs/Debug.mod.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#include <string.h>
#include <limits.h>
#define _Debug_H
#define _Debug_C

#   include "GASCII.h"
#   include "GNumberIO.h"
#   include "GStdIO.h"
#   include "Glibc.h"
#   include "GM2RTS.h"

#   define MaxNoOfDigits 12

/*
   Halt - writes a message in the format:
          Module:Line:Message

          It then terminates by calling HALT.
*/

void Debug_Halt (char *Message_, unsigned int _Message_high, unsigned int LineNo, char *Module_, unsigned int _Module_high);

/*
   DebugString - writes a string to the debugging device (Scn.Write).
                 It interprets 
 as carriage return, linefeed.
*/

void Debug_DebugString (char *a_, unsigned int _a_high);

/*
   WriteLn - writes a carriage return and a newline
             character.
*/

static void WriteLn (void);


/*
   WriteLn - writes a carriage return and a newline
             character.
*/

static void WriteLn (void)
{
  StdIO_Write (ASCII_cr);
  StdIO_Write (ASCII_lf);
}


/*
   Halt - writes a message in the format:
          Module:Line:Message

          It then terminates by calling HALT.
*/

void Debug_Halt (char *Message_, unsigned int _Message_high, unsigned int LineNo, char *Module_, unsigned int _Module_high)
{
  typedef struct _T1_a _T1;

  struct _T1_a { char array[MaxNoOfDigits+1]; };
  _T1 No;
  char Message[_Message_high+1];
  char Module[_Module_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (Message, Message_, _Message_high+1);
  memcpy (Module, Module_, _Module_high+1);

  Debug_DebugString ((char *) Module, _Module_high);
  NumberIO_CardToStr (LineNo, 0, (char *) &No.array[0], MaxNoOfDigits);
  Debug_DebugString ((char *) ":", 1);
  Debug_DebugString ((char *) &No.array[0], MaxNoOfDigits);
  Debug_DebugString ((char *) ":", 1);
  Debug_DebugString ((char *) Message, _Message_high);
  Debug_DebugString ((char *) "\\n", 2);
  M2RTS_HALT (0);
}


/*
   DebugString - writes a string to the debugging device (Scn.Write).
                 It interprets 
 as carriage return, linefeed.
*/

void Debug_DebugString (char *a_, unsigned int _a_high)
{
  unsigned int n;
  unsigned int high;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  high = _a_high;
  n = 0;
  while ((n <= high) && (a[n] != ASCII_nul))
    {
      if (a[n] == '\\')
        {
          /* avoid dangling else.  */
          if ((n+1) <= high)
            {
              /* avoid gcc warning by using compound statement even if not strictly necessary.  */
              if (a[n+1] == 'n')
                {
                  WriteLn ();
                  n += 1;
                }
              else if (a[n+1] == '\\')
                {
                  StdIO_Write ('\\');
                  n += 1;
                }
            }
        }
      else
        StdIO_Write (a[n]);
      n += 1;
    }
}

void _M2_Debug_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}

void _M2_Debug_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}
