/* do not edit automatically generated by mc from PushBackInput.  */
/* PushBackInput.mod provides a method for pushing back and consuming input.

Copyright (C) 2001-2019 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   if !defined (FALSE)
#      define FALSE (1==0)
#   endif

#include <stddef.h>
#include <string.h>
#include <limits.h>
#define _PushBackInput_H
#define _PushBackInput_C

#   include "GFIO.h"
#   include "GDynamicStrings.h"
#   include "GASCII.h"
#   include "GDebug.h"
#   include "GStrLib.h"
#   include "GNumberIO.h"
#   include "GStrIO.h"
#   include "GStdIO.h"
#   include "Glibc.h"

#   define MaxPushBackStack 8192
#   define MaxFileName 4096
typedef struct _T1_a _T1;

typedef struct _T2_a _T2;

struct _T1_a { char array[MaxFileName+1]; };
struct _T2_a { char array[MaxPushBackStack+1]; };
static _T1 FileName;
static _T2 CharStack;
static unsigned int ExitStatus;
static unsigned int Column;
static unsigned int StackPtr;
static unsigned int LineNo;
static unsigned int Debugging;

/*
   Open - opens a file for reading.
*/

FIO_File PushBackInput_Open (char *a_, unsigned int _a_high);

/*
   GetCh - gets a character from either the push back stack or
           from file, f.
*/

char PushBackInput_GetCh (FIO_File f);

/*
   PutCh - pushes a character onto the push back stack, it also
           returns the character which has been pushed.
*/

char PushBackInput_PutCh (FIO_File f, char ch);

/*
   PutString - pushes a string onto the push back stack., it also
*/

void PushBackInput_PutString (FIO_File f, char *a_, unsigned int _a_high);

/*
   Error - emits an error message with the appropriate file, line combination.
*/

void PushBackInput_Error (char *a_, unsigned int _a_high);

/*
   WarnError - emits an error message with the appropriate file, line combination.
               It does not terminate but when the program finishes an exit status of
               1 will be issued.
*/

void PushBackInput_WarnError (char *a_, unsigned int _a_high);

/*
   WarnString - emits an error message with the appropriate file, line combination.
                It does not terminate but when the program finishes an exit status of
                1 will be issued.
*/

void PushBackInput_WarnString (DynamicStrings_String s);

/*
   Close - closes the opened file.
*/

void PushBackInput_Close (FIO_File f);

/*
   GetExitStatus - returns the exit status which will be 1 if any warnings were issued.
*/

unsigned int PushBackInput_GetExitStatus (void);

/*
   SetDebug - sets the debug flag on or off.
*/

void PushBackInput_SetDebug (unsigned int d);

/*
   GetColumnPosition - returns the column position of the current character.
*/

unsigned int PushBackInput_GetColumnPosition (void);

/*
   GetCurrentLine - returns the current line number.
*/

unsigned int PushBackInput_GetCurrentLine (void);

/*
   ErrChar - writes a char, ch, to stderr.
*/

static void ErrChar (char ch);

/*
   Init - initialize global variables.
*/

static void Init (void);


/*
   ErrChar - writes a char, ch, to stderr.
*/

static void ErrChar (char ch)
{
  FIO_WriteChar (FIO_StdErr, ch);
}


/*
   Init - initialize global variables.
*/

static void Init (void)
{
  ExitStatus = 0;
  StackPtr = 0;
  LineNo = 1;
  Column = 0;
}


/*
   Open - opens a file for reading.
*/

FIO_File PushBackInput_Open (char *a_, unsigned int _a_high)
{
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  Init ();
  StrLib_StrCopy ((char *) a, _a_high, (char *) &FileName.array[0], MaxFileName);
  return FIO_OpenToRead ((char *) a, _a_high);
}


/*
   GetCh - gets a character from either the push back stack or
           from file, f.
*/

char PushBackInput_GetCh (FIO_File f)
{
  char ch;

  if (StackPtr > 0)
    {
      StackPtr -= 1;
      if (Debugging)
        {
          StdIO_Write (CharStack.array[StackPtr]);
        }
      return CharStack.array[StackPtr];
    }
  else
    {
      if ((FIO_EOF (f)) || (! (FIO_IsNoError (f))))
        {
          ch = ASCII_nul;
        }
      else
        {
          do {
            ch = FIO_ReadChar (f);
          } while (! (((ch != ASCII_cr) || (FIO_EOF (f))) || (! (FIO_IsNoError (f)))));
          if (ch == ASCII_lf)
            {
              Column = 0;
              LineNo += 1;
            }
          else
            {
              Column += 1;
            }
        }
      if (Debugging)
        {
          StdIO_Write (ch);
        }
      return ch;
    }
}


/*
   PutCh - pushes a character onto the push back stack, it also
           returns the character which has been pushed.
*/

char PushBackInput_PutCh (FIO_File f, char ch)
{
  if (StackPtr < MaxPushBackStack)
    {
      CharStack.array[StackPtr] = ch;
      StackPtr += 1;
    }
  else
    {
      Debug_Halt ((char *) "max push back stack exceeded, increase MaxPushBackStack", 55, 130, (char *) "../../gcc-versionno/gcc/m2/gm2-libs/PushBackInput.mod", 53);
    }
  return ch;
}


/*
   PutString - pushes a string onto the push back stack., it also
*/

void PushBackInput_PutString (FIO_File f, char *a_, unsigned int _a_high)
{
  unsigned int l;
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  l = StrLib_StrLen ((char *) a, _a_high);
  while (l > 0)
    {
      l -= 1;
      if ((PushBackInput_PutCh (f, a[l])) != a[l])
        {
          Debug_Halt ((char *) "assert failed", 13, 112, (char *) "../../gcc-versionno/gcc/m2/gm2-libs/PushBackInput.mod", 53);
        }
    }
}


/*
   Error - emits an error message with the appropriate file, line combination.
*/

void PushBackInput_Error (char *a_, unsigned int _a_high)
{
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  StdIO_PushOutput ((StdIO_ProcWrite) {(StdIO_ProcWrite_t) ErrChar});
  StrIO_WriteString ((char *) &FileName.array[0], MaxFileName);
  StdIO_Write (':');
  NumberIO_WriteCard (LineNo, 0);
  StdIO_Write (':');
  StrIO_WriteString ((char *) a, _a_high);
  StrIO_WriteLn ();
  StdIO_PopOutput ();
  FIO_Close (FIO_StdErr);
  libc_exit (1);
}


/*
   WarnError - emits an error message with the appropriate file, line combination.
               It does not terminate but when the program finishes an exit status of
               1 will be issued.
*/

void PushBackInput_WarnError (char *a_, unsigned int _a_high)
{
  char a[_a_high+1];

  /* make a local copy of each unbounded array.  */
  memcpy (a, a_, _a_high+1);

  StdIO_PushOutput ((StdIO_ProcWrite) {(StdIO_ProcWrite_t) ErrChar});
  StrIO_WriteString ((char *) &FileName.array[0], MaxFileName);
  StdIO_Write (':');
  NumberIO_WriteCard (LineNo, 0);
  StdIO_Write (':');
  StrIO_WriteString ((char *) a, _a_high);
  StrIO_WriteLn ();
  StdIO_PopOutput ();
  ExitStatus = 1;
}


/*
   WarnString - emits an error message with the appropriate file, line combination.
                It does not terminate but when the program finishes an exit status of
                1 will be issued.
*/

void PushBackInput_WarnString (DynamicStrings_String s)
{
  char * p;

  p = DynamicStrings_string (s);
  StrIO_WriteString ((char *) &FileName.array[0], MaxFileName);
  StdIO_Write (':');
  NumberIO_WriteCard (LineNo, 0);
  StdIO_Write (':');
  do {
    if (p != NULL)
      {
        if ((*p) == ASCII_lf)
          {
            StrIO_WriteLn ();
            StrIO_WriteString ((char *) &FileName.array[0], MaxFileName);
            StdIO_Write (':');
            NumberIO_WriteCard (LineNo, 0);
            StdIO_Write (':');
          }
        else
          {
            StdIO_Write ((*p));
          }
        p += 1;
      }
  } while (! ((p == NULL) || ((*p) == ASCII_nul)));
  ExitStatus = 1;
}


/*
   Close - closes the opened file.
*/

void PushBackInput_Close (FIO_File f)
{
  FIO_Close (f);
}


/*
   GetExitStatus - returns the exit status which will be 1 if any warnings were issued.
*/

unsigned int PushBackInput_GetExitStatus (void)
{
  return ExitStatus;
}


/*
   SetDebug - sets the debug flag on or off.
*/

void PushBackInput_SetDebug (unsigned int d)
{
  Debugging = d;
}


/*
   GetColumnPosition - returns the column position of the current character.
*/

unsigned int PushBackInput_GetColumnPosition (void)
{
  if (StackPtr > Column)
    {
      return 0;
    }
  else
    {
      return Column-StackPtr;
    }
}


/*
   GetCurrentLine - returns the current line number.
*/

unsigned int PushBackInput_GetCurrentLine (void)
{
  return LineNo;
}

void _M2_PushBackInput_init (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
  PushBackInput_SetDebug (FALSE);
  Init ();
}

void _M2_PushBackInput_finish (__attribute__((unused)) int argc, __attribute__((unused)) char *argv[])
{
}