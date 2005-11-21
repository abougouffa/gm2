/* Copyright (C) 2005 Free Software Foundation, Inc. */
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


#include <p2c/p2c.h>


#define StdIOG
#include "GStdIO.h"


#ifndef IOH
#include "GIO.h"
#endif

#ifndef M2RTSH
#include "GM2RTS.h"
extern void   M2RTS_HALT PV() ATTRIBUTE_NORETURN;
#endif

#define MaxStack        40


Static Void (*Stack[MaxStack + 1]) PP((Char));
Static long StackPtr;


/*
   Read - is the generic procedure that all higher application layers
          should use to receive a character.
*/

Void StdIO_Read(ch)
Char *ch;
{
  IO_Read(ch);
}


/*
   Write - is the generic procedure that all higher application layers
           should use to emit a character.
*/

Void StdIO_Write(ch)
Char ch;
{
  (*Stack[StackPtr])(ch);
}


/*
   PushOutput - pushes the current Write procedure onto a stack,
                any future references to Write will actually invoke
                procedure, p.
*/

Void StdIO_PushOutput(p)
Void (*p) PP((Char));
{
  if (StackPtr == MaxStack)
    _Escape(0);
  StackPtr++;
  Stack[StackPtr] = p;
}


/*
   PopOutput - restores Write to use the previous output procedure.
*/

Void StdIO_PopOutput()
{
  if (StackPtr == 1)
    _Escape(0);
  StackPtr--;
}

/*
   GetCurrentOutput - returns the current output procedure.
*/

/* --fixme-- p2c makes a mistake the function returned should be p(Char) not p(void) */
void (*(StdIO_GetCurrentOutput(void)))(void)
{
  if (StackPtr > 0) {
    return (void *) (Stack[StackPtr]);
  }
  M2RTS_HALT();
}


void _M2_StdIO_init()
{
  Void (*TEMP) PP((char ch));

  static int _was_initialized = 0;
  if (_was_initialized++)
    return;
  StackPtr = 0;
  TEMP = IO_Write;
  StdIO_PushOutput(TEMP);
}
