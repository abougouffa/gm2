/* Output from p2c, the Pascal-to-C translator */
/* From input file "StdIO.mod" */


#include <p2c/p2c.h>


#define StdIOG
#include "StdIO.h"


#ifndef IOH
#include "IO.h"
#endif

#ifndef M2RTSH
#include "M2RTS.h"
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
/* p2c: Note: Remember to call _StdIO_init() in main program [215] */



/* End. */
