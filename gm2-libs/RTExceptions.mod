(* Copyright (C) 2008 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA *)

IMPLEMENTATION MODULE RTExceptions ;

FROM ASCII IMPORT nul ;
FROM StrLib IMPORT StrLen ;
FROM Storage IMPORT ALLOCATE ;
FROM SYSTEM IMPORT ADDRESS, ADR ;
FROM libc IMPORT write ;

CONST
   MaxBuffer = 4096 ;

TYPE
   Handler = POINTER TO handler ;  (* to help p2c *)
   handler =            RECORD
                           p    : ProcedureHandler ;
                           n    : CARDINAL ;
                           right,
                           left,
                           stack: Handler ;
                        END ;

   EHBlock = POINTER TO ehblock ;  (* to help p2c *)
   ehblock =            RECORD
                           buffer  : ARRAY [0..MaxBuffer] OF CHAR ;
                           number  : CARDINAL ;
                           handlers: Handler ;
                           right   : EHBlock ;
                        END ;

   PtrToChar = POINTER TO CHAR ;

(* %%%FORWARD%%%
PROCEDURE NewHandler () : Handler ; FORWARD ;
PROCEDURE KillHandlers (h: Handler) : Handler ; FORWARD ;
   %%%FORWARD%%% *)

VAR
   freeHandler: Handler ;
   freeEHB,
   currentEHB : EHBlock ;


(*
   ErrorString - writes a string to stderr.
*)

PROCEDURE ErrorString (a: ARRAY OF CHAR) ;
VAR
   n: INTEGER ;
BEGIN
   n := write(2, ADR(a), StrLen(a))
END ErrorString ;


(*
   findHandler - 
*)

PROCEDURE findHandler (e: EHBlock; number: CARDINAL) : Handler ;
VAR
   h: Handler ;
BEGIN
   h := e^.handlers^.right ;
   WHILE (h#e^.handlers) AND (number#h^.n) DO
      h := h^.right
   END ;
   IF h=e^.handlers
   THEN
      RETURN( NIL )
   ELSE
      RETURN( h )
   END
END findHandler ;


(*
   InvokeHandler - invokes the associated handler for the current
                   exception in the active EHB.
*)

PROCEDURE InvokeHandler ;
VAR
   h: Handler ;
BEGIN
   h := findHandler(currentEHB, currentEHB^.number) ;
   IF h=NIL
   THEN
      ErrorString(currentEHB^.buffer) ;
      HALT
   ELSE
      h^.p
   END
END InvokeHandler ;


(*
   addChar - adds, ch, to the current exception handler text buffer
             at index, i.  The index in then incremented.
*)

PROCEDURE addChar (ch: CHAR; VAR i: CARDINAL) ;
BEGIN
   IF (i<=MaxBuffer) AND (currentEHB#NIL)
   THEN
      currentEHB^.buffer[i] := ch ;
      INC(i)
   END
END addChar ;


(*
   addStr - adds a C string from address, s, into the current
            handler text buffer.
*)

PROCEDURE addStr (s: ADDRESS; VAR i: CARDINAL) ;
VAR
   p: PtrToChar ;
BEGIN
   p := s ;
   WHILE (p#NIL) AND (p^#nul) DO
      addChar(p^, i) ;
      INC(p)
   END
END addStr ;


(*
   addNum - adds a number, n, to the current handler
            text buffer.
*)

PROCEDURE addNum (n: CARDINAL; VAR i: CARDINAL) ;
BEGIN
   IF n<10
   THEN
      addChar(CHR(n MOD 10 + ORD('0')), i)
   ELSE
      addNum(n DIV 10, i) ;
      addNum(n MOD 10, i)
   END
END addNum ;


(*
   Raise - invoke the exception handler associated with, number,
           in the active EHBlock.  It keeps a record of the number
           and message in the EHBlock for later use.
*)

PROCEDURE Raise (number: CARDINAL;
                 file: ADDRESS; line: CARDINAL;
                 column: CARDINAL; function: ADDRESS;
                 message: ADDRESS) ;
VAR
   i: CARDINAL ;
BEGIN
   currentEHB^.number := number ;
   i := 0 ;
   addStr(file, i) ;
   addChar(':', i) ;
   addNum(line, i) ;
   addChar(':', i) ;
   addNum(column, i) ;
   addChar(':', i) ;
   addStr(function, i) ;
   addChar(':', i) ;
   addStr(message, i) ;
   InvokeHandler
END Raise ;


(*
   SetExceptionBlock - sets, source, as the active EHB.
*)

PROCEDURE SetExceptionBlock (source: EHBlock) ;
BEGIN
   currentEHB := source
END SetExceptionBlock ;


(*
   GetExceptionBlock - returns the active EHB.
*)

PROCEDURE GetExceptionBlock () : EHBlock ;
BEGIN
   RETURN( currentEHB )
END GetExceptionBlock ;


(*
   GetTextBuffer - returns the address of the EHB buffer.
*)

PROCEDURE GetTextBuffer (e: EHBlock) : ADDRESS ;
BEGIN
   RETURN( ADR(e^.buffer) )
END GetTextBuffer ;


(*
   GetTextBufferSize - return the size of the EHB text buffer.
*)

PROCEDURE GetTextBufferSize (e: EHBlock) : CARDINAL ;
BEGIN
   RETURN SIZE(e^.buffer)
END GetTextBufferSize ;


(*
   GetNumber - return the exception number associated with,
               source.
*)

PROCEDURE GetNumber (source: EHBlock) : CARDINAL ;
BEGIN
   RETURN( source^.number )
END GetNumber ;


(*
   New - returns a new EHBlock.
*)

PROCEDURE New () : EHBlock ;
VAR
   e: EHBlock ;
BEGIN
   IF freeEHB=NIL
   THEN
      NEW(e)
   ELSE
      e := freeEHB ;
      freeEHB := freeEHB^.right
   END ;
   RETURN( e )
END New ;


(*
   InitExceptionBlock - creates and returns a new exception block.
*)

PROCEDURE InitExceptionBlock () : EHBlock ;
VAR
   e: EHBlock ;
BEGIN
   e := New() ;
   WITH e^ DO
      number := MAX(CARDINAL) ;
      handlers := NewHandler() ;   (* add the dummy onto the head *)
      right := NIL
   END ;
   RETURN( e )
END InitExceptionBlock ;


(*
   KillExceptionBlock - destroys the EHB, e, and all its handlers.
*)

PROCEDURE KillExceptionBlock (e: EHBlock) : EHBlock ;
BEGIN
   e^.handlers := KillHandlers(e^.handlers) ;
   e^.right := freeEHB ;
   freeEHB := e ;
   RETURN( NIL )
END KillExceptionBlock ;


(*
   NewHandler - returns a new handler.
*)

PROCEDURE NewHandler () : Handler ;
VAR
   h: Handler ;
BEGIN
   IF freeHandler=NIL
   THEN
      NEW(h)
   ELSE
      h := freeHandler ;
      freeHandler := freeHandler^.right
   END ;
   RETURN( h )
END NewHandler ;


(*
   KillHandler - returns, NIL, and places, h, onto the free list.
*)

PROCEDURE KillHandler (h: Handler) : Handler ;
BEGIN
   h^.right := freeHandler ;
   freeHandler := h ;
   RETURN( NIL )
END KillHandler ;


(*
   KillHandlers - kills all handlers in the list.
*)

PROCEDURE KillHandlers (h: Handler) : Handler ;
BEGIN
   h^.left^.right := freeHandler ;
   freeHandler := h ;
   RETURN( NIL )
END KillHandlers ;


(*
   InitHandler - 
*)

PROCEDURE InitHandler (h: Handler; l, r, s: Handler; number: CARDINAL; proc: ProcedureHandler) : Handler ;
BEGIN
   WITH h^ DO
      p := proc ;
      n := number ;
      right := r ;
      left := l ;
      stack := s
   END ;
   RETURN( h )
END InitHandler ;


(*
   PushHandler - install a handler in EHB, e.
*)

PROCEDURE PushHandler (e: EHBlock; number: CARDINAL; p: ProcedureHandler) ;
VAR
   h: Handler ;
BEGIN
   h := findHandler(e, number) ;
   IF h#NIL
   THEN
      (* remove, h, *)
      h^.right^.left := h^.left ;
      h^.left^.right := h^.right
   END ;
   e^.handlers := InitHandler(NewHandler(), e^.handlers, e^.handlers^.right, h, number, p)
END PushHandler ;


(*
   PopHandler - removes the handler associated with, number, from
                EHB, e.
*)

PROCEDURE PopHandler (e: EHBlock; number: CARDINAL) ;
VAR
   h, i: Handler ;
BEGIN
   h := findHandler(e, number) ;
   IF h#NIL
   THEN
      (* remove, h, *)
      h^.right^.left := h^.left ;
      h^.left^.right := h^.right ;
      i := h^.stack ;
      h := KillHandler(h) ;
      e^.handlers := InitHandler(i, e^.handlers, e^.handlers^.right, i^.stack, number, i^.p)
   END
END PopHandler ;


(*
   Init - initialises this module.
*)

PROCEDURE Init ;
BEGIN
   freeHandler := NIL ;
   freeEHB := NIL ;
   currentEHB := NIL
END Init ;


BEGIN
   Init
END RTExceptions.
