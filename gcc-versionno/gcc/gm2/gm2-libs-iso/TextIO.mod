(* Copyright (C) 2008, 2009, 2010
                 Free Software Foundation, Inc. *)
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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
MA  02110-1301  USA *)

IMPLEMENTATION MODULE TextIO ;


IMPORT IOChan, IOConsts, CharClass, ASCII ;
FROM SYSTEM IMPORT ADR ;

  (* The following procedures do not read past line marks *)

PROCEDURE CanRead (cid: IOChan.ChanId) : BOOLEAN ;
BEGIN
   RETURN( (IOChan.ReadResult(cid)=IOConsts.notKnown) OR
           (IOChan.ReadResult(cid)=IOConsts.allRight) )
END CanRead ;

PROCEDURE WasGoodChar (cid: IOChan.ChanId) : BOOLEAN ;
BEGIN
   RETURN( (IOChan.ReadResult(cid)#IOConsts.endOfLine) AND
           (IOChan.ReadResult(cid)#IOConsts.endOfInput) )
END WasGoodChar ;


(*
   SetResult - assigns the result in cid.
               If s is empty then leave as endOfInput
                  or endOfLine
               If s is not empty then assign allRight
               If range and i exceeds, h, then assign outOfRange
*)

PROCEDURE SetResult (cid: IOChan.ChanId; i: CARDINAL;
                     VAR s: ARRAY OF CHAR; range: BOOLEAN) ;
BEGIN
   IF i<=HIGH(s)
   THEN
      s[i] := ASCII.nul ;
      IF i>0
      THEN
         IOChan.SetReadResult(cid, IOConsts.allRight)
      END
   ELSIF range
   THEN
      IOChan.SetReadResult(cid, IOConsts.outOfRange)
   END
END SetResult ;


PROCEDURE ReadChar (cid: IOChan.ChanId; VAR ch: CHAR);
  (* If possible, removes a character from the input stream
     cid and assigns the corresponding value to ch.  The
     read result is set to the value allRight, endOfLine, or
     endOfInput.
  *)
VAR
   res: IOConsts.ReadResults ;
BEGIN
   IF CanRead(cid)
   THEN
      IOChan.Look(cid, ch, res) ;
      IF res=IOConsts.allRight
      THEN
         IOChan.Skip(cid)
      END
   END
END ReadChar ;

PROCEDURE ReadRestLine (cid: IOChan.ChanId; VAR s: ARRAY OF CHAR);
  (* Removes any remaining characters from the input stream
     cid before the next line mark,  copying to s as many as
     can be accommodated as a string value.  The read result is
     set to the value allRight, outOfRange, endOfLine, or
     endOfInput.
  *)
VAR
   i, h    : CARDINAL ;
   finished: BOOLEAN ;
BEGIN
   h := HIGH(s) ;
   i := 0 ;
   finished := FALSE ;
   WHILE (i<=h) AND CanRead(cid) AND (NOT finished) DO
      ReadChar(cid, s[i]) ;
      IF WasGoodChar(cid)
      THEN
         INC(i)
      ELSE
         finished := TRUE
      END
   END ;
   WHILE CanRead(cid) DO
      IOChan.Skip(cid)
   END ;
   SetResult(cid, i, s, TRUE)
END ReadRestLine ;

PROCEDURE ReadString (cid: IOChan.ChanId; VAR s: ARRAY OF CHAR);
  (* Removes only those characters from the input stream cid
     before the next line mark that can be accommodated in s
     as a string value, and copies them to s.  The read result
     is set to the value allRight, endOfLine, or endOfInput.
  *)
VAR
   i, h    : CARDINAL ;
   finished: BOOLEAN ;
BEGIN
   h := HIGH(s) ;
   i := 0 ;
   finished := FALSE ;
   WHILE (i<=h) AND CanRead(cid) AND (NOT finished) DO
      ReadChar(cid, s[i]) ;
      IF WasGoodChar(cid)
      THEN
         INC(i)
      ELSE
         finished := TRUE
      END
   END ;
   SetResult(cid, i, s, FALSE)
END ReadString ;

PROCEDURE ReadToken (cid: IOChan.ChanId; VAR s: ARRAY OF CHAR);
  (* Skips leading spaces, and then removes characters from
     the input stream cid before the next space or line mark,
     copying to s as many as can be accommodated as a string
     value.  The read result is set to the value allRight,
     outOfRange, endOfLine, or endOfInput.
  *)
VAR
   i, h: CARDINAL ;
BEGIN
   h := HIGH(s) ;
   i := 0 ;
   WHILE (i<=h) AND CanRead(cid) DO
      ReadChar(cid, s[i]) ;
      IF CharClass.IsWhiteSpace(s[i])
      THEN
         SetResult(cid, i, s, TRUE) ;
         RETURN
      END ;
      INC(i)
   END ;
   SetResult(cid, i, s, TRUE)
END ReadToken ;

  (* The following procedure reads past the next line mark *)

PROCEDURE SkipLine (cid: IOChan.ChanId);
  (* Removes successive items from the input stream cid up
     to and including the next line mark, or until the end
     of input is reached.  The read result is set to the
     value allRight, or endOfInput.
  *)
VAR
   ch : CHAR ;
   res: IOConsts.ReadResults ;
BEGIN
   IOChan.Look(cid, ch, res) ;
   WHILE res=IOConsts.allRight DO
      IOChan.SkipLook(cid, ch, res)
   END ;
   IF res=IOConsts.endOfLine
   THEN
      IOChan.Skip(cid)
   END
END SkipLine ;

  (* Output procedures *)

PROCEDURE WriteChar (cid: IOChan.ChanId; ch: CHAR);
  (* Writes the value of ch to the output stream cid. *)
BEGIN
   IOChan.TextWrite(cid, ADR(ch), SIZE(ch))
END WriteChar ;

PROCEDURE WriteLn (cid: IOChan.ChanId);
  (* Writes a line mark to the output stream cid. *)
BEGIN
   IOChan.WriteLn(cid)
END WriteLn ;

PROCEDURE WriteString (cid: IOChan.ChanId; s: ARRAY OF CHAR);
  (* Writes the string value in s to the output stream cid. *)
BEGIN
   IOChan.TextWrite(cid, ADR(s), LENGTH(s))
END WriteString ;


END TextIO.