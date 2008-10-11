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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
MA  02110-1301  USA *)

IMPLEMENTATION MODULE STextIO;

IMPORT StdChans, TextIO ;

  (* Input and output of character and string types over default channels. The read result
     is of the type IOConsts.ReadResults.
  *)

  (* The following procedures do not read past line marks *)

PROCEDURE ReadChar (VAR ch: CHAR);
  (* If possible, removes a character from the default input
     stream, and assigns the corresponding value to ch.
     The read result is set to allRight, endOfLine or endOfInput.
  *)
BEGIN
   TextIO.ReadChar(StdChans.StdInChan(), ch)
END ReadChar ;


PROCEDURE ReadRestLine (VAR s: ARRAY OF CHAR);
  (* Removes any remaining characters from the default input
     stream before the next line mark, copying to s as many
     as can be accommodated as a string value.  The read result
     is set to the value allRight, outOfRange, endOfLine, or
     endOfInput.
  *)
BEGIN
   TextIO.ReadRestLine(StdChans.StdInChan(), s)
END ReadRestLine ;


PROCEDURE ReadString (VAR s: ARRAY OF CHAR);
  (* Removes only those characters from the default input stream
     before the next line mark that can be accommodated in s as
     a string value, and copies them to s. The read result
     is set to the value allRight, endOfLine, or endOfInput.
  *)
BEGIN
   TextIO.ReadString(StdChans.StdInChan(), s)
END ReadString ;


PROCEDURE ReadToken (VAR s: ARRAY OF CHAR);
  (* Skips leading spaces, and then removes characters from the
     default input stream before the next space or line mark,
     copying to s as many as can be accommodated as a string
     value.  The read result is set to the value allRight,
     outOfRange, endOfLine, or endOfInput.
  *)
BEGIN
   TextIO.ReadToken(StdChans.StdInChan(), s)
END ReadToken ;


  (* The following procedure reads past the next line mark *)

PROCEDURE SkipLine;
  (* Removes successive items from the default input stream up
     to and including the next line mark or until the end of
     input is reached. The read result is set to the value
     allRight, or endOfInput.
  *)
BEGIN
   TextIO.SkipLine(StdChans.StdInChan())
END SkipLine ;


  (* Output procedures *)

PROCEDURE WriteChar (ch: CHAR);
  (* Writes the value of ch to the default output stream. *)
BEGIN
   TextIO.WriteChar(StdChans.StdOutChan(), ch)
END WriteChar ;


PROCEDURE WriteLn;
  (* Writes a line mark to the default output stream. *)
BEGIN
   TextIO.WriteLn(StdChans.StdOutChan())
END WriteLn ;


PROCEDURE WriteString (s: ARRAY OF CHAR);
  (* Writes the string value of s to the default output stream. *)
BEGIN
   TextIO.WriteString(StdChans.StdOutChan(), s)
END WriteString ;


END STextIO.
