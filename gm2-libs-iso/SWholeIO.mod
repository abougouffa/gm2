(* Copyright (C) 2009 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE SWholeIO;

IMPORT StdChans, WholeIO ;

  (* Input and output of whole numbers in decimal text form over
     default channels. The read result is of the type
     IOConsts.ReadResults.
  *)

PROCEDURE ReadInt (VAR int: INTEGER);
  (* Skips leading spaces, and removes any remaining characters
     from the default input stream that form part of a signed
     whole number.  The value of this number is assigned to int.
     The read result is set to the value allRight, outOfRange,
     wrongFormat, endOfLine, or endOfInput.
  *)
BEGIN
   WholeIO.ReadInt(StdChans.StdInChan(), int)
END ReadInt ;


PROCEDURE ReadCard (VAR card: CARDINAL);
  (* Skips leading spaces, and removes any remaining characters
     from the default input stream that form part of an unsigned
     whole number.  The value of this number is assigned to card.
     The read result is set to the value allRight, outOfRange,
     wrongFormat, endOfLine, or endOfInput.
  *)
BEGIN
   WholeIO.ReadCard(StdChans.StdInChan(), card)
END ReadCard ;


  (* Output procedures *)

PROCEDURE WriteInt (int: INTEGER; width: CARDINAL);
  (* Writes the value of int to the default output stream in
     text form, in a field of the given minimum width. *)
BEGIN
   WholeIO.WriteInt(StdChans.StdOutChan(), int, width)
END WriteInt ;


PROCEDURE WriteCard (card: CARDINAL; width: CARDINAL);
  (* Writes the value of card to the default output stream in
     text form, in a field of the given minimum width. *)
BEGIN
   WholeIO.WriteCard(StdChans.StdOutChan(), card, width)
END WriteCard ;


END SWholeIO.
