(* Copyright (C) 2013
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

IMPLEMENTATION MODULE SShortIO ;

IMPORT StdChans, ShortIO ;

  (* Input and output of real numbers in decimal text form over
     default channels.  The read result is of the type
     IOConsts.ReadResults.
  *)

  (* The text form of a signed fixed-point real number is
       ["+" | "-"], decimal digit, {decimal digit},
       [".", {decimal digit}]

     The text form of a signed floating-point real number is
       signed fixed-point real number,
       "E", ["+" | "-"], decimal digit, {decimal digit}
  *)

PROCEDURE ReadReal (VAR real: SHORTREAL);
  (* Skips leading spaces, and removes any remaining characters
     from the default input channel that form part of a signed
     fixed or floating point number. The value of this number
     is assigned to real.  The read result is set to the value
     allRight, outOfRange, wrongFormat, endOfLine, or endOfInput.
  *)
BEGIN
   ShortIO.ReadReal(StdChans.StdInChan(), real)
END ReadReal ;

PROCEDURE WriteFloat (real: SHORTREAL; sigFigs: CARDINAL; width: CARDINAL);
  (* Writes the value of real to the default output channel in
     floating-point text form, with sigFigs significant figures,
     in a field of the given minimum width.
  *)
BEGIN
   ShortIO.WriteFloat(StdChans.StdOutChan(), real, sigFigs, width)
END WriteFloat ;

PROCEDURE WriteEng (real: SHORTREAL; sigFigs: CARDINAL; width: CARDINAL);
  (* As for WriteFloat, except that the number is scaled with one to
     three digits in the whole number part, and with an exponent that
     is a multiple of three.
  *)
BEGIN
   ShortIO.WriteFloat(StdChans.StdOutChan(), real, sigFigs, width)
END WriteEng ;

PROCEDURE WriteFixed (real: SHORTREAL; place: INTEGER; width: CARDINAL);
  (* Writes the value of real to the default output channel in
     fixed-point text form, rounded to the given place relative
     to the decimal point, in a field of the given minimum width.
  *)
BEGIN
   ShortIO.WriteFixed(StdChans.StdOutChan(), real, place, width)
END WriteFixed ;

PROCEDURE WriteReal (real: SHORTREAL; width: CARDINAL);
  (* Writes the value of real to the default output channel, as
     WriteFixed if the sign and magnitude can be shown in the
     given width, or otherwise as WriteFloat. The number of
     places or significant digits depends on the given width.
  *)
BEGIN
   ShortIO.WriteReal(StdChans.StdOutChan(), real, width)
END WriteReal ;

END SShortIO.
