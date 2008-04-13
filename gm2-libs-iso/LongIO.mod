(* Copyright (C) 2005 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE LongIO ;

(*
   Input and output of long real numbers in decimal text form over
   specified channels.  The read result is of the type
   IOConsts.ReadResults.
*)

IMPORT IOChan;

(*
   The text form of a signed fixed-point real number is
   ["+" | "-"], decimal digit, {decimal digit}, [".", {decimal digit}]

   The text form of a signed floating-point real number is
      signed fixed-point real number,
      "E", ["+" | "-"], decimal digit, {decimal digit}
*)


(*
   ReadReal - Skips leading spaces, and removes any remaining characters
              from cid that form part of a signed fixed or floating point
              number.  The value of this number is assigned to real.
              The read result is set to the value allRight, outOfRange,
              wrongFormat, endOfLine, or endOfInput.
*)

PROCEDURE ReadReal (cid: IOChan.ChanId; VAR real: LONGREAL) ;
BEGIN
   
END ReadReal ;


(*
   WriteFloat - writes the value of real to cid in floating-point text
                form, with sigFigs significant figures, in a field of
                the given minimum width.
*)

PROCEDURE WriteFloat (cid: IOChan.ChanId; real: LONGREAL;
                      sigFigs: CARDINAL; width: CARDINAL) ;
VAR
   s, w   : String ;
   i, l, f: CARDINAL ;
BEGIN
   w := NIL ;
   l := VAL(CARDINAL, fabsl(logl(real))) ;
   f := width - l ;
   s := LongrealToString(real, width, f) ;
   d := Index(s, '.', 0) ;
   IF d>=0
   THEN
      w := Slice(s, 0, d)
   ELSE
      w := Dup(s)
   END ;
   w := Slice(w, 0, sigFigs) ;
   i := Length(w) ;
   IF i<=sigFigs
   THEN
      Fin(KillString(OutputString(cid, InitStringChar('.')))) ;
      WHILE i<sigFigs DO
         Fin(KillString(OutputString(cid, InitStringChar('0')))) ;
         INC(i)
      END
   END ;
   WHILE i<width DO
      Fin(KillString(OutputString(cid, InitStringChar(' ')))) ;
      INC(i)
   END ;
   s := KillString(s) ;
   w := KillString(w)
END WriteFloat ;


(*
   WriteEng - as for WriteFloat, except that the number is scaled with
              one to three digits in the whole number part, and with an
              exponent that is a multiple of three.
*)

PROCEDURE WriteEng (cid: IOChan.ChanId; real: LONGREAL;
                    sigFigs: CARDINAL; width: CARDINAL) ;
BEGIN
   
END WriteEng ;


(*
   WriteFixed - writes the value of real to cid in fixed-point text form,
                rounded to the given place
                relative to the decimal point, in a field of the given
                minimum width.
*)

PROCEDURE WriteFixed (cid: IOChan.ChanId; real: LONGREAL;
                      place: INTEGER; width: CARDINAL) ;
BEGIN
   
END WriteFixed ;


(*
   WriteReal - writes the value of real to cid, as WriteFixed if the
               sign and magnitude can be shown in the given width,
               or otherwise as WriteFloat.  The number of places or
               significant digits depends on the given width.
*)

PROCEDURE WriteReal (cid: IOChan.ChanId; real: LONGREAL; width: CARDINAL) ;
BEGIN
   
END WriteReal ;


END LongIO.
