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

IMPLEMENTATION MODULE RealConv ;

FROM ConvTypes IMPORT ScanClass ;
FROM CharClass IMPORT IsNumeric, IsWhiteSpace ;
FROM DynamicStrings IMPORT String, InitString, KillString, Length, Slice, Mark, Index ;
FROM StringConvert IMPORT StringToLongreal, LongrealToString ;
FROM M2RTS IMPORT Halt ;
FROM Builtins IMPORT log10, exp10 ;
IMPORT EXCEPTIONS ;


TYPE
   RealConvException = (noException, invalid) ;

VAR
   realConv:  EXCEPTIONS.ExceptionSource ;


(* Low-level REAL/string conversions *)

(* Represents the start state of a finite state scanner for real
   numbers - assigns class of inputCh to chClass and a procedure
   representing the next state to nextState.
*)

PROCEDURE ScanReal (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                    VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsNumeric(inputCh)
   THEN
      nextState := scanSecondDigit ;
      chClass := valid
   ELSIF (inputCh='+') OR (inputCh='-')
   THEN
      nextState := scanFirstDigit ;
      chClass := valid
   ELSIF IsWhiteSpace(inputCh)
   THEN
      nextState := ScanReal ;
      chClass := padding
   ELSE
      nextState := ScanReal ;
      chClass := invalid
   END
END ScanReal ;


(*
   scanFirstDigit - 
*)

PROCEDURE scanFirstDigit (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                          VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsNumeric(inputCh)
   THEN
      nextState := scanSecondDigit ;
      chClass := valid
   ELSE
      nextState := scanFirstDigit ;
      chClass := invalid
   END
END scanFirstDigit ;


(*
   scanSecondDigit - 
*)

PROCEDURE scanSecondDigit (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                           VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsNumeric(inputCh)
   THEN
      nextState := scanSecondDigit ;
      chClass := valid
   ELSIF inputCh='.'
   THEN
      nextState := scanFixed ;
      chClass := valid
   ELSIF inputCh='E'
   THEN
      nextState := scanScientific ;
      chClass := valid
   ELSE
      nextState := noOpFinished ;
      chClass := terminator
   END
END scanSecondDigit ;


(*
   scanFixed - 
*)

PROCEDURE scanFixed (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                     VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsNumeric(inputCh)
   THEN
      nextState := scanFixed ;
      chClass := valid
   ELSIF inputCh='E'
   THEN
      nextState := scanScientific ;
      chClass := valid
   ELSE
      nextState := noOpFinished ;
      chClass := terminator
   END
END scanFixed ;


(*
   scanScientific - 
*)

PROCEDURE scanScientific (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                          VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsNumeric(inputCh)
   THEN
      nextState := scanScientificSecond ;
      chClass := valid
   ELSIF (inputCh='-') OR (inputCh='+')
   THEN
      nextState := scanScientificSign ;
      chClass := valid
   ELSE
      nextState := scanScientific ;
      chClass := invalid
   END
END scanScientific ;


(*
   scanScientificSign - 
*)

PROCEDURE scanScientificSign (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                              VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsNumeric(inputCh)
   THEN
      nextState := scanScientificSecond ;
      chClass := valid
   ELSE
      nextState := scanScientificSign ;
      chClass := invalid
   END
END scanScientificSign ;


(*
   scanScientificSecond - 
*)

PROCEDURE scanScientificSecond (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                                VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsNumeric(inputCh)
   THEN
      nextState := scanScientificSecond ;
      chClass := valid
   ELSE
      nextState := noOpFinished ;
      chClass := terminator
   END
END scanScientificSecond ;


(*
   noOpFinished - 
*)

PROCEDURE noOpFinished (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                        VAR nextState: ConvTypes.ScanState) ;
BEGIN
   nextState := noOpFinished ;
   chClass := terminator ;
   (* should we raise an exception here? *)
END noOpFinished ;


(* Returns the format of the string value for conversion to REAL. *)

PROCEDURE FormatReal (str: ARRAY OF CHAR) : ConvResults ;
VAR
   proc   : ConvTypes.ScanState ;
   chClass: ConvTypes.ScanClass ;
   i, h   : CARDINAL ;
BEGIN
   i := 1 ;
   h := LENGTH(str) ;
   ScanReal(str[0], chClass, proc) ;
   WHILE (i<h) AND (chClass=padding) DO
      proc(str[i], chClass, proc) ;
      INC(i)
   END ;
   IF chClass=terminator
   THEN
      RETURN( strEmpty )
   END ;
   WHILE (i<h) AND (chClass=valid) DO
      proc(str[i], chClass, proc) ;
      INC(i)
   END ;
   CASE chClass OF

   padding   :  RETURN( strWrongFormat ) |
   terminator,
   valid     :  RETURN( strAllRight ) |
   invalid   :  RETURN( strWrongFormat )

   END
END FormatReal ;


(* Returns the value corresponding to the real number string value
   str if str is well-formed; otherwise raises the RealConv
   exception.
*)

PROCEDURE ValueReal (str: ARRAY OF CHAR) : REAL ;
BEGIN
   IF FormatReal(str)=strAllRight
   THEN
      RETURN( doValueReal(str) )
   ELSE
      EXCEPTIONS.RAISE(realConv, ORD(invalid),
                       'RealConv.' + __FUNCTION__ + ': real number is invalid')
   END
END ValueReal ;


(*
   doValueReal - str, is a well-formed real number and its
                 value is returned.
*)

PROCEDURE doValueReal (str: ARRAY OF CHAR) : REAL ;
VAR
   s    : String ;
   r    : REAL ;
   found: BOOLEAN ;
BEGIN
   s := InitString(str) ;
   r := StringToLongreal(s, found) ;
   IF NOT found
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__,
           'expecting a well formed floating point string ' +
           '(StringToLongreal is incompatible with FormatReal)')
   END ;
   s := KillString(s) ;
   RETURN( r )
END doValueReal ;


(*
   log10i - returns the rounded value of log10(real)
*)

PROCEDURE log10i (real: REAL) : INTEGER ;
BEGIN
   RETURN( VAL(INTEGER, log10(real)) )
END log10i ;


(*
   exp10i - returns the real value of exp10(i)
*)

PROCEDURE exp10i (i: INTEGER) : REAL ;
BEGIN
   RETURN( exp10(FLOAT(i)) )
END exp10i ;


(*
   log10ii - returns the log10 of, i.
*)

PROCEDURE log10ii (i: INTEGER) : INTEGER ;
BEGIN
   RETURN( TRUNC(log10(FLOAT(i))) )
END log10ii ;


(*
   normalise - 
*)

PROCEDURE normalise (VAR real: REAL; VAR powerOfTen: INTEGER) ;
VAR
   n: BOOLEAN ;
BEGIN
   IF real<0.0
   THEN
      n := TRUE ;
      real := -real
   ELSE
      n := FALSE
   END ;
   powerOfTen := log10i(real) ;
   IF real>=10.0
   THEN
      real := real / exp10i(powerOfTen-1)
   END ;
   IF n
   THEN
      real := -real
   END
END normalise ;


(* Returns the number of characters in the floating-point string
   representation of real with sigFigs significant figures.
*)

PROCEDURE LengthFloatReal (real: REAL; sigFigs: CARDINAL) : CARDINAL ;
VAR
   powerOfTen: INTEGER ;
   s         : String ;
   l         : CARDINAL ;
BEGIN
   normalise(real, powerOfTen) ;
   IF sigFigs>0
   THEN
      s := LongrealToString(VAL(LONGREAL, real), sigFigs+1, sigFigs-1) ;
      l := VAL(INTEGER, Length(s))+1+1+log10ii(powerOfTen) ;   (* 'E' + 'sign' *)
      s := KillString(s) ;
      RETURN( l )
   ELSE
      RETURN( 0 )
   END
END LengthFloatReal ;


(* Returns the number of characters in the floating-point engineering
   string representation of real with sigFigs significant figures.
*)

PROCEDURE LengthEngReal (real: REAL; sigFigs: CARDINAL) : CARDINAL ;
VAR
   powerOfTen: INTEGER ;
   s         : String ;
   l         : CARDINAL ;
BEGIN
   normalise(real, powerOfTen) ;
   CASE powerOfTen MOD 3 OF

   0:  |
   1:  real := real * 10.0 ;
       DEC(powerOfTen) |
   2:  real := real * 100.0 ;
       DEC(powerOfTen, 2)

   END ;
      
   IF sigFigs>0
   THEN
      s := LongrealToString(VAL(LONGREAL, real), sigFigs+1, sigFigs-1) ;
      l := VAL(INTEGER, Length(s))+1+1+log10ii(powerOfTen) ;   (* 'e' + 'sign' *)
      s := KillString(s) ;
      RETURN( l )
   ELSE
      RETURN( 0 )
   END
END LengthEngReal ;


(* Returns the number of characters in the fixed-point string
   representation of real rounded to the given place relative to the
   decimal point.
*)

PROCEDURE LengthFixedReal (real: REAL; place: INTEGER) : CARDINAL ;
VAR
   s: String ;
   l: CARDINAL ;
   i: INTEGER ;
BEGIN
   s := LongrealToString(VAL(LONGREAL, real), 0, 0) ;
   i := Index(s, '.', 0) ;
   IF i>=0
   THEN
      RETURN( i+place )
   END ;
   l := Length(s) ;
   s := KillString(s) ;
   RETURN( l )
END LengthFixedReal ;


(* Returns TRUE if the current coroutine is in the exceptional
   execution state because of the raising of an exception in a
   routine from this module; otherwise returns FALSE.
*)

PROCEDURE IsRConvException () : BOOLEAN ;
BEGIN
   RETURN( EXCEPTIONS.IsCurrentSource(realConv) )
END IsRConvException ;


BEGIN
   EXCEPTIONS.AllocateSource(realConv)
END RealConv.
