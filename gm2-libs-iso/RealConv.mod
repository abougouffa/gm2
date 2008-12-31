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

FROM SYSTEM IMPORT ADDRESS ;
FROM ConvTypes IMPORT ScanClass ;
FROM CharClass IMPORT IsNumeric, IsWhiteSpace ;
FROM DynamicStrings IMPORT String, InitString, InitStringCharStar, KillString, Length, Slice, Mark, Index ;
FROM dtoa IMPORT Mode, dtoa, strtod ;
FROM M2RTS IMPORT Halt ;
FROM Builtins IMPORT log10, exp10 ;
FROM libc IMPORT free ;
IMPORT EXCEPTIONS ;


TYPE
   RealConvException = (noException, invalid, outofrange) ;

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
   r    : REAL ;
   error: BOOLEAN ;
BEGIN
   r := strtod(str, error) ;
   IF error
   THEN
      EXCEPTIONS.RAISE(realConv, ORD(outofrange),
                       'RealConv.' + __FUNCTION__ + ': real number is out of range')
   END ;
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


(* Returns the number of characters in the floating-point string
   representation of real with sigFigs significant figures.
*)

PROCEDURE LengthFloatReal (real: REAL; sigFigs: CARDINAL) : CARDINAL ;
VAR
   point,
   powerOfTen: INTEGER ;
   s         : String ;
   r         : ADDRESS ;
   l         : CARDINAL ;
   sign      : BOOLEAN ;
BEGIN
   r := dtoa(real, maxsignificant, sigFigs, point, sign) ;
   IF sigFigs>0
   THEN
      s := InitStringCharStar(r) ;
      (* free(r) ; *)
      l := Length(s) ;
      s := KillString(s) ;
      IF sigFigs<l
      THEN
         l := sigFigs
      END ;

      l := l+1+1+1+VAL(CARDINAL, log10ii(ABS(point))+1) ;   (* '.' + 'E' + 'sign' *)

      IF sign
      THEN
         INC(l)
      END ;
      RETURN( l )
   ELSE
      (* free(r) ; *)
      RETURN( 0 )
   END
END LengthFloatReal ;


(* Returns the number of characters in the floating-point engineering
   string representation of real with sigFigs significant figures.
*)

PROCEDURE LengthEngReal (real: REAL; sigFigs: CARDINAL) : CARDINAL ;
VAR
   r         : ADDRESS ;
   sign      : BOOLEAN ;
   powerOfTen,
   l, point  : INTEGER ;
   s         : String ;
BEGIN
   r := dtoa(real, maxsignificant, sigFigs, point, sign) ;
   s := InitStringCharStar(r) ;
   (* free(r) ; *)
   l := Length(s) ;
   s := KillString(s) ;
   IF point>=0
   THEN
      CASE point MOD 3 OF

      0:  powerOfTen := point |
      1:  powerOfTen := point-1 |
      2:  powerOfTen := point-2

      END
   ELSE
      CASE ABS(point) MOD 3 OF

      0:  powerOfTen := point |
      1:  powerOfTen := point+1 |
      2:  powerOfTen := point+2

      END
   END ;
   IF point>=l
   THEN
      INC(l, 2)  (* append '.0' *)
   ELSE
      INC(l)     (* insert '.' *)
   END ;
      
   IF sigFigs>0
   THEN
      l := l+1+1+log10ii(powerOfTen)+1 ;   (* 'E' + 'sign' *)
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
   point: INTEGER ;
   sign : BOOLEAN ;
   r    : ADDRESS ;
BEGIN
   r := dtoa(real, maxsignificant, 100, point, sign) ;
   (* free(r) ; *)
   IF place>=0
   THEN
      RETURN( point+place+1 )   (* +1 to include '.' *)
   ELSE
      RETURN( point+2 )   (* need to append '.0' *)
   END
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
