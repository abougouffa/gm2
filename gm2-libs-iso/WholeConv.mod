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

IMPLEMENTATION MODULE WholeConv ;


(*
   ScanInt - represents the start state of a finite state scanner
             for signed whole numbers - assigns class of inputCh
             to chClass and a procedure representing the next state
             to nextState.
*)

PROCEDURE ScanInt (inputCh: CHAR;
                   VAR chClass: ConvTypes.ScanClass;
                   VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsNumeric(inputCh)
   THEN
      nextState := scanRemainingDigits ;
      chClass := valid
   ELSIF (inputCh='+') OR (inputCh='-')
   THEN
      nextState := scanFirstDigit ;
      chClass := valid
   ELSIF IsWhiteSpace(inputCh)
   THEN
      nextState := scanSpace ;
      chClass := padding
   ELSE
      nextState := ScanInt ;
      chClass := invalid
   END
END ScanInt ;


PROCEDURE scanFirstDigit (ch: CHAR;
                          VAR chClass: Convtypes.ScanClass;
                          VAR nextState: ScanState) ;
BEGIN
   IF IsNumeric(ch)
   THEN
      chClass := valid ;
      nextState := scanRemainingDigits
   ELSE
      chClass := invalid
   END
END scanFirstDigit ;


PROCEDURE scanRemainingDigits (ch: CHAR;
                               VAR chClass: Convtypes.ScanClass;
                               VAR nextState: ScanState) ;
BEGIN
   IF IsNumeric(ch)
   THEN
      chClass := valid
   ELSE
      chClass := terminator
   END
END scanRemainingDigits ;


PROCEDURE scanSpace (ch: CHAR;
                     VAR chClass: Convtypes.ScanClass;
                     VAR nextState: ScanState) ;
BEGIN
   IF IsWhiteSpace(ch)
   THEN
      chClass := padding
   ELSIF (ch='+') OR (ch='-')
   THEN
      chClass := valid ;
      nextState := scanFirstDigit
   ELSE
      chClass := invalid
   END
END scanSpace ;


(*
   FormatInt - returns the format of the string value for
               conversion to INTEGER.
*)

PROCEDURE FormatInt (str: ARRAY OF CHAR) : ConvResults ;
VAR
   proc   : ScanState ;
   chClass: ScanClass ;
   i, h   : CARDINAL ;
BEGIN
   i := 0 ;
   h := LENGTH(str) ;
   ScanInt(str[0], chClass, proc) ;
   REPEAT
      proc(str[i], chClass, proc) ;
      INC(i) ;
   UNTIL (i>=h) OR (chClass#padding) ;
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
END FormatInt ;


(*
   ValueInt - returns the value corresponding to the signed whole
              number string value str if str is well-formed;
              otherwise raises the WholeConv exception.
*)

PROCEDURE ValueInt (str: ARRAY OF CHAR) : INTEGER;
BEGIN
   
END ValueInt ;


(*
   LengthInt - returns the number of characters in the string
               representation of int.
*)

PROCEDURE LengthInt (int: INTEGER) : CARDINAL ;
VAR
   c, l: CARDINAL ;
BEGIN
   IF int<0
   THEN
      l := 2 ;
      IF int=MIN(INTEGER)
      THEN
         c := VAL(CARDINAL, MAX(INTEGER))+1
      ELSE
         c := -int
      END
   ELSE
      l := 1 ;
      c := int
   END ;
   WHILE c>9 DO
      c := c DIV 10 ;
      INC(l)
   END ;
   RETURN( l )
END LengthInt ;


(*
   ScanCard - represents the start state of a finite state scanner for
              unsigned whole numbers - assigns class of inputCh to
              chClass and a procedure representing the next state to
              nextState.
*)

PROCEDURE ScanCard (inputCh: CHAR;
                    VAR chClass: ConvTypes.ScanClass;
                    VAR nextState: ConvTypes.ScanState) ;
BEGIN
   IF IsNumeric(inputCh)
   THEN
      nextState := scanRemainingDigits ;
      chClass := valid
   ELSIF IsWhiteSpace(inputCh)
   THEN
      nextState := scanSpace ;
      chClass := padding
   ELSE
      nextState := ScanInt ;
      chClass := invalid
   END
END ScanCard ;


(*
   FormatCard - returns the format of the string value for
                conversion to CARDINAL.
*)

PROCEDURE FormatCard (str: ARRAY OF CHAR) : ConvResults ;
BEGIN
   
END FormatCard ;


(*
   ValueCard - returns the value corresponding to the unsigned
               whole number string value str if str is well-formed;
               otherwise raises the WholeConv exception.
*)

PROCEDURE ValueCard (str: ARRAY OF CHAR) : CARDINAL ;
BEGIN
   
END ValueCard ;


(*
   LengthCard - returns the number of characters in the string
                representation of, card.
*)

PROCEDURE LengthCard (card: CARDINAL) : CARDINAL ;
VAR
   l: CARDINAL ;
BEGIN
   l := 1 ;
   WHILE card>9 DO
      card := card DIV 10 ;
      INC(l)
   END ;
   RETURN( l )
END LengthCard ;


(*
   IsWholeConvException - returns TRUE if the current coroutine is
                          in the exceptional execution state because
                          of the raising of an exception in a routine
                          from this module; otherwise returns FALSE.
*)

PROCEDURE IsWholeConvException () : BOOLEAN ;
BEGIN
   
END IsWholeConvException ;


END WholeConv.
