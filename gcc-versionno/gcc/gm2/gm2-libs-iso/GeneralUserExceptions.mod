(* Copyright (C) 2010-2019
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
Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA  02110-1301  USA *)

IMPLEMENTATION MODULE GeneralUserExceptions ;

FROM EXCEPTIONS IMPORT ExceptionSource, RAISE, AllocateSource, CurrentNumber,
                       IsCurrentSource, IsExceptionalExecution ;

FROM M2RTS IMPORT NoException ;
FROM SYSTEM IMPORT ADR ;


VAR
   general: ExceptionSource ;


(*
   RaiseGeneralException - raises exception using text as the associated
                           message.
*)

PROCEDURE RaiseGeneralException (exception: GeneralExceptions; text: ARRAY OF CHAR) ;
BEGIN
   RAISE (general, ORD (exception), text)
END RaiseGeneralException ;


(*
   IsGeneralException - returns TRUE if the current coroutine is in the
                        exceptional execution state because of the raising
                        of an exception from GeneralExceptions; otherwise
                        returns FALSE.
*)

PROCEDURE IsGeneralException () : BOOLEAN ;
BEGIN
   RETURN IsExceptionalExecution () AND IsCurrentSource (general)
END IsGeneralException ;


(*
   GeneralException - if the current coroutine is in the exceptional
                      execution state because of the raising of an
                      exception from GeneralExceptions, returns the
                      corresponding enumeration value, and otherwise
                      raises an exception.
*)

PROCEDURE GeneralException () : GeneralExceptions;
BEGIN
   IF IsGeneralException ()
   THEN
      RETURN VAL (GeneralExceptions, CurrentNumber (general))
   ELSE
      NoException (ADR (__FILE__), __LINE__,
                   __COLUMN__, ADR (__FUNCTION__),
      ADR ("not in the exceptional execution state"))
   END
END GeneralException ;


BEGIN
   AllocateSource (general)
END GeneralUserExceptions.
