(* Copyright (C) 2001 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)
IMPLEMENTATION MODULE M2Pass ;


FROM M2Error IMPORT InternalError ;

TYPE
   Pass = (NoPass, Pass1, Pass2, Pass3, CodeGeneration, ErrorPass, HiddenPass) ;

VAR
   CurrentPass: Pass ;


(*
   SetPassToNoPass - sets the pass state to no Pass.
*)

PROCEDURE SetPassToNoPass ;
BEGIN
   CurrentPass := NoPass
END SetPassToNoPass ;


(*
   SetPassToPass1 - sets the pass state to Pass 1.
*)

PROCEDURE SetPassToPass1 ;
BEGIN
   IF CurrentPass=NoPass
   THEN
      CurrentPass := Pass1
   ELSE
      InternalError('attempting to set CurrentPass to Pass1', __FILE__, __LINE__)
   END
END SetPassToPass1 ;



(*
   SetPassToPass2 - sets the pass state to Pass 2.
*)

PROCEDURE SetPassToPass2 ;
BEGIN
   IF CurrentPass=NoPass
   THEN
      CurrentPass := Pass2
   ELSE
      InternalError('attempting to set CurrentPass to Pass2', __FILE__, __LINE__)
   END
END SetPassToPass2 ;



(*
   SetPassToPass3 - sets the pass state to Pass 3.
*)

PROCEDURE SetPassToPass3 ;
BEGIN
   IF CurrentPass=NoPass
   THEN
      CurrentPass := Pass3
   ELSE
      InternalError('attempting to set CurrentPass to Pass3', __FILE__, __LINE__)
   END
END SetPassToPass3 ;


(*
   SetPassToErrorPass - sets the pass state to no Error Pass.
*)

PROCEDURE SetPassToErrorPass ;
BEGIN
   CurrentPass := ErrorPass
END SetPassToErrorPass ;


(*
   SetPassToPassHidden - sets the pass state to the hidden type pass.
*)

PROCEDURE SetPassToPassHidden ;
BEGIN
   CurrentPass := HiddenPass
END SetPassToPassHidden ;


(*
   SetPassToCodeGeneration - sets the pass state to CodeGeneration.
*)

PROCEDURE SetPassToCodeGeneration ;
BEGIN
   IF CurrentPass=NoPass
   THEN
      CurrentPass := CodeGeneration
   ELSE
      InternalError('attempting to set CurrentPass to CodeGeneration', __FILE__, __LINE__)
   END
END SetPassToCodeGeneration ;


(*
   IsNoPass - returns true if currently in no Pass.
*)

PROCEDURE IsNoPass () : BOOLEAN ;
BEGIN
   RETURN( CurrentPass=NoPass )
END IsNoPass ;


(*
   IsPass1 - returns true if currently in Pass 1.
*)

PROCEDURE IsPass1 () : BOOLEAN ;
BEGIN
   RETURN( CurrentPass=Pass1 )
END IsPass1 ;


(*
   IsPass2 - returns true if currently in Pass 2.
*)

PROCEDURE IsPass2 () : BOOLEAN ;
BEGIN
   RETURN( CurrentPass=Pass2 )
END IsPass2 ;


(*
   IsPass3 - returns true if currently in Pass 3.
*)

PROCEDURE IsPass3 () : BOOLEAN ;
BEGIN
   RETURN( CurrentPass=Pass3 )
END IsPass3 ;


(*
   IsPassCodeGeneration - returns true if currently in the CodeGeneration Pass.
*)

PROCEDURE IsPassCodeGeneration () : BOOLEAN ;
BEGIN
   RETURN( CurrentPass=CodeGeneration )
END IsPassCodeGeneration ;


(*
   IsPassHidden - returns TRUE if currently parsing for hidden types.
*)

PROCEDURE IsPassHidden () : BOOLEAN ;
BEGIN
   RETURN( CurrentPass=HiddenPass )
END IsPassHidden ;


(*
   IsErrorPass - returns true if currently in the Error Pass.
*)

PROCEDURE IsErrorPass () : BOOLEAN ;
BEGIN
   RETURN( CurrentPass=ErrorPass )
END IsErrorPass ;


BEGIN
   SetPassToNoPass
END M2Pass.