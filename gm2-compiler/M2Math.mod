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
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)
IMPLEMENTATION MODULE M2Math ;


FROM SymbolTable IMPORT NulSym,
                        SetCurrentModule,
                        StartScope,
                        EndScope,
      	       	     	MakeConstVar,
                        MakeProcedure,
                        PutFunction,
                        PutConst,
                        PutConstString,
                        PopValue ;

FROM M2Base IMPORT Real, LongReal ;
FROM M2Batch IMPORT MakeDefinitionSource ;
FROM NameKey IMPORT MakeKey ;
FROM M2ALU IMPORT PushString ;


(*
   IsPseudoMathFunction - returns TRUE if Sym is an in built math function.
*)

PROCEDURE IsPseudoMathFunction (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (Sym=sin) OR (Sym=cos) OR (Sym=tan) OR (Sym=atan) OR (Sym=sqrt)
         )
END IsPseudoMathFunction ;


(*
   InitMath - initializes the module MATH
*)

PROCEDURE InitMath ;
VAR
   Math: CARDINAL ;
BEGIN
   (* create MATH module *)
   Math := MakeDefinitionSource(MakeKey('MATH')) ;
   StartScope(Math) ;

   (* predefined constants *)
   pi := MakeConstVar(MakeKey('pi')) ;
   PutConst(pi, LongReal) ;
   PushString(MakeKey('3.1415926535897931158542633056640625')) ;
   (*
      dont worry about the inaccuracy of the number above, or the
      errors that will creap in during str -> long real conversion
      because the code generator M2Gen487 treats pi as a special
      and hardwires this constant (using special instructions)
      whereever it can.

      however inaccuracy will occur when we perform constant folding

         we should really implement this by using a hex constant
            - a trade off between accuracy and portability.
            - we shouldn't really do the above as we introduce
              more errors each time we convert string -> longreal.
   *)

   PopValue(pi) ;

   (* And now the predefined pseudo functions *)

   sin := MakeProcedure(MakeKey('sin')) ;     (* Function        *)
(*
   PutFunction(sin, Real) ;                   (* Return Type     *)
                                              (* REAL/LONGREAL   *)
*)

   cos := MakeProcedure(MakeKey('cos')) ;     (* Function        *)
(*
   PutFunction(cos, Real) ;                   (* Return Type     *)
                                              (* REAL/LONGREAL   *)
*)

   tan := MakeProcedure(MakeKey('tan')) ;     (* Function        *)
(*
   PutFunction(tan, Real) ;                   (* Return Type     *)
                                              (* REAL/LONGREAL   *)
*)
   atan := MakeProcedure(MakeKey('atan')) ;   (* Function        *)
(*
   PutFunction(atan, Real) ;                  (* Return Type     *)
                                              (* REAL/LONGREAL   *)
*)
   sqrt := MakeProcedure(MakeKey('sqrt')) ;   (* Function        *)
(*
   PutFunction(sqrt, Real) ;                  (* Return Type     *)
                                              (* REAL/LONGREAL   *)
*)
   EndScope
END InitMath ;


END M2Math.
