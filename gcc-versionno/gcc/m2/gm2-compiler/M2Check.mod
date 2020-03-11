(* M2Check.def perform rigerous type checking for fully declared symbols.

Copyright (C) 2020 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE M2Check ;

(*
    Title      : M2Check
    Author     : Gaius Mulley
    System     : GNU Modula-2
    Date       : Fri Mar  6 15:32:10 2020
    Revision   : $Version$
    Description: provides a module to check the symbol type compatibility.
                 It assumes that the declaration of all dependants
                 is complete.
*)

FROM M2Base IMPORT IsParameterCompatible, IsAssignmentCompatible, IsExpressionCompatible ;
FROM Lists IMPORT InitList, KillList, IncludeItemIntoList ;
FROM Indexing IMPORT InitIndex, GetIndice, PutIndice, KillIndice ;
FROM Storage IMPORT ALLOCATE ;


TYPE
   pair = POINTER TO RECORD
                        left, right: CARDINAL ;
                        pairStatus : status ;
                     END ;

   typeCheckFunction = PROCEDURE (status; tinfo; CARDINAL; CARDINAL) : status ;

   tInfo = RECORD
              kind     : (parameter, assignment, expression) ;
              actual,
              formal,
              left,
              right,
              procedure,
              nth      : CARDINAL ;
              error    : Error ;
              checkFunc: typeCheckFunction ;
           END ;

   status = (true, false, unknown, visit, unused) ;


(*
   checkTypeEquivalence -
*)

PROCEDURE checkTypeEquivalence (result: status; tinfo: tInfo; left, right: CARDINAL) : status ;
VAR
   leftT, rightT: CARDINAL ;
BEGIN
   IF isKnown (result)
   THEN
      RETURN result
   ELSE
      leftT := left ;
      WHILE (leftT # NulSym) AND IsType (leftT) DO
         rightT := right ;
         WHILE (rightT # NulSym) AND IsType (rightT) DO
            IF leftT = rightT
            THEN
               RETURN true
            END ;
            rightT := GetDType (rightT)
         END ;
         leftT := GetDType (leftT)
      END ;
      RETURN result
   END
END checkTypeEquivalence ;


(*
   checkArrayOfTypeEquivalence -
*)

PROCEDURE checkArrayOfTypeEquivalence (result: status; tinfo: tInfo;
                                       left, right: CARDINAL) : status ;
BEGIN
   IF isKnown (result)
   THEN
      RETURN result
   ELSE

   END
END checkArrayOfTypeEquivalence ;


(*
   checkGenericTypeEquivalence -
*)

PROCEDURE checkGenericTypeEquivalence (result: status; tinfo: tInfo;
                                       left, right: CARDINAL) : status ;
BEGIN
   IF isKnown (result)
   THEN
      RETURN result
   ELSE

   END
END checkGenericTypeEquivalence ;


(*
   checkBaseEquivalence - the catch all check for types not specifically
                          handled by this module.
*)

PROCEDURE checkBaseEquivalence (result: status; tinfo: tInfo;
                                left, right: CARDINAL) : status ;
BEGIN
   IF isKnown (result)
   THEN
      RETURN result
   ELSE
      CASE tinfo.kind OF

      parameter :  IF IsParameterCompatible (left, right)
                   THEN
                      RETURN true
                   END |
      assignment:  IF IsAssignmentCompatible (left, right)
                   THEN
                      RETURN true
                   END |
      expression:  IF IsExpressionCompatible (left, right)
                   THEN
                      RETURN true
                   END

      ELSE
         InternalError ('unexpected kind value', __FILE__, __LINE__)
      END
   END ;
   RETURN false
END checkBaseEquivalence ;


(*
   checkTypeKindEquivalence -
*)

PROCEDURE checkTypeKindEquivalence (result: status; tinfo: tInfo;
                                    left, right: CARDINAL) : status ;
BEGIN
   IF isKnown (result)
   THEN
      RETURN result
   ELSIF (left = NulSym) OR (right = NulSym)
   THEN
      RETURN true
   ELSE
      (* long cascade of all type kinds.  *)
      IF IsSet (left) AND IsSet (right)
      THEN
         RETURN checkSetEquivalent (result, tinfo, left, right)
(*
      ELSIF IsArray (left) AND IsArray (right)
      THEN
         RETURN checkArrayEquivalence (result, info, left, right)
*)
      ELSIF IsRecord (left) AND IsRecord (right)
      THEN
         RETURN checkRecordEquivalence (result, info, left, right)
      ELSE
         RETURN checkBaseEquivalence (result, info, left, right)
      END
   END
END checkTypeKindEquivalence ;


(*
   isSkipEquivalence -
*)

PROCEDURE isSkipEquivalence (left, right: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN SkipType (left) = SkipType (right)
END isSkipEquivalence ;


(*
   checkTypeRangeEquivalence -
*)

PROCEDURE checkTypeRangeEquivalence (result: status; tinfo: tInfo;
                                     left, right: CARDINAL) : status ;
VAR
   result: status ;
BEGIN
   IF isKnown (result)
   THEN
      RETURN result
   ELSIF isSkipEquivalence (left, right)
   THEN
      RETURN true
   ELSE
      result := checkValueEquivalence (result, tinfo, GetTypeMin (left), GetTypeMin (right)) ;
      RETURN checkValueEquivalence (result, tinfo, GetTypeMax (left), GetTypeMax (right))
   END
END checkTypeRangeEquivalence ;


(*
   checkSetEquivalent - compares set types, left and right.
*)

PROCEDURE checkSetEquivalent (result: status; tinfo: tInfo;
                              left, right: CARDINAL) : status ;
BEGIN
   IF isKnown (result)
   THEN
      RETURN result
   ELSIF isSkipEquivalence (left, right)
   THEN
      RETURN true
   ELSE
      result := checkTypeKindEquivalence (result, tinfo, GetDType (left), GetDType (right)) ;
      RETURN checkTypeRangeEquivalence (result, tinfo, GetDType (left), GetDType (right)) ;
   END
END checkSetEquivalent ;


(*
   checkRecordEquivalence - compares record types, left and right.
*)

PROCEDURE checkRecordEquivalence (result: status; tinfo: tInfo;
                                  left, right: CARDINAL) : status ;
BEGIN
   RETURN result
END checkRecordEquivalence ;


(*
   determineParameterCompatible - check for parameter compatibility by checking
                                  equivalence, array, generic and type kind.
*)

PROCEDURE determineParameterCompatible (tinfo: tInfo; left, right: CARDINAL) : status ;
BEGIN
   result := checkTypeEquivalence (unknown, tinfo, GetDType (left), GetDType (right)) ;
   result := checkArrayOfTypeEquivalence (result, tinfo, GetSType (left), GetSType (right)) ;
   result := checkGenericTypeEquivalence (result, tinfo, GetSType (left), GetSType (right)) ;
   result := checkTypeKindEquivalence (result, tinfo, GetSType (left), GetSType (right)) ;
   RETURN result
END determineParameterCompatible ;


(*
   getCompatible -
*)

PROCEDURE getCompatible (result: status; tinfo: tInfo; left, right: CARDINAL) : status ;
BEGIN
   IF in (resolved, left, right)
   THEN
      RETURN getStatus (resolved, left, right)
   ELSE
      RETURN tinfo.checkFunc (result, tinfo, left, right)
   END
END getCompatible ;


(*
   doCheck - keep obtaining an unresolved pair and check for the
             type compatibility.  This is the main check routine used by
             parameter, assignment and expression compatibility.
             It tests all unknown pairs and calls the appropriate
             check function
*)

PROCEDURE doCheck (tinfo: tInfo) : BOOLEAN ;
BEGIN
   WHILE get (unresolved, left, right, unknown) DO
      IF NOT in (visited, left, right)
      THEN
         result := tinfo.checkFunc (unknown, tinfo, left, right) ;
         IF isKnown (result)
         THEN
            (* remove this pair from the unresolved list.  *)
            exclude (unresolved, left, right) ;
            include (resolved, left, right, result) ;  (* if this needed?  *)
            RETURN result = true
         ELSE
            (* We set to mark as visited and move on.  *)
            include (visited, left, right)
         END
      END
   END ;
   RETURN TRUE
END doCheck ;


(*
   in - returns TRUE if the pair is in the list.
*)

PROCEDURE in (pairs: Index; left, right: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN ...
END in ;


(*
   get - returns TRUE if a pair can be found in pairs which have a status
         of s.  If true then left, right is set to the pair of symbols.
*)

PROCEDURE get (pairs: Index; VAR left, right: CARDINAL; s: status) : BOOLEAN ;
VAR
   i, n: CARDINAL ;
   p   : pair ;
BEGIN
   i := 1 ;
   n := HighIndicec (pairs) ;
   WHILE i <= n DO
      GetIndex (pairs, i, p) ;
      IF p^.pairStatus = s
      THEN
         left := p^.left ;
         right := p^.right ;
         RETURN TRUE
      END ;
      INC (i)
   END ;
   RETURN FALSE
END get ;


(*
   ParameterTypeCompatible - returns TRUE if the nth procedure parameter formal
                             is compatible with actual.
*)

PROCEDURE ParameterTypeCompatible (format: ARRAY OF CHAR;
                                   procedure, formal, actual, nth: CARDINAL) : BOOLEAN ;
VAR
   formalT, actualT: CARDINAL ;
   tinfo           : tInfo ;
BEGIN
   formalT := GetSType (formal) ;
   actualT := GetSType (actual) ;
   tinfo.kind := parameter ;
   tinfo.actual := actual ;
   tinfo.formal := formal ;
   tinfo.procedure := procedure ;
   tinfo.nth := nth ;
   tinfo.error := NIL ;
   tinfo.left := formalT ;
   tinfo.right := actualT ;
   tinfo.checkFunc := determineParameterCompatible ;
   include (unresolved, pairs, actual, formal) ;
   IF doCheck (tinfo, pairs)
   THEN
      KillList (visited) ;
      RETURN TRUE
   ELSE
      KillList (visited) ;
      RETURN FALSE
   END
END ParameterTypeCompatible ;


(*
   Init -
*)

PROCEDURE Init ;
BEGIN
   visited := InitIndex (1) ;
   unresolved := InitIndex (1)
END Init ;


BEGIN
   Init
END M2Check.
