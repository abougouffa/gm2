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
FROM Indexing IMPORT Index, InitIndex, GetIndice, PutIndice, KillIndex, HighIndice, LowIndice, IncludeIndiceIntoIndex ;
FROM M2Error IMPORT Error, InternalError ;
FROM M2Debug IMPORT Assert ;
FROM SymbolTable IMPORT NulSym, IsRecord, IsSet, GetDType, GetSType, IsType, SkipType ;
FROM M2GCCDeclare IMPORT GetTypeMin, GetTypeMax ;
FROM m2expr IMPORT AreConstantsEqual ;
FROM SymbolConversion IMPORT Mod2Gcc ;
FROM Storage IMPORT ALLOCATE ;


TYPE
   pair = POINTER TO RECORD
                        left, right: CARDINAL ;
                        pairStatus : status ;
                        next       : pair ;
                     END ;

   typeCheckFunction = PROCEDURE (status, tInfo, CARDINAL, CARDINAL) : status ;

   checkType = (parameter, assignment, expression) ;

   tInfo = POINTER TO RECORD
                         kind      : checkType ;
                         actual,
                         formal,
                         left,
                         right,
                         procedure,
                         nth       : CARDINAL ;
                         error     : Error ;
                         checkFunc : typeCheckFunction ;
                         visited,
                         resolved,
                         unresolved: Index ;
                         next      : tInfo ;
                      END ;

   status = (true, false, unknown, visited, unused) ;


VAR
   pairFreeList : pair ;
   tinfoFreeList: tInfo ;


(*
   isKnown - returns BOOLEAN:TRUE if result is status:true or status:false.
*)

PROCEDURE isKnown (result: status) : BOOLEAN ;
BEGIN
   RETURN (result = true) OR (result = false) OR (result = visited)
END isKnown ;


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
      CASE tinfo^.kind OF

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
         RETURN checkRecordEquivalence (result, tinfo, left, right)
      ELSE
         RETURN checkBaseEquivalence (result, tinfo, left, right)
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
   checkValueEquivalence -
*)

PROCEDURE checkValueEquivalence (result: status; tinfo: tInfo;
                                 left, right: CARDINAL) : status ;
BEGIN
   IF isKnown (result)
   THEN
      RETURN result
   ELSIF left = right
   THEN
      RETURN true
   ELSE
      IF AreConstantsEqual (Mod2Gcc (left), Mod2Gcc (right))
      THEN
         RETURN true
      ELSE
         RETURN false
      END
   END
END checkValueEquivalence ;


(*
   and -
*)

PROCEDURE and (left, right: status) : status ;
BEGIN
   IF (left = true) AND (right = true)
   THEN
      RETURN true
   ELSE
      RETURN false
   END
END and ;


(*
   checkTypeRangeEquivalence -
*)

PROCEDURE checkTypeRangeEquivalence (result: status; tinfo: tInfo;
                                     left, right: CARDINAL) : status ;
VAR
   result2, result3: status ;
BEGIN
   result := visit (result, tinfo, left, right) ;
   result := checkSkipEquivalence (result, tinfo, left, right) ;
   result2 := checkValueEquivalence (result, tinfo, GetTypeMin (left), GetTypeMin (right)) ;
   result3 := checkValueEquivalence (result, tinfo, GetTypeMax (left), GetTypeMax (right)) ;
   RETURN return (and (result2, result3), tinfo, left, right)
END checkTypeRangeEquivalence ;


(*
   include - include pair left:right into pairs with status, s.
*)

PROCEDURE include (pairs: Index; left, right: CARDINAL; s: status) ;
VAR
   p: pair ;
BEGIN
   p := newPair () ;
   p^.left := left ;
   p^.right := right ;
   p^.pairStatus := s ;
   p^.next := NIL ;
   IncludeIndiceIntoIndex (pairs, p)
END include ;


(*
   exclude - exclude pair left:right from pairs.
*)

PROCEDURE exclude (pairs: Index; left, right: CARDINAL) ;
VAR
   p   : pair ;
   i, n: CARDINAL ;
BEGIN
   i := 1 ;
   n := HighIndice (pairs) ;
   WHILE i <= n DO
      p := GetIndice (pairs, i) ;
      IF (p # NIL) AND (p^.left = left) AND (p^.right = right)
      THEN
         PutIndice (pairs, i, NIL) ;
         disposePair (p) ;
         RETURN
      END ;
      INC (i)
   END
END exclude ;


(*
   getStatus -
*)

PROCEDURE getStatus (pairs: Index; left, right: CARDINAL) : status ;
VAR
   p   : pair ;
   i, n: CARDINAL ;
BEGIN
   i := 1 ;
   n := HighIndice (pairs) ;
   WHILE i <= n DO
      p := GetIndice (pairs, i) ;
      IF (p # NIL) AND (p^.left = left) AND (p^.right = right)
      THEN
         RETURN p^.pairStatus
      END ;
      INC (i)
   END ;
   RETURN unknown
END getStatus ;


(*
   return -
*)

PROCEDURE return (result: status; tinfo: tInfo; left, right: CARDINAL) : status ;
BEGIN
   IF result # unknown
   THEN
      IF isKnown (result)
      THEN
         include (tinfo^.resolved, left, right, result)
      END
   END ;
   exclude (tinfo^.visited, left, right) ;
   RETURN result
END return ;


(*
   visit - check that we have not already visited left/right.  If we have seen this
           pair then it will return visited.
*)

PROCEDURE visit (result: status; tinfo: tInfo; left, right: CARDINAL) : status ;
BEGIN
   IF isKnown (result)
   THEN
      RETURN result
   ELSIF in (tinfo^.visited, left, right)
   THEN
      RETURN visited
   ELSE
      include (tinfo^.visited, left, right, result) ;
      RETURN result
   END
END visit ;


(*
   checkSkipEquivalence - return true if left right are equivalent.
*)

PROCEDURE checkSkipEquivalence (result: status; tinfo: tInfo;
                                left, right: CARDINAL) : status ;
BEGIN
   IF isKnown (result)
   THEN
      RETURN result
   ELSIF isSkipEquivalence (left, right)
   THEN
      RETURN true
   ELSE
      RETURN result
   END
END checkSkipEquivalence ;


(*
   checkSetEquivalent - compares set types, left and right.
*)

PROCEDURE checkSetEquivalent (result: status; tinfo: tInfo;
                              left, right: CARDINAL) : status ;
BEGIN
   result := visit (result, tinfo, left, right) ;
   result := checkSkipEquivalence (result, tinfo, left, right) ;
   result := checkTypeKindEquivalence (result, tinfo, GetDType (left), GetDType (right)) ;
   result := checkTypeRangeEquivalence (result, tinfo, GetDType (left), GetDType (right)) ;
   RETURN return (result, tinfo, left, right)
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

PROCEDURE determineParameterCompatible (result: status; tinfo: tInfo; left, right: CARDINAL) : status ;
BEGIN
   result := checkTypeEquivalence (result, tinfo, GetDType (left), GetDType (right)) ;
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
   IF in (tinfo^.resolved, left, right)
   THEN
      RETURN getStatus (tinfo^.resolved, left, right)
   ELSE
      RETURN tinfo^.checkFunc (result, tinfo, left, right)
   END
END getCompatible ;


(*
   get -
*)

PROCEDURE get (pairs: Index; VAR left, right: CARDINAL; s: status) : BOOLEAN ;
VAR
   i, n: CARDINAL ;
   p   : pair ;
BEGIN
   i := 1 ;
   n := HighIndice (pairs) ;
   WHILE i <= n DO
      p := GetIndice (pairs, i) ;
      IF (p # NIL) AND (p^.pairStatus = s)
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
   doCheck - keep obtaining an unresolved pair and check for the
             type compatibility.  This is the main check routine used by
             parameter, assignment and expression compatibility.
             It tests all unknown pairs and calls the appropriate
             check function
*)

PROCEDURE doCheck (tinfo: tInfo) : BOOLEAN ;
VAR
   result     : status ;
   left, right: CARDINAL ;
BEGIN
   WHILE get (tinfo^.unresolved, left, right, unknown) DO
      IF NOT in (tinfo^.visited, left, right)
      THEN
         result := tinfo^.checkFunc (unknown, tinfo, left, right) ;
         IF isKnown (result)
         THEN
            (* remove this pair from the unresolved list.  *)
            exclude (tinfo^.unresolved, left, right) ;
            include (tinfo^.resolved, left, right, result) ;  (* is this needed?  *)
            RETURN result = true
         END
      END
   END ;
   RETURN TRUE
END doCheck ;


(*
   in - returns TRUE if the pair is in the list.
*)

PROCEDURE in (pairs: Index; left, right: CARDINAL) : BOOLEAN ;
VAR
   i, n: CARDINAL ;
   p   : pair ;
BEGIN
   i := 1 ;
   n := HighIndice (pairs) ;
   WHILE i <= n DO
      p := GetIndice (pairs, i) ;
      IF (p # NIL) AND (p^.left = left) AND (p^.right = right)
      THEN
         RETURN TRUE
      END ;
      INC (i)
   END ;
   RETURN FALSE
END in ;


(*
   newPair -
*)

PROCEDURE newPair () : pair ;
VAR
   p: pair ;
BEGIN
   IF pairFreeList = NIL
   THEN
      NEW (p)
   ELSE
      p := pairFreeList ;
      pairFreeList := p^.next
   END ;
   Assert (p # NIL) ;
   RETURN p
END newPair ;


(*
   disposePair - adds pair, p, to the free list.
*)

PROCEDURE disposePair (p: pair) ;
BEGIN
   p^.next := pairFreeList ;
   pairFreeList := p
END disposePair ;


(*
   deconstructIndex -
*)

PROCEDURE deconstructIndex (pairs: Index) : Index ;
VAR
   p   : pair ;
   i, n: CARDINAL ;
BEGIN
   i := 1 ;
   n := HighIndice (pairs) ;
   WHILE i <= n DO
      p := GetIndice (pairs, i) ;
      IF p # NIL
      THEN
         disposePair (p)
      END ;
      INC (i)
   END ;
   RETURN KillIndex (pairs)
END deconstructIndex ;


(*
   deconstruct - deallocate the List data structure.
*)

PROCEDURE deconstruct (tinfo: tInfo) ;
BEGIN
   tinfo^.visited := deconstructIndex (tinfo^.visited) ;
   tinfo^.resolved := deconstructIndex (tinfo^.resolved) ;
   tinfo^.unresolved := deconstructIndex (tinfo^.unresolved)
END deconstruct ;


(*
   newtInfo -
*)

PROCEDURE newtInfo () : tInfo ;
VAR
   tinfo: tInfo ;
BEGIN
   IF tinfoFreeList = NIL
   THEN
      NEW (tinfo)
   ELSE
      tinfo := tinfoFreeList ;
      tinfoFreeList := tinfoFreeList^.next
   END ;
   RETURN tinfo
END newtInfo ;


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
   tinfo := newtInfo () ;
   formalT := GetSType (formal) ;
   actualT := GetSType (actual) ;
   tinfo^.kind := parameter ;
   tinfo^.actual := actual ;
   tinfo^.formal := formal ;
   tinfo^.procedure := procedure ;
   tinfo^.nth := nth ;
   tinfo^.error := NIL ;
   tinfo^.left := formalT ;
   tinfo^.right := actualT ;
   tinfo^.checkFunc := determineParameterCompatible ;
   tinfo^.visited := InitIndex (1) ;
   tinfo^.resolved := InitIndex (1) ;
   tinfo^.unresolved := InitIndex (1) ;
   include (tinfo^.unresolved, actual, formal, unknown) ;
   IF doCheck (tinfo)
   THEN
      deconstruct (tinfo) ;
      RETURN TRUE
   ELSE
      deconstruct (tinfo) ;
      RETURN FALSE
   END
END ParameterTypeCompatible ;


(*
   init - initialise all global data structures for this module.
*)

PROCEDURE init ;
BEGIN
   pairFreeList  := NIL ;
   tinfoFreeList := NIL
END init ;


BEGIN
   init
END M2Check.
