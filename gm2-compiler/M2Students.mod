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
IMPLEMENTATION MODULE M2Students ;


FROM SymbolTable IMPORT FinalSymbol, IsVar, GetVarFather, IsProcedure, IsModule,
                        GetMainModule, IsType, NulSym, IsRecord, GetSymName, GetNth, GetNthProcedure, GetDeclared, NoOfParam ;
FROM NameKey IMPORT GetKey, WriteKey, MakeKey, IsSameExcludingCase, NulName ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM M2Lexical IMPORT FormatWarningMessage2, GetTokenNo ;
FROM StrLib IMPORT StrEqual, StrConCat, StrLen ;
FROM Lists IMPORT List, InitList, IsItemInList, IncludeItemIntoList ;
FROM M2Reserved IMPORT IsReserved, toktype ;
FROM ASCII IMPORT nul ;


VAR
   ErrantSymbols: List ;



(*
   IsNotADuplicate - returns TRUE if either s1 or s2 have been reported before.
*)

PROCEDURE IsNotADuplicate (s1, s2: CARDINAL) : BOOLEAN ;
BEGIN
   IF (NOT IsItemInList(ErrantSymbols, s1)) AND (NOT IsItemInList(ErrantSymbols, s2))
   THEN
      IncludeItemIntoList(ErrantSymbols, s1) ;
      IncludeItemIntoList(ErrantSymbols, s2) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsNotADuplicate ;


(*
   CheckForVariableThatLooksLikeKeyword - checks for a identifier that looks the same
                                          as a keyword except for its case.
*)

PROCEDURE CheckForVariableThatLooksLikeKeyword (a: ARRAY OF CHAR; name: CARDINAL) ;
VAR
   name2  : CARDINAL ;
   i, high: CARDINAL ;
   ch     : CHAR ;
   token  : toktype ;
BEGIN
   (* inlined StrToUpperCase for speed *)
   high := HIGH(a) ;
   i := 0 ;
   ch := a[0] ;
   WHILE (i<high) AND (ch#nul) DO
      IF (ch>='a') AND (ch<='z')
      THEN
         a[i] := CHR( ORD(ch)-ORD('a')+ORD('A') )
      END ;
      INC(i) ;
      ch := a[i]
   END ;
   (* end of inlined procedure *)
   name2 := MakeKey(a) ;
   IF IsReserved(name2, token)
   THEN
      IF IsNotADuplicate(name, name)
      THEN
         FormatWarningMessage2('either the identifier has the same name as a keyword or alternatively a keyword has the wrong case (%s and %s). Note that this symbol name is legal as an identifier, however as such it might cause confusion and is considered bad programming practice',
                                name, name2, GetTokenNo())
      END
   END
END CheckForVariableThatLooksLikeKeyword ;


(*
   CheckAsciiName - checks to see whether ascii names, s1, and, s2, are similar.
*)

PROCEDURE CheckAsciiName (previous, s1, newblock, s2: CARDINAL) ;
VAR
   n1, n2: CARDINAL ;
BEGIN
   n1 := GetSymName(s1) ;
   n2 := GetSymName(s2) ;
   IF (n1=n2) AND (n1#NulName)
   THEN
      IF IsNotADuplicate(s1, s2)
      THEN
         FormatWarningMessage2('idential symbol name in two different scopes, scope (%s) has symbol called (%s)',
                               GetSymName(previous), GetSymName(s1), GetDeclared(s1)) ;
         FormatWarningMessage2('idential symbol name in two different scopes, scope (%s) has symbol called (%s)',
                               GetSymName(newblock), GetSymName(s2), GetDeclared(s2))
      END
   ELSIF IsSameExcludingCase(n1, n2)
   THEN
      IF IsNotADuplicate(s1, s2)
      THEN
         FormatWarningMessage2('very similar symbol names (different case) in two different scopes, scope (%s) has symbol called (%s)',
                               GetSymName(previous), GetSymName(s1), GetDeclared(s1)) ;
         FormatWarningMessage2('very similar symbol names (different case) in two different scopes, scope (%s) has symbol called (%s)',
                               GetSymName(newblock), GetSymName(s2), GetDeclared(s2))
      END
   END
END CheckAsciiName ;


(*
   CheckProcedure - checks the procedure, p, for symbols which look like, s.
*)

PROCEDURE CheckProcedure (m, p: CARDINAL) ;
VAR
   i, n1,
   j, n2: CARDINAL ;
BEGIN
   IF p#NulSym
   THEN
      i := 1 ;   (* I would have used NoOfParam(p)+1 but Stuart wants parameters checked as well - maybe he is right *)
      REPEAT
         n1 := GetNth(p, i) ;
         IF n1#NulSym
         THEN
            IF IsVar(n1) OR IsType(n1) OR IsProcedure(n1) OR IsRecord(n1)
            THEN
               j := 1 ;
               REPEAT
                  n2 := GetNth(m, j) ;
                  IF n2#NulSym
                  THEN
                     IF IsVar(n2) OR IsType(n2) OR IsProcedure(n2) OR IsRecord(n2)
                     THEN
                        CheckAsciiName(m, n2, p, n1)
                     END
                  END ;
                  INC(j)
               UNTIL n2=NulSym
            END
         END ;
         INC(i)
      UNTIL n1=NulSym
   END
END CheckProcedure ;


(*
   CheckModule - checks the module, m, for symbols which look like, s.
*)

PROCEDURE CheckModule (m, s: CARDINAL) ;
VAR
   i, n: CARDINAL ;
BEGIN
   IF m#NulSym
   THEN
      i := 1 ;
      REPEAT
         n := GetNth(m, i) ;
         IF n#NulSym
         THEN
            IF (n#NulSym) AND (n#s)
            THEN
               IF IsVar(n) OR IsType(n) OR IsProcedure(n) OR IsRecord(n)
               THEN
                  CheckAsciiName(m, s, m, n)
               END
            END
         END ;
         INC(i)
      UNTIL n=NulSym
   END
END CheckModule ;


(*
   StudentVariableCheck - checks to see that variables are quite different from keywords and
                          issues an message if they are not. It ignores case so to catch
                          1st and 2nd semester programming errors.
*)

PROCEDURE StudentVariableCheck ;
VAR
   i, n, m: CARDINAL ;
BEGIN
   m := GetMainModule() ;
   (* first check global scope *)
   i := 1 ;
   REPEAT
      n := GetNth(m, i) ;
      IF n#NulSym
      THEN
         IF IsVar(n) OR IsType(n) OR IsProcedure(n) OR IsRecord(n)
         THEN
            CheckModule(m, n)
         END
      END ;
      INC(i)
   UNTIL n=NulSym ;
   (* now check local scope *)
   i := 1 ;
   REPEAT
      n := GetNthProcedure(m, i) ;
      IF n#NulSym
      THEN
         IF IsProcedure(n)
         THEN
            CheckProcedure(m, n)
         END
      END ;
      INC(i)
   UNTIL n=NulSym
END StudentVariableCheck ;


BEGIN
   InitList(ErrantSymbols)
END M2Students.
