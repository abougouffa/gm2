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
IMPLEMENTATION MODULE M2Reserved ;


FROM SymbolKey IMPORT SymbolTree, InitTree, PutSymKey, GetSymKey ;
FROM NameKey IMPORT MakeKey ;
FROM ASCII IMPORT nul ;

CONST
   eof = 032C ;

VAR
   NameTotok,
   tokToName: SymbolTree ;


(*
   AddKeyword - adds the Name and enumerated value of a keyword
                into the binary tree.
*)

PROCEDURE AddKeyword (n: Name; tok: toktype) ;
BEGIN
   PutSymKey(NameTotok, n, tok) ;
   PutSymKey(tokToName, VAL(Name, tok), n)
END AddKeyword ;


PROCEDURE Init ;
VAR
   a: ARRAY [0..1] OF CHAR ;
BEGIN
   InitTree(NameTotok) ;
   InitTree(tokToName) ;

   NulTok := NIL ;

   PlusTok := MakeKey('+') ;
   AddKeyword(PlusTok, ORD(plustok)) ;

   MinusTok := MakeKey('-') ;
   AddKeyword(MinusTok, ORD(minustok)) ;

   TimesTok := MakeKey('*') ;
   AddKeyword(TimesTok, ORD(timestok)) ;

   DivideTok := MakeKey('/') ;
   AddKeyword(DivideTok, ORD(dividetok)) ;

   BecomesTok := MakeKey(':=') ;
   AddKeyword(BecomesTok, ORD(becomestok)) ;

   AmbersandTok := MakeKey('&') ;
   AddKeyword(AmbersandTok, ORD(ambersandtok)) ;

   PeriodTok := MakeKey('.') ;
   AddKeyword(PeriodTok, ORD(periodtok)) ;

   CommaTok := MakeKey(',') ;
   AddKeyword(CommaTok, ORD(commatok)) ;

   SemiColonTok := MakeKey(';') ;
   AddKeyword(SemiColonTok, ORD(semicolontok)) ;

   LParaTok := MakeKey('(') ;
   AddKeyword(LParaTok, ORD(lparatok)) ;

   LSBraTok := MakeKey('[') ;
   AddKeyword(LSBraTok, ORD(lsbratok)) ;

   LCBraTok := MakeKey('{') ;
   AddKeyword(LCBraTok, ORD(lcbratok)) ;

   UpArrowTok := MakeKey('^') ;
   AddKeyword(UpArrowTok, ORD(uparrowtok)) ;

   SingleQuoteTok := MakeKey("'") ;
   AddKeyword(SingleQuoteTok, ORD(singlequotetok)) ;

   EqualTok := MakeKey('=') ;
   AddKeyword(EqualTok, ORD(equaltok)) ;

   HashTok := MakeKey('#') ;
   AddKeyword(HashTok, ORD(hashtok)) ;

   LessTok := MakeKey('<') ;
   AddKeyword(LessTok, ORD(lesstok)) ;

   GreaterTok := MakeKey('>') ;
   AddKeyword(GreaterTok, ORD(greatertok)) ;

   LessGreaterTok := MakeKey('<>') ;
   AddKeyword(LessGreaterTok, ORD(lessgreatertok)) ;

   LessEqualTok := MakeKey('<=') ;
   AddKeyword(LessEqualTok, ORD(lessequaltok)) ;

   GreaterEqualTok := MakeKey('>=') ;
   AddKeyword(GreaterEqualTok, ORD(greaterequaltok)) ;

   PeriodPeriodTok := MakeKey('..') ;
   AddKeyword(PeriodPeriodTok, ORD(periodperiodtok)) ;

   ColonTok := MakeKey(':') ;
   AddKeyword(ColonTok, ORD(colontok)) ;

   RParaTok := MakeKey(')') ;
   AddKeyword(RParaTok, ORD(rparatok)) ;

   RSBraTok := MakeKey(']') ;
   AddKeyword(RSBraTok, ORD(rsbratok)) ;

   RCBraTok := MakeKey('}') ;
   AddKeyword(RCBraTok, ORD(rcbratok)) ;

   BarTok := MakeKey('|') ;
   AddKeyword(BarTok, ORD(bartok)) ;

   DoubleQuotesTok := MakeKey('"') ;
   AddKeyword(DoubleQuotesTok, ORD(doublequotestok)) ;


   AndTok := MakeKey('AND') ;
   AddKeyword(AndTok, ORD(andtok)) ;

   ArrayTok := MakeKey('ARRAY') ;
   AddKeyword(ArrayTok, ORD(arraytok)) ;

   BeginTok := MakeKey('BEGIN') ;
   AddKeyword(BeginTok, ORD(begintok)) ;

   ByTok := MakeKey('BY') ;
   AddKeyword(ByTok, ORD(bytok)) ;

   CaseTok := MakeKey('CASE') ;
   AddKeyword(CaseTok, ORD(casetok)) ;

   ConstTok := MakeKey('CONST') ;
   AddKeyword(ConstTok, ORD(consttok)) ;

   DefinitionTok := MakeKey('DEFINITION') ;
   AddKeyword(DefinitionTok, ORD(definitiontok)) ;

   DivTok := MakeKey('DIV') ;
   AddKeyword(DivTok, ORD(divtok)) ;

   DoTok := MakeKey('DO') ;
   AddKeyword(DoTok, ORD(dotok)) ;

   ElseTok := MakeKey('ELSE') ;
   AddKeyword(ElseTok, ORD(elsetok)) ;

   ElsifTok := MakeKey('ELSIF') ;
   AddKeyword(ElsifTok, ORD(elsiftok)) ;

   EndTok := MakeKey('END') ;
   AddKeyword(EndTok, ORD(endtok)) ;

   ExitTok := MakeKey('EXIT') ;
   AddKeyword(ExitTok, ORD(exittok)) ;

   ExportTok := MakeKey('EXPORT') ;
   AddKeyword(ExportTok, ORD(exporttok)) ;

   ForTok := MakeKey('FOR') ;
   AddKeyword(ForTok, ORD(fortok)) ;

   FromTok := MakeKey('FROM') ;
   AddKeyword(FromTok, ORD(fromtok)) ;

   IfTok := MakeKey('IF') ;
   AddKeyword(IfTok, ORD(iftok)) ;

   ImplementationTok := MakeKey('IMPLEMENTATION') ;
   AddKeyword(ImplementationTok, ORD(implementationtok)) ;

   ImportTok := MakeKey('IMPORT') ;
   AddKeyword(ImportTok, ORD(importtok)) ;

   InTok := MakeKey('IN') ;
   AddKeyword(InTok, ORD(intok)) ;

   LoopTok := MakeKey('LOOP') ;
   AddKeyword(LoopTok, ORD(looptok)) ;

   ModTok := MakeKey('MOD') ;
   AddKeyword(ModTok, ORD(modtok)) ;

   ModuleTok := MakeKey('MODULE') ;
   AddKeyword(ModuleTok, ORD(moduletok)) ;

   NotTok := MakeKey('NOT') ;
   AddKeyword(NotTok, ORD(nottok)) ;

   OfTok := MakeKey('OF') ;
   AddKeyword(OfTok, ORD(oftok)) ;

   OrTok := MakeKey('OR') ;
   AddKeyword(OrTok, ORD(ortok)) ;

   PointerTok := MakeKey('POINTER') ;
   AddKeyword(PointerTok, ORD(pointertok)) ;

   ProcedureTok := MakeKey('PROCEDURE') ;
   AddKeyword(ProcedureTok, ORD(proceduretok)) ;

   QualifiedTok := MakeKey('QUALIFIED') ;
   AddKeyword(QualifiedTok, ORD(qualifiedtok)) ;

   UnQualifiedTok := MakeKey('UNQUALIFIED') ;
   AddKeyword(UnQualifiedTok, ORD(unqualifiedtok)) ;

   RecordTok := MakeKey('RECORD') ;
   AddKeyword(RecordTok, ORD(recordtok)) ;

   RepeatTok := MakeKey('REPEAT') ;
   AddKeyword(RepeatTok, ORD(repeattok)) ;

   ReturnTok := MakeKey('RETURN') ;
   AddKeyword(ReturnTok, ORD(returntok)) ;

   SetTok := MakeKey('SET') ;
   AddKeyword(SetTok, ORD(settok)) ;

   ThenTok := MakeKey('THEN') ;
   AddKeyword(ThenTok, ORD(thentok)) ;

   ToTok := MakeKey('TO') ;
   AddKeyword(ToTok, ORD(totok)) ;

   TypeTok := MakeKey('TYPE') ;
   AddKeyword(TypeTok, ORD(typetok)) ;

   UntilTok := MakeKey('UNTIL') ;
   AddKeyword(UntilTok, ORD(untiltok)) ;

   VarTok := MakeKey('VAR') ;
   AddKeyword(VarTok, ORD(vartok)) ;

   WhileTok := MakeKey('WHILE') ;
   AddKeyword(WhileTok, ORD(whiletok)) ;

   WithTok := MakeKey('WITH') ;
   AddKeyword(WithTok, ORD(withtok)) ;

   AsmTok := MakeKey('ASM') ;
   AddKeyword(AsmTok, ORD(asmtok)) ;

   VolatileTok := MakeKey('VOLATILE') ;
   AddKeyword(VolatileTok, ORD(volatiletok)) ;

   DateTok := MakeKey('__DATE__') ;     (* C compatible preprocessor primatives *)
   AddKeyword(DateTok, ORD(datetok)) ;

   LineTok := MakeKey('__LINE__') ;
   AddKeyword(LineTok, ORD(linetok)) ;

   FileTok := MakeKey('__FILE__') ;
   AddKeyword(FileTok, ORD(filetok)) ;

   AttributeTok := MakeKey('__ATTRIBUTE__') ; (* GCC extension incorporated into gm2 *)
   AddKeyword(AttributeTok, ORD(attributetok)) ;

   BuiltinTok := MakeKey('__BUILTIN__') ; (* GCC extension incorporated into gm2 *)
   AddKeyword(BuiltinTok, ORD(builtintok)) ;

   a[0] := eof ;
   a[1] := nul ;
   EofTok := MakeKey(a)              (* Not a reserved token *)
END Init ;


(*
   IsReserved - returns TRUE if the symbol, Name, is a reserved word.
                If TRUE it also sets tok to the appropriate enumerated
                value. It will set tok to eoftok if appropriate.
*)

PROCEDURE IsReserved (n: Name; VAR tok: toktype) : BOOLEAN ;
VAR
   t: CARDINAL ;
BEGIN
   t := GetSymKey(NameTotok, n) ;
   IF t=0
   THEN
      (* eoftok is not a reserved word *)
      IF n=EofTok
      THEN
         tok := eoftok
      END ;
      RETURN( FALSE )
   ELSE
      tok := VAL(toktype, t) ;
      RETURN( TRUE )
   END
END IsReserved ;


(*
   tokToTok - returns a Tok given the enumerated variable, t.
*)

PROCEDURE tokToTok (t: toktype) : Name ;
BEGIN
   RETURN( GetSymKey(tokToName, VAL(Name, t)) )
END tokToTok ;


BEGIN
   Init
END M2Reserved.
