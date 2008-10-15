(* Copyright (C) 2008 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE M2MetaError ;


FROM NameKey IMPORT Name, KeyToCharStar, NulName ;
FROM M2LexBuf IMPORT GetTokenNo ;
FROM M2Error IMPORT Error, NewError, NewWarning, ErrorString, InternalError, ChainError, FlushErrors ;
FROM FIO IMPORT StdOut, WriteLine ;
FROM SFIO IMPORT WriteS ;

FROM DynamicStrings IMPORT String, InitString, InitStringCharStar,
                           ConCat, ConCatChar, Mark, string, KillString,
                           Dup, char, Length, Mult ;

FROM SymbolTable IMPORT IsDefImp, IsModule, IsInnerModule,
                        IsUnknown, IsType, IsProcedure, IsParameter,
                        IsParameterUnbounded, IsParameterVar, IsVarParam,
                        IsUnboundedParam, IsPointer, IsRecord, IsVarient,
                        IsFieldVarient, IsEnumeration, IsFieldEnumeration,
                        IsUnbounded, IsArray, IsRecordField, IsProcType,
                        IsVar, IsConst, IsConstString, IsConstLit, IsConstSet,
                        IsConstructor, IsDummy, IsTemporary, IsVarAParam,
                        IsSubscript, IsSubrange, IsSet, IsHiddenType,
                        IsError, GetSymName, GetScope, IsExported,
                        GetType, SkipType, GetDeclared, GetFirstUsed ;

TYPE
   errorType = (error, warning, chained) ;


(* %%%FORWARD%%%
PROCEDURE ebnf (VAR e: Error; VAR t: errorType;
                VAR r: String; s: String;
                sym: ARRAY OF CARDINAL; count: CARDINAL;
                VAR i: INTEGER; l: INTEGER) ; FORWARD ;
   %%%FORWARD%%% *)


(*
   ebnf := { percent
             | lbra
             | any                  % copy ch %
           }
         =:

   percent := '%' anych             % copy anych %
            =:

   lbra := '{' percenttoken '}' =:

   percenttoken := '%' (
                         '1'        % doOperand(1) %
                             op
                       | '2'        % doOperand(2) %
                             op
                       | '3'        % doOperand(3) %
                             op
                       )
                       } =:

   op := {'a'|'q'|'t'|'d'|'s'|'D'|'U'|'E'|'W'} then =:

   then := [ ':' ebnf ] =:
*)


(*
   InternalFormat - produces an informative internal error.
*)

PROCEDURE InternalFormat (s: String; i: INTEGER; m: ARRAY OF CHAR) ;
VAR
   e: Error ;
BEGIN
   e := NewError(GetTokenNo()) ;
   s := WriteS(StdOut, s) ;
   WriteLine(StdOut) ;
   s := KillString(s) ;
   IF i>0
   THEN
      DEC(i)
   END ;
   s := Mult(InitString(' '), i) ;
   s := ConCatChar(s, '^') ;
   s := WriteS(StdOut, s) ;
   WriteLine(StdOut) ;
   InternalError(m, __FILE__, __LINE__)
END InternalFormat ;


(*
   then := [ ':' ebnf ] =:
*)

PROCEDURE then (VAR e: Error; VAR t: errorType;
                VAR r: String; s: String;
                sym: ARRAY OF CARDINAL; count: CARDINAL;
                VAR i: INTEGER; l: INTEGER;
                o: String) ;
BEGIN
   IF char(s, i)=':'
   THEN
      INC(i) ;
      IF Length(o)>0
      THEN
         ebnf(e, t, r, s, sym, count, i, l)
      ELSE
         ebnf(e, t, r, s, sym, 0, i, l)
      END ;
      IF (i<l) AND (char(s, i)#'}')
      THEN
         InternalFormat(s, i, 'expecting to see }')
      END
   END
END then ;


PROCEDURE doAscii (bol: CARDINAL; count: CARDINAL;
                   sym: ARRAY OF CARDINAL; o: String) : String ;
BEGIN
   IF (Length(o)>0) OR (count=0)
   THEN
      RETURN( o )
   ELSE
      RETURN( ConCat(o, InitStringCharStar(KeyToCharStar(GetSymName(sym[bol])))) )
   END
END doAscii ;
   

PROCEDURE doQualified (bol: CARDINAL; count: CARDINAL;
                       sym: ARRAY OF CARDINAL; o: String) : String ;
VAR
   mod: ARRAY [0..1] OF CARDINAL ;
BEGIN
   IF (Length(o)>0) OR (count=0)
   THEN
      RETURN( o )
   ELSE
      mod[0] := GetScope(sym[bol]) ;
      IF IsDefImp(mod[0]) AND IsExported(mod[0], sym[bol])
      THEN
         o := doAscii(0, 1, mod, o) ;
         o := ConCatChar(o, '.') ;
         o := ConCat(o, InitStringCharStar(KeyToCharStar(GetSymName(sym[bol]))))
      ELSE
         o := doAscii(bol, count, sym, o)
      END ;
      RETURN( o )
   END
END doQualified ;


(*
   doType - returns a string containing the type name of
            sym.  It will skip pseudonym types.  It also
            returns the type symbol found.
*)

PROCEDURE doType (bol: CARDINAL; count: CARDINAL;
                  VAR sym: ARRAY OF CARDINAL; o: String) : String ;
BEGIN
   IF (Length(o)>0) OR (count=0)
   THEN
      RETURN( o )
   ELSE
      sym[bol] := GetType(sym[bol]) ;
      WHILE IsType(sym[bol]) AND (GetSymName(sym[bol])=NulName) DO
         IF IsType(sym[bol])
         THEN
            sym[bol] := GetType(sym[bol])
         END
      END ;
      IF GetSymName(sym[bol])=NulName
      THEN
         RETURN( o )
      ELSE
         RETURN( doAscii(bol, count, sym, o) )
      END
   END
END doType ;


(*
   doSkipType - will skip all pseudonym types.  It also
                returns the type symbol found and name.
*)

PROCEDURE doSkipType (bol: CARDINAL; count: CARDINAL;
                      VAR sym: ARRAY OF CARDINAL; o: String) : String ;
BEGIN
   IF (Length(o)>0) OR (count=0)
   THEN
      RETURN( o )
   ELSE
      sym[bol] := SkipType(sym[bol]) ;
      IF GetSymName(sym[bol])=NulName
      THEN
         RETURN( o )
      ELSE
         RETURN( doAscii(bol, count, sym, o) )
      END
   END
END doSkipType ;


(*
   doError - creates and returns an error note.
*)

PROCEDURE doError (e: Error; t: errorType; tok: CARDINAL) : Error ;
BEGIN
   CASE t OF

   chained:  IF e=NIL
             THEN
                InternalError('should not be chaining an error onto an empty error note', __FILE__, __LINE__)
             ELSE
                e := ChainError(tok, e)
             END |
   error  :  IF e=NIL
             THEN
                e := NewError(tok)
             END |
   warning:  IF e=NIL
             THEN
                e := NewWarning(tok)
             END

   ELSE
      InternalError('unexpected enumeration value', __FILE__, __LINE__)
   END ;
   RETURN( e )
END doError ;


(*
   doDeclared - creates an error note where sym[bol] was declared.
*)

PROCEDURE doDeclared (e: Error; t: errorType;
                      bol: CARDINAL; count: CARDINAL;
                      sym: ARRAY OF CARDINAL) : Error ;
BEGIN
   IF (count>HIGH(sym)) AND (bol<=HIGH(sym))
   THEN
      e := doError(e, t, GetDeclared(sym[bol]))
   END ;
   RETURN( e )
END doDeclared ;


(*
   doUsed - creates an error note where sym[bol] was first used.
*)

PROCEDURE doUsed (e: Error; t: errorType;
                  bol: CARDINAL; count: CARDINAL;
                  sym: ARRAY OF CARDINAL) : Error ;
BEGIN
   IF (count>HIGH(sym)) AND (bol<=HIGH(sym))
   THEN
      e := doError(e, t, GetFirstUsed(sym[bol]))
   END ;
   RETURN( e )
END doUsed ;


(*
   ConCatWord - joins sentances, a, b, together.
*)

PROCEDURE ConCatWord (a, b: String) : String ;
BEGIN
   IF (Length(a)=1) AND (char(a, 0)='a')
   THEN
      a := ConCatChar(a, 'n')
   ELSIF (Length(a)>1) AND (char(a, -1)='a') AND (char(a, -2)=' ')
   THEN
      a := ConCatChar(a, 'n')
   END ;
   IF Length(a)>0
   THEN
      a := ConCatChar(a, ' ')
   END ;
   RETURN( ConCat(a, b) )
END ConCatWord ;


(*
   symDesc - 
*)

PROCEDURE symDesc (sym: CARDINAL; o: String) : String ;
BEGIN
   IF IsTemporary(sym) OR IsDummy(sym) OR IsUnknown(sym)
   THEN
      RETURN( o )
   ELSIF IsConstLit(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('constant literal'))) )
   ELSIF IsConstSet(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('constant set'))) )
   ELSIF IsConstructor(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('constructor'))) )
   ELSIF IsConst(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('constant'))) )
   ELSIF IsArray(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('array'))) )
   ELSIF IsVar(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('variable'))) )
   ELSIF IsEnumeration(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('enumeration type'))) )
   ELSIF IsFieldEnumeration(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('enumeration field'))) )
   ELSIF IsUnbounded(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('unbounded parameter'))) )
   ELSIF IsProcType(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('procedure type'))) )
   ELSIF IsProcedure(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('procedure'))) )
   ELSIF IsPointer(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('pointer'))) )
   ELSIF IsParameterVar(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('var parameter'))) )
   ELSIF IsParameter(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('parameter'))) )
   ELSIF IsType(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('type'))) )
   ELSIF IsRecord(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('record'))) )
   ELSIF IsRecordField(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('record field'))) )
   ELSIF IsVarient(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('varient record'))) )
   ELSIF IsModule(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('module'))) )
   ELSIF IsDefImp(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('definition or implementation module'))) )
   ELSIF IsSet(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('set'))) )
   ELSIF IsSubrange(sym)
   THEN
      RETURN( ConCatWord(o, Mark(InitString('subrange'))) )
   ELSE
      RETURN( o )
   END
END symDesc ;


(*
   doDesc - 
*)

PROCEDURE doDesc (bol: CARDINAL; count: CARDINAL; sym: ARRAY OF CARDINAL; o: String) : String ;
BEGIN
   IF (Length(o)>0) OR (count=0)
   THEN
      RETURN( o )
   ELSE
      RETURN( symDesc(sym[bol], o) )
   END
END doDesc ;


(*
   addQuoted - if, o, is not empty then add it to, r.
*)

PROCEDURE addQuoted (r, o: String) : String ;
BEGIN
   IF Length(o)>0
   THEN
      r := ConCatChar(r, "'") ;
      r := ConCat(r, o) ;
      r := ConCatChar(r, "'")
   END ;
   RETURN( r )
END addQuoted ;


(*
   copySym - copies, n+1, symbols, from, ->, to.
*)

PROCEDURE copySym (from: ARRAY OF CARDINAL; VAR to: ARRAY OF CARDINAL; n: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   IF n>HIGH(to)
   THEN
      InternalError('not enough room in the destination array', __FILE__, __LINE__)
   ELSE
      i := 0 ;
      WHILE i<=n DO
         to[i] := from[i] ;
         INC(i)
      END
   END
END copySym ;


(*
   op := {'a'|'q'|'t'|'d'|'s'|'D'|'U'|'E'|'W'} then =:
*)

PROCEDURE op (VAR e: Error; VAR t: errorType;
              VAR r: String; s: String;
              sym: ARRAY OF CARDINAL; count: CARDINAL;
              VAR i: INTEGER; l: INTEGER;
              bol: CARDINAL) ;
VAR
   o: String ;
   c: ARRAY [0..2] OF CARDINAL ;
BEGIN
   copySym(sym, c, HIGH(sym)) ;
   o := InitString('') ;
   WHILE (i<l) AND (char(s, i)#'}') DO
      CASE char(s, i) OF

      'a':  o := doAscii(bol, count, sym, o) |
      'q':  o := doQualified(bol, count, sym, o) |
      't':  o := doType(bol, count, sym, o) |
      'd':  o := doDesc(bol, count, sym, o) |
      's':  o := doSkipType(bol, count, sym, o) |
      'D':  e := doDeclared(e, t, bol, count, sym) |
      'U':  e := doUsed(e, t, bol, count, sym) |
      'E':  t := error |
      'W':  t := warning |
      ':':  copySym(c, sym, HIGH(sym)) ;
            then(e, t, r, s, sym, count, i, l, o) ;
            o := KillString(o) ;
            IF (i<l) AND (char(s, i)#'}')
            THEN
               InternalFormat(s, i, 'expecting to see }')
            END ;
            DEC(i)

      ELSE
         InternalFormat(s, i, 'expecting one of [aqtdsDUEW:]')
      END ;
      INC(i) ;
   END ;
   r := addQuoted(r, o) ;
   o := KillString(o)
END op ;


(*
   percenttoken := '%' (
                         '1'        % doOperand(1) %
                             op
                       | '2'        % doOperand(2) %
                             op
                       | '3'        % doOperand(3) %
                             op
                       )
                       } =:
*)

PROCEDURE percenttoken (VAR e: Error; VAR t: errorType;
                        VAR r: String; s: String;
                        sym: ARRAY OF CARDINAL; count: CARDINAL;
                        VAR i: INTEGER; l: INTEGER) ;
BEGIN
   IF char(s, i)='%'
   THEN
      INC(i) ;
      CASE char(s, i) OF

      '1':  INC(i) ;
            op(e, t, r, s, sym, count, i, l, 0) |
      '2':  INC(i) ;
            op(e, t, r, s, sym, count, i, l, 1) |
      '3':  INC(i) ;
            op(e, t, r, s, sym, count, i, l, 2)

      ELSE
         InternalFormat(s, i, 'expecting one of [123]')
      END ;
      IF (i<l) AND (char(s, i)#'}')
      THEN
         InternalFormat(s, i, 'expecting to see }')
      END
   END
END percenttoken ;


(*
   percent := '%' anych           % copy anych %
            =:
*)

PROCEDURE percent (VAR r: String; s: String;
                   sym: ARRAY OF CARDINAL; count: CARDINAL;
                   VAR i: INTEGER; l: INTEGER) ;
BEGIN
   IF char(s, i)='%'
   THEN
      INC(i) ;
      IF i<l
      THEN
         r := ConCatChar(r, char(s, i)) ;
         INC(i)
      END
   END
END percent ;


(*
   lbra := '{' percenttoken '}' =:
*)

PROCEDURE lbra (VAR e: Error; VAR t: errorType;
                VAR r: String; s: String;
                sym: ARRAY OF CARDINAL; count: CARDINAL;
                VAR i: INTEGER; l: INTEGER) ;
BEGIN
   IF char(s, i)='{'
   THEN
      INC(i) ;
      percenttoken(e, t, r, s, sym, count, i, l) ;
      IF (i<l) AND (char(s, i)#'}')
      THEN
         InternalFormat(s, i, 'expecting to see }')
      END
   END
END lbra ;


(*
   ebnf := { percent
             | lbra
             | any                    % copy ch %
           }
         =:
*)

PROCEDURE ebnf (VAR e: Error; VAR t: errorType;
                VAR r: String; s: String;
                sym: ARRAY OF CARDINAL; count: CARDINAL;
                VAR i: INTEGER; l: INTEGER) ;
BEGIN
   WHILE i<l DO
      CASE char(s, i) OF

      '%':  percent(r, s, sym, count, i, l) |
      '{':  lbra(e, t, r, s, sym, count, i, l) ;
            IF (i<l) AND (char(s, i)#'}')
            THEN
               InternalFormat(s, i, 'expecting to see }')
            END |
      '}':  RETURN

      ELSE
         r := ConCatChar(r, char(s, i))
      END ;
      INC(i)
   END
END ebnf ;

  
(*
   doFormat - 
*)

PROCEDURE doFormat (VAR e: Error; VAR t: errorType;
                    s: String; sym: ARRAY OF CARDINAL) : String ;
VAR
   r   : String ;
   i, l: INTEGER ;
BEGIN
   r := InitString('') ;
   i := 0 ;
   l := Length(s) ;
   ebnf(e, t, r, s, sym, 1, i, l) ;
   s := KillString(s) ;
   RETURN( r )
END doFormat ;


PROCEDURE MetaErrorT1 (tok: CARDINAL; m: ARRAY OF CHAR; s: CARDINAL) ;
VAR
   str: String ;
   e  : Error ;
   sym: ARRAY [0..0] OF CARDINAL ;
   t  : errorType ;
BEGIN
   e := NIL ;
   sym[0] := s ;
   t := error ;
   str := doFormat(e, t, InitString(m), sym) ;
   e := doError(e, t, tok) ;
   ErrorString(e, str)
END MetaErrorT1 ;


PROCEDURE MetaErrorT2 (tok: CARDINAL; m: ARRAY OF CHAR; s1, s2: CARDINAL) ;
VAR
   str: String ;
   e  : Error ;
   sym: ARRAY [0..1] OF CARDINAL ;
   t  : errorType ;
BEGIN
   e := NIL ;
   sym[0] := s1 ;
   sym[1] := s2 ;
   t := error ;
   str := doFormat(e, t, InitString(m), sym) ;
   e := doError(e, t, tok) ;
   ErrorString(e, str)
END MetaErrorT2 ;


PROCEDURE MetaErrorT3 (tok: CARDINAL; m: ARRAY OF CHAR; s1, s2, s3: CARDINAL) ;
VAR
   str: String ;
   e  : Error ;
   sym: ARRAY [0..2] OF CARDINAL ;
   t  : errorType ;
BEGIN
   e := NIL ;
   sym[0] := s1 ;
   sym[1] := s2 ;
   sym[2] := s3 ;
   t := error ;
   str := doFormat(e, t, InitString(m), sym) ;
   e := doError(e, t, tok) ;
   ErrorString(e, str)
END MetaErrorT3 ;


PROCEDURE MetaError1 (m: ARRAY OF CHAR; s: CARDINAL) ;
BEGIN
   MetaErrorT1(GetTokenNo(), m, s)
END MetaError1 ;


PROCEDURE MetaError2 (m: ARRAY OF CHAR; s1, s2: CARDINAL) ;
BEGIN
   MetaErrorT2(GetTokenNo(), m, s1, s2)
END MetaError2 ;


PROCEDURE MetaError3 (m: ARRAY OF CHAR; s1, s2, s3: CARDINAL) ;
BEGIN
   MetaErrorT3(GetTokenNo(), m, s1, s2, s3)
END MetaError3 ;


(*
   wrapErrors - 
*)

PROCEDURE wrapErrors (tok: CARDINAL;
                      m1, m2: ARRAY OF CHAR;
                      sym: ARRAY OF CARDINAL) ;
VAR
   e, f: Error ;
   str : String ;
   t   : errorType ;
BEGIN
   e := NIL ;
   t := error ;
   str := doFormat(e, t, InitString(m1), sym) ;
   e := doError(e, t, tok) ;
   ErrorString(e, str) ;
   f := e ;
   t := chained ;
   str := doFormat(f, t, InitString(m2), sym) ;
   IF e=f
   THEN
      t := chained ;
      f := doError(e, t, tok)
   END ;
   ErrorString(f, str)
END wrapErrors ;


PROCEDURE MetaErrorsT1 (tok: CARDINAL; m1, m2: ARRAY OF CHAR; s: CARDINAL) ;
VAR
   sym: ARRAY [0..0] OF CARDINAL ;
BEGIN
   sym[0] := s ;
   wrapErrors(tok, m1, m2, sym)
END MetaErrorsT1 ;


PROCEDURE MetaErrorsT2 (tok: CARDINAL; m1, m2: ARRAY OF CHAR; s1, s2: CARDINAL) ;
VAR
   sym: ARRAY [0..1] OF CARDINAL ;
BEGIN
   sym[0] := s1 ;
   sym[1] := s2 ;
   wrapErrors(tok, m1, m2, sym)
END MetaErrorsT2 ;


PROCEDURE MetaErrorsT3 (tok: CARDINAL; m1, m2: ARRAY OF CHAR; s1, s2, s3: CARDINAL) ;
VAR
   sym : ARRAY [0..2] OF CARDINAL ;
BEGIN
   sym[0] := s1 ;
   sym[1] := s2 ;
   sym[2] := s3 ;
   wrapErrors(tok, m1, m2, sym)
END MetaErrorsT3 ;


PROCEDURE MetaErrors1 (m1, m2: ARRAY OF CHAR; s: CARDINAL) ;
BEGIN
   MetaErrorsT1(GetTokenNo(), m1, m2, s)
END MetaErrors1 ;


PROCEDURE MetaErrors2 (m1, m2: ARRAY OF CHAR; s1, s2: CARDINAL) ;
BEGIN
   MetaErrorsT2(GetTokenNo(), m1, m2, s1, s2)
END MetaErrors2 ;


PROCEDURE MetaErrors3 (m1, m2: ARRAY OF CHAR; s1, s2, s3: CARDINAL) ;
BEGIN
   MetaErrorsT3(GetTokenNo(), m1, m2, s1, s2, s3)
END MetaErrors3 ;


END M2MetaError.
