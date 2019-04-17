(* M2MetaError.mod provides a set of high level error routines.

Copyright (C) 2008-2019 Free Software Foundation, Inc.
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
along with GNU Modula-2; see the file COPYING.  If not,
see <https://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE M2MetaError ;


FROM M2Base IMPORT ZType, RType ;
FROM NameKey IMPORT Name, KeyToCharStar, NulName ;
FROM StrLib IMPORT StrLen ;
FROM M2LexBuf IMPORT GetTokenNo ;
FROM M2Error IMPORT Error, NewError, NewWarning, NewNote, ErrorString, InternalError, ChainError, FlushErrors, SetColor ;
FROM FIO IMPORT StdOut, WriteLine ;
FROM SFIO IMPORT WriteS ;
FROM StringConvert IMPORT ctos ;
FROM M2Printf IMPORT printf1, printf0 ;
FROM M2Options IMPORT LowerCaseKeywords ;
FROM StrCase IMPORT Lower ;
FROM libc IMPORT printf ;
FROM SYSTEM IMPORT ADDRESS ;

FROM DynamicStrings IMPORT String, InitString, InitStringCharStar,
                           ConCat, ConCatChar, Mark, string, KillString,
                           Dup, char, Length, Mult, EqualArray ;

FROM SymbolTable IMPORT NulSym,
                        IsDefImp, IsModule, IsInnerModule,
                        IsUnknown, IsType, IsProcedure, IsParameter,
                        IsParameterUnbounded, IsParameterVar, IsVarParam,
                        IsUnboundedParam, IsPointer, IsRecord, IsVarient,
                        IsFieldVarient, IsEnumeration, IsFieldEnumeration,
                        IsUnbounded, IsArray, IsRecordField, IsProcType,
                        IsVar, IsConst, IsConstString, IsConstLit, IsConstSet,
                        IsConstructor, IsDummy, IsTemporary, IsVarAParam,
                        IsSubscript, IsSubrange, IsSet, IsHiddenType,
                        IsError, GetSymName, GetScope, IsExported,
                        GetType, SkipType, GetDeclaredDef, GetDeclaredMod,
                        GetFirstUsed, IsNameAnonymous ;

IMPORT M2ColorString ;


CONST
   MaxStack  = 100 ;
   Debugging = FALSE ;

TYPE
   errorType = (none, error, warning, note, chained) ;
   colorType = (noColor, quoteColor, filenameColor, errorColor,
                warningColor, noteColor, keywordColor, locusColor,
                insertColor, deleteColor, typeColor, range1Color, range2Color) ;

   errorBlock = RECORD
                   e         : Error ;
                   type      : errorType ;
                   out, in   : String ;
                   highplus1 : CARDINAL ;
                   len,
                   ini, outi : INTEGER ;
                   chain,
                   root,
                   quotes,
                   positive  : BOOLEAN ;
                   cachedCol,               (* the color sent down the device/string.         *)
                   currentCol: colorType ;  (* the color of the text before being sent down.  *)
                END ;
VAR
   colorStack: ARRAY [0..MaxStack] OF colorType ;
   stackPtr  : CARDINAL ;
   lastRoot  : Error ;
   lastColor : colorType ;


(*
   lookupColor - looks up the color enum from the string.
*)

PROCEDURE lookupColor (s: String) : colorType ;
BEGIN
   IF EqualArray (s, "filename")
   THEN
      RETURN filenameColor
   ELSIF EqualArray (s, "quote")
   THEN
      RETURN quoteColor
   ELSIF EqualArray (s, "error")
   THEN
      RETURN errorColor
   ELSIF EqualArray (s, "warning")
   THEN
      RETURN warningColor ;
   ELSIF EqualArray (s, "note")
   THEN
      RETURN warningColor ;
   ELSIF EqualArray (s, "locus")
   THEN
      RETURN locusColor
   ELSIF EqualArray (s, "insert")
   THEN
      RETURN insertColor
   ELSIF EqualArray (s, "delete")
   THEN
      RETURN deleteColor
   ELSIF EqualArray (s, "type")
   THEN
      RETURN typeColor
   ELSIF EqualArray (s, "range1")
   THEN
      RETURN range1Color
   ELSIF EqualArray (s, "range2")
   THEN
      RETURN range2Color
   END ;
   RETURN noColor
END lookupColor ;


(*
   readColor -
*)

PROCEDURE readColor (eb: errorBlock) : colorType ;
VAR
   s: String ;
   c: colorType ;
BEGIN
   s := InitString ('') ;
   WHILE (eb.ini < eb.len) AND (char (eb.in, eb.ini) # "}") DO
      IF char (eb.in, eb.ini) = "%"
      THEN
         INC (eb.ini)
      END ;
      s := ConCatChar (s, char (eb.in, eb.ini)) ;
      INC (eb.ini)
   END ;
   c := lookupColor (s) ;
   s := KillString (s) ;
   RETURN noColor
END readColor ;


(*
   keyword -
*)

PROCEDURE keyword (VAR eb: errorBlock) ;
BEGIN
   IF char (eb.in, eb.ini) = 'K'
   THEN
      INC (eb.ini) ;
      flushColor (eb) ;
      pushColor (eb.currentCol) ;
      eb.currentCol := keywordColor ;
      flushColor (eb) ;
      WHILE (eb.ini < eb.len) AND (char (eb.in, eb.ini) # "}") DO
         IF Debugging
         THEN
            dump (eb)
         END ;
         IF char (eb.in, eb.ini) = "%"
         THEN
            INC (eb.ini)
         END ;
         copyKeywordChar (eb) ;
         INC (eb.ini)
      END ;
      flushColor (eb) ;
      eb.currentCol := popColor ()
   ELSE
      InternalError ('expecting index to be on the K for keyword', __FILE__, __LINE__)
   END
END keyword ;


(*
   pushColor -
*)

PROCEDURE pushColor (c: colorType) ;
BEGIN
   IF stackPtr > MaxStack
   THEN
      HALT
   ELSE
      colorStack[stackPtr] := c ;
      INC (stackPtr)
   END
END pushColor ;


(*
   popColor -
*)

PROCEDURE popColor () : colorType ;
BEGIN
   IF stackPtr > 0
   THEN
      DEC (stackPtr)
   ELSE
      HALT
   END ;
   RETURN colorStack[stackPtr]
END popColor ;


(*
   initErrorBlock - initialise an error block with the, input, string.
*)

PROCEDURE initErrorBlock (VAR eb: errorBlock; input: String; sym: ARRAY OF CARDINAL) ;
BEGIN
   WITH eb DO
      e          := NIL ;
      type       := error ;  (* default to the error color.  *)
      out        := InitString ('') ;
      in         := input ;
      highplus1  := HIGH (sym) + 1 ;
      len        := Length (input) ;
      ini        := 0 ;
      outi       := 0 ;
      quotes     := FALSE ;
      positive   := TRUE ;
      root       := FALSE ;
      chain      := FALSE ;
      cachedCol  := noColor ;
      currentCol := findColorType (input)
   END
END initErrorBlock ;


(*
   findColorType - return the color of the string.  This is determined by the first
                   occurrance of an error, warning or note marker.  An error message
                   is assumed to either be: a keyword category, error category, note
                   category, warning category or to be chained from a previous error.
*)

PROCEDURE findColorType (s: String) : colorType ;
VAR
   i: CARDINAL ;
BEGIN
   i := 0 ;
   WHILE i < Length (s) DO
      IF char (s, i) = "{"
      THEN
         INC (i) ;
         IF char (s, i) = "%"
         THEN
            INC (i) ;
            WHILE (i < Length (s)) AND (char (s, i) # "}") DO
               IF char (s, i) = "%"
               THEN
                  INC (i)
               END ;
               CASE char (s, i) OF

               "K":  RETURN errorColor |   (* keyword errors start with the fatal error color.  *)
               "E":  RETURN errorColor |
               "O":  RETURN noteColor |
               "W":  RETURN warningColor |
               "C":  RETURN lastColor

               ELSE
               END ;
               INC (i)
            END
         END
      END ;
      INC (i)
   END ;
   RETURN errorColor  (* default to the error color.  *)
END findColorType ;


(*
   killErrorBlock - deallocates the dynamic strings associated with the error block.
*)

PROCEDURE killErrorBlock (VAR eb: errorBlock) ;
BEGIN
   WITH eb DO
      out := KillString (out) ;
      in := KillString (in)
   END
END killErrorBlock ;


(*
   ebnf := { percent
             | lbra
             | any                  % copy ch %
           }
         =:

   percent := '%' ( "<" |           % open quote
                    ">" |           % close quote
                    anych )         % copy anych %
            =:

   lbra := '{' [ '!' ] percenttoken '}' =:

   percenttoken := '%' (
                         '1'        % doOperand(1) %
                             op
                       | '2'        % doOperand(2) %
                             op
                       | '3'        % doOperand(3) %
                             op
                       | '4'        % doOperand(4) %
                             op
                       )
                       } =:

   op := {'a'|'q'|'t'|'d'|'n'|'s'|'D'|'I'|'U'|'E'|'W'} then =:

   then := [ ':' ebnf ] =:
*)


(*
   InternalFormat - produces an informative internal error.
*)

PROCEDURE InternalFormat (s: String; i: INTEGER; m: ARRAY OF CHAR; line: CARDINAL) ;
VAR
   e: Error ;
BEGIN
   InternalError (m, __FILE__, line) ;
   e := NewError (GetTokenNo ()) ;
   s := WriteS (StdOut, s) ;
   WriteLine (StdOut) ;
   s := KillString (s) ;
   IF i > 0
   THEN
      DEC (i)
   END ;
   s := Mult (InitString (' '), i) ;
   s := ConCatChar (s, '^') ;
   s := WriteS (StdOut, s) ;
   WriteLine (StdOut) ;
   InternalError (m, __FILE__, line)
END InternalFormat ;


(*
   x - checks to see that a=b.
*)

PROCEDURE x (a, b: String) : String ;
BEGIN
   IF a # b
   THEN
      InternalError ('different string returned', __FILE__, __LINE__)
   END ;
   RETURN a
END x ;


(*
   IsWhite - returns TRUE if, ch, is a space.
*)

PROCEDURE IsWhite (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN ch = ' '
END IsWhite ;


(*
   skip - skips over this level input until the next '}'.
*)

PROCEDURE skip (VAR sb: errorBlock) ;
VAR
   level: INTEGER ;
BEGIN
   level := 0 ;
   WHILE sb.ini < sb.len DO
      IF (level = 0) AND (char (sb.in, sb.ini) = "}")
      THEN
         RETURN
      END ;
      IF char (sb.in, sb.ini) = "}"
      THEN
         DEC (level)
      ELSIF char (sb.in, sb.ini) = "{"
      THEN
         INC (level)
      END ;
      INC (sb.ini)
   END
END skip ;


(*
   ifNulThen := [ ':' ebnf ] =:
*)

PROCEDURE ifNulThen (VAR sb: errorBlock;
                     sym: ARRAY OF CARDINAL;
                     operand: String) ;
BEGIN
   IF char (sb.in, sb.ini) = ':'
   THEN
      INC (sb.ini) ;
      IF sb.positive
      THEN
         IF Length (operand) = 0
         THEN
            (* carry on processing input text.  *)
            ebnf (sb, sym)
         ELSE
            (* skip over this level of input text.  *)
            skip (sb)
         END
      ELSE
         IF Length (operand) = 0
         THEN
            (* skip over this level of input text.  *)
            skip (sb)
         ELSE
            (* carry on processing input text.  *)
            ebnf (sb, sym)
         END
      END ;
      IF (sb.ini < sb.len) AND (char (sb.in, sb.ini) # '}')
      THEN
         InternalFormat (sb.in, sb.ini, 'expecting to see }', __LINE__)
      END
   END
END ifNulThen ;


(*
   doNumber -
*)

PROCEDURE doNumber (VAR sb: errorBlock;
                    sym: ARRAY OF CARDINAL; bol: CARDINAL; operand: String) : String ;
BEGIN
   IF Length (operand) > 0
   THEN
      RETURN operand
   ELSE
      sb.quotes := FALSE ;
      RETURN ConCat (operand, ctos (sym[bol], 0, ' '))
   END
END doNumber ;


(*
   doCount -
*)

PROCEDURE doCount (VAR sb: errorBlock;
                   sym: ARRAY OF CARDINAL; bol: CARDINAL; operand: String) : String ;
BEGIN
   IF Length (operand) > 0
   THEN
      RETURN operand
   ELSE
      sb.quotes := FALSE ;
      operand := ConCat (operand, ctos(sym[bol], 0, ' ')) ;
      CASE sym[bol] MOD 100 OF

      11..13:  operand := ConCat (operand, Mark (InitString ('th')))

      ELSE
         CASE sym[bol] MOD 10 OF

         1:  operand := ConCat (operand, Mark (InitString ('st'))) |
         2:  operand := ConCat (operand, Mark (InitString ('nd'))) |
         3:  operand := ConCat (operand, Mark (InitString ('rd')))

         ELSE
            operand := ConCat (operand, Mark (InitString ('th')))
         END
      END ;
      RETURN operand
   END
END doCount ;


PROCEDURE doAscii (sym: ARRAY OF CARDINAL; bol: CARDINAL; operand: String) : String ;
BEGIN
   IF (Length (operand) > 0) OR IsTemporary (sym[bol]) OR IsNameAnonymous (sym[bol])
   THEN
      RETURN operand
   ELSE
      RETURN ConCat (operand, InitStringCharStar (KeyToCharStar (GetSymName (sym[bol]))))
   END
END doAscii ;


PROCEDURE doName (VAR sb: errorBlock;
                  sym: ARRAY OF CARDINAL; bol: CARDINAL; operand: String) : String ;
BEGIN
   IF (Length (operand) > 0) OR IsTemporary (sym[bol]) OR IsNameAnonymous (sym[bol])
   THEN
      RETURN operand
   ELSE
      IF sym[bol] = ZType
      THEN
         sb.quotes := FALSE ;
         RETURN ConCat (operand, Mark(InitString('the ZType')))
      ELSIF sym[bol] = RType
      THEN
         sb.quotes := FALSE ;
         RETURN ConCat (operand, Mark (InitString ('the RType')))
      ELSE
         RETURN doAscii (sym, bol, operand)
      END
   END
END doName ;


PROCEDURE doQualified (sym: ARRAY OF CARDINAL; bol: CARDINAL; operand: String) : String ;
VAR
   mod: ARRAY [0..1] OF CARDINAL ;
BEGIN
   IF (Length (operand) > 0) OR IsTemporary (sym[bol]) OR IsNameAnonymous (sym[bol])
   THEN
      RETURN operand
   ELSE
      mod[0] := GetScope (sym[bol]) ;
      IF IsDefImp (mod[0]) AND IsExported (mod[0], sym[bol])
      THEN
         operand := doAscii (mod, 0, operand) ;
         operand := ConCatChar (operand, '.') ;
         operand := ConCat (operand, InitStringCharStar (KeyToCharStar (GetSymName (sym[bol]))))
      ELSE
         operand := doAscii (sym, bol, operand)
      END ;
      RETURN operand
   END
END doQualified ;


(*
   doType - returns a string containing the type name of
            sym.  It will skip pseudonym types.  It also
            returns the type symbol found.
*)

PROCEDURE doType (sym: ARRAY OF CARDINAL; bol: CARDINAL; operand: String) : String ;
BEGIN
   IF (Length(operand) > 0) OR (GetType (sym[bol]) = NulSym)
   THEN
      RETURN operand
   ELSE
      sym[bol] := GetType(sym[bol]) ;
      WHILE IsType(sym[bol]) AND ((GetSymName(sym[bol])=NulName) OR
                                  IsNameAnonymous(sym[bol])) DO
         sym[bol] := GetType (sym[bol])
      END ;
      RETURN doAscii (sym, bol, operand)
   END
END doType ;


(*
   doSkipType - will skip all pseudonym types.  It also
                returns the type symbol found and name.
*)

PROCEDURE doSkipType (sym: ARRAY OF CARDINAL; bol: CARDINAL; operand: String) : String ;
BEGIN
   IF (Length(operand) > 0) OR (GetType (sym[bol]) = NulSym)
   THEN
      RETURN operand
   ELSE
      sym[bol] := SkipType(sym[bol]) ;
      WHILE IsType(sym[bol]) AND ((GetSymName(sym[bol])=NulName) OR
                                  IsNameAnonymous(sym[bol])) DO
         sym[bol] := GetType (sym[bol])
      END ;
      RETURN doAscii (sym, bol, operand)
   END
END doSkipType ;


(*
   doChain -
*)

PROCEDURE doChain (VAR eb: errorBlock; tok: CARDINAL) ;
BEGIN
   IF lastRoot=NIL
   THEN
      InternalError('should not be chaining an error onto an empty error note', __FILE__, __LINE__)
   ELSE
      eb.e := ChainError (tok, lastRoot)
   END
END doChain ;


(*
   doError - creates and returns an error note.
*)

PROCEDURE doError (VAR eb: errorBlock; tok: CARDINAL) ;
BEGIN
   IF eb.chain
   THEN
      doChain (eb, tok)
   ELSE
      CASE eb.type OF

      chained:  doChain (eb, tok) |
      none,
      error  :  IF eb.e=NIL
                THEN
                   eb.e := NewError (tok)
                END |
      warning:  IF eb.e=NIL
                THEN
                   eb.e := NewWarning (tok)
                END |
      note   :  IF eb.e=NIL
                THEN
                   eb.e := NewNote (tok)
                END

      ELSE
         InternalError('unexpected enumeration value', __FILE__, __LINE__)
      END
   END ;
   IF eb.root
   THEN
      lastRoot := eb.e ;
      lastColor := findColorType (eb.in)
   END ;
   eb.e := SetColor (eb.e)
END doError ;


(*
   declaredDef - creates an error note where sym[bol] was declared.
*)

PROCEDURE declaredDef (eb: errorBlock; sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
BEGIN
   IF bol <= HIGH (sym)
   THEN
      doError (eb, GetDeclaredDef (sym[bol]))
   END
END declaredDef ;


(*
   doDeclaredMod - creates an error note where sym[bol] was declared.
*)

PROCEDURE declaredMod (eb: errorBlock; sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
BEGIN
   IF bol <= HIGH (sym)
   THEN
      doError (eb, GetDeclaredMod (sym[bol]))
   END
END declaredMod ;


(*
   used - creates an error note where sym[bol] was first used.
*)

PROCEDURE used (eb: errorBlock; sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
BEGIN
   IF bol <= HIGH (sym)
   THEN
      doError (eb, GetFirstUsed (sym[bol]))
   END
END used ;


(*
   ConCatWord - joins sentances, a, b, together.
*)

PROCEDURE ConCatWord (a, b: String) : String ;
BEGIN
   IF (Length (a) = 1) AND (char(a, 0) = 'a')
   THEN
      a := x (a, ConCatChar (a, 'n'))
   ELSIF (Length (a) > 1) AND (char (a, -1) = 'a') AND IsWhite (char(a, -2))
   THEN
      a := x (a, ConCatChar (a, 'n'))
   END ;
   IF (Length (a) > 0) AND (NOT IsWhite (char (a, -1)))
   THEN
      a := x (a, ConCatChar (a, ' '))
   END ;
   RETURN x (a, ConCat(a, b))
END ConCatWord ;


(*
   symDesc -
*)

PROCEDURE symDesc (sym: CARDINAL; o: String) : String ;
BEGIN
   IF IsConstLit(sym)
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
   ELSIF IsParameter(sym)
   THEN
      IF IsParameterVar(sym)
      THEN
         RETURN( ConCatWord(o, Mark(InitString('var parameter'))) )
      ELSE
         RETURN( ConCatWord(o, Mark(InitString('parameter'))) )
      END
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

PROCEDURE doDesc (VAR sb: errorBlock;
                  sym: ARRAY OF CARDINAL; bol: CARDINAL; operand: String) : String ;
BEGIN
   IF Length (operand) > 0
   THEN
      RETURN operand
   ELSE
      operand := symDesc (sym[bol], operand) ;
      IF Length (operand) > 0
      THEN
         sb.quotes := FALSE
      END ;
      RETURN operand
   END
END doDesc ;


(*
   addQuoted - if, o, is not empty then add it to, r.
*)

PROCEDURE addQuoted (VAR eb: errorBlock; o: String) ;
BEGIN
   IF Length (o) > 0
   THEN
      IF (Length (eb.out) > 0) AND (NOT IsWhite (char (eb.out, -1)))
      THEN
         eb.out := ConCatChar (eb.out, " ")
      END ;
      IF eb.quotes
      THEN
         flushColor (eb) ;
         pushColor (eb.currentCol) ;
         eb.currentCol := quoteColor ;
         flushColor (eb) ;
         eb.out := M2ColorString.quoteOpen (eb.out)
      END ;
      eb.out := ConCat (eb.out, o) ;
      IF eb.quotes
      THEN
         flushColor (eb) ;
         eb.out := M2ColorString.quoteClose (eb.out) ;
         flushColor (eb) ;
         eb.currentCol := popColor ()
      END
   END
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
   op := {'a'|'q'|'t'|'d'|'n'|'s'|'D'|'I'|'U'|'E'|'W'} then =:
*)

PROCEDURE op (VAR eb: errorBlock;
              sym: ARRAY OF CARDINAL; bol: CARDINAL) ;
VAR
   o: String ;
BEGIN
   o := InitString ('') ;
   eb.quotes := TRUE ;
   WHILE (eb.ini < eb.len) AND (char (eb.in, eb.ini) # '}') DO
      IF Debugging
      THEN
         printf0 ("while loop in op\n") ;
         dump (eb) ;
         printf1 ("o = %s\n", o)
      END ;
      CASE char (eb.in, eb.ini) OF

      '!':  eb.positive := NOT eb.positive |
      'a':  o := doName (eb, sym, bol, o) |
      'q':  o := doQualified (sym, bol, o) |
      't':  o := doType (sym, bol, o) |
      'd':  o := doDesc (eb, sym, bol, o) |
      'n':  o := doNumber (eb, sym, bol, o) |
      'N':  o := doCount (eb, sym, bol, o) |
      's':  o := doSkipType (sym, bol, o) |
      'D':  declaredDef (eb, sym, bol) |
      'M':  declaredMod (eb, sym, bol) |
      'U':  used (eb, sym, bol) |
      'E':  eb.type := error |
      'W':  eb.type := warning |
      'O':  eb.type := note |
      'C':  eb.chain := TRUE |
      'R':  eb.root := TRUE |
      'P':  pushColor (eb.currentCol) |
      'p':  eb.currentCol := popColor () |
      'c':  eb.currentCol := readColor (eb) ;
            DEC (eb.ini) |
      'K':  keyword (eb) ;
            DEC (eb.ini) |
      ':':  ifNulThen (eb, sym, o) ;
            o := KillString (o) ;
            o := InitString ('') ;
            DEC (eb.ini)

      ELSE
         InternalFormat (eb.in, eb.ini, 'expecting one of [aqtdnpsCDEKNPOUW:<>%]', __LINE__)
      END ;
      INC (eb.ini)
   END ;
   addQuoted (eb, o) ;
   IF Debugging
   THEN
      printf0 ("finishing op\n") ;
      dump (eb)
   END ;
   o := KillString (o)
END op ;


(*
   percenttoken := '%' (
                         '1'        % doOperand(1) %
                             op
                       | '2'        % doOperand(2) %
                             op
                       | '3'        % doOperand(3) %
                             op
                       | '4'        % doOperand(4) %
                             op
                       )
                       } =:
*)

PROCEDURE percenttoken (VAR eb: errorBlock; sym: ARRAY OF CARDINAL) ;
BEGIN
   IF char (eb.in, eb.ini) = '%'
   THEN
      INC (eb.ini) ;
      CASE char (eb.in, eb.ini) OF

      '1':  INC (eb.ini) ;
            op (eb, sym, 0) |
      '2':  INC (eb.ini) ;
            op (eb, sym, 1) |
      '3':  INC (eb.ini) ;
            op (eb, sym, 2) |
      '4':  INC (eb.ini) ;
            op (eb, sym, 3)

      ELSE
         op (eb, sym, 0)
      END ;
      IF (eb.ini < eb.len) AND (char (eb.in, eb.ini) # '}')
      THEN
         InternalFormat (eb.in, eb.ini, 'expecting to see }', __LINE__)
      END
   END
END percenttoken ;


(*
   flushColor - flushes any outstanding color change.
*)

PROCEDURE flushColor (VAR eb: errorBlock) ;
BEGIN
   IF eb.cachedCol # eb.currentCol
   THEN
      IF eb.cachedCol # noColor
      THEN
         eb.out := M2ColorString.endColor (eb.out)
      END ;
      emitColor (eb, eb.currentCol) ;
      eb.cachedCol := eb.currentCol
   END
END flushColor ;


(*
   emitColor - adds the appropriate color string to the output string.
*)

PROCEDURE emitColor (VAR eb: errorBlock; c: colorType) ;
BEGIN
   CASE c OF

   noColor      :  eb.out := M2ColorString.endColor (eb.out) |
   quoteColor   :  eb.out := M2ColorString.quoteColor (eb.out) |
   filenameColor:  eb.out := M2ColorString.filenameColor (eb.out) |
   errorColor   :  eb.out := M2ColorString.errorColor (eb.out) |
   warningColor :  eb.out := M2ColorString.warningColor (eb.out) |
   noteColor    :  eb.out := M2ColorString.noteColor (eb.out) |
   keywordColor :  eb.out := M2ColorString.locusColor (eb.out) |
   locusColor   :  eb.out := M2ColorString.locusColor (eb.out) |
   insertColor  :  eb.out := M2ColorString.insertColor (eb.out) |
   deleteColor  :  eb.out := M2ColorString.deleteColor (eb.out) |
   typeColor    :  eb.out := M2ColorString.typeColor (eb.out) |
   range1Color  :  eb.out := M2ColorString.range1Color (eb.out) |
   range2Color  :  eb.out := M2ColorString.range2Color (eb.out)

   END
END emitColor ;


(*
   copyChar - copies a character from in string to out string.
*)

PROCEDURE copyChar (VAR eb: errorBlock) ;
BEGIN
   IF eb.ini < eb.len
   THEN
      flushColor (eb) ;
      eb.out := x (eb.out, ConCatChar (eb.out, char (eb.in, eb.ini)))
   END
END copyChar ;


(*
   copyKeywordChar - copies a character from in string to out string
                     it will convert the character to lower case if the
                     -fm2-lower-case option was specified.
*)

PROCEDURE copyKeywordChar (VAR eb: errorBlock) ;
VAR
   ch: CHAR ;
BEGIN
   IF eb.ini < eb.len
   THEN
      flushColor (eb) ;
      ch := char (eb.in, eb.ini) ;
      IF LowerCaseKeywords
      THEN
         ch := Lower (ch)
      END ;
      eb.out := x (eb.out, ConCatChar (eb.out, ch))
   END
END copyKeywordChar ;


(*
   percent := '%' anych           % copy anych %
            =:
*)

PROCEDURE percent (VAR eb: errorBlock; sym: ARRAY OF CARDINAL) ;
BEGIN
   IF char (eb.in, eb.ini)='%'
   THEN
      INC (eb.ini) ;
      IF eb.ini < eb.len
      THEN
         IF char (eb.in, eb.ini) = '<'
         THEN
            flushColor (eb) ;
            pushColor (eb.currentCol) ;
            eb.currentCol := quoteColor ;
            flushColor (eb) ;
            eb.out := M2ColorString.quoteOpen (eb.out)
         ELSIF char (eb.in, eb.ini) = '>'
         THEN
            flushColor (eb) ;
            eb.out := M2ColorString.quoteClose (eb.out) ;
            eb.currentCol := popColor ()
         ELSE
            copyChar (eb)
         END
      END
   END
END percent ;


(*
   lbra := '{' [ '!' ] percenttoken '}' =:
*)

PROCEDURE lbra (VAR eb: errorBlock; sym: ARRAY OF CARDINAL) ;
VAR
   positive: BOOLEAN ;
BEGIN
   IF char (eb.in, eb.ini) = '{'
   THEN
      eb.positive := TRUE ;
      INC (eb.ini) ;
      IF char (eb.in, eb.ini) = '!'
      THEN
         eb.positive := FALSE ;
         INC (eb.ini)
      END ;
      IF char (eb.in, eb.ini) # '%'
      THEN
         InternalFormat (eb.in, eb.ini, 'expecting to see %', __LINE__)
      END ;
      percenttoken (eb, sym) ;
      IF (eb.ini < eb.len) AND (char (eb.in, eb.ini) # '}')
      THEN
         InternalFormat (eb.in, eb.ini, 'expecting to see }', __LINE__)
      END
   END
END lbra ;


PROCEDURE stop ; BEGIN END stop ;


(*
   dumpErrorType -
*)

PROCEDURE dumpErrorType (e: errorType) ;
BEGIN
   CASE e OF

   none   :  printf0 ("none") |
   error  :  printf0 ("error") |
   warning:  printf0 ("warning") |
   note   :  printf0 ("note") |
   chained:  printf0 ("chained")

   END
END dumpErrorType ;


(*
   dumpColorType -
*)

PROCEDURE dumpColorType (c: colorType) ;
BEGIN
   CASE c OF

   noColor      :  printf0 ("noColor") |
   quoteColor   :  printf0 ("quoteColor") |
   filenameColor:  printf0 ("filenameColor") |
   errorColor   :  printf0 ("errorColor") |
   warningColor :  printf0 ("warningColor") |
   noteColor    :  printf0 ("noteColor") |
   keywordColor :  printf0 ("keywordColor") |
   locusColor   :  printf0 ("locusColor") |
   insertColor  :  printf0 ("insertColor") |
   deleteColor  :  printf0 ("deleteColor") |
   typeColor    :  printf0 ("typeColor") |
   range1Color  :  printf0 ("range1Color") |
   range2Color  :  printf0 ("range2Color")

   END
END dumpColorType ;


(*
   dump -

*)

PROCEDURE dump (eb: errorBlock) ;
VAR
   ch: CHAR ;
BEGIN
   printf0 ("\n\nerrorBlock\n") ;
   printf0 ("\ntype      = ") ; dumpErrorType (eb.type) ;
   printf1 ("\nout       = %s", eb.out) ;
   printf1 ("\nin        = %s", eb.in) ;
   printf1 ("\nlen       = %d", eb.len) ;
   printf1 ("\nhighplus1 = %d", eb.highplus1) ;
   printf1 ("\nquotes    = %d", eb.quotes) ;
   printf1 ("\npositive  = %d", eb.positive) ;
   printf0 ("\ncachedCol = ") ; dumpColorType (eb.cachedCol) ;
   printf0 ("\ncurrentCol = ") ; dumpColorType (eb.currentCol) ;
   printf1 ("\nini        = %d", eb.ini) ;
   IF eb.ini < eb.len
   THEN
      ch := char (eb.in, eb.ini) ;
      printf1 ("\ncurrent char = %c", ch)
   END ;
   printf0 ("\n")
END dump ;


(*
   ebnf := { percent
             | lbra
             | any                    % copy ch %
           }
         =:
*)

PROCEDURE ebnf (VAR eb: errorBlock; sym: ARRAY OF CARDINAL) ;
BEGIN
   IF Debugging
   THEN
      printf0 ("top of ebnf\n") ;
      dump (eb)
   END ;
   WHILE eb.ini < eb.len DO
      IF Debugging
      THEN
         printf0 ("while loop ebnf\n") ;
         dump (eb)
      END ;
      CASE char (eb.in, eb.ini) OF

      '!':  eb.positive := NOT eb.positive |
      '%':  percent (eb, sym) |
      '{':  lbra (eb, sym) ;
            IF (eb.ini < eb.len) AND (char (eb.in, eb.ini) # '}')
            THEN
               InternalFormat (eb.in, eb.ini, 'expecting to see }', __LINE__)
            END |
      '}':  RETURN

      ELSE
         IF ((IsWhite (char (eb.in, eb.ini)) AND (Length (eb.out) > 0) AND
              (NOT IsWhite (char (eb.out, -1)))) OR
            (NOT IsWhite (char (eb.in, eb.ini)))) AND (eb.highplus1 > 0)
         THEN
            flushColor (eb) ;
            eb.out := x (eb.out, ConCatChar (eb.out, char (eb.in, eb.ini)))
         END
      END ;
      INC (eb.ini)
   END ;
   eb.currentCol := noColor ;
   flushColor (eb) ;
   IF Debugging
   THEN
      printf0 ("finishing ebnf\n") ;
      dump (eb)
   END
END ebnf ;


PROCEDURE MetaErrorStringT0 (tok: CARDINAL; m: String) ;
VAR
   eb : errorBlock ;
   sym: ARRAY [0..0] OF CARDINAL ;
BEGIN
   sym[0] := NulSym ;
   initErrorBlock (eb, m, sym) ;
   ebnf (eb, sym) ;
   flushColor (eb) ;
   doError (eb, tok) ;
   ErrorString (eb.e, Dup (eb.out)) ;
   killErrorBlock (eb)
END MetaErrorStringT0 ;


PROCEDURE MetaErrorStringT1 (tok: CARDINAL; m: String; s: CARDINAL) ;
VAR
   eb : errorBlock ;
   sym: ARRAY [0..0] OF CARDINAL ;
BEGIN
   sym[0] := s ;
   initErrorBlock (eb, m, sym) ;
   ebnf (eb, sym) ;
   flushColor (eb) ;
   doError (eb, tok) ;
   ErrorString (eb.e, Dup (eb.out)) ;
   killErrorBlock (eb)
END MetaErrorStringT1 ;


PROCEDURE MetaErrorT1 (tok: CARDINAL; m: ARRAY OF CHAR; s: CARDINAL) ;
BEGIN
   MetaErrorStringT1(tok, InitString(m), s)
END MetaErrorT1 ;


PROCEDURE MetaErrorStringT2 (tok: CARDINAL; m: String; s1, s2: CARDINAL) ;
VAR
   eb : errorBlock ;
   sym: ARRAY [0..1] OF CARDINAL ;
BEGIN
   sym[0] := s1 ;
   sym[1] := s2 ;
   initErrorBlock (eb, m, sym) ;
   ebnf (eb, sym) ;
   flushColor (eb) ;
   doError (eb, tok) ;
   ErrorString (eb.e, Dup (eb.out)) ;
   killErrorBlock (eb)
END MetaErrorStringT2 ;


PROCEDURE MetaErrorT2 (tok: CARDINAL; m: ARRAY OF CHAR; s1, s2: CARDINAL) ;
BEGIN
   MetaErrorStringT2 (tok, InitString (m), s1, s2)
END MetaErrorT2 ;


PROCEDURE MetaErrorStringT3 (tok: CARDINAL; m: String; s1, s2, s3: CARDINAL) ;
VAR
   eb : errorBlock ;
   sym: ARRAY [0..2] OF CARDINAL ;
BEGIN
   sym[0] := s1 ;
   sym[1] := s2 ;
   sym[2] := s3 ;
   initErrorBlock (eb, m, sym) ;
   eb.highplus1 := HIGH (sym) + 1 ;
   ebnf (eb, sym) ;
   flushColor (eb) ;
   doError (eb, tok) ;
   ErrorString (eb.e, Dup (eb.out)) ;
   killErrorBlock (eb)
END MetaErrorStringT3 ;


PROCEDURE MetaErrorT3 (tok: CARDINAL; m: ARRAY OF CHAR; s1, s2, s3: CARDINAL) ;
BEGIN
   MetaErrorStringT3 (tok, InitString (m), s1, s2, s3) ;
END MetaErrorT3 ;


PROCEDURE MetaErrorStringT4 (tok: CARDINAL; m: String; s1, s2, s3, s4: CARDINAL) ;
VAR
   eb : errorBlock ;
   sym: ARRAY [0..3] OF CARDINAL ;
BEGIN
   sym[0] := s1 ;
   sym[1] := s2 ;
   sym[2] := s3 ;
   sym[3] := s4 ;
   initErrorBlock (eb, m, sym) ;
   ebnf (eb, sym) ;
   flushColor (eb) ;
   doError (eb, tok) ;
   ErrorString (eb.e, Dup (eb.out)) ;
   killErrorBlock (eb)
END MetaErrorStringT4 ;


PROCEDURE MetaErrorT4 (tok: CARDINAL; m: ARRAY OF CHAR; s1, s2, s3, s4: CARDINAL) ;
BEGIN
   MetaErrorStringT4 (tok, InitString (m), s1, s2, s3, s4) ;
END MetaErrorT4 ;


PROCEDURE MetaError1 (m: ARRAY OF CHAR; s: CARDINAL) ;
BEGIN
   MetaErrorT1 (GetTokenNo (), m, s)
END MetaError1 ;


PROCEDURE MetaError2 (m: ARRAY OF CHAR; s1, s2: CARDINAL) ;
BEGIN
   MetaErrorT2 (GetTokenNo (), m, s1, s2)
END MetaError2 ;


PROCEDURE MetaError3 (m: ARRAY OF CHAR; s1, s2, s3: CARDINAL) ;
BEGIN
   MetaErrorT3 (GetTokenNo (), m, s1, s2, s3)
END MetaError3 ;


PROCEDURE MetaError4 (m: ARRAY OF CHAR; s1, s2, s3, s4: CARDINAL) ;
BEGIN
   MetaErrorT4 (GetTokenNo (), m, s1, s2, s3, s4)
END MetaError4 ;


(*
   wrapErrors -
*)

PROCEDURE wrapErrors (tok: CARDINAL;
                      m1, m2: ARRAY OF CHAR;
                      sym: ARRAY OF CARDINAL) ;
VAR
   eb: errorBlock ;
BEGIN
   initErrorBlock (eb, InitString (m1), sym) ;
   ebnf (eb, sym) ;
   flushColor (eb) ;
   doError (eb, tok) ;
   lastRoot := eb.e ;
   ErrorString (eb.e, Dup (eb.out)) ;
   killErrorBlock (eb) ;
   initErrorBlock (eb, InitString (m2), sym) ;
   eb.type := chained ;
   ebnf (eb, sym) ;
   flushColor (eb) ;
   doError (eb, tok) ;
   ErrorString (eb.e, Dup (eb.out)) ;
   killErrorBlock (eb)
END wrapErrors ;


PROCEDURE MetaErrorsT1 (tok: CARDINAL; m1, m2: ARRAY OF CHAR; s: CARDINAL) ;
VAR
   sym: ARRAY [0..0] OF CARDINAL ;
BEGIN
   sym[0] := s ;
   wrapErrors (tok, m1, m2, sym)
END MetaErrorsT1 ;


PROCEDURE MetaErrorsT2 (tok: CARDINAL; m1, m2: ARRAY OF CHAR; s1, s2: CARDINAL) ;
VAR
   sym: ARRAY [0..1] OF CARDINAL ;
BEGIN
   sym[0] := s1 ;
   sym[1] := s2 ;
   wrapErrors (tok, m1, m2, sym)
END MetaErrorsT2 ;


PROCEDURE MetaErrorsT3 (tok: CARDINAL; m1, m2: ARRAY OF CHAR; s1, s2, s3: CARDINAL) ;
VAR
   sym : ARRAY [0..2] OF CARDINAL ;
BEGIN
   sym[0] := s1 ;
   sym[1] := s2 ;
   sym[2] := s3 ;
   wrapErrors (tok, m1, m2, sym)
END MetaErrorsT3 ;


PROCEDURE MetaErrorsT4 (tok: CARDINAL; m1, m2: ARRAY OF CHAR; s1, s2, s3, s4: CARDINAL) ;
VAR
   sym : ARRAY [0..3] OF CARDINAL ;
BEGIN
   sym[0] := s1 ;
   sym[1] := s2 ;
   sym[2] := s3 ;
   sym[3] := s4 ;
   wrapErrors (tok, m1, m2, sym)
END MetaErrorsT4 ;


PROCEDURE MetaErrors1 (m1, m2: ARRAY OF CHAR; s: CARDINAL) ;
BEGIN
   MetaErrorsT1 (GetTokenNo (), m1, m2, s)
END MetaErrors1 ;


PROCEDURE MetaErrors2 (m1, m2: ARRAY OF CHAR; s1, s2: CARDINAL) ;
BEGIN
   MetaErrorsT2 (GetTokenNo (), m1, m2, s1, s2)
END MetaErrors2 ;


PROCEDURE MetaErrors3 (m1, m2: ARRAY OF CHAR; s1, s2, s3: CARDINAL) ;
BEGIN
   MetaErrorsT3 (GetTokenNo (), m1, m2, s1, s2, s3)
END MetaErrors3 ;


PROCEDURE MetaErrors4 (m1, m2: ARRAY OF CHAR; s1, s2, s3, s4: CARDINAL) ;
BEGIN
   MetaErrorsT4 (GetTokenNo (), m1, m2, s1, s2, s3, s4)
END MetaErrors4 ;


PROCEDURE MetaErrorString1 (m: String; s: CARDINAL) ;
BEGIN
   MetaErrorStringT1 (GetTokenNo (), m, s)
END MetaErrorString1 ;


PROCEDURE MetaErrorString2 (m: String; s1, s2: CARDINAL) ;
BEGIN
   MetaErrorStringT2 (GetTokenNo (), m, s1, s2)
END MetaErrorString2 ;


PROCEDURE MetaErrorString3 (m: String; s1, s2, s3: CARDINAL) ;
BEGIN
   MetaErrorStringT3 (GetTokenNo (), m, s1, s2, s3)
END MetaErrorString3 ;


PROCEDURE MetaErrorString4 (m: String; s1, s2, s3, s4: CARDINAL) ;
BEGIN
   MetaErrorStringT4 (GetTokenNo (), m, s1, s2, s3, s4)
END MetaErrorString4 ;


BEGIN
   lastRoot := NIL ;
   lastColor := noColor
END M2MetaError.
