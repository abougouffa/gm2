(* Copyright (C) 2001 Free Software Foundation, Inc. *)
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
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA *)
IMPLEMENTATION MODULE DynamicStrings ;

FROM libc IMPORT strlen ;
FROM StrLib IMPORT StrLen ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM SYSTEM IMPORT ADR ;
FROM ASCII IMPORT nul, tab ;

CONST
   MaxBuf   = 127 ;
   PoisonOn = TRUE ;    (* enable debugging, if FALSE no overhead is incurred *)

TYPE
   Contents = RECORD
                 buf : ARRAY [0..MaxBuf-1] OF CHAR ;
                 len : CARDINAL ;
                 next: String ;
              END ;

   Descriptor = POINTER TO descriptor ;   (* forward declaration necessary for p2c *)

   String = POINTER TO RECORD
                          contents: Contents ;
                          head    : Descriptor ;
                       END ;

   descriptor =            RECORD
                              charStarUsed : BOOLEAN ;     (* can we garbage collect this? *)
                              charStar     : ADDRESS ;
                              charStarSize : CARDINAL ;
                              charStarValid: BOOLEAN ;
                              state        : (inuse, marked, onlist, poisoned) ;
                              garbage      : String ;      (* temporary strings to be destroyed
                                                              once this string is killed *)
                           END ;


VAR
   captured: String ;  (* debugging aid *)

PROCEDURE Capture (s: String) : CARDINAL ;
BEGIN
   captured := s ;
   RETURN 1;
END Capture ;

(*
   Min - 
*)

PROCEDURE Min (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a<b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Min ;


(*
   Max - 
*)

PROCEDURE Max (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a>b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Max ;


(*
   ConcatContents - add the contents of string, a, where, h, is the
                    total length of, a. The offset is in, o.
*)

PROCEDURE ConcatContents (VAR c: Contents; a: ARRAY OF CHAR; h, o: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   i := c.len ;
   WHILE (o<h) AND (i<MaxBuf) DO
      c.buf[i] := a[o] ;
      INC(o) ;
      INC(i)
   END ;
   IF o<h
   THEN
      c.len := MaxBuf ;
      NEW(c.next) ;
      WITH c.next^ DO
         head := NIL ;
         contents.len := 0 ;
         contents.next := NIL ;
         ConcatContents(contents, a, h, o)         
      END
   ELSE
      c.len := i
   END
END ConcatContents ;


(*
   InitString - creates and returns a String type object.
                Initial contents are, a.
*)

PROCEDURE InitString (a: ARRAY OF CHAR) : String ;
VAR
   s: String ;
BEGIN
   NEW(s) ;
   WITH s^ DO
      WITH contents DO
         len := 0 ;
         next := NIL
      END ;
      ConcatContents(contents, a, StrLen(a), 0);
      NEW(head) ;
      WITH head^ DO
         charStarUsed  := FALSE ;
         charStar      := NIL ;
         charStarSize  := 0;
         charStarValid := FALSE ;
         garbage       := NIL ;
         state         := inuse
      END
   END ;
   RETURN( s )
END InitString ;


(*
   DeallocateCharStar - deallocates any charStar.
*)

PROCEDURE DeallocateCharStar (s: String) ;
BEGIN
   IF (s#NIL) AND (s^.head#NIL)
   THEN
      WITH s^.head^ DO
         IF charStarUsed AND (charStar#NIL)
         THEN
            DEALLOCATE(charStar, charStarSize)
         END ;
         charStarUsed  := FALSE ;
         charStar      := NIL ;
         charStarSize  := 0 ;
         charStarValid := FALSE
      END
   END
END DeallocateCharStar ;


(*
   CheckPoisoned - checks for a poisoned string, s.
*)

PROCEDURE CheckPoisoned (s: String) : String ;
BEGIN
   IF PoisonOn AND (s#NIL) AND (s^.head#NIL) AND (s^.head^.state=poisoned)
   THEN
      HALT
   END ;
   RETURN( s )
END CheckPoisoned ;


(*
   KillString - frees String, s, and its contents.
                NIL is returned.
*)

PROCEDURE KillString (s: String) : String ;
VAR
   t: String ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned(s)
   END ;
   IF s#NIL
   THEN
      IF PoisonOn
      THEN
         IF s^.head#NIL
         THEN
            s^.head^.state := poisoned
         END
      ELSE
         WITH s^ DO
            IF head#NIL
            THEN
               WITH head^ DO
                  garbage := KillString(garbage) ;
                  DeallocateCharStar(s)
               END
            END
         END ;
         t := KillString(s^.contents.next) ;
         DISPOSE(s)
      END
   END ;
   RETURN( NIL )
END KillString ;


(*
   Fin - finishes with a string, it calls KillString with, s.
         The purpose of the procedure is to provide a short cut
         to calling KillString and then testing the return result.
*)

PROCEDURE Fin (s: String) ;
BEGIN
   IF KillString(s)#NIL
   THEN
      HALT
   END
END Fin ;


(*
   MarkInvalid - marks the char * version of String, s, as invalid.
*)

PROCEDURE MarkInvalid (s: String) ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned(s)
   END ;
   IF s^.head#NIL
   THEN
      s^.head^.charStarValid := FALSE
   END
END MarkInvalid ;


(*
   ConcatContentsAddress - concatenate the string, a, where, h, is the
                           total length of, a.
*)

PROCEDURE ConcatContentsAddress (VAR c: Contents; a: ADDRESS; h: CARDINAL) ;
VAR
   p   : POINTER TO CHAR ;
   i, j: CARDINAL ;
BEGIN
   j := 0 ;
   i := c.len ;
   p := a ;
   WHILE (j<h) AND (i<MaxBuf) DO
      c.buf[i] := p^ ;
      INC(i) ;
      INC(j) ;
      INC(p)
   END ;
   IF j<h
   THEN
      c.len := MaxBuf ;
      NEW(c.next) ;
      WITH c.next^ DO
         head         := NIL ;
         contents.len := 0 ;
         ConcatContentsAddress(contents, p, h-j)
      END
   ELSE
      c.len := i
   END
END ConcatContentsAddress ;


(*
   InitStringCharStar - initializes and returns a String to contain the C string.
*)

PROCEDURE InitStringCharStar (a: ADDRESS) : String ;
VAR
   s: String ;
BEGIN
   NEW(s) ;
   WITH s^ DO
      WITH contents DO
         len := 0 ;
         next := NIL
      END ;
      IF a#NIL
      THEN
         ConcatContentsAddress(contents, a, strlen(a))
      END ;
      NEW(head) ;
      WITH head^ DO
         charStarUsed  := FALSE ;
         charStar      := NIL ;
         charStarSize  := 0 ;
         charStarValid := FALSE ;
         garbage       := NIL ;
         state         := inuse
      END
   END ;
   RETURN( s )
END InitStringCharStar ;


(*
   InitStringChar - initializes and returns a String to contain the single character, ch.
*)

PROCEDURE InitStringChar (ch: CHAR) : String ;
VAR
   a: ARRAY [0..1] OF CHAR ;
BEGIN
   a[0] := ch ;
   a[1] := nul ;
   RETURN( InitString(a) )
END InitStringChar ;


(*
   Mark - marks String, s, ready for garbage collection.
*)

PROCEDURE Mark (s: String) : String ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned(s)
   END ;
   IF (s#NIL) AND (s^.head^.state=inuse)
   THEN
      s^.head^.state := marked
   END ;
   RETURN( s )
END Mark ;


(*
   AddToGarbage - adds String, b, onto the garbage list of, a. Providing
                  the state of b is marked. The state is then altered to onlist.
                  String, a, is returned.
*)

PROCEDURE AddToGarbage (a, b: String) : String ;
BEGIN
   IF PoisonOn
   THEN
      a := CheckPoisoned(a) ;
      b := CheckPoisoned(b)
   END ;
   IF (a#NIL) AND (b#NIL) AND (b^.head^.state=marked)
   THEN
      WITH b^.head^ DO
         state   := onlist ;
         garbage := a^.head^.garbage
      END ;
      a^.head^.garbage := b
   END ;
   RETURN( a )
END AddToGarbage ;


(*
   Length - returns the length of the String, s.
*)

PROCEDURE Length (s: String) : CARDINAL ;
BEGIN
   IF s=NIL
   THEN
      RETURN( 0 )
   ELSE
      RETURN( s^.contents.len + Length(s^.contents.next) )
   END
END Length ;


(*
   ConCat - returns String, a, after the contents of, b, have been appended.
*)

PROCEDURE ConCat (a, b: String) : String ;
VAR
   t: String ;
BEGIN
   IF PoisonOn
   THEN
      a := CheckPoisoned(a) ;
      b := CheckPoisoned(b)
   END ;
   IF a=b
   THEN
      RETURN( ConCat(a, Mark(Dup(b))) )
   ELSIF a#NIL
   THEN
      a := AddToGarbage(a, b) ;
      MarkInvalid(a) ;
      t := a ;
      WHILE b#NIL DO
         WHILE (t^.contents.len=MaxBuf) AND (t^.contents.next#NIL) DO
            t := t^.contents.next
         END ;
         ConcatContents(t^.contents, b^.contents.buf, b^.contents.len, 0) ;
         b := b^.contents.next
      END
   END ;
   IF (a=NIL) AND (b#NIL)
   THEN
      HALT
   END ;
   RETURN( a )
END ConCat ;


(*
   ConCatChar - returns String, a, after character, ch, has been appended.
*)

PROCEDURE ConCatChar (a: String; ch: CHAR) : String ;
VAR
   b: ARRAY [0..1] OF CHAR ;
   t: String ;
BEGIN
   IF PoisonOn
   THEN
      a := CheckPoisoned(a)
   END ;
   b[0] := ch ;
   b[1] := nul ;
   t := a ;
   WHILE (t^.contents.len=MaxBuf) AND (t^.contents.next#NIL) DO
      t := t^.contents.next
   END ;
   ConcatContents(t^.contents, b, 1, 0) ;
   RETURN( a )
END ConCatChar ;


(*
   Assign - assigns the contents of, b, into, a.
            String, a, is returned.
*)

PROCEDURE Assign (a, b: String) : String ;
BEGIN
   IF PoisonOn
   THEN
      a := CheckPoisoned(a) ;
      b := CheckPoisoned(b)
   END ;
   IF (a#NIL) AND (b#NIL)
   THEN
      WITH a^ DO
         contents.next := KillString(contents.next) ;
         contents.len  := 0
      END
   END ;
   RETURN( ConCat(a, b) )
END Assign ;


(*
   Dup - duplicate a String, s, returning the copy of s.
*)

PROCEDURE Dup (s: String) : String ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned(s)
   END ;
   RETURN( Assign(InitString(''), s) )
END Dup ;


(*
   Add - returns a new String which contains the contents of a and b.
*)

PROCEDURE Add (a, b: String) : String ;
BEGIN
   IF PoisonOn
   THEN
      a := CheckPoisoned(a) ;
      b := CheckPoisoned(b)
   END ;
   RETURN( ConCat(ConCat(InitString(''), a), b) )
END Add ;


(*
   Equal - returns TRUE if String, a, and, b, are equal.
*)

PROCEDURE Equal (a, b: String) : BOOLEAN ;
VAR
   i: CARDINAL ;
BEGIN
   IF PoisonOn
   THEN
      a := CheckPoisoned(a) ;
      b := CheckPoisoned(b)
   END ;
   IF Length(a)=Length(b)
   THEN
      WHILE (a#NIL) AND (b#NIL) DO
         i := 0 ;
         WHILE i<a^.contents.len DO
            IF a^.contents.buf[i]#b^.contents.buf[i]
            THEN
               RETURN( FALSE )
            END ;
            INC(i)
         END ;
         a := a^.contents.next ;
         b := b^.contents.next
      END ; 
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END Equal ;


(*
   EqualCharStar - returns TRUE if contents of String, s, is the same as the
                   string, a.
*)

PROCEDURE EqualCharStar (s: String; a: ADDRESS) : BOOLEAN ;
VAR
   t: String ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned(s)
   END ;
   t := InitStringCharStar(a) ;
   IF Equal(t, s)
   THEN
      t := KillString(t) ;
      RETURN( TRUE )
   ELSE
      t := KillString(t) ;
      RETURN( FALSE )
   END
END EqualCharStar ;


(*
   EqualArray - returns TRUE if contents of String, s, is the same as the
                string, a.
*)

PROCEDURE EqualArray (s: String; a: ARRAY OF CHAR) : BOOLEAN ;
VAR
   t: String ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned(s)
   END ;
   t := InitString(a) ;
   IF Equal(t, s)
   THEN
      t := KillString(t) ;
      RETURN( TRUE )
   ELSE
      t := KillString(t) ;
      RETURN( FALSE )
   END
END EqualArray ;


(*
   Mult - returns a new string which is n concatenations of String, s.
*)

PROCEDURE Mult (s: String; n: CARDINAL) : String ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned(s)
   END ;
   IF n<=0
   THEN
      RETURN( InitString('') )
   ELSE
      RETURN( ConCat(Mult(s, n-1), s) )
   END
END Mult ;


(*
   Slice - returns a new string which contains the elements
           low..high-1

           strings start at element 0
           Slice(s, 0, 2)  will return elements 0, 1 but not 2
           Slice(s, 1, 3)  will return elements 1, 2 but not 3
           Slice(s, 2, 0)  will return elements 2..max
           Slice(s, 3, -1) will return elements 3..max-1
           Slice(s, 4, -2) will return elements 4..max-2
*)

PROCEDURE Slice (s: String; low, high: INTEGER) : String ;
VAR
   d, t         : String ;
   start, end, o: CARDINAL ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned(s)
   END ;
   IF low<0
   THEN
      low := Length(s)+low
   END ;
   IF high<=0
   THEN
      high := Length(s)+high
   END ;
   d := InitString('') ;
   o := 0 ;
   t := d ;
   WHILE s#NIL DO
      IF low<o+s^.contents.len
      THEN
         IF o>high
         THEN
            s := NIL
         ELSE
            (* found sliceable unit *)
            IF low<o
            THEN
               start := 0
            ELSE
               start := low-o
            END ;
            end := Max(Min(MaxBuf, high-o), 0) ;
            WHILE t^.contents.len=MaxBuf DO
               IF t^.contents.next=NIL
               THEN
                  NEW(t^.contents.next) ;
                  WITH t^.contents.next^ DO
                     head         := NIL ;
                     contents.len := 0 ;
                  END
               END ;
               t := t^.contents.next
            END ;
            ConcatContentsAddress(t^.contents,
                                  ADR(s^.contents.buf[start]), end-start) ;
            INC(o, s^.contents.len) ;
            s := s^.contents.next
         END
      ELSE
         INC(o, s^.contents.len) ;
         s := s^.contents.next
      END ;
   END ;
   RETURN( d )
END Slice ;


(*
   Index - returns the indice of the first occurance of, ch, in
           String, s. -1 is returned if, ch, does not exist.
           The search starts at position, o.
           -1 is returned if, ch, is not found.
*)

PROCEDURE Index (s: String; ch: CHAR; o: CARDINAL) : INTEGER ;
VAR
   i, k: INTEGER ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned(s)
   END ;
   k := 0 ;
   WHILE s#NIL DO
      WITH s^ DO
         IF k+contents.len<o
         THEN
            INC(k, contents.len)
         ELSE
            i := o-k ;
            WHILE i<contents.len DO
               IF contents.buf[i]=ch
               THEN
                  RETURN( k+i )
               END ;
               INC(i)
            END ;
            INC(k, i) ;
            o := k
         END
      END ;
      s := s^.contents.next
   END ;
   RETURN( -1 )
END Index ;


(*
   RIndex - returns the indice of the last occurance of, ch,
            in String, s. The search starts at position, o.
            -1 is returned if, ch, is not found.
*)

PROCEDURE RIndex (s: String; ch: CHAR; o: CARDINAL) : INTEGER ;
VAR
   i, j, k: INTEGER ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned(s)
   END ;
   j := -1 ;
   k :=  0 ;
   WHILE s#NIL DO
      WITH s^ DO
         IF k+contents.len<o
         THEN
            INC(k, contents.len)
         ELSE
            IF o<k
            THEN
               i := 0
            ELSE
               i := o-k
            END ;
            WHILE i<contents.len DO
               IF contents.buf[i]=ch
               THEN
                  j := k
               END ;
               INC(k) ;
               INC(i)
            END
         END
      END ;
      s := s^.contents.next
   END ;
   RETURN( j )
END RIndex ;


(*
   char - returns the character, ch, at position, i, in String, s.
*)

PROCEDURE char (s: String; i: CARDINAL) : CHAR ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned(s)
   END ;
   WHILE (s#NIL) AND (i>s^.contents.len) DO
      DEC(i, s^.contents.len) ;
      s := s^.contents.next
   END ;
   IF (s=NIL) OR (i>=s^.contents.len)
   THEN
      RETURN( nul )
   ELSE
      RETURN( s^.contents.buf[i] )
   END
END char ;


(*
   string - returns the C style char * of String, s.
*)

PROCEDURE string (s: String) : ADDRESS ;
VAR
   a   : String ;
   l, i: CARDINAL ;
   p   : POINTER TO CHAR ;
BEGIN
   IF PoisonOn
   THEN
      s := CheckPoisoned(s)
   END ;
   IF s=NIL
   THEN
      RETURN( NIL )
   ELSE
      IF NOT s^.head^.charStarValid
      THEN
         l := Length(s) ;
         WITH s^.head^ DO
            IF NOT (charStarUsed AND (charStarSize>l))
            THEN
               DeallocateCharStar(s) ;
               ALLOCATE(charStar, l+1) ;
               charStarSize := l+1 ;
               charStarUsed := TRUE
            END ;
            p := charStar ;
         END ;
         a := s ;
         WHILE a#NIL DO
            i := 0 ;
            WHILE i<a^.contents.len DO
               p^ := a^.contents.buf[i] ;
               INC(i) ;
               INC(p)
            END ;
            a := a^.contents.next
         END ;
         p^ := nul ;
         s^.head^.charStarValid := TRUE
      END ;
      RETURN( s^.head^.charStar )
   END
END string ;


(*
   IsWhite - returns TRUE if, ch, is a space or a tab.
*)

PROCEDURE IsWhite (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( (ch=' ') OR (ch=tab) )
END IsWhite ;


(*
   RemoveWhitePrefix - removes any leading white space from String, s.
                       A new string is returned.
*)

PROCEDURE RemoveWhitePrefix (s: String) : String ;
VAR
   i: CARDINAL ;
BEGIN
   i := 0 ;
   WHILE IsWhite(char(s, i)) DO
      INC(i)
   END ;
   RETURN( Slice(s, INTEGER(i), 0) )
END RemoveWhitePrefix ;


(*
   ToUpper - returns string, s, after it has had its lower case characters
             replaced by upper case characters.
             The string, s, is not duplicated.
*)

PROCEDURE ToUpper (s: String) : String ;
VAR
   ch: CHAR ;
   i : CARDINAL ;
   t : String ;
BEGIN
   IF s#NIL
   THEN
      t := s ;
      IF PoisonOn
      THEN
         t := CheckPoisoned(t)
      END ;
      t^.head^.charStarValid := FALSE ;
      WHILE t#NIL DO
         WITH t^ DO
            i := 0 ;
            WHILE i<contents.len DO
               ch := contents.buf[i] ;
               IF (ch>='a') AND (ch<='z')
               THEN
                  contents.buf[i] := CHR( ORD(ch)-ORD('a')+ORD('A') )
               END ;
               INC(i)
            END
         END ;
         t := t^.contents.next
      END
   END ;
   RETURN( s )
END ToUpper ;


(*
   ToLower - returns string, s, after it has had its upper case characters
             replaced by lower case characters.
             The string, s, is not duplicated.
*)

PROCEDURE ToLower (s: String) : String ;
VAR
   ch: CHAR ;
   i : CARDINAL ;
   t : String ;
BEGIN
   IF s#NIL
   THEN
      t := s ;
      IF PoisonOn
      THEN
         t := CheckPoisoned(t)
      END ;
      t^.head^.charStarValid := FALSE ;
      WHILE t#NIL DO
         WITH t^ DO
            i := 0 ;
            WHILE i<contents.len DO
               ch := contents.buf[i] ;
               IF (ch>='A') AND (ch<='Z')
               THEN
                  contents.buf[i] := CHR( ORD(ch)-ORD('A')+ORD('a') )
               END ;
               INC(i)
            END
         END ;
         t := t^.contents.next
      END
   END ;
   RETURN( s )
END ToLower ;


END DynamicStrings.
