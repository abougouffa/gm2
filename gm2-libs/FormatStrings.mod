IMPLEMENTATION MODULE FormatStrings ;

FROM Strings IMPORT String, InitString, InitStringChar, Mark, ConCat, Slice, Index, char,
                    Assign, Length, Mult, Dup, ConCatChar ;

FROM StringConvert IMPORT IntegerToString ;
FROM ASCII IMPORT nul, nl, tab ;
FROM SYSTEM IMPORT ADDRESS ;


(*
   IsDigit - returns TRUE if ch lies in the range: 0..9
*)

PROCEDURE IsDigit (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( (ch>='0') AND (ch<='9') )
END IsDigit ;


(*
   HandleEscape - translates \n and \t into their respective ascii codes.
*)

PROCEDURE HandleEscape (s: String) : String ;
VAR
   d   : String ;
   i, j: INTEGER ;
   ch  : CHAR ;
BEGIN
   d := InitString('') ;
   i := Index(s, '\', 0) ;
   j := 0 ;
   WHILE i>=0 DO
      IF i>0
      THEN
         (* initially i might be zero which means the end of the string, which is not what we want *)
         d := ConCat(d, Slice(s, j, i))
      END ;
      ch := char(s, i+1) ;
      IF ch='n'
      THEN
         (* requires a newline *)
         d := ConCat(d, Mark(InitStringChar(nl)))
      ELSIF ch='t'
      THEN
         (* requires a tab (yuck) *)
         d := ConCat(d, Mark(InitStringChar(tab)))
      ELSE
         (* copy escaped character *)
         d := ConCat(d, Mark(InitStringChar(ch)))
      END ;
      INC(i, 2) ;
      j := i ;
      i := Index(s, '\', CARDINAL(i))
   END ;
   RETURN( Assign(s, Mark(ConCat(d, Mark(Slice(s, j, 0))))) )
END HandleEscape ;

      
(*
   FormatString - returns a String containing, s, together with encapsulated
                  entity, w. It only formats the first %s or %d with n.
                  A new string is returned.
*)

PROCEDURE FormatString (s: String; w: WORD) : String ;
VAR
   left   : BOOLEAN ;
   width,
   i, j, k: INTEGER ;
   leader,
   ch     : CHAR ;
   p      : String ;
BEGIN
   i := 0 ;
   j := Index(s, '%', 0) ;
   IF j=0
   THEN
      k := -Length(s)
   ELSE
      k := j
   END ;
   IF j>=0
   THEN
      IF char(s, j+1)='-'
      THEN
         left := TRUE ;
         INC(j)
      ELSE
         left := FALSE
      END ;
      ch := char(s, j+1) ;
      IF ch='0'
      THEN
         leader := '0'
      ELSE
         leader := ' '
      END ;
      width := 0 ;
      WHILE IsDigit(ch) DO
         width := (width*10)+ORD(ch)-ORD('0') ;
         INC(j) ;
         ch := char(s, j+1)
      END ;
      IF (ch='c') OR (ch='s')
      THEN
         IF (ch='c')
         THEN
            p := ConCatChar(InitString(''), VAL(CHAR, w))
         ELSE
            p := Dup(w)
         END ;
         IF (width>0) AND (Length(p)<width)
         THEN
            IF left
            THEN
               (* place trailing spaces after, p *)
               p := ConCat(p,
                           Mark(Mult(Mark(InitString(' ')), width-Length(p))))
            ELSE
               (* padd string, p, with leading spaces *)
               p := ConCat(Mult(Mark(InitString(' ')), width-Length(p)),
                           Mark(p))
            END
         END ;
         (* include string, p, into s *)
         RETURN( ConCat(ConCat(Slice(s, i, k), Mark(p)), Slice(s, j+2, 0)) )
      ELSIF ch='d'
      THEN
         RETURN( ConCat(ConCat(Slice(s, i, k), IntegerToString(w, width, leader, FALSE, 10, FALSE)),
                        Slice(s, j+2, 0)) )
      ELSE
         RETURN( ConCat(ConCat(Slice(s, i, k), Mark(InitStringChar(ch))), Slice(s, j+1, 0)) )
      END
   ELSE
      RETURN( Dup(s) )
   END
END FormatString ;


(*
   Sprintf0 - returns a String containing, s, after it has had its
              escape sequences translated.
*)

PROCEDURE Sprintf0 (s: String) : String ;
BEGIN
   RETURN( HandleEscape(s) )
END Sprintf0 ;


(*
   Sprintf1 - returns a String containing, s, together with encapsulated
              entity, w. It only formats the first %s or %d with n.
*)

PROCEDURE Sprintf1 (s: String; w: WORD) ;
BEGIN
   RETURN( FormatString(HandleEscape(s), w) )
END Sprintf1 ;


(*
   Sprintf2 - returns a string, s, which has been formatted.
*)

PROCEDURE Sprintf2 (s: String; w1, w2: WORD) : String ;
BEGIN
   RETURN( FormatString(FormatString(HandleEscape(s), w1), w2) )
END Sprintf2 ;


(*
   Sprintf3 - returns a string, s, which has been formatted.
*)

PROCEDURE Sprintf3 (s: String; w1, w2, w3: WORD) : String ;
BEGIN
   RETURN( FormatString(FormatString(FormatString(HandleEscape(s), w1), w2), w3) )
END Sprintf3 ;


(*
   Sprintf4 - returns a string, s, which has been formatted.
*)

PROCEDURE Sprintf4 (s: String; w1, w2, w3, w4: WORD) : String ;
BEGIN
   RETURN( FormatString(FormatString(FormatString(FormatString(HandleEscape(s), w1), w2), w3), w4) )
END Sprintf4 ;


END FormatStrings.
