IMPLEMENTATION MODULE FormatStrings ;

FROM Strings IMPORT String, InitString, InitStringChar, Mark, ConCat, Slice, Index, Char,
                    Assign, Length, Mult ;

FROM ASCII IMPORT nul, nl, tab ;
FROM SYSTEM IMPORT ADDRESS ;
FROM libc IMPORT strlen ;


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
      ch := Char(s, i+1) ;
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
   Integer - converts INTEGER, i, into a String. The field with can be specified
             if non zero. Leading characters are defined by padding and this
             function will prepend a + if sign is set to TRUE.
*)

PROCEDURE Integer (i: INTEGER; width: CARDINAL; padding: CHAR; sign: BOOLEAN) : String ;
VAR
   s: String ;
BEGIN
   IF i<0
   THEN
      IF i=MIN(INTEGER)
      THEN
         IF width>0
         THEN
            RETURN( ConCat(Integer(i DIV 10, width-1, padding, sign),
                           Integer(i MOD 10, 0, ' ', FALSE)) )
         ELSE
            RETURN( ConCat(Integer(i DIV 10, 0, padding, sign),
                           Integer(i MOD 10, 0, ' ', FALSE)) )
         END
      ELSE
         s := InitString('-')
      END ;
      i := -i
   ELSE
      IF sign
      THEN
         s := InitString('+')
      ELSE
         s := InitString('')
      END
   END ;
   IF i>9
   THEN
      s := ConCat(ConCat(s, Integer(i DIV 10, 0, ' ', FALSE)), Integer(i MOD 10, 0, ' ', FALSE))
   ELSE
      s := ConCat(s, InitStringChar(CHR(i+ORD('0'))))
   END ;
   IF width>Length(s)
   THEN
      RETURN( ConCat(Mult(InitStringChar(padding), width-Length(s)), s) )
   END ;
   RETURN( s )
END Integer ;


(*
   FormatString - returns a String containing, s, together with encapsulated
                  entity, w. It only formats the first %s or %d with n.
*)

PROCEDURE FormatString (s: String; w: WORD) : String ;
VAR
   i, j, k: INTEGER ;
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
      ch := Char(s, j+1) ;
      IF ch='s'
      THEN
         p := w ;
         (* include string, p, into s *)
         RETURN( ConCat(ConCat(Slice(s, i, k), p), Slice(s, j+2, 0)) )
      ELSIF ch='d'
      THEN
         RETURN( ConCat(ConCat(Slice(s, i, k), Integer(w, 0, ' ', FALSE)),
                        Slice(s, j+2, 0)) )
      ELSIF (ch='0') AND (Char(s, j+2)='d')
      THEN
         RETURN( ConCat(ConCat(Slice(s, i, k), Integer(w, 0, '0', FALSE)), Slice(s, j+3, 0)) )
      ELSIF (ch='0') AND IsDigit(Char(s, j+2)) AND (Char(s, j+3)='d')
      THEN
         RETURN( ConCat(ConCat(Slice(s, i, k), Integer(w, ORD(Char(s, j+2))-ORD('0'), '0', FALSE)),
                        Slice(s, j+4, 0)) )
      ELSIF IsDigit(ch) AND (Char(s, j+2)='d')
      THEN
         RETURN( ConCat(ConCat(Slice(s, i, k), Integer(w, ORD(ch)-ORD('0'), ' ', FALSE)),
                        Slice(s, j+3, 0)) )
      ELSE
         RETURN( ConCat(ConCat(Slice(s, i, k), Mark(InitStringChar(ch))), Slice(s, j+1, 0)) )
      END
   ELSE
      RETURN( s )
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
