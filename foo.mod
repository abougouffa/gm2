MODULE foo ;


TYPE
   proc = PROCEDURE (CHAR) ;

   rec = RECORD
            a, b: CARDINAL ;
         END ;

   colours = (red, blue, yellow) ;

   varrec= RECORD
              CASE type:colours OF

              red : x: CARDINAL |
              blue: y: CHAR

              END
           END ;

VAR
   myp : POINTER TO rec ;
   this: rec ;
   that: RECORD
            c, d: CARDINAL ;
            e   : rec ;
         END ;

   other: varrec ;

   inline: RECORD
             CASE type:colours OF

             red : x: CARDINAL |
             blue: y: CHAR

             END
          END ;
   array: ARRAY [1..10] OF CHAR ;
   large: ARRAY [1..100], [1..200] OF rec ;
   it   : proc ;
   card : CARDINAL ;
(*
TYPE

   sub     = [1..10] ;
   bar = CARDINAL ;
VAR
   z      : bar ;
*)
   i, j, k: INTEGER ;
(*
   c      : colours ;
   b      : BOOLEAN ;
*)


PROCEDURE dummy (ch: CHAR; q: CARDINAL) : CARDINAL ;
VAR
   t: CARDINAL ;
   a: CHAR ;
BEGIN
   t := 123 ;
   RETURN( t )
END dummy ;


BEGIN
   (* card := dummy('a', card) *)
   i := 100
   (* i := j+k *)
END foo.
