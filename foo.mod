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
   another,
   card   : CARDINAL ;

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

(* *)
PROCEDURE dummy (q: CARDINAL) ;
VAR
   t: CARDINAL ;
   a: CHAR ;
BEGIN
   t := 123 ;
   (* another := *) dummy(t) ;
(*   RETURN( t ) *)
END dummy ;


BEGIN
   card := 12 ;
   (* myproc('a', card) ; *)
   (*  *)
   (* another := *) dummy(card);
   i := 100
   (* i := j+k *)
END foo.
