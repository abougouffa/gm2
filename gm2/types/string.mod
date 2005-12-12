MODULE string ;


(* string, tests whether the compiler can declare strings after their use *)

(*
   length - 
*)

PROCEDURE length () : CARDINAL ;
CONST
   both = pre + post ;
   pre = 'my ' ;
   post = mystr ;
BEGIN
   RETURN( HIGH(both) )
END length ;



CONST
   mystr = 'hello world' ;

BEGIN
   IF length()=5
   THEN
   END
END string.
