MODULE testlength2 ;

FROM libc IMPORT exit ;
(* FROM Strings IMPORT Length ; *)

PROCEDURE Length (a: ARRAY OF CHAR) : CARDINAL ;
BEGIN
   RETURN 12
END Length ;

VAR
   a: ARRAY [0..20] OF CHAR ;
   l: CARDINAL ;
BEGIN
   a := "hello world" ;
   l := LENGTH(a) ;
   IF l#12
   THEN
      exit(1)
   END
END testlength2.
