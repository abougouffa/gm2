MODULE foo2 ;


PROCEDURE foobar (param, more: CHAR) : CARDINAL ;
VAR
(*
   seven, eight,
   five, six,
   isit, now,
*)
   working: CARDINAL;
   then,
   now : CHAR ;
BEGIN
   working := 1 ;
   IF param='a'
   THEN
      RETURN( 0 )
   ELSE
      RETURN( 1 )
   END
END foobar ;

VAR
   i1    : CARDINAL ;
   global: CHAR ;
   c1    : CHAR ;
   a     : ARRAY [1..200] OF CHAR ;
BEGIN
   i1 := foobar(global, c1)
END foo2.
