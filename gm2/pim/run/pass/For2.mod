MODULE For2 ;

FROM libc IMPORT exit ;

VAR
   i, c: CARDINAL ;
BEGIN
   c := 0 ;
   FOR i := 19 TO 0 BY -2 DO
      INC(c, i)
   END ;
   IF c#1+3+5+7+9+11+13+15+17+19
   THEN
      exit(1)
   END
END For2.
