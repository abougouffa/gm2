MODULE test3 ;

IMPORT StdIO ;

VAR
   i: CARDINAL ;
BEGIN
   i := StdIO.FileNo(StdIO.stdin) ;
   IF StdIO.Fputc('x', StdIO.stdin)
   THEN
   END
END test3.