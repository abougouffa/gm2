MODULE cpph ;

FROM SYSTEM IMPORT ADR ;
FROM libc IMPORT exit, printf ;

PROCEDURE InternalError (a, file: ARRAY OF CHAR;
                         line: CARDINAL) ;
VAR
   r: INTEGER ;
BEGIN
   r := printf("%s:%d:internal error, %s\n",
               ADR(file), line, ADR(a)) ;
   exit(1)
END InternalError ;



VAR
   i, j: CARDINAL ;
BEGIN
   i := 1 ;
   j := 1 ;
   IF i#j
   THEN
      InternalError('trivial assignment failed', "cpp.mod", 25)
   END ;
   i := 2 DIV 2 ;
   IF i#j
   THEN
      InternalError('trivial division failed', "cpp.mod", 30)
   END ;
   InternalError('ignore this error just checking cpp', "cpp.mod", 32)
END cpph.
