MODULE cpp ;

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

#define ERROR(X)  InternalError(X, __FILE__, __LINE__)

VAR
   i, j: CARDINAL ;
BEGIN
   i := 1 ;
   j := 1 ;
   IF i#j
   THEN
      ERROR('trivial assignment failed')
   END ;
   i := 2 DIV 2 ;
   IF i#j
   THEN
      ERROR('trivial division failed')
   END ;
   ERROR('ignore this error just checking cpp')
END cpp.
