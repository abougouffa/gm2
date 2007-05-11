IMPLEMENTATION MODULE testcse50 ;


FROM SYSTEM IMPORT ADR ;
FROM libc IMPORT getenv ;
FROM ASCII IMPORT nul ;
FROM StrLib IMPORT StrCopy ;


PROCEDURE GetEnvironment (Env: ARRAY OF CHAR; VAR a: ARRAY OF CHAR) : BOOLEAN ;
VAR
   High,
   i   : CARDINAL ;
   Addr: POINTER TO CHAR ;
BEGIN
   Addr := getenv(ADR(Env)) ;
(*
   i := 0 ;
   High := HIGH(a) ;
   Addr := getenv(ADR(Env)) ;
*)
(*
   WHILE (i<High) AND (Addr#NIL) AND (Addr^#nul) DO
      a[i] := Addr^ ;
      INC(Addr) ;
      INC(i)
   END ;
   IF i<High
   THEN
      a[i] := nul
   END ;
   RETURN( Addr#NIL )
*)
   RETURN FALSE
END GetEnvironment ;


END testcse50.
