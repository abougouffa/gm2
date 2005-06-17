MODULE cast3 ;

FROM SYSTEM IMPORT CAST, LOC;

VAR
   x :LOC ;
   n :CARDINAL ;
BEGIN
   (*cast*)
   n := 2 ;
   x := CAST(LOC, n)
END cast3.
