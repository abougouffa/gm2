MODULE array5 ;

FROM Storage IMPORT ALLOCATE ;

VAR
   s: POINTER TO ARRAY [1..5] OF CARDINAL ;
BEGIN
   NEW(s) ;
   s^[1] := 1 ;
   s^[1] := 1 ;
   s^[1] := 1 ;
   s^[1] := 1 ;
   s^[1] := 1 ;
   s^[1] := 1 ;
   s^[1] := 1 ;
   s^[1] := 1 ;
   s^[1] := 1 ;
   s^[1] := 1 ;
   s^[1] := 1 ;
   s^[1] := 1 ;
   s^[1] := 1 ;
   s^[1] := 1 ;
   s^[1] := 1 ;
   s^[1] := 1 ;
   s^[1] := 1 ;
   s^[1] := 1
END array5.
