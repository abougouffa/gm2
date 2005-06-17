MODULE index;

TYPE
   A = ARRAY [0..1] OF CHAR ;
VAR
   a :ARRAY [0..1] OF A ;

BEGIN 
   a[0,0] := 'A'  (* Reported as error by GM2 of 2005-06-03 *)
END index.