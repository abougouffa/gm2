MODULE array2;

TYPE
   ctype = INTEGER [-1 .. 24];
   btype = ctype;

VAR
   bvar : ARRAY btype OF INTEGER;
BEGIN
   bvar[0] := 0;
END array2.
