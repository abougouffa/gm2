MODULE variant9 ;

FROM libc IMPORT exit, printf ;

TYPE
  DataType = (card, other);
  RcdType = RECORD
              CASE Data : DataType OF
                card  :  j : CARDINAL;
                         k : CARDINAL |
                other : st : CHAR ;
              END
            END ;
      
VAR
  s: RcdType;
  r: INTEGER ;
BEGIN
   WITH s DO
      Data := card;
      j := 123;
      k := 456;
      r := printf('j = %d and k = %d\n', j, k)
   END ;
   IF (s.j#123) OR (s.k#456)
   THEN
      exit(1)
   END
END variant9.
