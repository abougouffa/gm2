MODULE varient;

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
  R1 : RcdType;
  r  : INTEGER ;
BEGIN
   WITH R1 DO
      Data := card;
      j := 123;
      k := 456;
      r := printf('j = %d and k = %d\n', j, k)
   END ;
   IF (R1.j#123) OR (R1.k#456)
   THEN
      exit(1)
   END
END varient.
