MODULE testset ;

FROM SYSTEM IMPORT BITSET ;

CONST
   shift = 0;
   Tandem = { shift + 15 };

VAR
   b: BITSET ;
BEGIN
   b := Tandem
END testset.
