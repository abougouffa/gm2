MODULE setbec ;

FROM SYSTEM IMPORT BITSET ;

TYPE
   SmallSet = SET OF [0..16] ;

VAR
   s: SmallSet ;
   b: BITSET ;
BEGIN
   s := SmallSet{1, 3, 5, 7};
   b := s
END setbec.
