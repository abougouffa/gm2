MODULE subrange7 ;

CONST
   low = ind0(50);
   high = ind0(100);
TYPE
   ind = [low..high];
   ind0 = [60..100];

VAR
   a : ARRAY [10..100] OF INTEGER;
   b : ARRAY ind OF INTEGER;

BEGIN
END subrange7.