MODULE subrange4 ;

CONST
   low = ind0(-100);
   high = ind0(100);
TYPE
   ind = [low..high];
   ind0 = [-100..100];

VAR
   a : ARRAY [0..100] OF INTEGER;
   b : ARRAY ind OF INTEGER;

BEGIN
END subrange4.
