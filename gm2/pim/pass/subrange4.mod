MODULE subrange5 ;

TYPE
   ind0 = [-100..100];

CONST
   low = ind0(-100);
CONST
   high = ind0(100);
TYPE
   ind = [low..high];

VAR
   a : ARRAY [0..100] OF INTEGER;
VAR
   b : ARRAY ind OF INTEGER;

BEGIN
END subrange4.
