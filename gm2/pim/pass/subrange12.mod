MODULE subrange12 ;

TYPE  ind0 = [-100..100];

CONST low  = ind0(-100);
CONST high = ind0(100);
TYPE  ind  = [low..high];

VAR a: ARRAY [0..100] OF CHAR ;
VAR b: ARRAY ind OF CHAR ;

BEGIN
END subrange12.
