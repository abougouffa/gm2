MODULE subrange11 ;

PROCEDURE outer;

CONST
   beforeinner = 1000 + 1 ;

   PROCEDURE inner;
   TYPE
      ind1 = [-200..200];
   CONST
      low = ind1(-200);
   BEGIN
   END inner;

VAR
   a: array ;
TYPE
   ind0 = [-100..100];
   array = ARRAY [low..0] OF CHAR ;
CONST
   low = ind0(-100);


BEGIN
END outer;

BEGIN
   outer
END subrange11.
