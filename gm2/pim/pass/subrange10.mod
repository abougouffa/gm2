MODULE subrange10 ;

CONST
   beforeouter = 1000 + 1 ;

PROCEDURE outer;
CONST
   beforeinner = 2000 + 1 ;

   PROCEDURE inner;
   TYPE
      ind1 = [-200..200];
   CONST
      low = ind1(-200);
   BEGIN
   END inner;

TYPE
   ind0 = [-100..100];
CONST
   low = ind0(-100);
   inouter = 3000 + 1;

BEGIN
END outer;

BEGIN
   outer
END subrange10.
