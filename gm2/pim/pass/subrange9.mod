MODULE subrange9 ;

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

TYPE
   ind0 = [-100..100];
CONST
   low = ind0(-100);


BEGIN
END outer;

BEGIN
   outer
END subrange9.
