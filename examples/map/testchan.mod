MODULE testchance ;

FROM NumberIO IMPORT WriteCard ;
FROM Chance IMPORT GetRand ;

VAR
   i: CARDINAL ;
BEGIN
   FOR i := 1 TO 100 DO
      WriteCard(GetRand(2), 1) ;
   END
END testchance.
