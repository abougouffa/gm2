MODULE testch2 ;

FROM NumberIO IMPORT WriteCard ;
FROM Chance IMPORT InitRandom, KillRandom, GetAndDeleteRandom, AddRandom ;
FROM StrIO IMPORT WriteLn ;

VAR
   Index: CARDINAL ;
   i,  j: CARDINAL ;
BEGIN
   FOR i := 1 TO 15 DO
      Index := InitRandom() ;
      AddRandom(Index, 5) ;
      FOR j := 1 TO 6 DO
         WriteCard(GetAndDeleteRandom(Index), 1)
      END ;
      WriteLn ;
      KillRandom(Index)
   END
END testch2.
