MODULE testcoords ;


FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM StoreCoords IMPORT InitCoords, KillCoords,
                        AddCoord, GetAndDeleteRandomCoord ;

VAR
   Index: CARDINAL ;
   i, j : CARDINAL ;
   x, y : CARDINAL ;
BEGIN
   FOR i := 1 TO 10 DO
      Index := InitCoords() ;
      WriteString('Index:') ; WriteCard(Index, 4) ; WriteString('Coords') ;
      FOR j := 1 TO 5 DO
         AddCoord(Index, j, j)
      END ;
      FOR j := 1 TO 6 DO
         GetAndDeleteRandomCoord(Index, x, y) ;
         WriteCard(x, 4) ; WriteCard(y, 2)
      END ;
      WriteLn ;
      KillCoords(Index)
   END
END testcoords.
