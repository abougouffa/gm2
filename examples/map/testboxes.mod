MODULE testboxes ;


FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM MakeBoxes IMPORT InitBoxes, KillBoxes, GetAndDeleteRandomBox ;

VAR
   Index: CARDINAL ;
   i    : CARDINAL ;
   x, y : CARDINAL ;
BEGIN
   Index := InitBoxes(3, 10, 3, 10) ;
   FOR i := 1 TO 5 DO
      WriteString('Box no') ; WriteCard(i, 4) ; WriteLn ;
      GetAndDeleteRandomBox(Index, x, y) ;
      WriteString('x:') ; WriteCard(x, 4) ;
      WriteString('     y:') ; WriteCard(y, 4) ; WriteLn
   END ;
   KillBoxes(Index) ;
   Index := InitBoxes(7, 3, 10, 3) ;
   FOR i := 1 TO 5 DO
      WriteString('Box no') ; WriteCard(i, 4) ; WriteLn ;
      GetAndDeleteRandomBox(Index, x, y) ;
      WriteString('x:') ; WriteCard(x, 4) ;
      WriteString('     y:') ; WriteCard(y, 4) ; WriteLn
   END ;
   KillBoxes(Index) ;
END testboxes.
