MODULE GenMap ;

(*
   Title      : GenMap
   Author     : Gaius Mulley
   Date       : 10/7/88
   LastEdit   : 10/7/88
   System     : LOGITECH MODULA-2/86
   Description: Generates a random map for Dungeon
*)

FROM Storage IMPORT ALLOCATE ;
FROM StdIO IMPORT Read, Write ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM Random IMPORT RandomCard ;


CONST
   MaxCard           = 65535 ;

   MaxX              =  79 ;
   MaxY              =  24 ;
   CorridorWidth     =   5 ;
   MaxCorridorLength = 160 ;
   MaxRoomLength     =  25 ;
   MinRoomLength     =   3 ;


TYPE
   Square = RECORD
               Contents: (Empty, Wall, Treasure) ;
               RoomNo  : CARDINAL ;
            END ;

   PtrToMap = POINTER TO Map ;
   Map      = ARRAY [1..MaxX], [1..MaxY] OF Square ;

VAR
   ConsistantMap: PtrToMap ;
   TrialMap     : PtrToMap ;
   NoOfRooms    : CARDINAL ;


(*
   InitializeMap - Initializes ConsistantMap.
                   ConsistantMap has its boarder set to a Wall and middle
                   is set to Empty.
*)

PROCEDURE InitializeMap ;
VAR
   i, j: CARDINAL ;
BEGIN
   NEW(ConsistantMap) ;
   FOR i := 2 TO MaxX-1 DO
      FOR j := 2 TO MaxY-1 DO
         WITH ConsistantMap^[i, j] DO
            Contents := Empty ;
            RoomNo := 0
         END
      END
   END ;
   FOR i := 1 TO MaxX DO
      ConsistantMap^[i, 1].Contents    := Wall ;
      ConsistantMap^[i, MaxY].Contents := Wall
   END ;
   FOR j := 1 TO MaxY DO
      ConsistantMap^[1, j].Contents    := Wall ;
      ConsistantMap^[MaxX, j].Contents := Wall
   END ;
   NoOfRooms := 0
END InitializeMap ;


(*
   Init - Initialize the module and start the generation of a map.
*)

PROCEDURE Init ;
BEGIN
   InitializeMap ;
   NEW(TrialMap) ;
   CreateMap ;
   DisplayMap
END Init ;


(*
   CreateMap - 
*)

PROCEDURE CreateMap ;
BEGIN
   CorridorMap
END CreateMap ;


(*
   CorridorMap - makes a map based arround central corridors.
*)

PROCEDURE CorridorMap ;
BEGIN
   CreateCorridor
END CorridorMap ;


(*
   CreateCorridors - creates a length of corridor on the map.
*)

PROCEDURE CreateCorridor ;
VAR
   Length,
   LengthLeft: CARDINAL ;
BEGIN
   LengthLeft := MaxCorridorLength ;
   REPEAT
      Length := RandomCard(MaxRoomLength) + MinRoomLength ;
      IF MakeCorridor(Length, CorridorWidth)
      THEN
         IF LengthLeft>Length
         THEN
            DEC(LengthLeft, Length)
         ELSE
            LengthLeft := 0
         END
      ELSE
         DEC(LengthLeft)   (* Fail safe exit *)
      END
   UNTIL LengthLeft=0
END CreateCorridor ;


(*
   MakeCorridor - attempts to make a length of corridor.
                  The length of corridor is added perpendicular
                  to the map.
                  The result of the attempt is returned.
*)

PROCEDURE MakeCorridor (Length, Width: CARDINAL) : BOOLEAN ;
BEGIN
   IF RandomCard(2)=1
   THEN
      RETURN(
              MakeCorridorX(Width, Length) OR
              MakeCorridorY(Length, Width)
            )
   ELSE
      RETURN(
              MakeCorridorY(Length, Width) OR
              MakeCorridorX(Width, Length)
            )
   END
END MakeCorridor ;


(*
  MakeCorridorX - attempts to add the rectangle Length, Width onto a
                  corridor running parallel to the X axes.
*)

PROCEDURE MakeCorridorX (Length, Width: CARDINAL) : BOOLEAN ;
VAR
   Success: BOOLEAN ;
   i      : CARDINAL ;
BEGIN
   IF NoOfRooms=0
   THEN
      Success := FindX(0, Length, Width)
   ELSE
      i := NoOfRooms ;
      REPEAT
         Success := FindX(i, Length, Width) ;
         DEC(i)
      UNTIL Success OR (i=0)
   END ;
   RETURN( Success )
END MakeCorridorX ;


PROCEDURE MakeCorridorY (Length, Width: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( MakeCorridorX(Length, Width) )
END MakeCorridorY ;


PROCEDURE FindX (Room, Length, Width: CARDINAL) : BOOLEAN ;
VAR
   x, y: CARDINAL ;
BEGIN
   x := RandomCard(MaxX-Length)+1 ;
   y := RandomCard(MaxY-Width)+1 ;
   Place(x, y, Length, Width, NoOfRooms+1) ;
   RETURN( TRUE )
END FindX ;


PROCEDURE Place (x, y, Length, Width, NewRoomNo: CARDINAL) ;
VAR
   i, j: CARDINAL ;
BEGIN
   FOR i := x TO x+Length DO
      FOR j := y TO y+Width DO
         WITH ConsistantMap^[i, j] DO
            Contents := Empty ;
            RoomNo := NewRoomNo
         END
      END
   END ;
   NoOfRooms := NewRoomNo
END Place ;


PROCEDURE DisplayMap ;
VAR
   i, j: CARDINAL ;
   ch  : CHAR ;
BEGIN
   FOR j := 1 TO MaxY DO
      FOR i := 1 TO MaxX DO
         DisplaySquare(i, j)
      END ;
      WriteLn
   END
   ; Read(ch)
END DisplayMap ;


PROCEDURE DisplaySquare (i, j: CARDINAL) ;
VAR
   r: CARDINAL ;
BEGIN
   CASE ConsistantMap^[i, j].Contents OF

   Empty   : DisplayRoom( ConsistantMap^[i, j].RoomNo ) |
   Wall    : Write('#') |
   Treasure: Write('+')

   ELSE
      WriteString('Should never get here') ; WriteLn ;
      HALT
   END
END DisplaySquare ;


PROCEDURE DisplayRoom (Room: CARDINAL) ;
BEGIN
   IF Room=0
   THEN
      Write(' ')
   ELSE
      Write(CHR(Room+ORD('a')-1))
   END
END DisplayRoom ;


BEGIN
   Init
END GenMap.
