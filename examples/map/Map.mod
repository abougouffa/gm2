MODULE Map ;

FROM RoomMap IMPORT CreateRoomMap ;
FROM WriteMap IMPORT WriteMapText ;

BEGIN
   CreateRoomMap ;
   WriteMapText
END Map.
