MODULE Map ;


FROM RoomMap IMPORT CreateRoomMap ;

(* FROM MonMap IMPORT Monitor ; *)

FROM WriteMap IMPORT WriteMapText ;


BEGIN
   CreateRoomMap ;
   (* Monitor ; *)
   WriteMapText
END Map.
