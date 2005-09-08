MODULE LogitechLong;

(***************************************************************************************)
(*        NOTE: THIS IS TEST CODE AND MAY BE INCORRECT             *)
(***************************************************************************************)

FROM InOut IMPORT WriteLn;

FROM LongIO IMPORT WriteLongInt;

VAR
  i,j : LONGINT;

BEGIN
  i := MAX(LONGINT);
  WriteLongInt(i,0);
  WriteLn;
  j := MIN(LONGINT);
  WriteLongInt(j,0);
  WriteLn;
  j := MIN(LONGINT) + 1;
  WriteLongInt(j,0);
  WriteLn
END LogitechLong.
