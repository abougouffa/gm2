(* Copyright (C) 2008 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA *)

IMPLEMENTATION MODULE RawIO ;

FROM IOChan IMPORT RawWrite, RawRead, ReadResult ;
FROM IOConsts IMPORT ReadResults ;


(* Reading and writing data over specified channels using raw
   operations, that is, with no conversion or interpretation.
   The read result is of the type IOConsts.ReadResults.
*)

(*
   Reads - storage units from cid, and assigns them to successive
           components of to. The read result is set to the value
           allRight, wrongFormat, or endOfInput.
*)

PROCEDURE Read (cid: IOChan.ChanId; VAR to: ARRAY OF SYSTEM.LOC) ;
VAR
   i, n: CARDINAL ;
   a   : SYSTEM.ADDRESS ;
BEGIN
   a := SYSTEM.ADR(to) ;
   n := HIGH(to)+1 ;
   LOOP
      RawRead(cid, a, n, i) ;
      IF (n=0) OR (ReadResult(cid)#allRight)
      THEN
         EXIT
      ELSE
         INC(a, i) ;
         DEC(n, i)
      END
   END
END Read ;


(*
   Writes - storage units to cid from successive components of from.
*)

PROCEDURE Write (cid: IOChan.ChanId; from: ARRAY OF SYSTEM.LOC);
BEGIN
   RawWrite(cid, SYSTEM.ADR(from), HIGH(from)+1)
END Write ;


END RawIO.
