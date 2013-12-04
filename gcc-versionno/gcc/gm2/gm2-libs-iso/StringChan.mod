(* Copyright (C) 2009, 2010
                 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE StringChan ;


FROM DynamicStrings IMPORT Length, char ;
FROM TextIO IMPORT WriteChar ;


(*
   writeString - writes a string, s, to ChanId, cid.
                 The string, s, is not destroyed.
*)

PROCEDURE writeString (cid: IOChan.ChanId; s: String) ;
VAR
   i, h: CARDINAL ;
BEGIN
   h := Length(s) ;
   i := 0 ;
   WHILE i<h DO
      WriteChar(cid, char(s, i)) ;
      INC(i)
   END
END writeString ;


(*
   writeFieldWidth - writes a string, s, to ChanId, cid.
                     The string, s, is not destroyed and it
                     is prefixed by spaces so that at least,
                     width, characters are written.  If the
                     string, s, is longer than width then
                     no spaces are prefixed to the output
                     and the entire string is written.
*)

PROCEDURE writeFieldWidth (cid: IOChan.ChanId;
                           s: String; width: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   i := Length(s) ;
   WHILE i<width DO
      WriteChar(cid, ' ') ;
      INC(i)
   END ;
   writeString(cid, s)
END writeFieldWidth ;


END StringChan.
