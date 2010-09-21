(* Copyright (C) 2008, 2009, 2010 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE RTio ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;


TYPE
   ChanId = POINTER TO RECORD
                          did  : IOLink.DeviceId ;
                          dtp  : IOLink.DeviceTablePtr ;
                          file : FIO.File ;
                       END ;


(*
   InitChanId - return a new ChanId.
*)

PROCEDURE InitChanId () : ChanId ;
VAR
   c: ChanId ;
BEGIN
   NEW(c) ;
   RETURN( c )
END InitChanId ;


(*
   InitChanId - deallocate a ChanId.
*)

PROCEDURE KillChanId (c: ChanId) : ChanId ;
BEGIN
   DISPOSE(c) ;
   RETURN( NIL )
END KillChanId ;


(*
   NilChanId - return a NIL pointer.
*)

PROCEDURE NilChanId () : ChanId ;
BEGIN
   RETURN( NIL )
END NilChanId ;


(*
   GetDeviceId - returns the device id, from, c.
*)

PROCEDURE GetDeviceId (c: ChanId) : IOLink.DeviceId ;
BEGIN
   RETURN( c^.did )
END GetDeviceId ;


(*
   SetDeviceId - returns the device id, from, c.
*)

PROCEDURE SetDeviceId (c: ChanId; d: IOLink.DeviceId) ;
BEGIN
   c^.did := d
END SetDeviceId ;


(*
   GetDevicePtr - returns the device table ptr, from, c.
*)

PROCEDURE GetDevicePtr (c: ChanId) : IOLink.DeviceTablePtr ;
BEGIN
   RETURN( c^.dtp )
END GetDevicePtr ;

(*
   SetDevicePtr - sets the device table ptr in, c.
*)

PROCEDURE SetDevicePtr (c: ChanId; p: IOLink.DeviceTablePtr) ;
BEGIN
   c^.dtp := p
END SetDevicePtr ;


(*
   GetFile - returns the file field from, c.
*)

PROCEDURE GetFile (c: ChanId) : FIO.File ;
BEGIN
   RETURN( c^.file )
END GetFile ;


(*
   SetFile - sets the file field in, c.
*)

PROCEDURE SetFile (c: ChanId; f: FIO.File) ;
BEGIN
   c^.file := f
END SetFile ;


END RTio.
