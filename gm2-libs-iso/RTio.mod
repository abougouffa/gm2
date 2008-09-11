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

IMPLEMENTATION MODULE RTio ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;


TYPE
   ChanId = POINTER TO RECORD
                          file : FIO.File ;
                          flags: ChanConsts.FlagSet ;
                          open : ChanConsts.OpenResults ;
                          read : IOConsts.ReadResults ;
                          error: INTEGER ;
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
   SetChanId - assign all fields in ChanId.
*)

PROCEDURE SetChanId (c: ChanId;
                     f: FIO.File;
                     s: ChanConsts.FlagSet;
                     o: ChanConsts.OpenResults;
                     r: IOConsts.ReadResults;
                     e: INTEGER) : ChanId ;
BEGIN
   WITH c^ DO
      file := f ;
      flags := s ;
      open := o ;
      read := r ;
      error := e
   END ;
   RETURN( c )
END SetChanId ;


(*
   GetChanId - assign all fields in ChanId.
*)

PROCEDURE GetChanId (c: ChanId;
                     VAR f: FIO.File;
                     VAR s: ChanConsts.FlagSet;
                     VAR o: ChanConsts.OpenResults;
                     VAR r: IOConsts.ReadResults;
                     VAR e: INTEGER) ;
BEGIN
   WITH c^ DO
      f := file ;
      s := flags ;
      o := open ;
      r := read ;
      e := error
   END
END GetChanId ;


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


(*
   GetFile - returns the flags field from, c.
*)

PROCEDURE GetFlags (c: ChanId) : ChanConsts.FlagSet ;
BEGIN
   RETURN( c^.flags )
END GetFlags ;


(*
   SetFile - sets the flags field in, c.
*)

PROCEDURE SetFlags (c: ChanId; s: ChanConsts.FlagSet) ;
BEGIN
   c^.flags := s
END SetFlags ;


(*
   GetOpen - returns the open field from, c.
*)

PROCEDURE GetOpen (c: ChanId) : ChanConsts.OpenResults ;
BEGIN
   RETURN( c^.open )
END GetOpen ;


(*
   SetOpen - sets the flags field in, c.
*)

PROCEDURE SetOpen (c: ChanId; o: ChanConsts.OpenResults) ;
BEGIN
   c^.open := o
END SetOpen ;


(*
   GetRead - assign all fields in ChanId.
*)

PROCEDURE GetRead (c: ChanId) : IOConsts.ReadResults ;
BEGIN
   RETURN( c^.read )
END GetRead ;


(*
   SetRead - assign all fields in ChanId.
*)

PROCEDURE SetRead (c: ChanId; r: IOConsts.ReadResults) ;
BEGIN
   c^.read := r
END SetRead ;


(*
   GetError - return error field in ChanId.
*)

PROCEDURE GetError (c: ChanId) : INTEGER ;
BEGIN
   RETURN( c^.error )
END GetError ;


(*
   SetError - assign error field in ChanId.
*)

PROCEDURE SetError (c: ChanId; e: INTEGER) ;
BEGIN
   c^.error := e
END SetError ;


END RTio.
