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

IMPLEMENTATION MODULE RTfio ;

FROM IOLink IMPORT DeviceTablePtr ;
FROM RTio IMPORT GetFile ;
FROM errno IMPORT geterrno ;

FROM FIO IMPORT File, ReadChar, UnReadChar, WriteChar, ReadNBytes, WriteNBytes,
                WriteLine, EOF, EOLN, IsNoError ;


(*
   doreadchar - returns a CHAR from the file associated with, g.
*)

PROCEDURE doreadchar (g: GenDevIF; d: DeviceTablePtr) : CHAR ;
VAR
   f: File ;
BEGIN
   WITH d^ DO
      f := GetFile(cid) ;
      RETURN( ReadChar(f) )
   END
END doreadchar ;


(*
   dounreadchar - pushes a CHAR back onto the file associated with, g.
*)

PROCEDURE dounreadchar (g: GenDevIF; d: DeviceTablePtr; ch: CHAR) : CHAR ;
VAR
   f: File ;
BEGIN
   WITH d^ DO
      f := GetFile(cid) ;
      UnReadChar(f, ch) ;
      RETURN( ch )
   END
END dounreadchar ;


(*
   dogeterrno - returns the errno relating to the generic device.
*)

PROCEDURE dogeterrno (g: GenDevIF; d: DeviceTablePtr) : INTEGER ;
BEGIN
   RETURN geterrno()
END dogeterrno ;


(*
   dorbytes - reads upto, max, bytes setting, actual, and
              returning FALSE if an error (not due to eof)
              occurred.
*)

PROCEDURE dorbytes (g: GenDevIF; d: DeviceTablePtr;
                    to: ADDRESS;
                    max: CARDINAL;
                    VAR actual: CARDINAL) : BOOLEAN ;
VAR
   f: File ;
BEGIN
   WITH d^ DO
      f := GetFile(cid) ;
      actual := ReadNBytes(f, max, to) ;
      RETURN( EOF(f) OR IsNoError(f) )
   END
END dorbytes ;


(*
   dowbytes - 
*)

PROCEDURE dowbytes (g: GenDevIF; d: DeviceTablePtr;
                    from: ADDRESS;
                    nBytes: CARDINAL;
                    VAR actual: CARDINAL) : BOOLEAN ;
VAR
   f: File ;
BEGIN
   WITH d^ DO
      f := GetFile(cid) ;
      actual := WriteNBytes(f, nBytes, from) ;
      RETURN( IsNoError(f) )
   END
END dowbytes ;


(*
   dowriteln - attempt to write an end of line marker to the
               file and returns TRUE if successful.
*)

PROCEDURE dowriteln (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
VAR
   f: File ;
BEGIN
   f := GetFile(d^.cid) ;
   WriteLine(f) ;
   RETURN( IsNoError(f) )
END dowriteln ;


(*
   iseof - returns TRUE if end of file is seen.
*)

PROCEDURE iseof (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
VAR
   f: File ;
BEGIN
   WITH d^ DO
      f := GetFile(cid) ;
      RETURN( EOF(f) )
   END
END iseof ;


(*
   iseoln - returns TRUE if end of line is seen.
*)

PROCEDURE iseoln (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
VAR
   f: File ;
BEGIN
   WITH d^ DO
      f := GetFile(cid) ;
      RETURN( EOLN(f) )
   END
END iseoln ;


(*
   iserror - returns TRUE if an error was seen on the device.
*)

PROCEDURE iserror (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
VAR
   f: File ;
BEGIN
   WITH d^ DO
      f := GetFile(cid) ;
      RETURN( NOT IsNoError(f) )
   END
END iserror ;


END RTfio.
