(* Copyright (C) 2001 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

IMPLEMENTATION MODULE SFIO ;

FROM Strings IMPORT string, Length ;
FROM FIO IMPORT exists, openToRead, openToWrite, openForRandom, WriteNBytes ;


(*
   Exists - returns TRUE if a file named, fname exists for reading.
*)

PROCEDURE Exists (fname: String) : BOOLEAN ;
BEGIN
   RETURN( exists(string(fname), Length(fname)) )
END Exists ;


(*
   OpenToRead - attempts to open a file, fname, for reading and
                it returns this file.
                The success of this operation can be checked by
                calling IsNoError.
*)

PROCEDURE OpenToRead (fname: String) : File ;
BEGIN
   RETURN( openToRead(string(fname), Length(fname)) )
END OpenToRead ;


(*
   OpenToWrite - attempts to open a file, fname, for write and
                 it returns this file.
                 The success of this operation can be checked by
                 calling IsNoError.
*)

PROCEDURE OpenToWrite (fname: String) : File ;
BEGIN
   RETURN( openToWrite(string(fname), Length(fname)) )
END OpenToWrite ;


(*
   OpenForRandom - attempts to open a file, fname, for random access
                   read or write and it returns this file.
                   The success of this operation can be checked by
                   calling IsNoError.
                   towrite, determines whether the file should be
                   opened for writing or reading.
*)

PROCEDURE OpenForRandom (fname: String; towrite: BOOLEAN) : File ;
BEGIN
   RETURN( openForRandom(string(fname), Length(fname), towrite) )
END OpenForRandom ;


(*
   WriteS - writes a string, s, to, file. It returns the number of
            bytes written.
*)

PROCEDURE WriteS (file: File; s: String) : String ;
VAR
   nBytes: CARDINAL ;
BEGIN
   nBytes := WriteNBytes(file, Length(s), string(s)) ;
   RETURN( s )
END WriteS ;


END SFIO.
