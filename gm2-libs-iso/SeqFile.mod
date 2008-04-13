(* Copyright (C) 2005 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE SeqFile ;

FROM DynamicString IMPORT String, InitString, KillString ;
FROM SFIO IMPORT OpenToWrite, Close, OpenForRead ;


TYPE
   cidInfo = RECORD
                name: String ;
                used: BOOLEAN ;
                flag: FlagSet ;
                file: FIO.File ;
             END ;

VAR
   cidIndex: ARRAY [0..MaxCid] OF cidInfo ;
   usedCid : CARDINAL ;


(*
   newCid - returns a ChanId which represents the opened file, name.
            res is set appropriately on return.
*)

PROCEDURE newCid (fname: ARRAY OF CHAR; flags: FlagSet;
                  VAR res: OpenResults; toWrite: BOOLEAN) : ChanId ;
VAR
   i: ChanId ;
BEGIN
   i := 0 ;
   WHILE i<=usedCid DO
      IF NOT cidIndex[i].used
      THEN
         WITH cidIndex[i] DO
            name := InitString(fname) ;
            used := TRUE ;
            flag := flags ;
            IF toWrite
            THEN
               file := OpenToWrite(name)
            ELSE
               file := OpenToRead(name)
            END
         END
      END ;
      INC(i)
   END ;
   res := outOfChans ;
   RETURN 0
END newCid ;


(*
   Attempts to obtain and open a channel connected to a stored rewindable
   file of the given name.  The write flag is implied; without the raw
   flag, text is implied.  If successful, assigns to cid the identity of
   the opened channel, assigns the value opened to res, and selects
   output mode, with the write position at the start of the file (i.e.
   the file is of zero length).  If a channel cannot be opened as required,
   the value of res indicates the reason, and cid identifies the
   invalid channel.
*)

PROCEDURE OpenWrite (VAR cid: ChanId; name: ARRAY OF CHAR; flags: FlagSet;
                     VAR res: OpenResults) ;
BEGIN
   cid := newCid(name, flags, res, FALSE)
END OpenWrite ;


(*
   OpenRead - Attempts to obtain and open a channel connected to a stored rewindable
 file of the given name.  The read and old flags are implied; without
 the raw flag, text is implied.  If successful, assigns to cid the
 identity of the opened channel, assigns the value opened to res, and
 selects input mode, with the read position corresponding to the start
 of the file.  If a channel cannot be opened as required, the value of
 res indicates the reason, and cid identifies the invalid channel.
*)

PROCEDURE OpenRead (VAR cid: ChanId; name: ARRAY OF CHAR; flags: FlagSet;
                    VAR res: OpenResults) ;
BEGIN
   cid := newCid(name, flags, res, TRUE)   
END OpenRead ;


(*
   IsSeqFile - tests if the channel identified by cid is open to a
               rewindable sequential file.
*)

PROCEDURE IsSeqFile (cid: ChanId) : BOOLEAN ;
BEGIN
   IF cid=0
   THEN
      RETURN FALSE
   ELSE
      WITH cidIndex[cid] DO
         RETURN used AND (readFlag IN flag)
      END
   END
END IsSeqFile ;


(*
   If the channel identified by cid is not open to a rewindable
   sequential file, the exception wrongDevice is raised; otherwise
   attempts to set the read position to the start of the file, and
   to select input mode.  If the operation cannot be performed
   (perhaps because of insufficient permissions) neither input
   mode nor output mode is selected.
*)

PROCEDURE Reread (cid: ChanId) ;
BEGIN
   WITH cidIndex[cid] DO
      IF used AND (NOT (writeFlag IN flag))
      THEN
         SetPositionFromBeginning(file, 0)
      ELSE
         RaiseException (* --fixme-- finish this code *)
      END
   END
END Reread ;


(*
    If the channel identified by cid is not open to a rewindable
    sequential file, the exception wrongDevice is raised; otherwise,
    attempts to truncate the file to zero length, and to select
    output mode.
    If the operation cannot be performed (perhaps because of
    insufficient permissions) neither input mode nor output
    mode is selected.
  *)

PROCEDURE Rewrite (cid: ChanId) ;
BEGIN
   IF IsSeqFile(cid)
   THEN
      WITH cidIndex[cid] DO
         FIO.Close(file) ;
         EXCL(flag, readFlag) ;
         INCL(flag, writeFlag) ;
         file := OpenToWrite(name)
      END         
   ELSE
      RaiseException(wrongDevice)
   END
END Rewrite ;


(*
    If the channel identified by cid is not open to a rewindable
    sequential file, the exception wrongDevice is raised; otherwise
    closes the channel, and assigns the value identifying the
    invalid channel to cid.
*)

PROCEDURE Close (VAR cid: ChanId) ;
BEGIN
   IF IsSeqFile(cid)
   THEN
      WITH cidIndex[cid] DO
         IF used
         THEN
            name := KillString(name) ;
            used := FALSE
         END
      END
   ELSE
      RaiseException(wrongDevice)
   END ;
   cid := 0
END Close ;


(*
   Init - initialize the current module data structures.
*)

PROCEDURE Init ;
BEGIN
   usedCid := 0
END Init ;



BEGIN
   Init
END SeqFile.
