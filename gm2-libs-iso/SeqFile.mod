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

IMPLEMENTATION MODULE SeqFile ;

IMPORT FIO, IOChan, RTio, errno, ErrnoCategory ;

FROM EXCEPTIONS IMPORT ExceptionSource, RAISE, AllocateSource,
                       IsCurrentSource, IsExceptionalExecution ;

VAR
   source: ExceptionSource ;


(*
   newCid - returns a ChanId which represents the opened file, name.
            res is set appropriately on return.
*)

PROCEDURE newCid (fname: ARRAY OF CHAR; flags: FlagSet;
                  VAR res: OpenResults;
                  toRead, toWrite: BOOLEAN) : ChanId ;
VAR
   c   : RTio.ChanId ;
   file: FIO.File ;
   e   : INTEGER ;
BEGIN
   IF toRead AND toWrite
   THEN
      file := FIO.OpenForRandom(name)
   ELSIF toWrite
   THEN
      file := FIO.OpenToWrite(name)
   ELSE
      file := FIO.OpenToRead(name)
   END ;
   e := errno.geterrno() ;
   res := ErrnoCategory.GetOpenResults(e) ;
   IF FIO.IsNoError(file)
   THEN
      c := RTio.SetChanId(RTio.InitChanId(), file, s, res, flags, e) ;
      RETURN( IOChan.ChanId(c) )
   ELSE
      RETURN( IOChan.InvalidChan() )
   END
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
   INCL(flags, ChanConsts.write) ;
   IF NOT ChanConsts.rawFlag IN flags
   THEN
      INCL(flags, ChanConsts.textFlag)
   END ;
   cid := newCid(name, flags, res, ChanConst.read IN flags, TRUE)
END OpenWrite ;


(*
   Attempts to obtain and open a channel connected to a stored rewindable
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
   flags := flags + ChanConsts.read + ChanConsts.old ;
   IF NOT ChanConsts.rawFlag IN flags
   THEN
      INCL(flags, ChanConsts.textFlag)
   END ;
   cid := newCid(name, flags, res, TRUE, ChanConst.write IN flags)
END OpenRead ;


(*
   IsSeqFile - tests if the channel identified by cid is open to a
               rewindable sequential file.
*)

PROCEDURE IsSeqFile (cid: ChanId) : BOOLEAN ;
BEGIN
   RETURN(
          (cid # NIL) AND (IOChan.InvalidChan() # cid) AND
          (ChanConsts.readFlag IN IOChan.CurrentFlags(cid)) AND
          (ChanConsts.opened = RTio.GetOpen(RTio.ChanId(c)))
         )
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
   IF IsSeqFile(cid)
   THEN
      FIO.SetPositionFromBeginning(RTio.GetFile(cid), 0)
   ELSE
      IOLink.RAISEdevException(cid,
                               --fixme-- got to here
                               ORD(IOChan.wrongDevice),
                               'performing a reread on a file which is not sequentially readable')
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
      FIO.SetPositionFromBeginning(RTio.GetFile(cid), 0)
   ELSE
      IOLink.RAISEdevException(cid,
                               --fixme-- got to here
                               ORD(IOChan.wrongDevice),
                               'performing a reread on a file which is not sequentially readable')
   END
END Rewrite ;


(*
    If the channel identified by cid is not open to a rewindable
    sequential file, the exception wrongDevice is raised; otherwise
    closes the channel, and assigns the value identifying the
    invalid channel to cid.
*)

PROCEDURE Close (VAR cid: ChanId) ;
VAR
   f: FIO.File ;
BEGIN
   IF IsSeqFile(cid)
   THEN
      f := RTio.GetFile(RTio.ChanId(cid)) ;
      FIO.FlushBuffer(f) ;
      IF FIO.IsNoError(f)
      THEN
         FIO.Close(f) ;
      END ;
      CheckError(errno) ;
      cid := IOChan.Id(KillChanId(RTio.ChanId(cid)))
   ELSE
      IOLink.RAISEdevException(cid,
                               --fixme-- got to here
                               ORD(IOChan.wrongDevice),
                               'performing a Close on a file which was not open for sequential access')
   END
END Close ;


(*
   Init - initialize the current module data structures.
*)

PROCEDURE Init ;
BEGIN
   AllocateSource(source)
END Init ;


BEGIN
   Init
END SeqFile.
