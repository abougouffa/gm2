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

IMPLEMENTATION MODULE RndFile ;


FROM RTgen IMPORT ChanDev, DeviceType,
                  InitChanDev, doLook, doSkip, doSkipLook, doWriteLn,
                  doReadText, doWriteText, doReadLocs, doWriteLocs,
                  checkErrno ;

FROM RTfio IMPORT doreadchar, dounreadchar, dogeterrno, dorbytes,
                  dowbytes, dowriteln, iseof, iseoln, iserror ;

FROM IOLink IMPORT DeviceId, DeviceTablePtr, IsDevice, MakeChan, UnMakeChan,
                   DeviceTablePtrValue, RAISEdevException, AllocateDeviceId,
                   ResetProc ;

FROM RTgenif IMPORT GenDevIF, InitGenDevIF ;
FROM FIO IMPORT File ;
FROM errno IMPORT geterrno ;
FROM IOConsts IMPORT ReadResults ;
FROM ChanConsts IMPORT readFlag, writeFlag ;

FROM EXCEPTIONS IMPORT ExceptionNumber, RAISE,
                       AllocateSource, ExceptionSource, IsCurrentSource,
                       IsExceptionalExecution ;

IMPORT FIO, SYSTEM, RTio, errno, ErrnoCategory ;


VAR
   dev             : ChanDev ;
   did             : DeviceId ;
   rndfileException: ExceptionSource ;


PROCEDURE look (d: DeviceTablePtr;
                VAR ch: CHAR; VAR r: ReadResults) ;
BEGIN
   doLook(dev, d, ch, r)
END look ;


PROCEDURE skip (d: DeviceTablePtr) ;
BEGIN
   doSkip(dev, d)
END skip ;


PROCEDURE skiplook (d: DeviceTablePtr;
                    VAR ch: CHAR; VAR r: ReadResults) ;
BEGIN
   doSkipLook(dev, d, ch, r)
END skiplook ;


PROCEDURE lnwrite (d: DeviceTablePtr) ;
BEGIN
   doWriteLn(dev, d)
END lnwrite ;


PROCEDURE textread (d: DeviceTablePtr;
                    to: SYSTEM.ADDRESS;
                    maxChars: CARDINAL;
                    VAR charsRead: CARDINAL) ;
BEGIN
   doReadText(dev, d, to, maxChars, charsRead)
END textread ;


PROCEDURE textwrite (d: DeviceTablePtr;
                     from: SYSTEM.ADDRESS;
                     charsToWrite: CARDINAL);
BEGIN
   doWriteText(dev, d, from, charsToWrite)
END textwrite ;


PROCEDURE rawread (d: DeviceTablePtr;
                   to: SYSTEM.ADDRESS;
                   maxLocs: CARDINAL;
                   VAR locsRead: CARDINAL) ;
BEGIN
   doReadLocs(dev, d, to, maxLocs, locsRead)
END rawread ;


PROCEDURE rawwrite (d: DeviceTablePtr;
                    from: SYSTEM.ADDRESS;
                    locsToWrite: CARDINAL) ;
BEGIN
   doWriteLocs(dev, d, from, locsToWrite)
END rawwrite ;


PROCEDURE getname (d: DeviceTablePtr;
                   VAR a: ARRAY OF CHAR) ;
BEGIN
   FIO.GetFileName(RTio.GetFile(d^.cid), a)
END getname ;


PROCEDURE flush (d: DeviceTablePtr) ;
BEGIN
   FIO.FlushBuffer(RTio.GetFile(d^.cid))
END flush ;


(*
   newCid - returns a ChanId which represents the opened file, name.
            res is set appropriately on return.
*)

PROCEDURE newCid (fname: ARRAY OF CHAR;
                  f: FlagSet;
                  VAR res: OpenResults;
                  toRead: BOOLEAN;
                  whichreset: ResetProc) : ChanId ;
VAR
   c   : RTio.ChanId ;
   file: FIO.File ;
   e   : INTEGER ;
   p   : DeviceTablePtr ;
BEGIN
   file := FIO.OpenForRandom(fname, NOT toRead) ;
   e := errno.geterrno() ;
   res := ErrnoCategory.GetOpenResults(e) ;

   IF FIO.IsNoError(file)
   THEN
      MakeChan(did, c) ;
      RTio.SetFile(c, file) ;
      p := DeviceTablePtrValue(c, did, IOChan.wrongDevice,
                               'RndFile: cannot obtain device table pointer') ;
      WITH p^ DO
         flags := f ;
         errNum := e ;
         doLook := look ;
         doSkip := skip ;
         doSkipLook := skiplook ;
         doLnWrite := lnwrite ;
         doTextRead := textread ;
         doTextWrite := textwrite ;
         doRawRead := rawread ;
         doRawWrite := rawwrite ;
         doGetName := getname ;
         doReset := whichreset ;
         doFlush := flush ;
         doFree := handlefree
      END ;
      RETURN( c )
   ELSE
      RETURN( IOChan.InvalidChan() )
   END
END newCid ;


(*
   handlefree - 
*)

PROCEDURE handlefree (d: DeviceTablePtr) ;
VAR
   f: File ;
BEGIN
   WITH d^ DO
      doFlush(d) ;
      checkErrno(dev, d) ;
      f := RTio.GetFile(RTio.ChanId(cid)) ;
      IF FIO.IsNoError(f)
      THEN
         FIO.Close(f) ;
      END ;
      checkErrno(dev, d)
   END
END handlefree ;


PROCEDURE resetRandom (d: DeviceTablePtr) ;
BEGIN
   WITH d^ DO
      IF IsRndFile(cid)
      THEN
         (* --fixme --, finish this *)
      ELSE
         RAISEdevException(cid, did, IOChan.wrongDevice,
                           'RndFile.' + __FUNCTION__ +
                           ': channel is not a sequential file')
      END
   END
END resetRandom ;


PROCEDURE OpenOld (VAR cid: ChanId; name: ARRAY OF CHAR; flags: FlagSet;
                   VAR res: OpenResults);
  (* Attempts to obtain and open a channel connected to a stored random
     access file of the given name.
     The old flag is implied; without the write flag, read is implied;
     without the text flag, raw is implied.
     If successful, assigns to cid the identity of the opened channel,
     assigns the value opened to res, and sets the read/write position
     to the start of the file.
     If a channel cannot be opened as required, the value of res indicates
     the reason, and cid identifies the invalid channel.
  *)
BEGIN
   INCL(flags, ChanConsts.oldFlag) ;
   IF NOT ChanConsts.writeFlag IN flags
   THEN
      INCL(flags, ChanConsts.readFlag)
   END ;
   IF NOT ChanConsts.textFlag IN flags
   THEN
      INCL(flags, ChanConsts.rawFlag)
   END ;
   cid := newCid(name, flags, res, FALSE, resetRandom)
END OpenOld ;


PROCEDURE OpenClean (VAR cid: ChanId; name: ARRAY OF CHAR; flags: FlagSet;
                     VAR res: OpenResults);
  (* Attempts to obtain and open a channel connected to a stored random
     access file of the given name.
     The write flag is implied; without the text flag, raw is implied.
     If successful, assigns to cid the identity of the opened channel,
     assigns the value opened to res, and truncates the file to zero length.
     If a channel cannot be opened as required, the value of res indicates
     the reason, and cid identifies the invalid channel.
  *)
BEGIN
   INCL(flags, ChanConsts.writeFlag) ;
   IF NOT ChanConsts.textFlag IN flags
   THEN
      INCL(flags, ChanConsts.rawFlag)
   END ;
   cid := newCid(name, flags, res, FALSE, resetRandom)
END OpenClean ;


PROCEDURE IsRndFile (cid: ChanId): BOOLEAN;
  (* Tests if the channel identified by cid is open to a random access file. *)
BEGIN
   RETURN( (cid # NIL) AND (IOChan.InvalidChan() # cid) AND
           (IsDevice(cid, did)) AND
           ((ChanConsts.readFlag IN IOChan.CurrentFlags(cid)) OR
            (ChanConsts.writeFlag IN IOChan.CurrentFlags(cid))) )
END IsRndFile ;


PROCEDURE IsRndFileException (): BOOLEAN;
  (* Returns TRUE if the current coroutine is in the exceptional execution
     state because of the raising of a RndFile exception; otherwise returns
     FALSE.
  *)
BEGIN
   RETURN( IsCurrentSource (rndfileException) )
END IsRndFileException ;



PROCEDURE StartPos (cid: ChanId): FilePos;
  (* If the channel identified by cid is not open to a random access file,
     the exception wrongDevice is raised; otherwise returns the position of
     the start of the file.
  *)
VAR
   d: DeviceTablePtr ;
BEGIN
   IF IsRndFile(cid)
   THEN
      d := DeviceTablePtrValue(cid, did, IOChan.wrongDevice,
                               'RndFile.Rewrite: incorrect channel') ;
      RETURN( 0 )
   ELSE
      RAISEdevException(cid, did, IOChan.wrongDevice,
                        'RndFile.' + __FUNCTION__ +
                        ': channel is not a sequential file')
   END
END StartPos ;


PROCEDURE CurrentPos (cid: ChanId): FilePos;
  (* If the channel identified by cid is not open to a random access file,
     the exception wrongDevice is raised; otherwise returns the position
     of the current read/write position.
  *)
VAR
   d: DeviceTablePtr ;
BEGIN
   IF IsRndFile(cid)
   THEN
      d := DeviceTablePtrValue(cid, did, IOChan.wrongDevice,
                               'RndFile.Rewrite: incorrect channel') ;
      WITH d^ DO
         RETURN( FIO.FindPosition(RTio.GetFile(cid)) )
      END
   ELSE
      RAISEdevException(cid, did, IOChan.wrongDevice,
                        'RndFile.' + __FUNCTION__ +
                        ': channel is not a sequential file')
   END
END CurrentPos ;


PROCEDURE EndPos (cid: ChanId): FilePos;
  (* If the channel identified by cid is not open to a random access file,
     the exception wrongDevice is raised; otherwise returns the first
     position after which there have been no writes.
  *)
VAR
   d  : DeviceTablePtr ;
   end,
   old: FilePos ;
BEGIN
   IF IsRndFile(cid)
   THEN
      d := DeviceTablePtrValue(cid, did, IOChan.wrongDevice,
                               'RndFile.Rewrite: incorrect channel') ;
      old := CurrentPos(cid) ;
      WITH d^ DO
         FIO.SetPositionFromEnd(RTio.GetFile(cid), 0) ;
         checkErrno(dev, d) ;
         end := CurrentPos(cid) ;
         FIO.SetPositionFromBeginning(RTio.GetFile(cid), end) ;
         RETURN( end )
      END
   ELSE
      RAISEdevException(cid, did, IOChan.wrongDevice,
                        'RndFile.' + __FUNCTION__ +
                        ': channel is not a sequential file')
   END
END EndPos ;


PROCEDURE NewPos (cid: ChanId; chunks: INTEGER; chunkSize: CARDINAL;
                  from: FilePos): FilePos;
  (* If the channel identified by cid is not open to a random access file,
     the exception wrongDevice is raised; otherwise returns the position
     (chunks * chunkSize) relative to the position given by from, or
     raises the exception posRange if the required position cannot be
     represented as a value of type FilePos.
  *)
VAR
   d: DeviceTablePtr ;
BEGIN
   IF IsRndFile(cid)
   THEN
      d := DeviceTablePtrValue(cid, did, IOChan.wrongDevice,
                               'RndFile.Rewrite: incorrect channel') ;
      WITH d^ DO
         RETURN( from+VAL(FilePos, chunks*VAL(INTEGER, chunkSize))-
                 CurrentPos(cid) )
      END
   ELSE
      RAISEdevException(cid, did, IOChan.wrongDevice,
                        'RndFile.' + __FUNCTION__ +
                        ': channel is not a sequential file')
   END
END NewPos ;


PROCEDURE SetPos (cid: ChanId; pos: FilePos);
  (* If the channel identified by cid is not open to a random access file,
     the exception wrongDevice is raised; otherwise sets the read/write
     position to the value given by pos.
  *)
VAR
   d: DeviceTablePtr ;
BEGIN
   IF IsRndFile(cid)
   THEN
      d := DeviceTablePtrValue(cid, did, IOChan.wrongDevice,
                               'RndFile.Rewrite: incorrect channel') ;
      WITH d^ DO
         FIO.SetPositionFromBeginning(RTio.GetFile(cid), pos) ;
         checkErrno(dev, d)
      END
   ELSE
      RAISEdevException(cid, did, IOChan.wrongDevice,
                        'RndFile.' + __FUNCTION__ +
                        ': channel is not a sequential file')
   END
END SetPos ;


PROCEDURE Close (VAR cid: ChanId);
  (* If the channel identified by cid is not open to a random access file,
     the exception wrongDevice is raised; otherwise closes the channel,
     and assigns the value identifying the invalid channel to cid.
  *)
BEGIN
   IF IsRndFile(cid)
   THEN
      UnMakeChan(did, cid) ;
      cid := IOChan.InvalidChan()
   ELSE
      RAISEdevException(cid, did, IOChan.wrongDevice,
                        'RndFile.' + __FUNCTION__ +
                        ': channel is not a random file')
   END
END Close ;


(*
   Init - 
*)

PROCEDURE Init ;
VAR
   gen: GenDevIF ;
BEGIN
   AllocateDeviceId(did) ;
   gen := InitGenDevIF(did, doreadchar, dounreadchar,
                       dogeterrno, dorbytes, dowbytes,
                       dowriteln,
                       iseof, iseoln, iserror) ;
   dev := InitChanDev(streamfile, did, gen) ;
   AllocateSource (rndfileException)
END Init ;


BEGIN
   Init
END RndFile.