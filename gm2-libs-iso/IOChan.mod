(* Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008
   Free Software Foundation, Inc. *)
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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE IOChan ;

IMPORT FIO, EXCEPTIONS, M2EXCEPTION, RTio, IOConsts, errno, ErrnoCategory ;

FROM EXCEPTIONS IMPORT ExceptionSource, RAISE, AllocateSource,
                       IsCurrentSource, IsExceptionalExecution ;
FROM Storage IMPORT ALLOCATE ;


TYPE
   ChanId = RTio.ChanId ;

VAR
   iochan: ExceptionSource ;


PROCEDURE InvalidChan () : ChanId ;
  (* Returns the value identifying the invalid channel. *)
BEGIN
   RETURN( RTio.NilChanId() )
END InvalidChan ;


PROCEDURE CheckValid (cid: ChanId) ;
  (* internal routine to check whether we have a valid channel *)
BEGIN
   IF cid=InvalidChan()
   THEN
      RAISE(iochan, ORD(notAChannel), 'ChanId specified is invalid')
   END
END CheckValid ;


PROCEDURE CheckErrno (cid: ChanId) ;
  (* internal routine to check a number of error conditions *)
VAR
   e: INTEGER ;
BEGIN
   CheckValid(cid) ;
   IF NOT FIO.IsNoError(RTio.GetFile(cid))
   THEN
      e := errno.geterrno() ;
      IF ErrnoCategory.IsErrnoHard(e)
      THEN
         RTio.SetError(cid, e) ;
         RAISE(iochan, ORD(hardDeviceError), 'unrecoverable (errno)')
      ELSIF ErrnoCategory.UnAvailable(e)
      THEN
         RTio.SetError(cid, e) ;
         RAISE(iochan, ORD(notAvailable), 'unavailable (errno)')
      ELSIF e>0
      THEN
         RTio.SetError(cid, e) ;
         RAISE(iochan, ORD(softDeviceError), 'recoverable (errno)')
      END
   END
END CheckErrno ;


PROCEDURE CheckPreRead (cid: ChanId) ;
BEGIN
   CheckValid(cid) ;
   IF FIO.EOLN(RTio.GetFile(cid))
   THEN
      RTio.SetRead(cid, IOConsts.endOfLine)
   ELSIF FIO.EOF(RTio.GetFile(cid))
   THEN
      RTio.SetRead(cid, IOConsts.endOfInput) ;
      RAISE(iochan, ORD(skipAtEnd), 'attempting to read beyond end of file')
   END
END CheckPreRead ;


PROCEDURE CheckPostRead (cid: ChanId) ;
BEGIN
   CheckValid(cid) ;
   CheckErrno(cid) ;
   IF FIO.EOLN(RTio.GetFile(cid))
   THEN
      RTio.SetRead(cid, IOConsts.endOfLine)
   ELSIF FIO.EOF(RTio.GetFile(cid))
   THEN
      RTio.SetRead(cid, IOConsts.endOfInput)
   ELSE
      RTio.SetRead(cid, IOConsts.allRight)
   END
END CheckPostRead ;


PROCEDURE CheckPreWrite (cid: ChanId) ;
BEGIN
   CheckValid(cid)
END CheckPreWrite ;


PROCEDURE CheckPostWrite (cid: ChanId) ;
BEGIN
   CheckValid(cid) ;
   CheckErrno(cid)   
END CheckPostWrite ;


  (* For each of the following operations, if the device supports the
     operation on the channel, the behaviour of the procedure conforms
     with the description below.  The full behaviour is defined for
     each device module.  If the device does not support the operation
     on the channel, the behaviour of the procedure is to raise the exception
     notAvailable.
  *)

  (* Text operations - these perform any required translation between
     the internal and external representation of text.
  *)

PROCEDURE Look (cid: ChanId; VAR ch: CHAR; VAR res: IOConsts.ReadResults) ;
  (* If there is a character as the next item in the input stream cid,
     assigns its value to ch without removing it from the stream;
     otherwise the value of ch is not defined.
     res (and the stored read result) are set to the value
     allRight, endOfLine, or endOfInput.
  *)
VAR
   f: FIO.File ;
BEGIN
   CheckValid(cid) ;
   f := RTio.GetFile(cid) ;
   IF FIO.EOF(f)
   THEN
      RTio.SetRead(cid, IOConsts.endOfInput)
   ELSE
      ch := FIO.ReadChar(f) ;
      FIO.UnReadChar(f, ch) ;
      CheckPostRead(cid)
   END ;
   res := RTio.GetRead(cid)
END Look ;


PROCEDURE Skip (cid: ChanId) ;
  (* If the input stream cid has ended, the exception skipAtEnd is raised;
     otherwise the next character or line mark in cid is removed,
     and the stored read result is set to the value allRight.
  *)
VAR
   f : FIO.File ;
   ch: CHAR ;
BEGIN
   CheckPreRead(cid) ;
   f := RTio.GetFile(cid) ;
   ch := FIO.ReadChar(f) ;
   CheckPostRead(cid)
END Skip ;


PROCEDURE SkipLook (cid: ChanId; VAR ch: CHAR; VAR res: IOConsts.ReadResults) ;
  (* If the input stream cid has ended, the exception skipAtEnd is raised;
     otherwise the next character or line mark in cid is removed.
     If there is a character as the next item in cid stream,
     assigns its value to ch without removing it from the stream.
     Otherwise, the value of ch is not defined.
     res (and the stored read result) are set to the value allRight,
     endOfLine, or endOfInput.
  *)
VAR
   f: FIO.File ;
BEGIN
   CheckPreRead(cid) ;
   f := RTio.GetFile(cid) ;
   ch := FIO.ReadChar(f) ;
   IF FIO.IsNoError(f)
   THEN
      FIO.UnReadChar(f, ch) ;
      CheckPostRead(cid)
   END ;
   res := RTio.GetRead(cid)
END SkipLook ;


PROCEDURE WriteLn (cid: ChanId) ;
  (* Writes a line mark over the channel cid. *)
VAR
   f: FIO.File ;
BEGIN
   CheckPreWrite(cid) ;
   f := RTio.GetFile(cid) ;
   FIO.WriteLine(f) ;
   CheckPostWrite(cid)
END WriteLn ;


PROCEDURE TextRead (cid: ChanId; to: SYSTEM.ADDRESS; maxChars: CARDINAL;
                    VAR charsRead: CARDINAL);
  (* Reads at most maxChars characters from the current line in cid,
     and assigns corresponding values to successive components of an
     ARRAY OF CHAR variable for which the address of the first
     component is to. The number of characters read is assigned
     to charsRead. The stored read result is set to allRight, 
     endOfLine, or endOfInput.
  *)
BEGIN
   CheckPreRead(cid) ;
   charsRead := FIO.ReadNBytes(RTio.GetFile(cid), maxChars, to) ;
   CheckPostRead(cid)
END TextRead ;

PROCEDURE TextWrite (cid: ChanId; from: SYSTEM.ADDRESS;
                     charsToWrite: CARDINAL);
  (* Writes a number of characters given by the value of charsToWrite,
     from successive components of an ARRAY OF CHAR variable for which
     the address of the first component is from, to the channel cid.
  *)
BEGIN
   CheckPreWrite(cid) ;
   IF charsToWrite#FIO.WriteNBytes(RTio.GetFile(cid), charsToWrite, from)
   THEN
      (* this error will be caught by checking errno  *)
      CheckErrno(cid) ;
      (* if our target system does not support errno then we *)
      RAISE(iochan, ORD(hardDeviceError), 'unrecoverable errno')
   END ;
   CheckPostWrite(cid)
END TextWrite ;

  (* Direct raw operations - these do not effect translation between
     the internal and external representation of data
  *)

PROCEDURE RawRead (cid: ChanId; to: SYSTEM.ADDRESS; maxLocs: CARDINAL;
                   VAR locsRead: CARDINAL);
  (* Reads at most maxLocs items from cid, and assigns corresponding
     values to successive components of an ARRAY OF LOC variable for
     which the address of the first component is to. The number of
     characters read is assigned to charsRead. The stored read result
     is set to the value allRight, or endOfInput.
  *)
BEGIN
   CheckPreRead(cid) ;
   locsRead := FIO.ReadNBytes(RTio.GetFile(cid), maxLocs, to) ;
   CheckPostRead(cid)
END RawRead ;

PROCEDURE RawWrite (cid: ChanId; from: SYSTEM.ADDRESS; locsToWrite: CARDINAL);
  (* Writes a number of items given by the value of charsToWrite,
     from successive components of an ARRAY OF LOC variable for
     which the address of the first component is from, to the channel cid.
  *)
BEGIN
   CheckPreWrite(cid) ;
   IF locsToWrite#FIO.WriteNBytes(RTio.GetFile(cid), locsToWrite, from)
   THEN
      (* this error will be caught by checking errno  *)
      CheckErrno(cid) ;
      (* if our target system does not support errno then we *)
      RAISE(iochan, ORD(hardDeviceError), 'unrecoverable errno')
   END ;
   CheckPostWrite(cid)
END RawWrite ;

  (* Common operations *)

PROCEDURE GetName (cid: ChanId; VAR s: ARRAY OF CHAR);
  (* Copies to s a name associated with the channel cid, possibly truncated
     (depending on the capacity of s).
  *)
BEGIN
   CheckValid(cid) ;
   FIO.GetFileName(RTio.GetFile(cid), s)
END GetName ;

PROCEDURE Reset (cid: ChanId);
  (* Resets the channel cid to a state defined by the device module. *)
BEGIN
   CheckValid(cid) ;
   RTio.SetError(cid, 0)
END Reset ;

PROCEDURE Flush (cid: ChanId);
  (* Flushes any data buffered by the device module out to the channel cid. *)
BEGIN
   CheckValid(cid) ;
   FIO.FlushBuffer(RTio.GetFile(cid)) ;
   CheckErrno(cid)
END Flush ;

  (* Access to read results *)

PROCEDURE SetReadResult (cid: ChanId; res: IOConsts.ReadResults);
  (* Sets the read result value for the channel cid to the value res. *)
BEGIN
   CheckValid(cid) ;
   RTio.SetRead(cid, res)
END SetReadResult ;

PROCEDURE ReadResult (cid: ChanId): IOConsts.ReadResults;
  (* Returns the stored read result value for the channel cid.
     (This is initially the value notKnown).
  *)
BEGIN
   CheckValid(cid) ;
   RETURN( RTio.GetRead(cid) )
END ReadResult ;

  (* Users can discover which flags actually apply to a channel *)

PROCEDURE CurrentFlags (cid: ChanId): ChanConsts.FlagSet;
  (* Returns the set of flags that currently apply to the channel cid. *)
BEGIN
   CheckValid(cid) ;
   RETURN( RTio.GetFlags(cid) )
END CurrentFlags ;

  (* The following exceptions are defined for this module and its clients *)

PROCEDURE IsChanException (): BOOLEAN;
  (* Returns TRUE if the current coroutine is in the exceptional
     execution state because of the raising of an exception from
     ChanExceptions; otherwise returns FALSE.
  *)
BEGIN
   RETURN( IsExceptionalExecution() AND IsCurrentSource(iochan) )
END IsChanException ;


PROCEDURE ChanException (): ChanExceptions;
  (* If the current coroutine is in the exceptional execution state
     because of the raising of an exception from ChanExceptions,
     returns the corresponding enumeration value, and otherwise
     raises an exception.
  *)
BEGIN
   IF IsChanException()
   THEN
      RETURN( VAL(ChanExceptions, EXCEPTIONS.CurrentNumber(iochan)) )
   ELSE
      RAISE(iochan, ORD(M2EXCEPTION.exException),
            'coroutine is not in the exceptional state caused by IOChan')
   END
END ChanException ;

  (* When a device procedure detects a device error, it raises the
     exception softDeviceError or hardDeviceError.  If these exceptions
     are handled, the following facilities may be used to discover
     an implementation-defined error number for the channel.
  *)

PROCEDURE DeviceError (cid: ChanId): DeviceErrNum;
  (* If a device error exception has been raised for the channel cid,
     returns the error number stored by the device module.
  *)
BEGIN
   CheckValid(cid) ;
   RETURN( RTio.GetError(cid) )
END DeviceError ;


BEGIN
   AllocateSource(iochan)
END IOChan.
