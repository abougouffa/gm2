(* Copyright (C) 2003 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE IOChan ;

IMPORT FIO ;

TYPE
  ChanId = FIO.File ; (* Values of this type are used to identify channels *)


PROCEDURE InvalidChan (): ChanId;
  (* Returns the value identifying the invalid channel. *)
BEGIN
   RETURN -1
END InvalidChan ;


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

PROCEDURE Look (cid: ChanId; VAR ch: CHAR; VAR res: IOConsts.ReadResults);
  (* If there is a character as the next item in the input stream cid,
     assigns its value to ch without removing it from the stream;
     otherwise the value of ch is not defined.
     res (and the stored read result) are set to the value
     allRight, endOfLine, or endOfInput.
  *)
VAR
   f: File ;
BEGIN
   f := File(cid) ;
   ch := ReadChar(f) ;
   IF FIO.IsNoError(f)
   THEN
      FIO.UnReadChar(f, ch) ;
      res := IOConsts.allRight
   END ;
   IF FIO.EOLN(f)
   THEN
      res := IOConsts.eofOfLine
   ELSIF FIO.EOF(f)
   THEN
      res := IOConsts.endOfInput
   END
END Look ;


PROCEDURE Skip (cid: ChanId);
  (* If the input stream cid has ended, the exception skipAtEnd is raised;
     otherwise the next character or line mark in cid is removed,
     and the stored read result is set to the value allRight.
  *)
VAR
   f: File ;
BEGIN
   f := File(cid) ;
   IF EOF(f)
   THEN
      (* raise exception skipAtEnd *)
   END ;
   ch := ReadChar(f) ;
   IF FIO.IsNoError(f)
   THEN
      res := IOConsts.allRight
   END
END Skip ;


PROCEDURE SkipLook (cid: ChanId; VAR ch: CHAR; VAR res: IOConsts.ReadResults);
  (* If the input stream cid has ended, the exception skipAtEnd is raised;
     otherwise the next character or line mark in cid is removed.
     If there is a character as the next item in cid stream,
     assigns its value to ch without removing it from the stream.
     Otherwise, the value of ch is not defined.
     res (and the stored read result) are set to the value allRight,
     endOfLine, or endOfInput.
  *)
VAR
   f: File ;
BEGIN
   f := File(cid) ;
   IF EOF(f)
   THEN
      (* raise exception skipAtEnd *)
   END ;
   ch := ReadChar(f) ;
   IF FIO.IsNoError(f)
   THEN
      FIO.UnReadChar(f, ch) ;
      res := IOConsts.allRight
   END ;
   IF FIO.EOLN(f)
   THEN
      res := IOConsts.eofOfLine
   ELSIF FIO.EOF(f)
   THEN
      res := IOConsts.endOfInput
   END
END SkipLook ;

PROCEDURE WriteLn (cid: ChanId);
  (* Writes a line mark over the channel cid. *)
VAR
   f: File ;
BEGIN
   f := File(cid) ;
   FIO.WriteLine(f)
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
   (* --fixme-- complete it *)
END TextRead ;

PROCEDURE TextWrite (cid: ChanId; from: SYSTEM.ADDRESS;
                     charsToWrite: CARDINAL);
  (* Writes a number of characters given by the value of charsToWrite,
     from successive components of an ARRAY OF CHAR variable for which
     the address of the first component is from, to the channel cid.
  *)
BEGIN
   (* --fixme-- complete it *)
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
   locsRead := FIO.ReadNBytes(File(cid), maxLocs, to)
END RawRead ;

PROCEDURE RawWrite (cid: ChanId; from: SYSTEM.ADDRESS; locsToWrite: CARDINAL);
  (* Writes a number of items given by the value of charsToWrite,
     from successive components of an ARRAY OF LOC variable for
     which the address of the first component is from, to the channel cid.
  *)
BEGIN
   IF locsToWrite#FIO.WriteNBytes(File(cid), from, locsToWrite)
   THEN
      (* should we raise an exception, perhaps not,
         but something has gone wrong *)
   END
END RawWrite ;

  (* Common operations *)

PROCEDURE GetName (cid: ChanId; VAR s: ARRAY OF CHAR);
  (* Copies to s a name associated with the channel cid, possibly truncated
     (depending on the capacity of s).
  *)

PROCEDURE Reset (cid: ChanId);
  (* Resets the channel cid to a state defined by the device module. *)

PROCEDURE Flush (cid: ChanId);
  (* Flushes any data buffered by the device module out to the channel cid. *)

  (* Access to read results *)

PROCEDURE SetReadResult (cid: ChanId; res: IOConsts.ReadResults);
  (* Sets the read result value for the channel cid to the value res. *)

PROCEDURE ReadResult (cid: ChanId): IOConsts.ReadResults;
  (* Returns the stored read result value for the channel cid.
     (This is initially the value notKnown).
  *)

  (* Users can discover which flags actually apply to a channel *)

PROCEDURE CurrentFlags (cid: ChanId): ChanConsts.FlagSet;
  (* Returns the set of flags that currently apply to the channel cid. *)

  (* The following exceptions are defined for this module and its clients *)

TYPE
  ChanExceptions =
    (wrongDevice,      (* device specific operation on wrong device *)
     notAvailable,     (* operation attempted that is not available on that channel *)
     skipAtEnd,        (* attempt to skip data from a stream that has ended *)
     softDeviceError,  (* device specific recoverable error *)
     hardDeviceError,  (* device specific non-recoverable error *)
     textParseError,   (* input data does not correspond to a character
                          or line mark - optional detection *)
     notAChannel       (* given value does not identify a channel
                          - optional detection *)
    );

PROCEDURE IsChanException (): BOOLEAN;
  (* Returns TRUE if the current coroutine is in the exceptional
     execution state because of the raising of an exception from
     ChanExceptions; otherwise returns FALSE.
  *)

PROCEDURE ChanException (): ChanExceptions;
  (* If the current coroutine is in the exceptional execution state
     because of the raising of an exception from ChanExceptions,
     returns the corresponding enumeration value, and otherwise
     raises an exception.
  *)

  (* When a device procedure detects a device error, it raises the
     exception softDeviceError or hardDeviceError.  If these exceptions
     are handled, the following facilities may be used to discover
     an implementation-defined error number for the channel.
  *)

TYPE
  DeviceErrNum = INTEGER;

PROCEDURE DeviceError (cid: ChanId): DeviceErrNum;
  (* If a device error exception has been raised for the channel cid,
     returns the error number stored by the device module.
  *)

END IOChan.
