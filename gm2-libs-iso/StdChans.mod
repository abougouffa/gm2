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

IMPLEMENTATION MODULE StdChans ;

IMPORT FIO, RTio, ChanConsts, IOConsts ;

VAR
   stdin, stdout,
   stderr, stdnull,
   in, out, err   : ChanId ;


PROCEDURE StdInChan (): ChanId;
  (* Returns the identity of the implementation-defined standard source for
     program input.
  *)
BEGIN
   RETURN( stdin )
END StdInChan ;


PROCEDURE StdOutChan (): ChanId;
  (* Returns the identity of the implementation-defined standard source for program
     output.
  *)
BEGIN
   RETURN( stdout )
END StdOutChan ;


PROCEDURE StdErrChan (): ChanId;
  (* Returns the identity of the implementation-defined standard destination for program
     error messages.
  *)
BEGIN
   RETURN( stderr )
END StdErrChan ;


PROCEDURE NullChan (): ChanId;
  (* Returns the identity of a channel open to the null device. *)
BEGIN
   RETURN( stdnull )
END NullChan ;


  (* The following functions return the default channel values *)

PROCEDURE InChan (): ChanId;
  (* Returns the identity of the current default input channel. *)
BEGIN
   RETURN( in )
END InChan ;


PROCEDURE OutChan (): ChanId;
  (* Returns the identity of the current default output channel. *)
BEGIN
   RETURN( out )
END OutChan ;


PROCEDURE ErrChan (): ChanId;
  (* Returns the identity of the current default error message channel. *)
BEGIN
   RETURN( err )
END ErrChan ;

  (* The following procedures allow for redirection of the default channels *)

PROCEDURE SetInChan (cid: ChanId);
  (* Sets the current default input channel to that identified by cid. *)
BEGIN
   in := cid
END SetInChan ;


PROCEDURE SetOutChan (cid: ChanId);
  (* Sets the current default output channel to that identified by cid. *)
BEGIN
   out := cid
END SetOutChan ;


PROCEDURE SetErrChan (cid: ChanId);
  (* Sets the current default error channel to that identified by cid. *)
BEGIN
   err := cid
END SetErrChan ;


(*
   SafeClose - closes the channel, c, if it is open.
*)

PROCEDURE SafeClose (c: ChanId) ;
BEGIN
   IF (c#ChanId(RTio.NilChanId())) AND (ChanConsts.opened=RTio.GetOpen(c))
   THEN
      FIO.Close(RTio.GetFile(c)) ;
      RTio.SetOpen(c, ChanConsts.noSuchFile)
   END
END SafeClose ;


BEGIN
   stdin := ChanId(RTio.SetChanId(RTio.InitChanId(),
                                  FIO.StdIn,
                                  ChanConsts.read+ChanConsts.raw+ChanConsts.interactive,
                                  ChanConsts.opened,
                                  IOConsts.notKnown,
                                  0)) ;
   stdout := ChanId(RTio.SetChanId(RTio.InitChanId(),
                                   FIO.StdOut,
                                   ChanConsts.write+ChanConsts.raw+ChanConsts.interactive,
                                   ChanConsts.opened,
                                   IOConsts.notKnown,
                                   0)) ;
   stderr := ChanId(RTio.SetChanId(RTio.InitChanId(),
                                   FIO.StdErr,
                                   ChanConsts.write+ChanConsts.raw+ChanConsts.interactive,
                                   ChanConsts.opened,
                                   IOConsts.notKnown,
                                   0)) ;
   stdnull := ChanId(RTio.SetChanId(RTio.InitChanId(),
                                    FIO.OpenForRandom('/dev/null', TRUE),
                                    ChanConsts.read+ChanConsts.write+ChanConsts.raw,
                                    ChanConsts.opened,
                                    IOConsts.notKnown,
                                    0)) ;
   in := stdin ;
   out := stdout ;
   err := stderr
FINALLY
   SafeClose(in) ;
   SafeClose(out) ;
   SafeClose(err)
END StdChans.
