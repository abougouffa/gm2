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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE StdChans ;


PROCEDURE StdInChan (): ChanId;
  (* Returns the identity of the implementation-defined standard source for
     program input.
  *)
BEGIN
   
END StdInChan ;


PROCEDURE StdOutChan (): ChanId;
  (* Returns the identity of the implementation-defined standard source for program
     output.
  *)
BEGIN

END StdOutChan ;


PROCEDURE StdErrChan (): ChanId;
  (* Returns the identity of the implementation-defined standard destination for program
     error messages.
  *)
BEGIN
END StdErrChan ;


PROCEDURE NullChan (): ChanId;
  (* Returns the identity of a channel open to the null device. *)
BEGIN
END NullChan ;


  (* The following functions return the default channel values *)

PROCEDURE InChan (): ChanId;
  (* Returns the identity of the current default input channel. *)
BEGIN
END InChan ;


PROCEDURE OutChan (): ChanId;
  (* Returns the identity of the current default output channel. *)
BEGIN
   
END OutChan ;


PROCEDURE ErrChan (): ChanId;
  (* Returns the identity of the current default error message channel. *)
BEGIN
   
END ErrChan ;

  (* The following procedures allow for redirection of the default channels *)

PROCEDURE SetInChan (cid: ChanId);
  (* Sets the current default input channel to that identified by cid. *)
BEGIN
   
END SetInChan ;


PROCEDURE SetOutChan (cid: ChanId);
  (* Sets the current default output channel to that identified by cid. *)
BEGIN

ENd SetOutChan ;


PROCEDURE SetErrChan (cid: ChanId);
  (* Sets the current default error channel to that identified by cid. *)
BEGIN
   
END SetErrChan ;


END StdChans.
