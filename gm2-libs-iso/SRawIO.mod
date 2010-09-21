(* Copyright (C) 2009, 2010 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE SRawIO ;

IMPORT StdChans, RawIO ;

(*
   Read - reads storage units from the default input channel, and
          assigns them to successive components of to.  The read
          result is set to the value allRight, wrongFormat, or
          endOfInput.
*)

PROCEDURE Read (VAR to: ARRAY OF SYSTEM.LOC) ;
BEGIN
   RawIO.Read(StdChans.StdInChan(), to)
END Read ;


(*
   Write - writes storage units to the default output channel from
           successive components of from.
*)

PROCEDURE Write (from: ARRAY OF SYSTEM.LOC) ;
BEGIN
   RawIO.Write(StdChans.StdOutChan(), from)   
END Write ;


END SRawIO.
