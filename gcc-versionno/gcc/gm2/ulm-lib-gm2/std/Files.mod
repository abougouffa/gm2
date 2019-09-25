(* Files.mod.

Copyright (C) 2004-2019 Free Software Foundation, Inc.
Contributed by University of Ulm.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)


(* Ulm's Modula-2 Library
   Copyright (C) 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991,
   1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
   2002, 2003, 2004, 2005
   by University of Ulm, SAI, D-89069 Ulm, Germany
*)

IMPLEMENTATION MODULE Files;

   (* high level module for file handling *)

   FROM StdIO IMPORT read, write, Fopen, Fclose, Fputc, Fgetc,
      Ftell, Fseek;
   FROM SysLink IMPORT Link;
   FROM SysUnlink IMPORT Unlink;
   FROM SystemTypes IMPORT OFF;

   (* (* from definition module *)

   IMPORT StdIO;

   TYPE FILE = StdIO.FILE;

   VAR Done: BOOLEAN;

   *)

   PROCEDURE OpenRead(VAR f: FILE; filename: ARRAY OF CHAR);
   BEGIN
      Done := Fopen(f, filename, read, (* buffered = *) TRUE);
   END OpenRead;

   PROCEDURE OpenWrite(VAR f: FILE; filename: ARRAY OF CHAR);
   BEGIN
      Done := Fopen(f, filename, write, (* buffered = *) TRUE);
   END OpenWrite;

   PROCEDURE Close(f: FILE);
   BEGIN
      Done := Fclose(f);
   END Close;

   PROCEDURE SetPos(f: FILE; pos: OFF);
   BEGIN
      Done := Fseek(f, pos, 0);
   END SetPos;

   PROCEDURE GetPos(f: FILE; VAR pos: OFF);
   BEGIN
      Done := Ftell(f, pos);
   END GetPos;

   PROCEDURE Reset(f: FILE);
   BEGIN
      Done := Fseek(f, 0, 0);
   END Reset;

   PROCEDURE Delete(filename: ARRAY OF CHAR);
   BEGIN
      Done := Unlink(filename);
   END Delete;

   PROCEDURE Rename(oldname, newname: ARRAY OF CHAR);
   BEGIN
      IF Unlink(newname) THEN (* ignore result *) END;
      (* short circuit evaluation !!! *)
      Done := Link(oldname, newname) AND Unlink(oldname);
   END Rename;

END Files.
(*
 * Local variables:
 *  compile-command: "gm2 -c -g -I../sys:. Files.mod"
 * End:
 *)
