(* Copyright (C) 2004, 2005 Free Software Foundation, Inc. *)
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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

This file was originally part of the University of Ulm library
*)


(* Ulm's Modula-2 Library
   Copyright (C) 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991,
   1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
   2002, 2003, 2004, 2005
   by University of Ulm, SAI, D-89069 Ulm, Germany
*)

IMPLEMENTATION MODULE PipeIO; (* AFB 6/84 *)

   FROM SYSTEM IMPORT ADR, ADDRESS;
   FROM StdIO IMPORT FILE, MODE, Fdopen, Fclose;
   FROM SysFork IMPORT Fork;
   FROM SysExec IMPORT Exec;
   FROM SysWait IMPORT Wait;
   FROM SysPipe IMPORT Pipe;
   FROM SysExit IMPORT Exit;
   FROM UnixString IMPORT Buffer, Copy;
   IMPORT SysClose;
   FROM SysDup IMPORT Dup2;

   PROCEDURE Close(fd: CARDINAL);
   BEGIN
      IF SysClose.Close(fd) THEN (* ignore result *) END;
   END Close;

   PROCEDURE Popen(VAR f: FILE; cmd: ARRAY OF CHAR; mode: MODE;
                   buffered: BOOLEAN) : BOOLEAN;
      CONST
         stdin = 0;
         stdout = 1;
      VAR
         fd: CARDINAL;
         args: ARRAY[0..3] OF ADDRESS;
         arg1, arg2: ARRAY[0..15] OF CHAR;
         child: CARDINAL;
         ReadFileDesc, WriteFileDesc: CARDINAL;
         cmdbuf: Buffer; (* cmd with terminating 0-byte *)
   BEGIN
      IF NOT Pipe(ReadFileDesc, WriteFileDesc) THEN RETURN FALSE END;
      IF NOT Fork(child) THEN
         Close(ReadFileDesc);
         Close(WriteFileDesc);
         RETURN FALSE;
      END;
      IF child = 0 THEN (* son *)
         IF mode = read THEN
            IF NOT Dup2(WriteFileDesc, stdout) THEN Exit(1) END;
         ELSE
            IF NOT Dup2(ReadFileDesc, stdin) THEN Exit(1) END;
         END;
         Close(ReadFileDesc);
         Close(WriteFileDesc);
         arg1 := "/bin/sh";
         arg2 := "-c";
         args[0] := ADR(arg1);
         args[1] := ADR(arg2);
         Copy(cmdbuf, cmd);
         args[2] := ADR(cmdbuf);
         args[3] := ADDRESS(0);
         Exec(arg1, ADR(args));
         Exit(1);
      END;
      (* father *)
      IF mode = read THEN
         Close(WriteFileDesc);
         fd := ReadFileDesc;
      ELSE
         Close(ReadFileDesc);
         fd := WriteFileDesc;
      END;
      RETURN Fdopen(f, fd, mode, buffered);
   END Popen;

   PROCEDURE Pclose(f: FILE) : BOOLEAN;
      VAR child, status: CARDINAL; resultofclose: BOOLEAN;
   BEGIN
      resultofclose := Fclose(f);
      IF NOT Wait(child, status) THEN (* ignore result *) END;
      RETURN resultofclose AND (status = 0);
   END Pclose;

END PipeIO.
