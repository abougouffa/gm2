(* CallShell.mod.

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

IMPLEMENTATION MODULE CallShell;

   FROM SysSignal IMPORT Signal, ignore, old;
   FROM SystemTypes IMPORT Sig;
   FROM SysExec IMPORT Exec;
   FROM SysFork IMPORT Fork;
   FROM SysWait IMPORT Wait;
   FROM SysExit IMPORT Exit;
   FROM UnixString IMPORT Buffer, Copy;
   FROM SYSTEM IMPORT ADR, ADDRESS;

   PROCEDURE Shell(cmd: ARRAY OF CHAR; VAR status: CARDINAL) : BOOLEAN;
      CONST shell = "/bin/sh";
         ExecFailed = 255;
      VAR child, pid: CARDINAL;
          args: ARRAY[0..3] OF ADDRESS;
          arg1, arg2: ARRAY[0..15] OF CHAR;
          intproc, quitproc: PROC;
          ign: BOOLEAN;
          CmdBuffer: Buffer; (* cmd with null byte *)
   BEGIN
      IF NOT Fork(child) THEN RETURN FALSE END;
      IF child = 0 THEN (* son *)
         arg1 := shell; arg2 := "-c";
         args[0] := ADR(arg1);
         args[1] := ADR(arg2);
         Copy(CmdBuffer, cmd);
         args[2] := ADR(CmdBuffer);
         args[3] := ADDRESS(0);
         Exec(shell, ADR(args));
         Exit(ExecFailed);
      ELSE (* father *)
	 ign := Signal(SIGINT, ignore); intproc := old;
	 ign := Signal(SIGQUIT, ignore); quitproc := old;
         WHILE Wait(pid, status) AND (pid <> child) DO END;
         ign := Signal(SIGINT, intproc);
         ign := Signal(SIGQUIT, quitproc);
         RETURN status DIV 100H <> ExecFailed;
      END;
   END Shell;

END CallShell.
(*
 * Local variables:
 *  compile-command: "gm2 -c -g -I../sys:. CallShell.mod"
 * End:
 *)
