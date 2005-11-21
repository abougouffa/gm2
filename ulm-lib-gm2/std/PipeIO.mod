(* Ulm's Modula-2 Library
   Copyright (C) 1984-1997 by University of Ulm, SAI, D-89069 Ulm, Germany
   ----------------------------------------------------------------------------
   Ulm's Modula-2 Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either version
   2 of the License, or (at your option) any later version.

   Ulm's Modula-2 Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
   ----------------------------------------------------------------------------
   E-mail contact: gm2@glam.ac.uk
   ----------------------------------------------------------------------------
   $Id: PipeIO.mod,v 1.4 2005/11/21 12:09:59 gaius Exp $
   ----------------------------------------------------------------------------
   $Log: PipeIO.mod,v $
   Revision 1.4  2005/11/21 12:09:59  gaius
   updated Copyright notices and dates

   Revision 1.3  2004/06/29 08:51:42  gaius
   * made flex lexical analysers ignore carriage return
   * fixed bug in M2Quads.mod checking parameter of
     a const var before value was known.
   * fixed local MODULEs so that they can FROM mod IMPORT
   * tidied up some ulm implementation modules in ulm-lib-gm2/std

   Revision 1.2  2004/05/05 21:34:58  gaius
   * added SHIFT and ROTATE into ISO SYSTEM and
     made the compiler shift and rotate word and multi-word
     set types. Multi-word set rotate and shifts are implemented
     by calling ISO SYSTEM runtime procedures. Word sized sets or
     smaller are implemented inline using shift/rotate instructions.
     Currently not yet debugged, but mostly complete code.

   * fixed bug report by Paul Whittington <pwhittington@nitrodata.com>
     (see testsuite/gm2/link/pim/fail/import.mod).

   * updated gm2.texi to reflect new options and changes to the
     run-time system.

   * introduced -Wunbounded-by-reference option which will make a
     reference to non VAR unbounded data providing it is not written to
     within the callee procedure.
   * introduced -Wverbose-unbounded option which displays names of
     unbounded parameters which the compiler will implement as
     references even though they were specified as non VAR parameters.

   * introduced -Wcase, -Wnil runtime checks
   * introduced -Wcheck-all to enable all runtime flags
   * updated documentation to refect new options

   Revision 1.1  2003/12/27 00:16:05  gaius
   added ulm libraries into the gm2 tree. Currently these
   are only used when regression testing, but later they
   will be accessible by users of gm2.

   Revision 0.2  1997/02/28  15:50:18  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:29  borchert
   Initial revision

   ----------------------------------------------------------------------------
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
