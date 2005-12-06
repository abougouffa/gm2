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

IMPLEMENTATION MODULE Environment; (* AFB 4/85 *)

   FROM SysLocations IMPORT Environment;
   FROM SYSTEM IMPORT ADDRESS, WORD;

   (* $T- *)
   (* $R- *)

   CONST
      MaxEnv = 1;
      MaxText = 1;
   TYPE
      ArgList = POINTER TO ARRAY[0..MaxEnv-1] OF
			POINTER TO ARRAY[0..MaxText-1] OF CHAR;
   VAR
      Env: ArgList;
      MaxEnvIndex: CARDINAL;
      NULL: ADDRESS;

   PROCEDURE GetEnv(name: ARRAY OF CHAR; (* parameter name to be looked for *)
		    VAR text: ARRAY OF CHAR; (* parameter contents *)
		    VAR ok: BOOLEAN);
      VAR index, i, j, namelen: CARDINAL;
   BEGIN
      ok := TRUE;
      namelen := 0;
      WHILE (namelen <= HIGH(name)) AND (name[namelen] <> 0C) DO
	 INC(namelen);
      END;
      FOR index := 0 TO MaxEnvIndex-1 DO
	 i := 0;
	 (* compare parameter names *)
	 WHILE (i < namelen) AND (Env^[index]^[i] = name[i]) DO
	    INC(i);
	 END;
	 IF (i = namelen) AND (Env^[index]^[i] = '=') THEN (* found ? *)
	    (* copy parameter contents *)
	    FOR j := 0 TO HIGH(text) DO
	       INC(i);
	       text[j] := Env^[index]^[i];
	       IF text[j] = 0C THEN
		  RETURN
	       END;
	    END;
	    RETURN
	 END;
      END;
      ok := FALSE;
   END GetEnv;

   PROCEDURE EnvPar(index: CARDINAL; (* ranging [0.. MaxEnvIndex-1] *)
		    VAR text: ARRAY OF CHAR; (* "name=contents" *)
		    VAR ok: BOOLEAN);
      VAR i: CARDINAL;

   BEGIN
      IF index >= MaxEnvIndex THEN
	 ok := FALSE;
      ELSE
	 ok := TRUE;
	 FOR i := 0 TO HIGH(text) DO
	    text[i] := Env^[index]^[i];
	    IF text[i] = 0C THEN
	       RETURN
	    END;
	 END;
      END;
   END EnvPar;

BEGIN
   Env := Environment;
   MaxEnvIndex := 0;
   NULL := ADDRESS(0);
   WHILE Env^[MaxEnvIndex] <> NULL DO
      INC(MaxEnvIndex);
   END;
END Environment.
(*
 * Local variables:
 *  compile-command: "gm2 -c -g -I../sys:. Environment.mod"
 * End:
 *)
