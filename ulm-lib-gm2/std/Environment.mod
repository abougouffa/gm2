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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   ----------------------------------------------------------------------------
   E-mail contact: gm2@glam.ac.uk
   ----------------------------------------------------------------------------
   $Id: Environment.mod,v 1.2 2004/06/29 08:51:41 gaius Exp $
   ----------------------------------------------------------------------------
   $Log: Environment.mod,v $
   Revision 1.2  2004/06/29 08:51:41  gaius
   * made flex lexical analysers ignore carriage return
   * fixed bug in M2Quads.mod checking parameter of
     a const var before value was known.
   * fixed local MODULEs so that they can FROM mod IMPORT
   * tidied up some ulm implementation modules in ulm-lib-gm2/std

   Revision 1.1  2003/12/27 00:16:05  gaius
   added ulm libraries into the gm2 tree. Currently these
   are only used when regression testing, but later they
   will be accessible by users of gm2.

   Revision 0.2  1997/02/28  15:50:00  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:23  borchert
   Initial revision

   ----------------------------------------------------------------------------
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
