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
   $Id: Arguments.mod,v 1.3 2004/06/29 08:51:41 gaius Exp $
   ----------------------------------------------------------------------------
   $Log: Arguments.mod,v $
   Revision 1.3  2004/06/29 08:51:41  gaius
   * made flex lexical analysers ignore carriage return
   * fixed bug in M2Quads.mod checking parameter of
     a const var before value was known.
   * fixed local MODULEs so that they can FROM mod IMPORT
   * tidied up some ulm implementation modules in ulm-lib-gm2/std

   Revision 1.2  2004/04/05 10:42:45  gaius
   made gm2 64 bit clean, essentially this means a clear separation between
   int/word objects and pointer objects.

   Revision 1.1  2003/12/27 00:16:05  gaius
   added ulm libraries into the gm2 tree. Currently these
   are only used when regression testing, but later they
   will be accessible by users of gm2.

   Revision 0.2  1997/02/28  15:49:48  borchert
   header fixed

   Revision 0.1  1997/02/21  19:18:19  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE Arguments;	(* mh 5/85 *)
					(* rev mh 6/88 *)

   FROM ASCII	 IMPORT nl;
   FROM StdIO	 IMPORT Fputc, stderr;
   FROM StrToNum IMPORT StrToInt, StrToCard, StrToOct, StrToHex;
   FROM Strings	 IMPORT StrCpy, StrLen;
   FROM StrSpec	 IMPORT StrPartCpy;
   FROM SysExit	 IMPORT Exit;

   IMPORT Args ;

   CONST
      maxstrlen = 1024;
      defaultinfostring = "";

   TYPE
      String = ARRAY [0..maxstrlen-1] OF CHAR;

   VAR
      argindex,					(* ARGV index of argument *)
      lastarg,					(* ARGV index of last arg.*)
      arglen,					(* = StrLen(argument) *)
      charindex:  CARDINAL;
      argument,		(* argument[charindex] = next character to be read *)
      infostring: String;


   PROCEDURE ErrWrite(ch: CHAR);
   BEGIN
      IF ~Fputc(ch,stderr) THEN			(* cannot write to stderr *)
	 Exit(1)
      END;
   END ErrWrite;

   PROCEDURE ErrWriteString(str: ARRAY OF CHAR);
      VAR index: CARDINAL;
   BEGIN
      index := 0;
      WHILE (index <= HIGH(str)) & (str[index] > 0C) DO
	 ErrWrite(str[index]);
	 INC(index);
      END;
   END ErrWriteString;

   PROCEDURE Usage;					(* EXPORTED *)
      VAR cmdname: String;
   BEGIN
      IF Args.GetArg(cmdname,0)
      THEN
         ErrWriteString("Usage: ");
         ErrWriteString(cmdname); ErrWrite(" ");
         ErrWriteString(infostring); ErrWrite(nl)
      END ;
      Exit(1)						(* NO RETURN *)
   END Usage;

   PROCEDURE NextArg;
   BEGIN
      IF argindex > lastarg THEN
	 RETURN
      END;
      INC(argindex);
      IF argindex <= lastarg THEN
         IF Args.GetArg(argument, argindex) THEN
            arglen := StrLen(argument)
         END
      END;
      charindex := 0;
   END NextArg;

   PROCEDURE InitArgs(is: ARRAY OF CHAR);		(* EXPORTED *)
   BEGIN
      argindex := 0;
      NextArg;
      StrCpy(infostring,is);
   END InitArgs;

   PROCEDURE AllArgs;					(* EXPORTED *)
   BEGIN
      IF argindex <= lastarg THEN
	 Usage
      END;
   END AllArgs;

   PROCEDURE GetFlag(VAR flag: CHAR): BOOLEAN;		(* EXPORTED *)
   BEGIN
      IF (argindex > lastarg) OR (argument[0] # '-') OR
	    (argument[1] = 0C) THEN
	 RETURN FALSE
      END;
      IF charindex = 0 THEN
	 IF (argument[1] = "-") & (argument[2] = 0C) THEN
	    NextArg;
	    RETURN FALSE
	 END;
	 INC(charindex);
      END;
      flag := argument[charindex];
      INC(charindex);
      IF charindex >= arglen THEN
	 NextArg;
      END;
      RETURN TRUE
   END GetFlag;

   PROCEDURE GetOpt( VAR flag: CHAR; VAR plus: BOOLEAN): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      IF (argindex > lastarg) OR
	    (argument[0] # "-") & (argument[0] # '+') OR
	    (argument[1] = 0C) THEN
	 RETURN FALSE
      END;
      plus := argument[0] = "+";
      IF charindex = 0 THEN
	 IF ~plus & (argument[1] = "-") & (argument[2] = 0C) THEN
	    NextArg;
	    RETURN FALSE
	 END;
	 INC(charindex);
      END;
      flag := argument[charindex];
      INC(charindex);
      IF charindex >= arglen THEN
	 NextArg;
      END;
      RETURN TRUE
   END GetOpt;

   PROCEDURE GetArg(VAR arg: ARRAY OF CHAR): BOOLEAN;	(* EXPORTED *)
   BEGIN
      IF argindex > lastarg THEN
	 RETURN FALSE
      END;
      StrCpy(arg, argument);
      NextArg;
      RETURN TRUE
   END GetArg;

   PROCEDURE FetchString(VAR string: ARRAY OF CHAR);	(* EXPORTED *)
   BEGIN
      IF argindex > lastarg THEN
	 Usage
      END;
      StrPartCpy(string, argument, charindex, maxstrlen);
      NextArg;
   END FetchString;

   PROCEDURE FetchCard(  VAR number: CARDINAL);		(* EXPORTED *)
      VAR buffer: String;
   BEGIN
      FetchString(buffer);
      IF ~StrToCard(buffer, number) THEN
	 Usage
      END;
   END FetchCard;

   PROCEDURE FetchInt(   VAR number: INTEGER);		(* EXPORTED *)
      VAR buffer: String;
   BEGIN
      FetchString(buffer);
      IF ~StrToInt(buffer, number) THEN
	 Usage
      END;
   END FetchInt;

   PROCEDURE FetchOct(   VAR number: CARDINAL);		(* EXPORTED *)
      VAR buffer: String;
   BEGIN
      FetchString(buffer);
      IF ~StrToOct(buffer, number) THEN
	 Usage
      END;
   END FetchOct;

   PROCEDURE FetchHex(   VAR number: CARDINAL);		(* EXPORTED *)
      VAR buffer: String;
   BEGIN
      FetchString(buffer);
      IF ~StrToHex(buffer, number) THEN
	 Usage
      END;
   END FetchHex;

   PROCEDURE UngetArg;					(* EXPORTED *)
   BEGIN
      IF charindex > 0 THEN
	 charindex := 0;
      ELSE
	 IF argindex > 1 THEN
	    DEC(argindex,2);
	    NextArg;
	 END;
      END;
   END UngetArg;

   PROCEDURE UngetOpt;					(* EXPORTED *)
   BEGIN
      IF charindex > 1 THEN
	 DEC(charindex);
      ELSE
	 charindex := 0;
	 UngetArg;
	 IF arglen > 0 THEN
	    charindex := arglen-1;
	 END;
      END;
   END UngetOpt;

BEGIN
   lastarg := Args.Narg()-1;
   argindex := 0;
   NextArg;
   infostring := defaultinfostring;
END Arguments.
(*
 * Local variables:
 *  compile-command: "gm2 -c -g -I../sys:. Arguments.mod"
 * End:
 *)
