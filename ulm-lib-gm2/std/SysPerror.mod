(* Ulm's Modula-2 Library
   Copyright (C) 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992,
   1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
   2004, 2005
   by University of Ulm, SAI, D-89069 Ulm, Germany
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
   $Id: SysPerror.mod,v 1.4 2005/11/22 15:13:21 gaius Exp $
   ----------------------------------------------------------------------------
   $Log: SysPerror.mod,v $
   Revision 1.4  2005/11/22 15:13:21  gaius
   fixed Copyright dates

   Revision 1.3  2005/11/21 12:09:59  gaius
   updated Copyright notices and dates

   Revision 1.2  2004/06/29 08:51:42  gaius
   * made flex lexical analysers ignore carriage return
   * fixed bug in M2Quads.mod checking parameter of
     a const var before value was known.
   * fixed local MODULEs so that they can FROM mod IMPORT
   * tidied up some ulm implementation modules in ulm-lib-gm2/std

   Revision 1.1  2003/12/27 00:16:06  gaius
   added ulm libraries into the gm2 tree. Currently these
   are only used when regression testing, but later they
   will be accessible by users of gm2.

   Revision 0.3  1997/02/28  15:50:44  borchert
   header fixed

   Revision 0.2  1997/02/28  15:48:08  borchert
   implementation takes now advantage of Errno.message

   Revision 0.1  1997/02/21  19:18:38  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE SysPerror; (* AFB 2/84 *)

   FROM Errno IMPORT errno, message, maxerror;
   FROM FtdIO IMPORT FwriteString, FwriteInt, FwriteLn;
   FROM StdIO IMPORT stderr;
   FROM Strings IMPORT StrCpy;

   CONST
      unknownError = "unknown error code";

   PROCEDURE Perror(str: ARRAY OF CHAR);
   BEGIN
      FwriteString(stderr, str);
      FwriteString(stderr, ": ");
      IF (errno <= maxerror) & (message[errno][0] # 0C) THEN
	 FwriteString(stderr, message[errno]);
      ELSE
	 FwriteString(stderr, unknownError);
	 FwriteString(stderr, " (");
	 FwriteInt(stderr, errno, 1);
	 FwriteString(stderr, ")");
      END;
      FwriteLn(stderr);
   END Perror;

   PROCEDURE GetErrorString(errno: CARDINAL; VAR str: ARRAY OF CHAR);
   BEGIN
      IF (errno <= maxerror) & (message[errno][0] # 0C) THEN
	 StrCpy(str, message[errno]);
      ELSE
	 StrCpy(str, unknownError);
      END;
   END GetErrorString;

END SysPerror.
