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
   $Id: SysConf.mod,v 1.4 2005/11/22 15:13:21 gaius Exp $
   ----------------------------------------------------------------------------
   $Log: SysConf.mod,v $
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

   Revision 0.2  1999/01/25  09:46:04  borchert
   release updated

   Revision 0.1  1997/03/04  19:31:58  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE SysConf; (* AFB 2/97 *)

   (* configuration parameters of the installation *)

   FROM Strings IMPORT StrCpy;

   CONST
      (* === following parameters are updated automatically === *)
      libdir = "/usr/local/lib/modula";
      release = "3.0b6";
      (* === end of parameter section === *)

   PROCEDURE GetLibDir(VAR libdirBuf: ARRAY OF CHAR);
      (* GetLibDir returns the directory where the Modula-2 library
         has been installed to; it does not honour the MODLIB
	 environment variable
      *)
   BEGIN
      StrCpy(libdirBuf, libdir);
   END GetLibDir;

   PROCEDURE GetRelease(VAR releaseBuf: ARRAY OF CHAR);
      (* returns the release of the Modula-2 installation *)
   BEGIN
      StrCpy(releaseBuf, release);
   END GetRelease;

END SysConf.
