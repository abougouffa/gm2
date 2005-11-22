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
   E-mail contact: modula@mathematik.uni-ulm.de
   ----------------------------------------------------------------------------
   $Id: SysGetuid.mod,v 1.3 2005/11/22 15:05:41 gaius Exp $
   ----------------------------------------------------------------------------
   $Log: SysGetuid.mod,v $
   Revision 1.3  2005/11/22 15:05:41  gaius
   fixed Copyright dates

   Revision 1.2  2005/11/21 12:09:59  gaius
   updated Copyright notices and dates

   Revision 1.1  2003/12/27 00:16:08  gaius
   added ulm libraries into the gm2 tree. Currently these
   are only used when regression testing, but later they
   will be accessible by users of gm2.

   Revision 0.2  1997/02/28  15:47:39  borchert
   header fixed

   Revision 0.1  1997/02/21  19:05:34  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE SysGetuid;

   FROM SYSTEM IMPORT UNIXCALL;
   FROM Sys IMPORT getuid, getgid;

   PROCEDURE Getuid() : CARDINAL;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(getuid, r0, r1) THEN END;
      RETURN r0
   END Getuid;

   PROCEDURE Geteuid() : CARDINAL;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(getuid, r0, r1) THEN END;	(*same as geteuid *)
      RETURN r1
   END Geteuid;

   PROCEDURE Getgid() : CARDINAL;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(getgid, r0, r1) THEN END;
      RETURN r0
   END Getgid;

   PROCEDURE Getegid() : CARDINAL;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(getgid, r0, r1) THEN END;	(*same as getegid *)
      RETURN r1
   END Getegid;

END SysGetuid.
