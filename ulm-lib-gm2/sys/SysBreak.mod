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
   E-mail contact: modula@mathematik.uni-ulm.de
   ----------------------------------------------------------------------------
   $Id: SysBreak.mod,v 1.3 2005/11/21 12:09:59 gaius Exp $
   ----------------------------------------------------------------------------
   $Log: SysBreak.mod,v $
   Revision 1.3  2005/11/21 12:09:59  gaius
   updated Copyright notices and dates

   Revision 1.2  2004/04/05 10:42:46  gaius
   made gm2 64 bit clean, essentially this means a clear separation between
   int/word objects and pointer objects.

   Revision 1.1  2003/12/27 00:16:07  gaius
   added ulm libraries into the gm2 tree. Currently these
   are only used when regression testing, but later they
   will be accessible by users of gm2.

   Revision 0.2  1997/02/28  15:47:24  borchert
   header fixed

   Revision 0.1  1997/02/21  19:05:29  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE SysBreak;

   FROM SYSTEM IMPORT ADDRESS, UNIXCALL;
   FROM Sys IMPORT brk;
   FROM Errno IMPORT errno;
   IMPORT SysLocations;

   PROCEDURE Break(addr: ADDRESS) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(brk, r0, r1, addr) THEN
	 SysLocations.Break := addr;
	 RETURN TRUE;
      ELSE
	 errno := r0;
	 RETURN FALSE;
      END;
   END Break;

   PROCEDURE Sbreak(incr: CARDINAL) : ADDRESS;
      VAR oldBreak: ADDRESS;
   BEGIN
      oldBreak := SysLocations.Break;
      INC(SysLocations.Break, VAL(ADDRESS, incr));
      IF Break(SysLocations.Break) THEN
	 RETURN oldBreak;
      ELSE
         SysLocations.Break := oldBreak;
	 RETURN NIL;
      END;
   END Sbreak;

END SysBreak.
