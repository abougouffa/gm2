(* SysDup.mod.

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

IMPLEMENTATION MODULE SysDup;

   FROM Sys IMPORT dup;
   FROM Errno IMPORT errno, EBADF;
   FROM SYSTEM IMPORT UNIXCALL;
   FROM SysClose IMPORT Close;
   FROM SysFcntl IMPORT Fcntl, dupfd;

   PROCEDURE Dup(fd: CARDINAL; VAR newfd: CARDINAL) : BOOLEAN;
      VAR r0, r1: INTEGER;
   BEGIN
      IF UNIXCALL(dup, r0, r1, fd) THEN
         newfd := r0;
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END Dup;

   PROCEDURE Dup2(fd, newfd: CARDINAL) : BOOLEAN;
      VAR fd2: CARDINAL;
   BEGIN
      fd2 := newfd;
      IF NOT Close(fd2) THEN END;
      IF Fcntl(fd, dupfd, fd2) THEN
	 IF fd2 = newfd THEN
	    RETURN TRUE;
	 ELSE
	    errno := EBADF;
	    IF NOT Close(fd2) THEN END;
	    RETURN FALSE;
	 END;
      ELSE
	 RETURN FALSE;
      END;
   END Dup2;

END SysDup.
