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
   E-mail contact: modula@mathematik.uni-ulm.de
   ----------------------------------------------------------------------------
   $Id: SysAlarm.mod,v 1.1 2003/12/27 00:16:07 gaius Exp $
   ----------------------------------------------------------------------------
   $Log: SysAlarm.mod,v $
   Revision 1.1  2003/12/27 00:16:07  gaius
   added ulm libraries into the gm2 tree. Currently these
   are only used when regression testing, but later they
   will be accessible by users of gm2.

   Revision 0.2  1997/02/28  15:47:22  borchert
   header fixed

   Revision 0.1  1997/02/21  19:05:28  borchert
   Initial revision

   ----------------------------------------------------------------------------
*)

IMPLEMENTATION MODULE SysAlarm;

   FROM SYSTEM IMPORT UNIXCALL, ADR;
   FROM Sys IMPORT setitimer;	(* Nixdorf: svc call alarm *)
   FROM Errno IMPORT errno;

   PROCEDURE Alarm(sec: CARDINAL) : BOOLEAN;
      TYPE
	 TIMEVAL = RECORD
			tvsec : CARDINAL;
			tvusc : CARDINAL;
		   END(* RECORD *);
	 ITIMERVAL = RECORD
			itinterval : TIMEVAL;
			itvalue  : TIMEVAL;
		     END (* RECORD *);
      VAR r0, r1: CARDINAL;
	  value : ITIMERVAL;
	  oldvalue: ITIMERVAL;
   BEGIN
      WITH value DO
	 itinterval.tvsec := 0;
	 itinterval.tvusc := 0;
	 itvalue.tvsec := sec;
	 itvalue.tvusc := 0;
      END;
      IF UNIXCALL(setitimer, r0, r1,
		  (* ITIMER_REAL = *) 0,
		  ADR(value), ADR(oldvalue)) THEN
         previous := oldvalue.itvalue.tvsec;
         RETURN TRUE
      ELSE
         errno := r0;
         RETURN FALSE
      END;
   END Alarm;

END SysAlarm.