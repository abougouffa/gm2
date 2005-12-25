(* Copyright (C) 2005 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA *)

IMPLEMENTATION MODULE TimeDate ;


FROM libc IMPORT tm, time ;


PROCEDURE TimeToString () ;
BEGIN
   
END TimeToString ;


PROCEDURE TimeToZero (VAR t: Time) ;
BEGIN
   WITH t DO
      day := 0 ;
      minute := 0 ;
      millisec := 0
   END
END TimeToZero ;


PROCEDURE CompareTime (t1, t2: Time) : INTEGER ;
BEGIN
   IF t1.day<t2.day
   THEN
      RETURN -1
   ELSIF t1.day>t2.day
   THEN
      RETURN 1
   ELSE
      IF t1.minute<t2.minute
      THEN
         RETURN -1
      ELSIF t1.minute>t2.minute
      THEN
         RETURN 1
      ELSE
         IF t1.millisec<t2.millisec
         THEN
            RETURN -1
         ELSIF t1.millisec>t2.millisec
         THEN
            RETURN 1
         ELSE
            RETURN 0
         END
      END
   END
END CompareTime ;


PROCEDURE SetTime (curTime: Time) ;
BEGIN
   (* does nothing *)
END SetTime ;


PROCEDURE GetTime (VAR curTime: Time) ;
VAR
   t   : tm ;
   a   : ADDRESS ;
   tv  : Timeval ;
   s, u: CARDINAL ;
BEGIN
   a := time(ADR(t)) ;
   tv := InitTime(0, 0) ;
   GetTimeOfDay(tv) ;
   WITH curTime DO
      day := t.tm_mday+t.tm_mon*32+t.tm_year*512 ;
      minute := t.tm_min+t.tm_hour*60 ;
      GetTime(tv, s, u) ;
      millisec := u DIV 1000
   END ;
   tv := KillTime(tv)
END GetTime ;


END TimeDate.
