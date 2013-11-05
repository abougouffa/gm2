(* Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010
                 Free Software Foundation, Inc. *)
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


FROM SYSTEM IMPORT ADDRESS, ADR, TSIZE ;
FROM libc IMPORT tm, time, time_t, memcpy, localtime ;
FROM DynamicStrings IMPORT String, Mark, ConCat, InitString, KillString, CopyOut ;
FROM FormatStrings IMPORT Sprintf3 ;

IMPORT Selective ;


(*
   TimeToString - convert time, t, to a string.
                  The string, s, should be at least 19 characters
                  long and the returned string will be

                  yyyy-mm-dd hh:mm:ss
*)

PROCEDURE TimeToString (t: Time; VAR s: ARRAY OF CHAR) ;
VAR
   q              : String ;
   y, m, d, h, sec: CARDINAL ;
BEGIN
   WITH t DO
      y := day DIV 512 + 1900 ;
      m := (day DIV 32) MOD 16 ;
      d := day MOD 32 ;
      q := Sprintf3(Mark(InitString('%04d-%02d-%02d')), y, m, d) ;
      h := minute DIV 60 ;
      m := minute MOD 60 ;
      sec := millisec DIV 1000 ;
      q := ConCat(q, Mark(Sprintf3(Mark(InitString(' %02d:%02d:%02d')), h, m, sec))) ;
      CopyOut(s, q) ;
      q := KillString(q) ;
   END
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
   l   : time_t ;
   r   : INTEGER ;
   t   : tm ;
   a   : ADDRESS ;
   tv  : Selective.Timeval ;
   s, u: CARDINAL ;
BEGIN
   tv := Selective.InitTime(0, 0) ;
   r := Selective.GetTimeOfDay(tv) ;
   l := time(NIL) ;
   IF l#-1
   THEN
      a := localtime(l) ;
      a := memcpy(ADR(t), a, TSIZE(t)) ;
      WITH curTime DO
         day := t.tm_mday+(t.tm_mon+1)*32+t.tm_year*512 ;
         minute := t.tm_min+t.tm_hour*60 ;
         Selective.GetTime(tv, s, u) ;
         (* s MOD 61, to allow for leap seconds *)
         millisec := (u DIV 1000) MOD (60 * 1000) + ((s MOD 61) * 1000)
      END
   END ;
   tv := Selective.KillTime(tv)
END GetTime ;


END TimeDate.
