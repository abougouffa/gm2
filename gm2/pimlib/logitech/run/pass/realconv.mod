(* Copyright (C) 2005 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

MODULE realconv ;

FROM RealConversions IMPORT RealToString, StringToReal ;
FROM libc IMPORT exit, printf ;
FROM SYSTEM IMPORT ADR ;
FROM StrLib IMPORT StrEqual ;


PROCEDURE Assert (v: BOOLEAN; f: ARRAY OF CHAR; l: CARDINAL; e: ARRAY OF CHAR) ;
VAR
   r: INTEGER ;
BEGIN
   IF v
   THEN
      r := printf("successfully evaluated assertion (%s)\n", ADR(e))
   ELSE
      r := printf("%s:%d assertion failed when evaluating %s\n", ADR(f), l, ADR(e)) ;
      res := 1
   END
END Assert ;


VAR
   d  : REAL ;
   l  : LONGREAL ;
   res,
   r  : INTEGER ;
   a  : ARRAY [0..100] OF CHAR ;
   ok : BOOLEAN ;
BEGIN
   res := 0 ;
   RealToString(100.0, 10, 10, a, ok) ;
   Assert(ok, __FILE__, __LINE__, 'testing ok return BOOLEAN') ;
   r := printf('value returned is %s\n', ADR(a)) ;
   Assert(StrEqual('100.000000', a), __FILE__, __LINE__, 'testing return value of "100.000000"') ;
   RealToString(100.0, -5, 12, a, ok) ;
   Assert(ok, __FILE__, __LINE__, 'testing ok return BOOLEAN') ;
   r := printf('value returned is %s\n', ADR(a)) ;
   Assert(StrEqual('100.00000E+0', a), __FILE__, __LINE__, 'testing return value of "100.00000E+0"') ;

   RealToString(123.456789, 10, 10, a, ok) ;
   Assert(ok, __FILE__, __LINE__, 'testing ok return BOOLEAN') ;
   r := printf('value returned is %s\n', ADR(a)) ;
   Assert(StrEqual('123.456789', a), __FILE__, __LINE__, 'testing return value of "123.456789"') ;
   RealToString(123.456789, -5, 13, a, ok) ;
   Assert(ok, __FILE__, __LINE__, 'testing ok return BOOLEAN') ;
   r := printf('value returned is %s\n', ADR(a)) ;
   Assert(StrEqual('1234.56789E-1', a), __FILE__, __LINE__, 'testing return value of "1234.56789E-1"') ;

   RealToString(123.456789, -2, 15, a, ok) ;
   Assert(ok, __FILE__, __LINE__, 'testing ok return BOOLEAN') ;
   r := printf('value returned is %s\n', ADR(a)) ;
   Assert(StrEqual('  1234567.89E-4', a), __FILE__, __LINE__, 'testing return value of "  1234567.89E-4"') ;

   StringToReal('  1234567.89E-4', d, ok) ;
   Assert(ok, __FILE__, __LINE__, 'testing ok return BOOLEAN') ;
   r := printf('value returned is %f\n', d) ;

   exit(res)
END realconv.
