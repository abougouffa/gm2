(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE M2Quiet ;

(* importing from M2Options is the reason why it is not a good idea to put this into M2Printf *)
FROM M2Options IMPORT Quiet ;
FROM M2Printf IMPORT printf0, printf1, printf2, printf3, printf4 ;


PROCEDURE qprintf0 (a: ARRAY OF CHAR) ;
BEGIN
   IF NOT Quiet
   THEN
      printf0(a)
   END
END qprintf0 ;


PROCEDURE qprintf1 (a: ARRAY OF CHAR; w: ARRAY OF BYTE) ;
BEGIN
   IF NOT Quiet
   THEN
      printf1(a, w)
   END
END qprintf1 ;


PROCEDURE qprintf2 (a: ARRAY OF CHAR; w1, w2: ARRAY OF BYTE) ;
BEGIN
   IF NOT Quiet
   THEN
      printf2(a, w1, w2)
   END
END qprintf2 ;


PROCEDURE qprintf3 (a: ARRAY OF CHAR; w1, w2, w3: ARRAY OF BYTE) ;
BEGIN
   IF NOT Quiet
   THEN
      printf3(a, w1, w2, w3)
   END
END qprintf3 ;


PROCEDURE qprintf4 (a: ARRAY OF CHAR; w1, w2, w3, w4: ARRAY OF BYTE) ;
BEGIN
   IF NOT Quiet
   THEN
      printf4(a, w1, w2, w3, w4)
   END
END qprintf4 ;


END M2Quiet.
