(* Copyright (C) 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE CharClass ;

(* Classification of values of the type CHAR *)

(* Returns TRUE if and only if ch is classified as a numeric character *)

PROCEDURE IsNumeric (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN (ch>='0') AND (ch<='9')
END IsNumeric ;


(* Returns TRUE if and only if ch is classified as a letter *)

PROCEDURE IsLetter (ch: CHAR): BOOLEAN;
BEGIN
   RETURN ((ch>='a') AND (ch<='z')) OR ((ch>='A') AND (ch<='Z'))
END IsLetter ;


(* Returns TRUE if and only if ch is classified as an upper case letter *)

PROCEDURE IsUpper (ch: CHAR): BOOLEAN;
BEGIN
   RETURN (ch>='A') AND (ch<='Z')
END IsUpper ;


(* Returns TRUE if and only if ch is classified as a lower case letter *)

PROCEDURE IsLower (ch: CHAR): BOOLEAN;
BEGIN
   RETURN (ch>='a') AND (ch<='z')
END IsLower ;


(* Returns TRUE if and only if ch represents a control function *)

PROCEDURE IsControl (ch: CHAR): BOOLEAN;
BEGIN
   RETURN (ch<' ')
END IsControl ;


(* Returns TRUE if and only if ch represents a space character or a format effector *)

PROCEDURE IsWhiteSpace (ch: CHAR): BOOLEAN;
BEGIN
   RETURN (ch=' ') OR (ch=cr) OR (ch=lf) OR (ch=tab)
END IsWhiteSpace ;


END CharClass.
