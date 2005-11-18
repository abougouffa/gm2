(* Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc. *)
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
MODULE testmin ;


FROM SYSTEM IMPORT TSIZE, BITSET ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard, WriteInt ;

TYPE
   barfoo      = CARDINAL ;
   foobar      = barfoo ;
   ColourCodes = (black, brown, red, orange) ;

VAR
   i: INTEGER ;
   b: BOOLEAN ;
   c: CARDINAL ;
   s: BITSET ;
   t: ColourCodes ;
BEGIN
   WriteString('INTEGER  ') ; WriteCard(TSIZE(INTEGER), 8) ; WriteLn ;
   WriteString('INTEGER  ') ; WriteInt(MAX(INTEGER), 12) ; WriteInt(MIN(INTEGER), 12) ; WriteLn ;
   WriteString('CARDINAL ') ; WriteCard(MAX(CARDINAL), 12) ; WriteCard(MIN(CARDINAL), 12) ; WriteLn ;
   WriteString('BOOLEAN  ') ; WriteCard(MAX(BOOLEAN), 12) ; WriteCard(MIN(BOOLEAN), 12) ; WriteLn ;
   WriteString('BITSET   ') ; WriteCard(MAX(BITSET), 12) ; WriteCard(MIN(BITSET), 12) ; WriteLn ;
   FOR t := MIN(ColourCodes) TO MAX(ColourCodes) DO

   END
END testmin.
