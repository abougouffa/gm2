(* Copyright (C) 2001 Free Software Foundation, Inc. *)
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
MODULE cycles ;


FROM FpuIO IMPORT StrToLongReal, WriteLongReal ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM libc IMPORT exit ;


CONST
   DefaultClockFreq = 133.0 * 1000000.0 ;
   MaxString        = 100 ;
VAR
   ClockFreq,
   Period   : LONGREAL ;
BEGIN
   StrToLongReal('350', ClockFreq) ;
   Period := 1.0/(ClockFreq * 1000000.0) ;
   IF Period>1.0
   THEN
      WriteString('floating point code generator failed') ; WriteLn ;
      exit(1)
   ELSE
      WriteString('simple fpu code generator test passed') ; WriteLn
   END
END cycles.
