(* Copyright (C) 2001 Free Software Foundation, Inc. *)
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
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA *)
IMPLEMENTATION MODULE IO ;


FROM StrLib IMPORT StrCopy ;
FROM SYSTEM IMPORT ADR, SIZE ;
FROM M2RTS IMPORT InstallTerminationProcedure ;
FROM libc IMPORT read, write, system, isatty ;
FROM FIO IMPORT StdIn, StdOut, StdErr, WriteChar, ReadChar ;
FROM ASCII IMPORT cr, eof, nl;


VAR
   IsInRawMode: BOOLEAN ;
   Eof        : BOOLEAN ;


PROCEDURE Read (VAR ch: CHAR) ;
BEGIN
   IF IsInRawMode
   THEN
      IF Eof OR (read(0, ADR(ch), 1)#1)
      THEN
         Eof := TRUE ;
         ch := eof
      END
   ELSE
      ch := ReadChar(StdIn)
   END
END Read ;


PROCEDURE Write (ch: CHAR) ;
VAR
   res: INTEGER ;
BEGIN
   IF IsInRawMode
   THEN
      res := write(1, ADR(ch), SIZE(ch))
   ELSE
      WriteChar(StdOut, ch)
   END
END Write ;


PROCEDURE Error (ch: CHAR) ;
BEGIN
   WriteChar(StdErr, ch)
END Error ;


PROCEDURE IOInRawMode ;
VAR
   Command: ARRAY [0..30] OF CHAR ;
   res    : INTEGER ;
BEGIN
   IF isatty()
   THEN
      IsInRawMode := TRUE ;
      StrCopy("stty raw -echo", Command) ;
      res := system(ADR(Command))
   END
END IOInRawMode ;


PROCEDURE IOInBufferedMode ;
VAR
   Command: ARRAY [0..30] OF CHAR ;
   res    : INTEGER ;
BEGIN
   IF IsInRawMode
   THEN
      StrCopy("stty sane", Command) ;
      res := system(ADR(Command))
   END
END IOInBufferedMode ;


BEGIN
   IsInRawMode := FALSE ;
   Eof := FALSE ;
   InstallTerminationProcedure(IOInBufferedMode)
END IO.
