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
(*
 * Program written by sabroadh
 * Created on Thu Oct 16 13:44:36 BST 1997
 * SDP coursework
*)

MODULE prog112;

FROM StrIO IMPORT WriteString,WriteLn,ReadString;
  FROM NumberIO IMPORT ReadInt,WriteInt;
    FROM IO IMPORT Read,Write;
VAR
N		:INTEGER;
D		:INTEGER;
R		:INTEGER;
max_iteration	:INTEGER;
max_counter	:INTEGER;
answer		:ARRAY [0..1] OF CHAR;

BEGIN

N := 0;
D := 0;
R := 0;
answer := ('y');
WriteString('INPUT MAXIMIN NUMBERS TO BE DISPLAYED : ');
ReadInt(max_iteration);

WHILE (answer[0] = 'y') OR (answer[0] = 'Y') DO
	WriteString('INPUT DEVISER  : ');
	ReadInt(D);
	WriteLn;
	WriteString('INPUT NUMBER : ');
	ReadInt(N);
	WriteLn;
	WriteInt (N,1);
	WriteString (', ');
	max_counter :=0;

	WHILE (N >= D) AND (max_counter < max_iteration) DO
	max_counter := max_counter+1;
 	
	  R := N MOD D;

		IF R = 0 THEN
			N := N DIV D
                ELSE
                        IF (R > 0) AND (R <= D-2) THEN
                                N := N * (D +1) - R
                ELSE
                                N:=N*(D+1) +1

                END; (*IF*)

                WriteInt (N,1);
        END;        (* End first While*)

        WriteLn;
        WriteString ('Do you wish to do another numbers test   [Y/N]:');

        ReadString (answer);

        END

END; prog112.