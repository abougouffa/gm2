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
(* Program written by lcmorley *)
(* Created on Mon Apr 20 15:10:28 BST 1998*)
(* SE108 Coursework 2*)

MODULE prog32;

FROM StrIO IMPORT WriteString, ReadString,WriteLn;
FROM StrLib IMPORT StrEqual;
FROM IO IMPORT Read;
FROM NumberIO IMPORT WriteCard;
FROM Sort IMPORT BubbleSort;

TYPE
	FieldName = ARRAY [0..9] OF CHAR;
	
	Races = RECORD
		Name  : FieldName;
		Score : CARDINAL;
	END;
	
VAR
	Ending			:	FieldName;
	Count, Count2		:	CARDINAL;
	Venue			:	FieldName;
	Exists, Exited, Finish 	:	BOOLEAN;
	Results			:	Races;
	Position		:	CARDINAL;
	Entered_Name		:	FieldName;
	Dump			:	CHAR;
	Less_Than		:	BOOLEAN;
	Races_Array		:	ARRAY [0..50] OF Races;


PROCEDURE Sorting_Time;
BEGIN
   BubbleSort(Count - 1,Less, Swap);
END Sorting_Time;


PROCEDURE Swap(Val_1: CARDINAL; VAR Val_2 : CARDINAL);
VAR
   Temporary : ARRAY [0..1] OF Races;
BEGIN
   Temporary[0] := Races_Array[Val_1];
   Races_Array[Val_1] := Races_Array[Val_2];
   Races_Array[Val_2] := Temporary[0];
END Swap;


PROCEDURE Less(Value_1, Value_2 : CARDINAL) : BOOLEAN;
BEGIN
   Less_Than := Races_Array[Value_1].Score < 
                Races_Array[Value_2].Score;
   RETURN Less_Than;
END Less;


BEGIN
   Ending := 'qqqqqqqqqq';
   Sorting_Time
END prog32.