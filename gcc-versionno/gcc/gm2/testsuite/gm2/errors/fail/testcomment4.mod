(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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
(*          *)
(* Program written by lprice *)
(* Created on Thu Apr 10 10:02:44 BST 1997     *)
(* SE108 Coursework 2
(*      *)
MODULE testcomment ;

FROM StrIO IMPORT WriteLn,WriteString,ReadString;
FROM StdIO IMPORT Read,Write;
FROM NumberIO IMPORT WriteInt,WriteCard, ReadInt;
FROM StrLib IMPORT StrEqual;

TYPE
  stringten = ARRAY [0..9] OF CHAR;
  FreqName = RECORD
       Name : stringten;
       Score : INTEGER;
  END ;

DataArray = ARRAY [1..20] OF FreqName;

VAR (*global*)
   arraypersons : DataArray;
          MAX   : CARDINAL;


(*----------------------------------------------*)
(*        *)
(* Put your program description here    *)
(*        *)
(*----------------------------------------------*)


(*----------------------------------------------*)
(*        *)
(* Put your procedures here      *)
(*        *)
(*----------------------------------------------*)

PROCEDURE Initialise(VAR InitData: DataArray) ;
VAR
  Index : INTEGER;
BEGIN
   FOR Index := 1 TO 20 DO
       InitData[Index].Name := 'EMPTY';
       InitData[Index].Score := 0;
   END;
END Initialise ;

PROCEDURE ReadData(VAR Presname: DataArray);
VAR
        Person, Location, Blank : stringten;
        I, Points : CARDINAL;
        FOUND : BOOLEAN;
BEGIN
WriteString('Please enter the location ');
ReadString(Location);
WriteLn;
  WHILE NOT StrEqual(Location,'qqqqqqqqqq') DO
    ReadString(Blank);
    Points := 3;
    WriteString('Please enter the Name ');
    WriteLn;
    ReadString(Person);
    WriteLn;
      WHILE NOT StrEqual(Person, 'qqqqqqqqqq') DO
          I := 1;
          FOUND := FALSE;
          WHILE (NOT FOUND) AND (NOT StrEqual(Presname[I].Name,'EMPTY')) DO
               WriteCard(I,2);
               WriteLn;
               IF StrEqual (Presname[I].Name,Person)
                    THEN FOUND := TRUE;
               END;(*if*)
               I := I + 1;
          END;(*while*)
             IF NOT FOUND THEN
                 MAX := MAX + 1;
                 Presname [MAX].Name := Person;
                 Presname [MAX].Score:=Points;
             ELSE
              Presname[I-1].Score:=Points;
             END;(*if*)
               IF Points > 0 THEN
                  Points := Points - 1;
               END;(*if*)
          ReadString(Person);
      END;
      ReadString(Blank);
      ReadString(Location);
  END;(*while*)
END ReadData;

BEGIN
MAX:=0;
     WriteString('Sports Day Results');
     WriteLn;

     Initialise(arraypersons) ;
     ReadData(arraypersons) ;
    (*Inform the user about the program*)

(*----------------------------------------------*)
(*        *)
(* Put your program instructions here   *)
(*        *)
(*----------------------------------------------*)

    (*Now thank the user*)

END testcomment.
