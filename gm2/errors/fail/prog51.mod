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
 * Created on Thu Nov 13 13:16:24 GMT 1997
 * sdp coursework
*)

MODULE prog51;

FROM StrIO IMPORT WriteString,WriteLn,ReadString;
  FROM NumberIO IMPORT ReadInt,WriteInt;
    FROM IO IMPORT Read,Write;

VAR
dname  :ARRAY [0..30] OF CHAR;
d_age  :INTEGER;
l_accident :INTEGER;
car_code :CHAR;
v_car  :INTEGER;
premium  :REAL;
excess  :REAL;
type  :ARRAY [0..20] OF CHAR;
rubbish  :CHAR;

PROCEDURE dinput;
 VAR
 dname  :ARRAY [0..30] OF CHAR;
 d_age  :INTEGER;
 l_accident :INTEGER;
 v_car  :INTEGER; (*value of the car*)

 BEGIN
   WriteString ('Enter name (Z to end)  ');
   ReadString (dname);
   WriteLn;
   WHILE NOT (dname [0]= 'z') OR (dname [0] = 'Z') DO 
    WriteString ('Enter age  ');
    ReadInt (d_age);
    WriteLn;
    WriteString ('Enter number of years since last accident  ');
    ReadInt (l_accident);
    WriteLn;
   WriteString ('Enter car code (U = U.K., F = foreign) ');
   Read (car_code);
   Read (rubbish);
    WriteString ('Please enter in the value of there car : ');
    ReadInt (v_car);
  END; (*while*)
END dinput;

PROCEDURE mainif;
 VAR 
 d_age  :INTEGER;
 l_accident :INTEGER;
 car_code :CHAR;

 BEGIN
   IF d_age > 25 THEN (*1*)
      IF l_accident >3 THEN (*2*)
         IF (car_code ='u') OR (car_code = 'U')THEN (*3*)
    premium := 6;
    excess  := 0;
    type  := "Compiehensie";
   ELSE
    premium := 6;
    excess  := 100;
    type    := "Compiehensie";
   END; (*3*)
  ELSE
   IF (car_code ='u')OR (car_code = 'U')THEN (*4*)
    premium := 7;
    excess  := 100;
    type    := "Compiehensie";
   ELSE
    premium := 7;
    excess  := 100;
    type := "Third party";
   END; (*4*)
  END; (*2*)
  ELSE
  IF  l_accident >3 THEN (*5*)
   IF (car_code ='u') OR (car_code = 'U')THEN (*6*)
    premium := 8;
    excess := 200;
    type := "Compiehensie";
   ELSE
    premium := 9;
    excess := 250;
    type := "Compiehensie";
   END; (*6*)
  ELSE
   IF (car_code ='u')OR (car_code = 'U')THEN (*7*)
    premium := 0;
    excess := 0;
    type  := "risk is declined";
   ELSE
    premium := 0;
    excess  := 0;
    type := "risk is declined";
   END; (*7*)
  END; (*5*)
 END; (*1*)
END mainif;


PROCEDURE premiumV;

BEGIN
 premium := (v_car/100*premium);

END premiumV;    

PROCEDURE Fioutput;
 VAR
 dname  :ARRAY [0..30] OF CHAR;
 premium  :REAL;
 excess  :REAL;
 type  :ARRAY [0..20] OF CHAR;

 BEGIN
 WriteString ('UCIC qiote');
 WriteLn;
 WriteString (dname);
 WriteLn;
 WriteString (type);
 WriteLn;
 WriteString ('Excess  ');
 WirteString (excess);
 WriteLn;
 WriteString ('Premium  ');
 WriteString (premium);
 WriteLn;

END Fioutput;

END prog51.
