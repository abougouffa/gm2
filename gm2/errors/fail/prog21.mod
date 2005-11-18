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
(*
Program written by sabroadh
Created on Tue Feb 24 15:02:59 GMT 1998
swd coursework 1 - file handling
*)

MODULE prog21;
(* your solution to employee problem - swd 1st c/w 1997/8 *)
IMPORT FIO;
FROM NumberIO IMPORT WriteInt,ReadInt,StrToInt,IntToStr;
FROM FpuIO IMPORT ReadReal,WriteReal,StrToReal,RealToStr;
FROM StdIO IMPORT Write;
IMPORT StrIO; (* WriteString,WriteLn,ReadString; *)
FROM StrCase IMPORT Cap;
FROM menu IMPORT popup,MenuType;
FROM Cursor IMPORT Clear;
CONST
EXITOPT = 8;
TYPE
string4  = ARRAY[0..3] OF CHAR;
string10 = ARRAY[0..9] OF CHAR;
string20 = ARRAY[0..19] OF CHAR;
string40 = ARRAY[0..39] OF CHAR;

		employeeRecordType = RECORD
Forename : string20;
Surname  : string40;
EmpNumber: string10;
Address1 : string20;
Address2 : string20;
Address3 : string20;
Address4 : string20;
END;(*record*)
VAR
menu : MenuType;
selection : CARDINAL;
filehandle : File;
crlf : ARRAY[0..1] OF CHAR;

PROCEDURE continue;
VAR
	s : string4;
BEGIN
		StrIO.WriteLn;
		StrIO.WriteString("press ENTER to continue > ");
		StrIO.ReadString(s);
END continue;
PROCEDURE blankRec(VAR blank : employeeRecordType) ;
BEGIN
WITH blank DO
	Forename := '		    ';
	Surname  := '					';
	EmpNumber:='	  ';
	Address1 :='		    ';
	Address2 :='		    ';
	Address3 :='		    ';
	Address4 :='		    ';
END; (*with*)
END blankRec;

(*This procedure is passed the data for  an employee . *)
(* It prints out a formatted copy of the data in this record.*)
PROCEDURE printEntry(which : employeeRecordType) ;
BEGIN
END printEntry; (*printEntry*)
(*this gets a new entry from the user *)
PROCEDURE getEntry(VAR newrec : employeeRecordType) ;
(*Data to be filled in by user responses.*)
BEGIN
END getEntry ;
PROCEDURE getRecord(VAR employeeRec: employeeRecordType);
BEGIN
END getRecord;
(*This procedure reads in the data from the input file It simply calls
getRecord over AND again.  If it ever fails for any reason it aborts.*)
PROCEDURE getFile() ;
VAR
	inRec : employeeRecordType;
BEGIN
	(*Reset the file to read it all in.*) filehandle
:=FIO.OpenToRead ('employee.txt');
(*Process all the lines in the file.*)
WHILE NOT EOF(filehandle ) DO
	getRecord(inRec);
	printEntry(inRec);
END ; (* WHILE NOT EOF() *)
END getFile ;
PROCEDURE CreateNewFile;
BEGIN
END CreateNewFile;
PROCEDURE RenameExistingFile;
BEGIN
END RenameExistingFile;
PROCEDURE addRecord;
BEGIN
END addRecord;
PROCEDURE DeleteRecord;
BEGIN
END DeleteRecord;
PROCEDURE ListRecords;
BEGIN
Clear;
getFile;
FIO.WriteString ('Forename');
continue;
END ListRecords;

PROCEDURE runSubMenu;
BEGIN
END runSubMenu;
PROCEDURE provideHelp;
BEGIN
END provideHelp;
(* START OF MAIN MODULE.*)
BEGIN    (*body of MODULE*)
WITH menu DO
NumberOfOptions := EXITOPT;
Option[1] := "=A9reate a new file";
OptionLetter[1] := "C";
Option[2] := "=AEename existing file";
OptionLetter[2] := "R";
Option[3] := "(A)dd a new record";
OptionLetter[3] := "A";
		Option[4] := "(D)elete record by surname";
		OptionLetter[4] := "D";
		Option[5] := "(L)ist the whole file";
		OptionLetter[5] := "L";
		Option[6] := "(S)ub menu for enhancements";
		OptionLetter[6] := "S";
		Option[7] := "(?) for help";
		OptionLetter[7] := "?";
		Option[8] := "(E)xit program";
		OptionLetter[8] := "E";
END; (*with*)
		(*menu we get a valid input by calling popup with the
parameter menu*) Clear; selection := popup(menu,10,10);
	(*Do until finished.*)
WHILE selection <>  EXITOPT DO
	(*Perform the appropriate action - only valid input gets here.*)
CASE selection OF
	1 : CreateNewFile;
	| 	2 : RenameExistingFile;
	|	3 : addRecord;
	|	4 : DeleteRecord;
	|	5 : ListRecords;
	|	6 : runSubMenu;
	|	7 : provideHelp;
	END ; (* CASE selection *)
	(*menu we get a valid input by calling popup with the parameter
menu*) selection := popup(menu,10,10);
	(*if the user types ? then they will get more detailed help.*)
	END ; (* WHILE selection <> EXITOPT *)
		(*EXIT the MODULE.*)
		StrIO.WriteString('Exiting - Thankyou for your help.');
		StrIO.WriteLn;
END prog21. (* MODULE *)
