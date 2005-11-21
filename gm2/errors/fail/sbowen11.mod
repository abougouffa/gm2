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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)
MODULE sbowen11;

FROM StrIO IMPORT WriteLn,WriteString,ReadString;
FROM NumberIO IMPORT WriteInt,ReadInt;
FROM StdIO IMPORT Read, Write;
(*FROM ASCII IMPORT eof; *)
(*FROM StrCase IMPORT Cap;*)

TYPE
    FreqRecord = RECORD
     Total_Freq : INTEGER ;
     Capital_Freq : INTEGER ;
    END ; (*End of RECORD*)

    Alphabet = ['A'..'Z']  ;
    DataArray = ARRAY['A'..'Z'] OF FreqRecord ;

VAR
    InputData : DataArray ;

(*--------------------------------------------------------------------------  
*)
(*This procedure initialise the frequency to all zeroes*)
(*--------------------------------------------------------------------------  
*)
PROCEDURE Initialise(VAR InitData: DataArray) ;
VAR
    Index : Alphabet ;
BEGIN
    FOR Index := 'A' TO 'Z' DO
  InitData[Index].Total_Freq := 0;
  InitData[Index].Capital_Freq := 0;
    END ;
END Initialise ; (*procedure Initialise*)

(*--------------------------------------------------------------------------  
*)
(* This procedure reads in and sets up the counts of the array *)
(*--------------------------------------------------------------------------  
*)
PROCEDURE ReadData(VAR DataGet: DataArray) ;
VAR
    This_one : CHAR ;

 BEGIN
   WriteString('Please enter your sentence :');
   WriteLn;
   Read(This_one);
 (*  WHILE (This_one <> eof) DO*)
       IF (This_one>='A') AND (This_one<='Z') THEN
          INC(DataGet[This_one].Total_Freq);
          INC(DataGet[This_one].Capital_Freq);
 (*    ELSE
          This_one := Cap(This_one); *)
       END;

(*      IF (This_one>='A') AND (This_one<='Z') THEN
          INC(DataGet[This_one].Total_Freq);
        WriteLn;
*)
(*     END; IF*)
      (*   Read(This_one);
    *)(*  END;IF*)
  (* END;WHILE*)
END ReadData;(*END ReadData PROCEDURE*)
(*--------------------------------------------------------------------------  
*)
(* This procedure prints out the data array as a histogram.*)
(*--------------------------------------------------------------------------  
*)
PROCEDURE Histogram(HistogramData: DataArray) ;
VAR
    Index : CHAR ;
    i,Freq,Count : INTEGER ;

BEGIN
  WriteLn;
  FOR Index:='A' TO 'Z' DO
           Freq:= HistogramData[Index].Total_Freq;
           Count:= HistogramData[Index].Capital_Freq;
           Write(CHR(ORD(Index)));
           WriteString(':');
        IF (Freq <10) THEN
           WriteInt(0,0);
        END;(*IF*)
           WriteInt(Freq,1);
      FOR i := 1 TO Count DO
           Write(CHR(ORD(Index)));
      END;(*FOR*)
       FOR i := 1 TO FreqRecord DO
           Write(CHR(ORD(Index)+(ORD('a')-ORD('A'))));
       END;(*FOR*)
           WriteLn;
  END;(*FOR*)
END Histogram ;(*END Histogram PROCEDURE*)
(*--------------------------------------------------------------------------  
*)
(*This procedure writes out the used cahracters in descending frequency 
order.*)
(*--------------------------------------------------------------------------  
*)
PROCEDURE FrequencyList(FrequencyData: DataArray) ;
VAR
    Max : INTEGER;
    This_one : CHAR;
    Best : CHAR;

BEGIN
   Best:= 'A';
   Max:=1;
  WHILE (Max <> 0) DO
   Max:=0;

    FOR This_one:= 'A' TO 'Z' DO
        IF Max < FrequencyData[This_one].Total_Freq THEN
         Max:=FrequencyData[This_one].Total_Freq;
         Best:=This_one;
        END;(*IF*)
    END;(*FOR*)
   FrequencyData[Best].Total_Freq:=0;
     IF (Max <> 0) THEN
       Write(Best);
     END;(*IF*)
  END;(*WHILE*)
    WriteLn;
END FrequencyList ; (*Procedure FrequencyList.*)

(*--------------------------------------------------------------------------  
*)


BEGIN
    (* Initialise the data array by passing it as a VAR argument*)
    Initialise(InputData) ;

    (*Read in data AND set up counts*)
    ReadData(InputData) ;

    (*Print out histogram*)
    Histogram(InputData) ;

    (*Print out frequency list*)
    FrequencyList(InputData) ;
END sbowen11.
