IMPLEMENTATION MODULE DisplayBuffer ;

FROM StdIO IMPORT Read ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM vga IMPORT vga_drawline, vga_setcolor ;


CONST
   MaxLine = 100 ;

TYPE
   Line = RECORD
             x1, y1, x2, y2: CARDINAL ;
             Colour        : CARDINAL ;
          END ;

VAR
   DisplayList  : ARRAY [0..1] OF ARRAY [1..MaxLine] OF Line ;
   Top          : ARRAY [0..1] OF CARDINAL ;
   CurrentBuffer: CARDINAL ;


(*
   FlipBuffer - flips the screen onto the other buffer.
*)

PROCEDURE FlipBuffer ;
VAR
   ch: CHAR ;
BEGIN
   EraseBuffer ;
   CurrentBuffer := 1-CurrentBuffer ;
   ShowBuffer
END FlipBuffer ;


(*
   AddLine - adds the line, x1, y1, x2, y2 into the line buffer.
*)

PROCEDURE AddLine (x1, y1, x2, y2: CARDINAL; Colour: CARDINAL) ;
VAR
   b, i: CARDINAL ;
BEGIN
   b := 1-CurrentBuffer ;
   IF Top[b]=MaxLine
   THEN
      WriteString('MaxLine - exceeded - increase MaxLine in DisplayBuffer') ;
      WriteLn ;
      HALT ;
   ELSE
      INC(Top[b]) ;
      i := Top[b] ;
      DisplayList[b][i].x1 := x1 ;
      DisplayList[b][i].y1 := y1 ;
      DisplayList[b][i].x2 := x2 ;
      DisplayList[b][i].y2 := y2 ;
      DisplayList[b][i].Colour := Colour
   END
END AddLine ;


(*
   EraseBuffer - erases the lines on the screen using the CurrentBuffer.
*)

PROCEDURE EraseBuffer ;
VAR
   i: CARDINAL ;
BEGIN
   i := Top[CurrentBuffer] ;
   WHILE i>0 DO
      WITH DisplayList[CurrentBuffer][i] DO
         vga_setcolor(0) ;
         vga_drawline(x1, y1, x2, y2)
      END ;
      DEC(i)
   END ;
   Top[CurrentBuffer] := 0
END EraseBuffer ;


(*
   ShowBuffer - shows the lines on the screen using the CurrentBuffer.
*)

PROCEDURE ShowBuffer ;
VAR
   i: CARDINAL ;
BEGIN
   i := Top[CurrentBuffer] ;
   WHILE i>0 DO
      WITH DisplayList[CurrentBuffer][i] DO
         vga_setcolor(Colour) ;
         vga_drawline(x1, y1, x2, y2)
      END ;
      DEC(i)
   END
END ShowBuffer ;


BEGIN
   CurrentBuffer := 0 ;
   Top[0] := 0 ;
   Top[1] := 0
END DisplayBuffer.
