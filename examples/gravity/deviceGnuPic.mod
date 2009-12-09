(* Copyright (C) 2009 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE deviceGnuPic ;

FROM libc IMPORT system, printf ;
FROM SYSTEM IMPORT ADR, ADDRESS ;
FROM FormatStrings IMPORT Sprintf1, Sprintf2, Sprintf3 ;
FROM DynamicStrings IMPORT String, InitString, KillString, CopyOut, string ;
FROM SeqFile IMPORT OpenResults, OpenWrite, write, text, ChanId, Close ;
FROM TextIO IMPORT WriteString, WriteLn ;
FROM RealIO IMPORT WriteFixed ;


CONST
   Debugging = TRUE ;
   Height    = 5.0 ;
   Width     = Height ;

VAR
   frameNo : CARDINAL ;
   filename: String ;
   f       : ChanId ;


(*
   newFrame - creates a new frame.
*)

PROCEDURE newFrame ;
VAR
   name: ARRAY [0..20] OF CHAR ;
   res : OpenResults ;
BEGIN
   INC(frameNo) ;
   filename := Sprintf1(InitString('f%06d.ms'), frameNo) ;
   CopyOut(name, filename) ;
   OpenWrite(f, name, text+write, res) ;
   WriteString(f, '.defcolor red rgb 0.65f 0.1f 0.2f') ; WriteLn(f) ;
   WriteString(f, '.defcolor green rgb 0.1f 0.4f 0.2f') ; WriteLn(f) ;
   WriteString(f, '.defcolor blue rgb 0.1f 0.2f 0.6f') ; WriteLn(f)
END newFrame ;


(*
   debugSystem - 
*)

PROCEDURE debugSystem (s: String) ;
VAR
   r: INTEGER ;
BEGIN
   IF Debugging
   THEN
      printf("%s  -> ", string(s))
   END ;
   r := system(string(s)) ;
   IF r=0
   THEN
      printf("ok\n")
   ELSE
      printf("returned %d\n", r)
   END
END debugSystem ;


(*
   renderFrame - 
*)

PROCEDURE renderFrame ;
VAR
   s: String ;
BEGIN
   Close(f) ;
   s := Sprintf2(InitString('groff -ms %s > f%06d.ps'),
                 filename, frameNo) ;
   debugSystem(s) ;
   s := KillString(s) ;
   s := Sprintf2(InitString('gs -dNOPAUSE -sDEVICE=pnmraw -sOutputFile=t%06d.pnm -dGraphicsAlphaBits=4 -q -dBATCH f%06d.ps'),
                 frameNo, frameNo) ;
   debugSystem(s) ;
   s := KillString(s) ;
   s := Sprintf2(InitString('pnmcrop -quiet < t%06d.pnm | pnmtopng > e%06d.png'),
                 frameNo, frameNo) ;
   debugSystem(s) ;
   s := KillString(s) ;

   s := Sprintf2(InitString('convert e%06d.png -type truecolor f%06d.png'),
                 frameNo, frameNo) ;
   debugSystem(s) ;
   s := KillString(s) ;
   s := Sprintf3(InitString('rm t%06d.pnm f%06d.ps e%06d.png'),
                 frameNo, frameNo, frameNo) ;
   debugSystem(s) ;
   s := KillString(s) ;
   filename := KillString(filename)
END renderFrame ;


(*
   WriteColour - 
*)

PROCEDURE WriteColour (c: Colour) ;
BEGIN
   CASE c OF

   black:  WriteString(f, '\M[default]') |
   red  :  WriteString(f, '\M[red]') |
   blue :  WriteString(f, '\M[blue]') |
   green:  WriteString(f, '\M[green]')

   END
END WriteColour ;


(*
   circleFrame - 
*)

PROCEDURE circleFrame (pos: Coord; r0: REAL; c: Colour) ;
BEGIN
   WriteString(f, ".sp |") ; WriteFixed(f, (1.0-pos.y)*Height, 4, 4) ; WriteString(f, 'i') ; WriteLn(f) ;
   WriteString(f, ".nop ") ;
   WriteColour(c) ;
   WriteString(f, "\h'") ; WriteFixed(f, pos.x*Width, 4, 4) ; WriteString(f, "i'") ;
   WriteString(f, "\D'C") ; WriteFixed(f, 2.0*r0*Width, 4, 4) ; WriteString(f, "i'\M[default]") ; WriteLn(f)
END circleFrame ;


(*
   polygonFrame - draw a polygon in the current frame.
*)

PROCEDURE polygonFrame (pos: Coord; n: CARDINAL; p: ARRAY OF Coord; c: Colour) ;
VAR
   i: CARDINAL ;
   l: Coord ;
BEGIN
   WriteString(f, ".sp |") ; WriteFixed(f, (1.0-pos.y)*Height, 4, 4) ; WriteString(f, 'i') ; WriteLn(f) ;
   WriteString(f, ".nop ") ;
   WriteColour(c) ;
   WriteString(f, "\h'") ; WriteFixed(f, pos.x*Width, 4, 4) ; WriteString(f, "i'") ;
   WriteString(f, "\D'p") ;
   l := Coord{0.0, 0.0} ;
   FOR i := 0 TO n DO
      WriteFixed(f, (p[i].x-l.x)*Width, 4, 4) ; WriteString(f, "i ") ;
      WriteFixed(f, (p[i].y-l.y)*Height, 4, 4) ; WriteString(f, "i ") ;
      l := p[i]
   END ;
   WriteString(f, "'\M[default]") ; WriteLn(f)
END polygonFrame ;


(*
   produceAVI - generate an AVI file from the sequence of png images.
*)

PROCEDURE produceAVI (fps: CARDINAL) ;
VAR
   s: String ;
BEGIN
   fps := 1 ;
   s := Sprintf1(InitString('mencoder "mf://f*.png" -mf w=800:h=600:fps=%d:type=png -ovc lavc -lavcopts vcodec=mpeg4 -oac copy -o movie.avi'),
                 fps) ;
   debugSystem(s) ;
   s := KillString(s) ;
   (*
   s := InitString('rm -f *.pnm *.png f*.ms') ;
   debugSystem(s) ;
   s := KillString(s)
   *)
END produceAVI ;


BEGIN
   frameNo := 0 ;
   filename := NIL
END deviceGnuPic.
