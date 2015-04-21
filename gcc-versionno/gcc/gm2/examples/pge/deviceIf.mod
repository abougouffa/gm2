(* Copyright (C) 2011, 2012, 2013, 2014, 2015
   Free Software Foundation, Inc.  *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  *)

IMPLEMENTATION MODULE deviceIf ;

IMPORT MemStream ;
IMPORT ClientSocket ;
IMPORT SeqFile ;
IMPORT RawIO ;
IMPORT Fractions ;

FROM IOChan IMPORT ChanId ;
FROM ChanConsts IMPORT read, write, raw, text, OpenResults ;
FROM SYSTEM IMPORT ADDRESS, ADR, TSIZE ;
FROM libc IMPORT printf, exit, system ;
FROM Fractions IMPORT Fract, one, zero, getFract, dup ;
FROM GC IMPORT garbage, entity, rootAllocate, markEntity, initGarbage, getData ;
FROM Points IMPORT Point, unRootPoint, dupPoint, initPoint, subPoint, addPoint, markPoint ;
FROM DynamicStrings IMPORT InitString ;
FROM NetworkOrder IMPORT writeCard, writeFract, writePoint, writeShort ;


CONST
   whiteCID   =    1 ;
   blackCID   =    2 ;
   redCID     =    3 ;
   greenCID   =    4 ;
   blueCID    =    5 ;
   yellowCID  =    6 ;
   purpleCID  =    7 ;
   nextCID    =    8 ;
   MaxColours = 4096 ;
   FPS        =   30 ;

TYPE
   SetOfColours = SET OF [0..MaxColours] ;

   configDesc = POINTER TO RECORD
                              centity      : entity ;
                              inMax, outMax: Point ;
                              same         : BOOLEAN ;
                              fps          : CARDINAL ;
                           END ;

VAR
   device      : (none, groff, rpc, buffer) ;
   file        : ChanId ;
   registered  : SetOfColours ;
   nextColour  : CARDINAL ;
   nextFrame   : CARDINAL ;
   bufferStart : ADDRESS ;
   bufferLength: CARDINAL ;
   bufferUsed  : CARDINAL ;
   configHeap  : garbage ;
   config      : configDesc ;


(*
   checkOpened - 
*)

PROCEDURE checkOpened ;
BEGIN
   IF device=none
   THEN
      printf ("device must be configured before anything can be displayed\n") ;
      exit (1)
   END
END checkOpened ;


(*
   registerColour - 
*)

PROCEDURE registerColour (cid: Colour; r, g, b: Fract) : Colour ;
BEGIN
   IF cid IN registered
   THEN
      printf ("colour %d already registered\n", cid);
   ELSE
      checkOpened ;
      printf ("register colour %d\n", cid);
      INCL(registered, cid) ;
      printf ("  output rc command\n");
      RawIO.Write (file, "rc") ;
      writeShort (file, cid) ;
      writeFract (file, r) ;
      writeFract (file, g) ;
      writeFract (file, b)
   END ;
   RETURN cid
END registerColour ;


(*
   white - returns the colour, white.
*)

PROCEDURE white () : Colour ;
BEGIN
   RETURN registerColour (whiteCID, one (), one (), one ())
END white ;


(*
   black - returns the colour, black.
*)

PROCEDURE black () : Colour ;
BEGIN
   RETURN registerColour (blackCID, zero (), zero (), zero ())
END black ;


(*
   red - returns the colour, red.
*)

PROCEDURE red () : Colour ;
BEGIN
   RETURN registerColour (redCID, one (), zero (), zero ())
END red ;


(*
   green - returns the colour, green.
*)

PROCEDURE green () : Colour ;
BEGIN
   RETURN registerColour (greenCID, zero (), one (), zero ())
END green ;


(*
   blue - returns the colour, blue.
*)

PROCEDURE blue () : Colour ;
BEGIN
   RETURN registerColour (blueCID, zero (), zero (), one ())
END blue ;


(*
   yellow - returns the colour, yellow.
*)

PROCEDURE yellow () : Colour ;
BEGIN
   RETURN registerColour (yellowCID, one (), one (), zero ())
END yellow ;


(*
   purple - returns the colour, purple.
*)

PROCEDURE purple () : Colour ;
BEGIN
   RETURN registerColour (purpleCID, one (), zero (), one ())
END purple ;


(*
   writeColour - 
*)

PROCEDURE writeColour (c: Colour) ;
BEGIN
   writeShort (file, c)
END writeColour ;


(*
   glyphLine - draw a line between:  start and end of, thick, thickness and colour, c.
*)

PROCEDURE glyphLine (start, end: Point; thick: Fract; c: Colour) ;
BEGIN
   checkOpened ;
   RawIO.Write (file, "dl") ;
   writePoint (file, start) ;
   writePoint (file, end) ;
   writeFract (file, thick) ;
   writeColour (c)
END glyphLine ;


(*
   glyphPolygon - draw a polygon given n absolute points.
                  If fill then it is filled with colour, c, else it is drawn with
                  thickness in outline using colour, c.
*)

PROCEDURE glyphPolygon (n: CARDINAL; p: ARRAY OF Point; fill: BOOLEAN; thick: Fract; c: Colour) ;
VAR
   i: CARDINAL ;
BEGIN
   checkOpened ;
   IF fill
   THEN
      RawIO.Write (file, "dP")
   ELSE
      RawIO.Write (file, "dp")
   END ;
   writeShort (file, n) ;
   FOR i := 0 TO n-1 DO
      writePoint (file, p[i])
   END ;
   IF fill
   THEN
      writeColour (c)
   ELSE
      writeFract (file, thick)
   END
END glyphPolygon ;


(*
   glyphCircle - draw a circle at point, pos.  If fill then it is filled by colour, c,
                 otherwise it is drawn in outline with a thickness, thick, in colour, c.
*)

PROCEDURE glyphCircle (pos: Point; fill: BOOLEAN; thick: Fract; rad: Fract; c: Colour) ;
BEGIN
   checkOpened ;
   IF fill
   THEN
      RawIO.Write (file, "dC")
   ELSE
      RawIO.Write (file, "dc")
   END ;
   writePoint (file, pos) ;
   writeFract (file, rad) ;
   IF fill
   THEN
      writeColour (c)
   ELSE
      writeFract (file, thick)
   END
END glyphCircle ;


(*
   flipBuffer - renders the current buffer and then sets up a new buffer to be
                populated by new glyphs.
*)

PROCEDURE flipBuffer ;
BEGIN
   IF device=none
   THEN
      useBuffer
   END ;
   RawIO.Write (file, "fb")
END flipBuffer ;


(*
   frameNote - emit a note to indicate a new frame has commenced.
*)

PROCEDURE frameNote ;
BEGIN
   checkOpened ;
   RawIO.Write (file, "fn") ;
   writeCard (file, nextFrame) ;
   INC(nextFrame)
END frameNote ;


(*
   emptyBuffer - empty the frame buffer (this only applies if the module is using
                 the buffer device).
*)

PROCEDURE emptyBuffer ;
BEGIN
   IF device=buffer
   THEN
      printf ("rewrite\n");
      MemStream.Rewrite (file)
   END
END emptyBuffer ;


(*
   writeTime - writes the delay, t, to the frame buffer (if t > 0.0).
*)

PROCEDURE writeTime (t: REAL) ;
BEGIN
   IF t > 0.0
   THEN
      checkOpened ;
      RawIO.Write (file, "sl") ;
      RawIO.Write (file, t)
   END
END writeTime ;


(*
   defineColour - defines a colour by: r, g, b.  The fracts
                  are given to the colour and a colour index
                  is returned.  Colours live for ever and are
                  never freed.
*)

PROCEDURE defineColour (r, g, b: Fract) : CARDINAL ;
VAR
   col: CARDINAL ;
BEGIN
   col := nextColour ;
   INC(nextColour) ;
   RETURN registerColour (col, r, g, b)
END defineColour ;


(*
   useGroff - use the groff device to render frames into png images.
*)

PROCEDURE useGroff ;
VAR
   res: OpenResults ;
BEGIN
   device := groff ;
   SeqFile.OpenWrite (file, 'output.raw', write+raw, res) ;
   IF res#opened
   THEN
      printf ("something went wrong when trying to open the raw output file\n");
      exit (1)
   END
END useGroff ;


(*
   useRPC - use the rpc device connect to a server to render frames.
*)

PROCEDURE useRPC ;
VAR
   res: OpenResults ;
   r  : INTEGER ;
BEGIN
   device := rpc ;
   REPEAT
      ClientSocket.OpenSocket(file, 'localhost', 6000, read+write+raw+text, res) ;
      IF res#opened
      THEN
         printf("unable to connect to localhost:6000\n") ;
         r := system(ADR("sleep 1"))
      END
   UNTIL res=opened
END useRPC ;


(*
   useBuffer - place the objects into a frame buffer.
*)

PROCEDURE useBuffer ;
VAR
   res: OpenResults ;
BEGIN
   device := buffer ;
   MemStream.OpenWrite (file, write+raw, res, bufferStart, bufferLength, bufferUsed, TRUE) ;
   IF res#opened
   THEN
      printf ("deviceIf.useBuffer: something went wrong when trying to open the memstream file\n");
      exit (1)
   END
END useBuffer ;


(*
   finish - close the device file.
*)

PROCEDURE finish ;
BEGIN
   checkOpened ;
   RawIO.Write (file, "ex") ;
   RawIO.Write (file, 0C) ;
   CASE device OF

   groff  :  SeqFile.Close (file) |
   rpc    :  ClientSocket.Close (file) |
   buffer :  MemStream.Close (file)

   END
END finish ;


(*
   scaleX - 
*)

PROCEDURE scaleX (x: Fract) : Fract ;
BEGIN
   IF config^.same
   THEN
      RETURN dup(x)
   ELSE
      RETURN Fractions.div(Fractions.mult(config^.outMax.x, x), config^.inMax.x) ;
   END
END scaleX ;


(*
   scaleY - it assumes that, y, is an absolute position on the y axis.
*)

PROCEDURE scaleY (y: Fract) : Fract ;
BEGIN
   IF config^.same
   THEN
      RETURN dup(y)
   ELSE
      RETURN Fractions.div(Fractions.mult(config^.outMax.y, y), config^.inMax.y) ;
   END
END scaleY ;


(*
   getFrameBuffer - collects the frame buffer limits in the following parameters.
*)

PROCEDURE getFrameBuffer (VAR start: ADDRESS; VAR length: CARDINAL; VAR used: CARDINAL) ;
BEGIN
   start := bufferStart ;
   length := bufferLength ;
   used := bufferUsed
   ; printf ("getFrameBuffer (addr = 0x%p, length = %d, used = %d)\n", start, length, used)
END getFrameBuffer ;


(*
   configDevice - configure the output device to have outMax resolution.
*)

PROCEDURE configDevice (inMax, outMax: Point; fps: CARDINAL) ;
BEGIN
   config^.inMax := dupPoint(inMax) ;
   config^.outMax := dupPoint(outMax) ;
   config^.same := Fractions.areEqual(inMax.x, outMax.x) AND
                   Fractions.areEqual(inMax.y, outMax.y) ;
   config^.fps := fps
END configDevice ;


(*
   markConfig - 
*)

PROCEDURE markConfig (e: entity) ;
VAR
   c: configDesc ;
BEGIN
   c := getData(e) ;
   IF c=config
   THEN
      WITH config^ DO
         markEntity(centity) ;
         markPoint(inMax) ;
         markPoint(outMax)
      END
   ELSE
      HALT
   END
END markConfig ;


(*
   Init - 
*)

PROCEDURE Init ;
VAR
   e: entity ;
BEGIN
   nextColour := nextCID ;
   registered := SetOfColours{} ;
   nextFrame := 1 ;
   bufferStart := NIL ;
   bufferLength := 0 ;
   bufferUsed := 0 ;
   device := none ;

   configHeap := initGarbage(markConfig, TSIZE(config^), InitString('config')) ;
   rootAllocate(configHeap, e, config) ;
   config^.centity := e ;

   configDevice(initPoint(one(), one()), initPoint(one(), one()), FPS)
END Init ;


BEGIN
   Init
END deviceIf.
