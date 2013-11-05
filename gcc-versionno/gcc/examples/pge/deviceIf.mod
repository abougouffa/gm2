(* Copyright (C) 2011 Free Software Foundation, Inc. *)
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
Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE deviceIf ;

IMPORT devicePy, deviceGroff ;

VAR
   groff: BOOLEAN ;


(*
   white - returns the colour, white.
*)

PROCEDURE white () : CARDINAL ;
BEGIN
   IF groff
   THEN
      RETURN deviceGroff.white()
   ELSE
      RETURN devicePy.white()
   END
END white ;


(*
   black - returns the colour, black.
*)

PROCEDURE black () : CARDINAL ;
BEGIN
   IF groff
   THEN
      RETURN deviceGroff.black()
   ELSE
      RETURN devicePy.black()
   END
END black ;


(*
   red - returns the colour, red.
*)

PROCEDURE red () : CARDINAL ;
BEGIN
   IF groff
   THEN
      RETURN deviceGroff.red()
   ELSE
      RETURN devicePy.red()
   END
END red ;


(*
   green - returns the colour, green.
*)

PROCEDURE green () : CARDINAL ;
BEGIN
   IF groff
   THEN
      RETURN deviceGroff.green()
   ELSE
      RETURN devicePy.green()
   END
END green ;


(*
   blue - returns the colour, blue.
*)

PROCEDURE blue () : CARDINAL ;
BEGIN
   IF groff
   THEN
      RETURN deviceGroff.blue()
   ELSE
      RETURN devicePy.blue()
   END
END blue ;


(*
   yellow - returns the colour, yellow.
*)

PROCEDURE yellow () : CARDINAL ;
BEGIN
   IF groff
   THEN
      RETURN deviceGroff.yellow()
   ELSE
      RETURN devicePy.yellow()
   END
END yellow ;


(*
   purple - returns the colour, purple.
*)

PROCEDURE purple () : CARDINAL ;
BEGIN
   IF groff
   THEN
      RETURN deviceGroff.purple()
   ELSE
      RETURN devicePy.purple()
   END
END purple ;


(*
   glyphLine - draw a line between:  start and end of, thick, thickness and colour, c.
*)

PROCEDURE glyphLine (start, end: Point; thick: Fract; c: Colour) ;
BEGIN
   IF groff
   THEN
      deviceGroff.glyphLine(start, end, thick, c)
   ELSE
      devicePy.glyphLine(start, end, thick, c)
   END
END glyphLine ;


(*
   glyphPolygon - draw a polygon given n absolute points.
                  If fill then it is filled with colour, c, else it is drawn with
                  thickness in outline using colour, c.
*)

PROCEDURE glyphPolygon (n: CARDINAL; p: ARRAY OF Point; fill: BOOLEAN; thick: Fract; c: Colour) ;
BEGIN
   IF groff
   THEN
      deviceGroff.glyphPolygon(n, p, fill, thick, c)
   ELSE
      devicePy.glyphPolygon(n, p, fill, thick, c)
   END
END glyphPolygon ;


(*
   glyphCircle - draw a circle at point, pos.  If fill then it is filled by colour, c,
                 otherwise it is drawn in outline with a thickness, thick, in colour, c.
*)

PROCEDURE glyphCircle (pos: Point; fill: BOOLEAN; thick: Fract; rad: Fract; c: Colour) ;
BEGIN
   IF groff
   THEN
      deviceGroff.glyphCircle(pos, fill, thick, rad, c)
   ELSE
      devicePy.glyphCircle(pos, fill, thick, rad, c)
   END
END glyphCircle ;


(*
   flipBuffer - renders the current buffer and then sets up a new buffer to be
                populated by new glyphs.
*)

PROCEDURE flipBuffer ;
BEGIN
   IF groff
   THEN
      deviceGroff.flipBuffer
   ELSE
      devicePy.flipBuffer
   END
END flipBuffer ;


(*
   defineColour - defines a colour by: r, g, b.  The fracts
                  are given to the colour and a colour index
                  is returned.  Colours live for ever and are
                  never freed.
*)

PROCEDURE defineColour (r, g, b: Fract) : CARDINAL ;
BEGIN
   IF groff
   THEN
      RETURN deviceGroff.defineColour(r, g, b)
   ELSE
      RETURN devicePy.defineColour(r, g, b)
   END
END defineColour ;


(*
   useGroff - if b is TRUE then deviceIf maps onto the groff device.
*)

PROCEDURE useGroff (b: BOOLEAN) ;
BEGIN
   deviceGroff.Init ;
   devicePy.Init ;
   groff := b
END useGroff ;


BEGIN
   groff := FALSE
END deviceIf.
