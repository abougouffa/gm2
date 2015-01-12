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

IMPORT devicePy, deviceGroff, deviceBuffer ;

VAR
   device: (groff, rpc, buffer) ;


(*
   white - returns the colour, white.
*)

PROCEDURE white () : CARDINAL ;
BEGIN
   CASE device OF

   groff :  RETURN deviceGroff.white() |
   rpc   :  RETURN devicePy.white() |
   buffer:  RETURN deviceBuffer.white()

   END
END white ;


(*
   black - returns the colour, black.
*)

PROCEDURE black () : CARDINAL ;
BEGIN
   CASE device OF

   groff :  RETURN deviceGroff.black() |
   rpc   :  RETURN devicePy.black() |
   buffer:  RETURN deviceBuffer.black()

   END
END black ;


(*
   red - returns the colour, red.
*)

PROCEDURE red () : CARDINAL ;
BEGIN
   CASE device OF

   groff :  RETURN deviceGroff.red() |
   rpc   :  RETURN devicePy.red() |
   buffer:  RETURN deviceBuffer.red()

   END
END red ;


(*
   green - returns the colour, green.
*)

PROCEDURE green () : CARDINAL ;
BEGIN
   CASE device OF

   groff :  RETURN deviceGroff.green() |
   rpc   :  RETURN devicePy.green() |
   buffer:  RETURN deviceBuffer.green()

   END
END green ;


(*
   blue - returns the colour, blue.
*)

PROCEDURE blue () : CARDINAL ;
BEGIN
   CASE device OF

   groff :  RETURN deviceGroff.blue() |
   rpc   :  RETURN devicePy.blue() |
   buffer:  RETURN deviceBuffer.blue()

   END
END blue ;


(*
   yellow - returns the colour, yellow.
*)

PROCEDURE yellow () : CARDINAL ;
BEGIN
   CASE device OF

   groff :  RETURN deviceGroff.yellow() |
   rpc   :  RETURN devicePy.yellow() |
   buffer:  RETURN deviceBuffer.yellow()

   END
END yellow ;


(*
   purple - returns the colour, purple.
*)

PROCEDURE purple () : CARDINAL ;
BEGIN
   CASE device OF

   groff :  RETURN deviceGroff.purple() |
   rpc   :  RETURN devicePy.purple() |
   buffer:  RETURN deviceBuffer.purple()

   END
END purple ;


(*
   glyphLine - draw a line between:  start and end of, thick, thickness and colour, c.
*)

PROCEDURE glyphLine (start, end: Point; thick: Fract; c: Colour) ;
BEGIN
   CASE device OF

   groff :  deviceGroff.glyphLine(start, end, thick, c) |
   rpc   :  devicePy.glyphLine(start, end, thick, c) |
   buffer:  deviceBuffer.glyphLine(start, end, thick, c)

   END
END glyphLine ;


(*
   glyphPolygon - draw a polygon given n absolute points.
                  If fill then it is filled with colour, c, else it is drawn with
                  thickness in outline using colour, c.
*)

PROCEDURE glyphPolygon (n: CARDINAL; p: ARRAY OF Point; fill: BOOLEAN; thick: Fract; c: Colour) ;
BEGIN
   CASE device OF

   groff :  deviceGroff.glyphPolygon(n, p, fill, thick, c) |
   rpc   :  devicePy.glyphPolygon(n, p, fill, thick, c) |
   buffer:  deviceBuffer.glyphPolygon(n, p, fill, thick, c)

   END
END glyphPolygon ;


(*
   glyphCircle - draw a circle at point, pos.  If fill then it is filled by colour, c,
                 otherwise it is drawn in outline with a thickness, thick, in colour, c.
*)

PROCEDURE glyphCircle (pos: Point; fill: BOOLEAN; thick: Fract; rad: Fract; c: Colour) ;
BEGIN
   CASE device OF

   groff :  deviceGroff.glyphCircle(pos, fill, thick, rad, c) |
   rpc   :  devicePy.glyphCircle(pos, fill, thick, rad, c) |
   buffer:  deviceBuffer.glyphCircle(pos, fill, thick, rad, c)

   END
END glyphCircle ;


(*
   flipBuffer - renders the current buffer and then sets up a new buffer to be
                populated by new glyphs.
*)

PROCEDURE flipBuffer ;
BEGIN
   CASE device OF

   groff :  deviceGroff.flipBuffer |
   rpc   :  devicePy.flipBuffer |
   buffer:  deviceBuffer.flipBuffer

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
   CASE device OF

   groff :  RETURN deviceGroff.defineColour(r, g, b) |
   rpc   :  RETURN devicePy.defineColour(r, g, b) |
   buffer:  RETURN deviceBuffer.defineColour(r, g, b)

   END
END defineColour ;


(*
   useGroff - use the groff device to render frames into png images.
*)

PROCEDURE useGroff ;
BEGIN
   deviceGroff.Init ;
   devicePy.Init ;
   deviceBuffer.Init ;
   device := groff
END useGroff ;


(*
   useRPC - use the rpc device connect to a server to render frames.
*)

PROCEDURE useRPC ;
BEGIN
   deviceGroff.Init ;
   devicePy.Init ;
   deviceBuffer.Init ;
   device := rpc
END useRPC ;


(*
   useBuffer - place the objects into a frame buffer.
*)

PROCEDURE useBuffer ;
BEGIN
   deviceGroff.Init ;
   devicePy.Init ;
   deviceBuffer.Init ;
   device := buffer
END useBuffer ;


BEGIN
   device := groff
END deviceIf.
