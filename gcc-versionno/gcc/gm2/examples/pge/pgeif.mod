(* Copyright (C) 2014 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE pgeif ;


FROM Storage IMPORT ALLOCATE ;
IMPORT deviceIf ;
IMPORT twoDsim ;
FROM deviceIf IMPORT useGroff ;
FROM SYSTEM IMPORT THROW ;
FROM Indexing IMPORT Index, InitIndex, GetIndice, PutIndice, HighIndice, IncludeIndiceIntoIndex, InBounds ;
FROM Fractions IMPORT Fract, putReal ;
FROM deviceIf IMPORT Colour ;


TYPE
   TypeOfDef = (colour, object) ;

   def = POINTER TO RECORD
                       type      : TypeOfDef ;
                       definition: CARDINAL ;
                    END ;


VAR
   listOfDefs: Index ;


(*
   init - initialise the modules data structures.
*)

PROCEDURE init ;
BEGIN
   listOfDefs := InitIndex (1) ;
   useGroff (FALSE)
END init ;


(*
   newDef - 
*)

PROCEDURE newDef (t: TypeOfDef; d: CARDINAL) : def ;
VAR
   f: def ;
BEGIN
   NEW (f) ;
   WITH f^ DO
      type := t ;
      definition := d
   END ;
   RETURN f
END newDef ;


(*
   addDef - adds a definition (type, d) into the global list and
            returns an index to the definition, id.
*)

PROCEDURE addDef (type: TypeOfDef; d: CARDINAL) : CARDINAL ;
BEGIN
   IncludeIndiceIntoIndex (listOfDefs, newDef (type, d)) ;
   RETURN HighIndice (listOfDefs)
END addDef ;


(*
   lookupDef - return the definition of, d, and check its type
               is, t.
*)

PROCEDURE lookupDef (t: TypeOfDef; d: CARDINAL) : CARDINAL ;
VAR
   f: def ;
BEGIN
   IF InBounds (listOfDefs, d)
   THEN
      f := GetIndice (listOfDefs, d) ;
      WITH f^ DO
         IF t=type
         THEN
            RETURN definition
         ELSE
            THROW (ORD (IncorrectType))
         END
      END
   ELSE
      THROW (ORD (IdOutOfBounds))
   END
END lookupDef ;


(*
   rgb - make a colour object using red, blue and green components.
         The colour object is returned.
*)

PROCEDURE rgb (r, g, b: REAL) : CARDINAL ;
VAR
   rf, gf, bf: Fract ;
BEGIN
   rf := putReal (r) ;
   gf := putReal (g) ;
   bf := putReal (b) ;
   RETURN addDef (colour, deviceIf.defineColour (rf, gf, bf))
END rgb ;



(*
   white - returns the colour, white.
*)

PROCEDURE white () : CARDINAL ;
BEGIN
   RETURN addDef (colour, deviceIf.white ())
END white ;


(*
   black - returns the colour, black.
*)

PROCEDURE black () : CARDINAL ;
BEGIN
   RETURN addDef (colour, deviceIf.black ())
END black ;


(*
   red - returns the colour, red.
*)

PROCEDURE red () : CARDINAL ;
BEGIN
   RETURN addDef (colour, deviceIf.red ())
END red ;


(*
   green - returns the colour, green.
*)

PROCEDURE green () : CARDINAL ;
BEGIN
   RETURN addDef (colour, deviceIf.green ())
END green ;


(*
   blue - returns the colour, blue.
*)

PROCEDURE blue () : CARDINAL ;
BEGIN
   RETURN addDef (colour, deviceIf.blue ())
END blue ;


(*
   yellow - returns the colour, yellow.
*)

PROCEDURE yellow () : CARDINAL ;
BEGIN
   RETURN addDef (colour, deviceIf.yellow ())
END yellow ;


(*
   purple - returns the colour, purple.
*)

PROCEDURE purple () : CARDINAL ;
BEGIN
   RETURN addDef (colour, deviceIf.purple ())
END purple ;


(*
   gravity - turn on gravity at: g m^2
*)

PROCEDURE gravity (g: REAL) ;
BEGIN
   twoDsim.gravity (g)
END gravity ;


(*
   box - place a box in the world at (x0,y0),(x0+i,y0+j)
*)

PROCEDURE box (x0, y0, i, j: REAL; c: CARDINAL) : CARDINAL ;
BEGIN
   RETURN addDef (object, twoDsim.box (x0, y0, i, j,
                                       lookupDef (colour, c)))
END box ;


(*
   poly3 - place a triangle in the world at:
           (x0,y0),(x1,y1),(x2,y2)
*)

PROCEDURE poly3 (x0, y0, x1, y1, x2, y2: REAL; c: CARDINAL) : CARDINAL ;
BEGIN
   RETURN addDef (object,
                  twoDsim.poly3 (x0, y0, x1, y1, x2, y2,
                                 lookupDef (colour, c)))
END poly3 ;


(*
   poly4 - place a rectangle in the world at:
           (x0,y0),(x1,y1),(x2,y2),(x3,y3)
*)

PROCEDURE poly4 (x0, y0, x1, y1, x2, y2, x3, y3: REAL; c: CARDINAL) : CARDINAL ;
BEGIN
   RETURN addDef (object,
                  twoDsim.poly4 (x0, y0, x1, y1, x2, y2, x3, y3,
                                 lookupDef (colour, c)))
END poly4 ;


(*
   poly5 - place a pentagon in the world at:
           (x0,y0),(x1,y1),(x2,y2),(x3,y3),(x4,y4)
*)

PROCEDURE poly5 (x0, y0, x1, y1, x2, y2, x3, y3, x4, y4: REAL; c: CARDINAL) : CARDINAL ;
BEGIN
   RETURN addDef (object,
                  twoDsim.poly5 (x0, y0, x1, y1, x2, y2, x3, y3, x4, y4,
                                 lookupDef (colour, c)))
END poly5 ;


(*
   poly6 - place a hexagon in the world at:
           (x0,y0),(x1,y1),(x2,y2),(x3,y3),(x4,y4),(x5,y5)
*)

PROCEDURE poly6 (x0, y0, x1, y1, x2, y2, x3, y3, x4, y4, x5, y5: REAL; c: CARDINAL) : CARDINAL ;
BEGIN
   RETURN addDef (object,
                  twoDsim.poly6 (x0, y0, x1, y1, x2, y2, x3, y3, x4, y4, x5, y5,
                                 lookupDef (colour, c)))
END poly6 ;


(*
   mass - specify the mass of an object and return the, id.
*)

PROCEDURE mass (id: CARDINAL; m: REAL) : CARDINAL ;
BEGIN
   RETURN twoDsim.mass (lookupDef (object, id), m)
END mass ;


(*
   fix - fix the object to the world.
*)

PROCEDURE fix (id: CARDINAL) : CARDINAL ;
BEGIN
   RETURN twoDsim.fix (lookupDef (object, id))
END fix ;


(*
   circle - adds a circle to the world.  Center
            defined by: x0, y0 radius, radius.
*)

PROCEDURE circle (x0, y0, radius: REAL; c: Colour) : CARDINAL ;
BEGIN
   RETURN addDef (object,
                  twoDsim.circle (x0, y0, radius, lookupDef (colour, c)))
END circle ;


(*
   pivot - pivot an object at position, (x0,y0).
*)

(* todo PROCEDURE pivot (x0, y0: REAL; id1: CARDINAL) : CARDINAL ; *)


(*
   velocity - give an object, id, a velocity, vx, vy.
*)

PROCEDURE velocity (id: CARDINAL; vx, vy: REAL) : CARDINAL ;
BEGIN
   RETURN twoDsim.velocity (lookupDef (object, id), vx, vy)
END velocity ;


(*
   accel - give an object, id, an acceleration, ax, ay.
*)

PROCEDURE accel (id: CARDINAL; ax, ay: REAL) : CARDINAL ;
BEGIN
   RETURN twoDsim.accel (lookupDef (object, id), ax, ay)
END accel ;


(*
   rotate - rotates object with a angular velocity, angle.
*)

PROCEDURE rotate (id: CARDINAL; angle: REAL) : CARDINAL ;
BEGIN
   RETURN twoDsim.rotate (lookupDef (object, id), angle)
END rotate ;


(*
   fps - set frames per second.
*)

PROCEDURE fps (f: REAL) ;
BEGIN
   twoDsim.fps (f)
END fps ;


(*
   getNextEvent -
*)

PROCEDURE getNextEvent () : ADDRESS ;
BEGIN
   RETURN NIL
END getNextEvent ;


BEGIN
   init
END pgeif.
