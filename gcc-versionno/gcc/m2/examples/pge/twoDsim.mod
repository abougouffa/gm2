(* Copyright (C) 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
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
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE twoDsim ;

FROM SYSTEM IMPORT ADR, BYTE ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM Indexing IMPORT Index, InitIndex, PutIndice, GetIndice, HighIndice ;
FROM libc IMPORT printf, exit ;
FROM deviceIf IMPORT flipBuffer, frameNote, glyphCircle, glyphPolygon, writeTime, blue, red, black, yellow, purple, white ;
FROM libm IMPORT sqrt, asin, sin, cos, atan ;
FROM roots IMPORT findQuartic, findQuadratic, findAllRootsQuartic, findOctic, nearZero, nearCoord, nearSame, setTrace ;
FROM Fractions IMPORT Fract, zero, one, putReal, initFract ;
FROM Points IMPORT Point, initPoint ;
FROM GC IMPORT collectAll ;
FROM coord IMPORT Coord, initCoord, normaliseCoord, perpendiculars, perpendicular, scaleCoord,
                  subCoord, addCoord, lengthCoord, rotateCoord, dotProd ;
FROM polar IMPORT Polar, initPolar, polarToCoord, coordToPolar, rotatePolar ;
FROM history IMPORT isDuplicate, occurred, anticipate, forgetFuture, isPair ;
FROM delay IMPORT getActualFPS ;
FROM MathLib0 IMPORT pi ;
FROM IOChan IMPORT ChanId ;
FROM ChanConsts IMPORT read, write, raw, text, OpenResults ;
FROM NetworkOrder IMPORT writeCard, writeFract, writePoint, writeShort, writeReal, writeCoord ;
FROM StrLib IMPORT StrCopy ;

IMPORT MemStream ;
IMPORT deviceIf ;
IMPORT gdbif ;


CONST
   MaxPolygonPoints       =     6 ;
   DefaultFramesPerSecond =   100.0 ;
   Debugging              = FALSE ;
   DebugTrace             = FALSE ;
   BufferedTime           =     0.1 ;
   InactiveTime           =     1.0 ;  (* the time we keep simulating after all collision events have expired *)
   Elasticity             =     0.98 ; (* how elastic are the collisions?                                     *)

TYPE
   ObjectType = (polygonOb, circleOb, springOb) ;

   eventKind = (frameKind, functionKind, collisionKind) ;

   eventType = (frameEvent, circlesEvent, circlePolygonEvent, polygonPolygonEvent, functionEvent) ;

   descP = PROCEDURE (eventDesc, CARDINAL, CARDINAL, CARDINAL, CARDINAL, whereHit, whereHit, Coord) : eventDesc ;

   cDesc = RECORD
              cPoint    : Coord ;      (* where in the 2D world does this collision happen *)
              cid1, cid2: CARDINAL ;   (* id of the two circles which will collide *)
           END ;

   cpDesc = RECORD
               cPoint  : Coord ;       (* where in the 2D world does this collision happen *)
               pid, cid: CARDINAL ;    (* id of the circle and polygon which will collide *)
               wpid    : whereHit ;    (* does the circle hit the corner or edge of pid  *)
               lineNo  : CARDINAL ;    (* if the edge is hit then this value will be 1..nPoints *)
                                       (* indicating that p[lineNo-1] p[lineNo] is the line which is hit *)
               pointNo : CARDINAL ;    (* if the corner is hit then this value will be 1..nPoints *)
                                       (* indicating that p[pointNo-1] is the corner which is hit *)
            END ;

   ppDesc = RECORD
               cPoint  : Coord ;       (* where in the 2D world does this collision happen *)
               pid1,                   (* one of pid1 corners will hit pid2.  *)
               pid2    : CARDINAL ;
               wpid1,                  (* where does the collision occur on pid1 and pid2  *)
               wpid2   : whereHit ;
               lineCorner1,
               lineCorner2: CARDINAL ; (* If the edge is hit then this value will be 1..nPoints *)
                                       (* indicating that p[lineCorner-1] p[lineCorner] is the line which is hit *)
                                       (* if the corner is hit then this value will be 1..nPoints *)
                                       (* indicating that p[lineCorner-1] is the corner which is hit *)
            END ;

   fcDesc = RECORD
               id: CARDINAL ;  (* id of the function to be called *)
            END ;


   eventDesc = POINTER TO RECORD
                              CASE etype: eventType OF

                              frameEvent         :  |
                              circlesEvent       :  cc: cDesc |
                              circlePolygonEvent :  cp: cpDesc |
                              polygonPolygonEvent:  pp: ppDesc |
			      functionEvent      :  fc: fcDesc

                              END ;
                              next: eventDesc ;
                           END ;

   Object = POINTER TO RECORD
                          id              : CARDINAL ;
                          deleted,
                          fixed,
                          stationary      : BOOLEAN ;
                          vx, vy, ax, ay  : REAL ;
                          inertia,
                          angleOrientation,
                          angularVelocity,
                          angularMomentum : REAL ;
			  interpen        : CARDINAL ;

                          CASE object: ObjectType OF

                          polygonOb :  p: Polygon |
                          circleOb  :  c: Circle |
                          springOb  :  s: Spring

                          END
                       END ;

   Spring = RECORD
               id1, id2: CARDINAL ;
	       k       : REAL ;
            END ;

   Circle = RECORD
               pos : Coord ;
               r   : REAL ;
               mass: REAL ;
               col : Colour ;
            END ;

   Polygon = RECORD
                nPoints: CARDINAL ;
                points : ARRAY [0..MaxPolygonPoints] OF Polar ;
                mass   : REAL ;
                col    : Colour ;
                cOfG   : Coord ;
             END ;

   eventProc = PROCEDURE (eventQueue) ;

   eventQueue = POINTER TO RECORD
                              kind: eventKind ;
                              time: REAL ;
                              p   : eventProc ;
                              ePtr: eventDesc ;
                              next: eventQueue ;
                           END ;


VAR
   objects            : Index ;
   maxId              : CARDINAL ;
   lastDrawTime,
   lastUpdateTime,
   currentTime,
   replayPerSecond,
   framesPerSecond    : REAL ;
   simulatedGravity   : REAL ;
   eventQ,
   freeEvents         : eventQueue ;
   freeDesc           : eventDesc ;
   trace,
   writeTimeDelay,
   drawPrediction,
   drawCollisionFrame : BOOLEAN ;
   haveCollisionColour: BOOLEAN ;
   collisionColour    : Colour ;
   bufferStart        : ADDRESS ;
   bufferLength       : CARDINAL ;
   bufferUsed         : CARDINAL ;
   fileOpened         : BOOLEAN ;
   file               : ChanId ;


(*
   Assert -
*)

PROCEDURE Assert (b: BOOLEAN; line: CARDINAL) ;
BEGIN
   IF NOT b
   THEN
      printf ("twoDsim.mod:%d:error assert failed\n", line) ;
      HALT
   END
END Assert ;


(*
   AssertR -
*)

PROCEDURE AssertR (a, b: REAL) ;
BEGIN
   IF NOT nearZero (a-b)
   THEN
      printf ("error assert failed: %g should equal %g difference is %g\n", a, b, a-b)
   END
END AssertR ;


(*
   AssertRDebug -
*)

PROCEDURE AssertRDebug (a, b: REAL; message: ARRAY OF CHAR) ;
VAR
   copy: ARRAY [0..10] OF CHAR ;
BEGIN
   IF NOT nearZero (a-b)
   THEN
      StrCopy (message, copy) ;
      printf ("%s failed  %g should equal %g difference is %g\n", ADR(copy), a, b, a-b)
   END
END AssertRDebug ;


(*
   dumpCircle -
*)

PROCEDURE dumpCircle (o: Object) ;
BEGIN
   WITH o^ DO
      printf ("circle at (%g, %g) radius %g mass %g colour %d\n", c.pos.x, c.pos.y, c.r, c.mass, c.col)
   END
END dumpCircle ;


(*
   dumpPolygon -
*)

PROCEDURE dumpPolygon (o: Object) ;
VAR
   i : CARDINAL ;
   c0: Coord ;
BEGIN
   WITH o^ DO
      printf ("polygon mass %g colour %d\n", p.mass, p.col) ;
      printf ("  c of g  (%g,%g)\n", p.cOfG.x, p.cOfG.y) ;
      FOR i := 0 TO p.nPoints-1 DO
         c0 := addCoord (p.cOfG, polarToCoord (rotatePolar (p.points[i], angleOrientation))) ;
         printf ("  point at (%g,%g)\n", c0.x, c0.y)
      END
   END
END dumpPolygon ;


(*
   checkDeleted -
*)

PROCEDURE checkDeleted (o: Object) ;
BEGIN
   IF o^.deleted
   THEN
      printf ("object %d has been deleted, should not be accessing it now\n", o^.id)
   END
END checkDeleted ;


(*
   DumpObject -
*)

PROCEDURE DumpObject (o: Object) ;
BEGIN
   WITH o^ DO
      printf ("object %d ", id) ;
      IF deleted
      THEN
         printf ("is deleted\n") ;
         RETURN
      ELSIF fixed
      THEN
         printf ("is fixed ")
      ELSE
         printf ("is movable ") ;
         IF stationary
         THEN
            printf ("but is now stationary ")
         ELSIF NOT nearZero (angularVelocity)
         THEN
            printf (" and has a rotating velocity of %g\n", angularVelocity)
         ELSIF NOT nearZero (angleOrientation)
         THEN
            printf (" and its current orientation is %g\n", angleOrientation)
         END
      END ;
      CASE object OF

      circleOb  :  dumpCircle (o) |
      polygonOb :  dumpPolygon (o) |
      springOb  :  printf ("spring\n")

      ELSE
      END ;
      IF (NOT fixed) AND (NOT stationary)
      THEN
         printf ("    velocity (%g, %g) acceleration (%g, %g)\n", vx, vy, ax, ay)
      END
   END
END DumpObject ;


(*
   c2p - returns a Point given a Coord.
*)

PROCEDURE c2p (c: Coord) : Point ;
BEGIN
   RETURN initPoint (putReal (c.x), putReal (c.y))
END c2p ;


(*
   gravity - turn on gravity at: g m^2
*)

PROCEDURE gravity (g: REAL) ;
BEGIN
   simulatedGravity := g
END gravity ;


(*
   newObject - creates an object of, type, and returns its, id.
*)

PROCEDURE newObject (type: ObjectType) : CARDINAL ;
VAR
   optr: Object ;
BEGIN
   INC(maxId) ;
   NEW(optr) ;
   WITH optr^ DO
      id               := maxId ;
      deleted          := FALSE ;
      fixed            := FALSE ;
      stationary       := FALSE ;
      object           := type ;
      vx               := 0.0 ;
      vy               := 0.0 ;
      ax               := 0.0 ;
      ay               := 0.0 ;
      angularVelocity  := 0.0 ;
      angularMomentum  := 0.0 ;
      angleOrientation := 0.0 ;
      inertia          := 0.0 ;
      interpen         := 0
   END ;
   PutIndice (objects, maxId, optr) ;
   RETURN maxId
END newObject ;


(*
   box - place a box in the world at (x0,y0), (x0+i,y0),
         (x0+i, y0+j), (x0, y0+j).
*)

PROCEDURE box (x0, y0, i, j: REAL; colour: Colour) : CARDINAL ;
BEGIN
   RETURN poly4 (x0, y0, x0+i, y0, x0+i, y0+j, x0, y0+j, colour)
END box ;


(*
   poly3 - place a triangle in the world at:
           (x0,y0), (x1,y1), (x2,y2)
*)

PROCEDURE poly3 (x0, y0, x1, y1, x2, y2: REAL; colour: Colour) : CARDINAL ;
VAR
   id, i: CARDINAL ;
   optr : Object ;
   co   : ARRAY [0..2] OF Coord ;
BEGIN
   IF Debugging
   THEN
      printf ("begin poly3 (%g, %g, %g, %g, %g, %g)\n",
              x0, y0, x1, y1, x2, y2)
   END ;
   id := newObject (polygonOb) ;
   optr := GetIndice (objects, id) ;
   co[0] := initCoord (x0, y0) ;
   co[1] := initCoord (x1, y1) ;
   co[2] := initCoord (x2, y2) ;
   WITH optr^ DO
      p.nPoints := 3 ;
      p.cOfG := calculateCofG (p.nPoints, co) ;
      FOR i := 0 TO p.nPoints-1 DO
         p.points[i] := coordToPolar (subCoord (co[i], p.cOfG))
      END ;
      p.col := colour ;
      p.mass := 0.0 ;
   END ;
   IF Debugging
   THEN
      printf ("end poly3\n");
      dumpWorld
   END ;
   RETURN id
END poly3 ;


(*
   poly4 - place a quadrangle in the world at:
           (x0,y0), (x1,y1), (x2,y2), (x3,y3)  the points must be in order,
           either anticlockwise or clockwise.
*)

PROCEDURE poly4 (x0, y0, x1, y1, x2, y2, x3, y3: REAL; colour: Colour) : CARDINAL ;
VAR
   id, i: CARDINAL ;
   optr : Object ;
   co   : ARRAY [0..3] OF Coord ;
BEGIN
   id := newObject (polygonOb) ;
   optr := GetIndice (objects, id) ;
   co[0] := initCoord (x0, y0) ;
   co[1] := initCoord (x1, y1) ;
   co[2] := initCoord (x2, y2) ;
   co[3] := initCoord (x3, y3) ;
   WITH optr^ DO
      p.nPoints := 4 ;
      p.cOfG := calculateCofG (p.nPoints, co) ;
      FOR i := 0 TO p.nPoints-1 DO
         p.points[i] := coordToPolar (subCoord(co[i], p.cOfG))
      END ;
      p.col := colour ;
      p.mass := 0.0
   END ;
   RETURN id
END poly4 ;


(*
   poly5 - place a pentagon in the world at:
           (x0,y0), (x1,y1), (x2,y2), (x3,y3), (x4,y4)
           the points must be in order, either anticlockwise or clockwise.
*)

PROCEDURE poly5 (x0, y0, x1, y1, x2, y2, x3, y3, x4, y4: REAL; colour: Colour) : CARDINAL ;
VAR
   id, i: CARDINAL ;
   optr : Object ;
   co   : ARRAY [0..4] OF Coord ;
BEGIN
   id := newObject (polygonOb) ;
   optr := GetIndice (objects, id) ;
   co[0] := initCoord (x0, y0) ;
   co[1] := initCoord (x1, y1) ;
   co[2] := initCoord (x2, y2) ;
   co[3] := initCoord (x3, y3) ;
   co[4] := initCoord (x4, y4) ;
   WITH optr^ DO
      p.nPoints := 5 ;
      p.cOfG := calculateCofG (p.nPoints, co) ;
      FOR i := 0 TO p.nPoints-1 DO
         p.points[i] := coordToPolar (subCoord(co[i], p.cOfG))
      END ;
      p.col := colour ;
      p.mass := 0.0
   END ;
   RETURN id
END poly5 ;


(*
   poly6 - place a hexagon in the world at:
           (x0,y0), (x1,y1), (x2,y2), (x3,y3), (x4,y4), (x5,y5)
           the points must be in order, either anticlockwise or clockwise.
*)

PROCEDURE poly6 (x0, y0, x1, y1, x2, y2, x3, y3, x4, y4, x5, y5: REAL; colour: Colour) : CARDINAL ;
VAR
   id, i: CARDINAL ;
   optr : Object ;
   co   : ARRAY [0..5] OF Coord ;
BEGIN
   id := newObject(polygonOb) ;
   optr := GetIndice(objects, id) ;
   co[0] := initCoord (x0, y0) ;
   co[1] := initCoord (x1, y1) ;
   co[2] := initCoord (x2, y2) ;
   co[3] := initCoord (x3, y3) ;
   co[4] := initCoord (x4, y4) ;
   co[5] := initCoord (x5, y5) ;
   WITH optr^ DO
      p.nPoints := 6 ;
      p.cOfG := calculateCofG (p.nPoints, co) ;
      FOR i := 0 TO p.nPoints-1 DO
         p.points[i] := coordToPolar (subCoord(co[i], p.cOfG))
      END ;
      p.col := colour ;
      p.mass := 0.0
   END ;
   RETURN id
END poly6 ;


(*
   mass - specify the mass of an object and return the, id.
          Only polygon (and box) and circle objects may have
          a mass.
*)

PROCEDURE mass (id: CARDINAL; m: REAL) : CARDINAL ;
VAR
   optr: Object ;
BEGIN
   optr := GetIndice(objects, id) ;
   WITH optr^ DO
      CASE object OF

      polygonOb :  p.mass := m |
      circleOb  :  c.mass := m

      ELSE
      END ;
      calcInertia (optr)
   END ;
   RETURN id
END mass ;


(*
   fix - fix the object to the world.
*)

PROCEDURE fix (id: CARDINAL) : CARDINAL ;
VAR
   optr: Object ;
BEGIN
   optr := GetIndice (objects, id) ;
   WITH optr^ DO
      fixed := TRUE
   END ;
   RETURN id
END fix ;


(*
   circle - adds a circle to the world.  Center
            defined by: x0, y0 radius, r.
*)

PROCEDURE circle (x0, y0, radius: REAL; colour: Colour) : CARDINAL ;
VAR
   id  : CARDINAL ;
   optr: Object ;
BEGIN
   id := newObject (circleOb) ;
   optr := GetIndice (objects, id) ;
   WITH optr^ DO
      c.pos.x := x0 ;
      c.pos.y := y0 ;
      c.r     := radius ;
      c.mass  := 0.0 ;
      c.col   := colour
   END ;
   RETURN id
END circle ;


(*
   get_xpos - returns the x coordinate of the center of gravity of object, id.
*)

PROCEDURE get_xpos (id: CARDINAL) : REAL ;
VAR
   dt    : REAL ;
   optr  : Object ;
BEGIN
   updatePhysics ;
   optr := GetIndice (objects, id) ;
   checkDeleted (optr) ;
   WITH optr^ DO
      CASE object OF

      polygonOb:  RETURN p.cOfG.x |
      circleOb :  RETURN c.pos.x

      ELSE
         printf ("get_xpos: only expecting polygon\n");
         HALT
      END
   END
END get_xpos ;


(*
   get_ypos - returns the first point, y, coordinate of object.
*)

PROCEDURE get_ypos (id: CARDINAL) : REAL ;
VAR
   dt    : REAL ;
   optr  : Object ;
BEGIN
   updatePhysics ;
   optr := GetIndice (objects, id) ;
   checkDeleted (optr) ;
   WITH optr^ DO
      CASE object OF

      polygonOb:  RETURN p.cOfG.y |
      circleOb :  RETURN c.pos.y

      ELSE
         printf ("get_ypos: only expecting polygon\n");
         HALT
      END
   END ;
END get_ypos ;


(*
   get_xvel - returns the x velocity of object.
*)

PROCEDURE get_xvel (id: CARDINAL) : REAL ;
VAR
   optr: Object ;
BEGIN
   IF trace
   THEN
      printf ("get_xvel for object %d\n", id)
   END ;
   down ;
   optr := GetIndice (objects, id) ;
   checkDeleted (optr) ;
   checkStationary (optr) ;
   up ;
   RETURN optr^.vx
END get_xvel ;


(*
   get_yvel - returns the y velocity of object.
*)

PROCEDURE get_yvel (id: CARDINAL) : REAL ;
VAR
   optr: Object ;
BEGIN
   IF trace
   THEN
      printf ("get_yvel for object %d\n", id)
   END ;
   down ;
   updatePhysics ;
   optr := GetIndice (objects, id) ;
   checkDeleted (optr) ;
   checkStationary (optr) ;
   up ;
   RETURN optr^.vy
END get_yvel ;


(*
   get_xaccel - returns the x acceleration of object.
*)

PROCEDURE get_xaccel (id: CARDINAL) : REAL ;
VAR
   optr: Object ;
BEGIN
   updatePhysics ;
   optr := GetIndice (objects, id) ;
   checkDeleted (optr) ;
   RETURN optr^.ax
END get_xaccel ;


(*
   get_yaccel - returns the y acceleration of object.
*)

PROCEDURE get_yaccel (id: CARDINAL) : REAL ;
VAR
   optr: Object ;
BEGIN
   updatePhysics ;
   optr := GetIndice (objects, id) ;
   checkDeleted (optr) ;
   RETURN optr^.ay
END get_yaccel ;


(*
   put_xvel - assigns the x velocity of object.
*)

PROCEDURE put_xvel (id: CARDINAL; r: REAL) ;
VAR
   optr: Object ;
BEGIN
   down ;
   updatePhysics ;
   optr := GetIndice (objects, id) ;
   checkDeleted (optr) ;
   optr^.vx := r ;
   checkStationary (optr) ;
   up
END put_xvel ;


(*
   put_yvel - assigns the y velocity of object.
*)

PROCEDURE put_yvel (id: CARDINAL; r: REAL) ;
VAR
   optr: Object ;
BEGIN
   down ;
   updatePhysics ;
   optr := GetIndice (objects, id) ;
   checkDeleted (optr) ;
   optr^.vy := r ;
   checkStationary (optr) ;
   up
END put_yvel ;


(*
   put_xaccel - assigns the x acceleration of object.
*)

PROCEDURE put_xaccel (id: CARDINAL; r: REAL) ;
VAR
   optr: Object ;
BEGIN
   down ;
   optr := GetIndice (objects, id) ;
   checkDeleted (optr) ;
   optr^.ax := r ;
   up
END put_xaccel ;


(*
   put_yaccel - assigns the y acceleration of object.
*)

PROCEDURE put_yaccel (id: CARDINAL; r: REAL) ;
VAR
   optr: Object ;
BEGIN
   down ;
   optr := GetIndice (objects, id) ;
   checkDeleted (optr) ;
   optr^.ay := r ;
   up
END put_yaccel ;


(*
   moving_towards - returns TRUE if object, id, is moving towards
                    a point x, y.
*)

PROCEDURE moving_towards (id: CARDINAL; x, y: REAL) : BOOLEAN ;
VAR
   optr: Object ;
   res : BOOLEAN ;
BEGIN
   down ;
   optr := GetIndice (objects, id) ;
   WITH optr^ DO
      IF NOT fixed
      THEN
         CASE object OF

         circleOb:  res := circle_moving_towards (optr, x, y)

         ELSE
            res := FALSE
         END
      END
   END ;
   up ;
   RETURN res
END moving_towards ;


(*
   circle_moving_towards - return TRUE if object, optr, is moving towards
                           point, x, y.  The object may still miss point x, y
                           but it is moving closer to this position.
*)

PROCEDURE circle_moving_towards (optr: Object; x, y: REAL) : BOOLEAN ;
VAR
   contactVel      : REAL ;
   relativePosition,
   relativeVelocity: Coord ;
BEGIN
   relativePosition := initCoord (optr^.c.pos.x - x, optr^.c.pos.y - y) ;
   relativeVelocity := initCoord (optr^.vx, optr^.vy) ;

   contactVel := dotProd (relativeVelocity, relativePosition) ;

   RETURN contactVel < 0.0
END circle_moving_towards ;


(*
   apply_impulse - apply an impulse to object, id,
                   along the vector [x, y] with magnitude, m.
                   Nothing happens if the object is fixed.
                   Currently only circles can have impulses
                   applied.
*)

PROCEDURE apply_impulse (id: CARDINAL; x, y: REAL; m: REAL) ;
VAR
   optr: Object ;
BEGIN
   down ;
   optr := GetIndice (objects, id) ;
   WITH optr^ DO
      IF NOT fixed
      THEN
         CASE object OF

         circleOb:  apply_impulse_to_circle (optr, x, y, m)

         ELSE
         END
      END
   END ;
   up
END apply_impulse ;


(*
   apply_impulse_to_circle - apply an impulse to moving circle, movable,
                             along the vector [x, y] with magnitude, m.
*)

PROCEDURE apply_impulse_to_circle (movable: Object; x, y: REAL; m: REAL) ;
VAR
   contactVel,
   theta,  r, j    : REAL ;
   c,
   impulsePos,
   unitCollision,
   relativeVelocity: Coord ;
BEGIN
   (* calculate normal collision value *)
   (* gdbif.sleepSpin ; *)
   impulsePos.x := movable^.c.pos.x ;
   impulsePos.y := movable^.c.pos.y ;

   IF nearZero (x)
   THEN
      IF y > 0.0
      THEN
         impulsePos.y := impulsePos.y + movable^.c.r
      ELSE
         impulsePos.y := impulsePos.y - movable^.c.r
      END
   ELSIF nearZero (y)
   THEN
      IF x > 0.0
      THEN
         impulsePos.x := impulsePos.x + movable^.c.r
      ELSE
         impulsePos.x := impulsePos.x - movable^.c.r
      END
   ELSE
      IF (x > 0.0) AND (y > 0.0)
      THEN
         theta := atan (y/x) ;
         impulsePos.x := impulsePos.x + cos (theta) * movable^.c.r ;
         impulsePos.y := impulsePos.y + sin (theta) * movable^.c.r
      ELSIF (x < 0.0) AND (y < 0.0)
      THEN
         x := -x ;
	 y := -y ;
         theta := atan (y/x) ;
         impulsePos.x := impulsePos.x - cos (theta) * movable^.c.r ;
         impulsePos.y := impulsePos.y - sin (theta) * movable^.c.r
      ELSIF (x > 0.0) AND (y < 0.0)
      THEN
         y := -y ;
         theta := atan (y/x) ;
         impulsePos.x := impulsePos.x + cos (theta) * movable^.c.r ;
         impulsePos.y := impulsePos.y - sin (theta) * movable^.c.r
      ELSE
         x := -x ;
         theta := atan (y/x) ;
         impulsePos.x := impulsePos.x - cos (theta) * movable^.c.r ;
         impulsePos.y := impulsePos.y + sin (theta) * movable^.c.r
      END
   END ;

   c := initCoord (movable^.c.pos.x - impulsePos.x, movable^.c.pos.y - impulsePos.y) ;

(*
   frameNote ;
   drawFrame (NIL) ;
   debugCircle (impulsePos, 0.02, white ()) ;
*)

   r := sqrt (c.x*c.x+c.y*c.y) ;

   unitCollision := initCoord (c.x/r, c.y/r) ;
   relativeVelocity := initCoord (movable^.vx, movable^.vy) ;

(*
   debugLine (impulsePos, addCoord (impulsePos, c), yellow ()) ;
   flipBuffer ;
*)

   contactVel := dotProd (relativeVelocity, c) ;

   IF contactVel < 0.0
   THEN
      (* moving towards.  *)
      j := (-(1.0+1.0) *
         ((relativeVelocity.x * unitCollision.x) +
          (relativeVelocity.y * unitCollision.y)))/
           (((unitCollision.x*unitCollision.x) +
             (unitCollision.y*unitCollision.y)) *
            (1.0/movable^.c.mass)) ;

      movable^.vx := movable^.vx + (j * unitCollision.x) / movable^.c.mass ;
      movable^.vy := movable^.vy + (j * unitCollision.y) / movable^.c.mass
   END ;

   checkStationary (movable)
END apply_impulse_to_circle ;


(*
   objectExists -
*)

PROCEDURE objectExists (o: Object) : BOOLEAN ;
BEGIN
   RETURN (o # NIL) AND (NOT o^.deleted)
END objectExists ;


(*
   doCheckInterpenCircleCircle -
*)

PROCEDURE doCheckInterpenCircleCircle (fixed, movable: Object) : CARDINAL ;
VAR
   d, v: Coord ;
   h1,
   h0  : REAL ;
BEGIN
   d := subCoord (movable^.c.pos, fixed^.c.pos) ;
   h0 := fixed^.c.r + movable^.c.r ;
   h1 := sqrt (d.x*d.x + d.y*d.y) ;
   IF h0 > h1
   THEN
      IF trace
      THEN
         printf ("interpen found two moving circles interpenetrating %d, %d   h0 = %g, h1 = %g\n", fixed^.id, movable^.id, h0, h1)
      END ;
      (* adjust movable circle.  *)
      v := scaleCoord (normaliseCoord (d), h0) ;
      movable^.c.pos := addCoord (fixed^.c.pos, v) ;
      (* checkStationary (movable) ; *)
      movable^.vx := movable^.vx + v.x * (h0-h1)/h0 ;
      movable^.vy := movable^.vy + v.y * (h0-h1)/h0 ;  (* give it a little push as well.  *)

      INC (movable^.interpen) ;
      RETURN movable^.interpen
   END ;
   RETURN 0
END doCheckInterpenCircleCircle ;


(*
   doCheckInterpenCircleCircleMoving -
*)

PROCEDURE doCheckInterpenCircleCircleMoving (c1, c2: Object) : CARDINAL ;
VAR
   d, v: Coord ;
   h1,
   h0  : REAL ;
BEGIN
   d := subCoord (c2^.c.pos, c1^.c.pos) ;
   h0 := c1^.c.r + c2^.c.r ;
   h1 := sqrt (d.x*d.x + d.y*d.y) ;
   IF h0 > h1
   THEN
      IF trace
      THEN
         printf ("interpen found two moving circles interpenetrating %d, %d   h0 = %g, h1 = %g\n", c1^.id, c2^.id, h0, h1)
      END ;
      (* we should really adjust the circle with the lowest interpen value.  *)
      v := scaleCoord (normaliseCoord (d), h0) ;
      c2^.c.pos := addCoord (c1^.c.pos, v) ;
      c2^.vx := c2^.vx + v.x * (h0-h1)/h0 ;
      c2^.vy := c2^.vy + v.y * (h0-h1)/h0 ;  (* give it a little push as well.  *)

      (* checkStationary (c2) ; *)
      (* checkStationary (c1) ; *)
      INC (c2^.interpen) ;
      RETURN c2^.interpen
   END ;
   RETURN 0
END doCheckInterpenCircleCircleMoving ;


(*
   distanceLinePoint - c is a point.  p1->p2 is the line in question.
                       p3 is assigned to the closest point on the line
                       to the point, c.  d is the distance from c to p3.
                       TRUE is returned if the point, c, lies above or
                       below the line once the line is rotated onto the x
                       axis.  (The point, c, would also rotated to solve
                       this question).
*)

PROCEDURE distanceLinePoint (c, p1, p2: Coord; VAR p3: Coord; VAR d: REAL) : BOOLEAN ;
VAR
   A, B, C, D,
   dot, lengthSq,
   normalised   : REAL ;
BEGIN
   A := c.x - p1.x ;
   B := c.y - p1.y ;
   C := p2.x - p1.x ;
   D := p2.y - p1.y ;
   dot := A * C + B * D ;
   lengthSq := sqr (C) + sqr (D) ;
   normalised := -1.0 ;
   IF NOT nearZero (lengthSq)
   THEN
      (* the dot product divided by length squared
         gives you the projection distance from p1.
         This is the fraction of the line that the point c
         is the closest.  *)
      normalised := dot / lengthSq
   END ;
   IF normalised < 0.0
   THEN
      (* misses line.  *)
      p3 := p1 ;
      RETURN FALSE
   ELSIF normalised > 1.0
   THEN
      (* misses line.  *)
      p3 := p2 ;
      RETURN FALSE
   END ;
   p3 := checkZeroCoord (initCoord (p1.x + normalised * C,
                                    p1.y + normalised * D)) ;

   d := lengthCoord (subCoord (c, p3)) ;
   RETURN TRUE
END distanceLinePoint ;


(*
   checkLimits -
*)

PROCEDURE checkLimits (c: Coord; r: REAL) : Coord ;
BEGIN
   IF c.x-r < 0.0
   THEN
      c.x := r
   ELSIF c.x+r > 1.0
   THEN
      c.x := 1.0-r
   END ;
   IF c.y-r < 0.0
   THEN
      c.y := r
   ELSIF c.y+r > 1.0
   THEN
      c.y := 1.0-r
   END ;
   RETURN c
END checkLimits ;


(*
   doCheckInterpenCirclePolygon -
*)

PROCEDURE doCheckInterpenCirclePolygon (iptr, jptr: Object) : CARDINAL ;
VAR
   d, r  : REAL ;
   i, n  : CARDINAL ;
   v,
   c, p1,
   p2, p3: Coord ;
BEGIN
   Assert (iptr^.object = circleOb, __LINE__) ;
   Assert (jptr^.object = polygonOb, __LINE__) ;
   c := checkZeroCoord (iptr^.c.pos) ;
   r := iptr^.c.r ;
   n := jptr^.p.nPoints ;
   i := 1 ;
   WHILE i <= n DO
      getPolygonLine (i, jptr, p1, p2) ;
      IF distanceLinePoint (c, p1, p2, p3, d) AND (NOT nearZero (r-d)) AND (r > d)
      THEN
         (* circle collides with line and point, p3, is the closest
            point on line, p1->p2 to, c.  *)
         IF NOT iptr^.fixed
         THEN
            (* circle is not fixed, move it.  *)
	    IF nearZero (d)
            THEN
               (*
               printf ("circle old position %g, %g\n",
                       iptr^.c.pos.x, iptr^.c.pos.y) ;
               printf ("line p1 = %g, %g -> %g, %g and point %g, %g nearest point %g, %g\n",
                       p1.x, p1.y, p2.x, p2.y, c.x, c.y, p3.x, p3.y) ;
               printf ("seen collision between circle and line, adjusting %d using polygon cofg\n",
                       iptr^.id) ;
               *)
	       (* as d is zero we move the circle a radius distance away from the line
                  along the projection of the polygon's cofg and p3  *)
	       (*
               v := subCoord (jptr^.p.cOfG, p3) ;
	       d := lengthCoord (v) ;
               v := scaleCoord (v, (r+d)/d) ;
               iptr^.c.pos := addCoord (jptr^.p.cOfG, v) ;
               *)
               printf ("distance is nearzero, seen collision between circle and line, new position %g, %g\n",
                       iptr^.c.pos.x, iptr^.c.pos.y)
            ELSE
               (*
               printf ("line p1 = %g, %g -> %g, %g and point %g, %g nearest point %g, %g\n",
                       p1.x, p1.y, p2.x, p2.y, c.x, c.y, p3.x, p3.y) ;
               printf ("seen collision between circle and line, adjusting %d\n",
                       iptr^.id) ;
               printf ("radius = %g, distance = %g\n", r, d) ;
               printf ("seen collision between circle and line, old position %g, %g\n",
                       iptr^.c.pos.x, iptr^.c.pos.y) ;
                *)
               v := subCoord (c, p3) ;
               v := checkZeroCoord (scaleCoord (v, r/d)) ;
	       (*
               printf ("v = %g, %g   p3 = %g, %g\n", v.x, v.y, p3.x, p3.y) ;
               *)
               iptr^.c.pos := checkZeroCoord (addCoord (p3, v)) ;
               checkStationary (iptr) ;
	       (*
               IF iptr^.stationary
               THEN
                  printf ("seen collision between circle and line, new position %g, %g  (now stationary)\n",
                           iptr^.c.pos.x, iptr^.c.pos.y)
               ELSE
                  (*
                  iptr^.vx := iptr^.vx - v.x ;
                  iptr^.vy := iptr^.vy - v.y ;  (* give it a little push as well.  *)
		  *)
                  printf ("seen collision between circle and line, new position %g, %g, velocity %g, %g  (pushing it by: %g, %g)\n",
                           iptr^.c.pos.x, iptr^.c.pos.y, iptr^.vx, iptr^.vy, v.x, v.y)
               END
	       *)
            END ;
            iptr^.c.pos := checkLimits (iptr^.c.pos, r) ;
            INC (iptr^.interpen) ;
            RETURN iptr^.interpen
         END
      END ;
      INC (i)
   END ;
   iptr^.c.pos := checkLimits (iptr^.c.pos, r) ;
   RETURN 0
END doCheckInterpenCirclePolygon ;


(*
   doCheckInterpenPolygon -
*)

PROCEDURE doCheckInterpenPolygon (iptr, jptr: Object) : CARDINAL ;
BEGIN
   IF (iptr^.object = circleOb) AND (jptr^.object = polygonOb)
   THEN
      RETURN doCheckInterpenCirclePolygon (iptr, jptr)
   ELSIF (iptr^.object = polygonOb) AND (jptr^.object = circleOb)
   THEN
      RETURN doCheckInterpenCirclePolygon (jptr, iptr)
   END ;
   RETURN 0
END doCheckInterpenPolygon ;


(*
   doCheckInterpenCircle -
*)

PROCEDURE doCheckInterpenCircle (iptr, jptr: Object) : CARDINAL ;
BEGIN
   IF (iptr^.object = circleOb) AND (jptr^.object = circleOb)
   THEN
      IF iptr^.fixed AND (NOT jptr^.fixed)
      THEN
         RETURN doCheckInterpenCircleCircle (iptr, jptr)
      ELSIF (NOT iptr^.fixed) AND jptr^.fixed
      THEN
         RETURN doCheckInterpenCircleCircle (jptr, iptr)
      ELSE
         RETURN doCheckInterpenCircleCircleMoving (iptr, jptr)
      END
   END ;
   RETURN 0
END doCheckInterpenCircle ;


(*
   initInterpen -
*)

PROCEDURE initInterpen ;
VAR
   n, i: CARDINAL ;
   iptr: Object ;
BEGIN
   n := HighIndice (objects) ;
   i := 1 ;
   WHILE i<=n DO
      iptr := GetIndice (objects, i) ;
      IF objectExists (iptr)
      THEN
         iptr^.interpen := 0
      END ;
      INC (i)
   END
END initInterpen ;


(*
   max - return the maximum of a and b.
*)

PROCEDURE max (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a > b
   THEN
      RETURN a
   ELSE
      RETURN b
   END
END max ;


(*
   checkInterpenCircle -
*)

PROCEDURE checkInterpenCircle ;
VAR
   n, i, j, c: CARDINAL ;
   iptr, jptr: Object ;
BEGIN
   initInterpen ;
   n := HighIndice (objects) ;
   REPEAT
      c := 0 ;
      i := 1 ;
      WHILE i<=n DO
         iptr := GetIndice (objects, i) ;
         IF objectExists (iptr)
         THEN
            j := i+1 ;
            WHILE j<=n DO
               jptr := GetIndice (objects, j) ;
               IF objectExists (jptr)
               THEN
                  c := max (doCheckInterpenCircle (iptr, jptr), c)
               END ;
               INC (j)
            END
         END ;
         INC (i)
      END ;
      (* keep going until no interpentration was found or there is a cycle found.  *)
   UNTIL (c>=n) OR (c=0)
END checkInterpenCircle ;


(*
   checkInterpenPolygon -
*)

PROCEDURE checkInterpenPolygon ;
VAR
   n, i, j, c: CARDINAL ;
   iptr, jptr: Object ;
BEGIN
   initInterpen ;
   n := HighIndice (objects) ;
   REPEAT
      c := 0 ;
      i := 1 ;
      WHILE i<=n DO
         iptr := GetIndice (objects, i) ;
         IF objectExists (iptr)
         THEN
            j := i+1 ;
            WHILE j<=n DO
               jptr := GetIndice (objects, j) ;
               IF objectExists (jptr)
               THEN
                  c := max (doCheckInterpenPolygon (iptr, jptr), c)
               END ;
               INC (j)
            END
         END ;
         INC (i)
      END ;
      (* keep going until no interpentration was found or there is a cycle found.  *)
   UNTIL (c>=n) OR (c=0)
END checkInterpenPolygon ;


(*
   checkInterpen -
*)

PROCEDURE checkInterpen ;
BEGIN
   (* firstly we move circles away from polygons.  *)
   checkInterpenPolygon ;
   (* then we move circles away from circles.  *)
   checkInterpenCircle
END checkInterpen ;


(*
   resetStationary -
*)

PROCEDURE resetStationary ;
VAR
   n, i: CARDINAL ;
   iptr: Object ;
BEGIN
   n := HighIndice (objects) ;
   i := 1 ;
   WHILE i<=n DO
      iptr := GetIndice (objects, i) ;
      IF objectExists (iptr)
      THEN
         IF iptr^.stationary
         THEN
            (* unset stationary, but ensure velocity is zero.  *)
            iptr^.vx := 0.0 ;
            iptr^.vy := 0.0 ;
            iptr^.stationary := FALSE
         END
      END ;
      INC (i)
   END
END resetStationary ;


(*
   set_colour - set the colour of object, id, to colour.
                id must be a box or circle.
*)

PROCEDURE set_colour (id: CARDINAL; colour: Colour) ;
VAR
   optr: Object ;
BEGIN
   optr := GetIndice (objects, id) ;
   WITH optr^ DO
      CASE object OF

      polygonOb:  p.col := colour |
      circleOb :  c.col := colour

      ELSE
         printf ("cannot set the colour of this object\n")
      END
   END
END set_colour ;


(*
   spring - join object, id1, and, id2, with a string of defined
            by hooks constant, k.
*)

PROCEDURE spring (id1, id2: CARDINAL; k: REAL) : CARDINAL ;
VAR
   id  : CARDINAL ;
   optr: Object ;
BEGIN
   id := newObject (springOb) ;
   optr := GetIndice (objects, id) ;
   WITH optr^ DO
      s.k := k ;
      s.id1 := id1 ;
      s.id2 := id2
   END ;
   RETURN id
END spring ;


(*
   velocity - give an object, id, a velocity, vx, vy.
*)

PROCEDURE velocity (id: CARDINAL; vx, vy: REAL) : CARDINAL ;
VAR
   optr: Object ;
BEGIN
   optr := GetIndice (objects, id) ;
   IF optr^.fixed
   THEN
      printf("object %d is fixed and therefore cannot be given a velocity\n",
             id)
   ELSE
      optr^.vx := vx ;
      optr^.vy := vy
   END ;
   RETURN id
END velocity ;


(*
   accel - give an object, id, an acceleration, ax, ay.
*)

PROCEDURE accel (id: CARDINAL; ax, ay: REAL) : CARDINAL ;
VAR
   optr: Object ;
BEGIN
   optr := GetIndice (objects, id) ;
   IF optr^.fixed
   THEN
      printf("object %d is fixed and therefore cannot be given an acceleration\n",
             id)
   ELSE
      optr^.ax := ax ;
      optr^.ay := ay
   END ;
   RETURN id
END accel ;


(*
   calculateCofG -
*)

PROCEDURE calculateCofG (n: CARDINAL; p: ARRAY OF Coord) : Coord ;
VAR
   A, B,
   C, D,
   a, x, y: REAL ;
   i, j   : CARDINAL ;
BEGIN
   a := calcArea (n, p) * 6.0 ;
   x := 0.0 ;
   y := 0.0 ;
   IF Debugging
   THEN
      printf ("calculateCofG begin:  %d points\n", n);
      FOR i := 0 TO n-1 DO
         printf ("%d:   %g, %g\n", i, p[i].x, p[i].y)
      END
   END ;
   FOR i := 0 TO n-1 DO
      j := (i+1) MOD n ;
      IF Debugging
      THEN
         printf ("x = %g,  y = %g\n", x, y);
         A := (p[i].x + p[j].x) ;
         B := (p[i].x * p[j].y - p[j].x * p[i].y) ;
         C := (p[i].y + p[j].y) ;
         D := (p[i].x * p[j].y - p[j].x * p[i].y) ;
         printf ("A = %g,  B = %g\n", A, B);
         printf ("C = %g,  D = %g\n", A, B);
         printf ("A * B = %g,  C * D = %g\n", A*B, C*D)
      END ;
      x := x + (p[i].x + p[j].x) * (p[i].x * p[j].y - p[j].x * p[i].y) ;
      y := y + (p[i].y + p[j].y) * (p[i].x * p[j].y - p[j].x * p[i].y)
   END ;
   IF Debugging
   THEN
      printf ("cofg = %g, %g\n", x/a, y/a)
   END ;
   RETURN initCoord (x/a, y/a)
END calculateCofG ;


(*
   calcArea -
*)

PROCEDURE calcArea (n: CARDINAL; p: ARRAY OF Coord) : REAL ;
VAR
   i, j   : CARDINAL ;
   a, r, b: REAL ;
BEGIN
   a := 0.0 ;
   IF Debugging
   THEN
      printf ("calculating area: ")
   END ;
   FOR i := 0 TO n-1 DO
      IF Debugging
      THEN
         printf ("(%g, %g) ", p[i].x, p[i].y)
      END ;
      j := (i+1) MOD n ;
      r := (p[i].x * p[j].y) ;
      b := (p[i].y * p[j].x) ;
      IF Debugging
      THEN
         printf (" [x1 x y1 = %g x %g = %g = r] ",
                 p[i].x, p[j].y, r);
         printf (" [x1 x y1 = %g x %g = %g = b] ",
                 p[i].y, p[j].x, b)
      END ;
      a := a + r - b ;
      IF Debugging
      THEN
         printf (" [a = %g] ", a)
      END
   END ;
   IF Debugging
   THEN
      printf ("end area = %g\n", a / 2.0)
   END ;
   RETURN a / 2.0
END calcArea ;


(*
   toRPolygon - convert polygon, id, into a rotating polygon.
*)
(*
PROCEDURE toRPolygon (id: CARDINAL) ;
VAR
   nptr,
   optr: Object ;
   i   : CARDINAL ;
BEGIN
   optr := GetIndice(objects, id) ;
   checkDeleted(optr) ;
   IF optr^.object=polygonOb
   THEN
      NEW(nptr) ;
      nptr^ := optr^ ;
      WITH nptr^ DO
         object := rpolygonOb ;
         r.nPoints := optr^.p.nPoints ;
         r.cOfG := calculateCofG(r.nPoints, optr^.p.points) ;
         i := 0 ;
         WHILE i<r.nPoints DO
            r.points[i] := coordToPolar(subCoord(optr^.p.points[i], r.cOfG)) ;
            INC(i)
         END
      END ;
      optr^ := nptr^ ;
      DISPOSE(nptr)
   END
END toRPolygon ;
*)


(*
   rotate - gives object, id, an initial orientation.
*)

PROCEDURE rotate (id: CARDINAL; angle: REAL) : CARDINAL ;
VAR
   optr: Object ;
BEGIN
   IF NOT nearZero (angle)
   THEN
      optr := GetIndice (objects, id) ;
      checkDeleted (optr) ;
      IF optr^.fixed
      THEN
         printf("object %d is fixed and therefore cannot be given an angular velocity\n",
                id)
      ELSE
         optr^.angleOrientation := angle
      END
   END ;
   RETURN id
END rotate ;


(*
   rvel - gives object, id, an angular velocity, angle.
*)

PROCEDURE rvel (id: CARDINAL; angle: REAL) : CARDINAL ;
VAR
   optr: Object ;
BEGIN
   IF NOT nearZero(angle)
   THEN
      optr := GetIndice (objects, id) ;
      checkDeleted (optr) ;
      IF optr^.fixed
      THEN
         printf("object %d is fixed and therefore cannot be given an angular velocity\n",
                id)
      ELSE
         optr^.angularVelocity := angle
      END
   END ;
   RETURN id
END rvel ;


(*
   fps - set frames per second.
*)

PROCEDURE fps (f: REAL) ;
BEGIN
   framesPerSecond := f
END fps ;


(*
   debugCircle - displays a circle at position, p, with radius, r, in colour, c.
*)

PROCEDURE debugCircle (p: Coord; r: REAL; c: Colour) ;
BEGIN
   glyphCircle (c2p (p), TRUE, zero (), putReal (r), c)
END debugCircle ;


(*
   debugLine - displays a line from, p1, to, p2, in the debugging colour.
*)

PROCEDURE debugLine (p1, p2: Coord; c: Colour) ;
CONST
   thickness = 0.01 ;
VAR
   p      : ARRAY [0..3] OF Point ;
   dy, dxy: Coord ;
BEGIN
   sortLine (p1, p2) ;
   dy := initCoord (0.0, thickness * 2.0) ;
   dxy := initCoord (thickness, thickness) ;
   p[0] := c2p (subCoord (p1, dxy)) ;
   p[1] := c2p (addCoord (subCoord (p1, dxy), dy)) ;
   p[2] := c2p (addCoord (p2, dxy)) ;
   p[3] := c2p (subCoord (addCoord (p2, dxy), dy)) ;
   glyphPolygon (4, p, TRUE, zero (), c)
END debugLine ;


(*
   replayRate - set frames per second during replay.
*)

PROCEDURE replayRate (f: REAL) ;
BEGIN
   replayPerSecond := f
END replayRate ;


(*
   doCircle - pass parameters to the groffDevice.
*)

PROCEDURE doCircle (p: Coord; r: REAL; c: Colour) ;
BEGIN
   glyphCircle (c2p (p), TRUE, zero (), putReal (r), c)
END doCircle ;


(*
   doPolygon -
*)

PROCEDURE doPolygon (n: CARDINAL; p: ARRAY OF Coord; c: Colour) ;
VAR
   points: ARRAY [0..MaxPolygonPoints] OF Point ;
   i     : CARDINAL ;
BEGIN
   FOR i := 0 TO n-1 DO
      IF Debugging
      THEN
         printf ("polygon point %d: %g, %g\n",
                 i, p[i].x, p[i].y)
      END ;
      points[i] := c2p (p[i]) ;
   END ;
   glyphPolygon (n, points, TRUE, zero (), c)
END doPolygon ;


(*
   drawBoarder -
*)

PROCEDURE drawBoarder (c: Colour) ;
VAR
   p: ARRAY [0..3] OF Point ;
BEGIN
   p[0] := initPoint(zero(), zero ()) ;
   p[1] := initPoint(one(), zero ()) ;
   p[2] := initPoint(one(), one ()) ;
   p[3] := initPoint(zero(), one ()) ;
   glyphPolygon(4, p, FALSE, initFract (0, 1, 100), c)
END drawBoarder ;


(*
   drawBackground -
*)

PROCEDURE drawBackground (c: Colour) ;
VAR
   p: ARRAY [0..3] OF Point ;
BEGIN
   p[0] := initPoint (zero (), zero ()) ;
   p[1] := initPoint (one (), zero ()) ;
   p[2] := initPoint (one (), one ()) ;
   p[3] := initPoint (zero (), one ()) ;
   glyphPolygon (4, p, TRUE, zero (), c)
END drawBackground ;


(*
   getVelCoord - returns a velocity coordinate pair for Object, o.
*)

PROCEDURE getVelCoord (o: Object) : Coord ;
BEGIN
   checkDeleted (o) ;
   WITH o^ DO
      IF fixed OR stationary
      THEN
         RETURN initCoord (0.0, 0.0)
      ELSE
         RETURN initCoord (vx, vy)
      END
   END
END getVelCoord ;


(*
   getAccelCoord - returns an acceleration coordinate pair for Object, o.
*)

PROCEDURE getAccelCoord (o: Object) : Coord ;
BEGIN
   checkDeleted(o) ;
   WITH o^ DO
      IF fixed OR stationary
      THEN
         RETURN initCoord (0.0, 0.0)
      ELSE
         RETURN initCoord (ax, ay+simulatedGravity)
      END
   END
END getAccelCoord ;


(*
   setCollisionColour - assigns, c, as the colour for objects colliding.
*)

PROCEDURE setCollisionColour (c: Colour) ;
BEGIN
   collisionColour := c ;
   haveCollisionColour := TRUE
END setCollisionColour ;


(*
   getCollisionColour - returns the collision colour if requiredDebug,
                        otherwise return, c.
*)

PROCEDURE getCollisionColour (c: Colour; requiredDebug: BOOLEAN) : Colour ;
BEGIN
   IF requiredDebug AND haveCollisionColour
   THEN
      RETURN collisionColour
   ELSE
      RETURN c
   END
END getCollisionColour ;


(*
   doDrawFrame -
*)

PROCEDURE doDrawFrame (optr: Object; dt: REAL; needsDebug: BOOLEAN) ;
VAR
   i     : CARDINAL ;
   co,
   vc, ac: Coord ;
   po    : ARRAY [0..MaxPolygonPoints] OF Coord ;
   oc    : Colour ;
BEGIN
   IF DebugTrace
   THEN
      printf ("doDrawFrame (%g)\n", dt)
   END ;
   checkDeleted (optr) ;
   vc := getVelCoord (optr) ;
   ac := getAccelCoord (optr) ;
   WITH optr^ DO
      CASE object OF

      circleOb :  doCircle (newPositionCoord (c.pos, vc, ac, dt), c.r, getCollisionColour (c.col, needsDebug)) |
      springOb :  |
      polygonOb:  (* gdbif.sleepSpin ; *)
                  FOR i := 0 TO p.nPoints-1 DO
                     po[i] := newPositionRotationCoord (p.cOfG, vc, ac, dt,
                                                        angularVelocity, angleOrientation, p.points[i]) ;
                     IF Debugging
                     THEN
                        printf ("po[%d].x = %g, po[%d].y = %g\n", i, po[i].x, i, po[i].y)
                     END ;
                     co := addCoord (p.cOfG, polarToCoord (rotatePolar (p.points[i], angleOrientation))) ;
                     IF Debugging
                     THEN
                        printf (" [co.x = %g, co.y = %g]\n", co.x, co.y)
                     END ;
                     IF nearZero (dt)
                     THEN
                        IF (NOT nearZero (co.x-po[i].x)) OR (NOT nearZero (co.y-po[i].y))
                        THEN
                           printf ("these values should be the same\n") ;
                           exit (1)
                        END
                     END
                  END ;
                  doPolygon (p.nPoints, po, getCollisionColour (p.col, needsDebug))

      END
   END
END doDrawFrame ;


(*
   getCollisionObjects -
*)

PROCEDURE getCollisionObjects (VAR id1, id2: Object; e: eventQueue) ;
BEGIN
   id1 := NIL ;
   id2 := NIL ;
   IF e # NIL
   THEN
      WITH e^.ePtr^ DO
         CASE etype OF

         circlesEvent:  id1 := GetIndice(objects, cc.cid1) ;
                        id2 := GetIndice(objects, cc.cid2) |

         circlePolygonEvent:
                        id1 := GetIndice(objects, cp.cid) ;
                        id2 := GetIndice(objects, cp.pid) |

         polygonPolygonEvent:
                        id1 := GetIndice(objects, pp.pid1) ;
                        id2 := GetIndice(objects, pp.pid2)

         END
      END
   END
END getCollisionObjects ;


(*
   drawFrame - draws the current world into the frame buffer.
               If e is not NIL then it will be a collision event
               which describes the objects colliding.  The
               drawFrame will draw these objects using
               the debugging colour.
*)

PROCEDURE drawFrame (e: eventQueue) ;
VAR
   dt      : REAL ;
   i, n    : CARDINAL ;
   id1, id2,
   optr    : Object ;
BEGIN
   Assert ((e = NIL) OR (e^.kind = collisionKind), __LINE__) ;
   getCollisionObjects (id1, id2, e) ;
   IF DebugTrace
   THEN
      printf ("start drawFrame\n")
   END ;
   IF writeTimeDelay
   THEN
      writeTime (currentTime-lastDrawTime)
   END ;
   lastDrawTime := currentTime ;
   dt := currentTime-lastUpdateTime ;
   IF DebugTrace
   THEN
      printf ("before drawBoarder\n")
   END ;
   drawBoarder (black()) ;
   IF DebugTrace
   THEN
      printf ("after drawBoarder\n")
   END ;
   n := HighIndice (objects) ;
   i := 1 ;
   WHILE i<=n DO
      optr := GetIndice (objects, i) ;
      IF (optr#NIL) AND (NOT optr^.deleted)
      THEN
         IF Debugging
         THEN
            DumpObject (optr)
         END ;
         (* printf ("before doDrawFrame\n"); *)
         doDrawFrame (optr, dt, (optr=id1) OR (optr=id2)) ;
         (* printf ("after doDrawFrame\n"); *)
      END ;
      INC (i)
   END ;
   (* printf ("end drawFrame\n"); *)
END drawFrame ;


(*
   drawFrameEvent -
*)

PROCEDURE drawFrameEvent (e: eventQueue) ;
BEGIN
   IF DebugTrace
   THEN
      printf ("start drawFrameEvent\n")
   END ;
   frameNote ;
   IF DebugTrace
   THEN
      printf ("before drawFrame\n")
   END ;
   drawFrame (NIL) ;
   IF DebugTrace
   THEN
      printf ("before flipBuffer\n")
   END ;
   flipBuffer ;
   IF DebugTrace
   THEN
      printf ("before addEvent\n")
   END ;
   addEvent (frameKind, 1.0/framesPerSecond, drawFrameEvent) ;

   IF DebugTrace
   THEN
      printf ("collectAll\n")
   END ;
   collectAll ;

   IF DebugTrace
   THEN
      printf ("end drawFrameEvent\n")
   END
END drawFrameEvent ;


(*
   incRadians - return (a + b) mod 2pi.   The value returned will be between 0..2pi
*)

PROCEDURE incRadians (a, b: REAL) : REAL ;
BEGIN
   a := a + b ;
   IF nearZero (a)
   THEN
      a := 0.0
   ELSE
      WHILE a < 0.0 DO
         a := a + (2.0 * pi)
      END ;
      WHILE a > (2.0 * pi) DO
         a := a - (2.0 * pi)
      END
   END ;
   RETURN a
END incRadians ;


(*
   updatePolygon -
*)

PROCEDURE updatePolygon (optr: Object; dt: REAL) ;
BEGIN
   WITH optr^ DO
      IF NOT deleted
      THEN
         p.cOfG.x := newPositionScalar (p.cOfG.x, vx, ax, dt) ;
         p.cOfG.y := newPositionScalar (p.cOfG.y, vy, ay+simulatedGravity, dt) ;
         vx := vx + ax*dt ;
         vy := vy + (ay+simulatedGravity) * dt ;
         angleOrientation := incRadians (angleOrientation, angularVelocity * dt)
      END
   END
END updatePolygon ;


(*
   updateCircle -
*)

PROCEDURE updateCircle (optr: Object; dt: REAL) ;
VAR
   vn: REAL ;
BEGIN
   WITH optr^ DO
      IF NOT deleted
      THEN
         (*
         checkZero(dt) ;
         checkZero(vx) ;
         checkZero(vy) ;
         *)
         (* update vx and pos.x *)

         c.pos.x := newPositionScalar (c.pos.x, vx, ax, dt) ;
         vx := vx + ax*dt ;

         (* update vy and pos.y *)
         c.pos.y := newPositionScalar (c.pos.y, vy, ay+simulatedGravity, dt) ;
         vy := vy + (ay+simulatedGravity) * dt
      END
   END
END updateCircle ;


(*
   updateOb -
*)

PROCEDURE updateOb (optr: Object; dt: REAL) ;
BEGIN
   WITH optr^ DO
      IF (NOT deleted) AND (NOT fixed) AND (NOT stationary)
      THEN
         CASE object OF

         polygonOb :  updatePolygon (optr, dt) |
         circleOb  :  updateCircle (optr, dt) |
         springOb  :

         END
      END
   END
END updateOb ;


(*
   doUpdatePhysics - updates all positions of objects based on the passing of
                     dt seconds.
*)

PROCEDURE doUpdatePhysics (dt: REAL) ;
VAR
   i, n: CARDINAL ;
   optr: Object ;
BEGIN
   n := HighIndice(objects) ;
   i := 1 ;
   WHILE i<=n DO
      optr := GetIndice (objects, i) ;
      updateOb (optr, dt) ;
      INC (i)
   END
END doUpdatePhysics ;


(*
   updatePhysics - updates the velocity of all objects based on
                   the elapsed time from the last collision until now.
*)

PROCEDURE updatePhysics ;
BEGIN
   doUpdatePhysics (currentTime-lastUpdateTime) ;
   lastUpdateTime := currentTime
END updatePhysics ;


(*
   displayEvent -
*)

PROCEDURE displayEvent (e: eventQueue) ;
BEGIN
   WITH e^ DO
      printf ("%g %p ", time, p);
      IF kind = frameKind
      THEN
         printf ("frameKind ")
      ELSIF kind = collisionKind
      THEN
         printf ("collisionKind ")
      ELSIF kind = functionKind
      THEN
         printf ("functionEvent ")
      ELSE
         printf ("unknown kind ")
      END ;
      IF ePtr=NIL
      THEN
         printf ("\n")
      ELSE
         WITH ePtr^ DO
            CASE etype OF

            frameEvent         :   printf ("display frame event\n") |
            circlesEvent       :   printf ("circle %d and circle %d colliding event\n", cc.cid1, cc.cid2) |
            circlePolygonEvent :   printf ("circle %d and polygon %d colliding event\n", cp.cid, cp.pid) ;
                                   IF cp.wpid=corner
                                   THEN
                                      printf ("  hits polygon on its corner %d\n", cp.pointNo)
                                   ELSE
                                      printf ("  hits polygon on its edge %d\n", cp.lineNo)
                                   END |
            polygonPolygonEvent:   printf ("polygon %d and polygon %d colliding event\n", pp.pid1, pp.pid2) ;
                                   IF pp.wpid1=corner
                                   THEN
                                      printf ("  polygon (%d) corner %d hits", pp.pid1, pp.lineCorner1)
                                   ELSE
                                      printf ("  polygon (%d) edge %d hits ", pp.pid1, pp.lineCorner1)
                                   END ;
                                   IF pp.wpid2=corner
                                   THEN
                                      printf (" polygon (%d) on corner %d\n", pp.pid2, pp.lineCorner2)
                                   ELSE
                                      printf (" polygon (%d) on edge %d\n", pp.pid2, pp.lineCorner2)
                                   END |
            functionEvent      :   printf ("function event %d\n", fc.id)

            END
         END
      END
   END
END displayEvent ;


(*
   printQueue - prints out the event queue.
*)

PROCEDURE printQueue ;
VAR
   e: eventQueue ;
BEGIN
   IF DebugTrace OR TRUE
   THEN
      printf ("current time = %g,  lastDrawTime = %g,  lastUpdateTime = %g\n", currentTime, lastDrawTime, lastUpdateTime);
      printf ("The event queue\n");
      printf ("===============\n");
      e := eventQ ;
      WHILE e#NIL DO
         displayEvent (e) ;
         e := e^.next
      END
   END
END printQueue ;


(*
   updateStats -
*)

PROCEDURE updateStats (dt: REAL) ;
VAR
   lastTime: CARDINAL ;
   nextTime: CARDINAL ;
   fps     : CARDINAL ;
BEGIN
   lastTime := TRUNC (currentTime*10.0) ;
   nextTime := TRUNC ((currentTime+dt)*10.0) ;
   IF lastTime#nextTime
   THEN
      fps := getActualFPS () ;
      printf ("%d.%d seconds simulated, fps: %d\n", nextTime DIV 10, nextTime MOD 10, fps)
   END
END updateStats ;


(*
   doFunctionEvent -
*)

PROCEDURE doFunctionEvent (e: eventQueue) ;
BEGIN
   (* nothing to do.  *)
   IF DebugTrace
   THEN
      printf ("doFunctionEvent\n") ;
      printQueue
   END ;
   resetQueue ;
   IF DebugTrace
   THEN
      printf ("adjusting the queue\n") ;
      printQueue ;
      printf ("end of FunctionEvent\n")
   END
END doFunctionEvent ;


(*
   doNextEvent -
*)

PROCEDURE doNextEvent () : REAL ;
VAR
   e : eventQueue ;
   dt: REAL ;
   p : eventProc ;
BEGIN
   IF eventQ=NIL
   THEN
      printf ("no more events on the event queue\n") ;
      exit (1);
      RETURN 0.0
   ELSE
      e := eventQ ;
      eventQ := eventQ^.next ;
      dt := e^.time ;
      p  := e^.p ;
      currentTime := currentTime + dt ;
      Assert ((p=VAL(eventProc, drawFrameEvent)) OR
              (p=VAL(eventProc, doCollision)) OR
              (p=VAL(eventProc, debugFrame)) OR
	      (p=VAL(eventProc, doFunctionEvent)),
	      __LINE__) ;
      p (e) ;
      disposeDesc (e^.ePtr) ;
      disposeEvent (e) ;
      updateStats (dt) ;
      checkInterpen ;
      RETURN dt
   END
END doNextEvent ;


(*
   checkObjects - perform a check to make sure that all non fixed objects have a mass.
*)

PROCEDURE checkObjects ;
VAR
   i, n : CARDINAL ;
   optr : Object ;
   error: BOOLEAN ;
BEGIN
   error := FALSE ;
   n := HighIndice (objects) ;
   i := 1 ;
   WHILE i<=n DO
      optr := GetIndice (objects, i) ;
      WITH optr^ DO
         IF (NOT fixed) AND (NOT deleted)
         THEN
            CASE object OF

            polygonOb :  IF nearZero (p.mass)
                         THEN
                            printf ("polygon %d is not fixed and does not have a mass\n", optr^.id) ;
                            error := TRUE
                         END |
            circleOb  :  IF nearZero (c.mass)
                         THEN
                            printf ("circle %d is not fixed and does not have a mass\n", optr^.id) ;
                            error := TRUE
                         END

            ELSE
            END
         END
      END ;
      INC(i)
   END ;
   IF error
   THEN
      exit (1)
   END
END checkObjects ;


(*
   checkZero -
*)

PROCEDURE checkZero (VAR v: REAL) ;
BEGIN
   IF ((v>0.0) AND (v<0.01)) OR
      ((v<0.0) AND (v>-0.01))
   THEN
      v := 0.0
   END
END checkZero ;


(*
   checkZeroCoord -
*)

PROCEDURE checkZeroCoord (c: Coord) : Coord ;
BEGIN
   IF nearZero(c.x)
   THEN
      c.x := 0.0
   END ;
   IF nearZero (c.y)
   THEN
      c.y := 0.0
   END ;
   RETURN c
END checkZeroCoord ;


(*
   inElastic -
*)

PROCEDURE inElastic (VAR v: REAL) ;
BEGIN
   v := v * Elasticity ;
   checkZero (v)
END inElastic ;


(*
   nearZeroVelocity - returns TRUE if, r, is close to 0.0
*)

PROCEDURE nearZeroVelocity (r: REAL) : BOOLEAN ;
BEGIN
   IF r>=0.0
   THEN
      RETURN r<0.01
   ELSE
      RETURN (-r)<0.01
   END
END nearZeroVelocity ;


(*
   checkStationary - checks to see if object, o, should be put into
                     the stationary state.
*)

PROCEDURE checkStationary (o: Object) ;
BEGIN
   IF objectExists (o)
   THEN
      WITH o^ DO
         IF NOT fixed
         THEN
            inElastic (vx) ;
            inElastic (vy) ;
            stationary := nearZeroVelocity (vx) AND nearZeroVelocity (vy) ;
            IF stationary
            THEN
               vx := 0.0 ;
               vy := 0.0 ;
               IF Debugging
               THEN
                  DumpObject (o)
               END
            END
         END
      END
   END
END checkStationary ;


(*
   checkStationaryCollision - stationary object, a, has been bumped by
                              moving object, b.  We move a slightly and
                              give it an initial velocity and change its
                              state from stationary to moving.
*)

PROCEDURE checkStationaryCollision (a, b: Object) ;
BEGIN
   IF a^.stationary AND (NOT a^.deleted)
   THEN
      IF Debugging
      THEN
         printf ("object %d has bumped into a stationary object %d\n", b^.id, a^.id)
      END ;
      (* gdbif.sleepSpin ; *)
      a^.vy := 1.0 ;
      IF a^.c.pos.x<b^.c.pos.x
      THEN
         a^.c.pos.x := a^.c.pos.x-0.001
      ELSE
         a^.c.pos.x := a^.c.pos.x+0.001
      END ;
      a^.c.pos.y := a^.c.pos.y+0.001 ;
      a^.stationary := FALSE ;
      IF Debugging
      THEN
         DumpObject (a)
      END
   ELSIF b^.stationary AND (NOT b^.deleted)
   THEN
      checkStationaryCollision (b, a)
   END
END checkStationaryCollision ;


(*
   collideFixedCircles - works out the new velocity given that the circle
                         movable collides with the fixed circle.
*)

PROCEDURE collideFixedCircles (movable, fixed: Object) ;
BEGIN
   collideAgainstFixedCircle (movable, fixed^.c.pos)
END collideFixedCircles ;


(*
   collideAgainstFixedCircle - the movable object collides against a point, center.
                               center, is the center point of the other fixed circle.
                               This procedure works out the new velocity of the movable
                               circle given these constraints.
*)

PROCEDURE collideAgainstFixedCircle (movable: Object; center: Coord) ;
VAR
   r, j              : REAL ;
   c, normalCollision,
   relativeVelocity  : Coord ;
BEGIN
   (* calculate normal collision value *)
   c.x := movable^.c.pos.x - center.x ;
   c.y := movable^.c.pos.y - center.y ;
   r := sqrt (c.x*c.x+c.y*c.y) ;

   normalCollision.x := c.x/r ;
   normalCollision.y := c.y/r ;
   relativeVelocity.x := movable^.vx ;
   relativeVelocity.y := movable^.vy ;

   j := (-(1.0+1.0) *
         ((relativeVelocity.x * normalCollision.x) +
          (relativeVelocity.y * normalCollision.y)))/
        (((normalCollision.x*normalCollision.x) +
          (normalCollision.y*normalCollision.y)) *
         (1.0/movable^.c.mass)) ;

   movable^.vx := movable^.vx + (j * normalCollision.x) / movable^.c.mass ;
   movable^.vy := movable^.vy + (j * normalCollision.y) / movable^.c.mass ;

   checkStationary (movable)
END collideAgainstFixedCircle ;


(*
   collideMovableCircles -
*)

PROCEDURE collideMovableCircles (iptr, jptr: Object) ;
VAR
   r, j              : REAL ;
   c, normalCollision,
   relativeVelocity  : Coord ;
BEGIN
   (* calculate normal collision value *)
   c.x := iptr^.c.pos.x - jptr^.c.pos.x ;
   c.y := iptr^.c.pos.y - jptr^.c.pos.y ;
   r := sqrt (c.x*c.x+c.y*c.y) ;
   normalCollision.x := c.x/r ;
   normalCollision.y := c.y/r ;
   relativeVelocity.x := iptr^.vx - jptr^.vx ;
   relativeVelocity.y := iptr^.vy - jptr^.vy ;
   j := (-(1.0+1.0) *
         ((relativeVelocity.x * normalCollision.x) +
          (relativeVelocity.y * normalCollision.y)))/
        (((normalCollision.x*normalCollision.x) +
          (normalCollision.y*normalCollision.y)) *
         (1.0/iptr^.c.mass + 1.0/jptr^.c.mass)) ;

   iptr^.vx := iptr^.vx + (j * normalCollision.x) / iptr^.c.mass ;
   iptr^.vy := iptr^.vy + (j * normalCollision.y) / iptr^.c.mass ;

   jptr^.vx := jptr^.vx - (j * normalCollision.x) / jptr^.c.mass ;
   jptr^.vy := jptr^.vy - (j * normalCollision.y) / jptr^.c.mass ;

   checkStationaryCollision (iptr, jptr) ;

   checkStationary (iptr) ;
   checkStationary (jptr)

END collideMovableCircles ;


(*
   circleCollision - call fixed or movable circle collision depending upon whether
                     one or two circles are fixed.
                     Apart from taking into account rotation of either circle this
                     is complete.
*)

PROCEDURE circleCollision (iptr, jptr: Object) ;
BEGIN
   IF iptr^.fixed
   THEN
      collideFixedCircles (jptr, iptr)
   ELSIF jptr^.fixed
   THEN
      collideFixedCircles (iptr, jptr)
   ELSE
      collideMovableCircles (iptr, jptr)
   END
END circleCollision ;


(*
   collideCircleAgainstFixedEdge - modifies the circle velocity based upon the edge it hits.
                                   We use the formula:

                                   V = 2 * (-I . N ) * N + I

                                   where:

                                   I is the initial velocity vector
                                   V is the final velocity vector
                                   N is the normal to the line
*)

PROCEDURE collideCircleAgainstFixedEdge (cPtr: Object; p1, p2: Coord) ;
VAR
   l, vel: Coord ;
BEGIN
   (* firstly we need to find the normal to the line *)
   sortLine (p1, p2) ;    (* p1 and p2 are the start end positions of the line *)

   l := subCoord (p2, p1) ;     (* l is the vector p1 -> p2 *)
   vel := initCoord (cPtr^.vx, cPtr^.vy) ;   (* vel is the initial velocity *)

   vel := reflect (vel, l) ;

   cPtr^.vx := vel.x ;   (* update velocity of object, cPtr *)
   cPtr^.vy := vel.y ;

   checkStationary (cPtr)

END collideCircleAgainstFixedEdge ;


(*
   circlePolygonCollision -
*)

PROCEDURE circlePolygonCollision (e: eventQueue; cPtr, pPtr: Object) ;
VAR
   ln    : CARDINAL ;
   p1, p2: Coord ;
BEGIN
   WITH e^.ePtr^ DO
      IF etype=circlePolygonEvent
      THEN
         CASE cp.wpid OF

         corner:  IF cPtr^.fixed
                  THEN
                     (* moving polygon hits a fixed circle *)
                     (* --fixme--   to do later *)
                     HALT
                  ELSIF pPtr^.fixed
                  THEN
                                         (* moving circle hits fixed polygon corner *)
                     collideAgainstFixedCircle (cPtr, e^.ePtr^.cp.cPoint)
                  ELSE
                     (* both moving, to do later --fixme-- *)
                     HALT
                  END |
         edge  :  IF cPtr^.fixed
                  THEN
                     (* fixed circle against moving polygon *)
                     (* --fixme--   to do later *)
                     HALT
                  ELSIF pPtr^.fixed
                  THEN
                     (* moving circle hits fixed polygon, on the edge *)
                     ln := e^.ePtr^.cp.lineNo ;
                     getPolygonLine (ln, pPtr, p1, p2) ;
                     collideCircleAgainstFixedEdge (cPtr, p1, p2)
                  ELSE
                     (* both moving, to do later --fixme-- *)
                     HALT
                  END
        END
     ELSE
        HALT  (* should be circlePolygonEvent *)
     END
   END
END circlePolygonCollision ;


(*
   collidePolygonAgainstFixedCircle - polygon, o, is moving and has hit
                                      a fixed circle at position, collision.
*)

PROCEDURE collidePolygonAgainstFixedCircle (o: Object; collision: Coord) ;
BEGIN
   collideAgainstFixedCircle (o, collision) ;
   IF Debugging
   THEN
      DumpObject (o)
   END
END collidePolygonAgainstFixedCircle ;


(*
   collidePolygonAgainstFixedEdge - p1, p2 is the fixed edge and, o, is the
                                    moving polygon.
*)

PROCEDURE collidePolygonAgainstFixedEdge (o: Object; p1, p2: Coord) ;
BEGIN
   (* find the point of collision, this is the mid point along
      the shortest intersection between, o, and p1->p2.  *)

   collideCircleAgainstFixedEdge (o, p1, p2) ;
   IF Debugging
   THEN
      DumpObject (o)
   END
END collidePolygonAgainstFixedEdge ;


(*
   rotationalVelocity - return a vector containing the velocity
                        if polygon is rotating at angular speed, w,
                        and non rotation speed is, u and the relative
                        position is, r.
*)

PROCEDURE rotationalVelocity (w: REAL; u, r: Coord) : Coord ;
BEGIN
   RETURN addCoord (u, scaleCoord (r, w))
END rotationalVelocity ;


(*
   calcInertia - calculate the inertia of a polygon.
*)

PROCEDURE calcInertia (id: Object) ;
VAR
   ci, cj: Coord ;
   top,
   bot,
   t, b  : REAL ;
   i, j  : CARDINAL ;
BEGIN
   IF id^.object=polygonOb
   THEN
      WITH id^ DO
         top := 0.0 ;
         bot := 0.0 ;
         FOR i := 0 TO p.nPoints-1 DO
            j := (i+1) MOD p.nPoints ;
            ci := addCoord (p.cOfG, polarToCoord (rotatePolar (p.points[i], angleOrientation))) ;
            cj := addCoord (p.cOfG, polarToCoord (rotatePolar (p.points[j], angleOrientation))) ;
            t := (sqr (ci.x) + sqr (ci.y) + ci.x*cj.x + ci.y*cj.y + sqr (cj.x) + sqr (cj.y)) *
                 (ci.x * cj.y - cj.x * ci.y) ;
            top := top + t ;
            b := ci.x * cj.y - cj.x * ci.y ;
            bot := bot + b
         END
      END ;
      id^.inertia := (id^.p.mass * top) / (bot * 6.0)
   ELSE
      id^.inertia := id^.c.mass
   END
END calcInertia ;


(*
   updatePolygonVelocity - update both linear and angular velocities of
                           object, o, using impulse, j.
                           n is the perpendicular to the impact.
                           rp is the distance of the c of g of an object,
                           o, to the point of collision.
*)

PROCEDURE updatePolygonVelocity (o: Object; j: REAL; n, rpn: Coord) ;
VAR
   va: Coord ;
BEGIN
   (* linear velocity update.   (eq 8a)  *)
   va := initCoord (o^.vx, o^.vy) ;
   va := addCoord (va, scaleCoord (n, j/o^.p.mass)) ;
   o^.vx := va.x ;
   o^.vy := va.y ;

   (* angular velocity update.  (eq 8b)  *)
   o^.angularVelocity := o^.angularVelocity + dotProd (rpn, scaleCoord (n, j)) / o^.inertia
END updatePolygonVelocity ;


(*
   polygonPolygonCollision - two polygons collide, we call the appropriate routines
                             depending upon whether one polygon is fixed.
*)

PROCEDURE polygonPolygonCollision (e: eventQueue; id1, id2: Object) ;
BEGIN
   collidePolygonAgainstMovingPolygon (e, id1, id2)
(*
   IF id1^.fixed
   THEN
      collidePolygonAgainstFixedPolygon (e, id2, id1)
   ELSIF id2^.fixed
   THEN
      collidePolygonAgainstFixedPolygon (e, id1, id2)
   ELSE
      collidePolygonAgainstMovingPolygon (e, id1, id2)
   END
*)
END polygonPolygonCollision ;


(*
   reflect - reflect velocity, v, off line, l.

             V = 2 * (-I . n ) * n + I

             where:

             I is the initial velocity vector
             V is the final velocity vector
             n is the normal to the line, l.
*)

PROCEDURE reflect (v, l: Coord) : Coord ;
VAR
   n: Coord ;
BEGIN
   n := perpendicular (l) ;
   n := normaliseCoord (n) ;
   RETURN addCoord (scaleCoord (n, -2.0 * dotProd (v, n)), v)
END reflect ;


(*
   collidePolygonAgainstFixedPolygon - id1 is moving and id2 is fixed.
*)

PROCEDURE collidePolygonAgainstFixedPolygon (e: eventQueue; id1, id2: Object) ;
VAR
   I, m,
   j     : REAL ;
   l,
   n, rap,
   rapn,
   p1, p2,
   p, v  : Coord ;   (* point of collision  *)
BEGIN
   Assert (NOT id1^.fixed, __LINE__) ;
   Assert (id2^.fixed, __LINE__) ;
   Assert (e^.ePtr^.etype=polygonPolygonEvent, __LINE__) ;

   IF Debugging
   THEN
      printf ("collidePolygonAgainstFixedPolygon\n")
   END ;
   drawFrame (e) ;      (* ****************** *)

   p := e^.ePtr^.pp.cPoint ;
   IF e^.ePtr^.pp.wpid2=edge
   THEN
      getPolygonLine (e^.ePtr^.pp.lineCorner2, GetIndice (objects, e^.ePtr^.pp.pid2), p1, p2) ;
      l := subCoord (p2, p1) ;
      debugLine (p1, p2, yellow ())
   ELSE
      (* hits corner, so we use the normal from the corner to the C of G of he polygon.  *)
      l := subCoord (p, id2^.p.cOfG)
   END ;
   v := rotationalVelocity (id1^.angularVelocity, initCoord (id1^.vx, id1^.vy), subCoord (p, id1^.p.cOfG)) ;

   rap := subCoord (p, id1^.p.cOfG) ;
   rapn := perpendicular (rap) ;
   I := sqr (dotProd (rapn, n)) / id1^.inertia ;
   m := 1.0 / id1^.p.mass ;

   debugCircle (id1^.p.cOfG, 0.002, yellow ()) ;          (* ******************  c of g for id1            *)
   debugCircle (id2^.p.cOfG, 0.002, purple ()) ;          (* ******************  c of g for id2            *)

   j := (-(1.0) * dotProd (v, n)) / (dotProd (n, n) * m + I) ;
   updatePolygonVelocity (id1, -j, n, rapn) ;

   flipBuffer ;      (* ****************** *)

END collidePolygonAgainstFixedPolygon ;


(*
   collidePolygonAgainstMovingPolygon - both, id1, and, id2, are moving.
*)

PROCEDURE collidePolygonAgainstMovingPolygon (e: eventQueue; id1, id2: Object) ;
VAR
   p1, p2,
   p,
   n, n2,
   v1,
   va,  vb,
   vap, vbp,
   vab,
   rap, rbp,
   sum,
   vF1, vF2   : Coord ;
   ca, cb,
   denominator,
   vabDotN,
   modifiedVel,
   j1, j2, m  : REAL ;
BEGIN
   IF Debugging
   THEN
      displayEvent(e) ;
      DumpObject(id1) ;
      DumpObject(id2)
   END ;
   Assert (e^.ePtr^.etype=polygonPolygonEvent, __LINE__) ;
   p := e^.ePtr^.pp.cPoint ;

   frameNote ;      (* ****************** *)
   drawFrame (e) ;  (* ****************** *)

   IF (e^.ePtr^.pp.wpid1=edge) AND (e^.ePtr^.pp.wpid2=edge)
   THEN
      IF Debugging
      THEN
         printf ("the edges of two polygon collide\n")
      END ;
      getPolygonLine (e^.ePtr^.pp.lineCorner1, GetIndice (objects, e^.ePtr^.pp.pid1), p1, p2) ;
      sortLine (p1, p2) ;           (* p1 and p2 are the start end positions of the line  *)
      v1 := subCoord (p2, p1) ;     (* v1 is the vector p1 -> p2  *)
      perpendiculars (v1, n, n2) ;  (* n and n2 are normal vectors to the vector v1  *)
      (* n needs to point into id1.  *)
      debugLine (p1, p2, yellow ())
   ELSIF e^.ePtr^.pp.wpid1=edge
   THEN
      (* corner collision.  *)
      IF Debugging
      THEN
         printf ("the edge of polygon collides with corner of polygon\n")
      END ;
      Assert (e^.ePtr^.pp.wpid2=corner, __LINE__) ;

      getPolygonLine (e^.ePtr^.pp.lineCorner2, GetIndice (objects, e^.ePtr^.pp.pid2), p1, p2) ;
      v1 := subCoord (p2, p1) ;     (* v1 is the vector p1 -> p2  *)
      perpendiculars (v1, n, n2) ;  (* n and n2 are normal vectors to the vector v1  *)
      (* n needs to point into id1.  *)
      debugLine (p1, p2, yellow ())
   ELSIF e^.ePtr^.pp.wpid2=edge
   THEN
      IF Debugging
      THEN
         printf ("the edge of polygon collides with corner of polygon\n")
      END ;
      Assert (e^.ePtr^.pp.wpid1=corner, __LINE__) ;

      getPolygonLine (e^.ePtr^.pp.lineCorner1, GetIndice (objects, e^.ePtr^.pp.pid1), p1, p2) ;
      v1 := subCoord (p2, p1) ;     (* v1 is the vector p1 -> p2  *)
      perpendiculars (v1, n, n2) ;  (* n and n2 are normal vectors to the vector v1  *)
      (* n needs to point into id1.  *)
      debugLine (p1, p2, yellow ())
   ELSE
      printf ("the corners of two polygon collide\n");
   END ;

   debugCircle (id1^.p.cOfG, 0.002, yellow ()) ;          (* ******************  c of g for id1            *)
   debugCircle (id2^.p.cOfG, 0.002, purple ()) ;          (* ******************  c of g for id2            *)
   (* debugCircle (getPolygonCoord (id1, e^.ePtr^.pp.pointNo), 0.002, white ()) ; *)
                                                          (* ******************  point on id1 of collision *)

   (* calculate relative velocity.  *)
   rap := subCoord (p, id1^.p.cOfG) ;
   rbp := subCoord (p, id2^.p.cOfG) ;
   vap := addCoord (initCoord (id1^.vx, id1^.vy), scaleCoord (rap, id1^.angularVelocity)) ;
   vbp := addCoord (initCoord (id2^.vx, id2^.vy), scaleCoord (rap, id2^.angularVelocity)) ;
   vab := subCoord (vap, vbp) ;

   (* calculate impulse factor.  *)
   IF id1^.fixed
   THEN
      m := 0.0
   ELSE
      m := 1.0/id1^.p.mass
   END ;
   IF NOT id2^.fixed
   THEN
      m := m + 1.0/id2^.p.mass
   END ;

   denominator := m * dotProd (n, n) ;

   (* calculate angular factors.  *)

   IF id1^.fixed
   THEN
      ca := 0.0
   ELSE
      ca := sqr (dotProd (perpendicular (rap), n)) / id1^.inertia
   END ;

   IF id2^.fixed
   THEN
      cb := 0.0
   ELSE
      cb := sqr (dotProd (perpendicular (rbp), n)) / id2^.inertia
   END ;

   denominator := denominator + ca + cb ;

   (* calculate total impulse of collision, j.  *)

   vabDotN := dotProd (vab, n) ;
   IF Debugging
   THEN
      printf ("vabDotN = %g\n", vabDotN)
   END ;

   modifiedVel := vabDotN / denominator ;
   j1 := -(1.0 + Elasticity) * modifiedVel ;
   j2 := (1.0 + Elasticity) * modifiedVel ;

   IF Debugging
   THEN
      printf ("j1 = %g, j2 = %g\n", j1, j2)
   END ;

   (* update the velocities.  *)

   IF NOT id1^.fixed
   THEN
      vF1 := addCoord (initCoord (id1^.vx, id1^.vy), scaleCoord (n, j1/id1^.p.mass)) ;
      id1^.vx := vF1.x ;
      id1^.vy := vF1.y
   END ;

   IF NOT id2^.fixed
   THEN
      vF2 := addCoord (initCoord (id2^.vx, id2^.vy), scaleCoord (n, j2/id2^.p.mass)) ;
      id2^.vx := vF2.x ;
      id2^.vy := vF2.y
   END ;

   (* update the angular velocities.  *)

   IF id1^.fixed
   THEN
      j2 := j2 * 2.0
   END ;
   IF id2^.fixed
   THEN
      j1 := j1 * 2.0
   END ;

   IF NOT id1^.fixed
   THEN
      id1^.angularMomentum := id1^.angularMomentum + dotProd (rap, scaleCoord (n, j1)) ;
      id1^.angularVelocity := (1.0 / id1^.inertia) * id1^.angularMomentum
   END ;

   IF NOT id2^.fixed
   THEN
      id2^.angularMomentum := id2^.angularMomentum + dotProd (rbp, scaleCoord (n, j2)) ;
      id2^.angularVelocity := (1.0 / id2^.inertia) * id2^.angularMomentum
   END ;

   flipBuffer      (* ****************** *)

END collidePolygonAgainstMovingPolygon ;


(*
   physicsCollision - handle the physics of a collision between
                      the two objects defined in, e.
*)

PROCEDURE physicsCollision (e: eventQueue) ;
VAR
   id1, id2    : Object ;
BEGIN
   WITH e^.ePtr^ DO
      CASE etype OF

      circlesEvent:  id1 := GetIndice (objects, cc.cid1) ;
                     id2 := GetIndice (objects, cc.cid2) ;
                     circleCollision (id1, id2) |

      circlePolygonEvent:
                     id1 := GetIndice (objects, cp.cid) ;
                     id2 := GetIndice (objects, cp.pid) ;
                     circlePolygonCollision (e, id1, id2) |

      polygonPolygonEvent:
                     id1 := GetIndice (objects, pp.pid1) ;
                     id2 := GetIndice (objects, pp.pid2) ;
                     polygonPolygonCollision (e, id1, id2)

      END
   END
END physicsCollision ;


(*
   doCollision - called whenever a collision event is processed.
*)

PROCEDURE doCollision (e: eventQueue) ;
BEGIN
   updatePhysics ;
   collisionOccurred (e^.ePtr) ;
   IF drawCollisionFrame
   THEN
      IF Debugging
      THEN
         printf ("issuing collision draw frame\n")
      END ;
      frameNote ;
      drawFrame (e) ;
      flipBuffer ;
(*
      collectAll
*)
   END ;
   physicsCollision (e) ;
   addNextCollisionEvent
END doCollision ;


(*
   sqr -
*)

PROCEDURE sqr (v: REAL) : REAL ;
BEGIN
   RETURN v*v
END sqr ;


(*
   cub -
*)

PROCEDURE cub (v: REAL) : REAL ;
BEGIN
   RETURN v*v*v
END cub ;


(*
   quad -
*)

PROCEDURE quad (v: REAL) : REAL ;
BEGIN
   RETURN v*v*v*v
END quad ;


(*
   pent -
*)

PROCEDURE pent (v: REAL) : REAL ;
BEGIN
   RETURN quad (v) * v
END pent ;


(*
   hex -
*)

PROCEDURE hex (v: REAL) : REAL ;
BEGIN
   RETURN cub (v) * cub (v)
END hex ;


(*
   sept -
*)

PROCEDURE sept (v: REAL) : REAL ;
BEGIN
   RETURN quad (v) * cub (v)
END sept ;


(*
   oct -
*)

PROCEDURE oct (v: REAL) : REAL ;
BEGIN
   RETURN quad (v) * quad (v)
END oct ;


(*
   getCircleValues - assumes, o, is a circle and retrieves:
                     center    (x, y)
                     radius    radius
                     velocity  (vx, vy)
                     accel     (ax, ay)
*)

PROCEDURE getCircleValues (o: Object; VAR x, y, radius, vx, vy, ax, ay: REAL) ;
BEGIN
   WITH o^ DO
      x      := c.pos.x ;
      y      := c.pos.y ;
      radius := c.r
   END ;
   getObjectValues(o, vx, vy, ax, ay)
END getCircleValues ;


(*
   getObjectValues - fills in velocity and acceleration x, y, values.
*)

PROCEDURE getObjectValues (o: Object; VAR vx, vy, ax, ay: REAL) ;
BEGIN
   IF o^.fixed OR o^.stationary
   THEN
      vx := 0.0 ;
      vy := 0.0 ;
      ax := 0.0 ;
      ay := 0.0
   ELSE
      vx := o^.vx ;
      vy := o^.vy ;
      ax := o^.ax ;
      ay := o^.ay + simulatedGravity
   END
END getObjectValues ;


(*
   getObjectOrbitingValues -
*)

PROCEDURE getObjectOrbitingValues (o: Object; VAR r, w: REAL; VAR cofg: Coord) ;
BEGIN
   WITH o^ DO
      r := angleOrientation ;
      w := angularVelocity ;
      CASE object OF

      polygonOb:  cofg := p.cOfG |
      circleOb :  cofg := c.pos

      ELSE
         HALT
      END
   END
END getObjectOrbitingValues ;


(*
   maximaCircleCollisionOrbiting -
   x1 y1 x2 y2

   a, g, l, r       is  initial position of the point (not the c of g)
   b, h, m, s       is  initial velocity
   c, i, n, u       is  acceleration
   e, k, p, v       is  angular velocity
   f, q, k, w       is  the initial angular offset for the center of circle relative to the c of g.
                    The c of g is the center of the orbit.
   d,    o          the distance of the point from the c of g.
*)

PROCEDURE maximaCircleCollisionOrbiting (VAR array: ARRAY OF REAL;
                                         a, b, c, d, e, f,
                                         g, h, i, j, k,
                                         l, m, n, o, p, q,
                                         r, s, u, v, w,
                                         x               : REAL) ;
BEGIN
(* #  include "rotating-circles.m" *)
END maximaCircleCollisionOrbiting ;


(*
   earlierCircleCollisionOrbiting -

           t          is the time of this collision (if any)
           tc         is the time of the next collision.

           c1p        is the initial position of the center of circle 1.
                      This may not be the c of g of this circle if it is in orbit.
           c1radius   is the radius of the circle.
           c1r        rotational offset of point 1.
           c1cofg     center of gravity of point 1.
           c1w        rotational angular velocity of point 1.
           c1a        acceleration of point 1.

           c2p        is the initial position of the center of circle 2.
                      This may not be the c of g of this circle if it is in orbit.
           c2radius   is the radius of the circle.
           c2r        rotational offset of point 2.
           c2cofg     center of gravity of point 2.
           c2w        rotational angular velocity of point 2.
           c2a        acceleration of point 2.
*)

PROCEDURE earlierCircleCollisionOrbiting (VAR t, tc: REAL;
                                          c1p: Coord; c1radius: REAL;
                                          c1r, c1w: REAL; c1cofg, c1v, c1a,
                                          c2p: Coord; c2radius: REAL;
                                          c2r, c2w: REAL; c2cofg, c2v, c2a: Coord) : BOOLEAN ;
VAR
   d1, d2,
   A, B, C, D, E, F, G, H, I, T: REAL ;
   array                       : ARRAY [0..8] OF REAL ;
BEGIN
   d1 := lengthCoord (subCoord (c1p, c1cofg)) ;
   d2 := lengthCoord (subCoord (c2p, c2cofg)) ;

   maximaCircleCollisionOrbiting (array,
                                  (* describe line 1 by its X coordinates.  *)
                                  c1p.x, c1v.x, c1a.x, d1, c1w, c1r,
                                  (* describe line 2 by its X coordinates.  *)
                                  c2p.x, c2v.x, c2a.x, c2w, c2r,
                                  (* describe line 1 by its Y coordinates.  *)
                                  c1p.y, c1v.y, c1a.y, d2, c1w, c1r,
                                  (* describe line 2 by its Y coordinates.  *)
                                  c2p.y, c2v.y, c2a.y, c2w, c2r,
                                  c1radius+c2radius) ;

   A := array[8] ;
   B := array[7] ;
   C := array[6] ;
   D := array[5] ;
   E := array[4] ;
   F := array[3] ;
   G := array[2] ;
   H := array[1] ;
   I := array[0] ;

   (* now solve for values of t which satisfy:
      At^8 + Bt^7 + Ct^6 + Dt^5 + Et^4 + Ft^3 + Gt^2 + Ht + I = 0  *)
   IF findOctic (A, B, C, D, E, F, G, H, I, t)
   THEN
      T := A*oct(t) + B*sept(t) + C*hex(t) + D*pent(t) + E*quad(t) + F*cub(t) + G*sqr(t) + H*t + I ;
      IF Debugging
      THEN
         printf ("%gt^8 + %gt^7 +%gt^6 + %gt^5 + %gt^4 + %gt^3 + %gt^2 + %gt + %g = %g    (t=%g)\n",
                 A, B, C, D, E, F, G, H, I, T, t)
      END ;
      (* remember tc is -1.0 initially, to force it to be set once.  *)
      IF ((tc<0.0) OR (t<tc)) AND (NOT nearZero (t))
      THEN
         RETURN TRUE
      END
   END ;
   RETURN FALSE
END earlierCircleCollisionOrbiting ;


(*
   maximaCircleCollision -
*)

PROCEDURE maximaCircleCollision (VAR array: ARRAY OF REAL;
                                 a, b, c, d, e, f, g, h, k, l, m, n, o, p: REAL) ;
BEGIN
#  include "circles.m"
END maximaCircleCollision ;


(*
   earlierCircleCollision - let the following abreviations be assigned.
                            Note i is one circle, j is another circle.
                                 v is velocity, a, acceleration, x, y axis.
                                 r is radius.

                            Single letter variables are used since wxmaxima
                            only operates with these.  Thus the output from wxmaxima
                            can be cut and pasted into the program.

                            a = xi
                            b = xj
                            c = vxi
                            d = vxj
                            e = aix
                            f = ajx
                            g = yi
                            h = yj
                            k = vyi
                            l = vyj
                            m = aiy
                            n = ajy
                            o = ri
                            p = rj

                            t                       is the time of this collision (if any)
                            bestTimeOfCollision     is earlier known collision so far.
*)

PROCEDURE earlierCircleCollision (edesc: eventDesc; id1, id2: CARDINAL;
                                  VAR t: REAL; bestTimeOfCollision: REAL; VAR cp: Coord;
                                  a, b, c, d, e, f, g, h, k, l, m, n, o, p: REAL) : BOOLEAN ;
VAR
   A, B, C, D, E, T: REAL ;
   array           : ARRAY [0..4] OF REAL ;
   c1, c2, cp1, cp2,
   v12, r12        : Coord;
BEGIN
   (* thanks to wxmaxima  (expand ; factor ; ratsimp) *)

   A := sqr(n)-2.0*m*n+sqr(m)+sqr(f)-2.0*e*f+sqr(e) ;
   B := (4.0*l-4.0*k)*n+(4.0*k-4.0*l)*m+(4.0*d-4.0*c)*f+(4.0*c-4.0*d)*e ;
   C := (4.0*h-4.0*g)*n+(4.0*g-4.0*h)*m+4.0*sqr(l)-8.0*k*l+4.0*sqr(k)+(4.0*b-4.0*a)*f+(4.0*a-4.0*b)*e+4.0*sqr(d)-8.0*c*d+4.0*sqr(c) ;
   D := (8.0*h-8.0*g)*l+(8.0*g-8.0*h)*k+(8.0*b-8.0*a)*d+(8.0*a-8.0*b)*c ;
   E := 4.0*sqr(h)-8.0*g*h+4.0*sqr(g)+4.0*sqr(b)-8.0*a*b+4.0*sqr(a)-sqr(2.0*(p+o)) ;

   maximaCircleCollision (array,
                          a, b, c, d, e, f, g, h, k, l, m, n, o, p) ;

   AssertRDebug (array[4], A, "A") ;
   AssertRDebug (array[3], B, "B") ;
   AssertRDebug (array[2], C, "C") ;
   AssertRDebug (array[1], D, "D") ;
   AssertRDebug (array[0], E, "E") ;

   (* now solve for values of t which satisfy   At^4 + Bt^3 + Ct^2 + Dt^1 + Et^0 = 0  *)
   IF findQuartic (A, B, C, D, E, t)          (* this function will alter, t.  *)
   THEN
      T := A*(sqr(t)*sqr(t))+B*(sqr(t)*t)+C*sqr(t)+D*t+E ;
      IF Debugging
      THEN
         printf ("%gt^4 + %gt^3 +%gt^2 + %gt + %g = %g    (t=%g)\n",
                 A, B, C, D, E, T, t) ;
         printf ("found collision at %g\n", t)
      END ;
      Assert (t >= 0.0, __LINE__) ;
      (* remember edesc = NIL if bestTimeOfCollision is unassigned.  *)
      IF (edesc=NIL) OR (t<bestTimeOfCollision)
      THEN
         c1 := newPositionCoord (initCoord (a, g),
                                 initCoord (c, k),
                                 initCoord (e, m), t) ;
         c2 := newPositionCoord (initCoord (b, h),
                                 initCoord (d, l),
                                 initCoord (f, n), t) ;
         v12 := subCoord (c1, c2) ;
	 Assert (nearCoord (c1, addCoord (c2, v12)), __LINE__) ;
	 cp2 := addCoord (c2, scaleCoord (v12, o/(o+p))) ;
         cp1 := subCoord (c1, scaleCoord (v12, p/(o+p))) ;
	 cp := cp2 ;

	 IF nearSame (lengthCoord (v12), o+p)
         THEN
	    (*
            printf ("\nc1 = %g, %g\n", c1.x, c1.y) ;
            printf ("c2 = %g, %g\n", c2.x, c2.y) ;
            printf ("o = %g, p = %g\n", o, p) ;
            printf ("cp2 = %g, %g\n", cp2.x, cp2.y) ;
            printf ("v12 = c1 - c2 = %g, %g\n", v12.x, v12.y) ;
	    r12 := scaleCoord (v12, o/(o+p)) ;
	    printf ("r12 = v12 scaled by %g = %g, %g\n", o/(o+p), r12.x, r12.y) ;
            printf ("cp1 = c2 + r12 = %g, %g\n", cp1.x, cp1.y) ;
            *)
            Assert (nearCoord (cp1, cp2), __LINE__) ;
	    (*
	    IF nearCoord (cp1, cp2)
            THEN
               printf ("good cp1 and cp2 are the same\n")
            ELSE
               printf ("fail cp1 and cp2 are not the same\n")
            END ;
            *)
            (* Assert (nearCoord (cp1, cp2), __LINE__) ; *)
            (* found a value of t which is better than bestTimeOfCollision, but it might be a duplicate collision.  *)
            IF NOT isDuplicate (currentTime, t, id1, id2, edge, edge, cp)
            THEN
               (* ok, this has not been seen before.  *)
               RETURN TRUE
            END
         ELSE
            printf ("false the collisions points do not touch ignoring = %g, %g\n", cp.x, cp.y)
         END
      END
   END ;
   RETURN FALSE
END earlierCircleCollision ;


(*
   findCollisionCircles -

   using:

   S = UT + (AT^2)/2
   compute xin and yin which are the new (x,y) positions of object i at time, t.
   compute xjn and yjn which are the new (x,y) positions of object j at time, t.
   now compute difference between objects and if they are ri+rj  (radius of circle, i, and, j)
   apart then we have a collision at time, t.

   xin = xi + vxi * t + (aix * t^2) / 2.0
   yin = yi + vyi * t + (aiy * t^2) / 2.0

   xjn = xj + vxj * t + (ajx * t^2) / 2.0
   yjn = yj + vyj * t + (ajy * t^2) / 2.0

   ri + rj == sqrt(abs(xin-xjn)^2 + abs(yin-yjn)^2)     for values of t

   ri + rj == sqrt(((xi + vxi * t + aix * t^2 / 2.0) - (xj + vxj * t + ajx * t^2 / 2.0))^2 +
                   ((yi + vyi * t + aiy * t^2 / 2.0) - (yj + vyj * t + ajy * t^2 / 2.0))^2)

   let:

   a = xi
   b = xj
   c = vxi
   d = vxj
   e = aix
   f = ajx
   g = yi
   h = yj
   k = vyi
   l = vyj
   m = aiy
   n = ajy
   o = ri
   p = rj
   t = t

   o  + p  == sqrt(((a  + c   * t + e   * t^2 / 2.0) - (b  + d   * t +   f * t^2 / 2.0))^2 +
                   ((g  + k   * t + m   * t^2 / 2.0) - (h  + l   * t +   n * t^2 / 2.0))^2)

   o  + p  == sqrt(((a  + c   * t + e   * t^2 / 2.0) - (b  + d   * t +   f * t^2 / 2.0))^2 +
                   ((g  + k   * t + m   * t^2 / 2.0) - (h  + l   * t +   n * t^2 / 2.0))^2)

   0       == ((a  + c   * t + e   * t^2 / 2.0) - (b  + d   * t +   f * t^2 / 2.0))^2 +
              ((g  + k   * t + m   * t^2 / 2.0) - (h  + l   * t +   n * t^2 / 2.0))^2 -
              (o  + p)^2

   now using wxmaxima
   expand ; factor ; ratsimp

   p+o    ==  (sqrt((n^2-2*m*n+m^2+f^2-2*e*f+e^2)*t^4+
                   ((4*l-4*k)*n+(4*k-4*l)*m+(4*d-4*c)*f+(4*c-4*d)*e)*t^3+
                   ((4*h-4*g)*n+(4*g-4*h)*m+4*l^2-8*k*l+4*k^2+(4*b-4*a)*f+(4*a-4*b)*e+4*d^2-8*c*d+4*c^2)*t^2+
                   ((8*h-8*g)*l+(8*g-8*h)*k+(8*b-8*a)*d+(8*a-8*b)*c)*t+4*h^2-8*g*h+4*g^2+4*b^2-8*a*b+4*a^2))/2

   2*(p+o) ==  (sqrt((n^2-2*m*n+m^2+f^2-2*e*f+e^2)*t^4+
                    ((4*l-4*k)*n+(4*k-4*l)*m+(4*d-4*c)*f+(4*c-4*d)*e)*t^3+
                    ((4*h-4*g)*n+(4*g-4*h)*m+4*l^2-8*k*l+4*k^2+(4*b-4*a)*f+(4*a-4*b)*e+4*d^2-8*c*d+4*c^2)*t^2+
                    ((8*h-8*g)*l+(8*g-8*h)*k+(8*b-8*a)*d+(8*a-8*b)*c)*t+4*h^2-8*g*h+4*g^2+4*b^2-8*a*b+4*a^2))

   (2*(p+o))^2 == ((n^2-2*m*n+m^2+f^2-2*e*f+e^2)*t^4+
                   ((4*l-4*k)*n+(4*k-4*l)*m+(4*d-4*c)*f+(4*c-4*d)*e)*t^3+
                   ((4*h-4*g)*n+(4*g-4*h)*m+4*l^2-8*k*l+4*k^2+(4*b-4*a)*f+(4*a-4*b)*e+4*d^2-8*c*d+4*c^2)*t^2+
                   ((8*h-8*g)*l+(8*g-8*h)*k+(8*b-8*a)*d+(8*a-8*b)*c)*t+4*h^2-8*g*h+4*g^2+4*b^2-8*a*b+4*a^2))

   0           ==  (n^2-2*m*n+m^2+f^2-2*e*f+e^2)*t^4+
                   ((4*l-4*k)*n+(4*k-4*l)*m+(4*d-4*c)*f+(4*c-4*d)*e)*t^3+
                   ((4*h-4*g)*n+(4*g-4*h)*m+4*l^2-8*k*l+4*k^2+(4*b-4*a)*f+(4*a-4*b)*e+4*d^2-8*c*d+4*c^2)*t^2+
                   ((8*h-8*g)*l+(8*g-8*h)*k+(8*b-8*a)*d+(8*a-8*b)*c)*t+
                   4*h^2-8*g*h+4*g^2+4*b^2-8*a*b+4*a^2)-
                   ((2*(p+o))^2)

   solve polynomial:

   A := sqr(n)-2.0*m*n+sqr(m)+sqr(f)-2.0*e*f+sqr(e) ;
   B := (4.0*l-4.0*k)*n+(4.0*k-4.0*l)*m+(4.0*d-4.0*c)*f+(4.0*c-4.0*d)*e ;
   C := (4.0*h-4.0*g)*n+(4.0*g-4.0*h)*m+4.0*sqr(l)-8.0*k*l+4.0*sqr(k)+(4.0*b-4.0*a)*f+(4.0*a-4.0*b)*e+4.0*sqr(d)-8.0*c*d+4.0*sqr(c) ;
   D := (8.0*h-8.0*g)*l+(8.0*g-8.0*h)*k+(8.0*b-8.0*a)*d+(8.0*a-8.0*b)*c ;
   E := 4.0*sqr(h)-8.0*g*h+4.0*sqr(g)+4.0*sqr(b)-8.0*a*b+4.0*sqr(a)-sqr(2.0*(p+o)) ;
*)

PROCEDURE findCollisionCircles (iptr, jptr: Object; VAR edesc: eventDesc; VAR tc: REAL) ;
VAR
   a, b, c, d, e,
   f, g, h, k, l,
   m, n, o, p, t: REAL ;
   i, j         : CARDINAL ;
   T            : REAL ;
   cp           : Coord ;
BEGIN
(*
   DumpObject(iptr) ;
   DumpObject(jptr) ;
*)
   (*
        a        xi
        g        yi
        o        ri
        c        vxi
        k        vyi
        e        axi
        m        ayi
    *)
   getCircleValues (iptr, a, g, o, c, k, e, m) ;

   (*
        b         xj
        h         yj
        p         rj
        d         vxj
        l         vyj
        f         ajx
        n         ajy
   *)

   getCircleValues (jptr, b, h, p, d, l, f, n) ;
   IF earlierCircleCollision (edesc, iptr^.id, jptr^.id,
                              t, tc, cp,
                              a, b, c, d, e, f, g, h, k, l, m, n, o, p)
   THEN
      tc := t ;
      edesc := makeCirclesDesc (edesc, iptr^.id, jptr^.id, cp)
   END
END findCollisionCircles ;


(*
   stop -
*)

PROCEDURE stop ;
BEGIN
END stop ;


(*
   makeCirclesPolygonDesc - returns an eventDesc describing the collision between a circle and a polygon.
*)

PROCEDURE makeCirclesPolygonDesc (edesc: eventDesc; cid, pid: CARDINAL;
                                  lineNo, pointNo: CARDINAL;
                                  wpid1, wpid2: whereHit;
                                  collisionPoint: Coord) : eventDesc ;
BEGIN
   Assert (wpid1=corner, __LINE__) ;  (* circle must always be treated as corner.  *)
   IF edesc=NIL
   THEN
      edesc := newDesc ()
   END ;
   WITH edesc^ DO
      edesc^.etype := circlePolygonEvent ;
      edesc^.cp.pid := pid ;
      edesc^.cp.cid := cid ;
      edesc^.cp.cPoint := collisionPoint ;
      edesc^.cp.wpid := wpid2 ;
      edesc^.cp.lineNo := lineNo ;
      edesc^.cp.pointNo := pointNo
   END ;
   RETURN edesc
END makeCirclesPolygonDesc ;


(*
   checkIfPointHits - return TRUE if t0 is either the first hit found or
                      is sooner than, tc.  It determines a hit by working out
                      the final position of partical:

                      x = s + ut + 1/2a t^2

                      if x>=0.0 and x <= length then it hits.
*)

PROCEDURE checkIfPointHits (timeOfPrevCollision: REAL; t, length: REAL; s, u, a: REAL) : BOOLEAN ;
VAR
   x: REAL ;
BEGIN
   (* if t is later than timeOfPrevCollision, then we don't care as we already have found an earlier hit.  *)
   IF trace
   THEN
      printf ("current best collision time is %g the new collision exists at time %g\n", timeOfPrevCollision, t)
   END ;
   IF (timeOfPrevCollision=-1.0) OR (t<timeOfPrevCollision)
   THEN
      (* at time, t, what is the value of x ? *)
      x := newPositionScalar (s, u, a, t) ;

      IF trace
      THEN
         printf ("line 0.0 .. %g  and point at %g ", length, x)
      END ;

      (* if x lies between 0 .. length then it hits! *)
      IF (x>=0.0) AND (x<=length)
      THEN
         (* new earlier collision time found *)
         IF trace
         THEN
            printf ("will hit line\n")
         END ;
         RETURN TRUE
      END ;
      IF trace
      THEN
         printf ("misses line\n")
      END
   ELSIF trace
   THEN
      printf ("the new time %g should be ignored as the best is %g\n", t, timeOfPrevCollision)
   END ;

   RETURN FALSE
END checkIfPointHits ;


(*
   newPositionScalar - calculates the new position of a scalar in the future.
*)

PROCEDURE newPositionScalar (s, u, a, t: REAL) : REAL ;
BEGIN
   RETURN s + u*t + a * (t*t) / 2.0
END newPositionScalar ;


(*
   newPositionRotationSinScalar - works out the new Y position for a point whose:

                                  current cofg Y position is:   c
                                  initial Y velocity is     :   u
                                  Y acceleration is         :   a
                                  angular velocity          :   w
                                  orientation               :   o
                                  polar coord position rel
                                  to cofg is                :   p
*)

PROCEDURE newPositionRotationSinScalar (c, u, a, t, w, o: REAL; p: Polar) : REAL ;
VAR
   O: REAL ;
BEGIN
   IF Debugging
   THEN
      printf ("c = %g, u = %g, a = %g, t = %g\n", c, u, a, t)
   END ;
   O := newPositionScalar(c, u, a, t) ;
   IF Debugging
   THEN
      printf ("O = %g, p.r = %g, p.w = %g, sin (w*t + o + p.w) = %g\n",
              O, p.r, p.w, sin (w*t + o + p.w))
   END ;
   RETURN O + p.r * sin (w*t + o + p.w)
END newPositionRotationSinScalar ;


(*
   newPositionRotationCosScalar - works out the new X position for a point whose:

                                  current cofg X position is:   c
                                  initial X velocity is     :   u
                                  X acceleration is         :   a
                                  angular velocity          :   w
                                  orientation               :   o
                                  polar coord position rel
                                  to cofg is                :   p
*)

PROCEDURE newPositionRotationCosScalar (c, u, a, t, w, o: REAL; p: Polar) : REAL ;
VAR
   O: REAL ;
BEGIN
   O := newPositionScalar (c, u, a, t) ;
   RETURN O + p.r * cos (w*t + o + p.w)
END newPositionRotationCosScalar ;


(*
   newPositionCoord - calculates the new position of point in the future.
*)

PROCEDURE newPositionCoord (c, u, a: Coord; t: REAL) : Coord ;
BEGIN
   RETURN initCoord (newPositionScalar (c.x, u.x, a.x, t),
                     newPositionScalar (c.y, u.y, a.y, t))
END newPositionCoord ;


(*
   newPositionRotationCoord - calculates the new position of point, c+v, in the future.
                              Given angular velocity         : w
                                    orientation              : o
                                    time                     : t
                                    initial vel              : u
                                    accel                    : a
                                    c of g                   : c
                                    polar coord of the point : p
*)

PROCEDURE newPositionRotationCoord (c, u, a: Coord; t, w, o: REAL; p: Polar) : Coord ;
BEGIN
   IF Debugging
   THEN
      printf ("t = %g, w = %g, o = %g\n", t, w, o)
   END ;
   RETURN initCoord (newPositionRotationCosScalar (c.x, u.x, a.x, t, w, o, p),
                     newPositionRotationSinScalar (c.y, u.y, a.y, t, w, o, p))
END newPositionRotationCoord ;


(*
   hLine - debugging procedure to display a line on a half scale axis.
*)

PROCEDURE hLine (p1, p2: Coord; c: Colour) ;
VAR
   p: ARRAY [0..1] OF Point ;
BEGIN
   p1 := scaleCoord (p1, 0.5) ;
   p2 := scaleCoord (p2, 0.5) ;
   p1 := addCoord (p1, initCoord (0.5, 0.5)) ;
   p2 := addCoord (p2, initCoord (0.5, 0.5)) ;
   p[0] := c2p (p1) ;
   p[1] := c2p (p2) ;
   glyphPolygon (2, p, FALSE, initFract (0, 1, 16), c)
END hLine ;


(*
   hPoint - debugging procedure to display a line on a half scale axis.
*)

PROCEDURE hPoint (p: Coord; c: Colour) ;
BEGIN
   p := scaleCoord (p, 0.5) ;
   p := addCoord (p, initCoord(0.5, 0.5)) ;
   glyphCircle (c2p (p), TRUE, zero (), putReal (0.05), c)
END hPoint ;


(*
   hCircle - debugging procedure to display a circle on a half scale axis.
*)

PROCEDURE hCircle (p: Coord; r: REAL; c: Colour) ;
BEGIN
   p := scaleCoord (p, 0.5) ;
   p := addCoord (p, initCoord (0.5, 0.5)) ;
   glyphCircle (c2p (p), TRUE, zero (), putReal (r), c)
END hCircle ;


(*
   hVec - display a normalised vector on a half scale axis
*)

PROCEDURE hVec (p: Coord; c: Colour) ;
BEGIN
   p := normaliseCoord (p) ;
   hLine (initCoord (0.0, 0.0), initCoord (p.x, 0.0), c) ;
   hLine (initCoord (0.0, 0.0), initCoord (0.0, p.y), c)
END hVec ;


(*
   hFlush - flip the debugging buffer.
*)

PROCEDURE hFlush ;
BEGIN
   frameNote ;
   drawBoarder (black ()) ;
   flipBuffer ;
   collectAll
END hFlush ;


(*
   checkPointCollision -
*)

PROCEDURE checkPointCollision (VAR timeOfPrevCollision: REAL; t, length, cx, rvx, rax: REAL;
                               c, cvel, caccel: Coord; VAR collisionPoint: Coord;
                               id1, id2: CARDINAL) : BOOLEAN ;
BEGIN
   IF trace
   THEN
      printf ("entering checkPointCollision with the current best time %g\n", timeOfPrevCollision)
   END ;
   IF checkIfPointHits (timeOfPrevCollision, t, length, cx, rvx, rax)
   THEN
      IF trace
      THEN
         printf ("it crosses the region of interest (current best time %g)\n", timeOfPrevCollision)
      END ;

      (* a hit, find where *)
      collisionPoint := newPositionCoord (c, cvel, caccel, t) ;

      (* return TRUE providing that we do not already know about it *)
      IF isDuplicate (currentTime, t, id1, id2, edge, edge, collisionPoint)
      THEN
         IF trace
         THEN
            printf ("but it is a duplicate (best time still %g)\n", timeOfPrevCollision)
         END ;

         RETURN FALSE
      ELSE
         IF trace
         THEN
            printf ("point hits line, new best time of collision (%g)\n", t)
         END ;

         timeOfPrevCollision := t ;
         RETURN TRUE
      END
   ELSE
      IF trace
      THEN
         printf ("the point missed the line (best time is still %g)\n", timeOfPrevCollision)
      END
   END ;
   RETURN FALSE
END checkPointCollision ;


(*
   earlierPointLineCollision - returns TRUE if we can find a collision between a point, c,
                               travelling at cvel with acceleration, caccel and a line
                               p1, p2, travelling at velocity, lvel, and acceleration laccel.
                               If a collision is found then the collisionPoint is also
                               calculated.
*)

PROCEDURE earlierPointLineCollision (VAR timeOfCollision: REAL;
                                     c, cvel, caccel,
                                     p1, p2, lvel, laccel: Coord;
                                     VAR collisionPoint: Coord;
                                     id1, id2: CARDINAL) : BOOLEAN ;
VAR
   p3, c0, c1,
   rvel, raccel: Coord ;
   x,
   t0, t1,
   hypot, theta: REAL ;
BEGIN
   IF trace
   THEN
      printf ("earlierPointLineCollision entered and best time is %g\n", timeOfCollision)
   END ;

   (* we pretend that the line is stationary, by computing the relative velocity and acceleration *)
   rvel := subCoord (cvel, lvel) ;
   raccel := subCoord (caccel, laccel) ;
   IF trace
   THEN
      printf ("relative vel  (%g, %g),  accel (%g, %g)\n", rvel.x, rvel.y, raccel.x, raccel.y)
   END ;

   (* now translate p1 onto the origin *)
   p3 := subCoord (p2, p1) ;
   hypot := lengthCoord (p3) ;
   (* we have a line from 0, 0 to hypot, 0 *)

   (* now find theta the angle of the vector, p3 *)
   theta := asin (p3.y / hypot) ;
   IF trace
   THEN
      printf ("rotating line by %g degrees  (length of line is %g)\n", 180.0*theta/3.14159, hypot)
   END ;

   c0 := subCoord (c, p1) ;             (* translate c by the same as the line *)
   c1 := rotateCoord (c0, -theta) ;     (* and rotate point, c0. *)
   rvel := rotateCoord (rvel, -theta) ; (* and relative velocity *)
   raccel := rotateCoord (raccel, -theta) ; (* and relative acceleration *)

   raccel := checkZeroCoord (raccel) ;
   rvel := checkZeroCoord (rvel) ;
   IF trace
   THEN
      printf ("after rotation we have relative vel  (%g, %g),  accel (%g, %g)\n", rvel.x, rvel.y, raccel.x, raccel.y)
   END ;

   IF FALSE
   THEN
      hLine(initCoord(0.0, 0.0), initCoord(hypot, 0.0), purple()) ;
      hPoint(c1, purple()) ;
      hFlush()
   END ;

   (*
      now solve for, t, when y=0, use S = UT + 1/2 AT^2
      at y = 0 we have:

      0 = rvel.y * t + 1/2 * raccel.y * t^2

      Using quadratic:

      at^2 + bt + c = 0
   *)
   IF findQuadratic (raccel.y / 2.0, rvel.y, c1.y, t0, t1)
   THEN
      IF trace
      THEN
         printf ("earlierPointLineCollision after findQuadratic and best time is %g\n", timeOfCollision)
      END ;

      IF (t0<0.0) AND (t1<0.0)
      THEN
         IF trace
         THEN
            printf ("the point never crosses the line in the future\n")
         END ;
         (* get out of here quick - no point of predicting collisions in the past :-) *)
         RETURN FALSE
      ELSE
         IF t0=t1
         THEN
            IF trace
            THEN
               printf ("the point crosses the line once\n")
            END ;

            (* only one root *)
            IF checkPointCollision (timeOfCollision, t0, hypot, c1.x, rvel.x, raccel.x,
                                    c, cvel, caccel, collisionPoint, id1, id2)
            THEN
               RETURN TRUE
            END
         ELSE
            (* two roots, ignore a negative root *)
            IF t0<0.0
            THEN
               IF trace
               THEN
                  printf ("the point crosses the line once in the future and once in the past\n") ;
                  printf ("only examining root %g, remember best is %g\n", t1, timeOfCollision)
               END ;

               (* test only positive root, t1 *)
               IF checkPointCollision (timeOfCollision, t1, hypot, c1.x, rvel.x, raccel.x,
                                       c, cvel, caccel, collisionPoint, id1, id2)
               THEN
                  RETURN TRUE
               END
            ELSIF t1<0.0
            THEN
               IF trace
               THEN
                  printf ("the point crosses the line once in the future and once in the past\n")
               END ;

               (* test only positive root, t0 *)
               IF checkPointCollision (timeOfCollision, t0, hypot, c1.x, rvel.x, raccel.x,
                                       c, cvel, caccel, collisionPoint, id1, id2)
               THEN
                  RETURN TRUE
               END
            ELSE
               IF trace
               THEN
                  printf ("the point crosses the line twice in the future\n")
               END ;

               (* ok two positive roots, test smallest (earlist first and then bail out if it hits) *)
               IF t0<t1
               THEN
                  IF checkPointCollision (timeOfCollision, t0, hypot, c1.x, rvel.x, raccel.x,
                                          c, cvel, caccel, collisionPoint, id1, id2)
                  THEN
                     IF trace
                     THEN
                        printf ("the point crosses the line first time in correct place\n")
                     END ;
                     RETURN TRUE
                  END ;
                  IF checkPointCollision (timeOfCollision, t1, hypot, c1.x, rvel.x, raccel.x,
                                          c, cvel, caccel, collisionPoint, id1, id2)
                  THEN
                     IF trace
                     THEN
                        printf ("the point crosses the line, first time misses and second time hits\n")
                     END ;
                     RETURN TRUE
                  END
               ELSE
                  IF checkPointCollision (timeOfCollision, t1, hypot, c1.x, rvel.x, raccel.x,
                                          c, cvel, caccel, collisionPoint, id1, id2)
                  THEN
                     IF trace
                     THEN
                        printf ("the point crosses the line first time in correct place\n")
                     END ;
                     RETURN TRUE
                  END ;
                  IF checkPointCollision (timeOfCollision, t0, hypot, c1.x, rvel.x, raccel.x,
                                          c, cvel, caccel, collisionPoint, id1, id2)
                  THEN
                     IF trace
                     THEN
                        printf ("the point crosses the line, first time misses and second time hits\n")
                     END ;
                     RETURN TRUE
                  END
               END
            END
         END
      END
   END ;

   IF trace
   THEN
      printf ("this point and line should be discarded\n")
   END ;
   RETURN FALSE
END earlierPointLineCollision ;


(*
   sortLine - orders points, p1 and, p2, according to their x value.
*)

PROCEDURE sortLine (VAR p1, p2: Coord) ;
VAR
   t: Coord ;
BEGIN
   IF p1.x>p2.x
   THEN
      t := p1 ;
      p1 := p2 ;
      p2 := t
   ELSIF (p1.x=p2.x) AND (p1.y>p2.y)
   THEN
      t := p1 ;
      p1 := p2 ;
      p2 := t
   END
END sortLine ;


(*
   earlierCircleEdgeCollision - return TRUE if an earlier time, t, is found than tc for when circle
                                hits a line.  The circle is defined by a, center, radius and has
                                acceleration, accelCircle, and velocity, velCircle.
                                The line is between p1 and p2 and has velocity, velLine, and
                                acceleration, accelLine.
*)

PROCEDURE findEarlierCircleEdgeCollision (VAR timeOfCollision: REAL;
                                          cid, pid: CARDINAL;
                                          lineP, lineC: CARDINAL;
                                          VAR edesc: eventDesc;
                                          center: Coord; radius: REAL;
                                          velCircle, accelCircle: Coord;
                                          p1, p2, velLine, accelLine: Coord;
                                          createDesc: descP) ;
VAR
   v1, n1, d1, d2,
   p3, p4, p5, p6: Coord ;
   collisonPoint : Coord ;
BEGIN
   sortLine (p1, p2) ;
   (* create the vector p1 -> p2.  *)
   v1 := subCoord (p2, p1) ;

   (* compute the normal for v1, normalise it, and multiply by radius.  *)
   perpendiculars (v1, d1, d2) ;
   d1 := scaleCoord (normaliseCoord (d1), radius) ;
   d2 := scaleCoord (normaliseCoord (d2), radius) ;

   (* now add d1, d2 to p1 to obtain p3, p4.  *)
   p3 := addCoord (p1, d1) ;
   p4 := addCoord (p1, d2) ;

   (* now add d1 and d2 to p2 to get p5 and p6.  *)
   p5 := addCoord (p2, d1) ;
   p6 := addCoord (p2, d2) ;

   (* we now have two lines p3 -> p5   and  p4 -> p6.  *)

   (* ok, now we only need to find when line between p3, p5 hits the centre of the circle.  *)
   IF earlierPointLineCollision (timeOfCollision, center, velCircle, accelCircle,
                                 p3, p5, velLine, accelLine, collisonPoint, cid, pid)
   THEN
      (* circle hits line, p1, in tc seconds.  *)
      IF Debugging
      THEN
         printf ("circle hits line (%g, %g) (%g, %g) in %g\n", p1.x, p1.y, p2.x, p2.y, timeOfCollision)
      END ;
      edesc := createDesc (edesc, cid, pid, lineP, lineC, corner, edge, collisonPoint) ;
      IF drawPrediction
      THEN
         frameNote ;
         drawFrame (NIL) ;
         debugCircle (center, 0.02, white ()) ;
         debugLine (p3, p5, yellow ()) ;
         debugCircle (collisonPoint, 0.02, white ()) ;
         flipBuffer ;
         collectAll
      END
   END ;

   (* ok, now we only need to find when line between p4, p6 hits the centre of the circle.  *)
   IF earlierPointLineCollision (timeOfCollision, center, velCircle, accelCircle,
                                 p4, p6, velLine, accelLine, collisonPoint, cid, pid)
   THEN
      (* circle hits line, p1, in tc seconds *)
      IF Debugging
      THEN
         printf ("circle hits line (%g, %g) (%g, %g) in %g\n", p1.x, p1.y, p2.x, p2.y, timeOfCollision)
      END ;
      edesc := createDesc (edesc, cid, pid, lineP, lineC, corner, edge, collisonPoint) ;
      IF drawPrediction
      THEN
         frameNote ;
         drawFrame (NIL) ;
         debugCircle (center, 0.02, white ()) ;
         debugLine (p4, p6, yellow ()) ;
         debugCircle (collisonPoint, 0.02, white ()) ;
         flipBuffer ;
         collectAll
      END
   END
END findEarlierCircleEdgeCollision ;


(*
   getPolygonCoord -
*)

PROCEDURE getPolygonCoord (pPtr: Object; pointno: CARDINAL) : Coord ;
BEGIN
   WITH pPtr^ DO
      RETURN checkZeroCoord (addCoord (p.cOfG, polarToCoord (rotatePolar (p.points[pointno], angleOrientation))))
   END
END getPolygonCoord ;


(*
   getPolygonLine - assigns, p1, p2, with the, line, coordinates of polygon, pPtr.
*)

PROCEDURE getPolygonLine (line: CARDINAL; pPtr: Object; VAR c1, c2: Coord) ;
BEGIN
   WITH pPtr^ DO
      c1 := getPolygonCoord (pPtr, line-1) ;
      IF line=pPtr^.p.nPoints
      THEN
         c2 := getPolygonCoord (pPtr, 0)
      ELSE
         c2 := getPolygonCoord (pPtr, line)
      END
   END
END getPolygonLine ;


(*
   findCollisionCircleLine - find the time (if any) between line number, lineP, in polygon, pPtr,
                             and the circle, cPtr.  cPtr can also be a polygon in which case lineC
                             is the particular line under question.  Line on line collision is broken
                             down into circle line calls which allows for code reuse.

                             Neither cPtr or pPtr are orbiting.
*)

PROCEDURE findCollisionCircleLine (cPtr, pPtr: Object;
                                   lineP, lineC: CARDINAL; center: Coord; radius: REAL;
                                   VAR edesc: eventDesc; VAR timeOfCollision: REAL; createDesc: descP) ;
VAR
   velCircle, accelCircle,
   velLine, accelLine,
   p1, p2, cp                   : Coord ;
   cx, cy, r, cvx, cvy, cax, cay,
   pvx, pvy, pax, pay, t        : REAL ;
   cid, pid                     : CARDINAL ;
BEGIN
   cid := cPtr^.id ;
   pid := pPtr^.id ;

   getPolygonLine (lineP, pPtr, p1, p2) ;

   (* we perform 4 checks.

         (i) and (ii)     pretend the circle has radius 0.0 and see if it hits two new circles at
                          point, p1, and, p2 with the original radius.
         (iii) and (iv)   now draw two lines between the edge of the two new circles and see if the
                          center of the original circle intersects with either line.

         the smallest positive time is the time of the next collision.
    *)

   getObjectValues (cPtr, cvx, cvy, cax, cay) ;
   getObjectValues (pPtr, pvx, pvy, pax, pay) ;

   (* i *)
   IF earlierCircleCollision (edesc, cid, pid,
                              t, timeOfCollision, cp,
                              p1.x, center.x, pvx, cvx, pax, cax,
                              p1.y, center.y, pvy, cvy, pay, cay, radius, 0.0)
   THEN
      (* circle hits corner of the line, p1, in tc seconds.  *)
      IF Debugging
      THEN
         printf ("circle hits corner at %g, %g  in %g\n", p1.x, p1.y, t)
      END ;
      timeOfCollision := t ;
      edesc := createDesc (edesc, cid, pid, lineP, lineC, corner, corner, p1) ;  (* point no, lineC.  *)
      IF drawPrediction
      THEN
         frameNote ;
         drawFrame (NIL) ;
         debugCircle (center, 0.02, white ()) ;
         debugCircle (p1, 0.02, white ()) ;
         flipBuffer ;
         collectAll
      END
   END ;

   (* ii *)
   IF earlierCircleCollision (edesc, cid, pid,
                              t, timeOfCollision, cp,
                              p2.x, center.x, pvx, cvx, pax, cax,
                              p2.y, center.y, pvy, cvy, pay, cay, radius, 0.0)
   THEN
      (* circle hits corner of the line, p2, in tc seconds.  *)
      IF Debugging
      THEN
         printf ("circle hits corner at %g, %g  in %g  (lineP+1)\n", p2.x, p2.y, t)
      END ;
      timeOfCollision := t ;
      edesc := createDesc (edesc, cid, pid, lineP+1, lineC, corner, corner, p2) ;  (* point no, lineP+1.  *)
      IF drawPrediction
      THEN
         frameNote ;
         drawFrame (NIL) ;
         debugCircle (cPtr^.c.pos, 0.02, white ()) ;
         debugCircle (p2, 0.02, white ()) ;
         flipBuffer ;
         collectAll
      END
   END ;

   velCircle   := initCoord (cvx, cvy) ;
   accelCircle := initCoord (cax, cay) ;
   velLine     := initCoord (pvx, pvy) ;
   accelLine   := initCoord (pax, pay) ;

   (* iii and iv *)
   findEarlierCircleEdgeCollision (timeOfCollision,
                                   cid, pid,
                                   lineP, lineC, edesc,
                                   center, radius, velCircle, accelCircle,
                                   p1, p2, velLine, accelLine, createDesc)

END findCollisionCircleLine ;


(*
   findCollisionCircleLineOrbiting - find the time (if any) between line number, lineP, in polygon, pPtr,
                                     and the circle, cPtr.  cPtr can also be a polygon in which case lineC
                                     is the particular line under question.  Line on line collision is broken
                                     down into circle line calls which allows for code reuse.

                                     Either cPtr or pPtr or both are orbiting.
*)

PROCEDURE findCollisionCircleLineOrbiting (cPtr, pPtr: Object;
                                           lineP, lineC: CARDINAL; center: Coord; radius: REAL;
                                           VAR edesc: eventDesc; VAR timeOfCollision: REAL; createDesc: descP) ;
VAR
   t,
   cr, cw,
   pr, pw                       : REAL ;
   cv, ca, ccofg,
   pv, pa, pcofg,
   p1, p2                       : Coord ;
   cid, pid                     : CARDINAL ;
BEGIN
   cid := cPtr^.id ;
   pid := pPtr^.id ;

   getPolygonLine (lineP, pPtr, p1, p2) ;

   (* we perform 4 checks.

         (i) and (ii)     pretend the circle has radius 0.0 and see if it hits two new circles at
                          point, p1, and, p2 with the original radius.
         (iii) and (iv)   now draw two lines between the edge of the two new circles and see if the
                          center of the original circle intersects with either line.

         the smallest positive time is the time of the next collision.
    *)

   getObjectValues (cPtr, cv.x, cv.y, ca.x, ca.y) ;
   getObjectValues (pPtr, pv.x, pv.y, pa.x, pa.y) ;
   getObjectOrbitingValues (cPtr, cr, cw, ccofg) ;
   getObjectOrbitingValues (pPtr, pr, pw, pcofg) ;

   (* i *)
   IF earlierCircleCollisionOrbiting (t, timeOfCollision,
                                      center, radius, cr, cw, ccofg, cv, ca,
                                      p1, 0.0, pr, pw, pcofg, pv, pa)
   THEN
      (* circle hits corner of the line, p1, in tc seconds.  *)
      IF Debugging
      THEN
         printf ("circle hits corner at %g, %g  in %g seconds\n", p1.x, p1.y, t)
      END ;
      timeOfCollision := t ;
      edesc := createDesc (edesc, cid, pid, lineP, lineC, corner, corner, p1) ;  (* point no, lineC.  *)
      IF drawPrediction
      THEN
         frameNote ;
         drawFrame (NIL) ;
         debugCircle (center, 0.02, white ()) ;
         debugCircle (p1, 0.02, white ()) ;
         flipBuffer ;
         collectAll
      END
   END ;

   (* ii *)
   IF earlierCircleCollisionOrbiting (t, timeOfCollision,
                                      center, radius, cr, cw, ccofg, cv, ca,
                                      p2, 0.0, pr, pw, pcofg, pv, pa)
   THEN
      (* circle hits corner of the line, p2, in tc seconds.  *)
      IF Debugging
      THEN
         printf ("circle hits corner at %g, %g  in %g seconds  (lineP+1)\n", p2.x, p2.y, t)
      END ;
      timeOfCollision := t ;
      edesc := createDesc (edesc, cid, pid, lineP+1, lineC, corner, corner, p2) ;  (* point no, lineP+1.  *)
      IF drawPrediction
      THEN
         frameNote ;
         drawFrame (NIL) ;
         debugCircle (cPtr^.c.pos, 0.02, white ()) ;
         debugCircle (p2, 0.02, white ()) ;
         flipBuffer ;
         collectAll
      END
   END ;

(* got to here
   (* iii and iv *)
   findEarlierCircleEdgeCollisionOrbiting (timeOfCollision,
                                           cid, pid,
                                           lineP, lineC, edesc,
                                           center, radius, cv, ca, cr, cw,
                                           p1, p2, pv, pa, pr, pw, createDesc)
*)

END findCollisionCircleLineOrbiting ;


(*
   findCollisionCirclePolygon - find the smallest positive time (if any) between the polygon and circle.
                                If a collision if found then, tc, is assigned to the time and cid, pid
                                are set to the circle id and polygon id respectively.
*)

PROCEDURE findCollisionCirclePolygon (cPtr, pPtr: Object; VAR edesc: eventDesc; VAR tc: REAL) ;
VAR
   i: CARDINAL ;
BEGIN
   Assert (cPtr^.object=circleOb, __LINE__) ;
   Assert (pPtr^.object=polygonOb, __LINE__) ;
   IF isOrbiting (pPtr)
   THEN
      FOR i := 1 TO pPtr^.p.nPoints DO
         findCollisionCircleLineOrbiting (cPtr, pPtr, i, 0, cPtr^.c.pos, cPtr^.c.r, edesc, tc, makeCirclesPolygonDesc)
      END
   ELSE
      FOR i := 1 TO pPtr^.p.nPoints DO
         findCollisionCircleLine (cPtr, pPtr, i, 0, cPtr^.c.pos, cPtr^.c.r, edesc, tc, makeCirclesPolygonDesc)
      END
   END
END findCollisionCirclePolygon ;


(*
   makePolygonPolygonDesc - return a new eventDesc indicating that we have a polygon/polygon collision
                            event.
*)

PROCEDURE makePolygonPolygon (edesc: eventDesc; id1, id2: CARDINAL;
                              lineCorner1, lineCorner2: CARDINAL;
                              wpid1, wpid2: whereHit;
                              collisionPoint: Coord) : eventDesc ;
BEGIN
   IF edesc=NIL
   THEN
      edesc := newDesc ()
   END ;
   edesc^.etype := polygonPolygonEvent ;
   edesc^.pp.cPoint := collisionPoint ;
   edesc^.pp.pid1 := id1 ;
   edesc^.pp.pid2 := id2 ;
   edesc^.pp.wpid1 := wpid1 ;
   edesc^.pp.wpid2 := wpid2 ;
   edesc^.pp.lineCorner1 := lineCorner1 ;
   edesc^.pp.lineCorner2 := lineCorner2 ;
   RETURN edesc
END makePolygonPolygon ;


(*
   isOrbiting - return TRUE if object, o, is rotating.
*)

PROCEDURE isOrbiting (o: Object) : BOOLEAN ;
BEGIN
   RETURN NOT nearZero (o^.angularVelocity)
END isOrbiting ;


(*
   findCollisionLineLine - find the smallest time in the future when two lines collide.
                           The event descriptor, edesc, will contain the description of the collision
                           and, tc, the time of collision in the future.
*)

PROCEDURE findCollisionLineLine (iPtr, jPtr: Object; iLine, jLine: CARDINAL; VAR edesc: eventDesc; VAR tc: REAL) ;
BEGIN
   IF isOrbiting (iPtr) OR isOrbiting (jPtr)
   THEN
      findCollisionLineLineOrbiting (iPtr, jPtr, iLine, jLine, edesc, tc)
   ELSE
      findCollisionLineLineNonOrbiting (iPtr, jPtr, iLine, jLine, edesc, tc)
   END
END findCollisionLineLine ;



PROCEDURE findCollisionLineLineOrbiting (iPtr, jPtr: Object; iLine, jLine: CARDINAL; VAR edesc: eventDesc; VAR tc: REAL) ;
VAR
   i0, i1,
   j0, j1: Coord ;
BEGIN
   getPolygonLine (iLine, iPtr, i0, i1) ;
   getPolygonLine (jLine, jPtr, j0, j1) ;

   (* test i0 crossing jLine *)
   findCollisionCircleLineOrbiting (iPtr, jPtr, iLine, jLine, i0, 0.0, edesc, tc, makePolygonPolygon) ;

   (* test i1 crossing line j *)
   findCollisionCircleLineOrbiting (iPtr, jPtr, iLine, jLine, i1, 0.0, edesc, tc, makePolygonPolygon) ;

   (* test j0 crossing line i *)
   findCollisionCircleLineOrbiting (jPtr, iPtr, iLine, jLine, j0, 0.0, edesc, tc, makePolygonPolygon) ;

   (* test j1 crossing line i *)
   findCollisionCircleLineOrbiting (jPtr, iPtr, iLine, jLine, j1, 0.0, edesc, tc, makePolygonPolygon)

END findCollisionLineLineOrbiting ;


PROCEDURE findCollisionLineLineNonOrbiting (iPtr, jPtr: Object; iLine, jLine: CARDINAL; VAR edesc: eventDesc; VAR tc: REAL) ;
VAR
   i0, i1,
   j0, j1: Coord ;
BEGIN
   getPolygonLine (iLine, iPtr, i0, i1) ;
   getPolygonLine (jLine, jPtr, j0, j1) ;

   (* test i0 crossing jLine *)
   findCollisionCircleLine (iPtr, jPtr, iLine, jLine, i0, 0.0, edesc, tc, makePolygonPolygon) ;

   (* test i1 crossing line j *)
   findCollisionCircleLine (iPtr, jPtr, iLine, jLine, i1, 0.0, edesc, tc, makePolygonPolygon) ;

   (* test j0 crossing line i *)
   findCollisionCircleLine (jPtr, iPtr, iLine, jLine, j0, 0.0, edesc, tc, makePolygonPolygon) ;

   (* test j1 crossing line i *)
   findCollisionCircleLine (jPtr, iPtr, iLine, jLine, j1, 0.0, edesc, tc, makePolygonPolygon)

END findCollisionLineLineNonOrbiting ;


(*
   getPolygonRPoint - get the point on rotating polygon, o.  i determines the point number (1..npoints).
                      pol is the polar coord relative offset from the cofg of the polygon.
                      u is the initial velocity.
                      a is the acceleration.
                      w is the angle of rotation.
*)

(*
PROCEDURE getPolygonRPoint (i: CARDINAL; o: Object; VAR cofg, u, a: Coord; VAR w: REAL; VAR pol: Polar) ;
BEGIN
   WITH o^ DO
      w := angularVelocity ;
      u := initCoord(vx, vy) ;
      a := initCoord(ax, ay) ;

      CASE object OF

      polygonOb :  cofg := calculateCofG(p.nPoints, p.points) ;
                   pol := coordToPolar(subCoord(p.points[i-1], cofg)) |
      circleOb  :  HALT |
      springOb  :  HALT |
      rpolygonOb:  pol := r.points[i-1] ;
                   cofg := r.cOfG

      ELSE
         HALT
      END
   END
END getPolygonRPoint ;
*)


(*
   Abs - return the absolute value of, r.
*)

PROCEDURE Abs (r: REAL) : REAL ;
BEGIN
   IF r<0.0
   THEN
      RETURN -r
   ELSE
      RETURN r
   END
END Abs ;


(*
   findAllTimesOfCollisionRLineRPoint -
                                                       4         4   4
                                             - ((16 j k  - 16 d e ) t

                                                       3               3         3               3   3
                                              + (64 j k  l - 32 %pi j k  - 64 d e  f + 32 %pi d e ) t

                                                       2  2             2            2          2                2  2
                                              + (96 j k  l  - 96 %pi j k  l + (24 %pi  - 32) j k  + 32 i - 96 d e  f

                                                          2                 2     2          2
                                              + 96 %pi d e  f + (32 - 24 %pi ) d e  - 32 c) t

                                                         3               2          2                              3
                                              + (64 j k l  - 96 %pi j k l  + (48 %pi  - 64) j k l + (32 %pi - 8 %pi ) j k

                                                               3               2               2
                                              + 64 h - 64 d e f  + 96 %pi d e f  + (64 - 48 %pi ) d e f

                                                      3                                 4             3
                                              + (8 %pi  - 32 %pi) d e - 64 b) t + 16 j l  - 32 %pi j l

                                                       2          2                  3            4        2
                                              + (24 %pi  - 32) j l  + (32 %pi - 8 %pi ) j l + (%pi  - 8 %pi  + 64) j + 64 g

                                                      4             3               2     2         3
                                              - 16 d f  + 32 %pi d f  + (32 - 24 %pi ) d f  + (8 %pi  - 32 %pi) d f

                                                      4        2
                                              + (- %pi  + 8 %pi  - 64) d - 64 a)/64 = 0
*)

PROCEDURE findAllTimesOfCollisionRLineRPoint (a, b, c, d, e, f,
                                              g, h, i, j, k, l: REAL;
                                              VAR t: ARRAY OF REAL) : CARDINAL ;
VAR
   A, B, C, D, E: REAL ;
BEGIN
   A := 16.0 * j * quad(k) - 16.0 * d * quad(e);
   B := 64.0 * j * cub(k) * l - 32.0 * pi * j * cub(k)  - 64.0 * d * cub(e) * f + 32.0 * pi * d * cub(e) ;
   C := 96.0 * j * sqr(k) * sqr(l) - 96.0 * pi * j * sqr(k) * l + (24.0 * sqr(pi) - 32.0) * j * sqr(k)  + 32.0 * i - 96.0 * d * sqr(e) * sqr(f)
        + 96.0 * pi * d * sqr(e) * f + (32.0 - 24.0 * sqr(pi) ) * d * sqr(e)  - 32.0 * c ;
   D := (64.0 * j * k * cub(l)  - 96.0 * pi * j * k * sqr(l)  + (48.0 * sqr(pi)  - 64.0) * j * k * l + (32.0 * pi - 8.0 * sqr(pi) ) * j * k
        + 64.0 * h - 64.0 * d * e * cub(f)  + 96.0 * pi * d * e * sqr(f)  + (64.0 - 48.0 * sqr(pi) ) * d * e * f
        + (8.0 * cub(pi) - 32.0 * pi) * d * e - 64.0 * b) ;
   E := 16.0 * j * quad(l)  - 32.0 * pi * j * cub(l)
        + (24.0 * sqr(pi)  - 32.0) * j * sqr(l)  + (32.0 * pi - 8.0 * cub(pi) ) * j * l + (cub(pi)  - 8.0 * sqr(pi) + 64.0) * j + 64.0 * g
        - 16.0 * d * quad(f)  + 32.0 * pi * d * cub(f) + (32.0 - 24.0 * sqr(pi) ) * d * sqr(f)  + (8.0 * cub(pi)  - 32.0 * pi) * d * f
        + (- quad(pi)  + 8.0 * sqr(pi)  - 64.0) * d - 64.0 * a ;
   RETURN findAllRootsQuartic(A, B, C, D, E, t)
END findAllTimesOfCollisionRLineRPoint ;


(*
   findEarliestCollisionRLineRPoint - find the earliest time when rotating point, j, collides with rotating line, i.
*)

PROCEDURE findEarliestCollisionRLineRPoint (iPtr, jPtr: Object; i, j: CARDINAL;
                                            VAR edesc: eventDesc; VAR tc: REAL;
                                            si, ui, ai, ri, wi, oi, sj, uj, aj, rj, wj, oj: REAL;
                                            p1, p2: Coord) ;
VAR
   t   : ARRAY [0..3] OF REAL ;    (* possible times of collision *)
   k, n: CARDINAL ;
   l   : REAL ;
BEGIN
(*
   (* when do the Y values of line, i, equal Y values for point, j? *)
   l := Abs(p2.x - p1.x) ;
   AssertR(p1.y, p2.y) ;

   n := findAllTimesOfCollisionRLineRPoint(si, ui, ai, ri, wi, oi,
                                           sj, uj, aj, rj, wj, oj,
                                           t) ;
   k := 0 ;
   getPolygonRPoint(j, jPtr, cofg, u,
   WHILE k<n DO
      (* at time t[k], do the X value of point, j, intersect with line, i *)
      IF t[k]>=0.0
      THEN
         IF
         x := newPositionRotationCosScalar(getCofG(jPtr),
      END ;
      INC(k)
   END
*)
END findEarliestCollisionRLineRPoint ;


(*
   findCollisionLineRPoint -
*)

PROCEDURE findCollisionLineRPoint (iPtr, rPtr: Object; i, j: CARDINAL; VAR edesc: eventDesc; VAR tc: REAL) ;
VAR
   jp, pol1, pol2: Polar ;
   o, offset,
   jw, iw        : REAL ;
   jpos,
   p1, p2,
   iu, ia,
   ju, ja,
   rcofg         : Coord ;
BEGIN
(*
   getPolygonRLine(i, iPtr, p1, p2, iu, ia, iw, pol1, pol2) ;
   rotateRPointsOntoYaxis(pol1, po2, p1, p2, o, offset) ;

   getPolygonRPoint(j, rPtr, rcofg, ju, ja, jw, jp, jpos) ;
*)
   (* now we ask when/if point jp crosses line p1, p2 *)
   findEarliestCollisionRLineRPoint (iPtr, rPtr, i, j, edesc, tc,
                                     p1.y, iu.y, ia.y, offset, iw, o,
                                     jpos.y, ju.y, ja.y, jp.r, jw, jp.w,
                                     p1, p2)

END findCollisionLineRPoint ;


(*
   findCollisionLineRLine - find the time of collision between line, iPtr, and rotating line, rPtr.
*)

PROCEDURE findCollisionLineRLine (iPtr, rPtr: Object; i, j: CARDINAL; VAR edesc: eventDesc; VAR tc: REAL) ;
BEGIN
   (* test point rj-1 crossing line i *)
   findCollisionLineRPoint (iPtr, rPtr, i, j-1, edesc, tc) ;

   (* test point rj crossing line i *)
   findCollisionLineRPoint (iPtr, rPtr, i, j, edesc, tc) ;

   (* test point ii-1 crossing line j *)
   findCollisionLineRPoint (rPtr, iPtr, j, i-1, edesc, tc) ;

   (* test point ii crossing line j *)
   findCollisionLineRPoint (rPtr, iPtr, j, i, edesc, tc)

END findCollisionLineRLine ;


(*
   findCollisionPolygonPolygon - find the smallest positive time (if any) between the polygons, iPtr
                                 and jPtr colliding.
                                 If a collision if found then, tc, is assigned to the time and the
                                 event descriptor is filled in.
*)

PROCEDURE findCollisionPolygonPolygon (iPtr, jPtr: Object; VAR edesc: eventDesc; VAR tc: REAL) ;
VAR
   i, j: CARDINAL ;
BEGIN
   Assert (iPtr#jPtr, __LINE__) ;
   i := 1 ;
   WHILE i<=iPtr^.p.nPoints DO
      j := 1 ;
      WHILE j<=jPtr^.p.nPoints DO
         findCollisionLineLine (iPtr, jPtr, i, j, edesc, tc) ;
         INC (j)
      END ;
      INC (i)
   END
END findCollisionPolygonPolygon ;


(*
   findCollisionPolygonRPolygon - find the smallest positive time (if any) between the polygons, iPtr
                                  and rPtr colliding.
                                  rPtr is a rotating polygon and iPtr is not rotating.
                                  If a collision if found then, tc, is assigned to the time and the
                                  event descriptor is filled in.
                                  We check possible collision times between all lines of both polygons,
                                  we separate out the rotating polygon from non rotating polygon
                                  as the collision equations only generate a polynomial order 4 rather
                                  than order 8 if both are rotating.
*)

PROCEDURE findCollisionPolygonRPolygon (iPtr, rPtr: Object; VAR edesc: eventDesc; VAR tc: REAL) ;
VAR
   i, j: CARDINAL ;
BEGIN
(*
   Assert (iPtr#rPtr, __LINE__) ;
   i := 1 ;
   WHILE i<=iPtr^.p.nPoints DO
      j := 1 ;
      WHILE j<=rPtr^.r.nPoints DO
         findCollisionLineRLine (iPtr, rPtr, i, j, edesc, tc) ;
         INC (j)
      END ;
      INC (i)
   END
*)
END findCollisionPolygonRPolygon ;


(*
   findCollision -
*)

PROCEDURE findCollision (iptr, jptr: Object; VAR edesc: eventDesc; VAR tc: REAL) ;
BEGIN
   IF trace
   THEN
      printf ("findCollision entered and best time is %g\n", tc)
   END ;
   IF NOT (iptr^.fixed AND jptr^.fixed)
   THEN
      IF (iptr^.object=circleOb) AND (jptr^.object=circleOb)
      THEN
         findCollisionCircles (iptr, jptr, edesc, tc) ;
         IF trace
         THEN
            printf ("findCollision (circles) best time is %g\n", tc)
         END
      ELSIF (iptr^.object=circleOb) AND (jptr^.object=polygonOb)
      THEN
         findCollisionCirclePolygon (iptr, jptr, edesc, tc)
      ELSIF (iptr^.object=polygonOb) AND (jptr^.object=circleOb)
      THEN
         findCollisionCirclePolygon (jptr, iptr, edesc, tc)
      ELSIF (iptr^.object=polygonOb) AND (jptr^.object=polygonOb)
      THEN
         findCollisionPolygonPolygon (jptr, iptr, edesc, tc)
      END
   END ;
   IF trace
   THEN
      printf ("findCollision exiting and best time is %g\n", tc)
   END
END findCollision ;


(*
   debugFrame - debug frame at time, e.
*)

PROCEDURE debugFrame (e: eventQueue) ;
BEGIN
   drawBackground (yellow ()) ;
   drawFrame (NIL)
END debugFrame ;


(*
   addDebugging - add a debugging event at time, t.
*)

PROCEDURE addDebugging (t: REAL; edesc: eventDesc) ;
VAR
   e: eventQueue ;
BEGIN
   e := newEvent () ;
   WITH e^ DO
      time := t ;
      p := debugFrame ;
      ePtr := edesc ;
      next := NIL
   END ;
   addRelative (e)
END addDebugging ;


(*
   anticipateCollision - stores the collision in the anticipated list.
*)

PROCEDURE anticipateCollision (tc: REAL; edesc: eventDesc) ;
BEGIN
   WITH edesc^ DO
      CASE etype OF

      circlesEvent       :  anticipate (currentTime+tc, cc.cid1, cc.cid2, cc.cPoint) |
      circlePolygonEvent :  anticipate (currentTime+tc, cp.pid, cp.cid, cp.cPoint) |
      polygonPolygonEvent:  anticipate (currentTime+tc, pp.pid1, pp.pid2, pp.cPoint)

      END
   END
END anticipateCollision ;


(*
   collisionOccurred - stores the collision in the history list.
*)

PROCEDURE collisionOccurred (edesc: eventDesc) ;
BEGIN
   WITH edesc^ DO
      CASE etype OF

      circlesEvent       :  occurred (currentTime, cc.cid1, cc.cid2, cc.cPoint) |
      circlePolygonEvent :  occurred (currentTime, cp.pid, cp.cid, cp.cPoint) |
      polygonPolygonEvent:  occurred (currentTime, pp.pid1, pp.pid2, pp.cPoint)

      END
   END
END collisionOccurred ;


(*
   subEvent - remove event, e, from the relative time ordered event queue.
*)

PROCEDURE subEvent (e: eventQueue) ;
VAR
   before, f: eventQueue ;
BEGIN
   f := eventQ ;
   before := NIL ;
   WHILE (f#e) AND (f#NIL) DO
      before := f ;
      f := f^.next
   END ;
   IF f#NIL
   THEN
      Assert(f=e, __LINE__) ;
      IF before=NIL
      THEN
         Assert (eventQ=f, __LINE__) ;
         Assert (eventQ=e, __LINE__) ;
         eventQ := eventQ^.next ;
         IF eventQ#NIL
         THEN
            eventQ^.time := eventQ^.time + e^.time
         END ;
      ELSE
         before^.next := e^.next ;
         IF e^.next#NIL
         THEN
            e^.next^.time := e^.next^.time + e^.time
         END
      END ;
      disposeEvent (e)
   END
END subEvent ;


(*
   removeCollisionEvent -
*)

PROCEDURE removeCollisionEvent ;
VAR
   e: eventQueue ;
BEGIN
   e := eventQ ;
   WHILE e#NIL DO
      IF e^.kind = collisionKind
      THEN
         subEvent (e) ;
         RETURN
      ELSE
         e := e^.next ;
      END
   END
END removeCollisionEvent ;


(*
   addNextCollisionEvent -
*)

PROCEDURE addNextCollisionEvent ;
VAR
   tc, old   : REAL ;
   ic, jc,
   i, j, n   : CARDINAL ;
   iptr, jptr: Object ;
   edesc     : eventDesc ;
BEGIN
   removeCollisionEvent ;
   (* gdbif.sleepSpin ; *)
   n := HighIndice (objects) ;
   i := 1 ;
   edesc := NIL ;
   tc := -1.0 ;
   WHILE i<=n DO
      iptr := GetIndice (objects, i) ;
      IF objectExists (iptr)
      THEN
         j := i+1 ;
         WHILE j<=n DO
            jptr := GetIndice (objects, j) ;
            IF (iptr#jptr) AND objectExists (jptr)
            THEN
               IF trace
               THEN
                  printf ("** checking pair %d, %d\n", iptr^.id, jptr^.id) ;
                  old := tc
               END ;
               findCollision (iptr, jptr, edesc, tc) ;
	       IF trace AND (old # tc)
               THEN
                  printf ("** collision found between pair %d, %d at time %g\n", iptr^.id, jptr^.id, tc)
               END
            END ;
            INC(j)
         END
      END ;
      INC(i)
   END ;
   IF edesc#NIL
   THEN
      addCollisionEvent (tc, doCollision, edesc) ;
      anticipateCollision (tc, edesc)
   ELSE
      printf ("no more collisions found\n")
   END
END addNextCollisionEvent ;


(*
   skipFor - skip displaying any frames for, t, simulated seconds.
*)

PROCEDURE skipFor (t: REAL) ;
BEGIN
   simulateFor (t)
END skipFor ;


(*
   resetQueue -
*)

PROCEDURE resetQueue ;
VAR
   c, f, e: eventQueue ;
BEGIN
   c := NIL ;  (* collision event *)
   f := NIL ;  (* draw frame event *)
   e := eventQ ;
   WHILE (e#NIL) AND ((c=NIL) OR (f=NIL)) DO
      IF e^.kind = collisionKind
      THEN
         c := e
      ELSIF e^.kind = frameKind
      THEN
         f := e
      END ;
      e := e^.next
   END ;
   IF f=NIL
   THEN
      addEvent (frameKind, 1.0/framesPerSecond - (currentTime - lastDrawTime), drawFrameEvent)
   END ;
   IF c=NIL
   THEN
      addNextCollisionEvent
   END
END resetQueue ;


(*
   simulateFor - render for, t, seconds.
*)

PROCEDURE simulateFor (t: REAL) ;
VAR
   s, dt: REAL ;
BEGIN
   (*
   gdbif.sleepSpin ;
   *)
   s := 0.0 ;
   checkObjects ;
   IF s<t
   THEN
      pumpQueue ;
      printQueue ;
      WHILE s<t DO
         dt := doNextEvent () ;
         s := s + dt
      END ;
      updatePhysics ;
      printQueue
   END
END simulateFor ;


(*
   disposeEvent - returns the event to the free queue.
*)

PROCEDURE disposeEvent (e: eventQueue) ;
BEGIN
   disposeDesc(e^.ePtr) ;
   e^.next := freeEvents ;
   freeEvents := e
END disposeEvent ;


(*
   disposeDesc - returns the event desc to the free queue.
*)

PROCEDURE disposeDesc (VAR d: eventDesc) ;
BEGIN
   IF d#NIL
   THEN
      d^.next := freeDesc ;
      freeDesc := d ;
      d := NIL
   END
END disposeDesc ;


(*
   newDesc - returns a new eventDesc.
*)

PROCEDURE newDesc () : eventDesc ;
VAR
   e: eventDesc ;
BEGIN
   IF freeDesc=NIL
   THEN
      NEW(e)
   ELSE
      e := freeDesc ;
      freeDesc := freeDesc^.next
   END ;
   RETURN e
END newDesc ;


(*
   newEvent - returns a new eventQueue.
*)

PROCEDURE newEvent () : eventQueue ;
VAR
   e: eventQueue ;
BEGIN
   IF freeEvents=NIL
   THEN
      NEW(e)
   ELSE
      e := freeEvents ;
      freeEvents := freeEvents^.next
   END ;
   e^.ePtr := NIL ;
   RETURN e
END newEvent ;


(*
   makeCirclesDesc - return a eventDesc which describes two circles colliding.
*)

PROCEDURE makeCirclesDesc (VAR edesc: eventDesc; cid1, cid2: CARDINAL; cp: Coord) : eventDesc ;
BEGIN
   IF edesc=NIL
   THEN
      edesc := newDesc()
   END ;
   edesc^.etype := circlesEvent ;
   edesc^.cc.cPoint := cp ;
   edesc^.cc.cid1 := cid1 ;
   edesc^.cc.cid2 := cid2 ;
   RETURN edesc
END makeCirclesDesc ;


(*
   addRelative - adds event, e, into the relative event queue.
*)

PROCEDURE addRelative (e: eventQueue) ;
VAR
   before, after: eventQueue ;
BEGIN
   IF Debugging
   THEN
      printf ("addRelative entered, event abs time = %g\n", e^.time) ;
      printQueue
   END ;
   IF eventQ=NIL
   THEN
      eventQ := e
   ELSIF e^.time<eventQ^.time
   THEN
      eventQ^.time := eventQ^.time - e^.time ;
      e^.next := eventQ ;
      eventQ := e
   ELSE
      IF Debugging
      THEN
         printQueue
      END ;
      before := eventQ ;
      after := eventQ^.next ;
      e^.time := e^.time - before^.time ;
      WHILE (after # NIL) AND (after^.time < e^.time) DO
         before := after ;
         e^.time := e^.time - before^.time ;
         after := after^.next
      END ;
      IF after#NIL
      THEN
         after^.time := after^.time-e^.time
      END ;
      before^.next := e ;
      e^.next := after ;
   END ;
   IF Debugging
   THEN
      printf ("addRelative changed queue to:\n") ;
      printQueue ;
      printf ("addRelative finishing now\n")
   END
END addRelative ;


(*
   addEvent - adds an event which has no collision associated with it.
              Typically this is a debugging event or display frame event.
*)

PROCEDURE addEvent (k: eventKind; t: REAL; dop: eventProc) ;
VAR
   e: eventQueue ;
BEGIN
   IF Debugging
   THEN
      printf ("new event will occur at time %g in the future\n", t)
   END ;
   Assert (t >= 0.0, __LINE__) ;
   e := newEvent() ;
   WITH e^ DO
      kind := k ;
      time := t ;
      p := dop ;
      ePtr := NIL ;
      next := NIL
   END ;
   addRelative (e)
END addEvent ;


(*
   addCollisionEvent - adds a collision event, the edesc is attached to the,
                       eventQueue, which is placed onto the eventQ.
*)

PROCEDURE addCollisionEvent (t: REAL; dop: eventProc; edesc: eventDesc) ;
VAR
   e: eventQueue ;
BEGIN
   IF Debugging
   THEN
      printf("collision will occur in %g simulated seconds\n", t)
   END ;
   Assert (t >= 0.0, __LINE__) ;
   e := newEvent () ;
   WITH e^ DO
      kind := collisionKind ;
      time := t ;
      p := dop ;
      ePtr := edesc ;
      next := NIL
   END ;
   IF Debugging
   THEN
      printf("collision about to be added to this queue\n", t) ;
      printQueue ;
      printf("collision about to be added to this queue\n", t)
   END ;
   addRelative (e) ;
   IF Debugging
   THEN
      printf("collision now added to this queue\n", t) ;
      printQueue
   END
END addCollisionEvent ;


(*
   getTime - returns the current time in the simulation.
*)

PROCEDURE getTime () : REAL ;
BEGIN
   RETURN currentTime
END getTime ;


(*
   isEvent - return TRUE if the next event is of kind, k.
*)

PROCEDURE isEvent (k: eventKind) : BOOLEAN ;
BEGIN
   IF eventQ=NIL
   THEN
      RETURN FALSE
   ELSE
      RETURN eventQ^.kind = k
   END
END isEvent ;


(*
   isCollision - returns TRUE if the next event is a collision event.
*)

PROCEDURE isCollision () : BOOLEAN ;
BEGIN
   IF Debugging
   THEN
      printf ("isCollision before pumpQueue\n")
   END ;
   pumpQueue ;
   RETURN isEvent (collisionKind)
END isCollision ;


(*
   isFrame - returns TRUE if the next event is a frame event.
*)

PROCEDURE isFrame () : BOOLEAN ;
BEGIN
   IF Debugging
   THEN
      printf ("isFrame before pumpQueue\n")
   END ;
   pumpQueue ;
   RETURN isEvent (frameKind)
END isFrame ;


(*
   isFunction - returns TRUE if the next event is a function event.
*)

PROCEDURE isFunction () : BOOLEAN ;
BEGIN
   IF Debugging
   THEN
      printf ("isFunction before pumpQueue\n")
   END ;
   pumpQueue ;
   RETURN isEvent (functionKind)
END isFunction ;


(*
   createFunctionEvent - creates a function event at time, t,
                         in the future.
*)

PROCEDURE createFunctionEvent (t: REAL; id: CARDINAL) ;
VAR
   e    : eventQueue ;
   edesc: eventDesc ;
BEGIN
   pumpQueue ;
   IF Debugging
   THEN
      printf("function event %d will occur in %g simulated seconds\n", id, t)
   END ;
   edesc := newDesc () ;
   edesc^.etype := functionEvent ;
   edesc^.fc.id := id ;
   e := newEvent () ;
   WITH e^ DO
      kind := functionKind ;
      time := t ;
      p := doFunctionEvent ;
      ePtr := edesc ;
      next := NIL
   END ;
   IF Debugging
   THEN
      printf ("queue before function event\n") ;
      printQueue
   END ;
   addRelative (e) ;
   IF Debugging
   THEN
      printf ("queue after function event\n") ;
      printQueue
   END
END createFunctionEvent ;


(*
   timeUntil - returns the relative time from now until the next event.
*)

PROCEDURE timeUntil () : REAL ;
BEGIN
   pumpQueue ;
   IF eventQ=NIL
   THEN
      RETURN 0.0
   ELSE
      RETURN eventQ^.time
   END
END timeUntil ;


(*
   dumpTime -
*)

PROCEDURE dumpTime ;
BEGIN
   printf ("  absolute time is %f\n", currentTime);
   printf ("  last update time is %f\n", lastUpdateTime)
END dumpTime ;


(*
   skipTime - attempts to skip, t, seconds.  It returns the amount
              of time actually skipped.  This function will not skip
              past the next event.
*)

PROCEDURE skipTime (t: REAL) : REAL ;
VAR
   dt: REAL ;
BEGIN
   IF Debugging
   THEN
      printf ("skipTime %f\n", t) ;
      dumpTime
   END ;
   pumpQueue ;
   IF eventQ=NIL
   THEN
      printf ("  no events in the queue\n") ;
      dumpTime ;
      printf ("finishing skipTime\n") ;
      RETURN 0.0
   ELSE
      IF t > eventQ^.time
      THEN
         dt := eventQ^.time ;
         IF NOT nearZero (dt)
         THEN
            currentTime := currentTime + dt
         END ;
         eventQ^.time := 0.0
      ELSE
         dt := eventQ^.time - t ;
         IF nearZero (dt)
         THEN
            currentTime := currentTime + eventQ^.time ;
            eventQ^.time := 0.0
         ELSE
            currentTime := currentTime + t ;
            eventQ^.time := eventQ^.time - t
         END
      END ;
      IF Debugging
      THEN
         dumpTime ;
         printf ("finishing skipTime\n")
      END ;
      RETURN dt
   END
END skipTime ;


(*
   recordEvent -
*)

PROCEDURE recordEvent ;
BEGIN
   checkOpened ;
   IF eventQ#NIL
   THEN
      IF DebugTrace
      THEN
         printf ("before writeEvent\n")
      END ;
      (* gdbif.sleepSpin ; *)
      writeEvent (eventQ) ;
      IF DebugTrace
      THEN
         printf ("after writeEvent\n")
      END
   END
END recordEvent ;


(*
   processEvent - skips any outstanding time and processes the next event.
                  Time is adjusted to the time of the next event.
*)

PROCEDURE processEvent ;
VAR
   dt: REAL ;
BEGIN
   (* gdbif.sleepSpin ; *)
   IF Debugging
   THEN
      printf ("processEvent before pumpQueue\n")
   END ;
   pumpQueue ;
   IF DebugTrace
   THEN
      printf ("before doNextEvent\n");
      printQueue
   END ;
   dt := doNextEvent () ;
   IF DebugTrace
   THEN
      printf ("finished doNextEvent\n")
   END
END processEvent ;


(*
   pumpQueue - prime the event queue with initial frame and collision events.
*)

PROCEDURE pumpQueue ;
BEGIN
   resetQueue ;
   recordEvent
END pumpQueue ;


(*
   rm - delete this object from the simulated world.
        The same id is returned.
*)

PROCEDURE rm (id: CARDINAL) : CARDINAL ;
VAR
   optr: Object ;
BEGIN
   down ;
   optr := GetIndice (objects, id) ;
   IF DebugTrace
   THEN
      printf ("rm %d\n", id) ;
      printf ("here is the world before rm\n") ;
      dumpWorld
   END ;
   WITH optr^ DO
      deleted := TRUE
   END ;
   IF DebugTrace
   THEN
      printf ("rm complete, here is the world after rm\n") ;
      dumpWorld
   END ;
   resetStationary ;
   up ;
   RETURN id
END rm ;


(*
   up - recreate the event queue.
        The pair up/down must be used to shutdown
        the event queue if the world is to be altered.
*)

PROCEDURE up ;
BEGIN
   forgetFuture ;
   resetQueue ;
   IF DebugTrace
   THEN
      printf ("exiting up\n") ;
      printQueue ;
      printf ("remaking queue\n") ;
      removeCollisionEvent ;
      printf ("collision event removed\n") ;
      printQueue ;
      printf ("collision event added\n") ;
      resetQueue ;
      printQueue ;
      printf ("and again (collision event added)\n") ;
      removeCollisionEvent ;
      resetQueue ;
      printQueue ;
      printf ("exiting finishing\n") ;
   END
END up ;


(*
   down - shutdown the event queue.
*)

PROCEDURE down ;
BEGIN
   IF DebugTrace
   THEN
      printf ("entered down\n") ;
      printQueue
   END ;
   updatePhysics ;
   removeCollisionEvent
END down ;


(*
   killQueue - destroys the event queue and returns events to the free list.
*)

PROCEDURE killQueue ;
VAR
   e: eventQueue ;
BEGIN
   IF eventQ#NIL
   THEN
      e := eventQ ;
      WHILE e^.next#NIL DO
         e := e^.next
      END ;
      e^.next := freeEvents ;
      freeEvents := eventQ ;
      eventQ := NIL
   END
END killQueue ;


(*
   writeCircles -
*)

PROCEDURE writeCircles (c: cDesc) ;
BEGIN
   IF DebugTrace
   THEN
      printf ("writeCircleCircle %d %d\n", c.cid1, c.cid2)
   END ;
   writeCoord (file, c.cPoint) ;
   writeCard (file, c.cid1) ;
   writeCard (file, c.cid2)
END writeCircles ;


(*
   writeKind -
*)

PROCEDURE writeKind (k: whereHit) ;
BEGIN
   writeCard (file, ORD (k))
END writeKind ;


(*
   writeCirclePolygon -
*)

PROCEDURE writeCirclePolygon (c: cpDesc) ;
BEGIN
   IF DebugTrace
   THEN
      printf ("writeCirclePolygon %d %d\n", c.pid, c.cid)
   END ;
   writeCoord (file, c.cPoint) ;
   writeCard (file, c.pid) ;
   writeCard (file, c.cid) ;
   writeKind (c.wpid)
END writeCirclePolygon ;


(*
   writePolygonPolygon -
*)

PROCEDURE writePolygonPolygon (p: ppDesc) ;
BEGIN
   IF DebugTrace
   THEN
      printf ("writePolygonPolygon %d %d\n", p.pid1, p.pid2)
   END ;
   writeCoord (file, p.cPoint) ;
   writeCard (file, p.pid1) ;
   writeCard (file, p.pid2) ;
   writeKind (p.wpid1) ;
   writeKind (p.wpid2)
END writePolygonPolygon ;


(*
   writeFunction -
*)

PROCEDURE writeFunction (fc: fcDesc) ;
BEGIN
   writeCard (file, fc.id)
END writeFunction ;


(*
   writeDesc -
*)

PROCEDURE writeDesc (p: eventDesc) ;
BEGIN
   IF p=NIL
   THEN
      writeCard (file, ORD (frameEvent))
   ELSE
      WITH p^ DO
         writeCard (file, ORD (etype)) ;
         CASE etype OF

         frameEvent         :  |
         circlesEvent       :  writeCircles (cc) |
         circlePolygonEvent :  writeCirclePolygon (cp) |
         polygonPolygonEvent:  writePolygonPolygon (pp) |
	 functionEvent      :  writeFunction (fc)

         END
      END
   END
END writeDesc ;


(*
   writeEvent - writes the first event to the file.
*)

PROCEDURE writeEvent (e: eventQueue) ;
BEGIN
   MemStream.Rewrite (file) ;
   IF DebugTrace
   THEN
      printf ("time of next event in twoDsim is %g\n", e^.time);
      displayEvent (e)
   END ;
   WITH e^ DO
      IF DebugTrace
      THEN
         printf ("check time is %g\n", time)
      END ;
      writeReal (file, time) ;
      writeDesc (ePtr)
   END
END writeEvent ;


(*
   memDump -
*)

PROCEDURE memDump (a: ADDRESS; len: CARDINAL) ;
VAR
   i, j: CARDINAL ;
   p   : POINTER TO BYTE ;
BEGIN
   p := a ;
   j := 0 ;
   FOR i := 0 TO len DO
      IF j MOD 16 = 0
      THEN
         printf ("\n%p  %02x", p, VAL(CARDINAL, p^))
      ELSE
         printf (" %02x", VAL(CARDINAL, p^))
      END ;
      INC(p) ;
      INC(j)
   END ;
   printf ("\n")
END memDump ;


(*
   getEventBuffer - collects the event buffer limits in the following parameters.
*)

PROCEDURE getEventBuffer (VAR start: ADDRESS; VAR length: CARDINAL; VAR used: CARDINAL) ;
VAR
   f: POINTER TO REAL ;
BEGIN
   recordEvent ;
   start := bufferStart ;
   length := bufferLength ;
   used := bufferUsed ;
   f := bufferStart ;
   IF DebugTrace
   THEN
      printf ("event buffer ptr = 0x%p, length = %d, used = %d\n", start, length, used);
      printf ("ptr to real is %g\n", f^) ;
      memDump (start, 8)
   END
END getEventBuffer ;


(*
   checkOpened - checks to see of the MemStream file has been created and if not then
                 it is opened.
*)

PROCEDURE checkOpened ;
VAR
   res: OpenResults ;
BEGIN
   IF NOT fileOpened
   THEN
      fileOpened := TRUE ;
      MemStream.OpenWrite (file, write+raw, res, bufferStart, bufferLength, bufferUsed, TRUE) ;
      IF res#opened
      THEN
         printf ("twoDsim.checkOpened: something went wrong when trying to open the memstream file (res = %d)\n", res);
         exit (1)
      END
   END
END checkOpened ;


(*
   buildFrame - populate the frame buffer contents with the world at the current time.
*)

PROCEDURE buildFrame ;
BEGIN
   frameNote ;
   drawFrame (NIL) ;
   flipBuffer
END buildFrame ;


(*
   emptyFbuffer - empty the frame buffer.
*)

PROCEDURE emptyFbuffer ;
BEGIN
   deviceIf.emptyFbuffer
END emptyFbuffer ;


(*
   emptyCbuffer - empty the colour buffer.
*)

PROCEDURE emptyCbuffer ;
BEGIN
   deviceIf.emptyCbuffer
END emptyCbuffer ;


(*
   useTimeDelay - should the frame buffer include the time delay command?
*)

PROCEDURE useTimeDelay (on: BOOLEAN) ;
BEGIN
   writeTimeDelay := on
END useTimeDelay ;


(*
   drawCollisionFrames - turn the drawing of collision frames on or off.
                         actual:   determines whether an extra frame is generated
                                   at the time of actual collision.
                         predict:  draws a frame predicting the next collision.
                                   It will show the points predicted to collide.
*)

PROCEDURE drawCollisionFrames (actual, predict: BOOLEAN) ;
BEGIN
   drawCollisionFrame := actual ;
   drawPrediction := predict
END drawCollisionFrames ;


(*
   dumpWorld - dump a list of all objects and their characteristics.
*)

PROCEDURE dumpWorld ;
VAR
   i, n: CARDINAL ;
BEGIN
   n := HighIndice(objects) ;
   i := 1 ;
   WHILE i<=n DO
      DumpObject (GetIndice (objects, i)) ;
      INC (i)
   END
END dumpWorld ;


(*
   Init -
*)

PROCEDURE Init ;
BEGIN
   trace := DebugTrace ;
   setTrace (trace) ;
   maxId := 0 ;
   objects := InitIndex (1) ;
   framesPerSecond := DefaultFramesPerSecond ;
   replayPerSecond := 0.0 ;
   simulatedGravity := 0.0 ;
   eventQ := NIL ;
   freeEvents := NIL ;
   freeDesc := NIL ;
   currentTime := 0.0 ;
   lastUpdateTime := 0.0 ;
   lastDrawTime := 0.0 ;
   drawCollisionFrame := FALSE ;
   drawPrediction := FALSE ;
   fileOpened := FALSE ;
   writeTimeDelay := TRUE ;
   haveCollisionColour := FALSE ;
   (* gdbif.sleepSpin *)
END Init ;


BEGIN
   Init
END twoDsim.
