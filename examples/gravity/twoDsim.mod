(* Copyright (C) 2008, 2009 Free Software Foundation, Inc. *)
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

IMPLEMENTATION MODULE twoDsim ;

FROM Storage IMPORT ALLOCATE ;
FROM Indexing IMPORT Index, InitIndex, PutIndice, GetIndice, HighIndice ;
FROM libc IMPORT printf ;
FROM deviceGnuPic IMPORT Coord, newFrame, renderFrame, circleFrame, polygonFrame, produceAVI ;


CONST
   MaxPolygonPoints       =  6 ;
   DefaultFramesPerSecond = 24.0 ;

TYPE
   ObjectType = (polygonOb, circleOb, pivotOb) ;

   Object = POINTER TO RECORD
                          id            : CARDINAL ;
                          fixed         : BOOLEAN ;
                          vx, vy, ax, ay: REAL ;
                          CASE object: ObjectType OF

                          polygonOb:  p: Polygon |
                          circleOb :  c: Circle |
                          pivotOb  :  v: Pivot

                          END
                       END ;

   Pivot = RECORD
              pos: Coord ;
              id1,
              id2: CARDINAL ;
           END ;

   Circle = RECORD
               pos : Coord ;
               r   : REAL ;
               mass: REAL ;
            END ;

   Polygon = RECORD
                pos    : Coord ;
                nPoints: CARDINAL ;
                points : ARRAY [0..MaxPolygonPoints] OF Coord ;
                mass   : REAL ;
             END ;

   eventQueue = POINTER TO RECORD
                              time: REAL ;
                              p   : PROC ;
                              next: eventQueue ;
                           END ;

VAR
   objects         : Index ;
   maxId           : CARDINAL ;
   framesPerSecond : REAL ;
   simulatedGravity: REAL ;
   eventQ,
   freeEvents      : eventQueue ;


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
      id     := maxId ;
      fixed  := FALSE ;
      object := type ;
      vx     := 0.0 ;
      vy     := 0.0 ;
      ax     := 0.0 ;
      ay     := 0.0
   END ;
   PutIndice(objects, maxId, optr) ;
   RETURN( maxId )
END newObject ;


(*
   box - place a box in the world at (x0,y0),(x0+i,y0+j)
*)

PROCEDURE box (x0, y0, i, j: REAL) : CARDINAL ;
VAR
   id: CARDINAL ;
   optr: Object ;
BEGIN
   id := newObject(polygonOb) ;
   optr := GetIndice(objects, id) ;
   WITH optr^ DO
      p.nPoints := 4 ;
      p.points[0].x := x0 ;
      p.points[0].y := y0 ;
      p.points[1].x := x0+i ;
      p.points[1].y := y0 ;
      p.points[2].x := x0+i ;
      p.points[2].y := y0+j ;
      p.points[3].x := x0 ;
      p.points[3].y := y0+j
   END ;
   RETURN id
END box ;


(*
   poly3 - place a triangle in the world at:
           (x0,y0),(x1,y1),(x2,y2)
*)

PROCEDURE poly3 (x0, y0, x1, y1, x2, y2: REAL) : CARDINAL ;
VAR
   id: CARDINAL ;
   optr: Object ;
BEGIN
   id := newObject(polygonOb) ;
   optr := GetIndice(objects, id) ;
   WITH optr^ DO
      p.nPoints := 3 ;
      p.points[0].x := x0 ;
      p.points[0].y := y0 ;
      p.points[1].x := x1 ;
      p.points[1].y := y1 ;
      p.points[2].x := x2 ;
      p.points[2].y := y2
   END ;
   RETURN id
END poly3 ;


(*
   poly5 - place a pentagon in the world at:
           (x0,y0),(x1,y1),(x2,y2),(x3,y3),(x4,y4)
*)

PROCEDURE poly5 (x0, y0, x1, y1, x2, y2, x3, y3, x4, y4: REAL) : CARDINAL ;
VAR
   id  : CARDINAL ;
   optr: Object ;
BEGIN
   id := newObject(polygonOb) ;
   optr := GetIndice(objects, id) ;
   WITH optr^ DO
      p.nPoints := 5 ;
      p.points[0].x := x0 ;
      p.points[0].y := y0 ;
      p.points[1].x := x1 ;
      p.points[1].y := y1 ;
      p.points[2].x := x2 ;
      p.points[2].y := y2 ;
      p.points[3].x := x2 ;
      p.points[3].y := y2 ;
      p.points[4].x := x2 ;
      p.points[4].y := y2
   END ;
   RETURN id
END poly5 ;


(*
   poly6 - place a hexagon in the world at:
           (x0,y0),(x1,y1),(x2,y2),(x3,y3),(x4,y4),(x5,y5)
*)

PROCEDURE poly6 (x0, y0, x1, y1, x2, y2, x3, y3, x4, y4, x5, y5: REAL) : CARDINAL ;
VAR
   id  : CARDINAL ;
   optr: Object ;
BEGIN
   id := newObject(pivotOb) ;
   optr := GetIndice(objects, id) ;
   WITH optr^ DO
      p.nPoints := 6 ;
      p.points[0].x := x0 ;
      p.points[0].y := y0 ;
      p.points[1].x := x1 ;
      p.points[1].y := y1 ;
      p.points[2].x := x2 ;
      p.points[2].y := y2 ;
      p.points[3].x := x2 ;
      p.points[3].y := y2 ;
      p.points[4].x := x2 ;
      p.points[4].y := y2 ;
      p.points[5].x := x2 ;
      p.points[5].y := y2
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

      polygonOb:  p.mass := m |
      circleOb:   c.mass := m

      ELSE
      END
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
   optr := GetIndice(objects, id) ;
   WITH optr^ DO
      fixed := TRUE
   END ;
   RETURN id
END fix ;


(*
   circle - adds a circle to the world.  Center
            defined by: x0, y0 radius, r.
*)

PROCEDURE circle (x0, y0, r: REAL) : CARDINAL ;
VAR
   id  : CARDINAL ;
   optr: Object ;
BEGIN
   id := newObject(circleOb) ;
   optr := GetIndice(objects, id) ;
   WITH optr^ DO
      c.pos.x := x0 ;
      c.pos.y := y0 ;
      c.r     := r ;
      c.mass  := 0.0
   END ;
   RETURN id
END circle ;


(*
   pivot - pivot an object to position, (x0, y0).
*)

PROCEDURE pivot (x0, y0: REAL; id1: CARDINAL) : CARDINAL ;
VAR
   id  : CARDINAL ;
   optr: Object ;
BEGIN
   id := newObject(pivotOb) ;
   optr := GetIndice(objects, id) ;
   WITH optr^ DO
      v.pos.x := x0 ;
      v.pos.y := y0 ;
      v.id1 := id1
   END ;
   RETURN id
END pivot ;


(*
   velocity - give an object, id, a velocity, vx, vy.
*)

PROCEDURE velocity (id: CARDINAL; vx, vy: REAL) : CARDINAL ;
VAR
   optr: Object ;
BEGIN
   optr := GetIndice(objects, id) ;
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
   optr := GetIndice(objects, id) ;
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
   fps - set frames per second.
*)

PROCEDURE fps (f: REAL) ;
BEGIN
   framesPerSecond := f
END fps ;


(*
   drawFrame - 
*)

PROCEDURE drawFrame ;
VAR
   i, n: CARDINAL ;
   optr: Object ;
BEGIN
   newFrame ;
   n := HighIndice(objects) ;
   i := 1 ;
   WHILE i<=n DO
      optr := GetIndice(objects, i) ;
      WITH optr^ DO
         CASE object OF

         circleOb :  circleFrame(c.pos, c.r) |
         polygonOb:  polygonFrame(p.pos, p.nPoints, p.points) |
         pivotOb  :

         END
      END ;
      INC(i)
   END ;
   renderFrame
END drawFrame ;


(*
   drawFrameEvent - 
*)

PROCEDURE drawFrameEvent ;
BEGIN
   drawFrame ;
   addEvent(1.0/framesPerSecond, drawFrameEvent)
END drawFrameEvent ;


(*
   updateCircle - 
*)

PROCEDURE updateCircle (optr: Object; dt: REAL) ;
VAR
   vn: REAL ;
BEGIN
   WITH optr^ DO
      (* update vx and pos.x *)
      vn := vx + ax*dt ;
      c.pos.x := c.pos.x+dt*(vx+vn)/2.0 ;
      vx := vn ;
      (* update vy and pos.y *)
      vn := vy + (ay+simulatedGravity)*dt ;
      c.pos.y := c.pos.y+dt*(vy+vn)/2.0 ;
      vy := vn
   END
END updateCircle ;


(*
   updateOb - 
*)

PROCEDURE updateOb (optr: Object; dt: REAL) ;
BEGIN
   WITH optr^ DO
      IF NOT fixed
      THEN
         CASE object OF

         polygonOb:  | (* updatePolygon(p) | *)
         circleOb :  updateCircle(optr, dt) |
         pivotOb  :  |

         END
      END
   END
END updateOb ;


(*
   updatePhysics - updates all positions of objects based on the passage of
                   dt seconds.
*)

PROCEDURE updatePhysics (dt: REAL) ;
VAR
   i, n: CARDINAL ;
   optr: Object ;
BEGIN
   n := HighIndice(objects) ;
   i := 1 ;
   WHILE i<=n DO
      optr := GetIndice(objects, i) ;
      updateOb(optr, dt) ;
      INC(i)
   END
END updatePhysics ;


(*
   doNextEvent - 
*)

PROCEDURE doNextEvent () : REAL ;
VAR
   e : eventQueue ;
   dt: REAL ;
   p : PROC ;
BEGIN
   IF eventQ=NIL
   THEN
      HALT
   ELSE
      e := eventQ ;
      eventQ := eventQ^.next ;
      dt := e^.time ;
      p  := e^.p ;
      e^.next := freeEvents ;
      freeEvents := e ;
      updatePhysics(dt) ;
      p ;
      RETURN( dt )
   END
END doNextEvent ;


(*
   simulateFor - render for, t, seconds.
*)

PROCEDURE simulateFor (t: REAL) ;
VAR
   s, dt: REAL ;
BEGIN
   s := 0.0 ;
   killQueue ;
   addEvent(0.0, drawFrameEvent) ;
   (* addCollisionEvent ; *)
   WHILE s<t DO
      dt := doNextEvent() ;
      s := s + dt
   END ;
   produceAVI(TRUNC(framesPerSecond))
END simulateFor ;


(*
   newEvent - 
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
   RETURN( e )
END newEvent ;


(*
   addRelative - adds event, e, into the relative event queue.
*)

PROCEDURE addRelative (e: eventQueue) ;
VAR
   before, after: eventQueue ;
BEGIN
   IF eventQ=NIL
   THEN
      eventQ := e
   ELSIF e^.time<eventQ^.time
   THEN
      eventQ^.time := eventQ^.time - e^.time ;
      e^.next := eventQ ;
      eventQ := e
   ELSE
      before := eventQ ;
      after := eventQ^.next ;
      WHILE (after#NIL) AND (after^.time<e^.time) DO
         before := after ;
         after := after^.next
      END ;
      IF after#NIL
      THEN
         after^.time := after^.time-e^.time
      END ;
      e^.time := e^.time-before^.time ;
      before^.next := e ;
      e^.next := after
   END
END addRelative ;


(*
   addEvent - 
*)

PROCEDURE addEvent (t: REAL; dop: PROC) ;
VAR
   e: eventQueue ;
BEGIN
   e := newEvent() ;
   WITH e^ DO
      time := t ;
      p := dop ;
      next := NIL
   END ;
   addRelative(e)
END addEvent ;


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
   Init - 
*)

PROCEDURE Init ;
BEGIN
   maxId := 0 ;
   objects := InitIndex(1) ;
   framesPerSecond := DefaultFramesPerSecond ;
   simulatedGravity := 0.0 ;
   eventQ := NIL ;
   freeEvents := NIL
END Init ;


BEGIN
   Init
END twoDsim.
