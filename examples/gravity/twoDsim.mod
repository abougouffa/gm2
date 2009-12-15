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

FROM SYSTEM IMPORT ADR ;
FROM Storage IMPORT ALLOCATE ;
FROM Indexing IMPORT Index, InitIndex, PutIndice, GetIndice, HighIndice ;
FROM libc IMPORT printf ;
FROM deviceGnuPic IMPORT Coord, Colour, newFrame, renderFrame, circleFrame, polygonFrame, produceAVI ;
FROM libm IMPORT sqrt ;
FROM gsl IMPORT gsl_poly_complex_workspace, gsl_poly_complex_solve, gsl_poly_eval,
                gsl_poly_complex_workspace_alloc, gsl_poly_complex_workspace_free ;


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

   eventProc = PROCEDURE (eventQueue) ;

   eventQueue = POINTER TO RECORD
                              time: REAL ;
                              p   : eventProc ;
                              id1,
                              id2 : CARDINAL ;
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
      p.pos.x := x0 ;
      p.pos.y := y0 ;
      p.nPoints := 4 ;
      p.points[0].x := i ;
      p.points[0].y := 0.0 ;
      p.points[1].x := 0.0 ;
      p.points[1].y := j ;
      p.points[2].x := -i ;
      p.points[2].y := 0.0 ;
      p.points[3].x := 0.0 ;
      p.points[3].y := -j ;
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
   getColour - 
*)

PROCEDURE getColour (i: CARDINAL; e: eventQueue) : Colour ;
VAR
   p: eventProc ;
BEGIN
   p := debugFrame ;
   IF (i=e^.id1) OR (i=e^.id2)
   THEN
      IF e^.p=p
      THEN
         RETURN blue
      ELSE
         RETURN red
      END
   ELSE
      RETURN black
   END
END getColour ;


(*
   drawFrame - 
*)

PROCEDURE drawFrame (e: eventQueue) ;
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

         circleOb :  circleFrame(c.pos, c.r, getColour(i, e)) |
         polygonOb:  polygonFrame(p.pos, p.nPoints, p.points, getColour(i, e)) |
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

PROCEDURE drawFrameEvent (e: eventQueue) ;
BEGIN
   drawFrame(e) ;
   addEvent(1.0/framesPerSecond, drawFrameEvent)
END drawFrameEvent ;


(*
   updatePolygon - 
*)

PROCEDURE updatePolygon (optr: Object; dt: REAL) ;
VAR
   vn: REAL ;
BEGIN
   WITH optr^ DO
      (* update vx and pos.x *)
      vn := vx + ax*dt ;
      p.pos.x := c.pos.x+dt*(vx+vn)/2.0 ;
      vx := vn ;
      (* update vy and pos.y *)
      vn := vy + (ay+simulatedGravity)*dt ;
      p.pos.y := c.pos.y+dt*(vy+vn)/2.0 ;
      vy := vn
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

         polygonOb:  updatePolygon(optr, dt) |
         circleOb :  updateCircle(optr, dt) |
         pivotOb  :  |

         END
      END
   END
END updateOb ;


(*
   updatePhysics - updates all positions of objects based on the passing of
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
   p : eventProc ;
BEGIN
   IF eventQ=NIL
   THEN
      HALT
   ELSE
      e := eventQ ;
      eventQ := eventQ^.next ;
      dt := e^.time ;
      p  := e^.p ;
      updatePhysics(dt) ;
      p(e) ;
      e^.next := freeEvents ;
      freeEvents := e ;
      RETURN( dt )
   END
END doNextEvent ;


(*
   circleCollision - 
*)

PROCEDURE circleCollision (iptr, jptr: Object) ;
BEGIN
   IF NOT iptr^.fixed
   THEN
      iptr^.vy := -iptr^.vy ;
      iptr^.ay := -iptr^.ay 
   END ;
   IF NOT jptr^.fixed
   THEN
      jptr^.vy := -jptr^.vy ;
      jptr^.ay := -jptr^.ay 
   END
END circleCollision ;


(*
   physicsCollision - handle the physics of a collision between
                      the two objects defined in, e.
*)

PROCEDURE physicsCollision (e: eventQueue) ;
VAR
   iptr, jptr: Object ;
BEGIN
   iptr := GetIndice(objects, e^.id1) ;
   jptr := GetIndice(objects, e^.id2) ;
   IF (iptr^.object=circleOb) AND (jptr^.object=circleOb)
   THEN
      circleCollision(iptr, jptr)
   END
END physicsCollision ;


(*
   doCollision - 
*)

PROCEDURE doCollision (e: eventQueue) ;
BEGIN
   drawFrameEvent(e) ;
   physicsCollision(e) ;
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
   findCollisionCircles - 

   using:

   S = UT + AT^2/2
   compute xin and yin which are the new (x,y) positions of object i at time, t.
   compute xjn and yjn which are the new (x,y) positions of object j at time, t.
   now compute difference between objects and if they are ri+rj  (radius of circle, i, and, j)
   appart then we have a collision at time, t.

   xin = xi + vxi * t + aix * t^2 / 2.0
   yin = yi + vyi * t + aiy * t^2 / 2.0

   xjn = xj + vxj * t + ajx * t^2 / 2.0
   yjn = yj + vyj * t + ajy * t^2 / 2.0

   ri + rj == sqrt(abs(xin-xjn)^2 + abs(yin-yjn)^2)     for values of t

   ri + rj == sqrt((xi + vxi * t + aix * t^2 / 2.0 - xj + vxj * t + ajx * t^2 / 2.0)^2 +
                   (yi + vyi * t + aiy * t^2 / 2.0 - yj + vyj * t + ajy * t^2 / 2.0)^2)

===
2nd attempt
-----------

   (ri + rj)^2 ==  (xi + vxi * t + aix * t^2 / 2.0 - xj + vxj * t + ajx * t^2 / 2.0)^2 +
                   (yi + vyi * t + aiy * t^2 / 2.0 - yj + vyj * t + ajy * t^2 / 2.0)^2

   0           ==  (xi + vxi * t + aix * t^2 / 2.0 - xj + vxj * t + ajx * t^2 / 2.0)^2 +
                   (yi + vyi * t + aiy * t^2 / 2.0 - yj + vyj * t + ajy * t^2 / 2.0)^2 -
                   (ri + rj)^2

   0           ==  (a + c * t + e * t^2.0/2.0 - b * d * t + f * t^2.0/2.0)^2.0 +
                   (g + k * t + m * t^2.0/2.0 - h + l * t + n * t^2.0/2.0)^2.0 -
                   (o + p)^2.0


   0           ==  ((n^2+2*m*n+m^2+f^2+2*e*f+e^2) * t^4 +
                   ((4*l+4*k)*n+(4*l+4*k)*m+(4*c-4*b*d)*f+(4*c-4*b*d)*e) * t^3 +
                   ((4*g-4*h)*n+(4*g-4*h)*m+4*l^2+8*k*l+4*k^2+4*a*f+4*a*e+4*b^2*d^2-8*b*c*d+4*c^2) * t^2 +
                   ((8*g-8*h)*l+(8*g-8*h)*k-8*a*b*d+8*a*c) * t -
                    4*p^2-8*o*p-4*o^2+4*h^2-8*g*h+4*g^2+4*a^2)/4

  A = (n^2+2*m*n+m^2+f^2+2*e*f+e^2)
  B = (4*l+4*k)*n+(4*l+4*k)*m+(4*c-4*b*d)*f+(4*c-4*b*d)*e
  C = (4*g-4*h)*n+(4*g-4*h)*m+4*l^2+8*k*l+4*k^2+4*a*f+4*a*e+4*b^2*d^2-8*b*c*d+4*c^2
  D = (8*g-8*h)*l+(8*g-8*h)*k-8*a*b*d+8*a*c
  E = 4*p^2-8*o*p-4*o^2+4*h^2-8*g*h+4*g^2+4*a^2

===
1st attempt
-----------

   ri + rj == sqrt(((xi - xj) + (vxi - vxj) * t + (aix - ajx) * t^2 / 2.0)^2 +
                    (yi - yj) + (vyi - vyj) * t + (aiy - ajy) * t^2 / 2.0)^2)

   (ri + rj)^2 = ((xi - xj) + (vxi - vxj) * t + (aix - ajx) * t^2 / 2.0)^2 +
                 ((yi - yj) + (vyi - vyj) * t + (aiy - ajy) * t^2 / 2.0)^2

   0           =  ((xi - xj) + (vxi - vxj) * t + (aix - ajx) * t^2 / 2.0)^2 +
                  ((yi - yj) + (vyi - vyj) * t + (aiy - ajy) * t^2 / 2.0)^2 -
                  (ri + rj)^2

   expand and collect terms of t:

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

   now using wxmaxima

   (((a-b)+(c-d)*t+((e-f)*t^2)/2)^2) +
   (((g-h)+(k-l)*t+((m-n)*t^2)/2)^2) -
   (o-p)^2;

   expand ; factor ; ratsimp

   we get:

   ((n^2-2*m*n+m^2+f^2-2*e*f+e^2) * t^4 +
   ((4*l-4*k)*n+(4*k-4*l)*m+(4*d-4*c)*f+(4*c-4*d)*e) * t^3 +
   ((4*h-4*g)*n+(4*g-4*h)*m+4*l^2-8*k*l+4*k^2+(4*b-4*a)*f+(4*a-4*b)*e+4*d^2-8*c*d+4*c^2) * t^2 +
   ((8*h-8*g)*l+(8*g-8*h)*k+(8*b-8*a)*d+(8*a-8*b)*c) * t -
   4*p^2+8*o*p-4*o^2+4*h^2-8*g*h+4*g^2+4*b^2-8*a*b+4*a^2)/4  =  0

   solve for t:

   (multiply both sides by 4)

   (n^2-2*m*n+m^2+f^2-2*e*f+e^2) * t^4 +
   ((4*l-4*k)*n+(4*k-4*l)*m+(4*d-4*c)*f+(4*c-4*d)*e) * t^3 +
   ((4*h-4*g)*n+(4*g-4*h)*m+4*l^2-8*k*l+4*k^2+(4*b-4*a)*f+(4*a-4*b)*e+4*d^2-8*c*d+4*c^2) * t^2 +
   ((8*h-8*g)*l+(8*g-8*h)*k+(8*b-8*a)*d+(8*a-8*b)*c) * t -
   4*p^2+8*o*p-4*o^2+4*h^2-8*g*h+4*g^2+4*b^2-8*a*b+4*a^2  =  0

   solve polynomial:

   A = (n^2-2*m*n+m^2+f^2-2*e*f+e^2)
   B = ((4*l-4*k)*n+(4*k-4*l)*m+(4*d-4*c)*f+(4*c-4*d)*e)
   C = ((4*h-4*g)*n+(4*g-4*h)*m+4*l^2-8*k*l+4*k^2+(4*b-4*a)*f+(4*a-4*b)*e+4*d^2-8*c*d+4*c^2)
   D = ((8*h-8*g)*l+(8*g-8*h)*k+(8*b-8*a)*d+(8*a-8*b)*c)
   E = 4*p^2+8*o*p-4*o^2+4*h^2-8*g*h+4*g^2+4*b^2-8*a*b+4*a^2

   using the GNU scientific library.
*)

PROCEDURE findCollisionCircles (iptr, jptr: Object; VAR ic, jc: CARDINAL; VAR tc: REAL) ;
TYPE
   arrayReal5 = ARRAY [0..4] OF REAL ;
VAR
   a, b, c, d, e,
   f, g, h, k, l,
   m, n, o, p, t,
   A, B, C, D, E: REAL ;
   V            : arrayReal5 ;
   R            : ARRAY [0..7] OF REAL ;
   W            : gsl_poly_complex_workspace ;
   i, j         : CARDINAL ;
BEGIN
   WITH iptr^ DO
      a := c.pos.x    (* xi *)
   END ;
   c := iptr^.vx ;    (* vxi *)
   WITH iptr^ DO
      IF fixed
      THEN
         e := 0.0 ;   (* aix *)
         m := 0.0     (* aiy *)
      ELSE
         e := ax ;    (* aix *)
         m := ay+simulatedGravity    (* aiy *)
      END ;
      g := c.pos.y ;  (* yi *)
      k := vy ;       (* vyi *)
      o := c.r        (* ri *)
   END ;

   WITH jptr^ DO
      b := c.pos.x ;  (* xj *)
      IF fixed
      THEN
         f := 0.0 ;  (* ajx *)
         n := 0.0    (* ajy *)
      ELSE
         f := ax ;   (* ajx *)
         n := ay+simulatedGravity  (* ajy *)
      END ;
      d := vx ;      (* vxj *)
      h := c.pos.y ; (* yj *)
      l := vy        (* vyj *)
   END ;
   p := jptr^.c.r ;  (* rj *)
(* 1st attempt *)
(*
   A := sqr(n) -2.0*m*n+ sqr(m) + sqr(f) -2.0*e*f + sqr(e) ;
   B := (4.0*l-4.0*k)*n+(4.0*k-4.0*l)*m+(4.0*d-4.0*c)*f+(4.0*c-4.0*d)*e ;
   C := (4.0*h-4.0*g)*n+(4.0*g-4.0*h)*m+4.0 * sqr(l)-8.0*k*l+4.0 * sqr(k) +
        (4.0*b-4.0*a)*f+(4.0*a-4.0*b)*e+4.0 * sqr(d) -8.0*c*d+4.0 * sqr(c) ;
   D := (8.0*h-8.0*g)*l+(8.0*g-8.0*h)*k+(8.0*b-8.0*a)*d+(8.0*a-8.0*b)*c ;
   E := 4.0 * sqr(p) + 8.0*o*p-4.0 * sqr(o) + 4.0 * sqr(h)
        -8.0*g*h+4.0 * sqr(g) +4.0 * sqr(b) -8.0*a*b+4.0* sqr(a) ;
*)
   (* 2nd attempt *)

   A := sqr(n)+2.0*m*n+sqr(m)+sqr(f)+2.0*e*f+sqr(e) ;
   B := (4.0*l+4.0*k)*n+(4.0*l+4.0*k)*m+(4.0*c-4.0*b*d)*f+(4.0*c-4.0*b*d)*e ;
   C := (4.0*g-4.0*h)*n+(4.0*g-4.0*h)*m+4.0*sqr(l)+8.0*k*l+4.0*sqr(k)+4.0*a*f+4.0*a*e+4.0*sqr(b)*sqr(d)-8.0*b*c*d+4.0*sqr(c) ;
   D := (8.0*g-8.0*h)*l+(8.0*g-8.0*h)*k-8.0*a*b*d+8.0*a*c ;
   E := 4.0*sqr(p)-8.0*o*p-4.0*sqr(o)+4.0*sqr(h)-8.0*g*h+4.0*sqr(g)+4.0*sqr(a) ;

   (* now solve for values of t which satisfy   At^4 + Bt^3 + Ct^2 + Dt^1 + Et^0 = 0 *)
   IF A=0.0
   THEN
      IF B=0.0
      THEN
         IF C=0.0
         THEN
            IF D=0.0
            THEN
               IF E=0.0
               THEN
                  tc := 0.0 ;
                  ic := iptr^.id ;
                  jc := jptr^.id ;
                  RETURN
               ELSE
                  (*
                  V[0] := E ;
                  j := 1 ;
                  *)
                  tc := 0.0 ;
                  ic := iptr^.id ;
                  jc := jptr^.id ;
                  RETURN
               END
            ELSE
               V[0] := E ;
               V[1] := D ;
               j := 2
            END
         ELSE
            V[0] := E ;
            V[1] := D ;
            V[2] := C ;
            j := 3
         END
      ELSE
         V[0] := E ;
         V[1] := D ;
         V[2] := C ;
         V[3] := B ;
         j := 4
      END
   ELSE
      V[0] := E ;
      V[1] := D ;
      V[2] := C ;
      V[3] := B ;
      V[4] := A ;
      j := 5
   END ;

   W := gsl_poly_complex_workspace_alloc (j);
   gsl_poly_complex_solve (ADR(V), j, W, ADR(R));
   gsl_poly_complex_workspace_free (W);

   FOR i := 0 TO j-2 DO
      t := sqrt(sqr(R[i*2]) + sqr(R[i*2+1])) ;
      printf("A = %g\n", A);
      printf("B = %g\n", B);
      printf("C = %g\n", C);
      printf("D = %g\n", D);
      printf("E = %g\n", E);
      printf("t = %g\n", t);
      printf("yields a value of At^4 + Bt^3 +Ct^2 + Dt + E = %g  == %g  ?\n",
             gsl_poly_eval(ADR(V), 5, t), t) ;
      t := 0.34974 ;
      printf("should yield a value of At^4 + Bt^3 +Ct^2 + Dt + E = %g   == %g ??? \n",
             gsl_poly_eval(ADR(V), 5, t), t) ;

      IF (R[i*2]>=0.0) AND (R[i*2+1]>=0.0)
      THEN
         t := sqrt(sqr(R[i*2]) + sqr(R[i*2+1])) ;
         (* remember tc is -1.0 initially, to force it to be set once *)
         IF (tc<0.0) OR (t<tc)
         THEN
            tc := t ;
            ic := iptr^.id ;
            jc := jptr^.id
         END
      END
   END
END findCollisionCircles ;


(*
   findCollision - 
*)

PROCEDURE findCollision (iptr, jptr: Object; VAR ic, jc: CARDINAL; VAR tc: REAL) ;
BEGIN
   IF NOT ((iptr^.fixed) AND (jptr^.fixed))
   THEN
      IF (iptr^.object=circleOb) AND (jptr^.object=circleOb)
      THEN
         findCollisionCircles(iptr, jptr, ic, jc, tc)
      END
   END
END findCollision ;


(*
   debugFrame - debug frame at time, e.
*)

PROCEDURE debugFrame (e: eventQueue) ;
BEGIN
   drawFrame(e) ;
END debugFrame ;


(*
   addDebugging - add a debugging event at time, t, which colours objects,
                  a, and, b, blue.
*)

PROCEDURE addDebugging (t: REAL; a, b: CARDINAL) ;
VAR
   e: eventQueue ;
BEGIN
   e := newEvent() ;
   WITH e^ DO
      time := t ;
      p := debugFrame ;
      id1 := a ;
      id2 := b ;
      next := NIL
   END ;
   addRelative(e)
END addDebugging ;


(*
   addNextCollisionEvent - 
*)

PROCEDURE addNextCollisionEvent ;
VAR
   tc        : REAL ;
   ic, jc,
   i, j, n   : CARDINAL ;
   iptr, jptr: Object ;
BEGIN
   n := HighIndice(objects) ;
   i := 1 ;
   tc := -1.0 ;
   ic := n+1 ;
   jc := n+1 ;
   WHILE i<=n DO
      iptr := GetIndice(objects, i) ;
      j := i+1 ;
      WHILE j<=n DO
         jptr := GetIndice(objects, j) ;
         findCollision(iptr, jptr, ic, jc, tc) ;
         INC(j)
      END ;
      INC(i)
   END ;
   IF tc>=0.0
   THEN
      addCollisionEvent(tc, doCollision, ic, jc)
   ELSE
      printf("no more collisions found\n")
   END
END addNextCollisionEvent ;


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
   addNextCollisionEvent ;
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

PROCEDURE addEvent (t: REAL; dop: eventProc) ;
VAR
   e: eventQueue ;
BEGIN
   e := newEvent() ;
   WITH e^ DO
      time := t ;
      p := dop ;
      id1 := 0 ;
      id2 := 0 ;
      next := NIL
   END ;
   addRelative(e)
END addEvent ;


(*
   addCollisionEvent - 
*)

PROCEDURE addCollisionEvent (t: REAL; dop: eventProc; a, b: CARDINAL) ;
VAR
   e: eventQueue ;
BEGIN
   printf("collision will occur in %g simulated seconds\n", t) ;
   e := newEvent() ;
   WITH e^ DO
      time := t ;
      p := dop ;
      id1 := a ;
      id2 := b ;
      next := NIL
   END ;
   addRelative(e)
END addCollisionEvent ;


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
