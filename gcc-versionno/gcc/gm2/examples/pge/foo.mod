MODULE foo ;

IMPORT popWorld ;
IMPORT twoDsim ;

FROM deviceIf IMPORT Colour, red, blue, green, yellow, purple, defineColour ;
FROM Fractions IMPORT Fract, initFract, zero, one, two, cardinal, negate, mult, div, pi ;
FROM Points IMPORT Point, initPoint ;

FROM macroObjects IMPORT Macro, circle, moveTo, up, down, left, right, rotate,
                         append, translate, rootMacro, dup, unRootMacro, initMacro, rectangle,
                         triangle, angledRect ;


CONST
   testCorner = FALSE ;
   testRight  = FALSE ;
   useGroff   = FALSE ;


(*
   placeFixed - 
*)

PROCEDURE placeFixed ;
VAR
   m: Macro ;
BEGIN
   m := initMacro() ;
   m := moveTo(m, initPoint(zero(), initFract(0,98,100))) ;
   m := angledRect (m, TRUE, zero(), light,
              initPoint (initFract(0,97,100), zero()),
              initPoint (zero(), negate(initFract(0,2,100)))) ;
   m := moveTo(m, initPoint(initFract(0,2,100), initFract(0,3,100))) ;
   m := angledRect (m, TRUE, zero(), blue(),
              initPoint (initFract(0,97,100), zero()),
              initPoint (zero(), negate(initFract(0,2,100)))) ;
   m := moveTo(m, initPoint(one(), initFract(0,98,100))) ;
   m := angledRect (m, TRUE, zero(), red(),
              initPoint (zero(), negate(initFract(0,97,100))),
              initPoint (negate(initFract(0,3,100)), zero())) ;
   m := moveTo(m, initPoint(initFract(0,3,100), initFract(0,98,100))) ;
   m := angledRect (m, TRUE, zero(), green(),
              initPoint (zero(), negate(initFract(0,97,100))),
              initPoint (negate(initFract(0,3,100)), zero())) ;
   m := moveTo(m, initPoint(initFract(0,6,100), initFract(0,53,100))) ;
   m := angledRect (m, TRUE, zero(), purple(),
              initPoint (initFract(0,56,100), zero()),
              initPoint (zero(), negate(initFract(0,14,100)))) ;

   m := rootMacro(m) ;
   popWorld.populate(m, TRUE, TRUE) ;

END placeFixed ;


(*
   placeNonFixed -
*)

PROCEDURE placeNonFixed ;
VAR
   m: Macro ;
BEGIN
   m := initMacro() ;
   m := moveTo(m, initPoint(initFract(0,84,100), initFract(0,23,100))) ;
   m := circle (m, TRUE, zero(), dark, initFract(0, 3,100)) ;

   m := rootMacro(m) ;

   popWorld.mass(cardinal(1)) ;

   popWorld.velocity(initPoint(mult(initFract(10, 0, 1),negate(initFract(0,13,100))), mult(initFract(10, 0, 1),initFract(0,35,100)))) ;
   popWorld.populate(m, FALSE, TRUE) ;

END placeNonFixed ;


VAR
   light, dark: Colour ;
BEGIN
   popWorld.init(useGroff) ;
   light := defineColour(initFract(0, 166, 256),
                         initFract(0, 124, 256),
                         initFract(0, 54, 256)) ;
   dark := defineColour(initFract(0, 76, 256),
                        initFract(0, 47, 256),
                        zero()) ;

   placeFixed ;
   twoDsim.gravity(-9.81) ;

   placeNonFixed ;

   twoDsim.simulateFor(25.0)
END foo.
