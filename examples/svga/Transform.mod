IMPLEMENTATION MODULE Transform ;

FROM RealMath IMPORT sin, cos ;
FROM Matrix3D IMPORT Matrix, MatrixValue, Init, Set ;


(*
   Reflect - returns a Matrix representing the reflect
             transformation in line: y = mx + c.
*)

PROCEDURE Reflect (m, x, c: REAL) : Matrix ;
VAR
   m1: Matrix ;
BEGIN
   RETURN( m1 )
END Reflect ;


(*
   Rotate - returns a Matrix representing the rotate
            transformation about 0, 0 with r Radians.
*)

PROCEDURE Rotate (r: REAL) : Matrix ;
VAR
   m: Matrix ;
   v: MatrixValue ;
BEGIN
   v[1, 1] := cos(r) ;   v[1, 2] := -sin(r) ;   v[1, 3] := 0.0 ;
   v[2, 1] := sin(r) ;   v[2, 2] :=  cos(r) ;   v[2, 3] := 0.0 ;
   v[3, 1] := 0.0    ;   v[3, 2] :=  0.0    ;   v[3, 3] := 1.0 ;
   RETURN( Set( Init(), v ) )
END Rotate ;


(*
   Scale - returns a Matrix representing the scale
           transformation by x, y.
*)

PROCEDURE Scale (x, y: REAL) : Matrix ;
VAR
   m: Matrix ;
   v: MatrixValue ;
BEGIN
   v[1, 1] := x      ;   v[1, 2] :=  0.0    ;   v[1, 3] := 0.0 ;
   v[2, 1] := 0.0    ;   v[2, 2] :=  y      ;   v[2, 3] := 0.0 ;
   v[3, 1] := 0.0    ;   v[3, 2] :=  0.0    ;   v[3, 3] := 1.0 ;
   RETURN( Set( Init(), v ) )
END Scale ;


(*
   Translate - returns a Matrix representing the translate
               transformation by x, y.
*)

PROCEDURE Translate (x, y: REAL) : Matrix ;
VAR
   m: Matrix ;
   v: MatrixValue ;
BEGIN
   v[1, 1] := 1.0    ;   v[1, 2] :=  0.0    ;   v[1, 3] := 0.0 ;
   v[2, 1] := 0.0    ;   v[2, 2] :=  1.0    ;   v[2, 3] := 0.0 ;
   v[3, 1] := x      ;   v[3, 2] :=  y      ;   v[3, 3] := 1.0 ;
   RETURN( Set( Init(), v ) )
END Translate ;


END Transform.
