MODULE Countdown;
(*
 * FOR loop marches down to the bottom of the earth.
 *
 * WARNING: CODE CARRIES NO WARRATY! USE AT OWN RISK!
 *)
IMPORT StrIO, NumberIO, libc ;

VAR
   x, n :CARDINAL;
BEGIN
   x := 0 ;
   FOR n := 10 TO 0 BY -1 DO
      NumberIO.WriteCard(n, 4); StrIO.WriteLn ;
      INC(x) ;
      IF x>20
      THEN
         StrIO.WriteString('FOR BY -1 test failed') ;
         StrIO.WriteLn ;
         libc.exit(1)
      END
   END (*FOR*)
END Countdown.
