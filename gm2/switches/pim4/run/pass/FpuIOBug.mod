MODULE FpuIOBug;

(*
 *  This module tests the implementation of InOut by stress testing
 *  StringConvert (and checks to see that the -Wpim4 flag does cause
 *  any unexpected DIV and MOD results).
 *)

FROM StringConvert IMPORT LongIntegerToString ;
FROM DynamicStrings IMPORT String, InitString, EqualArray, KillString, string ;
FROM libc IMPORT exit, printf ;
FROM SYSTEM IMPORT ADR ;


PROCEDURE Assert (v: BOOLEAN; f: ARRAY OF CHAR; l: CARDINAL; e: ARRAY OF CHAR) ;
VAR
   r: INTEGER ;
BEGIN
   IF v
   THEN
      r := printf("successfully evaluated %s\n", ADR(e))
   ELSE
      r := printf("%s:%d assertion failed when evaluating %s\n", ADR(f), l, ADR(e)) ;
      res := 1
   END
END Assert ;


PROCEDURE WriteLongInt (x: LONGINT; n: CARDINAL) : String ;
BEGIN
   RETURN LongIntegerToString(x, n, ' ', FALSE, 10, TRUE)
END WriteLongInt ;


VAR
   s     : String ;
   r, res: INTEGER ;
BEGIN
   res := 0 ;

   s := WriteLongInt(MAX(LONGINT), 0) ;
   r := printf('result of MAX(LONGINT) = %s\n', string(s)) ;
   Assert(EqualArray(s, '9223372036854775807'), __FILE__, __LINE__,
          'MAX(LONGINT) in LongIntegerToString') ;
   s := KillString(s) ;

   s := WriteLongInt(MIN(LONGINT), 0) ;
   r := printf('result of MIN(LONGINT) = %s\n', string(s)) ;
   Assert(EqualArray(s, '-9223372036854775808'), __FILE__, __LINE__,
          'MIN(LONGINT) in LongIntegerToString') ;
   s := KillString(s) ;

   s := WriteLongInt(MIN(LONGINT)+1, 0) ;
   r := printf('result of MIN(LONGINT)+1 = %s\n', string(s)) ;
   Assert(EqualArray(s, '-9223372036854775807'), __FILE__, __LINE__,
          'MIN(LONGINT)+1 in itos') ;
   s := KillString(s) ;
   exit(res)
END FpuIOBug.
