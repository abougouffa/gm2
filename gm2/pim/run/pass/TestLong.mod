MODULE TestLong;

FROM StrIO IMPORT WriteString, WriteLn;
FROM FpuIO IMPORT StrToLongInt, WriteLongInt, LongIntToStr;
FROM StrLib IMPORT StrEqual ;
FROM M2RTS IMPORT ExitOnHalt ;

TYPE
   String = ARRAY [0..255] OF CHAR;

VAR
   CorrectResult,
   LongIntegerVariable : LONGINT;
   SameResult,
   St                  : String;

BEGIN
   LongIntegerVariable := 12345678901234;
   WriteLongInt(LongIntegerVariable, 0);
   WriteLn;
   St := '12345678901234';
   WriteString(St);
   WriteLn;
   LongIntToStr(LongIntegerVariable, 0, SameResult) ;
   StrToLongInt(St, CorrectResult) ;
   WriteLongInt(CorrectResult, 0);
   WriteLn ;
   IF NOT StrEqual(St, SameResult)
   THEN
      WriteString('test failed: correct value is: ') ; WriteString(St) ;
      WriteString(' assignment produced ') ; WriteString(SameResult) ;
      WriteLn ;
      ExitOnHalt(1) ;
      HALT
   END
END TestLong.
