MODULE str6 ;
 
FROM StrIO IMPORT WriteString, WriteLn;
FROM StrLib IMPORT StrEqual ;
FROM M2RTS IMPORT Terminate ;
FROM libc IMPORT exit ;
 
TYPE
   String = ARRAY [0..255] OF CHAR;
 
VAR
   Str : String;
BEGIN
   Str := 'abcdefghij';
   WriteString(Str);
   WriteLn;
   Str := '1234';
   WriteString(Str);
   WriteLn ;
   IF NOT StrEqual(Str, '1234')
   THEN
      Terminate ;
      exit(1)
   END
END str6.
