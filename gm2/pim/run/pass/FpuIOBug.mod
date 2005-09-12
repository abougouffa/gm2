MODULE FpuIOBug;

FROM StrIO IMPORT WriteLn;
FROM FpuIO IMPORT WriteLongInt;

VAR
   i, j: LONGINT;
BEGIN
   i := MAX(LONGINT);
   WriteLongInt(i,0);
   WriteLn;
   j := MIN(LONGINT);
   WriteLongInt(j,0);
   WriteLn;
   j := MIN(LONGINT) + 1;
   WriteLongInt(j,0);
   WriteLn
END FpuIOBug.
