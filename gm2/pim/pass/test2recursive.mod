MODULE test2recursive ;

FROM StrIO IMPORT WriteString, WriteLn ;
FROM testrecursive IMPORT test_type;

VAR
   r: test_type ;
BEGIN
  WriteString('hello world') ; WriteLn;
END test2recursive.

