MODULE testlength4 ;

FROM libc IMPORT exit ;

CONST
   X = '0123' ;
   Y = '456789' ;
   l = LENGTH(X+Y) ;

VAR
   i: CARDINAL ;
BEGIN
   i := l
END testlength4.
