MODULE deftype ;

TYPE tL = RECORD m :INTEGER END;

TYPE tT = tL;

PROCEDURE T (t: tT) : INTEGER ;
BEGIN
   RETURN t.m
END T ;

BEGIN
END deftype .
