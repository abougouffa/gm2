IMPLEMENTATION MODULE impq;

IMPORT impc;

VAR
   a: C ;
   b: impc.C ;
BEGIN
   a := impc.red ;
   b := red ;
   a := b ;
   b := a
END impq.
