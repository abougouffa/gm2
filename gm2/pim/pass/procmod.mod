MODULE procmod ;

FROM StdIO IMPORT Write ;

PROCEDURE proc ;

   MODULE mod ;
   IMPORT Write ;
   BEGIN
      Write('b')
   END mod ;

BEGIN
   Write('c')
END proc ;

BEGIN
   Write('a') ;
   proc
END procmod.
