MODULE record4 ;

TYPE
   rec = RECORD
            i: INTEGER
         END ;

PROCEDURE Expression (r: rec): BOOLEAN;
BEGIN
  RETURN TRUE
END Expression ;

TYPE
   newrec =  rec ;

PROCEDURE Term (t: newrec) ;
BEGIN
   WHILE NOT Expression(t) DO
   END
END Term ;

VAR
   n: newrec ;
BEGIN
   Term(n)
END record4.
