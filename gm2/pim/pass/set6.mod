MODULE set6 ;


TYPE
   colour = (red, blue, green) ;
   ColourSet = SET OF [red..green] ;

PROCEDURE first (c: ColourSet) ;
BEGIN
   IF red IN c
   THEN

   END   
END first ;


VAR
   s: ColourSet ;
BEGIN
   s := {red, green} ;
   first(s) ;
   s := {red, blue} ;
   s := {red, blue, green}
END set6.
