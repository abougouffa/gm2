MODULE set5 ;


TYPE
   colour = (red, blue, green) ;
   ColourSet = SET OF colour ;

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
END set5.
