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
   s := ColourSet{red, green} ;
   first(s) ;
   s := ColourSet{red, blue} ;
   s := ColourSet{red, blue, green}
END set5.
