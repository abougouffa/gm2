MODULE set4 ;


TYPE
   colour = (red, blue, green) ;
   ColourSet = SET OF colour ;
VAR
   s: ColourSet ;
BEGIN
   s := ColourSet{red, green} ;
   s := ColourSet{red, blue} ;
   s := ColourSet{red, blue, green}
END set4.
