MODULE set4 ;


TYPE
   colour = (red, blue, green) ;
   ColourSet = SET OF colour ;
VAR
   s: ColourSet ;
BEGIN
   s := {red, green} ;
   s := {red, blue} ;
   s := {red, blue, green}
END set4.
