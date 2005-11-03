MODULE subrange17 ;

TYPE
   colour = (blue, red, yellow, green) ;
   t = [blue..yellow] ;
VAR
   s: t ;
BEGIN
   s := blue ;
   s := red ;
   s := red ;
   s := red ;
   s := red ;
   s := red ;
   s := yellow
END subrange17.
