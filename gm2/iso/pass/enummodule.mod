MODULE enummodule ;

MODULE m1 ;
EXPORT T1 ;
TYPE T1 = (red, blue, green) ;
END m1 ;

MODULE m2 ;
IMPORT T1 ;
EXPORT T2 ;
TYPE T2 = T1 ;
END m2 ;

CONST
   colour = m2.red ;

END enummodule.