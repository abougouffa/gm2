MODULE realbitscast;

FROM SYSTEM IMPORT CAST, WORD ;

TYPE
    BITS32 = SET OF [0..31];
    BITS64 = SET OF [0..63];
    BITS96 = SET OF [0..95] ;
    REAL32 = SHORTREAL;
    REAL64 = REAL;
    REAL96 = LONGREAL ;

VAR
    b32 : BITS32;
    b64 : BITS64;
    r32 : REAL32;
    r64 : REAL64;
    b96: BITS96 ;
    r96: REAL96 ;
    w   : WORD ;
BEGIN
   r32 := 1.0 ;
   b32 := CAST(BITS32,r32) ;
   b64 := CAST(BITS64,r64) ;
   b96 := CAST(BITS96,r96)
END realbitscast.
