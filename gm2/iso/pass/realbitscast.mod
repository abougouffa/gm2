MODULE realbitscast;

FROM SYSTEM IMPORT CAST, WORD ;

TYPE
    BITS32 = SET OF [0..31];
    BITS64 = SET OF [0..63];
    BITS96 = SET OF [0..95] ;
    REAL32 = SHORTREAL;
    REAL64 = REAL;
#if !defined(__sparc__) && !defined(__x86_64)
    REAL96 = LONGREAL ;  (* on the __sparc__ SIZE(LONGREAL) = SIZE(REAL) *)
    (* and on the x86_64 LONGREAL is 128 bits *)
#endif

VAR
    b32 : BITS32;
    b64 : BITS64;
    r32 : REAL32;
    r64 : REAL64;
#if !defined(__sparc__) && !defined(__x86_64)
    b96 : BITS96 ;
    r96 : REAL96 ;
#endif
    w   : WORD ;
BEGIN
   r32 := 1.0 ;
   b32 := CAST(BITS32,r32) ;
   b64 := CAST(BITS64,r64) ;
#if !defined(__sparc__) && !defined(__x86_64)
   b96 := CAST(BITS96,r96)
#endif
END realbitscast.
