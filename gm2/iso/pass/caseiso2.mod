MODULE caseiso2 ;

FROM SYSTEM IMPORT SIZE ;
FROM libc IMPORT exit ;

VAR
   r1 : RECORD
           CASE :CARDINAL OF   (* case without tag field has this 
                                syntax by ISO *)
             0 : v1 : CARDINAL;
           | 1 : v2 : INTEGER;
           ELSE
           END
        END;

VAR
   r2 : RECORD
           CASE CARDINAL OF
           | 0 : v1 : CARDINAL;       (* pipe is allowed before first 
                                     record field by ISO *)
           | 1 : v2 : INTEGER;
           ELSE
           END
        END;

VAR
   r3: RECORD
          v1: CARDINAL;
       END ;
BEGIN
   r1.v2 := -1 ;
   r2.v2 := -1 ;
   IF SIZE(r1)#SIZE(r2)
   THEN
      exit(1)
   END ;
   IF SIZE(r1)#SIZE(r3)
   THEN
      exit(2)
   END
END caseiso2.
