MODULE caseiso ;

VAR r1 : RECORD CASE :CARDINAL OF   (* case without tag field has this 
syntax by ISO *)
           0 : v1 : CARDINAL;
         | 1 : v2 : INTEGER;
         ELSE END END;

VAR r2 : RECORD CASE CARDINAL OF
         | 0 : v1 : CARDINAL;       (* pipe is allowed before first 
record field by ISO *)
         | 1 : v2 : INTEGER;
         ELSE END END;

BEGIN
END caseiso.
