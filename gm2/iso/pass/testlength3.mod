MODULE testlength3 ;

FROM libc IMPORT exit ;

VAR
   a: ARRAY [0..20] OF CHAR ;
   l: CARDINAL ;
BEGIN
   a := "hello world" ;
   l := LENGTH(a) ;
   IF l=11
   THEN
      (* do nothing *)
   ELSE
      exit(1)
   END ;
   a := "world" ;
   l := LENGTH(a) ;
   IF l=5
   THEN
      (* do nothing *)
   ELSE
      exit(1)
   END
END testlength3.
