MODULE hello ;

FROM libprintf IMPORT printf ;

BEGIN
   printf("hello %s", "world\n")
END hello.
