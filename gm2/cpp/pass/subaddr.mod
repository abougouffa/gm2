MODULE subaddr;

FROM SYSTEM IMPORT ADDRESS;

VAR
   x, y: ADDRESS;
   i   : CARDINAL;
BEGIN
#if !defined(__x86_64)
   i := CARDINAL(x - y)
#endif
END subaddr.
