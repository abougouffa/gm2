MODULE addadr1;

FROM SYSTEM IMPORT BYTE, ADDRESS, ADR, ADDADR, CAST;

VAR pbyte : POINTER TO BYTE;
    pchar : POINTER TO CHAR;
    byte : BYTE;
    ch : CHAR;

    a : ADDRESS;

BEGIN
  pbyte := ADR(byte);
  pchar := ADR(ch);

  pbyte := ADDADR(pbyte,SIZE(BYTE));
  pchar := ADDADR(pchar,SIZE(CHAR));

  a := ADDADR(pchar,SIZE(CHAR));

  a := ADDADR(CAST(ADDRESS,pchar),SIZE(CHAR));
END addadr1.

