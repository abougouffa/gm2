MODULE loopexit;

VAR a,b,c,d,e : BOOLEAN;
    i : CARDINAL;
BEGIN
  LOOP
    IF b THEN
      EXIT;
    ELSE
      IF c THEN
        EXIT;
      ELSE
        d := e;
      END;
    END;
    EXIT;
  END;

  FOR i := 0 TO 1 DO
    EXIT;
  END;

  WHILE a DO
    EXIT;
  END;

  REPEAT
    EXIT;
  UNTIL b;
END loopexit.
