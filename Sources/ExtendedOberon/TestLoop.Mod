MODULE^ TestLoop;  (*test LOOP statement / ^ after MODULE enables the retro compiler / AP 31.12.20 Extended Oberon*)
  IMPORT Out;

  PROCEDURE Go*;
    VAR i, j: INTEGER;
  BEGIN i := 0; j := 0;
    LOOP
      IF i < 5 THEN Out.Int(i, 2); Out.Str(" "); INC(i); j := 0;
        LOOP
          IF j < 3 THEN Out.Str("*"); INC(j)
          ELSE EXIT
          END
        END
      ELSE EXIT
      END
    END
  END Go;

END TestLoop.

ORP.Compile TestLoop.Mod/s ~
TestLoop.Go ~
