MODULE^ TestLoop;  (*test LOOP statement / ^ after MODULE enables the retro compiler / AP 11.1.22 Extended Oberon*)
  IMPORT Out;

  PROCEDURE Go*;
    VAR i, j: INTEGER;
  BEGIN i := 0; j := 0;
    LOOP
      IF i < 5 THEN Out.Int(i, 2); Out.String(" "); INC(i); j := 0;
        LOOP
          IF j < 3 THEN Out.String("*"); INC(j)
          ELSE EXIT
          END
        END
      ELSE EXIT
      END
    END
  END Go;

END TestLoop.

ORP.Compile ORS.Mod/s ORB.Mod/s ORG.Mod/s ORP.Mod/s ~   # compile the new compiler
System.Free ORTool ORP ORG ORB ORS ~                    # unload the old compiler

ORP.Compile TestLoop.Mod/s ~
TestLoop.Go ~
