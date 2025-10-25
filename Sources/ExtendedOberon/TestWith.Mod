MODULE^ TestWith;  (*test WITH statement / ^ after MODULE enables the retro compiler / AP 11.1.22 Extended Oberon*)
  IMPORT Texts, Oberon;
  TYPE R0 = RECORD fld0: INTEGER END ;
    R1 = RECORD (R0) fld1: INTEGER END ;
    R2 = RECORD (R0) fld2: INTEGER END ;

    P0 = POINTER TO R0;
    P1 = POINTER TO R1;
    P2 = POINTER TO R2;

    A = ARRAY OF INTEGER;  (*open array*)
    P = POINTER TO A;

  VAR p: P; p0: P0; p1: P1; p2: P2; q, q0, q1, q2: P0;
    r0: R0; r1: R1; r2: R2;

    W: Texts.Writer;

  PROCEDURE Str*(s: ARRAY OF CHAR);
  BEGIN Texts.WriteString(W, s); Texts.Append(Oberon.Log, W.buf)
  END Str;

  PROCEDURE Ln*;
  BEGIN Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf)
  END Ln;

  PROCEDURE Proc1(p0: P0); BEGIN END Proc1;
  PROCEDURE Proc2(VAR p1: P1); BEGIN END Proc2;
  PROCEDURE Proc3(VAR r0: R0); BEGIN r0 := r2 (*projection*) END Proc3;
  PROCEDURE Proc4(r0: R0); BEGIN END Proc4;
  PROCEDURE Proc5(VAR i: INTEGER); BEGIN END Proc5;
  PROCEDURE Proc6(b: BOOLEAN); BEGIN END Proc6;

  PROCEDURE check();
    VAR r0, r1, r2: P0;
  BEGIN r0 := q0; r1 := q1; r2 := q2;
    WITH
      r2: P2 DO Str("r2 IS P2"); Ln; q0:= p0; q1 := p1; q2 := p0
    | r1: P1 DO Str("r1 IS P1"); Ln; q0:= p0; q1 := p0; q2 := p0
    | r0: P0 DO Str("r0 IS P0"); Ln; q0:= p0; q1 := p0; q2 := p2
    END
  END check;

  (*
  PROCEDURE check1();
    VAR p, pa, pb: P0; pc: P1;
  BEGIN p := p1; p.fld0 := 33; pa := p; pb := p1; pc := p1;
    WITH 
      p: P1 DO
        pa := p;
        Proc1(p);
        Proc2(p);          (*ERROR "read-only" - cannot pass a with variable of a pointer type as a VAR parameter*)
        Proc5(p.fld1);
        p := p2;           (*ERROR "read-only" - cannot modify a with variable through an assignment*)
        p.fld1 := 123;
        WITH
          p: P1 DO
            p := p1;       (*ERROR "read-only" - cannot modify a with variable through an assignment*)
            Proc1(p);
            Proc2(p)       (*ERROR "read-only" - cannot pass a with variable of a pointer type as a VAR parameter*)
        |pa: P0 DO
          pa := pb;        (*ERROR "read-only" - cannot modify a with variable through an assignment*)
          pb := pa;
          p := pa;         (*ERROR "read-only" - cannot modify an (outer!) with variable through an assignment*)
          pc := p1
        END ;
        p := p1            (*ERROR "read-only" - cannot modify a with variable through an assignment*)
    | pc: P1 DO
      p1 := pc;
      pc := p1             (*ERROR "read-only" - cannot modify a with variable through an assignment*)
    ELSE p := p1
    END
  END check1;
  *)

  PROCEDURE Go*;
  BEGIN check()(*; check1()*)
  END Go;

BEGIN Texts.OpenWriter(W); NEW(p, 100); NEW(p0); NEW(p1); NEW(p2); q0 := p0; q1 := p1; q2 := p2
END TestWith.

ORP.Compile ORS.Mod/s ORB.Mod/s ORG.Mod/s ORP.Mod/s ~   # compile the new compiler
System.Free ORTool ORP ORG ORB ORS ~                    # unload the old compiler

ORP.Compile TestWith.Mod/s ~
System.Free TestWith ~
TestWith.Go
