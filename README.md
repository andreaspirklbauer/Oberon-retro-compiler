# Oberon-retro-compiler
A modified Oberon compiler which supports the LOOP, EXIT, WITH and multiple RETURN statements, as they have existed in the *original* versions of the Oberon and Oberon-2 languages defined around 1990.

This compiler implements a superset of the Extended Oberon compiler available at http://github.com/andreaspirklbauer/Oberon-extended, which itself adds some Oberon-2 language constructs (such as type-bound procedures) to the Oberon-07 language as defined at http://www.projectoberon.com (Project Oberon 2013).

This may help when porting *legacy* Oberon systems (e.g., Oberon V4) to Project Oberon 2013. However, we recommend to refactor all legacy code to use Oberon-07 language constructs only.

In order to invoke the retro compiler, the programmer must mark the source code of the module to be compiled with an arrow (^) immediately after the symbol MODULE:

    MODULE^ TestLoop;  (*the ^ character after the symbol MODULE enables the retro compiler*)
      PROCEDURE Go*;
        VAR i, j: INTEGER;
      BEGIN i := 0; j := 0;
        LOOP
          IF i < 5 THEN j := 0;
            LOOP
              IF j < 3 THEN INC(j) ELSE EXIT END
            END
          ELSE EXIT
          END
        END
      END Go;
    END TestLoop.

**PREREQUISITES**: A current version of Extended Oberon (http://github.com/andreaspirklbauer/Oberon-extended)

------------------------------------------------------
**IMPLEMENTED LANGUAGE CONSTRUCTS**

**1) LOOP and EXIT statements**

      LoopStatement = LOOP StatementSequence END.

Example:

      LOOP
        ReadInt(i);
        IF i < 0 THEN EXIT END;
        WriteInt(i)
      END

**2) WITH statement (Oberon-2 style regional type guard)**

      WithStatement = WITH Guard DO StatementSequence {"|" Guard DO StatementSequence}
                      [ELSE StatementSequence] END.
      Guard = Qualident ":" Qualident.

Example:

      WITH t0: Tree DO w := t1.width;
        | t1: CenterTree DO w := t1.width; h := t1.height
        | t2: SuperTree DO w := t2.width; h := t2.height; c := t2.subnode
      ELSE w := 0
      END

**3) Multiple RETURN statements in a single procedure**

      ReturnStatement = RETURN [Expression].

Example:

      PROCEDURE P(x): INTEGER;
      BEGIN
        IF x = 0 THEN RETURN 10
        ELSIF x = 1 THEN RETURN 100
        ELSE RETURN 1000
        END
      END P;

------------------------------------------------------
**DOWNLOADING AND BUILDING THE OBERON RETRO COMPILER**

Download the Oberon retro compiler from the [**Sources/ExtendedOberon**](Sources/ExtendedOberon) directory of this repository.

Convert the downloaded files to Oberon format (Oberon uses CR as line endings) using the command [**dos2oberon**](dos2oberon), also available in this repository (example shown for Mac or Linux):

     for x in *.Mod ; do ./dos2oberon $x $x ; done

Import the files to your Oberon system. If you use an emulator (e.g., **https://github.com/pdewacht/oberon-risc-emu**) to run the Oberon system, click on the *PCLink1.Run* link in the *System.Tool* viewer, copy the files to the emulator directory, and execute the following command on the command shell of your host system:

     cd oberon-risc-emu
     for x in *.Mod ; do ./pcreceive.sh $x ; sleep 1 ; done

Create the Oberon retro compiler:

     ORP.Compile ORS.Mod/s ORB.Mod/s ~
     ORP.Compile ORG.Mod/s ORP.Mod/s ~
     ORP.Compile ORL.Mod/s ORX.Mod/s ORTool.Mod/s ~
     System.Free ORTool ORP ORG ORB ORS ORL ORX ~

Compile any programs with LOOP, EXIT, WITH or multiple RETURNs that you may have.

------------------------------------------------------
**DIFFERENCES TO EXTENDED OBERON**

**ORS.Mod**

```diff
--- Oberon-extended/Sources/ORS.Mod	2021-01-05 11:12:18.000000000 +0100
+++ Oberon-retro-compiler/Sources/ExtendedOberon/ORS.Mod	2021-01-09 18:41:06.000000000 +0100
@@ -1,4 +1,4 @@
-MODULE ORS; (* NW 19.9.93 / 20.3.2017  Scanner in Oberon-07 / AP 31.12.20 Extended Oberon*)
+MODULE ORS; (* NW 19.9.93 / 20.3.2017  Scanner in Oberon-07 / AP 31.12.20 Extended Oberon with retro elements*)
   IMPORT SYSTEM, Texts, Oberon;
 
 (* Oberon Scanner does lexical analysis. Input is Oberon-Text, output is
@@ -11,7 +11,7 @@
   in ival, if real in rval, and if string in str (and slen) *)
   
   CONST IdLen* = 32;
-    NKW = 35;  (*nof keywords*)
+    NKW = 38;  (*nof keywords*)
     maxExp = 38; stringBufSize = 256;
   
     (*lexical symbols*)
@@ -22,11 +22,12 @@
     char* = 20; int* = 21; real* = 22; false* = 23; true* = 24;
     nil* = 25; string* = 26; not* = 27; lparen* = 28; lbrak* = 29;
     lbrace* = 30; ident* = 31;
-    if* = 32; while* = 34; repeat* = 35; case* = 36; for* = 37;
-    comma* = 40; colon* = 41; becomes* = 42; upto* = 43; rparen* = 44;
-    rbrak* = 45; rbrace* = 46; then* = 47; of* = 48; do* = 49;
-    to* = 50; by* = 51; semicolon* = 52; end* = 53; bar* = 54;
-    else* = 55; elsif* = 56; until* = 57; return* = 58;
+    if* = 32; while* = 33; repeat* = 34; loop* = 35; exit* = 36;
+    return* = 37; case* = 38; with* = 39; for* = 40;
+    comma* = 41; colon* = 42; becomes* = 43; upto* = 44; rparen* = 45;
+    rbrak* = 46; rbrace* = 47; then* = 48; of* = 49; do* = 50;
+    to* = 51; by* = 52; semicolon* = 53; end* = 54; bar* = 55;
+    else* = 56; elsif* = 57; until* = 58;
     array* = 60; record* = 61; pointer* = 62; const* = 63; type* = 64;
     var* = 65; procedure* = 66; begin* = 67; import* = 68; module* = 69; final* = 70; eot = 71;
 
@@ -290,6 +291,9 @@
   EnterKW(true, "TRUE");
   EnterKW(type, "TYPE");
   EnterKW(case, "CASE");
+  EnterKW(loop, "LOOP");
+  EnterKW(exit, "EXIT");
+  EnterKW(with, "WITH");
   KWX[4] := k;
   EnterKW(elsif, "ELSIF");
   EnterKW(false, "FALSE");
```

**ORP.Mod**

```diff
--- Oberon-extended/Sources/ORP.Mod	2021-01-12 16:38:12.000000000 +0100
+++ Oberon-retro-compiler/Sources/ExtendedOberon/ORP.Mod	2021-01-13 06:38:53.000000000 +0100
@@ -1,4 +1,4 @@
-MODULE ORP; (*N. Wirth 1.7.97 / 8.3.2020  Oberon compiler for RISC in Oberon-07 / AP 31.12.20 Extended Oberon*)
+MODULE ORP; (*N. Wirth 1.7.97 / 8.3.2020  Oberon compiler for RISC in Oberon-07 / AP 31.12.20 Extended Oberon with retro elements*)
   IMPORT Texts, Oberon, ORS, ORB, ORG;
   (*Author: Niklaus Wirth, 2014. Oberon-2 extensions by Andreas Pirklbauer, 2020.
     Parser of Oberon-RISC compiler. Uses Scanner ORS to obtain symbols (tokens),
@@ -6,7 +6,7 @@
     ORG to produce binary code. ORP performs type checking and data allocation.
     Parser is target-independent, except for part of the handling of allocations.*)
 
-  CONST NofCases = 256; C20 = 100000H;
+  CONST NofCases = 256; maxExit = 16; C20 = 100000H;
 
   TYPE PtrBase = POINTER TO PtrBaseDesc;
     PtrBaseDesc = RECORD  (*list of names of pointer base types*)
@@ -15,14 +15,15 @@
   
   VAR sym: INTEGER;   (*last symbol read*)
     dc: LONGINT;    (*data counter*)
-    level, exno, version: INTEGER;
-    newSF: BOOLEAN;  (*option flag*)
+    level, exno, version, looplev, exitno: INTEGER;
+    newSF, retro, return: BOOLEAN;  (*option, retro and return flags*)
     expression: PROCEDURE (VAR x: ORG.Item);  (*to avoid forward reference*)
     Type: PROCEDURE (VAR type: ORB.Type; expo: BOOLEAN);
     FormalType: PROCEDURE (VAR typ: ORB.Type; dim: INTEGER);
     modid: ORS.Ident;
     pbsList: PtrBase;   (*list of names of pointer base types*)
     dummy: ORB.Object;
+    exit: ARRAY maxExit OF INTEGER;
     W: Texts.Writer;
 
   PROCEDURE Check(s: INTEGER; msg: ARRAY OF CHAR);
@@ -82,11 +83,17 @@
     IF x.rdo THEN ORS.Mark("read-only") END
   END CheckReadOnly;
 
+  PROCEDURE CheckRetro;
+  BEGIN
+    IF ~retro THEN ORS.Mark("add ^ after MODULE") END
+  END CheckRetro;
+
   PROCEDURE CheckExport(VAR expo: BOOLEAN);
   BEGIN
-    IF sym = ORS.times THEN
+    IF (sym = ORS.times) OR (sym = ORS.minus) THEN
       expo := TRUE; ORS.Get(sym);
-      IF level # 0 THEN ORS.Mark("remove asterisk") END
+      IF level # 0 THEN ORS.Mark("remove export mark") END ;
+      IF sym = ORS.minus THEN CheckRetro END
     ELSE expo := FALSE
     END
   END CheckExport;
@@ -546,6 +553,32 @@
       ORG.FixLink(L0); DEC(obj.exno)
     END TypeCasePart;
 
+    PROCEDURE With(VAR L0: LONGINT);
+      VAR obj, typobj: ORB.Object; x: ORG.Item;
+        orgtype: ORB.Type;  (*original type of with var*)
+    BEGIN qualident(obj);
+      IF ((obj.type.form = ORB.Pointer) & (obj.class = ORB.Var) & (obj.type.base.form = ORB.Record) OR
+          (obj.type.form = ORB.Record) & (obj.class = ORB.Par)) & (obj.lev > 0) THEN
+        Check(ORS.colon, ": expected"); INC(obj.exno);
+        qualident(typobj); ORG.MakeItem(x, obj); orgtype := obj.type;
+        IF typobj.class # ORB.Typ THEN ORS.Mark("not a type") END ;
+        TypeTest(x, typobj.type, FALSE); obj.type := typobj.type;
+        ORG.CFJump(x); Check(ORS.do, "no DO"); StatSequence;
+        ORG.FJump(L0); ORG.Fixup(x); obj.type := orgtype; DEC(obj.exno)
+      ELSE ORS.Mark("invalid with variable"); Check(ORS.colon, ": expected"); Check(ORS.do, "no DO"); StatSequence
+      END
+    END With;
+
+    PROCEDURE WithPart;
+      VAR L0: LONGINT;
+    BEGIN L0 := 0; With(L0);
+      WHILE (sym < ORS.end) OR (sym = ORS.bar) DO
+        IF sym = ORS.bar THEN ORS.Get(sym) ELSE With(L0) END
+      END ;
+      IF sym = ORS.else THEN ORS.Get(sym); StatSequence END ;
+      ORG.FixLink(L0)
+    END WithPart;
+
     PROCEDURE CaseLabel(VAR x: ORG.Item);
     BEGIN expression(x); CheckConst(x);
       IF (x.type.form = ORB.String) & (x.b = 2) THEN ORG.StrToChar(x)
@@ -593,7 +626,7 @@
     END NumericCasePart;
 
   BEGIN (* StatSequence *)
-    REPEAT (*sync*) obj := NIL;
+    REPEAT (*sync*) obj := NIL; return := FALSE;
       IF ~((sym >= ORS.ident) & (sym <= ORS.for) OR (sym >= ORS.semicolon)) THEN
         ORS.Mark("statement expected");
         REPEAT ORS.Get(sym) UNTIL sym >= ORS.ident
@@ -657,6 +690,28 @@
           ORS.Get(sym); expression(x); CheckBool(x); ORG.CBJump(x, L0)
         ELSE ORS.Mark("missing UNTIL")
         END
+      ELSIF sym = ORS.loop THEN
+        ORS.Get(sym); CheckRetro; rx := exitno; INC(looplev);
+        L0 := ORG.Here(); StatSequence; ORG.BJump(L0); DEC(looplev);
+        WHILE exitno > rx DO DEC(exitno); ORG.FixLink(exit[exitno]) END ;
+        Check(ORS.end, "no END")
+      ELSIF sym = ORS.exit THEN
+        ORS.Get(sym); CheckRetro; L0 := 0; ORG.FJump(L0);
+        IF looplev = 0 THEN ORS.Mark("exit not allowed")
+        ELSIF exitno < maxExit THEN exit[exitno] := L0; INC(exitno)
+        ELSE ORS.Mark("too many exits")
+        END
+      ELSIF sym = ORS.return THEN
+        ORS.Get(sym);
+        IF level # 0 THEN return := TRUE;
+          obj := ORB.topScope; INC(obj.lev);
+          IF obj.type.base.form # ORB.NoTyp THEN expression(x);
+            IF ~CompTypes(obj.type.base, x.type, FALSE) THEN ORS.Mark("wrong result type") END
+          ELSE x.type := ORB.noType
+          END ;
+          ORG.Return(obj.type.base.form, x, obj.val, obj.expo)
+        ELSE ORS.Mark("return not allowed")
+        END
       ELSIF sym = ORS.for THEN
         ORS.Get(sym);
         IF sym = ORS.ident THEN
@@ -688,6 +743,8 @@
           NumericCasePart(x)
         END ;
         Check(ORS.end, "no END")
+      ELSIF sym = ORS.with THEN
+        ORS.Get(sym); WithPart; Check(ORS.end, "no END")
       END ;
       ORG.CheckRegs;
       IF sym = ORS.semicolon THEN ORS.Get(sym)
@@ -896,8 +953,8 @@
       x: ORG.Item; tp: ORB.Type; ptbase: PtrBase;
       expo: BOOLEAN; id: ORS.Ident;
   BEGIN (*sync*) pbsList := NIL;
-    IF (sym < ORS.const) & (sym # ORS.end) & (sym # ORS.return) THEN ORS.Mark("declaration?");
-      REPEAT ORS.Get(sym) UNTIL (sym >= ORS.const) OR (sym = ORS.end) OR (sym = ORS.return)
+    IF (sym < ORS.const) & (sym # ORS.end) THEN ORS.Mark("declaration?");
+      REPEAT ORS.Get(sym) UNTIL (sym >= ORS.const) OR (sym = ORS.end)
     END ;
     IF sym = ORS.const THEN
       ORS.Get(sym);
@@ -991,21 +1048,20 @@
       int, expo: BOOLEAN;
 
     PROCEDURE Body(proc: ORB.Object; parblksize: LONGINT; int: BOOLEAN);
-      VAR x: ORG.Item; locblksize: LONGINT;
+      VAR obj: ORB.Object; x: ORG.Item; locblksize: LONGINT;
     BEGIN Check(ORS.semicolon, "no ;"); locblksize := parblksize;
-      Declarations(locblksize); proc.type.dsc := ORB.topScope.next;
+      Declarations(locblksize); obj := ORB.topScope; proc.type.dsc := obj.next;
+      obj.type := proc.type; obj.val := locblksize; obj.lev := 0; obj.expo := int;  (*for RETURN statements*)
       WHILE sym = ORS.procedure DO ProcedureDecl; Check(ORS.semicolon, "no ;") END ;
       ORG.FixLink(proc.type.len);  (*fix forward references generated in ORG*)
-      proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next;
+      proc.val := ORG.Here() * 4; proc.type.dsc := obj.next;
       ORG.Enter(parblksize, locblksize, int);
       IF sym = ORS.begin THEN ORS.Get(sym); StatSequence END ;
-      IF sym = ORS.return THEN
-        ORS.Get(sym); expression(x);
-        IF proc.type.base = ORB.noType THEN ORS.Mark("this is not a function")
-        ELSIF ~CompTypes(proc.type.base, x.type, FALSE) THEN ORS.Mark("wrong result type")
+      IF proc.type.base.form # ORB.NoTyp THEN  (*function procedure*)
+        IF obj.lev = 0 THEN ORS.Mark("function without result")
+        ELSIF ~return OR (obj.lev # 1) THEN CheckRetro
         END
-      ELSIF proc.type.base.form # ORB.NoTyp THEN
-        ORS.Mark("function without result"); proc.type.base := ORB.noType
+      ELSIF obj.lev > 0 THEN CheckRetro
       END ;
       ORG.Return(proc.type.base.form, x, locblksize, int); Check(ORS.end, "no END");
       IF sym = ORS.ident THEN
@@ -1079,15 +1135,18 @@
     VAR key: LONGINT;
   BEGIN Texts.WriteString(W, "  compiling "); ORS.Get(sym);
     IF sym = ORS.module THEN
-      ORS.Get(sym);
-      IF sym = ORS.times THEN version := 0; dc := 8; Texts.Write(W, "*"); ORS.Get(sym) ELSE dc := 0; version := 1 END ;
+      ORS.Get(sym); retro := FALSE;
+      IF sym = ORS.times THEN version := 0; dc := 8; Texts.Write(W, "*"); ORS.Get(sym)
+      ELSE dc := 0; version := 1;
+        IF sym = ORS.arrow THEN retro := TRUE; Texts.Write(W, "^"); ORS.Get(sym) END
+      END ;
       ORB.Init; ORB.OpenScope;
       IF sym = ORS.ident THEN
         ORS.CopyId(modid); ORS.Get(sym);
         Texts.WriteString(W, modid); Texts.Append(Oberon.Log, W.buf)
       ELSE ORS.Mark("identifier expected")
       END ;
-      Check(ORS.semicolon, "no ;"); level := 0; exno := 1; key := 0;
+      Check(ORS.semicolon, "no ;"); level := 0; exno := 1; key := 0; looplev := 0; exitno := 0;
       IF sym = ORS.import THEN ImportList; Check(ORS.semicolon, "; missing") END ;
       ORG.Open(version); Declarations(dc); ORG.SetDataSize((dc + 3) DIV 4 * 4);
       WHILE sym = ORS.procedure DO ProcedureDecl; Check(ORS.semicolon, "no ;") END ;
@@ -1157,7 +1216,7 @@
     Oberon.Collect(0); Oberon.Return(res)
   END Compile;
 
-BEGIN Texts.OpenWriter(W); Texts.WriteString(W, "OR Compiler  8.3.2020 / AP 31.12.20");
+BEGIN Texts.OpenWriter(W); Texts.WriteString(W, "OR Retro Compiler  8.3.2020 / AP 31.12.20");
   Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf);
   NEW(dummy); dummy.class := ORB.Var; dummy.type := ORB.intType;
   expression := expression0; Type := Type0; FormalType := FormalType0
```

------------------------------------------------------
**IMPLEMENTATION NOTES**

**LOOP and EXIT statements**

* An EXIT statement is implemented as a *forward jump* to the end of the LOOP statement that contains it. Thus, a *fixup* is required to insert the branch destination once it is known. Note that LOOP statements can be nested.

* However, when compiling the statement sequence of the LOOP statement using procedure *ORP.StatSequence*, one cannot use a *local* variable to hold the code locations of EXIT statements that require *fixups* (as is usually done).

* This is because the compiler may (recursively) enter *ORP.StatSequence* again before it reaches any EXIT statement belonging to the corresponding LOOP statement.

* Example:

      LOOP
        ..
        EXIT;  (*exitno = 0*)
        ..
        IF cond1 THEN (*new StatSequence entered here*) EXIT END ; (*exitno = 1*)
        ..
        IF cond2 THEN (*new StatSequence entered here*) EXIT END ; (*exitno = 2*)
        ..
        LOOP
          ..
          EXIT;   (*exitno = 3*)
          ..
          IF cond3 THEN (*new StatSequence entered here*) EXIT END ; (*exitno = 4*)
          ..
          IF cond4 THEN (*new StatSequence entered here*) EXIT END ; (*exitno = 5*)
          ..
        END ;
        ..
        EXIT;  (*exitno = 3*)
         ..
         IF cond5 THEN (*new StatSequence entered here*) EXIT END  (*exitno = 4*)
      END

* This can be addressed by using a *global* table to hold the locations of the EXIT statements that require fixups, while keeping track of the stack of nested LOOP statements in the process.

  * When *entering* a LOOP statement, one remembers the *location* of the *next* EXIT statement that is to be processed. In the above example, when entering the inner LOOP statement, that location is exitno = 3.

  * When *exiting* a LOOP statement, one "fixes up" all exits statements from that location onward, i.e. only the EXIT statements of the LOOP itself. In the above example, when exiting the inner LOOP statement, only the EXIT instructions 3, 4, 5 are fixed up.

**Multiple return statements**

* Since *multiple* RETURN statements in a *single* procedure are now allowed in the language, the code generating procedure *ORG.Return* can no longer be called by the parser procedure that processes procedure declarations (*ORP.ProcedureDecl*), but must be called from the procedure handling statement *sequences* (*ORP.StatSequence*).

* There are three pieces of information required to call *ORG.Return*:

  * the procedure object in the symbol table (*proc*)
  * the size of the local variables (*locblksize*)
  * whether a procedure is an interrupt procedure or not (*int*).

* In our implementation, this information is made available as follows:

  * *ORB.topScope.type* is "abused" to point to the procedure's type object (*proc.type*).

  * *ORB.topScope.val* is "abused" to hold the size of the procedure's local variable (*locblksize*).

  * *ORB.topScope.expo* is "abused" to remember whether *proc* is an interrupt procedure (*int*).

  * In addition, *ORB.topScope.lev* is "abused" to count the number of RETURN statements in procedure (there hast to be at least one RETURN statement in a procedure).

* This works because each procedure object in the symbol table has its *own* type object (i.e. there is no sharing of *type* objects among procedures) and also because the "abused" fields of *ORB.topScope* are not otherwise used in the compiler.
