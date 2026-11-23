# Oberon-retro-compiler
A modified Oberon compiler which supports the LOOP, EXIT, WITH and multiple RETURN statements, and forward declarations of procedures, as they have existed in the *original* versions of the Oberon and Oberon-2 languages defined around 1990.

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

**4) Forward declarations of procedures**

     ForwardDeclaration = PROCEDURE "^" [Receiver] IdentDef [FormalParameters].

     DeclarationSequence = [CONST {ConstantDeclaration ";"}]
       [TYPE {TypeDeclaration ";"}] [VAR {VariableDeclaration ";"} ]
       {ProcedureDeclaration ";" | ForwardDeclaration ";"].

Example:

      PROCEDURE^ P(x: INTEGER);

      PROCEDURE Q;
      BEGIN P(1);
      END Q;

      PROCEDURE P(x: INTEGER);
      BEGIN (*...*)
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
--- Oberon-extended/Sources/ORS.Mod	2021-10-02 14:35:23.000000000 +0200
+++ Oberon-retro-compiler/Sources/ExtendedOberon/ORS.Mod	2022-01-21 06:26:37.000000000 +0100
@@ -1,4 +1,4 @@
-MODULE ORS; (* NW 19.9.93 / 20.3.2017  Scanner in Oberon-07 / AP 1.10.21 Extended Oberon*)
+MODULE ORS; (* NW 19.9.93 / 20.3.2017  Scanner in Oberon-07 / AP 11.1.22 Extended Oberon with retro elements*)
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
-    to* = 50; by* = 51; semicolon* = 52; bar* = 53; end* = 54;
-    else* = 55; elsif* = 56; until* = 57; return* = 58;
+    if* = 32; while* = 33; repeat* = 34; loop* = 35; exit* = 36;
+    return* = 37; case* = 38; with* = 39; for* = 40;
+    comma* = 41; colon* = 42; becomes* = 43; upto* = 44; rparen* = 45;
+    rbrak* = 46; rbrace* = 47; then* = 48; of* = 49; do* = 50;
+    to* = 51; by* = 52; semicolon* = 53; bar* = 54; end* = 55;
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

**ORB.Mod**

```diff
--- Oberon-extended/Sources/ORB.Mod	2022-06-19 08:47:26.000000000 +0200
+++ Oberon-retro-compiler/Sources/ExtendedOberon/ORB.Mod	2022-06-20 09:36:50.000000000 +0200
@@ -1,4 +1,4 @@
-MODULE ORB;   (*NW 25.6.2014  / AP 4.3.2020 / 5.3.2019  in Oberon-07 / AP 19.6.22 Extended Oberon*)
+MODULE ORB;   (*NW 25.6.2014  / AP 4.3.2020 / 5.3.2019  in Oberon-07 / AP 19.6.22 Extended Oberon with retro elements*)
   IMPORT Files, ORS;
   (*Definition of data types Object and Type, which together form the data structure
     called "symbol table". Contains procedures for creation of Objects, and for search:
@@ -80,6 +80,13 @@
     END
   END NewObj;
 
+  PROCEDURE FindObj*(id: ORS.Ident; list: Object): Object;  (*search id in list*)
+    VAR x: Object;
+  BEGIN x := list;
+    WHILE (x # NIL) & ((x.name # id) OR (x.class = Mod) & ~x.rdo) DO x := x.next END ;
+    RETURN x
+  END FindObj;
+
   PROCEDURE thisObj*(): Object;
     VAR s, x: Object;
   BEGIN s := topScope;
```

**ORG.Mod**

```diff
--- Oberon-extended/Sources/ORG.Mod	2022-06-12 10:50:57.000000000 +0200
+++ Oberon-retro-compiler/Sources/ExtendedOberon/ORG.Mod	2022-06-20 09:46:07.000000000 +0200
@@ -1,4 +1,4 @@
-MODULE ORG; (* N.Wirth, 16.4.2016 / 4.4.2017 / 31.5.2019  Oberon compiler; code generator for RISC / AP 12.6.22 Extended Oberon*)
+MODULE ORG; (* N.Wirth, 16.4.2016 / 4.4.2017 / 31.5.2019  Oberon compiler; code generator for RISC / AP 19.6.22 Extended Oberon with retro elements*)
   IMPORT SYSTEM, Files, ORS, ORB;
   (*Code generator for Oberon compiler for RISC processor.
      Procedural interface to Parser ORP; result in array "code".
@@ -160,10 +160,6 @@
     code[at] := code[at] DIV C24 * C24 + with MOD C24
   END fix3;
 
-  PROCEDURE FixOne*(at: LONGINT);
-  BEGIN fix3(at, pc-at-1)
-  END FixOne;
-
   PROCEDURE FixLinkWith(L, dst: LONGINT);
     VAR L1: LONGINT;
   BEGIN (*fix chain of branch instructions*)
@@ -174,6 +170,16 @@
   BEGIN FixLinkWith(L, pc)
   END FixLink;
 
+  PROCEDURE FixLinkMixed*(L: LONGINT);
+    VAR L1, format: LONGINT; p: INTEGER;
+  BEGIN (*fix chain of instructions of different formats*)
+    WHILE L # 0 DO p := code[L];
+      format := p DIV C30 MOD 4; L1 := p MOD C16;
+      IF format < 3 THEN fix1(L, (pc-L)*4) ELSE fix3(L, pc-L-1) END ;
+      L := L1
+    END
+  END FixLinkMixed;
+
   PROCEDURE FixLinkPair(L, adr: LONGINT);
     VAR L1: LONGINT; p, q: INTEGER;
   BEGIN (*fix chain of instruction pairs with an address that is spread across both instructions, 0 <= adr < C24*)
@@ -204,7 +210,8 @@
       IF x.type.size = 1 THEN op := Ldr+1 ELSE op := Ldr END ;
       IF x.mode = ORB.Const THEN
         IF x.type.form = ORB.Proc THEN
           IF x.r > 0 THEN (*local*) ORS.Mark("not allowed")
+          ELSIF x.a < 0 THEN (*forward*) Put3(BL, 7, 0); Put1(Add, RH, LNK, x.type.len); x.type.len := pc-1  (*fixed up in ORP.Body*)
           ELSIF x.r = 0 THEN (*global*) Put3(BL, 7, 0); Put1a(Sub, RH, LNK, pc*4 - x.a)
           ELSE (*imported*) PutPair(x.r, Add, RH, RH, x.a + C8, 1) (*mark as progbase-relative*)
           END
@@ -848,7 +855,8 @@
         Put2(Ldr, RH, RH, -4-x.a*4); Put3(BLR, 7, RH)
       END
     ELSIF x.mode = ORB.Const THEN  (*regular procedure*)
-      IF x.r >= 0 THEN Put3(BL, 7, (x.a DIV 4)-pc-1)
+      IF x.a < 0 THEN (*forward*) Put3(BL, 7, x.type.len); x.type.len := pc-1  (*fixed up in ORP.Body*)
+      ELSIF x.r >= 0 THEN Put3(BL, 7, (x.a DIV 4)-pc-1)
       ELSE (*imported*) Put3a(BL, -x.r, x.a, pc-fixorgP); fixorgP := pc-1
       END
     ELSE  (*installed procedure*)
```

**ORP.Mod**

```diff
--- Oberon-extended/Sources/ORP.Mod	2022-06-16 10:27:36.000000000 +0200
+++ Oberon-retro-compiler/Sources/ExtendedOberon/ORP.Mod	2022-06-20 09:36:51.000000000 +0200
@@ -1,4 +1,4 @@
-MODULE ORP; (*N. Wirth 1.7.97 / 8.3.2020  Oberon compiler for RISC in Oberon-07 / AP 15.6.22 Extended Oberon*)
+MODULE ORP; (*N. Wirth 1.7.97 / 8.3.2020  Oberon compiler for RISC in Oberon-07 / AP 19.6.22 Extended Oberon with retro elements*)
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
@@ -14,15 +14,16 @@
     END ;
   
   VAR sym: INTEGER;   (*last symbol read*)
-    dc: LONGINT;    (*data counter*)
-    level, exno, version: INTEGER;
-    newSF: BOOLEAN;  (*option flag*)
+    dc, fc: LONGINT;    (*data counter, forward counter*)
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
@@ -541,6 +548,32 @@
       ORG.FixLink(L0)
     END TypeCasePart;
 
+    PROCEDURE With(VAR L0: LONGINT);
+      VAR obj, typobj: ORB.Object; x: ORG.Item;
+        orgtype: ORB.Type;  (*original type of with var*)
+    BEGIN qualident(obj);
+      IF ((obj.type.form = ORB.Pointer) & (obj.class = ORB.Var) & (obj.type.base.form = ORB.Record) OR
+          (obj.type.form = ORB.Record) & (obj.class = ORB.Par)) & (obj.lev > 0) THEN
+        Check(ORS.colon, ": expected");
+        qualident(typobj); ORG.MakeItem(x, obj); orgtype := obj.type;
+        IF typobj.class # ORB.Typ THEN ORS.Mark("not a type") END ;
+        TypeTest(x, typobj.type, FALSE); obj.type := typobj.type;
+        ORG.CFJump(x); Check(ORS.do, "no DO"); StatSequence;
+        ORG.FJump(L0); ORG.Fixup(x); obj.type := orgtype
+      ELSE ORS.Mark("invalid with variable"); Check(ORS.colon, ": expected"); Check(ORS.do, "no DO"); StatSequence
+      END
+    END With;
+
+    PROCEDURE WithPart;
+      VAR L0: LONGINT;
+    BEGIN L0 := 0; With(L0);
+      WHILE sym <= ORS.bar DO
+        IF sym = ORS.bar THEN ORS.Get(sym) ELSE With(L0) END
+      END ;
+      IF sym = ORS.else THEN ORS.Get(sym); StatSequence END ;
+      ORG.FixLink(L0)
+    END WithPart;
+
     PROCEDURE CaseLabel(VAR x: ORG.Item);
     BEGIN expression(x); CheckConst(x);
       IF (x.type.form = ORB.String) & (x.b = 2) THEN ORG.StrToChar(x)
@@ -596,7 +629,7 @@
     END SkipCase;
 
   BEGIN (* StatSequence *)
-    REPEAT (*sync*)
+    REPEAT (*sync*) return := FALSE;
       IF ~((sym >= ORS.ident) & (sym <= ORS.for) OR (sym >= ORS.semicolon)) THEN
         ORS.Mark("statement expected");
         REPEAT ORS.Get(sym) UNTIL sym >= ORS.ident
@@ -660,6 +693,28 @@
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
@@ -686,6 +741,8 @@
         ELSE ORS.Mark("invalid case variable"); SkipCase
         END ;
         Check(ORS.end, "no END")
+      ELSIF sym = ORS.with THEN
+        ORS.Get(sym); WithPart; Check(ORS.end, "no END")
       END ;
       ORG.CheckRegs;
       IF sym = ORS.semicolon THEN ORS.Get(sym)
@@ -894,8 +951,8 @@
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
@@ -985,28 +1042,24 @@
     VAR proc, redef, obj: ORB.Object;
       type, typ, rec: ORB.Type;
       procid, recid: ORS.Ident;
-      parblksize: LONGINT; class: INTEGER;
-      int, expo: BOOLEAN;
+      parblksize: LONGINT; form, class: INTEGER;
+      int, body, expo: BOOLEAN;
 
     PROCEDURE Body(proc: ORB.Object; parblksize: LONGINT; int: BOOLEAN);
-      VAR x: ORG.Item; locblksize, L: LONGINT;
+      VAR obj: ORB.Object; x: ORG.Item; locblksize: LONGINT;
     BEGIN Check(ORS.semicolon, "no ;"); locblksize := parblksize;
-      Declarations(locblksize);
-      proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next;
-      IF sym = ORS.procedure THEN
-        L := 0; ORG.FJump(L);
-        REPEAT ProcedureDecl; Check(ORS.semicolon, "no ;") UNTIL sym # ORS.procedure;
-        ORG.FixOne(L); proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next
-      END ;
+      Declarations(locblksize); obj := ORB.topScope; proc.type.dsc := obj.next;
+      obj.type := proc.type; obj.val := locblksize; obj.lev := 0; obj.expo := int;  (*for RETURN statements*)
+      WHILE sym = ORS.procedure DO ProcedureDecl; Check(ORS.semicolon, "no ;") END ;
+      ORG.FixLinkMixed(proc.type.len);  (*fix forward references generated in ORG*)
+      proc.val := ORG.Here() * 4; proc.type.dsc := obj.next; DEC(fc);
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
@@ -1016,30 +1069,43 @@
       END
     END Body;
 
-  BEGIN (* ProcedureDecl *) int := FALSE; rec := NIL; ORS.Get(sym);
-    IF sym = ORS.times THEN ORS.Get(sym); int := TRUE END ;
+  BEGIN (* ProcedureDecl *) int := FALSE; body := TRUE; rec := NIL; ORS.Get(sym);
+    IF sym = ORS.times THEN (*interrupt*) ORS.Get(sym); int := TRUE
+    ELSIF sym = ORS.arrow THEN (*forward*) ORS.Get(sym); body := FALSE
+    END ;
     IF sym = ORS.lparen THEN
-      ORS.Get(sym); Receiver(class, recid, typ, rec);
+      ORS.Get(sym); Receiver(class, recid, typ, rec); form := ORB.TProc;
       IF level # 0 THEN ORS.Mark("local type-bound procedures not implemented") END
+    ELSE form := ORB.Proc
     END ;
     IF sym = ORS.ident THEN
       ORS.CopyId(procid); ORS.Get(sym); CheckExport(expo);
       IF int THEN parblksize := 12 ELSE parblksize := 4 END ;
-      NEW(type); type.size := ORG.WordSize;
+      NEW(type); type.size := ORG.WordSize; type.len := 0; (*len used as heading of fixup chain of forward refs*)
       IF rec = NIL THEN  (*regular procedure*)
-        ORB.NewObj(proc, procid, ORB.Const);
-        type.form := ORB.Proc; proc.type := type; proc.val := -1; proc.lev := level; proc.expo := expo;
-        IF expo THEN proc.exno := exno; INC(exno) END ;
+        proc := ORB.FindObj(procid, ORB.topScope.next);
+        IF proc = NIL THEN  (*identifier not found in the symbol table*)
+          ORB.NewObj(proc, procid, ORB.Const); INC(fc);
+          type.form := ORB.Proc; proc.type := type; proc.val := -1; proc.lev := level; proc.expo := expo;
+          IF expo THEN proc.exno := exno; INC(exno) END
+        END ;
         ORB.OpenScope; INC(level); type.base := ORB.noType;
         ProcedureType(type, parblksize); type.dsc := ORB.topScope.next  (*formal parameter list*)
       ELSE  (*type-bound procedure*)
-        ORB.NewMethod(rec, proc, redef, procid);
-        IF rec.typobj.val > 0 THEN ORS.Mark("invalid method order") ELSE DisallowMethods(rec.base) END ;
-        type.form := ORB.TProc; proc.type := type; proc.val := -1; proc.expo := expo;
-        IF expo THEN proc.exno := exno; INC(exno);
-          IF ~typ.typobj.expo THEN ORS.Mark("receiver must be exported") END ;
-          procid := "@"; ORB.NewObj(obj, procid, ORB.Const); obj.name[0] := 0X; (*dummy to preserve linear order of exno*)
-          obj.type := proc.type; obj.dsc := proc; obj.exno := proc.exno; obj.expo := FALSE
+        IF rec.base # NIL THEN redef := ORB.FindObj(procid, rec.base.dsc);  (*search in base types of receiver*)
+          IF (redef # NIL) & ((redef.class # ORB.Const) OR (redef.type.form # ORB.TProc)) THEN ORS.Mark("mult def") END
+        ELSE redef := NIL
+        END ;
+        proc := ORB.FindFld(procid, rec);  (*search in fields of receiver proper, but not of its base types*)
+        IF proc = NIL THEN
+          ORB.NewMethod(rec, proc, redef, procid); INC(fc);
+          IF rec.typobj.val > 0 THEN ORS.Mark("invalid method order") ELSE DisallowMethods(rec.base) END ;
+          type.form := ORB.TProc; proc.type := type; proc.val := -1; proc.expo := expo;
+          IF expo THEN proc.exno := exno; INC(exno);
+            IF ~typ.typobj.expo THEN ORS.Mark("receiver must be exported") END ;
+            procid := "@"; ORB.NewObj(obj, procid, ORB.Const); obj.name[0] := 0X; (*dummy to preserve linear order of exno*)
+            obj.type := proc.type; obj.dsc := proc; obj.exno := proc.exno; obj.expo := FALSE
+          END
         END ;
         ORB.OpenScope; INC(level); type.base := ORB.noType;
         ORB.NewObj(obj, recid, class);  (*insert receiver as first parameter*)
@@ -1052,7 +1118,13 @@
           END
         END
       END ;
-      Body(proc, parblksize, int); ORB.CloseScope; DEC(level)
+      IF proc.type # type THEN  (*identifier found in the symbol table*)
+        IF (proc.class # ORB.Const) OR (proc.type.form # form) OR (proc.val >= 0) OR ~body THEN ORS.Mark("mult def")
+        ELSIF (proc.expo # expo) OR ~EqualSignatures(proc.type, type) THEN ORS.Mark("must match forward declaration")
+        END
+      END ;
+      IF body THEN Body(proc, parblksize, int) END ;
+      ORB.CloseScope; DEC(level)
     ELSE ORS.Mark("proc id expected")
     END
   END ProcedureDecl;
@@ -1080,18 +1152,22 @@
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
+      Check(ORS.semicolon, "no ;"); level := 0; fc := 0; exno := 1; key := 0; looplev := 0; exitno := 0;
       IF sym = ORS.import THEN ImportList; Check(ORS.semicolon, "; missing") END ;
       ORG.Open(version); Declarations(dc); ORG.SetDataSize((dc + 3) DIV 4 * 4);
       WHILE sym = ORS.procedure DO ProcedureDecl; Check(ORS.semicolon, "no ;") END ;
+      IF fc > 0 THEN ORS.Mark("undefined forward declarations") END ;
       ORG.Header;
       IF sym = ORS.begin THEN ORS.Get(sym); StatSequence END ;
       ORG.Exit;
@@ -1158,7 +1234,7 @@
     Oberon.Collect(0); Oberon.Return(res)
   END Compile;
 
-BEGIN Texts.OpenWriter(W); Texts.WriteString(W, "OR Compiler  8.3.2020 / AP 15.6.22");
+BEGIN Texts.OpenWriter(W); Texts.WriteString(W, "OR Retro Compiler  8.3.2020 / AP 19.6.22");
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

**Forward declarations of procedures**

We implement forward *declarations* of procedures as described below:

Note that in our implementation both global *and* local procedures can be declared forward.

**1. Processing of the procedure heading of a forward-declared procedure P (ORP.ProcedureDecl)**:

When this *heading* of a forward-declared procedure P, i.e. the heading

      PROCEDURE^ P(x: INTEGER);

is processed, the field *obj.type.len* is set to 0 to indicate that no forward reference to P has been generated yet, and *obj.val* is set to -1 to indicate that the body of P has not been compiled yet. See *ORP.ProcedureDecl*:

        proc.val := -1;       (*<0: body of P has not been compiled yet; otherwise: entry address of P*)

The field *proc.type.len* is used as the heading of the fixup list for forward references to P (initially set to 0). This is acceptable, because every procedure object *obj* (of type *ORB.Object*) has its **own** type object *obj.type* (of type *ORB.Type*) and its field *obj.type.len* is not used otherwise.

The field *obj.type.len* is available during code generation as the field *x.type.len* in source level items generated from the procedure object *obj* using procedure *ORG.MakeItem*, while the field *obj.val* is available as the field *x.a*.

**2. Assigning P to a procedure variable, passing P as a procedure parameter, returning P as a result of a function procedure (ORG.load)**:

If a procedure P, whose body has not been compiled yet, is assigned to a procedure variable, passed as parameter to a procedure or returned as the result of a function procedure, a *forward reference* in the form of a *register* instruction is generated that will eventually contain an instruction operand.

This adds a single line to *ORG.load*:

       IF x.a < 0 THEN (*forward*) Put3(BL, 7, 0); Put1(Add, RH, LNK, x.type.len); x.type.len := pc-1    (*fixed up in ORP.Body*)

The purpose of the first instruction generated (branch zero step forward) is to merely deposit the link address PC+1 in register LNK ("LNK := PC+1"). To this address, we then add the code distance to procedure P (determined later during the fixup step in ORP.Body).

Here we use PC-relative addressing, so that we can fix up this instruction at *compile* time rather than only at module *load* time.

**3. Calling a forward-declared procedure P (ORG.Call)**:

If a procedure P, whose body has not been compiled yet, is *called*, a forward reference in the form of a *branch* instruction is generated that will eventually contain the branch displacement.

This adds a single line to *ORG.Call*:.

       IF x.a < 0 THEN (*forward*) Put3(BL, 7, x.type.len); x.type.len := pc-1  (*fixed up in ORP.Body*)

**4. Compilation of the procedure body of a forward-declared procedure P (ORP.Body)**:

When the procedure *body* of P is *compiled*, all forward references to P are *fixed up* with the now known actual entry address of P, and the field *obj.val* is changed to that address.

   This changes *ORP.Body* from:

        proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next;
        IF sym = ORS.procedure THEN
          L := 0; ORG.FJump(L);
          REPEAT ProcedureDecl; Check(ORS.semicolon, "no ;") UNTIL sym # ORS.procedure;
          ORG.FixOne(L); proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next
        END ;

   to:

        proc.type.dsc := ORB.topScope.next;
        WHILE sym = ORS.procedure DO ProcedureDecl; Check(ORS.semicolon, "no ;") END ;
        ORG.FixLinkMixed(proc.type.len);  (*fix forward references generated in ORG.load and ORG.Call*)
        proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next; DEC(fc);

As one can see, the forward jump at the beginning of P is no longer generated (no calls to *ORG.FJump* anymore).

The second assignment *proc.type.dsc := ORB.topScope.next* is necessary to cover the case, where *proc.type.dsc* has been NIL before local procedures have been processed (in this case, a new object for the local procedure will be added).

Note that procedure *ORG.FixLink* of the Project Oberon 2013 compiler assumes that the instructions to be fixed up are format-3 *branch* instructions. But in our implementation we also generate format-1 *register* instructions (in *ORG.load*, see above).

We could have decided to just generalize *ORG.FixLink* to also handle the format-1 ADD instruction generated in *ORG.load*.

However, we have opted to add a *separate* procedure *ORG.FixLinkMixed* that can handle both format-1 and format-3 instructions embedded in the *same* fixup list.

This has the advantage, that the (more complex) code in *ORG.FixLinkMixed* is only called when compiling procedure bodies, and not in all cases where format-3 branch instructions are to be fixed up. This choice also nicely isolates the addition of this feature, which, after all, exists only in the retro compiler, and only to implement forward declarations of procedures.

        PROCEDURE fix1(at, with: LONGINT);
          VAR v: LONGINT;
        BEGIN (*fix format-1 register instruction*)
           IF with < 0 THEN v := C28 (*set v bit*) ELSE v := 0 END ;
           code[at] := code[at] DIV C16 * C16 + with MOD C16 + v
        END fix1;

        PROCEDURE FixLinkMixed*(L: LONGINT);
          VAR L1, format: LONGINT; p: INTEGER;
        BEGIN (*fix chain of instructions of different formats*)
          WHILE L # 0 DO p := code[L];
            format := p DIV C30 MOD 4; L1 := p MOD C16;
            IF format < 3 THEN fix1(L, (pc-L)*4) ELSE fix(L, pc-L-1) END ;
            L := L1
          END
        END FixLinkMixed;

Note that it is *essential* that the origin of the fixup list for a procedure P is **not** rooted in a variable *L* local to the parsing procedure *ProcedureDecl* (as is typical in other parsing procedures), because forward references may be generated from *other* procedures in

* the surrounding scope,
* the same scope, or
* from within a nested scope (as in the example above).

However, the fixup list must be associated with P at all times. A field in the symbol table entry (such as *obj.type.len*) for P is therefore ideally suited for this purpose.

**5. Compilations of calls to P after the procedure body of P has been compiled**:

Any references to P later in the source text are *backward* references using the actual entry address of P, and no fixups are needed for such calls.

**6. More efficient forward references for nested procedures come**:

The implementation of forward declarations of procedures, as described above, automatically makes forward *references* for nested procedures more efficient, since only *forward* calls are generated from within a procedure body of a nested procedure.

If a procedure Q which is local to procedure P refers to the enclosing procedure P, as in

     PROCEDURE P;
       PROCEDURE Q;
       BEGIN (*body of Q*) P  (*forward reference from Q to P, as the body of P is not compiled yet*)
       END Q;
     BEGIN (*body of P*)
     END P;

then the official Oberon-07 compiler, as published on www.projectoberon.com, generates the following code:

     20  P'   BL  10         ; forward branch to line 31 (the body of P)
     21  Q    body of Q
              ...
              ...            ; any calls from Q to P are BACKWARD jumps to line 20 and from there forward to line 31
              ...
     31  P    body of P

whereas the modified compiler provided in **this** repository generates the following, more efficient, code:

     20  Q   body of Q
             ...
             ...             ; any calls from Q to P are FORWARD jumps to line 30, fixed up when the body of P is compiled
             ...
     30  P   body of P

i.e. it does **not** generate an extra forward jump in line 20 around Q to the body of P and backward jumps from Q to line 20. In Project Oberon 2013, the extra BL instruction in line 20 exists, so that Q can call P (Q is compiled before P).
