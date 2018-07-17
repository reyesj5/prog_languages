structure Compile : sig

  val compile        : string -> AST.term * TypeCheck.ty * AST.term
  val compileUntyped : string -> AST.term * AST.term

end = struct

  fun compile program =
    let
      val tokens = Scan.scan program
      val ast    = Parse.parse tokens
      val ty     = TypeCheck.typeof ast
      val result = Eval.eval ast
    in
      (ast, ty, result)
    end

  fun compileUntyped program =
    let
      val tokens = Scan.scan program
      val ast    = Parse.parse tokens
      val result = Eval.eval ast
    in
      (ast, result)
    end

  structure A = AST
  structure S = Scan
  
  (* crowdsourced tests from piazza @85 *)

  val prog00 = "{if {if T then T else F} then {if F then T else F} else {if F then T else F}}"
  val _ = Check.expect (#3 (compile prog00), A.False, "prog00")
  
  val prog01 = "{&& {|| F T} {if T then T else F}}"
  val _ = Check.expect (#3 (compile prog01), A.True, "prog01")
  
  val prog02 = "{|| {|| F T} {|| T F}}"
  val _ = Check.expect (#3 (compile prog02), A.True, "prog02")

  val prog03 = "{if {if {isz {+1 {+1 0}}} then F else {! F}} then {&& {isz 0} {|| T F}} else {! {isz {-1 0}}}}"
  val _ = Check.expect (#3 (compile prog03), A.True, "prog03")



  (* Scan Unit Tests *)
  val _ = Check.expect(S.scan("T"), [S.T], "scan 1")
  val _ = Check.expect(S.scan("{+1  {-1 0}}"), [S.LBrace, S.PlusOne, S.LBrace, S.MinusOne, S.Zero, S.RBrace, S.RBrace], "scan 2")
  val _ = Check.expect(S.scan("{&&{||     {!F} F} T}"), [S.LBrace, S.DoubleAmpersand, S.LBrace, S.DoublePipe, S.LBrace, S.Bang, S.F, S.RBrace, S.F, S.RBrace, S.T, S.RBrace], "scan 3")
  val _ = Check.expect(S.scan("{== T {if {isz 0} then T else F}}"), [S.LBrace, S.DoubleEqual, S.T, S.LBrace, S.If, S.LBrace, S.IsZ, S.Zero, S.RBrace, S.Then, S.T, S.Else, S.F, S.RBrace, S.RBrace], "scan 4")
  val _ = Check.expect(S.scan("\n       F  "), [S.F], "scan 5")
  val _ = Check.expect(S.scan("T //comment  here \n F"), [S.T, S.F], "scan 6")

end