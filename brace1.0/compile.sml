structure Compile : sig

  val compile        : string -> AST.term * Ty.ty * AST.term
  val file           : string -> AST.term * Ty.ty * AST.term

  val compileUntyped : string -> AST.term * AST.term
  val fileUntyped    : string -> AST.term * AST.term

  (* for testing *)
  val comp1           : string -> Scan.token list
  val testFile1       : string -> Scan.token list
  val comp2           : string -> AST.term
  val testFile2       : string -> AST.term
  (* end testing *)

end = struct

  fun println s = (print s; print "\n")

  fun compile program =
    let
      val _      = println ("starting scan: " ^ program)
      val tokens = Scan.scan program
      val _      = println "scan done"
      val ast    = Parse.parse tokens
      val _      = println "parse done"
      val ty     = TypeCheck.typeof ast
      val _      = println "typecheck done"
      val result = Eval.eval ast
    in
      (ast, ty, result)
    end

  fun file filename = compile (Read.file filename)

  fun compileUntyped program =
    let
      val tokens = Scan.scan program
      val ast    = Parse.parse tokens
      val result = Eval.eval ast
    in
      (ast, result)
    end

  fun fileUntyped filename = compileUntyped (Read.file filename)

  (* for testing *)
  fun comp1 program =
    let
      val _      = println ("starting scan: " ^ program)
      val tokens = Scan.scan program
    in
      tokens
    end

  fun comp2 program =
    let
      val _      = println ("starting scan: " ^ program)
      val tokens = Scan.scan program
      val _      = println "scan done"
      val ast    = Parse.parse tokens
    in
      ast
    end
  (* end testing *)
  
  (* for testing *)
  fun testFile1 filename = comp1 (Read.file filename)
  fun testFile2 filename = comp2 (Read.file filename)
  (* end testing *)

end
