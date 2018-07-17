structure Compile : sig

  datatype 'a attempt
    = Success of 'a
    | Failure of {reason: string}

  val compile   : string -> (Ty.ty * AST.term) attempt
  val compile'  : string -> (Ty.ty * AST.term list) attempt
  val compile'' : string -> unit
						   
  val file      : string -> (Ty.ty * AST.term) attempt
  val file'     : string -> (Ty.ty * AST.term list) attempt
  val file''    : string -> unit
						   
end = struct

  datatype 'a attempt
    = Success of 'a
    | Failure of {reason: string}

  val println = Utils.println

  fun cmp program =
    let
      val tokens = Scan.scan program
      val ast    = Parse.parse tokens
      val ty     = TypeCheck.typeof ast
      val result = Eval.eval ast
    in
      (ty, result)
    end

  fun compile program = (Success (cmp program)) handle Fail msg => Failure {reason=msg}
  
  fun cmp' program =
    let
      val tokens = Scan.scan program
      val ast    = Parse.parse tokens
      val ty     = TypeCheck.typeof ast
      val result = Eval.steps ast
    in
      (ty, result)
    end

  fun compile' program = (Success (cmp' program)) handle Fail msg => Failure {reason=msg}

  fun compile'' program =
    let
      val _ = println ("original program: " ^ program)
    in
      case compile' program
        of Failure {reason} => Utils.println ("compilation failed: " ^ reason)
 	 | Success (ty, terms) => (println "";
				   println ("type: " ^ Ty.tos ty);
				   println "";
				   println "evaluation:" ;
				   List.app (println o AST.tos) terms)
    end

  fun generalizedFile compileFn =
    fn filename => (compileFn (Read.file filename))
		   handle IO.Io _ => Failure {reason="file '" ^ filename ^ "' not found"}
      
  val file   = generalizedFile compile
  val file'  = generalizedFile compile'

  fun file'' filename = (compile'' (Read.file filename))
			handle IO.Io _ => println ("IO.Io exception: file '" ^ filename ^ "' not found")

  (* HW6 Crowdsourced Tests from Piazza @243 *)
  structure A = AST

  val _ = Check.expect(compile("({x: (-> Bool Bool) . {not T}} {x: Bool . T})"), Success(Ty.Bool,A.False),
  "compile application test 1")
  val _ = Check.expect(compile("{w <- {if T F F} in ({x: (-> Bool Bool) . {not w}} {y: Bool . T})}"),
  Success(Ty.Bool,A.True), "compile application test 2")
  val _ = Check.expect(compile("{w <- {if T F F} in ({x: {a:(-> Bool Bool)} .{not w}} {a = {y: Bool . T},b = T})}"),
  Success(Ty.Bool,A.True), "compile application test 3")
  val _ = Check.expect(compile("({x: Bool . x} T)"), Success(Ty.Bool,A.True),"compile application test 4")
  val _ = Check.expect(compile("({x: (-> Bool Bool) . {not (x F )}} {x: Bool . T})"), Success(Ty.Bool,A.False),
  "compile application test 5")
  val _ = Check.expect(compile("{w <- {if T F F} in {x: Bool .w}}"),
  Success(Ty.Function(Ty.Bool,Ty.Bool),A.Abs("x",Ty.Bool,A.False)), "compile abs test1")
  val _ = Check.expect(compile("{w <- {if T F F} in {w: Bool .w}}"),
  Success(Ty.Function(Ty.Bool,Ty.Bool),A.Abs("w",Ty.Bool,A.Var "w")), "compile abs test2")
  
  val let1 = "{b <- F in {if {not b} ({c : Unit . c} ()) ()}}"
  val simpref = "{:= {ref F} T}"
  val if1 = "{if {not {!{ref F}}} ({r : {a:Bool} . {#a r}} {b = (), a = T, c = F}) F}"
  val seq1 = "{$ " ^ let1 ^ " " ^ simpref ^ " " ^ if1 ^ "}"
  

  val _ = Check.expect (compile seq1, Success (Ty.Bool, AST.True), "TEST1")
  val _ = Check.expect (compile "{#a {b=F, a=T}}", Success (Ty.Bool, AST.True), "TEST4")

  (*failure tests; uncomment and fill in your personal error messages*)

  (*val _ = Check.expect (compile "{var <- {a=F, g={not T}, k={ref ()}} in ({rec : {k:Unit, a:Bool} . {!{#k rec}}} var)}", Failure {reason="type error: attempted read of non ref cell"}, "TEST4")
  val _ = Check.expect (compile "{#a {b={a=T}}}", Failure {reason="type error: record does not contain label"}, "TEST5")
  val _ = Check.expect (compile "{:= {ref {a=F, var=T}} {a=T, var=()}}", Failure {reason="type error: attempted write to non-matching type"}, "TEST6")
  val _ = Check.expect (compile "{:= {ref {a=F, var=T}} {a=T, var=T, b=T}}", Failure {reason="type error: attempted write to non-matching type"}, "TEST7")
  val _ = Check.expect (compile "{:= {ref {a=F, var=T}} {a=T, var=()}}", Failure {reason="type error: attempted write to non-matching type"}, "TEST8")
  *)
end

