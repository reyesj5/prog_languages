structure Compile : sig

(* Carefully read this signature to see what's available. *)
              
  datatype evaluation_strategy
    = CBV
    | FullBeta
    | Lazy
                                       
(* These compile programs directly. *)
  val compile  : evaluation_strategy -> string -> ULC.term
  val compile' : evaluation_strategy -> string -> ULC.term list

(* These open files and compile the programs in the them. *)
  val file     : evaluation_strategy -> string -> ULC.term
  val file'    : evaluation_strategy -> string -> ULC.term list

(* These last three are provided for convenience and quick testing. *)
  val cbv      : string -> ULC.term
  val fullBeta : string -> ULC.term
  val lazy     : string -> ULC.term
                                                           
end = struct

  fun println s = (TextIO.print s; TextIO.print "\n")

  datatype evaluation_strategy
    = CBV
    | FullBeta
    | Lazy

(* choose a reduction system *)    
  fun reducer CBV      = CallByValue.reduce
    | reducer FullBeta = FullBeta.reduce
    | reducer Lazy     = Lazy.reduce

  fun steps CBV      = CallByValue.steps
    | steps FullBeta = FullBeta.steps
    | steps Lazy     = Lazy.steps

  fun openTermWarning (t : ULC.term) : unit =
   (case Subst.freeVars t
      of [] => ()
       | ss => println ("*** warning: term contains free variables " ^
                        String.concatWith ", " ss))

  fun compile strategy program =
    let
      val tokens = Scan.scan program
      val ast    = Parse.parse tokens
      val ulc    = Desugar.term ast
      val _      = openTermWarning ulc
      val result = (reducer strategy) ulc
    in
      result
    end

  fun compile' strategy program =
    let
      val tokens = Scan.scan program
      val ast    = Parse.parse tokens
      val ulc    = Desugar.term ast      
      val _      = openTermWarning ulc
      val result = (steps strategy) ulc
    in
      result
    end

  fun file strategy filename  = compile strategy (Read.file filename)

  fun file' strategy filename = compile' strategy (Read.file filename)

  val cbv      = compile CBV
  val fullBeta = compile FullBeta
  val lazy     = compile Lazy

  val cbv'      = compile' CBV
  val fullBeta' = compile' FullBeta
  val lazy'     = compile' Lazy


  (* crowdsourced tests from piazza @184 *)
  val _ = Check.expect(cbv("({a a} {b b})"), ULC.Abs("b", ULC.Var("b")), "cbv('({a a} {b b})')")
  val _ = Check.expect(cbv("(({a {b a}} {c c}) {d d})"), ULC.Abs("c", ULC.Var("c")), "cbv('(({a {b a}} {c c} {d d}))")
  val _ = Check.expect(cbv("(x y)"), ULC.App(ULC.Var("x"), ULC.Var("y")), "cbv('(x y)')")
  val _ = Check.expect(cbv("(x {a a})"), ULC.App(ULC.Var("x"), ULC.Abs("a", ULC.Var("a"))), "cbv('(x {a a})')")
  val _ = Check.expect(cbv("({a a} x)"), ULC.App(ULC.Abs("a", ULC.Var("a")), ULC.Var("x")), "cbv('({a a} x)')")
  val _ = Check.expect(cbv("(({a {b b}} {c c}) {d d})"), ULC.Abs("d", ULC.Var("d")), "cbv('(({a {b b}} {c c} {d d}))")

  fun IDL x = "{" ^ x ^ " " ^ x ^ "}"
 fun appL (t1, t2) = "(" ^ t1 ^ " " ^ t2 ^ ")"
 fun appRevL (t2, t1) = appL (t1, t2)
 fun appMultL (t :: ts) = foldl appRevL t ts
   | appMultL [] = raise Fail ("application cannot be empty")
 val truL = "{t {f t}}"
 val flsL = "{t {f f}}"
 val testL = "{l {m {n ((l m) n)}}}"
 val andL = "{b {c ((b c) " ^ flsL ^ ")}}"
 val orL = "{b {c ((b " ^ truL ^ ") c)}}"
 val prog00 = appMultL [testL, truL, IDL "a", IDL "b"]
 val prog01 = appMultL [testL, flsL, IDL "a", IDL "b"]
 val prog02 = appMultL [andL, truL, truL]
 val prog03 = appMultL [andL, truL, flsL]
 val prog04 = appMultL [andL, flsL, truL]
 val prog05 = appMultL [andL, flsL, flsL]
 val prog05 = appMultL [andL, flsL, flsL]
 val prog06 = appMultL [orL, truL, truL]
 val compiler = fullBeta
 val _ = Check.expect (ULC.tos (compiler prog00), IDL "a", "test tru")
 val _ = Check.expect (ULC.tos (compiler prog01), IDL "b", "test fls")
 val _ = Check.expect (ULC.tos (compiler prog02), truL, "test and 1")
 val _ = Check.expect (ULC.tos (compiler prog03), flsL, "test and 2")
 val _ = Check.expect (ULC.tos (compiler prog04), flsL, "test and 3")
 val _ = Check.expect (ULC.tos (compiler prog05), flsL, "test and 4")
 
 val _ = Check.expect(
      ULC.tos(cbv("({x {y y}} a)")),
      "({x {y y}} a)",
      "lazy test 1"
    )

    val _ = Check.expect(
      ULC.tos(cbv("({x {y y}} a)")),
      "({x {y y}} a)",
      "lazy test 2"
    )

    val _ = Check.expect(
      ULC.tos(fullBeta("({x {y y}} a)")),
      "{y y}",
      "lazy test 3"
    )

    val _ = Check.expect(
      ULC.tos(lazy("({x {y y}} a)")),
      "{y y}",
      "lazy test 4"
    )

    val _ = Check.expect(
      ULC.tos(fullBeta("{y ({x x} y)}")),
      "{y y}",
      "lazy test 5"
    )

    val _ = Check.expect(
      ULC.tos(lazy("{y ({x x} y)}")),
      "{y ({x x} y)}",
      "lazy test 6"
    )

end

