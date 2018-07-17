structure Desugar : sig

  val term : AST.term -> MULC.term

end = struct

  structure A = AST
  structure M = MULC

  fun term (A.Var x) = M.Var x
    | term (A.Abs (ls, t)) = M.Abs (ls, term t)
    | term (A.App ls) = 
    	let 
    		fun lp [] = []
    		  | lp (t::terms) = [term t] @ lp terms
    	in 
    		M.App (lp ls)
    	end
    | term (A.Let (x, t1, t2)) = M.App [M.Abs([x], term t2), term t1]
    | term (A.LetRec (x, t1, t2)) = 
    	let 
    		fun fix (M.Abs (x::nil, t)) = M.App [M.Abs([x], fix (M.Abs (x::nil, t))), t]
    		  | fix _ = raise Fail ("LetRec failed")
    	in
    		M.App [M.Abs([x], fix (M.Abs ([x], term t1))), term t2]	
    	end 
    | term A.True = M.Abs (["t", "f"], M.Var "t")
    | term A.False = M.Abs (["t", "f"], M.Var "f")
    | term (A.If (t1, t2, t3)) = M.App [term t1, term t2, term t3]
    | term (A.Not t1) = M.App [M.Abs (["p"], M.App [M.Var "p", term A.False, term A.True]), term t1]
    | term (A.And (t1, t2)) = M.App [M.Abs (["b"], M.Abs (["c"], M.App [M.Var "b", M.Var "c", term A.False])), term t1, term t2]
    | term (A.Or (t1, t2)) = M.App [M.Abs (["b"], M.Abs (["c"], M.App [M.Var "b", term A.True, M.Var "c"])), term t1, term t2]
    | term (A.Pair (t1, t2)) = M.Abs (["b"], M.App [M.Var "b", term t1, term t2])
    | term (A.Select1 t1) = M.App [M.Abs (["p"], M.App [M.Var "p", term A.True]), term t1]
    | term (A.Select2 t1) = M.App [M.Abs (["p"], M.App [M.Var "p", term A.False]), term t1]
    | term (A.Nat n) = 
    	let 
    		fun lp 1 = M.App [M.Var "s", M.Var "z"]
    		  | lp n = M.App [M.Var "s", lp (n-1)] 
    	in 
    		if n=0 
    		then M.Abs (["s", "z"], M.Var "z")
    		else M.Abs (["s", "z"], lp n)
    	end 
    | term (A.Add (t1, t2)) = 
    	M.App [M.Abs (["m"], M.Abs (["n"], M.Abs (["s"], M.Abs (["z"], M.App [M.Var "m", M.Var "s", M.App [M.Var "n", M.Var "n", M.Var "z"]])))), term t1, term t2]
    | term (A.Mult (t1, t2)) = M.App [M.Abs (["m"], M.Abs 
    (["n"], M.App [M.Var "m", M.App [M.Abs (["a"], M.Abs (["b"], M.Abs 
    	(["c"], M.Abs (["d"], M.App [M.Var "a", M.Var "c", M.App 
    		[M.Var "b", M.Var "b", M.Var "d"]])))), M.Var "n"], M.Abs (["s"], M.Abs (["z"], M.Var "z"))])), term t1, term t2]
    | term (A.ID a) = M.Abs ([a], M.Var a)


(* HW5 Crowdsourced Tests from post @215 *)

val _ = Check.expect(term(A.Var("hi")), M.Var("hi"), "term0")
val _ = Check.expect(term(A.Abs(["hi", "there", "my"], A.Var("Friend"))),
M.Abs(["hi", "there", "my"], M.Var("Friend")), "term1")
val _ = Check.expect(term(A.App([A.Abs(["hi", "there", "my"], A.Var("Friend")), A.Var("ender")])),
M.App([M.Abs(["hi", "there", "my"], M.Var("Friend")), M.Var("ender")]), "term2")
val _ = Check.expect(term(A.True), M.Abs(["t", "f"], M.Var("t")), "term3")
val _ = Check.expect(term(A.False), M.Abs(["t", "f"], M.Var("f")), "term4")

val _ = Check.expect(term(A.If(A.True, A.Nat(0), A.Nat(1))),
M.App([term(A.True), term(A.Nat(0)), term(A.Nat(1))]), "term5")

val _ = Check.expect(term(A.Nat(0)), M.Abs(["s", "z"], M.Var("z")) , "term6")
val _ = Check.expect(term(A.Nat(3)), M.Abs(["s", "z"], M.App([M.Var("s"), M.App([M.Var("s"),
M.App([M.Var("s"), M.Var("z")])])])), "term7")
val _ = Check.expect(term(A.Pair(A.Nat(50), A.Nat(30))),
M.Abs(["b"], M.App([M.Var("b"),term(A.Nat(50)), term(A.Nat(30))])), "term8")
val _ = Check.expect(term(A.ID("x")), M.Abs(["x"], M.Var("x")), "term9")

end