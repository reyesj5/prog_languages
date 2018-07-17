structure Curry : sig

  val term : MULC.term -> ULC.term
	    
end = struct

(* Rewrite terms with multi-argument functions and multi-applications
 * into the plain ULC. Make sure that applications associate to the
 * left; that is, rewrite (a b c) to ((a b) c).
 *)

structure M = MULC
structure U = ULC


fun term (M.Var x) = U.Var x 
  | term (M.Abs (ls, t)) =
  	let
  		fun lp ([], t) = t
  		  | lp (x::ls', t) = U.Abs (x, lp (ls', t)) 
  	in 
  		lp (ls, term t)
  	end
  | term (M.App ls) =
  	let
  		fun lp ([], t) = t
  		  | lp (t'::ls', t) = lp (ls', U.App (t, term t'))
  	in
  		lp (List.tl (List.tl ls), U.App (term (List.hd ls), term ((List.hd (List.tl ls)))))
  	end

(* Write some tests here! This is tricky to get right. *)	
(* HW5 Crowdsourced Tests from post @215 *)
val cTerm1  = M.Var("x")
val cTerm2  = M.App [M.Var("a"), M.Var("b"), M.Var("c")]
val cTerm3  = M.App [M.Var("a"), M.Var("b"), M.Var("c"), M.Var("d")]
val cTerm4  = M.Abs (["x"], M.Var("y"))
val cTerm5  = M.Abs (["t", "f"], M.Var("t"))                  (*{[t f].t} -> {t.{f.t}}*)
val cTerm6  = M.Abs (["a","b","c","d"], M.Var("t"))           (*{[a,b,c,d].t} -> {a.{b.{c.{d.t}}}}*)
val cTerm7  = M.Abs (["a","b","c","d","e","f","g"], M.Var("h"))
val cTerm8  = M.Abs (["a","b"], M.App [M.Var("c"), M.Var("d"), M.Var("e")])
val cTerm9  = M.Abs (["x","y","z"], M.Abs(["a","b"],M.Var("c")))
val cTerm10 = M.App [M.Abs(["x","y"],M.Abs(["a","b"],M.Var("c"))), M.App [M.Var("d"),M.Var("e"),M.Var("f")]] 

val _ = Check.expect(term cTerm1,  U.Var("x"),"cTerm1")
val _ = Check.expect(term cTerm2,  U.App(U.App(U.Var("a"), U.Var("b")), U.Var("c")), "cTerm2")
val _ = Check.expect(term cTerm3,  U.App(U.App(U.App(U.Var("a"), U.Var("b")), U.Var("c")), U.Var("d")), "cTerm3")
val _ = Check.expect(term cTerm4,  U.Abs("x",U.Var("y")), "cTerm4")
val _ = Check.expect(term cTerm5,  U.Abs("t", U.Abs("f", U.Var("t"))), "cTerm5")
val _ = Check.expect(term cTerm6,  U.Abs("a",U.Abs("b",U.Abs("c",U.Abs("d",U.Var("t"))))), "cTerm6")
val _ = Check.expect(term cTerm7,  U.Abs("a",U.Abs("b",U.Abs("c",U.Abs("d",U.Abs("e",U.Abs("f",U.Abs("g",U.Var("h")))))))), "cTerm7")
val _ = Check.expect(term cTerm8,  U.Abs("a", U.Abs("b", U.App(U.App(U.Var("c"),U.Var("d")),U.Var("e")))), "cTerm8")
val _ = Check.expect(term cTerm9,  U.Abs("x",U.Abs("y",U.Abs("z", U.Abs("a",U.Abs("b", U.Var("c")))))), "cTerm9") 
val _ = Check.expect(term cTerm10, U.App(U.Abs("x",U.Abs("y",U.Abs("a",U.Abs("b",U.Var("c"))))), U.App(U.App(U.Var("d"),U.Var("e")),U.Var("f"))), "cTerm10") 

end