structure Alpha : sig

  val equiv : ULC.term * ULC.term -> bool

end = struct

(* Determine if any two terms are the same except for their variable names. *)

(* For example, {a . a} and {b . b} are alpha-equivalent. *)
(* Similarly, {t . {f . f}} and {s . {z . z}} are alpha-equivalent. *)
(* {t . {f . f}} and {@3 . {@4 . @4}} are alpha-equivalent. *)
(* {t . {f . t}} and {s . {z . z}} are not equivalent. *)

  structure U = ULC

  fun equiv (U.Var x, U.Var y) = true
  	| equiv (U.Abs (str1, t1), U.Abs (str2, t2)) =
  		if str1=str2
  		then 
  			(case (t1, t2)
  				of (U.Var x, U.Var y) => x = y
  				 | (_,_) => equiv (t1, t2))
  		else 
			let
				val newVar = Subst.freshVarName ()
				val t' = Subst.subst (str1, ULC.Var newVar, U.Abs (str1, t1))
				val t'' = Subst.subst (str2, ULC.Var newVar, U.Abs (str2, t2))
			in
				equiv (t', t'')
			end 
  	| equiv (U.App (t1, t2), U.App (t3, t4)) = 
  		(case (t1, t2, t3, t4)
  			of (U.Var w, U.Var x, U.Var y, U.Var z) => 
  				if w=x andalso y=z 
  				then true
  				else 
  					if not (w=x) andalso not (y=z)
  					then true
  					else false
  			 | (U.Var w, U.Var x, _, _) => false
  			 | (_, _, U.Var y, U.Var z) => false
  			 | (U.Var w, t', U.Var y, t'') => 
  			 	if w=y
		  		then equiv (t', t'')
		  		else 
					let
						fun subst (x, y, t) = Subst.subst (x, y, t)
						val newVar = Subst.freshVarName ()
						val t' = subst (w, ULC.Var newVar, U.App (t1, t2))
						val t'' = subst (y, ULC.Var newVar, U.App (t3, t4))
					in
						equiv (t', t'')
					end 
			 | (t', U.Var x, t'', U.Var z) => 
  			 	if x=z
		  		then equiv (t', t'')
		  		else 
					let
						fun subst (x, y, t) = Subst.subst (x, y, t)
						val newVar = Subst.freshVarName ()
						val t' = subst (x, ULC.Var newVar, U.App (t1, t2))
						val t'' = subst (z, ULC.Var newVar, U.App (t3, t4))
					in
						equiv (t', t'')
					end 
			 | (U.Abs (str1, t'), U.Abs (str2, t''), U.Abs (str3, t'''), U.Abs (str4, t'''')) =>
			 	let 
			 		fun variables (U.Var x) = [x]@[]
			 		  | variables (U.Abs (s, t)) = s::variables t
			 		  | variables (U.App (t_one, t_two)) = (variables t_one)@(variables t_two)
			 		fun freshVars [] = []
			 		  | freshVars (x::ls) = U.Var (Subst.freshVarName ()) :: freshVars ls
			 		val varsToSubst = variables t1
			 		val newVars = freshVars varsToSubst
			 		fun subst ([], _, t) = t
			 		  | subst (x::ls, s::rs, t) = subst (ls, rs, Subst.subst (x, s, t))
			 		  | subst _ = raise Fail ("Equiv: cannot parse abstractions")
			 		val t_one = subst (varsToSubst, newVars, U.App (t1, t2))
			 		val t_two = subst (variables t3, newVars, U.App (t3, t4))
			 	in 
			 		if equiv (t1, t3) 
			 		then
			 			(case (t_one, t_two)
			 				of (U.App (_, t_three), U.App (_, t_four)) => equiv (t_three, t_four)
			 				 | (_,_) => false)  
			 		else false
			 	end
			 | (U.Abs (str1, t'), _, U.Abs (str3, t'''), _) =>
			 	let 
			 		fun variables (U.Var x) = [x]@[]
			 		  | variables (U.Abs (s, t)) = s::variables t
			 		  | variables (U.App (t_one, t_two)) = (variables t_one)@(variables t_two)
			 		fun freshVars [] = []
			 		  | freshVars (x::ls) = U.Var (Subst.freshVarName ()) :: freshVars ls
			 		val varsToSubst = variables t1
			 		val newVars = freshVars varsToSubst
			 		fun subst ([], _, t) = t
			 		  | subst (x::ls, s::rs, t) = subst (ls, rs, Subst.subst (x, s, t))
			 		  | subst _ = raise Fail ("Equiv: cannot parse abstractions")
			 		val t_one = subst (varsToSubst, newVars, U.App (t1, t2))
			 		val t_two = subst (variables t3, newVars, U.App (t3, t4))
			 	in 
			 		if equiv (t1, t3) 
			 		then
			 			(case (t_one, t_two)
			 				of (U.App (_, t_three), U.App (_, t_four)) => equiv (t_three, t_four)
			 				 | (_,_) => false) 
			 		else false
			 	end
			 | (_, U.Abs (str2, t''), _, U.Abs (str4, t''')) =>
			 	let 
			 		fun variables (U.Var x) = [x]@[]
			 		  | variables (U.Abs (s, t)) = s::variables t
			 		  | variables (U.App (t_one, t_two)) = (variables t_one)@(variables t_two)
			 		fun freshVars [] = []
			 		  | freshVars (x::ls) = U.Var (Subst.freshVarName ()) :: freshVars ls
			 		val varsToSubst = variables t1
			 		val newVars = freshVars varsToSubst
			 		fun subst ([], _, t) = t
			 		  | subst (x::ls, s::rs, t) = subst (ls, rs, Subst.subst (x, s, t))
			 		  | subst _ = raise Fail ("Equiv: cannot parse abstractions")
			 		val t_one = subst (varsToSubst, newVars, U.App (t1, t2))
			 		val t_two = subst (variables t3, newVars, U.App (t3, t4))
			 	in 
			 		if equiv (t2, t4) 
			 		then
			 			(case (t_one, t_two)
			 				of (U.App (t_three, _), U.App (t_four, _)) => equiv (t_three, t_four)
			 				 | (_,_) => false)  
			 		else false
			 	end
			 | (U.Abs (str1, t'), U.Abs (str2, t''), _, _) => false
			 | (_, _, U.Abs (str3, t'''), U.Abs (str4, t'''')) => false
			 | (U.App (t_one', t_two'), U.App (t_three, t_four), U.App (t_five, t_six), U.App (t_seven, t_eight)) =>
			 	let 
			 		fun variables (U.Var x) = [x]@[]
			 		  | variables (U.Abs (s, t)) = s::variables t
			 		  | variables (U.App (t_one, t_two)) = (variables t_one)@(variables t_two)
			 		fun freshVars [] = []
			 		  | freshVars (x::ls) = U.Var (Subst.freshVarName ()) :: freshVars ls
			 		val varsToSubst = variables t1
			 		val newVars = freshVars varsToSubst
			 		fun subst ([], _, t) = t
			 		  | subst (x::ls, s::rs, t) = subst (ls, rs, Subst.subst (x, s, t))
			 		  | subst _ = raise Fail ("Equiv: cannot parse abstractions")
			 		val t_one = subst (varsToSubst, newVars, U.App (t1, t2))
			 		val t_two = subst (variables t3, newVars, U.App (t3, t4))
			 	in 
			 		if equiv (t1, t3) 
			 		then
			 			(case (t_one, t_two)
			 				of (U.App (_, t_three), U.App (_, t_four)) => equiv (t_three, t_four)
			 				 | (_,_) => false)  
			 		else false
			 	end
			 | (_, U.App (t_three, t_four), _, U.App (t_seven, t_eight)) => 
			 	let 
			 		fun variables (U.Var x) = [x]@[]
			 		  | variables (U.Abs (s, t)) = s::variables t
			 		  | variables (U.App (t_one, t_two)) = (variables t_one)@(variables t_two)
			 		fun freshVars [] = []
			 		  | freshVars (x::ls) = U.Var (Subst.freshVarName ()) :: freshVars ls
			 		val varsToSubst = variables t1
			 		val newVars = freshVars varsToSubst
			 		fun subst ([], _, t) = t
			 		  | subst (x::ls, s::rs, t) = subst (ls, rs, Subst.subst (x, s, t))
			 		  | subst _ = raise Fail ("Equiv: cannot parse abstractions")
			 		val t_one = subst (varsToSubst, newVars, U.App (t1, t2))
			 		val t_two = subst (variables t3, newVars, U.App (t3, t4))
			 	in 
			 		if equiv (t2, t4) 
			 		then
			 			(case (t_one, t_two)
			 				of (U.App (t_three, _), U.App (t_four, _)) => equiv (t_three, t_four)
			 				 | (_,_) => false)  
			 		else false
			 	end
			 | (U.App (t_one', t_two'), _, U.App (t_five, t_six), _) => 
			 	let 
			 		fun variables (U.Var x) = [x]@[]
			 		  | variables (U.Abs (s, t)) = s::variables t
			 		  | variables (U.App (t_one, t_two)) = (variables t_one)@(variables t_two)
			 		fun freshVars [] = []
			 		  | freshVars (x::ls) = U.Var (Subst.freshVarName ()) :: freshVars ls
			 		val varsToSubst = variables t1
			 		val newVars = freshVars varsToSubst
			 		fun subst ([], _, t) = t
			 		  | subst (x::ls, s::rs, t) = subst (ls, rs, Subst.subst (x, s, t))
			 		  | subst _ = raise Fail ("Equiv: cannot parse abstractions")
			 		val t_one = subst (varsToSubst, newVars, U.App (t1, t2))
			 		val t_two = subst (variables t3, newVars, U.App (t3, t4))
			 	in 
			 		if equiv (t1, t3) 
			 		then
			 			(case (t_one, t_two)
			 				of (U.App (_, t_three), U.App (_, t_four)) => equiv (t_three, t_four)
			 				 | (_,_) => false)  
			 		else false
			 	end
			 | (_, _, U.App (t_five, t_six), U.App (t_seven, t_eight)) => false
			 | (U.App (t_one, t_two), U.App (t_three, t_four), _, _) => false
			 | (_,_,_,_) => false)
	| equiv (_, _) = false
	
		      
(* Some tests would be nice here... *)
(* HW5 Crowdsourced Tests from post @215 *)

val term1  = U.Abs("x", U.Abs("y", U.Var("z")))                      (* {x.{y.z}} *)
val term2  = U.Abs("z", U.Abs("y", U.Abs("x", U.Var("z"))))          (* {z.{y.{x.z}}} *)
val term3  = U.Abs("a", U.Abs("b", U.Var("c")))                      (* {a.{b.c}} *)
val term4  = U.Abs("c", U.Abs("b", U.Abs("a", U.Var("c"))))          (* {c.{b.{a.c}}} *)
val term5  = U.App(term1, term2)                                     (* ({x.{y.z}}, {z.{y.{x.z}}}) *)
val term6  = U.App(term3, term4)                                     (* ({a.{b.c}}, {c.{b.{a.c}}}) *)
val term7  = U.App(term5, term6)                                 
val term8  = U.App(term6, term5)
val term9  = U.App(term1, term4)
val term10 = U.App(term3, term2)
val term11 = U.App(term2, term3)
val term12 = U.Abs("x", U.Var("y"))                                  (* {x.y} *)
val term13 = U.Abs("z", U.Var("w"))                                  (* {z.w} *)
val term14 = U.Abs("x", U.Abs("z", U.Var("w")))                      (* {x.{z.w}} *)
val term15 = U.Abs("x", U.Abs("y", U.Var("x")))                      (* {x.{y.x}} *)
val term16 = U.App(term12,term14)                                    (* ({x.y} {x.{z.w}}) *)
val term17 = U.App(term15,term13)                                    (* ({x.{y.x}} {z.w}) *)


(*val _ = Check.expect(equiv(term1,term2),   false, "equiv: t1, t2")
val _ = Check.expect(equiv(term1,term3),   true,  "equiv: t1, t3")
val _ = Check.expect(equiv(term2,term3),   false, "equiv: t2, t3")
val _ = Check.expect(equiv(term2,term4),   true,  "equiv: t2, t4")
val _ = Check.expect(equiv(term5,term6),   true,  "equiv: t5, t6")
val _ = Check.expect(equiv(term7,term8),   true,  "equiv: t7, t8")
val _ = Check.expect(equiv(term10,term9),  true,  "equiv: t10, t9")
val _ = Check.expect(equiv(term10,term11), false, "equiv: t10,t11")
val _ = Check.expect(equiv(term12,term13), true,  "equiv: t12,t13")
val _ = Check.expect(equiv(term13,term14), false, "equiv: t13,t14")
val _ = Check.expect(equiv(term14,term15), false, "equiv: t14,t15")
val _ = Check.expect(equiv(term16,term17), false, "equiv: t16,t17")
*)
end
