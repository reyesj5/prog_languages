structure Parse : sig

  val parse : Scan.token list -> AST.term

end = struct

  structure S = Scan
  structure A = AST

  fun isIdentifier (S.Identifier _) = true
    | isIdentifier _ = false
		    
  fun nextTerm t =
    let
      fun lp (S.Identifier x :: ts) = SOME (A.Var x, ts)
        | lp (S.LBrace :: S.Identifier x :: S.LeftArrow :: ts) =
           (case lp ts
              of SOME (t1, S.In::ts1) =>
                  (case lp ts1
                     of SOME (t2, S.RBrace::ts2) => SOME (A.Let (x, t1, t2), ts2)
                      | SOME _ => raise Fail "expected closing brace in let"
                      | NONE   => raise Fail "program ended unexpectedly in let (2)")
               | SOME _ => raise Fail "expected \"in\" in let"
               | NONE   => raise Fail "program ended unexpectedly in let (1)")
	| lp (S.LBrace :: S.Identifier x :: S.DoubleLeftArrow :: ts) =
	  (case lp ts
            of SOME (t1, S.In::ts1) =>
               (case lp ts1
                 of SOME (t2, S.RBrace::ts2) => SOME (A.LetRec (x, t1, t2), ts2)
                  | SOME _ => raise Fail "expected closing brace in letrec"
                  | NONE   => raise Fail "program ended unexpectedly in letrec (2)")
             | SOME _ => raise Fail "expected \"in\" in letrec"
             | NONE   => raise Fail "program ended unexpectedly in letrec (1)") 
        | lp (S.LBrace :: S.Identifier x :: ts) =
	    let
	      val (xs, ts1) = Utils.takeWhile isIdentifier ts
	      val xs' = List.map (fn (S.Identifier x) => x | _ => raise Fail "?") xs
	    in
		case ts1
		 of S.Dot::ts2 =>
		    (case lp ts2
		      of SOME (body, S.RBrace::ts3) => SOME (A.Abs (x::xs', body), ts3)
		       | SOME _ => raise Fail "expected closing brace in abstraction"
		       | NONE   => raise Fail "program ended unexpectedly in abstraction")
		 | _ => raise Fail "expected . in abstraction"
	    end
        | lp (S.LParen :: ts) =
	  (case untilRParen ts
	    of ([], _) => raise Fail "no terms in app"
	     | ([t], _) => raise Fail "only one term in app"
	     | (ts', ts'') => SOME (A.App ts', ts''))
	| lp (S.T :: ts) = SOME (A.True, ts)
	| lp (S.F :: ts) = SOME (A.False, ts)
	| lp (S.LBrace :: S.If :: ts) = ifIsSuchAPain ts
	| lp (S.LBrace :: S.Bang :: ts) = lp1 A.Not ts "not"
	| lp (S.LBrace :: S.DoubleAmpersand :: ts) = lp2 A.And ts "and"
	| lp (S.LBrace :: S.DoublePipe :: ts) = lp2 A.Or ts "or"
	| lp (S.Nat n :: ts) = SOME (A.Nat n, ts)
	| lp (S.LBrace :: S.Plus :: ts) = lp2 A.Add ts "add"
	| lp (S.LBrace :: S.Star :: ts) = lp2 A.Mult ts "mult"
	| lp (S.LBrace :: S.Hash :: ts) = lp2 A.Pair ts "pair"
	| lp (S.LBrace :: S.Hash1 :: ts) = lp1 A.Select1 ts "#1"
	| lp (S.LBrace :: S.Hash2 :: ts) = lp1 A.Select2 ts "#2"
	| lp (S.Slash :: S.Identifier x :: ts) = SOME (A.ID x, ts)
        | lp [] = NONE
        | lp ts = raise Fail ("parse error at " ^
			      (String.concatWith " " 
			       (List.map S.tos (Utils.upto 10 ts))))
      and lp1 k ts what =
       (case lp ts
	 of SOME (t1, S.RBrace::ts1) => SOME (k t1, ts1)
	  | SOME (t1, _) => raise Fail ("expected right brace after " ^
					A.unparse t1 ^ " in " ^ what)
	  | NONE => raise Fail ("program ended unexpectedly parsing " ^ what))
      and lp2 k ts what =
        (case lp ts
	  of SOME (t1, ts1) =>
	     (case lp ts1
		 of SOME (t2, S.RBrace::ts2) => SOME (k (t1, t2), ts2)
		  | SOME _ => raise Fail ("expected right brace after " ^
					  A.unparse t1 ^ " in " ^ what)
		  | NONE => raise Fail ("program ended unexpectedly parsing " ^
					what ^ " (2)"))		  
	   | NONE => raise Fail ("program ended unexpectedly parsing " ^
				 what ^ " (1)"))
      and untilRParen ts =
	  (case lp ts
	    of SOME (t, S.RParen::ts') => ([t], ts')
	     | SOME (t, ts') => cons1 t (untilRParen ts')
	     | NONE => raise Fail "program ended unexpectedly in app")
      and cons1 x (y, z) = (x::y, z)
      and ifIsSuchAPain ts =
        (case lp ts
	  of SOME (t1, S.Then::ts1) =>
	     (case lp ts1
	       of SOME (t2, S.Else::ts2) =>
		  (case lp ts2
		    of SOME (t3, S.RBrace::ts3) => SOME (A.If (t1, t2, t3), ts3)
		     | SOME _ => raise Fail "expected rbrace in if"
		     | NONE => raise Fail "program ended unexpectedly in if (3)")
		| SOME _ => raise Fail "expected \"else\" in if"
		| NONE => raise Fail "program ended unexpectedly in if (2)")
	   | SOME _ => raise Fail "expected \"then\" in if"
	   | NONE => raise Fail "program ended unexpectedly in if (1)")
    in
      lp t
    end
        
  fun parse tokens = 
    (case nextTerm tokens
       of SOME (t, []) => t
        | SOME _ => raise Fail "extra tokens after main term"
        | NONE => raise Fail "no main term?")

end
