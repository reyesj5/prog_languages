structure Parse : sig

  val parse : Scan.token list -> AST.term

end = struct

  structure S = Scan
  structure A = AST

(* Note: you may erase this implementation and replace with your own parser. *)
(* It is an optional starting point. *)
  fun nextTerm (tokens : S.token list) : (A.term * S.token list) option = 
    let
      fun lp [] = NONE
        | lp (S.T::ts)    = SOME (A.True, ts)
        | lp (S.F::ts)    = SOME (A.False, ts)
        | lp (S.Zero::ts) = SOME (A.Zero, ts)
        | lp (S.LBrace::S.If::ts) = 
           (case lp ts
              of SOME (t1, S.Then::ts1) =>
                  (case lp ts1
                     of SOME (t2, S.Else::ts2) =>
                         (case lp ts2
                            of SOME (t3, S.RBrace::ts3) => SOME (A.If (t1, t2, t3), ts3)
                             | SOME _ => raise Fail "expected }"
                             | NONE => raise Fail "parse error: if ended before }")
                      | SOME _ => raise Fail "expected else"
                      | NONE => raise Fail "parse error: if ended before else")
                | SOME _ => raise Fail "expected then"
                | NONE => raise Fail "parse error: if ended before then")
        | lp (S.LBrace::S.PlusOne::ts)  = lp1RBrace (A.Succ, ts, "+1")
        | lp (S.LBrace::S.MinusOne::ts) = lp1RBrace (A.Pred, ts, "-1")
        | lp (S.LBrace::S.IsZ::ts)      = lp1RBrace (A.IsZero, ts, "isz") 
        | lp (S.LBrace::S.Bang::ts)     = lp1RBrace (A.Not, ts, "!")
        | lp (S.LBrace::S.DoubleAmpersand::ts) = lp2RBrace (A.And, ts, "&&")
        | lp (S.LBrace::S.DoublePipe::ts)      = lp2RBrace (A.Or, ts, "||")
        | lp (S.LBrace::S.DoubleEqual::ts)     = lp2RBrace (A.Equal, ts, "==")
        | lp (S.LParen::S.RParen::ts) = SOME (A.Unit, ts)
        | lp (S.Identifier s::ts) = SOME (A.Var s, ts)
        | lp (S.LParen::S.Identifier s::S.LeftArrow::ts) =
          (case lp ts
              of SOME (t1, S.In::ts1) =>
                  (case lp ts1
                    of SOME (t2, S.RBrace::ts2) => SOME (A.Let (s, t1, t2), ts2)
                     | SOME _ => raise Fail "expected }"
                     | NONE => raise Fail "parse error: let ended before }")
                | SOME _ => raise Fail "expected in in variable binding"
                | NONE => raise Fail "parse error: let ended before in")
        | lp (S.LBrace::S.Hash::ts)  = lp2RBrace (A.Pair, ts, "#")
        | lp (S.LBrace::S.Hash1::ts) = lp1RBrace (A.Select1, ts, "#1")
        | lp (S.LBrace::S.Hash2::ts) = lp1RBrace (A.Select2, ts, "#2")
        | lp (S.LBrace::S.Some::ts)  = lp1RBrace (A.Some, ts, "some")
        | lp (S.LBrace::S.None::ts) =
          (case nextIdentifier ts
            of SOME (ty, S.RBrace::ts') => SOME (A.None (Ty.Opt ty), ts')
             | SOME _ => raise Fail "expected } in none type assignment"
             | NONE => raise Fail "parse error: none ended before }")
        | lp (S.LBrace::S.Case::ts) =
          (case lp ts
            of SOME (t1, S.Of::S.Some::S.Identifier s::S.RightArrow::ts1) =>
                  (case lp ts1
                    of SOME (t2, S.Pipe::S.None::S.RightArrow::ts2) => 
                      (case lp ts2
                            of SOME (t3, S.RBrace::ts3) => SOME (A.OptCase (t1, (s, t2), t3), ts3)
                             | SOME _ => raise Fail "expected } in OptCase"
                             | NONE => raise Fail "parse error: optcase ended before }")
                     | SOME _ => raise Fail "parse error: expected second term"
                     | NONE => raise Fail "parse error: identifier ended unexpectedly (2)")
                | SOME _ => raise Fail "expected term one in OptCase"
                | NONE => raise Fail "parse error: let ended unexpectedly (1)")
	      | lp _ = raise Fail "error or unimplemented expression form"
      and lp1RBrace (constructor, toks, nameOfThing) =
       (case lp toks
          of SOME (t1, S.RBrace::ts1) => SOME (constructor t1, ts1)
           | SOME _ => raise Fail ("expected } in " ^ nameOfThing)
           | NONE => raise Fail ("parse error: " ^ nameOfThing ^ " ended unexpectedly"))
      and lp2RBrace (constructor, toks, nameOfThing) = 
       (case lp toks
          of SOME (t1, ts1) =>
	           (case lp ts1
               of SOME (t2, S.RBrace::ts2) => SOME (constructor (t1, t2), ts2)
		            | SOME _ => raise Fail ("expected } in " ^ nameOfThing) 
                | NONE => raise Fail ("parse error: " ^ nameOfThing ^ " ended unexpectedly (2)"))
           | NONE => raise Fail ("parse error: " ^ nameOfThing ^ " ended unexpectedly (1)"))
      and nextIdentifier [] = NONE
        | nextIdentifier (S.Identifier s::ts) = 
         (case s
            of "Nat" => SOME (Ty.Nat, ts)
             | "Bool" => SOME (Ty.Bool, ts)
             | "Unit" => SOME (Ty.Unit, ts)
             | _ => raise Fail "parse error: none ended with unknown type")
        | nextIdentifier (S.LParen::S.Opt::ts) =
          (case nextIdentifier ts
            of SOME (ty1, S.RParen::ts') => SOME (Ty.Opt ty1, ts')
             | SOME _ => raise Fail "expected ) in none opt type assignment"
             | NONE => raise Fail "parse error: none ended before )")
        | nextIdentifier (S.LParen::S.Star::ts) =
          (case nextIdentifier ts
            of SOME (ty1, ts1) =>
               (case nextIdentifier ts1
                 of SOME (ty2, S.RParen::ts2) => SOME (Ty.Pair (ty1, ty2), ts2)
                  | SOME _ => raise Fail ("expected ) in identifier") 
                  | NONE => raise Fail ("parse error: identifier ended unexpectedly (2)"))
             | NONE => raise Fail ("parse error: identifier ended unexpectedly (1)"))
        | nextIdentifier _ = raise Fail ("parse error: unknown type given for none")

    in
      lp tokens
    end

  fun parse tokens = 
   (case nextTerm tokens
      of SOME (term, []) => term
       | SOME (term, extra) => raise Fail "extra tokens after main term in program"
       | NONE => raise Fail "empty program")

end
