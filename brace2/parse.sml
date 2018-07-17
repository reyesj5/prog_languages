structure Parse : sig

  val parse : Scan.token list -> AST.term

end = struct

  structure S = Scan
  structure A = AST

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
        | lp (S.LBrace :: S.Identifier x :: ts) =
           (case lp ts
              of SOME (body, S.RBrace::ts') => SOME (A.Abs (x, body), ts')
               | SOME _ => raise Fail "expected closing brace in abstraction"
               | NONE   => raise Fail "program ended unexpectedly in abstraction")
        | lp (S.LParen :: ts) =
           (case lp ts
              of SOME (t1, ts1) =>
                  (case lp ts1
                     of SOME (t2, S.RParen::ts2) => SOME (A.App (t1, t2), ts2)
                      | SOME _ => raise Fail "expected closing paren in application"
                      | NONE   => raise Fail "program ended unexpectedly in application (2)")
               | NONE => raise Fail "program ended unexpectedly in application (1)")
        | lp [] = NONE
        | lp ts = raise Fail "parse error"
    in
      lp t
    end
        
  fun parse tokens = 
    (case nextTerm tokens
       of SOME (t, []) => t
        | SOME _ => raise Fail "extra tokens after main term"
        | NONE => raise Fail "no main term?")

end
