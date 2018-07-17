structure Parse = struct

  structure S = Scan
  structure A = AST

  fun cons1 x (y, z) = (x::y, z)

  fun err context   = raise Fail ("parse error in " ^ context)
  fun unexp context = raise Fail ("parse error: program ended expectedly in " ^ context)

  fun nextTy tokens =
    let
      fun lp [] = NONE
        | lp (S.Identifier "Bool" :: ts) = SOME (Ty.Bool, ts)	  
        | lp (S.Identifier "Unit" :: ts) = SOME (Ty.Unit, ts)
	| lp (S.LParen :: S.RightArrow :: ts) =
	    lp2 ts Ty.Function "function type"
	| lp (S.LParen :: S.Identifier "Ref" :: ts) =
            (case lp ts
	       of SOME (ty, S.RParen::ts') => SOME (Ty.Ref ty, ts')
	        | SOME _ => err "ref type"
		| NONE => unexp "ref type")
	| lp (S.LBrace :: ts) = rcd ts
	| lp _ = raise Fail "parse error: error parsing type"
      and lp2 ts k what =
        (case lp ts 
           of SOME (ty1, ts1) =>
                (case lp ts1
                   of SOME (ty2, S.RParen::ts2) => SOME (k (ty1, ty2), ts2)
		    | SOME _ => err what 
                    | NONE => unexp (what^"2"))
               | NONE => unexp (what^"1"))
      and rcd ts =
       let
         fun rlp (S.Identifier label :: S.Colon :: ts) =
	      (case lp ts
	         of SOME (ty, S.Comma::ts') => cons1 (label,ty) (rlp ts')
		  | SOME (ty, S.RBrace::ts') => ([(label,ty)],ts')
		  | SOME _ => err "record type (2)"
		  | NONE => unexp "record type")
            | rlp _ = err "record type (1)"
       in
         case rlp ts
           of ([], _) => raise Fail "parse error: empty record type"
            | (items, ts') =>
              (**** edited to check for dup labels *)
                if Utils.dups (List.map #1 items)
                then raise Fail "parse error: duplicate labels in record type"
                else SOME (Ty.Record items, ts')
        end
    in
      lp tokens
    end

  fun nextTerm tokens =
    let
      fun lp [] = NONE
        | lp (S.Identifier x :: ts) = SOME (A.Var x, ts)
        | lp (S.LBrace :: S.Identifier x :: S.LeftArrow :: ts) =
            (case nextTerm ts
	       of SOME (t1, S.In::ts1) =>
		    (case nextTerm ts1
		       of SOME (t2, S.RBrace::ts2) => SOME (A.Let(x,t1,t2),ts2)
			| SOME _ => err "let (2)"
			| NONE => unexp "let (2)")
		| SOME _ => err "let (1)"
		| NONE => unexp "let (1)")
	| lp (S.LBrace :: S.Identifier x :: S.Colon :: ts) =
            (case nextTy ts
	       of SOME (ty1, S.Dot::ts1) =>
		    (case nextTerm ts1
		       of SOME (t2, S.RBrace::ts2) => SOME (A.Abs(x,ty1,t2),ts2)
			| SOME _ => err "abs (2)"
			| NONE => unexp "abs (2)")
		| SOME _ => err "abs (1)"
		| NONE => unexp "abs (1)")
	| lp (S.LParen :: S.RParen :: ts) = SOME (A.Unit, ts)
	| lp (S.LParen :: ts) =
	    (case untilParen ts
	       of ([t1,t2],ts') => SOME (A.App (t1, t2), ts')
	        | _ => err "app")
	| lp (S.T :: ts) = SOME (A.True, ts)
	| lp (S.F :: ts) = SOME (A.False, ts)
	| lp (S.LBrace :: S.Not :: ts) =
	    (case untilBrace ts
 	       of ([t1], ts') => SOME (A.Not t1, ts')
	        | _ => err "not")
	| lp (S.LBrace :: S.If :: ts) =
	    (case untilBrace ts
	       of ([t1, t2, t3], ts') => SOME (A.If (t1, t2, t3), ts')
	        | _ => err "if")
	| lp (S.LBrace :: S.Ref :: ts) =
	    (case untilBrace ts
	       of ([t1], ts') => SOME (A.Alloc t1, ts')
	        | _ => err "ref")
	| lp (S.LBrace :: S.Bang :: ts) =
	    (case untilBrace ts
	       of ([t1], ts') => SOME (A.Read t1, ts')
	        | _ => err "!")
	| lp (S.LBrace :: S.ColonEquals :: ts) =
	    (case untilBrace ts
	       of ([t1,t2],ts') => SOME (A.Write(t1,t2), ts')
	        | _ => err ":=")
    | lp (S.LBrace :: S.Identifier label :: S.Equals :: ts) =
                (case rcd (label, ts)
                  of (items, ts') =>
                     (****** edited to check for duplicate labels *)
                        if Utils.dups (List.map #1 items)
                        then raise Fail "parse error: duplicate labels in record"
                        else SOME (A.Record items, ts'))
	| lp (S.LBrace :: S.Hash :: S.Identifier label :: ts) =
            (case untilBrace ts
	       of ([t1],ts') => SOME (A.Select (label, t1), ts')
	        | _ => err "record selection")
	| lp (S.LBrace :: S.Dollar :: ts) =
	    (case untilBrace ts
	       of ([], _) => err "sequence empty?"  
		| (terms, ts') => SOME (A.Sequence terms, ts'))
        | lp ts =
            let
	      val s = String.concatWith " " (List.map S.tos (Utils.upto 10 ts))
	    in
	      raise Fail ("parse error at " ^ s)
	    end
      and rcd (label1, ts) =
        (case lp ts
	   of SOME (t1, S.Comma :: S.Identifier label2 :: S.Equals :: ts') =>
	        cons1 (label1,t1) (rcd (label2, ts'))
	    | SOME (t1, S.RBrace :: ts') => ((label1,t1)::[],ts')
	    | SOME _ => err "record"
	    | NONE => unexp "record")
      and untilBrace (tokens : S.token list) : A.term list * S.token list =
        (case lp tokens
           of SOME (term, S.RBrace :: tokens') => ([term], tokens')
 	    | SOME (term, tokens') => cons1 term (untilBrace tokens')
 	    | NONE => raise Fail "parse error: unexpected end of program")
      and untilParen (tokens : S.token list) : A.term list * S.token list =
        (case lp tokens
           of SOME (term, S.RParen :: tokens') => ([term], tokens')
 	    | SOME (term, tokens') => cons1 term (untilParen tokens')
 	    | NONE => raise Fail "parse error: unexpected end of program")
    in
      lp tokens    
    end

  fun parse tokens =
    (case nextTerm tokens
       of SOME (t, []) => t
        | SOME _ => raise Fail "extra tokens"
	| NONE => raise Fail "empty program")

  fun parseTy tokens =
    (case nextTy tokens
       of SOME (ty, _) => ty
        |  _  => raise Fail "fail")
	
end
