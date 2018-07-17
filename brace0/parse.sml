structure Parse : sig

  val parse : Scan.token list -> AST.term

end = struct

  structure S = Scan
  structure A = AST

  fun nextTerm (tokens : S.token list) : (A.term * S.token list) option = 
  	let 
  		fun lp [] = NONE
  		  | lp (S.T :: toks) = SOME (A.True, toks)
  		  | lp (S.F :: toks) = SOME (A.False, toks)
  		  | lp (S.Zero :: toks) = SOME (A.Zero, toks)



  		  | lp (S.LBrace :: S.If :: toks) =
  		  	(case lp toks 
  		  		of SOME (t1, S.Then :: toks) => SOME (A.If (t1,
  		  			(case lp toks
  		  				of SOME (t2, S.Else :: toks2) => t2
  			  			 | SOME _ => raise Fail ("Parse error: toks expected } in Else term")
  		  				 | NONE => raise Fail ("Parse error: Else ended unexpectedly")),
  		  			(case lp toks
  		  				of SOME (t2, S.Else :: toks2) => 
  		  					(case lp toks2
		  		  				of SOME (t3, S.RBrace :: toks3) => t3 	
		  			  			 | SOME _ => raise Fail ("Parse error: toks expected } in If term")
		  		  				 | NONE => raise Fail ("Parse error: If ended unexpectedly"))
  			  			 | SOME _ => raise Fail ("Parse error: toks expected } in Else term")
  		  				 | NONE => raise Fail ("Parse error: Else ended unexpectedly"))
  		  			), (case lp toks
  		  					of SOME (t2, S.Else :: toks2) => 
  		  						(case lp toks2
		  		  				of SOME (t3, S.RBrace :: toks3) => toks3 	
		  			  			 | SOME _ => raise Fail ("Parse error: toks expected } in If term")
		  		  				 | NONE => raise Fail ("Parse error: If ended unexpectedly"))
  			  			 | SOME _ => raise Fail ("Parse error: toks expected } in Else term")
  		  				 | NONE => raise Fail ("Parse error: Else ended unexpectedly"))
  		  			)
  		  		 | SOME _ => raise Fail ("Parse error: If expected more terms")
  		  		 | NONE => raise Fail ("Parse error: If ended unexpectedly"))
  		  
  		  | lp (S.LBrace :: S.PlusOne :: toks) = 
  		  	(case lp toks
  		  		of SOME (t1, S.RBrace :: toks) => SOME (A.Succ t1, toks)
  		  		 | SOME _ => raise Fail ("Parse error: toks expected } in Succ term")
  		  		 | NONE => raise Fail ("Parse error: Succ ended unexpectedly"))
  		  
  		  | lp (S.LBrace :: S.MinusOne :: toks) = 
  		  	(case lp toks
  		  		of SOME (t1, S.RBrace :: toks) => SOME (A.Pred t1, toks)
  		  		 | SOME _ => raise Fail ("Parse error: toks expected } in Pred term")
  		  		 | NONE => raise Fail ("Parse error: Pred ended unexpectedly"))
  		  
  		  | lp (S.LBrace :: S.IsZ :: toks) = 
  		  	(case lp toks
  		  		of SOME (t1, S.RBrace :: toks) => SOME (A.IsZero t1, toks)
  		  		 | SOME _ => raise Fail ("Parse error: toks expected } in IsZero term")
  		  		 | NONE => raise Fail ("Parse error: isz ended unexpectedly"))
  		  
  		  | lp (S.LBrace :: S.DoubleAmpersand :: toks) =
  		  	(case lp toks 
  		  		of SOME (t1, toks) => SOME (A.And (t1,
  		  			(case lp toks
  		  				of SOME (t2, S.RBrace :: toks2) => t2
  			  			 | SOME _ => raise Fail ("Parse error: toks expected } in DoubleAmpersand term")
  		  				 | NONE => raise Fail ("Parse error: && ended unexpectedly"))
  		  			), (case lp toks
  		  				of SOME (t2, S.RBrace :: toks2) => toks2
  			  			 | SOME _ => raise Fail ("Parse error: toks expected } in DoubleAmpersand term")
  		  				 | NONE => raise Fail ("Parse error: && ended unexpectedly"))
  		  			)
  		  		 | NONE => raise Fail ("Parse error: && ended unexpectedly"))

  		  | lp (S.LBrace :: S.DoublePipe :: toks) =
  		  	(case lp toks 
  		  		of SOME (t1, toks) => SOME (A.Or (t1,
  		  			(case lp toks
  		  				of SOME (t2, S.RBrace :: toks2) => t2
  			  			 | SOME _ => raise Fail ("Parse error: toks expected } in bang term")
  		  				 | NONE => raise Fail ("Parse error: ! ended unexpectedly"))
  		  			), (case lp toks
  		  				of SOME (t2, S.RBrace :: toks2) => toks2
  			  			 | SOME _ => raise Fail ("Parse error: toks expected } in bang term")
  		  				 | NONE => raise Fail ("Parse error: ! ended unexpectedly"))
  		  			)
  		  		 | NONE => raise Fail ("Parse error: || ended unexpectedly"))

  		  | lp (S.LBrace :: S.Bang :: toks) = 
  		  	(case lp toks
  		  		of SOME (t1, S.RBrace :: toks) => SOME (A.Not t1, toks)
  		  		 | SOME _ => raise Fail ("Parse error: toks expected } in bang term")
  		  		 | NONE => raise Fail ("Parse error: ! ended unexpectedly"))

  		  | lp (S.LBrace :: S.DoubleEqual :: toks) =
  		  	(case lp toks 
  		  		of SOME (t1, toks) => SOME (A.Equal (t1,
  		  			(case lp toks
  		  				of SOME (t2, S.RBrace :: toks2) => t2
  			  			 | SOME _ => raise Fail ("Parse error: toks expected } in DoubleEqual term")
  		  				 | NONE => raise Fail ("Parse error: == ended unexpectedly"))
  		  			), (case lp toks
  		  				of SOME (t2, S.RBrace :: toks2) => toks2
  			  			 | SOME _ => raise Fail ("Parse error: toks expected } in DoubleEqual term")
  		  				 | NONE => raise Fail ("Parse error: == ended unexpectedly"))
  		  			)
  		  		 | NONE => raise Fail ("Parse error: == ended unexpectedly"))

  		  | lp toks = raise Fail ("Parse error: cannot parse") 
  	in 
  		lp tokens
  	end

  fun parse tokens = 
    (* parsing the whole token sequence should result in exactly one term *)
    ( case (nextTerm tokens) 
    	of SOME (t, []) => t
    	 | SOME _ => raise Fail ("Parse error: Invalid input, found extra tokens after expression")
    	 | NONE => raise Fail ("Parse error: cannot parse empty list"))
			   
end