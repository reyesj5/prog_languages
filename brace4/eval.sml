structure Eval : sig

  val eval  : AST.term -> AST.term
  val steps : AST.term -> AST.term list
	  
end = struct

  structure A = AST
  structure S = Store

  fun step t =
    let
      fun lp (A.Var x) = NONE
        | lp (A.Abs _) = NONE
      	| lp (A.App (A.Abs (x, ty1, t2), v2)) =
            if A.isValue v2 
            then SOME (Subst.subst {replaceThis=x, withThis=v2, inThis=t2})
            else NONE
        | lp (A.App (t1, t2)) =
           (case step t1
              of SOME t1' => SOME (A.App (t1', t2))
               | NONE => if A.isValue t1
                         then (case step t2
                                of SOME t2' => SOME (A.App (t1, t2'))
                                 | NONE => NONE)
                         else NONE)
        | lp (A.Let (x, t1, t2)) = 
          if A.isValue t1 
          then 
            SOME (Subst.subst {replaceThis=x, withThis=t1, inThis=t2}) 
          else 
            (case step t1
            of SOME t1' => SOME (A.Let (x, t1', t2))
             | NONE => NONE)
        | lp A.Unit = NONE
      	| lp A.True = NONE
      	| lp A.False = NONE
      	| lp (A.Not t1) =
      	    (case t1
      	       of A.True  => SOME A.False
      	        | A.False => SOME A.True
      		      | _ => (case lp t1
      		              of SOME t1' => SOME (A.Not t1')
      			             | NONE => NONE))
        | lp (A.If (t1, t2, t3)) =
           (case t1
    	       of A.True => SOME t2
    	        | A.False => SOME t3
	            | _ => (case lp t1
	                    of SOME t1' => SOME (A.If (t1', t2, t3))
		                   | NONE => NONE))
        | lp (A.Alloc t1) = 
          if A.isValue t1 
          then SOME (A.Location (S.malloc t1))
          else 
            (case step t1
             of SOME t' => SOME (A.Alloc t')
              | NONE => NONE)
        | lp (A.Read t1) = 
          if A.isValue t1
          then
            (case t1
              of A.Location n => SOME (S.read n)
               | _ => NONE)
          else 
            (case step t1
             of SOME t' => SOME (A.Read t')
              | NONE => NONE)
        | lp (A.Write (t1, t2)) = 
          if A.isValue t1
          then 
            (case t1
              of A.Location n => 
                if A.isValue t2
                then 
                  let 
                    val _ = (S.write (n, t2))
                  in 
                    SOME A.Unit
                  end 
                else 
                  (case step t2
                   of SOME t' => SOME (A.Write (t1, t'))
                    | NONE => NONE)
               | _ => NONE)
          else 
            (case step t1
             of SOME t' => SOME (A.Write (t', t2))
              | NONE => NONE)
        | lp (A.Location _) = NONE
        | lp (A.Record items) = 
          let
            fun lp' [] = raise Fail "type error: sequence is empty"
              | lp' ((str, t) :: rest) = 
                (case step t
                  of SOME t' => SOME (A.Record ((str, t') :: rest))
                   | NONE => NONE)
          in
            lp' items
          end
      	| lp (A.Select (label, t1)) = 
          let
            fun lp' (lb, []) = NONE
              | lp' (lb, ((str, t) :: rest)) = if lb=str then SOME t else lp' (lb, rest)
          in
            (case t1
              of (A.Record types) => lp' (label, types)
               | _ => NONE)
          end
      	| lp (A.Sequence ts) = 
          let
            fun lp' [] = raise Fail "eval error: sequence is empty"
              | lp' (t :: []) =
                if A.isValue t
                then SOME t
                else 
                  (case step t
                   of SOME t' => SOME (A.Sequence (t' :: []))
                    | NONE => NONE)
              | lp' (A.Unit :: rest) = SOME (A.Sequence rest)
              | lp' (t :: rest) = 
                (case step t
                  of SOME t' => SOME (A.Sequence (t' :: rest))
                   | NONE => NONE)
          in
            lp' ts
          end
    in
      lp t
    end

  fun eval t =
    let
      val _ = Store.clear()
      fun lp t = 
        (case step t
           of SOME t' => lp t'
	    | NONE => t)
    in
      lp t
    end	

  fun steps t =
    let
      val _ = Store.clear()
      fun lp t =
        (case step t
           of SOME t' => t :: lp t'
            | NONE => [t])
    in
      lp t
    end

(* HW6 Crowdsourced Tests from Piazza @243 *)
	
end
