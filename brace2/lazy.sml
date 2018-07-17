structure Lazy : REDUCTION_SYSTEM = struct

  fun step (ULC.App (t1, t2)) = 
	  	(case step t1
		    of SOME t1' => SOME (ULC.App (t1', t2))
		     | NONE => 
		     	(case t1
					of (ULC.Abs (x, t)) => SOME (Subst.subst (x, t2, t))
					 | _ => NONE))
    | step _ = NONE
  
  fun reduce t = 
  	(case step t
      of NONE => t
       | SOME t' => reduce t')
  
  fun steps t = 
  	let
      fun lp t = 
       (case step t
          of NONE => []
           | SOME t' => t' :: lp t')
    in
      t :: lp t
    end  
  
end
