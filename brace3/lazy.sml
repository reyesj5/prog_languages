structure Lazy : REDUCTION_SYSTEM = struct

  structure U = ULC
  structure S = Subst
  
  fun step (U.Var _) = NONE
    | step (U.Abs _) = NONE
    | step (U.App (t1, t2)) =
       (case t1
          of U.Abs (x, b) => SOME (S.subst (x, t2, b))
	   | U.Var _ => NONE
	   | U.App _ => (case step t1
			   of SOME t1' => SOME (U.App (t1', t2))
	                    | NONE => NONE))
			      
  fun reduce t =
   (case step t
      of NONE => t
       | SOME t' => reduce t')

  fun steps t =
   (case step t
      of NONE => [t]
       | SOME t' => t :: steps t')

end
