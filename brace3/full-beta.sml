structure FullBeta : REDUCTION_SYSTEM = struct

  structure U = ULC
  structure S = Subst

  fun step (U.Var _) = NONE
    | step (U.Abs (x, t1)) =
       (case step t1
          of SOME t1' => SOME (U.Abs (x, t1'))
	   | NONE => NONE)
    | step (U.App (U.Abs (x, t1), t2)) = SOME (S.subst (x, t2, t1))
    | step (U.App (t1, t2)) =    
       (case step t1
          of SOME t1' => SOME (U.App (t1', t2))
	   | NONE => (case step t2
	                of SOME t2' => SOME (U.App (t1, t2'))
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
