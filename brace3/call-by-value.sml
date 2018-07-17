structure CallByValue : REDUCTION_SYSTEM = struct

  structure U = ULC
  structure S = Subst
  
  fun step (U.Var _) = NONE
    | step (U.Abs _) = NONE
    | step (U.App (U.Abs (x, t1), v2 as U.Abs _)) =
        SOME (S.subst (x, v2, t1))
    | step (U.App (t1, t2)) =
       (case step t1
          of SOME t1' => SOME (U.App (t1', t2))
	   | NONE => if U.isValue t1
	             then (case step t2
		             of SOME t2' => SOME (U.App (t1, t2'))
			      | NONE => NONE)
	             else NONE)
			      
  fun reduce t =
   (case step t
      of NONE => t
       | SOME t' => reduce t')

  fun steps t =
   (case step t
      of NONE => [t]
       | SOME t' => t :: steps t')

end
