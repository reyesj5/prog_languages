structure Eval : sig

  val step  : AST.term -> AST.term option
  val subst : string * AST.term * AST.term -> AST.term
  val eval  : AST.term -> AST.term

  val isNormalForm : AST.term -> bool
  val isStuck      : AST.term -> bool

end = struct

  structure A = AST

(* read subst as "rewrite x to v in t" *)
  fun subst (x, v, t) = 
    (case t
      of A.True => A.True
       | A.False => A.False
       | A.Zero => A.Zero
       | A.Not t => A.Not (subst (x, v, t))
       | A.Succ t => A.Succ (subst (x, v, t))
       | A.Pred t => A.Pred (subst (x, v, t))
       | A.Pair (t1, t2) => A.Pair (subst (x, v, t1), subst (x, v, t1))
       | A.Unit => A.Unit
       | A.Some t => A.Some (subst (x, v, t))
       | A.None ty => A.None ty
       | A.If (t1, t2, t3) => A.If ((subst(x, v, t1)), (subst(x, v, t2)), (subst(x, v, t3)))
       | A.IsZero t => A.IsZero (subst (x, v, t))
       | A.And (t1, t2) => A.And ((subst (x, v, t1)),(subst (x, v, t2)))
       | A.Or (t1, t2) => A.Or ((subst (x, v, t1)),(subst (x, v, t2)))
       | A.Equal (t1, t2) => A.Or ((subst (x, v, t1)),(subst (x, v, t2)))
       | A.Select1 t => A.Select1 (subst (x, v, t))
       | A.Select2 t => A.Select2 (subst (x, v, t))
       | A.OptCase (t1, (s, t2), t3) => A.OptCase ((subst (x, v, t1)), (s, (subst (x, v, t2))), (subst (x, v, t3)))
       | A.Let (y, t1, t2) => if x = y 
                                then (subst (x, (subst (x, v, t1)), t2))
                                else A.Let (y, (subst (x, v, t1)), (subst (x, v, t2)))
       | A.Var y => if x = y then v else A.Var y)

(* please note that most of step can be copied from hw2 *)
(* step should not raise an error on stuck terms, it should just return NONE *)	  
  fun step (A.If (A.True, t2, t3)) = SOME t2
    | step (A.If (A.False, t2, t3)) = SOME t3
    | step (A.If (t1, t2, t3)) = 
      (case step t1
        of SOME t1' => SOME (A.If (t1', t2, t3))
         | NONE => NONE)
    | step (A.Succ t1) = 
      (case step t1
        of SOME t1' => SOME (A.Succ t1')
         | NONE => NONE)
    | step (A.Pred A.Zero) = SOME A.Zero
    | step (A.Pred (A.Succ t1)) = 
      if A.isNumericValue t1 then SOME t1 else NONE
    | step (A.Pred t1) =
      (case step t1
        of SOME t1' => SOME (A.Pred t1')
         | NONE => NONE)
    | step (A.IsZero A.Zero) = SOME A.True
    | step (A.IsZero (A.Succ t1)) = 
      if A.isNumericValue t1 then SOME A.False else NONE
    | step (A.IsZero t1) =
      (case step t1
        of SOME t1' => SOME (A.IsZero t1')
         | NONE => NONE)
    | step (A.And (A.False, t2)) = SOME A.False
    | step (A.And (A.True, t2)) = SOME t2
    | step (A.And (t1, t2)) =
      (case step t1
        of SOME t1' => SOME (A.And (t1', t2))
         | NONE => NONE)
    | step (A.Or (A.True, t2)) = SOME A.True
    | step (A.Or (A.False, t2)) = SOME t2
    | step (A.Or (t1, t2)) =
      (case step t1
        of SOME t1' => SOME (A.Or (t1', t2))
         | NONE => NONE)
    | step (A.Not A.True) = SOME A.False
    | step (A.Not A.False) = SOME A.True
    | step (A.Not t1) =
      (case step t1
        of SOME t1' => SOME (A.Not t1')
         | NONE => NONE)
    | step (A.Equal (A.True, A.True)) = SOME A.True
    | step (A.Equal (A.True, A.False)) = SOME A.False
    | step (A.Equal (A.False, A.True)) = SOME A.False
    | step (A.Equal (A.False, A.False)) = SOME A.True
    | step (A.Equal (A.Zero, A.Zero)) = SOME A.True
    | step (A.Equal (A.Unit, A.Unit)) = SOME A.True
    | step (A.Equal (A.Pair (t1, t2), A.Pair (t3, t4))) =
        SOME (A.And (A.Equal (t1, t3), A.Equal (t2, t4)))
    | step (A.Equal (A.Some t1, A.Some t2)) = SOME (A.Equal (t1, t2))
    | step (A.Equal (A.None _, A.None _)) = SOME A.True
    | step (A.Equal (A.Succ t1, A.Zero)) =
      if A.isNumericValue t1 then SOME A.False else NONE
    | step (A.Equal (A.Zero, A.Succ t1)) =
      if A.isNumericValue t1 then SOME A.False else NONE
    | step (A.Equal (A.Succ t1, A.Succ t2)) =
      (case (A.isNumericValue t1, A.isNumericValue t2)
        of (true, true) => SOME (A.Equal (t1, t2))
         | _ => NONE)
    | step (A.Equal (t1, t2)) =
      if A.isValue t1
      then 
        (case step t2
            of SOME t2' => SOME (A.Equal (t1, t2'))
             | NONE => NONE)
      else
        (case step t1
            of SOME t1' => SOME (A.Equal (t1', t2))
             | NONE => NONE)
    | step (A.Let (s, t1, t2)) = 
      if A.isValue t1 
      then 
        SOME (subst (s, t1, t2)) 
      else 
        (case step t1
        of SOME t1' => SOME (A.Let (s, t1', t2))
         | NONE => NONE)
    | step (A.Select1 (A.Pair (t1, t2))) = 
      if A.isValue t1 andalso A.isValue t2 then SOME t1 else NONE
    | step (A.Select1 t1) = 
      (case step t1
        of SOME t1' => SOME (A.Select1 t1')
         | NONE => NONE)
    | step (A.Select2 (A.Pair (t1, t2))) = 
      if A.isValue t1 andalso A.isValue t2 then SOME t2 else NONE
    | step (A.Select2 t1) = 
      (case step t1
        of SOME t1' => SOME (A.Select2 t1')
         | NONE => NONE)
    | step (A.Pair (t1, t2)) = 
      if A.isValue t1
      then 
        (case step t2
            of SOME t2' => SOME (A.Pair (t1, t2'))
             | NONE => NONE)
      else
        (case step t1
            of SOME t1' => SOME (A.Pair (t1', t2))
             | NONE => NONE)
    | step (A.Some t1) =
      (case step t1
        of SOME t1' => SOME (A.Some t1')
         | NONE => NONE)
    | step (A.OptCase (t1, (s, t2), t3)) = 
      (case t1
        of (A.Some t1') => if A.isValue (A.Some t1') then SOME (subst (s, t1', t2)) else NONE
         | (A.None _) => SOME (t3)
         | _ => (case step t1
                  of SOME t1' => SOME (A.OptCase (t1', (s, t2), t3))
                   | NONE => NONE))
    | step _ = NONE

  fun eval t =
   (case step t
      of NONE => t
       | SOME t' => eval t')

(* note: isNormalForm and isStuck have one-line implementations *)

  fun isNormalForm t = A.equal(eval t, t)
           
  fun isStuck t = isNormalForm t andalso not (A.isValue t)

end
