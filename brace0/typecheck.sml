structure TypeCheck : sig

  datatype ty 
    = Nat
    | Bool
  
  val typeof : AST.term -> ty
  val check  : AST.term -> unit

end = struct

  structure A = AST

  datatype ty = Nat | Bool

  fun typeof (t:A.term) : ty =
    (case t
      of A.True => Bool
       | A.False => Bool
       | A.Zero => Nat
       | A.If (t1, t2, t3) =>
          (case (typeof t1, typeof t2, typeof t3)
            of (Bool, Bool, Bool) => Bool
             | (Bool, Nat, Nat) => Nat
             | _ => raise Fail ("Type error: Ill-typed If"))
       | A.Succ t => 
          (case typeof t
            of Nat => Nat
             | Bool => raise Fail ("Type error: Ill-typed Succ"))
       | A.Pred t =>
          (case typeof t
            of Nat => Nat
             | Bool => raise Fail ("Type error: Ill-typed Pred"))
       | A.IsZero t => 
          (case typeof t
            of Nat => Bool
             | Bool => raise Fail ("Type error: Ill-typed IsZero"))
       | A.And (t1, t2) =>
          (case (typeof t1, typeof t2)
            of (Bool, Bool) => Bool
             | _ => raise Fail ("Type error: Ill-typed And"))
       | A.Or (t1, t2) =>
          (case (typeof t1, typeof t2)
            of (Bool, Bool) => Bool
             | _ => raise Fail ("Type error: Ill-typed Or"))
       | A.Not t => 
          (case typeof t
            of Bool => Bool
             | Nat => raise Fail ("Type error: Ill-typed Not"))
       | A.Equal (t1, t2) =>
          (case (typeof t1, typeof t2)
            of (Bool, Bool) => Bool
             | (Nat, Nat) => Bool
             | _ => raise Fail ("Type error: Ill-typed Equal")))

  fun check t = 
    let
      val _ = typeof t
    in
      ()
    end

end