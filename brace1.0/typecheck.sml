structure TypeCheck : sig

  val typeof : AST.term -> Ty.ty
  val check  : AST.term -> unit

end = struct

  structure A = AST

(* A module within a module! *)
(* Typing environments track which variable has what type. *)
  structure TypeEnv : sig
    type env   = (string * Ty.ty) list
    val empty  : env
    val lookup : string * env -> Ty.ty option
    val extend : env * string * Ty.ty -> env
  end = struct
    type env   = (string * Ty.ty) list
    val empty = []
    fun lookup (key, []) = NONE
      | lookup (key, ((x,ty)::rest)) = if key=x then SOME ty else lookup (key, rest);
    fun extend (g, var, ty) = [(var, ty)]@g
  end

  structure E = TypeEnv

(* A convenience: infix notation for extending typing environments. *)	    
  infix <+>
  fun gamma <+> (x,ty) = E.extend (gamma, x, ty)

  fun typeof t =
    let
      (* lp is an internal function that has a typing environment argument as well as a term *)
      (* The typing environment is capital gamma in the formal presentation (and "g" here). *)
      fun lp (g, t) = 
        (case t
          of A.True => Ty.Bool
           | A.False => Ty.Bool
           | A.Zero => Ty.Nat
           | A.Unit => Ty.Unit
           | A.None ty => Ty.Opt ty
           | A.If (t1, t2, t3) =>
              (case (lp (g, t1), lp (g, t2), lp (g, t3))
                of (Ty.Bool, ty1, ty2) => if Ty.equal (ty1, ty2) 
                                          then ty1 
                                          else raise Fail ("Type error: Ill-typed If") 
                 | _ => raise Fail ("Type error: Ill-typed If"))
           | A.Succ t1 => 
              (case lp (g, t1)
                of Ty.Nat => Ty.Nat
                 | _ => raise Fail ("Type error: Ill-typed Succ"))
           | A.Pred t1 =>
              (case lp (g, t1)
                of Ty.Nat => Ty.Nat
                 | _ => raise Fail ("Type error: Ill-typed Pred"))
           | A.IsZero t1 => 
              (case lp (g, t1)
                of Ty.Nat => Ty.Bool
                 | _ => raise Fail ("Type error: Ill-typed IsZero"))
           | A.And (t1, t2) =>
              (case (lp (g, t1), lp (g, t2))
                of (Ty.Bool, Ty.Bool) => Ty.Bool
                 | _ => raise Fail ("Type error: Ill-typed And"))
           | A.Or (t1, t2) =>
              (case (lp (g, t1), lp (g, t2))
                of (Ty.Bool, Ty.Bool) => Ty.Bool
                 | _ => raise Fail ("Type error: Ill-typed Or"))
           | A.Not t1 => 
              (case lp (g, t1)
                of Ty.Bool => Ty.Bool
                 | _ => raise Fail ("Type error: Ill-typed Not"))
           | A.Equal (t1, t2) =>
              (case (lp (g, t1), lp (g, t2))
                of (ty1, ty2) => if Ty.equal (ty1, ty2) 
                                 then Ty.Bool 
                                 else raise Fail ("Type error: Ill-typed Equal"))
           | A.Var x => 
              (case E.lookup (x, g)
                of SOME ty => ty
                 | NONE => raise Fail ("Type error: Unbound variable"))
           | A.Pair (t1, t2) => 
              (case (lp (g, t1), lp (g, t2))
                of (ty1, ty2) => Ty.Pair (ty1, ty2))
           | A.Select1 (A.Pair (t1, t2)) =>
              (case lp (g, t1)
                of ty => ty)
           | A.Select2 (A.Pair (t1, t2)) =>
              (case lp (g, t2)
                of ty => ty)
           | A.Some t1 => 
              (case lp (g, t1)
                of ty => Ty.Opt ty)
           | A.Let (s, t1, t2) =>
              (case lp (g, t1)
                of ty => (case lp (g<+>(s,ty), t2)
                            of ty2 => ty2)) 
           | A.OptCase (t1, (s, t2), t3) =>
              (case lp (g, t1)
                of (Ty.Opt ty) => (case (lp (g<+>(s,ty), t2), lp (g<+>(s,ty), t3))
                                    of (ty2, ty3) => if Ty.equal (ty2, ty3) 
                                                     then ty2 
                                                     else raise Fail ("Type error: Ill-typed OptCase"))
                 | _ => raise Fail ("Type error: Ill-typed OptCase"))

           | _ => raise Fail "hello")
    in
      lp (E.empty, t)
    end

(* ignore is a build-in function of type 'a -> unit *)
  fun check t = ignore (typeof t)

end
