structure AST : sig

  datatype term
    = Var of string
    | Abs of string * term
    | App of term * term
    | Let of string * term * term

end = struct

  datatype term
    = Var of string
    | Abs of string * term
    | App of term * term
    | Let of string * term * term

  fun unparse t =
    let
      fun lp (Var x) = x
        | lp (Abs (x, t1)) = "{" ^ x ^ " " ^ lp t1 ^ "}"
        | lp (App (t1, t2)) = "(" ^ lp t1 ^ " " ^ lp t2 ^ ")"
        | lp (Let (x, t1, t2)) =
          "{" ^ x ^ " <- " ^ lp t1 ^ " in " ^ lp t2 ^ "}"
    in
      lp t
    end
  
end
