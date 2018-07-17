structure ULC : sig

(* ULC stands for Untyped Lambda Calculus *)
              
  datatype term
    = Var of string
    | Abs of string * term
    | App of term * term

  val isValue      : term -> bool
  val isStuck      : term -> bool
  val isNormalForm : term -> bool
                                 
  val tos : term -> string
  
end = struct

  datatype term
    = Var of string
    | Abs of string * term
    | App of term * term

  fun isValue (Var x) = false
    | isValue (Abs (x, t)) = true
    | isValue (App (t1, t2)) = false

  fun isStuck (Var x) = true
    | isStuck (Abs (x, t)) = false
    | isStuck (App (t1, t2)) = isStuck t1 orelse isStuck t2

  fun isNormalForm t = isValue t orelse isStuck t
                      
  fun tos t =
    let
      fun lp (Var x) = x
        | lp (Abs (x, t1)) = "{" ^ x ^ " " ^ lp t1 ^ "}"
        | lp (App (t1, t2)) = "(" ^ lp t1 ^ " " ^ lp t2 ^ ")"
    in
      lp t
    end

end
