structure MULC : sig

(* MULC stands for Many-argument Untyped Lambda Calculus *)
              
  datatype term
    = Var of string
    | Abs of string list * term
    | App of term list

  val tos : term -> string
			
end = struct

  datatype term
    = Var of string
    | Abs of string list * term
    | App of term list
			                         
  fun tos t =
    let
      val spc = String.concatWith " "
      fun lp (Var x) = x
        | lp (Abs (xs, t1)) = "{" ^ spc (xs @ [".",lp t1]) ^ "}"
        | lp (App ts) = "(" ^ spc (List.map lp ts) ^ ")"
    in
      lp t
    end

end
