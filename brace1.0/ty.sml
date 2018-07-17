structure Ty : sig

  datatype ty 
    = Nat
    | Bool
    | Unit
    | Pair of ty * ty
    | Opt of ty

  val equal   : ty * ty -> bool
  val unparse : ty -> string
		      
end = struct

  datatype ty 
    = Nat
    | Bool
    | Unit
    | Pair of ty * ty
    | Opt of ty

  fun equal (ty1:ty, ty2) = ty1=ty2

  fun unparse Nat = "Nat"
    | unparse Bool = "Bool"
    | unparse Unit = "Unit"
    | unparse (Pair (ty1, ty2)) = "(* " ^ unparse ty1 ^ " " ^ unparse ty2 ^ ")"
    | unparse (Opt ty1) = "(Opt " ^ unparse ty1 ^ ")"
 
end
