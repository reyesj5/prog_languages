structure AST : sig

  datatype term
    = Var of string
    | Abs of string list * term
    | App of term list
    | Let of string * term * term
    | LetRec of string * term * term
    | True
    | False
    | If of term * term * term
    | Not of term
    | And of term * term
    | Or of term * term
    | Nat of int
    | Add of term * term
    | Mult of term * term
    | Pair of term * term
    | Select1 of term
    | Select2 of term 
    | ID of string
			
  val unparse : term -> string

end = struct

  datatype term
    = Var of string
    | Abs of string list * term
    | App of term list
    | Let of string * term * term
    | LetRec of string * term * term
    | True
    | False
    | If of term * term * term
    | Not of term
    | And of term * term
    | Or of term * term
    | Nat of int
    | Add of term * term
    | Mult of term * term
    | Pair of term * term
    | Select1 of term
    | Select2 of term
    | ID of string
	      
  fun unparse t =
    let
      val spc = String.concatWith " "
      fun par s = "(" ^ s ^ ")"
      fun brace s = "{" ^ s ^ "}"
      fun lp (Var x) = x
        | lp (Abs (xs, t1)) = brace (spc xs ^ " . " ^ lp t1)
        | lp (App ts) = (par o spc o List.map lp) ts
        | lp (Let (x, t1, t2)) = brace (x ^ " <- " ^ lp t1 ^ " in " ^ lp t2)
	| lp (LetRec (x, t1, t2)) = brace (x ^ " <= " ^ lp t1 ^ " in " ^ lp t2)
	| lp True = "T"
	| lp False = "F"
	| lp (If (t1, t2, t3)) = brace (spc ["if",lp t1,"then",lp t2,"else",lp t3])
	| lp (Not t1) = unop "!" t1
	| lp (And (t1, t2)) = binop "&&" (t1, t2)
	| lp (Or (t1, t2)) = binop "||" (t1, t2)
	| lp (Nat n) = Int.toString n
	| lp (Add (t1, t2)) = binop "+" (t1, t2)
	| lp (Mult (t1, t2)) = binop "*" (t1, t2)
	| lp (Pair (t1, t2)) = binop "#" (t1, t2)
	| lp (Select1 t1) = unop "#1" t1
	| lp (Select2 t1) = unop "#2" t1
	| lp (ID x) = "/" ^ x
      and binop oper (t1, t2) = (brace o spc) [oper, lp t1, lp t2]
      and unop oper t1 = (brace o spc) [oper, lp t1]
    in
      lp t
    end
  
end
