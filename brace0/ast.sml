structure AST : sig

  datatype term
    = True
    | False
    | Zero
    | If of term * term * term
    | Succ of term
    | Pred of term
    | IsZero of term
    | And of term * term
    | Or of term * term
    | Not of term
    | Equal of term * term

  val unparse : term -> string
  val equal : term * term -> bool  
  
  val isValue : term -> bool
  val isNumericValue : term -> bool
  
end = struct

  datatype term
    = True
    | False
    | Zero
    | If of term * term * term
    | Succ of term
    | Pred of term
    | IsZero of term
    | And of term * term
    | Or of term * term
    | Not of term
    | Equal of term * term

  fun unparse True = "T"
    | unparse False = "F"
    | unparse Zero = "0"
    | unparse (If (t1, t2, t3)) = "{if " ^ unparse t1 ^ " then " ^ unparse t2 ^ " else " ^ unparse t3 ^ "}"
    | unparse (Succ t1) = "{+1 " ^ unparse t1 ^ "}"
    | unparse (Pred t1) = "{-1 " ^ unparse t1 ^ "}"
    | unparse (IsZero t1) = "{isz " ^ unparse t1 ^ "}"
    | unparse (And (t1, t2)) = "{&& " ^ unparse t1 ^ " " ^ unparse t2 ^ "}"
    | unparse (Or (t1, t2)) = "{|| " ^ unparse t1 ^ " " ^ unparse t2 ^ "}"
    | unparse (Not t1) = "{! " ^ unparse t1 ^ "}"
    | unparse (Equal (t1, t2)) = "{== " ^ unparse t1 ^ " " ^ unparse t2 ^ "}"

  fun equal (t1:term, t2) = t1 = t2

  fun isNumericValue Zero = true
    | isNumericValue (Succ t1:term) = isNumericValue t1
    | isNumericValue _ = false

  fun isValue True = true
    | isValue False = true 
    | isValue (t1:term) = isNumericValue t1
		
end