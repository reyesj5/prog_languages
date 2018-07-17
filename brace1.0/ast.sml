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
    | Unit	    
    | Var of string
    | Let of string * term * term
    | Pair of term * term
    | Select1 of term
    | Select2 of term
    | Some of term
    | None of Ty.ty
    | OptCase of term * (string * term) * term

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
    | Unit	    
    | Var of string
    | Let of string * term * term
    | Pair of term * term
    | Select1 of term
    | Select2 of term
    | Some of term
    | None of Ty.ty
    | OptCase of term * (string * term) * term

  fun unparse t =
    let
      fun embrace s = "{" ^ s ^ "}"
      fun br ss = embrace (String.concatWith " " ss)
      fun lp True = "T"
        | lp False = "F"
    	  | lp Zero = "0"
    	  | lp (If (t1, t2, t3)) = br ["if",lp t1,"then",lp t2,"else",lp t3]
    	  | lp (Succ t1) = unary "+1" t1
    	  | lp (Pred t1) = unary "-1" t1
    	  | lp (IsZero t1) = unary "isz" t1
    	  | lp (And (t1, t2)) = binary "&&" (t1, t2)
    	  | lp (Or (t1, t2)) = binary "||" (t1, t2)
    	  | lp (Not t1) = unary "!" t1
    	  | lp (Equal (t1, t2)) = binary "==" (t1, t2)
        | lp Unit = "()"
        | lp (Var x) = x
        | lp (Let (x, t1, t2)) = br [x,"<-",lp t1,"in",lp t2]
        | lp (Pair (t1, t2)) = binary "#" (t1, t2)
        | lp (Select1 t1) = unary "#1" t1
        | lp (Select2 t1) = unary "#2" t1
        | lp (Some t1) = unary "some" t1
	      | lp (None ty) = "{none " ^ Ty.unparse ty ^ "}"
	      | lp (OptCase (t1, (x, t2), t3)) =
	         br ["case",lp t1,"of","some",x,"->",lp t2,"|","none","->",lp t3]
           and unary  oper t1 = br [oper, lp t1]
           and binary oper (t1, t2) = br [oper, lp t1, lp t2]
    in
      lp t
    end

  fun equal (t1:term, t2) = t1=t2

  fun isNumericValue Zero = true
    | isNumericValue (Succ t1) = isNumericValue t1
    | isNumericValue (Pred t1) = isNumericValue t1
    | isNumericValue _ = false

  fun isSingleValue True = true
    | isSingleValue False = true
    | isSingleValue Unit = true
    | isSingleValue t = isNumericValue t

  fun isValue True = true
    | isValue False = true
    | isValue Unit = true
    | isValue (None ty) = true
    | isValue (Some t1) = isSingleValue t1
    | isValue (Pair (t1, t2)) = isSingleValue t1 andalso isSingleValue t2
    | isValue t = isNumericValue t

end
