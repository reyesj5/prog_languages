structure AST : sig

  datatype term
    = Var of string
    | Abs of string * Ty.ty * term
    | App of term * term
    | Let of string * term * term
    | Unit
    | True
    | False
    | Not of term
    | If of term * term * term
    | Alloc of term
    | Read of term
    | Write of term * term
    | Location of int
    | Record of (string * term) list
    | Select of string * term
    | Sequence of term list

  val tos     : term -> string
  val eq      : term * term -> bool

  val isValue : term -> bool
			    
end = struct

  datatype term
    = Var of string
    | Abs of string * Ty.ty * term
    | App of term * term
    | Let of string * term * term
    | Unit
    | True
    | False
    | Not of term
    | If of term * term * term
    | Alloc of term
    | Read of term
    | Write of term * term
    | Location of int
    | Record of (string * term) list
    | Select of string * term
    | Sequence of term list

  fun isValue (Abs (s, ty, t1)) = true
    | isValue Unit = true
    | isValue True = true
    | isValue False = true
    | isValue (Location n) = true
    | isValue (Record (ls)) =
      let 
        fun lp [] = true
          | lp ((str, t1)::rst) = isValue t1 andalso lp rst
      in 
        if List.null ls then raise Fail "value error: record is empty" else lp ls
      end
    | isValue _ = false
		       
  fun tos t =
    let
      val ty = Ty.tos
      fun brc s = "{" ^ s ^ "}"
      val spc = String.concatWith " "
      fun lp (Var x) = x
        | lp (Abs (x, tau, t1)) = brc (x^":"^(ty tau)^" . "^lp t1)
	| lp (App (t1, t2)) = "("^(lp t1)^" "^(lp t2)^")"
	| lp (Let (x, t1, t2)) =
	    brc (x^"<-"^(lp t1)^" in "^(lp t2))
	| lp Unit = "()"
	| lp True = "T"
	| lp False = "F"
	| lp (Not t1) = brc (spc ["not", lp t1])
	| lp (If (t1, t2, t3)) = brc (spc ["if", lp t1, lp t2, lp t3])
	| lp (Alloc t1) = brc (spc ["ref", lp t1])
	| lp (Read t1) = brc ("!" ^ lp t1)
	| lp (Write (t1, t2)) = brc (spc [":=", lp t1, lp t2])
        | lp (Location p) = "loc_" ^ Int.toString p
	| lp (Record items) =
	    let
	      fun item (label, tm) = label ^ "=" ^ lp tm
            in
	      brc (String.concatWith "," (List.map item items))
	    end
	| lp (Select (label, t1)) = brc ("#"^label^" "^(lp t1))
	| lp (Sequence ts) = brc (spc ("$"::List.map lp ts))
      in
        lp t
      end

  fun eq (t1:term, t2) = (t1=t2)
			     
end
