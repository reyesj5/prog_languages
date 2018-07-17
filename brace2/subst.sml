structure Subst : sig

  val subst    : string * ULC.term * ULC.term -> ULC.term
  val freeVars : ULC.term -> string list
			       
end = struct

  structure U = ULC

(* Note: variables, in this compiler, are just strings.
 * You do not need to implement a fast set.
 * Quadratic-time and/or quadratic-space operations are acceptable.
 * In a production setting, they wouldn't be.
 *)		    
  structure VarSet : sig
    type set
    val empty     : set
    val isEmpty   : set -> bool
    val size      : set -> int
    val singleton : string -> set
    val member    : string * set -> bool
    val union     : set * set -> set
    val remove    : set * string -> set
    val toList    : set -> string list
  end = struct
    type set = string list
    val empty = []
    fun isEmpty s         = List.null s
    fun size s            = List.length s
    fun singleton str     = str :: nil
    fun member (str1, str2 :: s) = if (str1 = str2) then true else member (str1, s)
      | member (str, nil) = false
    fun union (s1, s2)    = s1@s2
    fun remove ([], str)  = []
      | remove (str1 :: s, str2) = if (str1 = str2) 
                                    then remove(s, str2) 
                                    else str1 :: remove(s, str1) 
    fun toList s    = s
  end

  structure V = VarSet

  val counter = ref 0
  fun freshVarName () =
    let
      val n = "@" ^ Int.toString (!counter)
      val _ = (counter := (1 + !counter))
    in
      n
    end

(* Follow the definition of FV as in the text and the README. *)
  fun FV (t : U.term) : V.set = 
    (case t
      of (ULC.Var x) => V.singleton x
       | (ULC.Abs (x, t)) => if V.member (x, FV t) 
                              then V.remove (FV t, x)
                              else FV t
       | (ULC.App (t1, t2)) => V.union (FV t1, FV t2))

  val freeVars = V.toList o FV
	
  fun subst (x, s, t) = 
    (case (s, t)
      of (s, (ULC.Var y)) => 
        if (x = y) then s else (ULC.Var y)
       | (s, (ULC.App (t1, t2))) => (ULC.App (subst (x, s, t1), subst (x, s, t2)))
       | (s, (ULC.Abs (y, t1))) =>  
        if (x = y) then (ULC.Abs (y, t)) else 
          if V.member (y, FV s) 
          then subst (x, s, subst (y, ULC.Var (freshVarName ()), (ULC.Abs (y, t1))))
          else (ULC.Abs (y, subst (x, s, t1))))
	    
end
