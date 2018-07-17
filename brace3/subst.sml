structure Subst : sig

  val subst    : string * ULC.term * ULC.term -> ULC.term
  val freeVars : ULC.term -> string list
  val freshVarName : unit -> string
			       
end = struct

  structure U = ULC

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
    fun isEmpty s = null s
    fun size s = List.length s
    fun singleton x = [x]
    fun member (_, []) = false
      | member (x:string, s::ss) = x=s orelse member (x, ss)
    fun union ([], s2) = s2
      | union (s::s1, s2) = if member(s,s2) then union(s1,s2) else s::union(s1,s2)
    fun remove ([], _) = []
      | remove (s::ss, x:string) = if x=s then ss else s::(remove(ss,x))
    fun toList s = s
  end

  val counter = ref 0
  fun freshVarName () =
    let
      val n = "@" ^ Int.toString (!counter)
      val _ = (counter := (1 + !counter))
    in
      n
    end

  fun FV (t : U.term) : VarSet.set =
    let
      fun lp (U.Var x) = VarSet.singleton x
        | lp (U.Abs (x, t1)) = VarSet.remove (lp t1, x)
	| lp (U.App (t1, t2)) = VarSet.union (lp t1, lp t2)
    in
      lp t
    end

  val freeVars = VarSet.toList o FV
	
  fun subst (x, s, t) =
    let
      fun lp (U.Var y) =
            if x=y then s else U.Var y
          | lp (abs as U.Abs (y, t1)) =
      	    if x=y then abs
      	    else if VarSet.member (y, freeVars s) then lp (freshen (y, t1))
      	    else U.Abs (y, lp t1)
      	  | lp (U.App (t1, t2)) = U.App (lp t1, lp t2)
     in
       lp t
     end
     and freshen (y, t1) =
     let
       val newName = freshVarName ()
     in
       U.Abs (newName, subst (y, U.Var newName, t1))
     end
	    
end
