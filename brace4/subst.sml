structure Subst : sig

  val subst    : {replaceThis : string,
	          withThis : AST.term,
                  inThis: AST.term} -> AST.term

  val freeVars : AST.term -> string list
			       
end = struct

  structure A = AST

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

  fun FV (t : A.term) : VarSet.set =
    let
      val u = VarSet.union
      val empty = VarSet.empty
      fun lp (A.Var x) = VarSet.singleton x
        | lp (A.Abs (x, _, t1)) = VarSet.remove (lp t1, x)
	| lp (A.App (t1, t2)) = u (lp t1, lp t2)
	| lp (A.Let (x, t1, t2)) = u (lp t1, VarSet.remove (lp t2, x))
	| lp A.Unit = empty
	| lp A.True = empty
	| lp A.False = empty
	| lp (A.Not t1) = lp t1
	| lp (A.If (t1, t2, t3)) = u (lp t1, u (lp t2, lp t3))
	| lp (A.Alloc t1) = lp t1
	| lp (A.Read t1) = lp t1
	| lp (A.Write (t1, t2)) = u (lp t1, lp t2)
	| lp (A.Location _) = empty
	| lp (A.Record items) =
	    List.foldl (fn ((_,t),s) => u (lp t, s)) empty items
	| lp (A.Select (_, t)) = lp t
	| lp (A.Sequence terms) =
	    List.foldl (fn (t,s) => u (lp t, s)) empty terms
    in
      lp t
    end

  val freeVars = VarSet.toList o FV
	
  fun subst {replaceThis=x, withThis=s, inThis=t} =
    let
      fun lp (A.Var y) =
            if x=y then s else A.Var y
        | lp (abs as A.Abs (y, ty, t1)) =
	    if x=y then abs
	    else if VarSet.member (y, freeVars s)
	         then lp (freshen (y, ty, t1))
	         else A.Abs (y, ty, lp t1)
	| lp (A.App (t1, t2)) = A.App (lp t1, lp t2)
	| lp (A.Let (y, t1, t2)) =
	    if x=y
	    then A.Let (y, lp t1, t2)
	    else A.Let (y, lp t1, lp t2)
	| lp A.Unit = A.Unit
	| lp A.True = A.True
	| lp A.False = A.False
	| lp (A.Not t1) = A.Not (lp t1)
	| lp (A.If (t1, t2, t3)) = A.If (lp t1, lp t2, lp t3)
	| lp (A.Alloc t1) = A.Alloc (lp t1)
	| lp (A.Read t1) = A.Read (lp t1)
	| lp (A.Write (t1, t2)) = A.Write (lp t1, lp t2)
	| lp (A.Location loc) = A.Location loc
	| lp (A.Record items) =
	    A.Record (List.map (fn (s,ti) => (s,lp ti)) items)
	| lp (A.Select (label, t)) = A.Select (label, lp t)
	| lp (A.Sequence terms) = A.Sequence (List.map lp terms)
    in
      lp t
    end
  and freshen (y, ty, t1) =
    let
      val newName = freshVarName ()
      val args = {replaceThis=y, withThis=A.Var(newName),inThis=t1}
    in
      A.Abs (newName, ty, subst args)
    end
	    
end
