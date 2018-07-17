structure TypeCheck : sig

  val typeof : AST.term -> Ty.ty
  val check  : AST.term -> unit

end = struct

  structure A = AST

  structure TypeEnv : sig
    type env   = (string * Ty.ty) list
    val empty  : env
    val lookup : string * env -> Ty.ty option
    val extend : env * string * Ty.ty -> env
  end = struct
    type env   = (string * Ty.ty) list
    val empty = []
    fun lookup (x:string, []) = NONE
      | lookup (x, (y,ty)::gamma') =
	  if x=y
	  then SOME ty
	  else lookup (x, gamma')
    fun extend (gamma,x,t) = (x,t)::gamma
  end
  
  infix <+>
  fun g <+> (x,ty) = TypeEnv.extend (g, x, ty)

  fun typeof t =
    let
      fun lp (g, A.Var x) =
          (case TypeEnv.lookup (x, g)
    	      of SOME ty => ty
    	       | NONE => raise Fail ("type error: free variable " ^ x))
      	| lp (g, A.Abs (x, ty1, t1)) = Ty.Function (ty1, (lp ((g<+>(x,ty1)), t1)))
      	| lp (g, A.App (t1, t2)) = 
          (case (lp (g, t1), lp (g, t2))
            of (Ty.Function (ty1, ty2), ty3) =>
               if Ty.subtype (ty3, ty1)
               then ty2
               else raise Fail "type error: type mismatch in application branches"
             | _ => raise Fail "type error: non-application in application type")
      	| lp (g, A.Let (x, t1, t2)) = 
          (case lp (g, t1)
            of ty => (case lp (g<+>(x,ty), t2)
                        of ty2 => ty2)) 
      	| lp (_, A.Unit) = Ty.Unit
      	| lp (_, A.True) = Ty.Bool
      	| lp (_, A.False) = Ty.Bool
      	| lp (g, A.Not t1) =
      	    if Ty.eq (Ty.Bool, lp (g, t1))
      	    then Ty.Bool
      	    else raise Fail "type error: not applied to non-bool"
      	| lp (g, A.If (t1, t2, t3)) =
      	    (case (lp (g, t1), lp (g, t2), lp (g, t3))
      	       of (Ty.Bool, ty2, ty3) =>
      	            if Ty.eq (ty2, ty3)
      		          then ty2
                    else raise Fail "type error: type mismatch in if branches"
      	      	| _ => raise Fail "type error: non-bool test in if")
        | lp (g, A.Alloc t1) = 
          (case lp (g, t1)
            of ty => Ty.Ref ty) 
      	| lp (g, A.Read t1) = 
          (case lp (g, t1)
            of (Ty.Ref ty) => ty
             | _ => raise Fail "type error: dereferencing a non-location") 
        | lp (g, A.Write (t1, t2)) = 
          (case (lp (g, t1), lp (g, t2))
            of (Ty.Ref ty1, ty2) =>
               if Ty.eq (ty1, ty2)
                    then Ty.Unit
                    else raise Fail "type error: type mismatch in application branches"
                | _ => raise Fail "type error: non-application in application type")
      	| lp (_, A.Location _) = raise Fail "BUG: there no locations in surface language; this shouldn't happen"
        | lp (g, A.Record items) = 
          let
            fun lp' [] = []
              | lp' ((str, t) :: rest) = (str, (lp (g, t))) ::  (lp' rest)
          in
            Ty.Record (lp' items)
          end
      	| lp (g, A.Select (label, t1)) = 
          let
            fun lp' (lb, []) = raise Fail "type error: Selected field is not in the record"
              | lp' (lb, ((str, t) :: rest)) = if lb=str then t else lp' (lb, rest)
          in
            (case lp (g, t1)
              of (Ty.Record types) => lp' (label, types)
               | _ => raise Fail "type error: Selecting from a non-record term")
          end
      	| lp (g, A.Sequence (terms:A.term list)) = 
          let
            fun lp' [] = raise Fail "type error: sequence is empty"
              | lp' (t :: []) = lp (g, t)
              | lp' (t :: rest) = 
                (case lp (g, t)
                  of Ty.Unit => lp' rest
                   | _ => raise Fail "type error: sequence contains non-unit type terms")
          in
            lp' terms
          end
    in
      lp (TypeEnv.empty, t)
    end
    
  fun check t = (typeof t; ())

 
  (* HW6 Crowdsourced Tests from Piazza @243 *)


  val _ = Check.expect(typeof(A.Not(A.False)), Ty.Bool, "typeof 0")
  val _ = Check.expect(typeof(A.If(A.Not(A.False), A.Unit, A.Unit)), Ty.Unit, "typeof 1")
  val _ = Check.expect(typeof(A.Sequence([A.Unit, A.Unit, A.Unit, A.True])), Ty.Bool, "typeof 2")
  val _ = Check.expect(typeof(A.Record([("a", A.Alloc(A.Unit)), ("b", A.False), ("c", A.Unit), ("d", A.True)])),
  Ty.Record([("a", Ty.Ref(Ty.Unit)), ("b", Ty.Bool), ("c", Ty.Unit), ("d", Ty.Bool)]), "typeof 3")
  val _ = Check.expect(typeof(A.Select ("a", A.Record([("a", A.Alloc(A.Unit)), ("b", A.False), ("c", A.Unit), ("d", A.True)]))),
  Ty.Ref(Ty.Unit), "typeof 4")
  val _ = Check.expect(typeof(A.Select ("d", A.Record([("a", A.Alloc(A.Unit)), ("b", A.False), ("c", A.Unit), ("d", A.True)]))),
  Ty.Bool, "typeof 5")
  val _ = Check.expect(typeof(A.Select ("b", A.Record([("a", A.Alloc(A.Unit)), ("b", A.False), ("c", A.Unit), ("d", A.True)]))),
  Ty.Bool, "typeof 6")
  val _ = Check.expect(typeof(A.Read(A.Alloc(A.True))), Ty.Bool, "typeof 7")
  val _ = Check.expect(typeof(A.Read(A.Alloc(A.False))), Ty.Bool, "typeof 8")
  val _ = Check.expect(typeof(A.Write(A.Alloc(A.True), A.False)), Ty.Unit, "typeof 9")
  val _ = Check.expect(typeof(A.Alloc(A.True)), Ty.Ref(Ty.Bool), "typeof 10")
  

  (* sanity tests *)
    val $ = (typeof o Parse.parse o Scan.scan)
    val prog00 = "()"
    val prog01 = "T"
    val prog02 = "F" 
    val prog03 = "{not T}"                        (* not statement *)
    val prog04 = "{if T F T}"                     (* if statement *)
    val prog05 = "{ref T}"                        (* reference creation *)
    val prog06 = "{! {ref T}}"                    (* dereference *)
    val prog07 = "{:= {ref T} F}"                 (* ref reassignment *)
    val prog08 = "{$ () " ^ prog07 ^ " T}"        (* sequence *)
    val prog09 = "{x : Bool . {if x F T}}"        (* basic abstraction *)
    val prog10 = "{x <- F in {if x F T}}"         (* basic let statement *)
    val prog11 = "{one=(), two={ref T}, three=F}" (* record *)
    val prog12 = "{#three " ^ prog11 ^ "}"        (* record selection *)
    val prog11ty = Ty.Record 
        [("one", Ty.Unit), ("two", (Ty.Ref Ty.Bool)), ("three", Ty.Bool)]
    
    val _ = Check.expect($prog00, Ty.Unit, "prog00")
    val _ = Check.expect($prog01, Ty.Bool, "prog01")
    val _ = Check.expect($prog02, Ty.Bool, "prog02")
    val _ = Check.expect($prog03, Ty.Bool, "prog03")
    val _ = Check.expect($prog04, Ty.Bool, "prog04")
    val _ = Check.expect($prog05, (Ty.Ref Ty.Bool), "prog05")
    val _ = Check.expect($prog06, Ty.Bool, "prog06")
    val _ = Check.expect($prog07, Ty.Unit, "prog07")
    val _ = Check.expect($prog08, Ty.Bool, "prog08")
    val _ = Check.expect($prog09, Ty.Function (Ty.Bool, Ty.Bool), "prog09")
    val _ = Check.expect($prog10, Ty.Bool, "prog10")
    val _ = Check.expect($prog11, prog11ty, "prog11")
    val _ = Check.expect($prog12, Ty.Bool, "prog12")


fun test s = typeof (Parse.parse (Scan.scan s))

    val str1 = "({r:{a:Bool,b:Bool}.{not {#a r}}} {a=T,b=F,c=()})"
    val str2 = "{a=T, b={x:Bool.x}, c=()}"
    val str3 = "{a=T, b={x:Bool.x}, d= {ref F}, c=()}"
    val str3 = "{a=T, b={x:Bool.x}, d= {ref F}, c=()}"

    val ty2 = Ty.Record([("a",Ty.Bool),("b",Ty.Function (Ty.Bool,Ty.Bool)),("c",Ty.Unit)])
    val ty3 = Ty.Record([("a",Ty.Bool),("b",Ty.Function (Ty.Bool,Ty.Bool)),("d",Ty.Ref (Ty.Bool)),("c",Ty.Unit)])

    val foo1 = "{r : {d:(Ref Bool)}.{! {#d r}}}"

    fun app f t = test ("(" ^ f ^ t ^ ")")
  
    val _ = Check.expect(test "{#a {a=F}}",Ty.Bool,"test0")
    val _ = Check.expect(test str1,Ty.Bool,"test1")
    val _ = Check.expect(test str2,ty2,"test2")
    val _ = Check.expect(test str3,ty3,"test3")
    val _ = Check.expect(app foo1 str3,Ty.Bool,"test4")

    val _ = Check.exn(fn _ => test "{#a {b=F}}","Didn't fire error1")
    val _ = Check.exn(fn _ => app foo1 str2,"Didn't fire error2")
    
end
