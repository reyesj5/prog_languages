structure Scan : sig

  datatype token
    = LBrace
    | RBrace
    | LParen
    | RParen
    | Identifier of string
    | LeftArrow
    | In
	  
  val scan : string -> token list

  val tos : token -> string

end = struct

  datatype token
    = LBrace
    | RBrace
    | LParen
    | RParen
    | Identifier of string
    | LeftArrow
    | In

  fun tos LBrace = "LBrace"
    | tos RBrace = "RBrace"
    | tos LParen = "LParen"
    | tos RParen = "RParen"
    | tos (Identifier x) = "Identifier(" ^ x ^ ")"
    | tos LeftArrow = "LeftArrow"
    | tos In = "In"
	  
  fun afterNewline cs =
    let
      fun lp [] = []
        | lp (#"\n"::cs) = cs
        | lp (_::cs) = lp cs
    in
      lp cs
    end

(* take items from list as long as items pass the test f *)
  fun takeWhile f xs =
    let
      fun lp ([], acc) = (rev acc, [])
        | lp (list as x::xs, acc) =
            if f x 
            then lp (xs, x::acc)
            else (rev acc, list)
    in
      lp (xs, [])
    end

  (* remember this thing? *)
  infix \/
  fun p \/ q = fn x => (p x orelse q x)
	
  fun nextID (cs : char list) : token * char list  =
    let
      fun underscore c = c = #"_"
      val (cs1, cs2) = takeWhile (Char.isAlphaNum \/ underscore) cs
      val s = implode cs1
      val _ = Check.assertF (s="", "var name cannot be empty")
    in
        (* the only reserved word in the language is "in"; I account for that here *)
	(if s="in" then In else Identifier s, cs2)
    end

  fun nextToken (cs : char list) : (token * char list) option =
    let
      fun lp [] = NONE
        | lp (#"{" :: cs) = SOME (LBrace, cs)
        | lp (#"}" :: cs) = SOME (RBrace, cs)
        | lp (#"(" :: cs) = SOME (LParen, cs)
        | lp (#")" :: cs) = SOME (RParen, cs)
	| lp (#"<" :: #"-" :: cs) = SOME (LeftArrow, cs)
        | lp (#"/" :: #"/" :: cs) = lp (afterNewline cs)
        | lp ((#" " | #"\t" | #"\n") :: cs) = lp cs
        | lp (cs as c::_) =
            if Char.isAlpha c then SOME (nextID cs) 
            else raise Fail ("scan error at " ^ (implode cs))
    in
      lp cs
    end

  fun scan program =
    let
      fun lp cs = 
       (case nextToken cs
          of NONE => []
           | SOME (tok, cs') => tok :: lp cs')
    in
      lp (explode program)
    end  

end
