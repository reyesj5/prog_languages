structure Scan : sig

  datatype token
    = LBrace
    | RBrace
    | LParen
    | RParen
    | Identifier of string
    | LeftArrow
    | RightArrow
    | Colon
    | Dot
    | Comma
    | In
    | T
    | F
    | Not
    | If
    | Ref
    | Bang
    | Equals
    | ColonEquals
    | Hash
    | Dollar
	
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
    | RightArrow
    | Colon
    | Dot
    | Comma
    | In
    | T
    | F
    | Not
    | If
    | Ref
    | Bang
    | Equals
    | ColonEquals
    | Hash
    | Dollar

  fun tos LBrace = "LBrace"
    | tos RBrace = "RBrace"
    | tos LParen = "LParen"
    | tos RParen = "RParen"
    | tos (Identifier x) = "Identifier(" ^ x ^ ")"
    | tos LeftArrow = "LeftArrow"
    | tos RightArrow = "RightArrow"
    | tos Colon = "Colon"
    | tos Dot = "Dot"
    | tos Comma = "Comma"		      
    | tos In = "In"
    | tos T = "T"
    | tos F = "F"
    | tos Not = "Not"
    | tos If = "If"
    | tos Ref = "Ref"
    | tos Bang = "Bang"
    | tos Equals = "Equals"
    | tos ColonEquals = "ColonEquals"
    | tos Hash = "Hash"
    | tos Dollar = "Dollar"
		     
  fun afterNewline cs =
    let
      fun lp [] = []
        | lp (#"\n"::cs) = cs
        | lp (_::cs) = lp cs
    in
      lp cs
    end

  infix \/
  fun p \/ q = fn x => (p x orelse q x)

  val takeWhile = Utils.takeWhile
			   
  fun nextID (cs : char list) : token * char list  =
    let
      fun underscore c = c = #"_"
      val (cs1, cs2) = takeWhile (Char.isAlphaNum \/ underscore) cs
      val s = implode cs1
      val _ = Check.assertF (s="", "var name cannot be empty")
    in
	case s
	 of "in" => (In, cs2)
	  | "T"  => (T, cs2)
	  | "F"  => (F, cs2)
	  | "if" => (If, cs2)
	  | "ref" => (Ref, cs2)
	  | "not" => (Not, cs2)
	  | _ => (Identifier s, cs2)
    end

  fun nextToken (cs : char list) : (token * char list) option =
    let
      fun lp [] = NONE
        | lp (#"{" :: cs) = SOME (LBrace, cs)
        | lp (#"}" :: cs) = SOME (RBrace, cs)
        | lp (#"(" :: cs) = SOME (LParen, cs)
        | lp (#")" :: cs) = SOME (RParen, cs)
	| lp (#"<" :: #"-" :: cs) = SOME (LeftArrow, cs)
	| lp (#"-" :: #">" :: cs) = SOME (RightArrow, cs)
	| lp (#":" :: #"=" :: cs) = SOME (ColonEquals, cs)
	| lp (#":" :: cs) = SOME (Colon, cs)
	| lp (#"." :: cs) = SOME (Dot, cs)
	| lp (#"," :: cs) = SOME (Comma, cs)
	| lp (#"!" :: cs) = SOME (Bang, cs)
	| lp (#"#" :: cs) = SOME (Hash, cs)
	| lp (#"=" :: cs) = SOME (Equals, cs)
	| lp (#"$" :: cs) = SOME (Dollar, cs)
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
