structure Scan : sig

  datatype token
    = LBrace
    | RBrace
    | LParen
    | RParen
    | Dot
    | Identifier of string
    | LeftArrow
    | DoubleLeftArrow
    | In
    | T
    | F
    | If
    | Then
    | Else
    | Bang
    | DoubleAmpersand
    | DoublePipe
    | Nat of int
    | Plus
    | Star
    | Hash
    | Hash1
    | Hash2
    | Slash
	  
  val scan : string -> token list

  val tos : token -> string

end = struct

  datatype token
    = LBrace
    | RBrace
    | LParen
    | RParen
    | Dot
    | Identifier of string
    | LeftArrow
    | DoubleLeftArrow
    | In
    | T
    | F
    | If
    | Then
    | Else
    | Bang
    | DoubleAmpersand
    | DoublePipe
    | Nat of int
    | Plus
    | Star
    | Hash
    | Hash1
    | Hash2
    | Slash
      
  fun tos LBrace = "LBrace"
    | tos RBrace = "RBrace"
    | tos LParen = "LParen"
    | tos RParen = "RParen"
    | tos Dot = "Dot"
    | tos (Identifier x) = "Identifier(" ^ x ^ ")"
    | tos LeftArrow = "LeftArrow"
    | tos DoubleLeftArrow = "DoubleLeftArrow"
    | tos In = "In"
    | tos T = "T"
    | tos F = "F"
    | tos If = "If"
    | tos Then = "Then"
    | tos Else = "Else"
    | tos Bang = "Bang"
    | tos DoubleAmpersand = "DoubleAmpersand"
    | tos DoublePipe = "DoublePipe"
    | tos (Nat n) = "Nat(" ^ Int.toString n ^ ")"
    | tos Plus = "Plus"
    | tos Star = "Star"
    | tos Hash = "Hash"
    | tos Hash1 = "Hash1"
    | tos Hash2 = "Hash2"
    | tos Slash = "Slash"
		      
  fun afterNewline cs =
    let
      fun lp [] = []
        | lp (#"\n"::cs) = cs
        | lp (_::cs) = lp cs
    in
      lp cs
    end

  fun nextID (cs : char list) : token * char list  =
    let
      val (cs1, cs2) = Utils.takeWhile Char.isAlphaNum cs
      val s = implode cs1
      val _ = Check.assertF (s="", "var name cannot be empty")
      val tokenOrIdent =
        (case s
	  of "in" => In
	   | "T" => T
	   | "F" => F
	   | "if" => If
	   | "then" => Then
	   | "else" => Else
	   | _ =>  Identifier s)
    in
	(tokenOrIdent, cs2)
    end

  fun nextNat (cs : char list) : token * char list  =
    let
      val (cs1, cs2) = Utils.takeWhile Char.isDigit cs
      val s = implode cs1
      val _ = Check.assertF (s="", "nat const cannot be empty")
    in
      case Int.fromString s
        of SOME n => (Nat n, cs2)
        |  NONE => raise Fail ("bug parsing this nat: " ^ s)
    end

  fun nextToken (cs : char list) : (token * char list) option =
    let
      fun lp [] = NONE
        | lp ((#" " | #"\t" | #"\n") :: cs) = lp cs
        | lp (#"{" :: cs) = SOME (LBrace, cs)
        | lp (#"}" :: cs) = SOME (RBrace, cs)
        | lp (#"(" :: cs) = SOME (LParen, cs)
        | lp (#")" :: cs) = SOME (RParen, cs)
	| lp (#"." :: cs) = SOME (Dot, cs)
	| lp (#"<" :: #"-" :: cs) = SOME (LeftArrow, cs)
	| lp (#"<" :: #"=" :: cs) = SOME (DoubleLeftArrow, cs)
	| lp (#"/" :: #"/" :: cs) = lp (afterNewline cs)
	| lp (#"/" :: cs) = SOME (Slash, cs)
	| lp (#"!" :: cs) = SOME (Bang, cs)
	| lp (#"&" :: #"&" :: cs) = SOME (DoubleAmpersand, cs)
	| lp (#"|" :: #"|" :: cs) = SOME (DoublePipe, cs)
	| lp (#"+" :: cs) = SOME (Plus, cs)
	| lp (#"*" :: cs) = SOME (Star, cs)
	| lp (#"#" :: #"1" :: c :: cs) =
	    if Char.isDigit c
	    then SOME (Hash, #"1" :: c :: cs)
	    else SOME (Hash1, c::cs)
	| lp (#"#" :: #"2" :: c :: cs) =
	    if Char.isDigit c
	    then SOME (Hash, #"2" :: c :: cs)
	    else SOME (Hash2, c::cs)
	| lp (#"#" :: cs) = SOME (Hash, cs)
        | lp (cs as c::_) =
            if Char.isAlpha c then SOME (nextID cs)
	    else if Char.isDigit c then SOME (nextNat cs)
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
