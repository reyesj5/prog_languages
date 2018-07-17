structure Scan : sig

  datatype token
    = LBrace
    | RBrace
    | T
    | F
    | Zero
    | If
    | Then
    | Else
    | PlusOne
    | MinusOne
    | IsZ
    | DoubleAmpersand
    | DoublePipe
    | Bang
    | DoubleEqual
    | Identifier of string
    | In
    | Of
    | LeftArrow   (* this is "<-" *)
    | RightArrow  (* this is "->" *)
    | Hash
    | Hash1
    | Hash2
    | Some
    | None
    | LParen
    | RParen
    | Case
    | Pipe
    | Star
    | Plus
    | Opt

  val scan : string -> token list
  val tos  : token -> string

end = struct

  datatype token
    = LBrace
    | RBrace
    | T
    | F
    | Zero
    | If
    | Then
    | Else
    | PlusOne
    | MinusOne
    | IsZ
    | DoubleAmpersand
    | DoublePipe
    | Bang
    | DoubleEqual
    | Identifier of string
    | In
    | Of
    | LeftArrow
    | RightArrow
    | Hash
    | Hash1
    | Hash2
    | Some
    | None
    | LParen
    | RParen
    | Case
    | Pipe
    | Star
    | Plus
    | Opt

  fun afterNewline cs =
    let
      fun lp [] = []
        | lp (#"\n"::cs) = cs
        | lp (_::cs) = lp cs
    in
      lp cs
    end

  fun getIdentifier (var, cs) =
    let 
      fun lp (var, []) = (Identifier var, [])
        | lp (var, (#" " :: cs')) = (Identifier var, cs')
        | lp (var, (#"\n" :: cs')) = (Identifier var, cs')
        | lp (var, (#"\t" :: cs')) = (Identifier var, cs')
        | lp (var, (#"}" :: cs')) = (Identifier var, [#"}"]@cs')
        | lp (var, (#")" :: cs')) = (Identifier var, [#")"]@cs')
        | lp (var, (#"_" :: cs')) = lp (var ^ "_", cs')
        | lp (var, (x::cs')) = if Char.isAlphaNum(x) 
                                then lp (var ^ Char.toString(x), cs') 
                                else raise Fail ("Scan error: Invalid characters in identifier") 
    in 
        lp (var, cs)
    end

  fun nextToken (cs : char list) : (token * char list) option =
    let
(* This tokenizer handles the symbolic tokens, but none of the alphabetic ones. *)
(* Your scanner needs to determine whether a string of characters is a reserved word or an identifier. *)
(* An identifier is the broad category including variable names and type names. *)
      fun lp [] = NONE
        | lp (#"{" :: cs) = SOME (LBrace, cs)
        | lp (#"}" :: cs) = SOME (RBrace, cs)
        | lp (#"0" :: cs) = SOME (Zero, cs)
        | lp (#"+" :: #"1" :: cs) = SOME (PlusOne, cs)
        | lp (#"-" :: #"1" :: cs) = SOME (MinusOne, cs)
        | lp (#"&" :: #"&" :: cs) = SOME (DoubleAmpersand, cs)
        | lp (#"|" :: #"|" :: cs) = SOME (DoublePipe, cs)
        | lp (#"|" :: cs) = SOME (Pipe, cs)
        | lp (#"!" :: cs) = SOME (Bang, cs)
        | lp (#"=" :: #"=" :: cs) = SOME (DoubleEqual, cs)
        | lp (#"(" :: cs) = SOME (LParen, cs)
        | lp (#")" :: cs) = SOME (RParen, cs)
        | lp (#"<" :: #"-" :: cs) = SOME (LeftArrow, cs)
        | lp (#"-" :: #">" :: cs) = SOME (RightArrow, cs)
        | lp (#"#" :: #"1" :: cs) = SOME (Hash1, cs)
        | lp (#"#" :: #"2" :: cs) = SOME (Hash2, cs)
        | lp (#"#" :: cs) = SOME (Hash, cs)
        | lp (#"*" :: cs) = SOME (Star, cs)
        | lp (#"+" :: cs) = SOME (Plus, cs)
        | lp (#"/" :: #"/" :: cs) = lp (afterNewline cs)
        | lp (#" " :: cs) = lp cs
        | lp (#"\n" :: cs) = lp cs
        | lp (#"\t" :: cs) = lp cs
        | lp (#"T" :: []) = SOME (T, [])
        | lp (#"T" :: #"}" :: cs) = SOME (T, [#"}"]@cs)
        | lp (#"T" :: x :: cs) = if Char.isSpace(x) 
                                    then SOME (T, cs) 
                                    else SOME (getIdentifier ("T"^Char.toString(x), cs)) 
        | lp (#"F" :: []) = SOME (F, [])
        | lp (#"F" :: #"}" :: cs) = SOME (F, [#"}"]@cs)
        | lp (#"F" :: x :: cs) = if Char.isSpace(x) 
                                    then SOME (F, cs) 
                                    else SOME (getIdentifier ("F"^Char.toString(x), cs)) 
        | lp (#"i" :: #"f" :: []) = SOME (If, [])
        | lp (#"i" :: #"f" :: #"}" :: cs) = SOME (If, [#"}"]@cs)
        | lp (#"i" :: #"f" :: x :: cs) = if Char.isSpace(x) 
                                    then SOME (If, cs) 
                                    else SOME (getIdentifier ("if"^Char.toString(x), cs)) 
        | lp (#"t" :: #"h" :: #"e" :: #"n" :: []) = SOME (Then, [])
        | lp (#"t" :: #"h" :: #"e" :: #"n" :: #"}" :: cs) = SOME (Then, [#"}"]@cs)
        | lp (#"t" :: #"h" :: #"e" :: #"n" :: x :: cs) = if Char.isSpace(x) 
                                    then SOME (Then, cs) 
                                    else SOME (getIdentifier ("then"^Char.toString(x), cs)) 
        | lp (#"e" :: #"l" :: #"s" :: #"e" :: []) = SOME (Else, [])
        | lp (#"e" :: #"l" :: #"s" :: #"e" :: #"}" :: cs) = SOME (Else, [#"}"]@cs)
        | lp (#"e" :: #"l" :: #"s" :: #"e" :: x :: cs) = if Char.isSpace(x) 
                                    then SOME (Else, cs) 
                                    else SOME (getIdentifier ("else"^Char.toString(x), cs))
        | lp (#"i" :: #"s" :: #"z" :: []) = SOME (IsZ, [])
        | lp (#"i" :: #"s" :: #"z" :: #"}" :: cs) = SOME (IsZ, [#"}"]@cs)
        | lp (#"i" :: #"s" :: #"z" :: x :: cs) = if Char.isSpace(x) 
                                    then SOME (IsZ, cs) 
                                    else SOME (getIdentifier ("isz"^Char.toString(x), cs))
        | lp (#"i" :: #"n" :: []) = SOME (In, [])
        | lp (#"i" :: #"n" :: #"}" :: cs) = SOME (In, [#"}"]@cs)
        | lp (#"i" :: #"n" :: x :: cs) = if Char.isSpace(x) 
                                    then SOME (In, cs) 
                                    else SOME (getIdentifier ("in"^Char.toString(x), cs))
        | lp (#"s" :: #"o" :: #"m" :: #"e" :: []) = SOME (Some, [])
        | lp (#"s" :: #"o" :: #"m" :: #"e" :: #"}" :: cs) = SOME (Some, [#"}"]@cs)
        | lp (#"s" :: #"o" :: #"m" :: #"e" :: x :: cs) = if Char.isSpace(x) 
                                    then SOME (Some, cs) 
                                    else SOME (getIdentifier ("some"^Char.toString(x), cs))
        | lp (#"n" :: #"o" :: #"n" :: #"e" :: []) = SOME (None, [])
        | lp (#"n" :: #"o" :: #"n" :: #"e" :: #"}" :: cs) = SOME (None, [#"}"]@cs)
        | lp (#"n" :: #"o" :: #"n" :: #"e" :: x :: cs) = if Char.isSpace(x) 
                                    then SOME (None, cs) 
                                    else SOME (getIdentifier ("none"^Char.toString(x), cs))
        | lp (#"c" :: #"a" :: #"s" :: #"e" :: []) = SOME (Case, [])
        | lp (#"c" :: #"a" :: #"s" :: #"e" :: #"}" :: cs) = SOME (Case, [#"}"]@cs)
        | lp (#"c" :: #"a" :: #"s" :: #"e" :: x :: cs) = if Char.isSpace(x) 
                                    then SOME (Case, cs) 
                                    else SOME (getIdentifier ("case"^Char.toString(x), cs))
        | lp (#"o" :: #"f" :: []) = SOME (Of, [])
        | lp (#"o" :: #"f" :: #"}" :: cs) = SOME (Of, [#"}"]@cs)
        | lp (#"o" :: #"f" :: x :: cs) = if Char.isSpace(x) 
                                    then SOME (Of, cs) 
                                    else SOME (getIdentifier ("of"^Char.toString(x), cs))
        | lp (#"O" :: #"p" :: #"t" :: x :: cs) = if Char.isSpace(x) 
                                    then SOME (Opt, cs) 
                                    else SOME (getIdentifier ("Opt"^Char.toString(x), cs))
        | lp (x :: cs) = if Char.isAlpha(x) 
                            then SOME (getIdentifier (Char.toString(x), cs))
                            else raise Fail ("scan failure or unimplemented token at " ^ implode cs)
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

  fun tos LBrace = "{"
    | tos RBrace = "}"
    | tos T = "T"
    | tos F = "F"
    | tos Zero = "0"
    | tos If = "if"
    | tos Then = "then"
    | tos Else = "else"
    | tos PlusOne = "+1"
    | tos MinusOne = "-1"
    | tos IsZ = "isz"
    | tos DoubleAmpersand = "&&"
    | tos DoublePipe = "||"
    | tos Bang = "!"
    | tos DoubleEqual = "=="
    | tos (Identifier s) = "Identifier(" ^ s ^ ")"
    | tos In = "in"
    | tos Of = "of"
    | tos LeftArrow = "<-"
    | tos RightArrow = "->"
    | tos Hash = "#"
    | tos Hash1 = "#1"
    | tos Hash2 = "#2"
    | tos Some = "some"
    | tos None = "none"
    | tos LParen = "("
    | tos RParen = ")"
    | tos Case = "case"
    | tos Pipe = "|"
    | tos Star = "*"
    | tos Plus = "+"
    | tos Opt = "Opt"
    
end
