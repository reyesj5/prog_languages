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

  val scan : string -> token list

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


  fun nextToken (cs:char list) : (token * char list) option = 
    let 
        fun lp [] = NONE
          | lp (#"{" :: cs') = SOME (LBrace, cs')
          | lp (#"}" :: cs') = SOME (RBrace, cs')
          | lp (#"T" :: cs') = SOME (T, cs')
          | lp (#"F" :: cs') = SOME (F, cs')
          | lp (#"0" :: cs') = SOME (Zero, cs')
          | lp (#"i" :: #"f" :: cs') = SOME (If, cs')
          | lp (#"t" :: #"h" :: #"e" :: #"n" :: cs') = SOME (Then, cs')
          | lp (#"e" :: #"l" :: #"s" :: #"e" :: cs') = SOME (Else, cs')
          | lp (#"+" :: #"1" :: cs') = SOME (PlusOne, cs')
          | lp (#"-" :: #"1" :: cs') = SOME (MinusOne, cs')
          | lp (#"i" :: #"s" :: #"z" ::cs') = SOME (IsZ, cs')
          | lp (#"&" :: #"&" :: cs') = SOME (DoubleAmpersand, cs')
          | lp (#"|" :: #"|" :: cs') = SOME (DoublePipe, cs')
          | lp (#"!" :: cs') = SOME (Bang, cs')
          | lp (#"=" :: #"=" :: cs') = SOME (DoubleEqual, cs')
          | lp (#" " :: cs') = lp cs'
          | lp (#"\t" :: cs') = lp cs'
          | lp (#"\n" :: cs') = lp cs'
          | lp (#"/" :: #"/" :: cs') = 
            let
                fun comment [] = []
                  | comment (#"\n" :: cs') = cs'
                  | comment cs' = comment (tl cs')
            in
                lp (comment cs')
            end
          | lp cs' = raise Fail ("Scan error: cannot scan " ^ implode cs')
    in 
        lp cs
    end

  fun scan program : token list = 
    let 
        fun lp chars =
            (case nextToken chars 
                of NONE => []
                | SOME (tok, chars') => tok :: lp chars')
    in
        lp (explode program)
    end

end