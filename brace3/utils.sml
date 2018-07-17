structure Utils : sig

(* print the string, then a newline *)
  val println : string -> unit

(* take items from list as long as items pass the test *)
  val takeWhile : ('a -> bool) -> 'a list -> 'a list * 'a list

(* take as many as n items from the list *)
  val upto : int -> 'a list -> 'a list
				  
(* If you have any other general-purpose functions you might
 * want to use throughout, add them here.
 *)

end = struct

  fun println s = (TextIO.print s; TextIO.print "\n")

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

  fun upto _ [] = []
    | upto n (x::xs) = if n<=0 then [] else x::(upto (n-1) xs)

end
