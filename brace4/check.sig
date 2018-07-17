signature CHECK = sig

  (* check if given boolean is true *)
  val assertT : bool * string -> unit

  (* check if given boolean is false *)
  val assertF : bool * string -> unit

  (* check if two items are equal by built-in polymorphic equality *)
  val expect : ''a * ''a * string -> unit

  (* check if two items are equal by equality function *)
  val expectBy : ('a * 'a -> bool) * 'a * 'a * string -> unit

  (* check if the first item is among the list by built-in polymorphic equality *)
  val among : ''a * (''a list) * string -> unit

  (* check if the first item is among the list by equality function *)
  val amongBy : ('a * 'a -> bool) * 'a * ('a list) * string -> unit
                                                                   
  (* check if two floating-point values are within epsilon of another *)
  val within : real * real * real * string -> unit

  (* check if given delayed computation raises an exception *)
  val exn : (unit -> 'a) * string -> unit

  (* given a toString function, look at two results *)
  (* - this is useful for values where algorithmic equality *)
  (*   is either difficult or impossible *)
  val eyeball : ('a -> string) * 'a * 'a * string -> unit
					 
end
        
