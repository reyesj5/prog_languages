signature CHECK = sig

  (* check if given boolean is true *)
  val assertT : bool * string -> unit

  (* check if given boolean is false *)
  val assertF : bool * string -> unit

  (* check if two items are equal by built-in polymorphic equality *)
  val expect : ''a * ''a * string -> unit

  (* check if two items are equal by equality function *)
  val expectBy : ('a * 'a -> bool) * 'a * 'a * string -> unit

  (* check if two floating-point values are within epsilon of another *)
  val within : real * real * real * string -> unit

  (* check if given delayed computation raises an exception *)
  val exn : (unit -> 'a) * string -> unit

end
	
