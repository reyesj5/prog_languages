signature REDUCTION_SYSTEM = sig

(* step is, as usual, one step of evaluation, if possible *)
  val step   : ULC.term -> ULC.term option

(* reduce, formerly eval, is to iterate step until a normal form *)
  val reduce : ULC.term -> ULC.term

(* steps collects the terms step by step and returns the sequence *)
  val steps  : ULC.term -> ULC.term list

end
