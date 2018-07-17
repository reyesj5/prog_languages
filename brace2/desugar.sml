structure Desugar : sig

  val term : AST.term -> ULC.term

end = struct

  structure A = AST

  fun term (A.Var x) = ULC.Var x
    | term (A.Abs (x, t)) = ULC.Abs (x, term t)
    | term (A.App (t1, t2)) = ULC.App (term t1, term t2)
    | term (A.Let (x, t1, t2)) = Subst.subst (x, term t1, term t2)

(* Notice how helpful the type system is for implementing this
 * operation. This is SML at its very best.
 *)

end
          
