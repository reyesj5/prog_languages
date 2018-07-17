structure Read : sig

  (* file : Read the contents of the named file into a string. *)
  val file : string -> string

end = struct

  fun file filename =
    let
      val instrm = TextIO.openIn filename
      fun lp _ =
       (case TextIO.inputLine instrm
          of NONE => ""
           | SOME line => line ^ lp ())
      val fileContentsAsString = lp ()
      val _ = TextIO.closeIn instrm
    in
      fileContentsAsString
    end

end
