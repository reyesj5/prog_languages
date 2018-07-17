structure Store : sig

(* This is like CS154, but 1000x easier. *)
	      
  exception OutOfMemory
  exception UnallocatedMemory
  exception SegmentationFault

  type loc = int

(* Allocate a place to put the data, put it there, and return the location. *)		 
(* Raise an OutOfMemory exception if no more memory is available. *)
  val malloc : AST.term -> loc

(* Read the data at the location. *)			       
(* Raise an UnallocatedMemory exception if the location contains unitialized data. *)
(* Raise a SegmentationFault if the location is outside the heap. *)
  val read   : loc -> AST.term

(* Write the data to the location. *)			  
(* Raise an UnallocatedMemory exception if the location contains unitialized data. *)
(* Raise a SegmentationFault if the location is outside the heap. *)
  val write  : loc * AST.term -> unit

(* Erase all heap data and make all memory available again. *)
  val clear  : unit -> unit

end = struct

  exception OutOfMemory
  exception UnallocatedMemory
  exception SegmentationFault

  type loc = int

  val maxLen = 1000
  val heap = Array.array (1000, AST.Unit)
  val curr = ref 0

  fun malloc t = if !curr < maxLen 
                  then 
                    let 
                      val _ = Array.update (heap, !curr, t)
                      val location = !curr
                      val _ = (curr := (1 + !curr))
                    in
                      location
                    end  
                  else raise OutOfMemory

  fun read loc = if 0 <= loc andalso loc < maxLen
                  then 
                    if (loc < !curr) 
                    then Array.sub (heap, loc) 
                    else raise UnallocatedMemory
                  else raise SegmentationFault

  fun write (loc, t) = if 0 <= loc andalso loc < maxLen
                  then 
                    if (loc < !curr) 
                    then Array.update (heap, loc, t)
                    else raise UnallocatedMemory
                  else raise SegmentationFault

  fun clear () = 
    let
      val _ = (curr := 0)
      fun lp 0 = ()
        | lp n = (Array.update (heap, n-1, AST.Unit); lp (n - 1))
    in
      lp maxLen
    end


  (* HW6 Crowdsourced Tests from Piazza @243 *)

  val _ = clear()
  val _ = Check.expect(malloc(AST.True), 0, "malloc True")
  val _ = Check.expect(malloc(AST.False), 1, "malloc False")
  val _ = Check.expect(read(1), AST.False, "read 1")
  val _ = write(1, AST.True)
  val _ = Check.expect(read(1), AST.True, "read updated 1")
  val _ = Check.exn((fn _ => read(2)), "read 2")
  val _ = Check.exn((fn _ => read(~1)), "read -1")
  val _ = Check.exn((fn _ => read(1005)), "read 1005")
  val _ = Check.exn((fn _ => write(2, AST.True)), "write 2 True")
  val _ = Check.exn((fn _ => write(~1, AST.True)), "write -1 True")
  val _ = Check.exn((fn _ => write(1005, AST.True)), "write 1005 True")
  


  val _ = Check.expect(clear(), (), "clear 0")
  val _ = Check.expect(malloc(AST.Unit), 0, "malloc 0")
  val _ = Check.expect(malloc(AST.True), 1, "malloc 1")
  val _ = Check.expect(malloc(AST.Unit), 2, "malloc 2")
  val _ = Check.expect(malloc(AST.False), 3, "malloc 3")
  val _ = Check.expect(read(0), AST.Unit, "read 0")
  val _ = Check.expect(read(1), AST.True, "read 1")
  val _ = Check.expect(read(3), AST.False, "read 2")
  val _ = Check.expect(write(0, AST.False), (), "write 0")
  val _ = Check.expect(write(0, AST.True), (), "write 1")
  val _ = Check.expect(write(2, AST.Unit), (), "write 2")
  val _ = Check.expect(read(0), AST.True, "read 3")
  val _ = Check.expect(read(1), AST.True, "read 4")
  val _ = Check.expect(read(2), AST.Unit, "read 5")
  val _ = Check.expect(clear(), (), "clear 1")
  
  (*Error tests, commented out but just uncomment and check you got the right error*)

  (*val _ = Check.expect(read(0), AST.Unit, "read 6")*) (*Should get an unallocated memory store error*)
  (*val _ = Check.expect(read(~1), AST.Unit, "read 7")*) (*Should get a segmentation fault*)
  (*val _ = Check.expect(write(2, AST.Unit), (), "write 3")*) (*Should get an unallocated memory store error*)
  (*val _ = Check.expect(write(~1, AST.Unit), (), "write 4")*) (*Should get a segmentation fault*)
end
