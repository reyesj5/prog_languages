structure PSET = struct

  (* === Problem 1 === *)
			    
  (* Here are inductively defined natural numbers. *)
			      
  datatype nat
    = Zero
    | Succ of nat

  (* In natToInt, replace the raise expression with an
   * implementation of the function.
   *)
		
  fun natToInt (n : nat) : int = 
    (case n 
        of Zero => 0
        | Succ n => 1 + natToInt n)
    
  (* Having written natToInt, uncomment the following tests. *)
	  
  val _ = Check.expect (natToInt Zero, 0, "natToInt Zero")
  val _ = Check.expect (natToInt (Succ Zero), 1, "natToInt (Succ Zero)")
	  
  (* Continue by implementing intToNat and uncommenting the tests immediately below. *)
	  
  fun intToNat (i : int) : nat =
    if i = 0 then Zero 
        else if i = 1 then Succ Zero
            else Succ (intToNat (i-1))

    (* Tests *)
  val _ = Check.expect (intToNat 0, Zero, "intToNat 0")
  val _ = Check.expect (intToNat 1, Succ Zero, "intToNat 1")
  	  
  fun natAdd (m : nat, n : nat) : nat =
    intToNat (natToInt m + natToInt n)
	  
  (* Write some of your own Check.expect tests here, and continue
   * doing so throughout this set of exercises.
   *)
   (* Tests *)
   val _ = Check.expect (natAdd (Zero, Succ Zero), Succ Zero,  "natAdd Zero and 1")
   val _ = Check.expect (natAdd (Succ Zero, Succ Zero), Succ(Succ Zero), "natAdd Zero and 1")
	  
  (* Write natEq without calling natToInt or intToNat. *)

  fun natEq (m : nat, n : nat) : bool =
    (case (m,n) 
        of (Zero, Zero) => true
        | (Succ m, Succ n) => natEq (m, n)
        | _ => false)
        
    (* Tests *)
    val _ = Check.expect (natEq (Succ Zero, Succ Zero), true, "natEq 1 and 1")
    val _ = Check.expect (natEq (Zero, Succ Zero), false, "natAdd Zero and 1")
    val _ = Check.expectBy (natEq, Zero, Zero, "natEq for Zero and Zero")
      
      
  (* natGT checks if m is strictly greater than n. *)

  (* Write natGT without calling natToInt or intToNat. *)

  fun natGT (m : nat, n : nat) : bool =
    (case (m,n) 
        of (Succ m, Zero) => true
        | (Succ m, Succ n) => natGT (m, n)
        | _ => false)
        
    (* Tests *)
    val _ = Check.expect (natGT (Succ Zero, Succ Zero), false, "natEq 1 and 1")
    val _ = Check.expect (natGT (Zero, Succ Zero), false, "natAdd Zero and 1")
    val _ = Check.assertF (natGT (Zero, Succ Zero), "natAdd Zero and 1")
    val _ = Check.expect (natGT (Succ Zero, Zero), true, "natEq 1 and 0")
    val _ = Check.assertT (natGT (Succ Zero, Zero), "natEq 1 and 0")
	  	  
  (* natToString should build a strings like "Zero",
   * "Succ(Zero)","Succ(Succ(Zero))", etc.
   * The string concatenation operator is ^, as in "a" ^ "b".
   *)
	  
  fun natToString (n : nat) : string =
    (case n 
        of Zero => "Zero"
        | Succ n => "Succ(" ^ natToString n ^ ")")
	
    (* Tests *)
    val _ = Check.expect (natToString (Succ (Succ Zero)), "Succ(Succ(Zero))", "natToString 1 and 1")
    val _ = Check.expect (natToString (Succ Zero), "Succ(Zero)", "natToString Zero and 1")
    val _ = Check.expect (natToString Zero, "Zero", "natToString 1 and 1")
      
  (* === Problem 2 === *)
	  
  datatype filesize
    = B of int 
    | KB of real
    | MB of real
    | GB of real
    | TB of real
          
  fun toMB (s : filesize) : real =
    (case s
        of B x => real x / 1048576.0
        | KB x => x / 1024.0
        | MB x => x
        | GB x => x * 1024.0
        | TB x => x * 1048576.0)
	  
    (* Tests *)
    val _ = Check.within (toMB (B 13530), 0.012903213500976562, 0.001, "toMB with 13530 B")
    val _ = Check.within (toMB (KB 13530.0), 13.212890625, 0.001, "toMB with 13530 KB")
    val _ = Check.within (toMB (MB 13530.0), 13530.0, 0.001, "toMB with 13530 MB")
    val _ = Check.within (toMB (GB 3.2), 3276.8, 0.001, "toMB with 3.2 GB")
    val _ = Check.within (toMB (TB 0.2), 209715.2, 0.001, "toMB with 0.2 TB")
    

  (* === Problem 3 === *)
	  
  (* Here is a useful type synonym. *)
  type 'a pred = 'a -> bool

    (* Test Fucntions*)
    fun isPos (i:int) : bool = i>0
    
    fun isNeg (i:int) : bool = i<0
    
    fun isOne (i:int) : bool = i=1

  (* The infix directive instructs the parser that the
   * given identifier is an infix operator. 
   *)
	  
  infix \/
  infix /\

  (* \/ is a "disjunctive composition" operator for tests.
   * Assuming you have tests isPrime and isOdd, then
   * the test (isPrime \/ isOdd) identifies primes and/or odds.
   *)

  fun (p : 'a pred) \/ (q : 'a pred) : 'a pred =
    fn x => p (x) orelse q (x)
	  
    (* Tests *)
    val _ = Check.expect ((isPos \/ isNeg) ~2, true, "infix with isPos and isNeg on -2")
    val _ = Check.expect ((isPos \/ isNeg) 0, false, "infix with isPos and isNeg on 0")
    val _ = Check.expect ((isPos \/ isNeg) 2, true, "infix with isPos and isNeg on 2")
      
  (* /\ is a "conjunctive composition" operator for tests.
   * Assuming you have tests isPrime and isOdd, then
   * the test (isPrime /\ isOdd) identifies odd primes.
   *)
	  
  fun (p : 'a pred) /\ (q : 'a pred) : 'a pred =
    fn x => p (x) andalso q (x)

    (* Tests *)
    val _ = Check.expect ((isPos \/ isOne) ~2, false, "infix with isPos and isOne on -2")
    val _ = Check.expect ((isPos \/ isOne) 0, false, "infix with isPos and isOne on 0")
    val _ = Check.expect ((isPos \/ isOne) 1, true, "infix with isPos and isOne on 1")

  (* === Problem 4 === *)
	  
  (* Here is a mutually recursive datatype for trees
   * that alternate between having 2 and 3 children at
   * each level, and furthermore alternate between 
   * having 'a and 'b data at each level. 
   *)
	  
  (* E2 is an empty tree of type t2; E3 likewise for t3. *)
	  
  datatype ('a, 'b) t2
    = E2
    | Nd2 of 'a * ('a, 'b) t3 * ('a, 'b) t3
  and ('a, 'b) t3
    = E3
    | Nd3 of 'b * ('a, 'b) t2 * ('a, 'b) t2 * ('a, 'b) t2
			
            
    (* Sample Trees *)
    val tree1 = Nd2("node", E3, E3)
    val tree2 = Nd2("node", E3, Nd3("node", Nd2("node", E3, Nd3("node", E2, E2, E2)), E2, Nd2("node", E3, E3)))
    val tree3 = E3
            
  (* Count the number of nodes in a given tree. Nodes to be 
   * counted are Nd2 and Nd3 values, not E2 or E3.
   *)
						       
  fun numNodes2 (t : ('a, 'b) t2) : int =
    (case t 
        of E2 => 0
        | Nd2 (_,x,y) => 1 + numNodes3 x + numNodes3 y)
  and numNodes3 (t : ('a, 'b) t3) : int =
    (case t
        of E3 => 0
        | Nd3 (_,x,y,z) => 1 + numNodes2 x + numNodes2 y + numNodes2 z)
	    
    (* Tests *)
    val _ = Check.expect (numNodes2 tree1, 1, "numNodes2 with tree of 1 nodes")
    val _ = Check.expect (numNodes2 tree2, 5, "numNodes2 with tree of 5 nodes")
    val _ = Check.expect (numNodes3 tree3, 0, "numNodes2 with tree of 0 nodes")
    
  (* === Problem 5 === *)
	    
  datatype rank
    = Ace | Two | Three | Four | Five | Six | Seven
      | Eight | Nine | Ten | Jack | Queen | King
					      
  datatype suit
    = Diamond | Clubs | Hearts | Spades
				   
  datatype card
    = Card of rank * suit
		    
    (* Sample Cards *)
    val c1:card = Card (Ace, Spades);
    val c2:card = Card (Ace, Clubs);
    val c3:card = Card (Nine, Hearts);
    val c4:card = Card (King, Diamond);
    val c5:card = Card (Six, Spades);
            
  fun suitEq (s1 : suit, s2 : suit) : bool =
    (case (s1, s2)
        of (Diamond, Diamond) => true
        | (Clubs, Clubs) => true
        | (Hearts, Hearts) => true
        | (Spades, Spades) => true
        | (_, _) => false)
            
    (* Tests *)
    val _ = Check.expectBy (suitEq, Spades, Spades, "suitEq with Spades")
    val _ = Check.expect (suitEq (Spades, Diamond), false, "suitEq with Spades and Diamond")
    val _ = Check.expectBy (suitEq, Clubs, Clubs, "suitEq with Clubs")
        
  fun sameSuit (c1 : card, c2 : card) : bool =
    (case (c1, c2)
        of (Card (_, s1), Card (_, s2)) => suitEq(s1, s2))
    
    (* Tests *)
    val _ = Check.expectBy (sameSuit, c1, c5, "sameSuit with c1 and c5")
    val _ = Check.expect (sameSuit (c1, c5), true, "sameSuit with Spades cards")
    val _ = Check.expect (sameSuit (c1, c4), false, "sameSuit with different suit cards")
    
    
  fun differentRank (c1 : card, c2 : card) : bool =
    (case (c1, c2)
        of (Card (Ace, _), Card (Ace, _)) => false 
        | (Card (Two, _), Card (Two, _)) => false
        | (Card (Three, _), Card (Three, _)) => false
        | (Card (Four, _), Card (Four, _)) => false
        | (Card (Five, _), Card (Five, _)) => false
        | (Card (Six, _), Card (Six, _)) => false
        | (Card (Seven, _), Card (Seven, _)) => false
        | (Card (Eight, _), Card (Eight, _)) => false
        | (Card (Nine, _), Card (Nine, _)) => false
        | (Card (Jack, _), Card (Jack, _)) => false
        | (Card (Queen, _), Card (Queen, _)) => false
        | (Card (King, _), Card (King, _)) => false
        | (Card (_, _), Card (_, _)) => true)
	
    (* Tests *)
    (* Tests *)
    val _ = Check.expect (differentRank (c1, c5), true, "differentRank with c1 and c5")
    val _ = Check.expect (differentRank (c1, c2), false, "differentRank with same rank Ace")
    val _ = Check.expect (differentRank (c2, c3), true, "differentRank with different ranks")
    
    
  fun colorEq (s1 : suit, s2 : suit) : bool =
    if suitEq(s1,s2) then true else
    (case (s1, s2)
        of (Diamond, Hearts) => true
        | (Hearts, Diamond) => true
        | (Spades, Clubs) => true
        | (Clubs, Spades) => true
        | (_, _) => false)
        
    (* Tests *)
    val _ = Check.expectBy (colorEq, Spades, Spades, "colorEq with Spades")
    val _ = Check.expect (colorEq (Spades, Diamond), false, "colorEq with Spades and Diamond")
    val _ = Check.expectBy (colorEq, Clubs, Spades, "colorEq with Clubs and Spades")
    
  fun sameColor (c1 : card, c2 : card) : bool =
    (case (c1, c2)
        of (Card (_, s1), Card (_, s2)) => colorEq(s1, s2))
        
    (* Tests *)
    val _ = Check.expectBy (sameColor, c1, c5, "sameColor with c1 and c5")
    val _ = Check.expect (sameColor (c3, c4), true, "sameColor with Hearts and DIamond cards")
    val _ = Check.expect (sameColor (c2, c5), true, "sameColor with different suit cards but same color")

end
