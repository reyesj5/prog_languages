structure Ty : sig

  datatype ty
    = Bool
    | Unit
    | Function of ty * ty
    | Ref of ty
    | Record of (string * ty) list

  val tos : ty -> string
  val eq  : ty * ty -> bool

  val subtype : ty * ty -> bool
			 
end = struct

  datatype ty
    = Bool
    | Unit
    | Function of ty * ty
    | Ref of ty
    | Record of (string * ty) list

  val spc = String.concatWith " "

  fun tos Bool = "Bool"
    | tos Unit = "Unit"
    | tos (Function (t1, t2)) = "(-> " ^ tos t1 ^ " " ^ tos t2 ^ ")"
    | tos (Ref t1) = "(Ref " ^ tos t1 ^ ")"
    | tos (Record items) = "{" ^ record items ^ "}"
  and record items = String.concatWith "," (List.map (fn (l,t) => l^":"^(tos t)) items)

  fun eq (t1:ty, t2) = (t1=t2)

  fun recordMember ((str1, ty1), (str2, ty2) :: rest) = if (str1 = str2) andalso eq (ty1, ty2) 
                                                then true 
                                                else recordMember ((str1, ty1), rest)
    | recordMember (t, []) = false
  
  fun removeRecord ((str1, ty1), (str2, ty2) :: rest) = if (str1 = str2)
                                                then rest  
                                                else (str2, ty2) :: removeRecord((str1, ty1), rest)
    | removeRecord (t, []) = []

(* Return true if ty1 is a subtype of ty2, false otherwise. *)
  fun subtype (Bool, ty2) = eq (Bool, ty2)
    | subtype (Unit, ty2) = eq (Unit, ty2)
    | subtype (Ref ty1, Ref ty2) = 
      let
        fun isPermutation ([], []) = true
          | isPermutation ([], items) = false
          | isPermutation (items, []) = false
          | isPermutation ((str1, ty1) :: items', items'') =
            if recordMember ((str1, ty1), items'')
            then isPermutation (items', removeRecord ((str1, ty1), items''))
            else false
      in
        (case (ty1, ty2)
          of (Record items1, Record items2) => isPermutation (items1, items2)
           | _ => eq (ty1, ty2)) 
      end
    | subtype ((Function (s1, s2)), (Function (t1, t2))) = subtype (t1, s1) andalso subtype (s2, t2)
    | subtype (Record items1, Record items2) = 
      let
        fun isSubtype ([], []) = true
          | isSubtype ([], items) = false
          | isSubtype (items, []) = true
          | isSubtype ((str1, ty1) :: items', (str2, ty2) :: items'') = 
            if str1=str2 andalso subtype (ty1, ty2)
            then isSubtype (items', items'')
            else false
        fun member ((str1, ty1), (str2, ty2) :: rest) = if (str1 = str2) andalso subtype (ty1, ty2) 
                                                              then true 
                                                              else member ((str1, ty1), rest)
          | member (t, []) = false
        fun isPermutation ([], []) = true
          | isPermutation ([], items) = false
          | isPermutation (items, []) = true
          | isPermutation ((str1, ty1) :: items', items'') =
            if member ((str1, ty1), items'')
            then isPermutation (items', removeRecord ((str1, ty1), items''))
            else isPermutation (items', items'')
      in
        isSubtype (items1, items2) orelse isPermutation (items1, items2)
      end
    | subtype _ = false


  (* HW6 Crowdsourced Tests from Piazza @243 *)

  fun testMsg t1 t2 = "subtype " ^ tos t1 ^ " " ^ tos t2
 
  val tau1 = Record [("a", Bool), ("b", Unit)]
  val tau2 = Record [("a", Bool)]
  val tau3 = Record [("a", Record [("c", Unit), ("d", Bool)]), ("b", Unit)]
  val tau4 = Record [("a", Record [("d", Bool)])]
  val tau5 = Function (tau2, tau3)
  val tau6 = Function (tau1, tau4)
  val tau7 = Ref tau1
  val tau8 = Ref (Record [("b", Unit), ("a", Bool)])
  val tau9 = Ref tau2

  val _ = Check.assertT (subtype (Unit, Unit), "subtype self")
  val _ = Check.assertT (subtype (tau1, tau2), testMsg tau1 tau2)
  val _ = Check.assertT (subtype (tau3, tau4), testMsg tau3 tau4)
  val _ = Check.assertT (subtype (tau5, tau6), testMsg tau5 tau6)
  val _ = Check.assertT (subtype (tau7, tau8), testMsg tau7 tau8)
  val _ = Check.assertF (subtype (tau7, tau9), testMsg tau7 tau9)
  val _ = Check.assertF (subtype (tau9, tau7), testMsg tau9 tau7)
  val _ = Check.assertF (subtype (tau3, tau1), testMsg tau3 tau1)
  val _ = Check.assertF (subtype (tau3, tau2), testMsg tau3 tau2)
	

  val sub = Record [("a", Bool), ("b", Record [("c", Bool), ("d", Unit)])]
  val super = Record [("b", Record [("c", Bool)]),("a", Bool)]
  val sub2 = Record [("a", Bool)]
  val sub3 = Record [("b", Record [("c", Bool)])]
  val f1 = Function (Record [("a", Bool)], Record [("c", Bool), ("d", Bool), ("e", Bool)])
  val f2 = Function (Record [("a", Bool), ("b", Bool)], Record [("c", Bool), ("d", Bool)])


  val _ = Check.expect(true, subtype (sub, super), "type1")
  val _ = Check.expect(true, subtype (sub, sub), "type2")
  val _ = Check.expect(true, subtype (super, super), "type3")
  val _ = Check.expect (true, subtype (sub, sub2), "type4")
  val _ = Check.expect (true, subtype (sub, sub3), "type5")
  val _ = Check.expect (false, subtype (sub2, sub), "type6")
  val _ = Check.expect (false, subtype (sub3, sub), "type6")
  val _ = Check.expect (false, subtype (super, sub), "type6")
  val _ = Check.expect (true, subtype (f1, f2), "type7")		 
  

  (* more tests *)
    val type00 = Record [("a", Bool)]
    val type01 = Record [("a", Bool), ("b", Unit)]
    val type02 = Record [("a", Bool), ("c", Unit)]
    val type03 = Record [("a", Bool), ("b", Unit), ("c", Function (Bool, Bool))]
    val type04 = Record [("b", Unit), ("a", Bool)]
    val type05 = Bool (* lol why not *)
    val type06 = Ref Bool
    val type07 = Function (Bool, Unit)
    val type08 = Function (type01, type01)
    val type09 = Function (type00, type01)
    val type10 = Function (type01, type03)
    val type11 = Function (type00, type03)
    val type12 = Ref Unit
    val type13 = Ref type00
    val type14 = Ref type01
    val type15 = Ref type04

    val $ = subtype
    (* records *)
    val _ = Check.assertT($(type00, type00), "00 <: 00" ) (* record reflexivity *)
    val _ = Check.assertF($(type00, type01), "00 !<: 01")
    val _ = Check.assertT($(type01, type00), "01 <: 00" ) (* simple subtype *)
    val _ = Check.assertF($(type01, type02), "01 !<: 02") (* different labels *)
    val _ = Check.assertT($(type03, type01), "03 <: 01" )
    val _ = Check.assertT($(type03, type00), "03 <: 00" ) (* record transitivity *)
    val _ = Check.assertF($(type01, type03), "01 !<: 03")
    val _ = Check.assertT($(type01, type04), "01 <: 04" ) (* permutation *)
    val _ = Check.assertT($(type04, type01), "04 <: 01" ) (* permutation *)
    (* functions *)
    val _ = Check.assertT($(type09, type08), "09 <: 08" ) (* input supertype *)
    val _ = Check.assertF($(type08, type09), "08 !<: 09") (* sanity *)
    val _ = Check.assertT($(type10, type08), "10 <: 08" ) (* output subtype *)
    val _ = Check.assertT($(type11, type08), "11 <: 08" ) (* input & output *)
    (* refs *)
    val _ = Check.assertT($(type06, type06), "06 <: 06" ) (* reflexivity *)
    val _ = Check.assertF($(type06, type12), "06 !<: 06") (* ref bool ; ref unit *)
    val _ = Check.assertF($(type13, type14), "13 !<: 14") (* one-way subtype *)
    val _ = Check.assertT($(type14, type15), "14 <: 15" ) (* permutation *)
    (* disparate types - for sanity *)
    val _ = Check.assertF($(type06, type05), "06 !<: 05") (* ref bool ; bool *)
    val _ = Check.assertF($(type00, type13), "00 !<: 13") (* record ; ref *)
    val _ = Check.assertF($(type12, type10), "12 !<: 10") (* ref ; function *)

end
