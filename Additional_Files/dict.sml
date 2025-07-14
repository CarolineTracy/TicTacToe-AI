
structure TreeDict : DICT =
struct

  fun log2 (n : int) : int = 
      case n of 
          0 => 0 (* hack *)
        | 1 => 1
        | _ => 1 + log2 (n div 2)

  datatype ('k, 'v) tree =
      Empty
    | Node of ('k, 'v) tree * ('k * 'v) * ('k, 'v) tree

  type ('k,'v) dict = ('k, 'v) tree 

  val empty = Empty

  fun size t =
        case t of
            Empty => 0
          | Node(l,_,r) => 1 + size l + size r
      
  fun insert (cmp, d, (k, v)) =
    case d of
      Empty => Node (empty, (k,v), empty)
    | Node (L, (k', v'), R) =>
      case cmp (k,k') of
        EQUAL => Node (L, (k, v), R)
      | LESS => Node (insert (cmp, L, (k, v)), (k', v'), R)
      | GREATER => Node (L, (k', v'), insert (cmp, R, (k, v)))

  fun lookup (cmp, d, k) =
    case d of
      Empty => NONE
    | Node (L, (k', v'), R) =>
      case cmp (k,k') of
        EQUAL => SOME v'
      | LESS => lookup (cmp, L, k)
      | GREATER => lookup (cmp, R, k)

  fun toString (kvts, d) =
      case d of
          Empty => ""
        | Node(l,kv,r) => toString (kvts, l) ^ " " ^ kvts kv ^ " " ^ toString (kvts, r)

  fun lookup' (cmp : 'k * 'k -> order, d, k) = case (lookup (cmp, d, k)) of NONE => raise Fail "key not found in dictionary" | SOME v => v
      
  (* Purpose: Given a compare function cmp (that compares two 'k values), a sorted ('k, 'v) dict t, and a key k,
     splitAt returns: the tree of everything less than k, the tree of everything greater than k, and the value stored with k (if k was in t)
  *)
  (* splitAt pasted from HW09 *)
  fun splitAt (cmp : ('k * 'k -> order), t : ('k, 'v) dict, k : 'k) : ('k, 'v) dict * ('k, 'v) dict * 'v option =
    case t of
      Empty => (Empty, Empty, NONE)
      | Node(l,(kk,vv),r) => (case cmp(k, kk) of
                                  EQUAL => (l, r, SOME vv)
                                  | LESS => (let
                                                val (ls_l, ls_r, ls_k) = splitAt(cmp, l, k)
                                             in
                                                (case (lookup(cmp, t, k)) of
                                                    NONE => (ls_l, Node(ls_r, (kk, vv), r), NONE)
                                                    | SOME x => (ls_l, Node(ls_r, (kk, vv), r), SOME x)
                                                )
                                             end)
                                  | GREATER => (let
                                                  val (rs_l, rs_r, rs_k) = splitAt(cmp, r, k)
                                                  val lookup_k = (lookup(cmp, t, k))
                                                in
                                                  (case lookup_k of
                                                    NONE => (Node(l, (kk, vv), rs_l), rs_r, NONE)
                                                    | SOME x => (Node(l, (kk, vv), rs_l), rs_r, SOME x)
                                                  )
                                                end)
                                 )

  (* merge' (renamed merge in HW09) pasted from HW09 *)
  fun merge' (cmp, combine, d1, d2) = 
    case d1 of
      Empty => d2
      | Node(l, (k, v), r) => (let
                                  val (less_x_d2, more_x_d2, x_exists_d2) = splitAt(cmp, d2, k)
                                  val (less_x_d2_recurse, more_x_d2_recurse) = (merge'(cmp, combine, l, less_x_d2), merge'(cmp, combine, r, more_x_d2))
                               in
                                  case x_exists_d2 of
                                    NONE => Node(less_x_d2_recurse, (k, v), more_x_d2_recurse)
                                    | SOME y => Node(less_x_d2_recurse, (k, combine(v, y)), more_x_d2_recurse)
                               end
                              )


  (* optimize inserts: if merging with a 1-element dictionary, insert instead, because it only needs to walk down one path of the tree *)

  fun insertWith (cmp : 'k * 'k -> order, c : 'v * 'v -> 'v, d : ('k,'v) dict, (k : 'k, v : 'v)) : ('k,'v) dict =
    case d of
      Empty => Node (empty, (k,v), empty)
    | Node (L, (k', v'), R) =>
      case cmp (k,k') of
          EQUAL => Node (L, (k, (c(v,v'))), R)
        | LESS => Node (insertWith (cmp, c, L, (k, v)), (k', v'), R)
        | GREATER => Node (L, (k', v'), insertWith (cmp, c, R, (k, v)))

  fun merge (cmp : 'k * 'k -> order, c : 'v * 'v -> 'v, d1 : ('k,'v) dict , d2 : ('k,'v) dict) : ('k,'v) dict = 
      case d1 of
          Node(Empty, kv1, Empty) => insertWith (cmp, c, d2, kv1)
        | _ => case d2 of
                 Node(Empty, kv2, Empty) => insertWith (cmp, c, d1, kv2)
               | _ => merge' (cmp, c, d1,d2)

  (* toSeq pasted from HW09 *)
  fun toSeq d = 
    case d of
      Empty => Seq.empty()
      | Node(l, (k,v), r) => (let
                                val l_seq = toSeq(l)
                                val r_seq = toSeq(r)
                                val x_and_r_seq = Seq.cons((k,v), r_seq)
                              in
                                Seq.append(l_seq, x_and_r_seq)
                              end)

  fun map (f, d) = 
      case d of
          Empty => Empty
        | Node(l,(k,v),r) => Node (map (f, l) , (k, f v) , map (f, r))

                     
end

structure Dict :> DICT = TreeDict
