signature SEQUENCE =
sig
  include SEQUENCECORE

  (* mapreduce (l, e, n, s) == reduce (n, e, map (l, s)) *)
  val mapreduce : ('a -> 'b) * 'b * ('b * 'b -> 'b) * 'a seq -> 'b
  (* Intuitively toString computes the string representation of a sequence
   * in the same style as the string for a list, using the given function to
   * convert each element to a string. More precisely:
   * toString (elmToStr, <x1, ..., xn>) == "[" ^ elmToStr x1 ^ ", " ... ^ elmToStr xn ^ "]". *)
  val toString : ('a -> string) * 'a seq -> string

  (* repeat (n, x) == <x1, ..., xn> such that all xi == x *)
  val repeat  : int * 'a -> 'a seq

  (* zip (<x1, ..., xn>, <y1, ..., ym>) == <(x1, y1), ..., (xk, yk)> where
   * k = min(n,m). That is, zip truncates the longer sequence if needed *)
  val zip     : ('a seq * 'b seq) -> ('a * 'b) seq

  (* flatten ss == reduce append <> ss (the sequence analog of concat).
   * See below for the specification of append. *)
  val flatten : 'a seq seq -> 'a seq

  (* split (k, <x0,...xk-1,xk...xn>) == (<x0,...xk-1>, <xk,...xn>)
     (so the left result has length k)
     if the sequence has at least k elements

     or raises Range otherwise
  *)
  val split   : int * 'a seq -> 'a seq * 'a seq

  (* take (k, <x0,...xk-1,xk...xn> == <x0,...xk-1>)
     drop (k, <x0,...xk-1,xk...xn> == <xk,...xn>)
     if the sequence has at least k elements

     or raise Range otherwise
     *)
  val take    : int * 'a seq -> 'a seq
  val drop    : int * 'a seq -> 'a seq

  (* empty () == <> *)
  val empty : unit -> 'a seq
  (* cons (x1, <x2, ..., xn>) == <x1, ..., xn> *)
  val cons  : 'a * 'a seq -> 'a seq

  (* singleton x == <x> *)
  val singleton : 'a -> 'a seq
  (* append <x1, ..., xn> <y1, ..., ym> == <x1, ..., xn, y1, ..., ym> *)
  val append    : 'a seq * 'a seq -> 'a seq


  (* students: use these for testing only! they have bad work and completely ruin the span *)
  val tolist : 'a seq -> 'a list
  val fromlist : 'a list -> 'a seq

end
