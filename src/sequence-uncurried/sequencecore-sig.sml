signature SEQUENCECORE =
sig
  type 'a seq

  exception Range of string

  (* length <x1, ..., xn> == n *)
  val length : 'a seq -> int
  (* nth (i, <x0, ..., x_n-1>) == xi if 0 <= i < n, raises Range otherwise *)
  val nth    : int * 'a seq -> 'a

  (* tabulate (f, n) == <f 0, ..., f n-1> *)
  val tabulate : (int -> 'a) * int -> 'a seq
  (* filter (p, <x1, ..., xn>) == <xi | p xi == true>
   * that is, filter p s computes the sequence of all elements si of s such that
   * p si == true, in the original order. *)
  val filter : ('a -> bool) * 'a seq -> 'a seq

  (* map (f, <x1, ..., xn>) == <f x1, ..., f xn> *)
  val map : ('a -> 'b) * 'a seq -> 'b seq
  (* reduce (op, b, <x1, ..., xn>) == x1 op x2 ... op xn,
   * that is, reduce applies the given function between all elements of the
   * input sequence, using b as the base case. *)
  val reduce : (('a * 'a) -> 'a) * 'a * 'a seq -> 'a

  datatype 'a lview = Nil | Cons of 'a * 'a seq
  datatype 'a tview = Empty | Leaf of 'a | Node of 'a seq * 'a seq

  val showl : 'a seq -> 'a lview
  val hidel : 'a lview -> 'a seq

  val showt : 'a seq -> 'a tview
  val hidet : 'a tview -> 'a seq
end
