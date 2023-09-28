open Compare

(*
   Priority queue implemented using an AVL tree.

   Push, pop, and peek are all O(log n) operations.
*)
module type Pq = sig
  type elt
  type queue

  val empty : queue
  val push : elt -> queue -> queue
  val pop : queue -> elt option * queue
  val peek : queue -> elt option
  val size : queue -> int
  val is_empty : queue -> bool
end

module Make (Key : Comparable) : Pq with type elt = Key.t = struct
  module AvlTree = Avl.Make (Key)

  type elt = AvlTree.key
  type queue = int AvlTree.t

  let empty = AvlTree.empty
  let push e q = AvlTree.insert e 0 q

  let pop q =
    let e, q = AvlTree.pop_min q in
    Option.map fst e, q
  ;;

  (* TODO: Peek can be made O(1) if we cache the min and store it one push/pop. *)
  let peek q = Option.map fst (AvlTree.get_min q)
  let size q = AvlTree.size q
  let is_empty q = size q = 0
end
