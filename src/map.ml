open Compare

(*
   Symbol table (i.e. dictionary or map) implementation using an AVL tree.

   Put, get, and pop are all O(log n) operations.
*)
module type Map = sig
  type key
  type 'a t

  val empty : 'a t
  val put : key -> 'a -> 'a t -> 'a t
  val get : key -> 'a t -> 'a option
  val pop : key -> 'a t -> 'a option * 'a t
  val size : 'a t -> int
  val is_empty : 'a t -> bool
end

module Make (Key : Comparable) : Map with type key = Key.t = struct
  module AvlTree = Avl.MakeKV (Key)

  type key = AvlTree.key
  type 'a t = 'a AvlTree.t

  let empty = AvlTree.empty
  let put k v t = AvlTree.upsert k v t
  let get k t = AvlTree.find k t
  let pop k t = AvlTree.pop k t
  let size t = AvlTree.size t
  let is_empty t = size t = 0
end
