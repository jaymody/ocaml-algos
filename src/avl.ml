open Compare

(*
   An AVL tree (self-balancing binary search tree) that let's you store
   key-value pairs.

   Note: Keys need not be unique. That is, multiple nodes in the tree can have
   the same key. This can be avoided by using "upsert" exclusively instead of
   "insert". Insert will add the key-value pair, even if the key already exists
   in the tree. Upsert will update the value of a node if the key already
   exists, else it will insert it.

   There are three different modules you can choose to make:

   1. Make: This version exposes all internals. This is useful if you have a
   specific use case where you need access to the underlying node data (i.e. to
   perform a specifc type of traversal).

   2. MakeKV: This module hides the internals. Unless you need access to the
   internals, it is better to use MakeKV over Make, as the API surface is
   reduced and thus more robust (no potential to accidentally unbalance the
   tree). A tree of this module is always guarenteed to be height balanced
   (in the Make version, the user can corrupt the tree, say by modifying the
   height value).

   3. MakeJustV: This module exposes a version of the AVL implementation that
   only has keys (i.e. there is no value associated the key). This is useful
   if you want to implementation a Set or an ordered list.
*)

module Make (Key : Comparable) = struct
  type key = Key.t

  type 'a node =
    { l : 'a t
    ; k : key
    ; v : 'a
    ; r : 'a t
    ; h : int
    ; s : int
    }

  and 'a t =
    | Empty
    | Node of 'a node

  let cmp a b =
    let diff = Key.compare a b in
    if diff < 0 then Lt else if diff > 0 then Gt else Eq
  ;;

  let empty = Empty

  let size = function
    | Empty -> 0
    | Node { s; _ } -> s
  ;;

  let height = function
    | Empty -> 0
    | Node { h; _ } -> h
  ;;

  let leaf k v = Node { l = Empty; k; v; r = Empty; h = 1; s = 1 }

  let bal node =
    let update n =
      { n with h = 1 + max (height n.l) (height n.r); s = 1 + size n.l + size n.r }
    in
    let rotl n r = update { r with l = Node (update { n with r = r.l }) } in
    let rotr n l = update { l with r = Node (update { n with l = l.r }) } in
    let node =
      match node.l, node.r with
      | l, Node ({ r = Node rr; _ } as r) when rr.h > height l -> rotl node r
      | l, Node ({ l = Node rl; _ } as r) when rl.h > height l -> rotl node (rotr r rl)
      | Node ({ r = Node lr; _ } as l), r when lr.h > height r -> rotr node (rotl l lr)
      | Node ({ l = Node ll; _ } as l), r when ll.h > height r -> rotr node l
      | _ -> node
    in
    Node (update node)
  ;;

  let insert k v t =
    let rec aux = function
      | Empty -> leaf k v
      | Node n ->
        (match cmp k n.k with
         | Lt | Eq -> bal { n with l = aux n.l }
         | Gt -> bal { n with r = aux n.r })
    in
    aux t
  ;;

  let upsert k v t =
    let rec aux = function
      | Empty -> leaf k v
      | Node n ->
        (match cmp k n.k with
         | Eq -> bal { n with v }
         | Lt -> bal { n with l = aux n.l }
         | Gt -> bal { n with r = aux n.r })
    in
    aux t
  ;;

  let pop k t =
    let rec pop_successor n =
      match n.l with
      | Empty -> (n.k, n.v), n.r
      | Node l ->
        let successor, l = pop_successor l in
        successor, bal { n with l }
    in
    let rec aux = function
      | Empty -> None, Empty
      | Node n ->
        (match cmp k n.k with
         | Eq ->
           ( Some n.v
           , (match n.r with
              | Empty -> n.l
              | Node r ->
                let (k, v), r = pop_successor r in
                bal { n with k; v; r }) )
         | Lt ->
           let e, l = aux n.l in
           e, bal { n with l }
         | Gt ->
           let e, r = aux n.r in
           e, bal { n with r })
    in
    aux t
  ;;

  let find k t =
    let rec aux = function
      | Empty -> None
      | Node n ->
        (match cmp k n.k with
         | Eq -> Some n.v
         | Lt -> aux n.l
         | Gt -> aux n.r)
    in
    aux t
  ;;

  let rec get_min = function
    | Empty -> None
    | Node { l = Empty; k; v; _ } -> Some (k, v)
    | Node { l; _ } -> get_min l
  ;;

  let rec pop_min = function
    | Empty -> None, Empty
    | Node { l = Empty; k; v; r; _ } -> Some (k, v), r
    | Node n ->
      let e, l = pop_min n.l in
      e, bal { n with l }
  ;;

  let rec get_max = function
    | Empty -> None
    | Node { k; v; r = Empty; _ } -> Some (k, v)
    | Node { r; _ } -> get_max r
  ;;

  let rec pop_max = function
    | Empty -> None, Empty
    | Node { l; k; v; r = Empty; _ } -> Some (k, v), l
    | Node n ->
      let e, r = pop_max n.r in
      e, bal { n with r }
  ;;

  let to_list t =
    let rec aux acc = function
      | Empty -> acc
      | Node { l; k; v; r; _ } -> aux ((k, v) :: aux acc r) l
    in
    aux [] t
  ;;

  let rec calc_height = function
    | Empty -> 0
    | Node { l; r; _ } -> 1 + max (calc_height l) (calc_height r)
  ;;

  let rec calc_size = function
    | Empty -> 0
    | Node { l; r; _ } -> 1 + calc_size l + calc_size r
  ;;

  let rec is_height_balanced = function
    | Empty -> true
    | Node { l; r; h; s; _ } as node ->
      is_height_balanced l
      && is_height_balanced r
      && abs (calc_height l - calc_height r) < 2
      && h = calc_height node
      && s = calc_size node
  ;;
end

module MakeKV (Key : Comparable) : sig
  type key = Key.t
  type 'a t

  val empty : 'a t
  val size : 'a t -> int
  val insert : key -> 'a -> 'a t -> 'a t
  val upsert : key -> 'a -> 'a t -> 'a t
  val pop : key -> 'a t -> 'a option * 'a t
  val find : key -> 'a t -> 'a option
  val to_list : 'a t -> (key * 'a) list
  val get_min : 'a t -> (key * 'a) option
  val pop_min : 'a t -> (key * 'a) option * 'a t
  val get_max : 'a t -> (key * 'a) option
  val pop_max : 'a t -> (key * 'a) option * 'a t
end = struct
  include Make (Key)
end

module MakeJustV (Key : Comparable) : sig
  type key = Key.t
  type t

  val empty : t
  val size : t -> int
  val insert : key -> t -> t
  val upsert : key -> t -> t
  val remove : key -> t -> bool * t
  val remove_exn : key -> t -> t
  val mem : key -> t -> bool
  val to_list : t -> key list
  val get_min : t -> key option
  val pop_min : t -> key option * t
  val get_max : t -> key option
  val pop_max : t -> key option * t
end = struct
  module AvlT = Make (Key)

  type key = Key.t

  (* In order to use the AVL tree without the value field, we assign it
     a dummy type and ignore it *)
  type _dummy_t
  type t = _dummy_t option AvlT.t

  let empty = AvlT.empty
  let size = AvlT.size
  let insert k t = AvlT.insert k None t
  let upsert k t = AvlT.upsert k None t

  let remove k t =
    let x, t = AvlT.pop k t in
    Option.is_some x, t
  ;;

  let remove_exn k t =
    match AvlT.pop k t with
    | None, _ -> invalid_arg "key not in tree"
    | _, t -> t
  ;;

  let mem k t = AvlT.find k t |> Option.is_some
  let to_list t = AvlT.to_list t |> List.map fst
  let get_min t = AvlT.get_min t |> Option.map fst

  let pop_min t =
    let out, t = AvlT.pop_min t in
    let x = Option.map fst out in
    x, t
  ;;

  let get_max t = AvlT.get_max t |> Option.map fst

  let pop_max t =
    let out, t = AvlT.pop_max t in
    let x = Option.map fst out in
    x, t
  ;;
end
