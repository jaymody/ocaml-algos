open Compare

(*
   An AVL tree (self-balancing binary search tree).

   Note, that there exists two functions to add key-value pairs into the tree,
   insert and upsert. Insert allows adding duplicate entries, while upsert will
   update the value of existing entries. You probably don't want to use these in
   conjuction, as it is not guarenteed what key-value pair upsert will update.

   The purpose of having both functions is so that we can reuse this same AVL
   tree implementation for different data structures. For example, a map
   (i.e. symbol table) can be implemented using only upsert. Likewise, a
   priority queue requires only the use of insert, since we a pq must allow
   duplicate entries.
*)
module Make (Key : Comparable) : sig
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

  (* Returns an empty tree, which is used for initialization. *)
  val empty : 'a t

  (* Returns the number of nodes in the tree. *)
  val size : 'a t -> int

  (* Inserts the key-value pair into the tree. Duplicates keys are allowed, in
     the event of two keys being equal, the new key-value pair will be added
     to the left of the existing entry or entries. However, as things are added
     and removed from the tree, it will rebalance so it is not guarenteed that
     newer insertions will always be to the left of older ones. *)
  val insert : key -> 'a -> 'a t -> 'a t

  (* Insert or update the key-value pair into the tree. This differs from insert
     because we do not allow duplicate keys, instead if the key already exists
     in the tree, we simply update it's value. The functions insert and upsert
     should not be used together, as there are no guarentees which key-value
     pair will be updated in the event of duplicates. *)
  val upsert : key -> 'a -> 'a t -> 'a t

  (* Removes the key-value pair from the tree. If there is a matching key, we
     return it's associated value inside a Some option (in addition to the new
     tree with the key-value pair removed). Otherwise, we return None and the
     unmodified tree. *)
  val remove : key -> 'a t -> 'a option * 'a t

  (* Finds the value associated with the key in the tree. If the value doesn't
     exist, we return None, else we return a Some of the value. *)
  val find : key -> 'a t -> 'a option

  (* Returns the key-value pairs in the tree in a list sorted in
     ascending order. *)
  val to_list : 'a t -> (key * 'a) list

  (* Returns the min key-value pair in the tree if it exists (else None). *)
  val get_min : 'a t -> (key * 'a) option

  (* Returns the min key-value pair from the tree and returns it. *)
  val remove_min : 'a t -> (key * 'a) option * 'a t

  (* Returns the max key-value pair in the tree if it exists (else None). *)
  val get_max : 'a t -> (key * 'a) option

  (* Returns the max key-value pair from the tree and returns it. *)
  val remove_max : 'a t -> (key * 'a) option * 'a t
end = struct
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

  let remove k t =
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

  let rec remove_min = function
    | Empty -> None, Empty
    | Node { l = Empty; k; v; r; _ } -> Some (k, v), r
    | Node n ->
      let e, l = remove_min n.l in
      e, bal { n with l }
  ;;

  let rec get_max = function
    | Empty -> None
    | Node { k; v; r = Empty; _ } -> Some (k, v)
    | Node { r; _ } -> get_max r
  ;;

  let rec remove_max = function
    | Empty -> None, Empty
    | Node { l; k; v; r = Empty; _ } -> Some (k, v), l
    | Node n ->
      let e, r = remove_max n.r in
      e, bal { n with r }
  ;;

  let to_list t =
    let rec aux acc = function
      | Empty -> acc
      | Node { l; k; v; r; _ } -> aux ((k, v) :: aux acc r) l
    in
    aux [] t
  ;;
end
