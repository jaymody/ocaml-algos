open Order

module type S = sig
  type key
  type 'a t

  (* Create an empty tree. *)
  val empty : 'a t

  (* Adds the key-value pair to the tree. If the key is already in the tree,
     the value is updated. Takes O(log2 n) time on average, O(n) in the worst
     case. *)
  val add : key -> 'a -> 'a t -> 'a t

  (* Removes the entry that matches the provided key from the tree, returning an
     option holding the removed value and the new tree with the entry removed.
     If the key is in the tree, None is returned (along with the original tree
     untouched). Takes O(log2 n) time on average, O(n) in the worst case. *)
  val remove : key -> 'a t -> 'a t * 'a option

  (* Finds the entry matching the given key and returns the associated value. If
     there is no match, returns None. Takes O(log2 n) time on average, O(n) in
     the worst case. *)
  val find : key -> 'a t -> 'a option

  (* Returns a list of the key-value pairs in the tree in ascending order.
     Takes O(n) time. *)
  val to_list : 'a t -> (key * 'a) list
end

module Make (Key : Comparable) = struct
  type key = Key.t

  type 'a t =
    | Empty
    | Node of
        { l : 'a t
        ; k : key
        ; v : 'a
        ; r : 'a t
        }

  let empty = Empty

  let add k' v' t =
    let rec aux = function
      | Empty -> Node { l = Empty; k = k'; v = v'; r = Empty }
      | Node { l; k; v; r } ->
        (match cmp k k' with
         | Eq -> Node { l; k; v = v'; r }
         | Lt -> Node { l = aux l; k; v; r }
         | Gt -> Node { l; k; v; r = aux r })
    in
    aux t
  ;;

  let remove k' t =
    let rec pop_left_successor = function
      | Empty -> invalid_arg "unreachable"
      | Node { l = Empty; k; v; r } -> (k, v), r
      | Node { l; k; v; r } ->
        let succesor, l = pop_left_successor l in
        succesor, Node { l; k; v; r }
    in
    let rec aux = function
      | Empty -> None, Empty
      | Node { l; k; v; r } ->
        (match cmp k k' with
         | Eq ->
           (* Replaces the deleted node with it's left successor (the smallest
              node to the right of this node). If the right node is Empty,
              meaning there is no left successor, we simply replace the current
              node with it's left subtree. *)
           (match r with
            | Empty -> Some v, l
            | _ ->
              let (k, v), r = pop_left_successor r in
              Some v, Node { l; k; v; r })
         | Lt ->
           let e, l = aux l in
           e, Node { l; k; v; r }
         | Gt ->
           let e, r = aux r in
           e, Node { l; k; v; r })
    in
    aux t
  ;;

  let find k' t =
    (* Binary search. *)
    let rec aux = function
      | Empty -> None
      | Node { l; k; v; r } ->
        (match cmp k k' with
         | Eq -> Some v
         | Lt -> aux l
         | Gt -> aux r)
    in
    aux t
  ;;

  let to_list t =
    (* In order traversal. *)
    let rec aux acc = function
      | Empty -> acc
      | Node { l; k; v; r } -> aux ((k, v) :: aux acc l) r
    in
    aux [] t
  ;;
end
