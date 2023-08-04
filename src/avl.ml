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

  (* Stores left, key, value, right, height, size *)
  type 'a t =
    | Empty
    | Node of 'a t * key * 'a * 'a t * int * int

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
end = struct
  type key = Key.t

  type 'a t =
    | Empty
    | Node of 'a t * key * 'a * 'a t * int * int

  let empty = Empty

  let size = function
    | Empty -> 0
    | Node (_, _, _, _, _, n) -> n
  ;;

  let create l k v r =
    let height = function
      | Empty -> 0
      | Node (_, _, _, _, h, _) -> h
    in
    let make (l, k, v, r) =
      Node (l, k, v, r, 1 + max (height l) (height r), 1 + size l + size r)
    in
    let rotate_left (k, v, l) (rl, rk, rv, rr) = make (l, k, v, rl), rk, rv, rr in
    let rotate_right (k, v, r) (ll, lk, lv, lr) = ll, lk, lv, make (lr, k, v, r) in
    make
      (match l, r with
       | _, Node (rl, rk, rv, rr, _, _) when height rr > height l ->
         rotate_left (k, v, l) (rl, rk, rv, rr)
       | _, Node ((Node (rll, rlk, rlv, rlr, _, _) as rl), rk, rv, rr, _, _)
         when height rl > height l ->
         rotate_right (rk, rv, rr) (rll, rlk, rlv, rlr) |> rotate_left (k, v, l)
       | Node (ll, lk, lv, (Node (lrl, lrk, lrv, lrr, _, _) as lr), _, _), _
         when height lr > height r ->
         rotate_left (lk, lv, ll) (lrl, lrk, lrv, lrr) |> rotate_right (k, v, r)
       | Node (ll, lk, lv, lr, _, _), _ when height ll > height r ->
         rotate_right (k, v, r) (ll, lk, lv, lr)
       | _ -> l, k, v, r)
  ;;

  let insert k' v' t =
    let rec aux = function
      | Empty -> create Empty k' v' empty
      | Node (l, k, v, r, _, _) ->
        (match cmp k' k with
         | Lt | Eq -> create (aux l) k v r
         | Gt -> create l k v (aux r))
    in
    aux t
  ;;

  let upsert k' v' t =
    let rec aux = function
      | Empty -> create Empty k' v' empty
      | Node (l, k, v, r, _, _) ->
        (match cmp k' k with
         | Eq -> create l k v' r
         | Lt -> create (aux l) k v r
         | Gt -> create l k v (aux r))
    in
    aux t
  ;;

  let remove k' t =
    let rec pop_successor l k v r =
      match l with
      | Empty -> (k, v), r
      | Node (ll, lk, lv, lr, _, _) ->
        let succesor, l = pop_successor ll lk lv lr in
        succesor, create l k v r
    in
    let rec aux = function
      | Empty -> None, Empty
      | Node (l, k, v, r, _, _) ->
        (match cmp k' k with
         | Eq ->
           ( Some v
           , (match r with
              | Empty -> l
              | Node (rl, rk, rv, rr, _, _) ->
                let (k, v), r = pop_successor rl rk rv rr in
                create l k v r) )
         | Lt ->
           let e, l = aux l in
           e, create l k v r
         | Gt ->
           let e, r = aux r in
           e, create l k v r)
    in
    aux t
  ;;

  let find k' t =
    let rec aux = function
      | Empty -> None
      | Node (l, k, v, r, _, _) ->
        (match cmp k' k with
         | Eq -> Some v
         | Lt -> aux l
         | Gt -> aux r)
    in
    aux t
  ;;

  let to_list t =
    let rec aux acc = function
      | Empty -> acc
      | Node (l, k, v, r, _, _) -> aux ((k, v) :: aux acc r) l
    in
    aux [] t
  ;;
end
