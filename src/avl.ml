open Compare

module Make (Key : Comparable) = struct
  type key = Key.t

  type 'a t =
    | Empty
    | Node of
        { l : 'a t
        ; k : key
        ; v : 'a
        ; r : 'a t
        ; h : int
        }

  let empty = Empty

  let height = function
    | Empty -> 0
    | Node { h; _ } -> h
  ;;

  (* Balance requires the pre-condition that the nodes below it have correctly
     labeled hieghts. The height of the current node need not be correct. *)
  let balance =
    let rotate_left = function
      | Node { l; k; v; r = Node { l = rl; k = rk; v = rv; r = rr; h = n2 }; _ } ->
        Node { l = Node { l; k; v; r = rl; h = n2 - 1 }; k = rk; v = rv; r = rr; h = n2 }
      | _ -> invalid_arg "cannot rotate this node"
    in
    let rotate_right = function
      | Node { l = Node { l = ll; k = lk; v = lv; r = lr; h = n2 }; k; v; r; _ } ->
        Node { l = ll; k = lk; v = lv; r = Node { l = lr; k; v; r; h = n2 - 1 }; h = n2 }
      | _ -> invalid_arg "cannot rotate this node"
    in
    function
    | Node { l; r = Node { r = rr; _ }; _ } as node when height rr > height l ->
      rotate_left node
    | Node { l; k; v; r = Node { l = rl; h = rh; _ } as r; _ } when height rl > height l
      -> rotate_left (Node { l; k; v; r = rotate_right r; h = rh })
    | Node { l = Node { r = lr; h = lh; _ } as l; k; v; r; _ } when height lr > height r
      -> rotate_right (Node { l = rotate_left l; k; v; r; h = lh })
    | Node { l = Node { l = ll; _ }; r; _ } as node when height ll > height r ->
      rotate_right node
    | Node { l; k; v; r; _ } -> Node { l; k; v; r; h = 1 + max (height l) (height r) }
    | Empty -> Empty
  ;;

  let add k' v' t =
    let rec aux = function
      | Empty -> Node { l = Empty; k = k'; v = v'; r = Empty; h = 1 }
      | Node { l; k; v; r; h } ->
        (match cmp k' k with
         | Eq -> Node { l; k; v = v'; r; h }
         | Lt -> balance (Node { l = aux l; k; v; r; h })
         | Gt -> balance (Node { l; k; v; r = aux r; h }))
    in
    aux t
  ;;

  let remove k' t =
    let rec pop_left_successor = function
      | Empty -> invalid_arg "unreachable"
      | Node { l = Empty; k; v; r; _ } -> (k, v), r
      | Node { l; k; v; r; h } ->
        let succesor, l = pop_left_successor l in
        succesor, balance (Node { l; k; v; r; h })
    in
    let rec aux = function
      | Empty -> None, Empty
      | Node { l; k; v; r; h } ->
        (match cmp k' k with
         | Eq ->
           (* Replaces the deleted node with it's left successor (the smallest
              node to the right of this node). If the right node is Empty,
              meaning there is no left successor, we simply replace the current
              node with it's left subtree. *)
           (match r with
            | Empty -> Some v, l
            | _ ->
              let (k, v), r = pop_left_successor r in
              Some v, balance (Node { l; k; v; r; h }))
         | Lt ->
           let e, l = aux l in
           e, balance (Node { l; k; v; r; h })
         | Gt ->
           let e, r = aux r in
           e, balance (Node { l; k; v; r; h }))
    in
    aux t
  ;;

  let find k' t =
    (* Binary search. *)
    let rec aux = function
      | Empty -> None
      | Node { l; k; v; r; _ } ->
        (match cmp k' k with
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
      | Node { l; k; v; r; _ } -> aux ((k, v) :: aux acc r) l
    in
    aux [] t
  ;;
end
