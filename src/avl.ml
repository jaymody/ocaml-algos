open Compare

module Make (Key : Comparable) = struct
  type key = Key.t

  type 'a t =
    | Empty
    | Node of 'a t * key * 'a * 'a t * int

  let empty = Empty

  let height = function
    | Empty -> 0
    | Node (_, _, _, _, h) -> h
  ;;

  let create l k v r = Node (l, k, v, r, 1 + max (height l) (height r))

  let add k' v' t =
    let rec aux = function
      | Empty -> create Empty k' v' empty
      | Node (l, k, v, r, _) ->
        (match cmp k' k with
         | Eq -> create l k v' r
         | Lt -> create (aux l) k v r
         | Gt -> create l k v (aux r))
    in
    aux t
  ;;

  let remove k' t =
    let rec pop_successor = function
      | Empty -> invalid_arg "unreachable"
      | Node (Empty, k, v, r, _) -> (k, v), r
      | Node (l, k, v, r, _) ->
        let succesor, l = pop_successor l in
        succesor, create l k v r
    in
    let rec aux = function
      | Empty -> None, Empty
      | Node (l, k, v, r, _) ->
        (match cmp k' k with
         | Eq ->
           (match r with
            | Empty -> Some v, l
            | _ ->
              let (k, v), r = pop_successor r in
              Some v, create l k v r)
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
      | Node (l, k, v, r, _) ->
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
      | Node (l, k, v, r, _) -> aux ((k, v) :: aux acc r) l
    in
    aux [] t
  ;;
end
