type 'a node =
  { x : 'a
  ; mutable next : 'a node option
  }

type 'a t =
  { head : 'a node option
  ; tail : 'a node option
  ; size : int
  }

let empty = { head = None; tail = None; size = 0 }

let push x { head; tail; size } =
  let new_node = Some { x; next = None } in
  match tail with
  | None -> { head = new_node; tail = new_node; size = size + 1 }
  | Some tail ->
    tail.next <- new_node;
    { head; tail = new_node; size = size + 1 }
;;

let pop { head; tail; size } =
  match head with
  | None -> None, { head; tail; size }
  | Some { x; next } ->
    ( Some x
    , (match next with
       | None -> empty
       | head -> { head; tail; size = size - 1 }) )
;;

let peek { head; _ } =
  match head with
  | None -> None
  | Some { x; _ } -> Some x
;;

let size { size; _ } = size
let is_empty t = size t = 0
