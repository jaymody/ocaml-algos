type 'a t = 'a list * int

let empty = [], 0
let push e (l, n) = e :: l, n + 1

let pop = function
  | hd :: tl, n -> Some hd, (tl, n - 1)
  | t -> None, t
;;

let peek = function
  | hd :: _, _ -> Some hd
  | _ -> None
;;

let size (_, n) = n
let is_empty t = size t = 0
