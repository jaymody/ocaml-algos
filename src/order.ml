module type Comparable = sig
  type t

  val compare : t -> t -> int
end

type ordering =
  | Eq
  | Gt
  | Lt

let cmp a b =
  let diff = compare a b in
  if diff < 0 then Lt else if diff > 0 then Gt else Eq
;;
