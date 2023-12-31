module type Comparable = sig
  type t

  val compare : t -> t -> int
end

type comparison =
  | Eq
  | Gt
  | Lt
