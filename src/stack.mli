type 'a t

val empty : 'a t
val push : 'a -> 'a t -> 'a t
val pop : 'a t -> 'a option * 'a t
val peek : 'a t -> 'a option
val size : 'a t -> int
val is_empty : 'a t -> bool
