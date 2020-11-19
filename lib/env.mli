
type 'a t

val empty: 'a t

val extend: char -> 'a -> 'a t -> 'a t

val get: char -> 'a t -> 'a option
