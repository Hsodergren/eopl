
type 'a t

val empty: 'a t

val extend: string -> 'a -> 'a t -> 'a t

val get: string -> 'a t -> 'a option
