type 'a t = (string * 'a) list [@@derivng show, eq]

let empty = []

let extend k v t = (k, v) :: t

let rec get k t =
  match t with
  | [] -> None
  | (a, v) :: tl -> if a = k then Some v else get k tl
