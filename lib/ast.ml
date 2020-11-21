
type value =
  | Int of int [@printer fun fmt -> fprintf fmt "%d"]
  | Bool of bool [@printer fun fmt -> fprintf fmt "%b"]
  | Nil [@printer fun fmt () -> fprintf fmt "Nil"]
  | Cons of t * t [@printer fun fmt (a,b) -> fprintf fmt "(%s, %s)" (show a) (show b)]
[@@deriving show {with_path = false}]

and bin_op = (value -> value -> value) [@@deriving show]

and t =
  | If of t * t * t
  | Let of char * t * t
  | BinOp of bin_op * t * t
  | Zero of t
  | Neg of t
  | Var of char
  | Val of value [@printer fun fmt v -> fprintf fmt "%s" (show_value v)]
[@@deriving show]

let int_bin_op f str v1 v2 =
  match v1, v2 with
  | Int i1, Int i2 -> Int (f i1 i2)
  | _ -> failwith ("only int values allowed: "  ^ str)

let cmp int_cmp bool_cmp str v1 v2 =
  match v1, v2 with
  | Int i1, Int i2 -> Bool (int_cmp i1 i2)
  | Bool b1, Bool b2 -> Bool (bool_cmp b1 b2)
  | _, _ -> failwith ("can only compare values of same type "  ^ str)

let (<+>) = int_bin_op (+) "plus"
let (<->) = int_bin_op (-) "minus"
let (<*>) = int_bin_op ( * ) "mult"
let (</>) = int_bin_op (/) "div"

let (<=>) = cmp (=) (=) "equal"
let (<>>) = cmp (>) (>) "gt"
let (<<>) = cmp (<) (<) "lt"

let rec eval_env ast env =
  match ast with
  | BinOp (f,e1,e2) -> begin
      match eval_env e1 env, eval_env e2 env with
      | Val v1, Val v2 -> Val (f v1 v2)
      | _ -> failwith "invalid binop"
    end
  | If (pred,e1,e2) -> begin
      let eval_pred = eval_env pred env in
      match eval_pred with
      | Val(Bool true) -> eval_env e1 env
      | Val(Bool false) -> eval_env e2 env
      | _ -> failwith "if predicate not bool"
    end
  | Var c -> begin
      match Env.get c env with
      | Some v -> Val v
      | None -> failwith @@ Printf.sprintf "%c not in environment" c
    end
  | Val _ as v -> v
  | Zero e -> begin
      match eval_env e env with
      | Val(Int v) -> Val (Bool (v = 0))
      | _ -> failwith "Zero not int expression"
    end
  | Neg e -> begin
      match eval_env e env with
      | Val(Int i) -> Val(Int (-i))
      | _ -> failwith "Neg not int"
    end
  | Let (c,v,body) -> begin
    match eval_env v env with
    | Val a -> eval_env body (Env.extend c a env)
    | _ -> failwith "let value not value"
  end

let eval ast =
  match eval_env ast Env.empty with
  | Val v -> v
  | _ -> failwith "error"

  
