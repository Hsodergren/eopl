type env_val =
  | Value of value
  | Rec of {
      (* name : string; *)
      bound : id;
      body : t;
    }

and typ =
  | IntT
  | BoolT
  | Arrow of typ * typ

and value =
  | Int of int [@printer fun fmt -> fprintf fmt "%d"]
  | Bool of bool [@printer fun fmt -> fprintf fmt "%b"]
  | Proc of id * t * env_val Env.t [@printer fun fmt _ -> fprintf fmt "<fun>"]
  | Nil [@printer fun fmt () -> fprintf fmt "Nil"]
  | Cons of value * value
      [@printer
        fun fmt (a, b) -> fprintf fmt "(%s, %s)" (show_value a) (show_value b)]
[@@deriving show]

and bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | EQ
  | GT
  | LT
[@@deriving show]

and id = string * typ

and t =
  | If of t * t * t
  | Let of id * t * t
  | LetStar of (id * t) list * t
  | Unpack of (string * typ) list * t * t
  | BinOp of bin_op * t * t
  | LetRec of (typ * string * t) list * t
  | Procedure of id * t
  | ConsT of t * t
  | Apply of t * t
  | Zero of t
  | Car of t
  | Cdr of t
  | Null of t
  | Neg of t
  | Var of string
  | Val of value [@printer fun fmt v -> fprintf fmt "%s" (show_value v)]
[@@deriving show]

let equal_value v1 v2 =
  match (v1, v2) with
  | Proc _, Proc _ -> failwith "cannot compare functions"
  | v1, v2 -> v1 = v2

let rec eval_env ast env : value =
  (* print_endline @@ show ast; *)
  match ast with
  | BinOp (bop, e1, e2) -> eval_bop bop (eval_env e1 env) (eval_env e2 env)
  | Car e -> (
      match eval_env e env with
      | Cons (e, _) -> e
      | _ -> failwith "car only valid on cons")
  | Cdr e -> (
      match eval_env e env with
      | Cons (_, e) -> e
      | _ -> failwith "cdr only valid on cons")
  | Null e -> (
      match eval_env e env with
      | Nil -> Bool true
      | _ -> Bool false)
  | If (pred, e1, e2) -> (
      let eval_pred = eval_env pred env in
      match eval_pred with
      | Bool true -> eval_env e1 env
      | Bool false -> eval_env e2 env
      | _ -> failwith "if predicate not bool")
  | Var c -> (
      match Env.get c env with
      | Some (Value v) -> v
      | Some (Rec { bound; body; _ }) -> Proc (bound, body, env)
      | None -> failwith @@ Printf.sprintf "%s not in environment" c)
  | Procedure (c, body) -> Proc (c, body, env)
  | Apply (e1, e2) -> (
      match (eval_env e1 env, eval_env e2 env) with
      | Proc ((name, _typ), body, cap_env), v ->
          eval_env body (Env.extend name (Value v) cap_env)
      | _, _ -> failwith "first eval must be procedure")
  | Val s -> s
  | Zero e -> (
      match eval_env e env with
      | Int v -> Bool (v = 0)
      | _ -> failwith "Zero not int expression")
  | Neg e -> (
      match eval_env e env with
      | Int i -> Int (-i)
      | _ -> failwith "Neg not int")
  | Let ((c, _), v, body) ->
      eval_env body (Env.extend c (Value (eval_env v env)) env)
  | LetStar (((c, typ), t) :: tl, body) ->
      eval_env (LetStar (tl, Let ((c, typ), t, body))) env
  | LetStar ([], body) -> eval_env body env
  | LetRec (recs, body) ->
      let rec extend_env recs env =
        match recs with
        | (_, name, Procedure (bound, body)) :: tl ->
            let env = Env.extend name (Rec { bound; body }) env in
            extend_env tl env
        | [] -> env
        | _ -> failwith "letrec without procedure"
      in
      eval_env body (extend_env recs env)
  | Unpack (cs, t, body) -> (
      let t = eval_env t env in
      match (cs, t) with
      | [], Nil -> eval_env body env
      | (id, t) :: cs', Cons (e1, e2) ->
          eval_env (Unpack (cs', Val e2, Let ((id, t), Val e1, body))) env
      | [], Cons _ -> failwith "too many values in list"
      | _, Nil -> failwith "too many variables on left hand side"
      | _ -> failwith "Invalid value")
  | ConsT (t1, t2) -> Cons (eval_env t1 env, eval_env t2 env)

and eval_bop bop v1 v2 =
  match (bop, v1, v2) with
  | Add, Int i1, Int i2 -> Int (i1 + i2)
  | Sub, Int i1, Int i2 -> Int (i1 - i2)
  | Div, Int i1, Int i2 -> Int (i1 / i2)
  | Mul, Int i1, Int i2 -> Int (i1 * i2)
  | EQ, _, _ -> Bool (v1 = v2)
  | LT, Int i1, Int i2 -> Bool (i1 < i2)
  | GT, Int i1, Int i2 -> Bool (i1 > i2)
  | _ -> failwith "invalid binop"

let eval ast =
  match eval_env ast Env.empty with
  | v -> v
  | exception e -> failwith (Printexc.to_string e)
