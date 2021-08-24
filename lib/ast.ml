type typ =
  | IntT
  | BoolT
  | Arrow of typ * typ
[@@deriving eq, show]

type env_val =
  | Value of value
  | Rec of {
      (* name : string; *)
      bound : id;
      body : t;
    }

and value =
  | Int of int [@printer fun fmt -> fprintf fmt "%d"]
  | Bool of bool [@printer fun fmt -> fprintf fmt "%b"]
  | Proc of id * t * env_val Env.t [@printer fun fmt _ -> fprintf fmt "<fun>"]
  | Nil [@printer fun fmt () -> fprintf fmt "Nil"]
  | Cons of value * value
      [@printer
        fun fmt (a, b) -> fprintf fmt "(%s, %s)" (show_value a) (show_value b)]

and bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | EQ
  | GT
  | LT

and id = string * typ

and t =
  | If of t * t * t
  | Let of string * t * t
  | Unpack of string list * t * t
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

exception Type_err of t
let eq_type t1 t2 e = if equal_typ t1 t2 then () else raise (Type_err e)

let equal_value v1 v2 =
  match (v1, v2) with
  | Proc _, Proc _ -> failwith "cannot compare functions"
  | v1, v2 -> v1 = v2

let rec type_check ast tenv =
  match ast with
  | Var name -> (
      match Env.get name tenv with
      | Some t -> t
      | None -> failwith "unbound variable")
  | Val (Int _) -> IntT
  | Val (Bool _) -> BoolT
  | Val (Proc ((_, typ), t, _)) -> Arrow (typ, type_check t tenv)
  | Val _ -> failwith "only basic types and function for now, TODO lists"
  | BinOp (binop, t1, t2) -> type_check_binop binop t1 t2 tenv
  | Zero t ->
      eq_type (type_check t tenv) IntT t;
      BoolT
  | Let (name, exp, body) ->
      let env = Env.extend name (type_check exp tenv) tenv in
      type_check body env
  | If (e1, e2, e3) ->
      let tc t = type_check t tenv in
      let t1, t2, t3 = (tc e1, tc e2, tc e3) in
      eq_type t1 BoolT e1;
      eq_type t2 t3 ast;
      t2
  | Procedure ((name, typ), e) ->
      let e_typ = type_check e (Env.extend name typ tenv) in
      Arrow (typ, e_typ)
  | Apply (f_exp, op_exp) -> (
      let f_typ = type_check f_exp tenv in
      match f_typ with
      | IntT
      | BoolT ->
          failwith "type error, apply non function"
      | Arrow (t1, t2) ->
          let op_typ = type_check op_exp tenv in
          eq_type t1 op_typ op_exp;
          t2)
  | Unpack (_, _, _) -> failwith "todo"
  | LetRec (_, _)
  | ConsT (_, _)
  | Car _
  | Cdr _
  | Null _
  | Neg _ ->
      failwith "a"

and type_check_binop binop t1 t2 tenv =
  match binop with
  | Add
  | Mul
  | Div
  | Sub ->
      eq_type (type_check t1 tenv) IntT t1;
      eq_type (type_check t2 tenv) IntT t2;
      IntT
  | EQ ->
      let typ1 = type_check t1 tenv in
      eq_type typ1 (type_check t2 tenv) (BinOp (EQ, t1, t2));
      BoolT
  | LT
  | GT ->
      eq_type (type_check t1 tenv) BoolT t1;
      eq_type (type_check t2 tenv) BoolT t2;
      BoolT

let type_check ast = type_check ast Env.empty

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
  | Let (c, v, body) ->
      eval_env body (Env.extend c (Value (eval_env v env)) env)
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
      | id :: cs', Cons (e1, e2) ->
          eval_env (Unpack (cs', Val e2, Let (id, Val e1, body))) env
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
