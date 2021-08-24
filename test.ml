let type_testable = Alcotest.testable Lib.pp_typ Lib.equal_typ
let value_testable = Alcotest.testable Lib.pp_value Lib.equal_value

let test name prog value =
  Alcotest.(
    test_case name `Quick (fun () ->
        check value_testable "same_value" value Lib.(of_string prog |> eval)))

let test_int name prog value = test name prog (Int value)
let test_bool name prog value = test name prog (Bool value)

let t_test name prog typ =
  Alcotest.(
    test_case name `Quick (fun () ->
        check type_testable "type_check" typ Lib.(of_string prog |> type_check)))

let type_int name prog = t_test name prog IntT
let type_bool name prog = t_test name prog BoolT

let () =
  let open Alcotest in
  run "Progs"
    [
      ( "type",
        [
          type_int "simple int" "1";
          type_bool "simple bool" "true";
          t_test "simple proc" "proc ((x:int)) true" (Arrow (IntT, BoolT));
          type_int "add" "+(1,1)";
          type_bool "zero" "zero?(1)";
          type_bool "equal" "equal?(1,2)";
          type_int "if" "if true then 1 else 2";
          type_bool "if2" "if true then true else false";
          type_int "let" "let (a:int) = 2 in +(a,a)";
        ] );
      ( "eval",
        [
          test_int "add" "+(1,2) " 3;
          test_int "minus" "+(1,-2)" ~-1;
          test_int "sub" "-(1,2)" ~-1;
          test_int "mul" "*(2,2)" 4;
          test_int "div" "/(2,2)" 1;
          test_bool "zero_t" "zero?(0)" true;
          test_bool "zero_f" "zero?(1)" false;
          test_bool "eq_1" "equal?(1,1)" true;
          test_bool "eq_2" "equal?(1,0)" false;
          test_bool "eq_3" "equal?(true,true)" true;
          test_bool "eq_4" "equal?(true,false)" false;
          test_bool "less_1" "less?(1,2)" true;
          test_bool "less_2" "less?(1,0)" false;
          test_bool "greater_1" "greater?(1,2)" false;
          test_bool "greater_2" "greater?(1,0)" true;
          test "cons" "cons(1,2)" (Cons (Int 1, Int 2));
          test_int "car" "car(cons(1,2))" 1;
          test_int "cdr" "cdr(cons(1,2))" 2;
          test "list" "list(1,2)" (Cons (Int 1, Cons (Int 2, Nil)));
          test_bool "if1" "if zero?(0) then true else false" true;
          test_bool "if2" "if zero?(1) then true else false" false;
          test_int "if3" "if zero?(1) then +(1,2) else -(1,2)" ~-1;
          test_int "if4" "if zero?(0) then +(1,2) else -(1,2)" 3;
          test_int "let1" "let (x : int) = 4 in +(x,x)" 8;
          test_int "let2" "let (x:int) = 4 in let (x:int) = 2 in +(x,x)" 4;
          test_int "unpack"
            "unpack (a:int) (b:int) (c:int) = cons(1,cons(2,cons(3,nil))) in \
             +(a,+(b,c))"
            6;
          test_int "unpack2"
            "unpack (a:int) (b:int) (c:int) = list(1,2,3) in +(a,+(b,c))" 6;
          test_int "unpack3"
            "let (p : (int -> int)) = proc ((x:int)) list(x,x) in unpack \
             (a:int) (b:int) = (p 2) in +(a,b)"
            4;
          test_int "proc"
            "let (square : int -> int) = proc ((x:int)) *(x,x) in (square 2)" 4;
          test_int "proc2"
            "let (square: int -> int) = proc ((x:int),(y:int)) *(x,y) in \
             (square 2 3)"
            6;
          test_int "let*" "let* (a:int)=1 (b:int)=2 (c:int)=3 in +(a,+(b,c))" 6;
          test_int "letrec0" "letrec in 4" 4;
          test_int "letrec1" "letrec int f ((x:int)) = +(x,x) in 4" 4;
          test_int "letrec2" "letrec int f ((x:int)) = +(x,x) in (f 2)" 4;
          test_int "letrec3"
            "letrec int fac ((x:int)) = if zero?(x) then 1 else *(x, (fac \
             -(x,1))) in (fac 4)"
            24;
          test_int "letrec4"
            "letrec int double((x:int)) = if zero?(x) then 0 else +((double \
             -(x,1)), 2) in (double 6)"
            12;
          test_int "letrec4"
            "letrec int plus((x:int),(y:int)) = +(x,y) in (plus 1 100)" 101;
          test_int "letrec5"
            "let (double : int -> int)= proc((x:int)) letrec int \
             double((x:int),(y:int)) = if zero?(x) then y else (double -(x,1) \
             +(y,2)) in (double x 0) in (double 10)"
            20;
          test_bool "letrec6"
            "letrec int even((x:int)) = if zero?(x) then true else (odd \
             -(x,1)) int odd((x:int)) = if zero?(x) then false else (even \
             -(x,1)) in (even 100)"
            true;
        ] );
    ]
