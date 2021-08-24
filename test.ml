let value_testable = Alcotest.testable Lib.pp_value Lib.equal_value

let test name prog value =
  Alcotest.(
    test_case name `Quick (fun () ->
        check value_testable "same_value" value Lib.(of_string prog |> eval)))

let test_int name prog value = test name prog (Int value)
let test_bool name prog value = test name prog (Bool value)

let () =
  let open Alcotest in
  run "Progs"
    [
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
          test_int "let1" "let x = 4 in +(x,x)" 8;
          test_int "let2" "let x = 4 in let x = 2 in +(x,x)" 4;
          test_int "unpack"
            "unpack a b c = cons(1,cons(2,cons(3,nil))) in +(a,+(b,c))" 6;
          test_int "unpack2" "unpack a b c = list(1,2,3) in +(a,+(b,c))" 6;
          test_int "unpack3"
            "let p = proc (x) list(x,x) in unpack a b = (p 2) in +(a,b)" 4;
          test_int "proc" "let square = proc (x) *(x,x) in (square 2)" 4;
          test_int "proc2" "let square = proc (x,y) *(x,y) in (square 2 3)" 6;
          test_int "let*" "let* a=1 b=2 c=3 in +(a,+(b,c))" 6;
          test_int "letrec0" "letrec in 4" 4;
          test_int "letrec1" "letrec f (x) = +(x,x) in 4" 4;
          test_int "letrec2" "letrec f (x) = +(x,x) in (f 2)" 4;
          test_int "letrec3"
            "letrec fac (x) = if zero?(x) then 1 else *(x, (fac -(x,1))) in \
             (fac 4)"
            24;
          test_int "letrec4"
            "letrec double(x) = if zero?(x) then 0 else +((double -(x,1)), 2) \
             in (double 6)"
            12;
          test_int "letrec4" "letrec plus(x,y) = +(x,y) in (plus 1 100)" 101;
          test_int "letrec5"
            "let double = proc(x) letrec double(x,y) = if zero?(x) then y else \
             (double -(x,1) +(y,2)) in (double x 0) in (double 10)"
            20;
          test_bool "letrec6"
            "letrec even(x) = if zero?(x) then true else (odd -(x,1)) odd(x) = \
             if zero?(x) then false else (even -(x,1)) in (even 100)"
            true;
        ] );
    ]
