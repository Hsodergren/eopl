let () =
  let ast = Lib.of_string (Sys.argv.(1)) in
  Printf.printf "=== ast ===\n%s\n======\n" (Lib.show ast);
  Printf.printf "eval = %s" (Lib.show_value @@ Lib.eval ast)

