open Mathsat

let main = 
  let cfg = Enviroment.msat_create_config () in
  let ctx = Enviroment.msat_create_env cfg in
  let a = Term.msat_make_true ctx in
  let b = Solver.msat_assert_formula ctx a in
    match Solver.msat_solve ctx with
        Sat -> Printf.printf "sat!\n"
      | Unsat -> Printf.printf "unsat..\n"
      | Unknown -> Printf.printf " i dunno\n"
