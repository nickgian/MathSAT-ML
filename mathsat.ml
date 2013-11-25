open Ctypes
open Foreign

(** Definition of msat_config structure *)
type msat_config
let msat_config : msat_config structure typ = structure "msat_config"
let repr = field msat_config "repr" (ptr void)
let () = seal msat_config

(** Definition of msat_env structure *)
type msat_env
let msat_env : msat_env structure typ = structure "msat_env"
let repr = field msat_env "repr" (ptr void)
let () = seal msat_env

(** Definition of msat_term structure *)
type msat_term
let msat_term : msat_term structure typ = structure "msat_term"
let repr = field msat_term "repr" (ptr void)
let () = seal msat_term

(** Definition of msat_decl structure *)
type msat_decl
let msat_decl : msat_decl structure typ = structure "msat_decl"
let repr = field msat_decl "repr" (ptr void)
let () = seal msat_decl

(** Definition of msat_type structure *)
type msat_type
let msat_type : msat_type structure typ = structure "msat_type"
let repr = field msat_type "repr" (ptr void)
let () = seal msat_type

(** MathSat result *)
type validity = Unknown | Unsat | Sat
let validity_of_int = function
  -1 -> Unknown
  | 0 -> Unsat
  | 1 -> Sat
  | _ -> failwith "Only three values applicable to validity"

let int_of_validity = function
    Unknown -> -1
  | Unsat -> 0
  | Sat -> 1

type msat_result = validity
let msat_result = view (int) ~read:validity_of_int ~write:int_of_validity

(** MathSat truth value *)
type truth = Undef | False | True
let truth_of_int = function
  -1 -> Undef
  | 0 -> False
  | 1 -> True
  | _ -> failwith "Only three values applicable to truth"

let int_of_truth = function
    Undef -> -1
  | False -> 0
  | True -> 1

type msat_truth_value = truth
let msat_truth_value = view (int) ~read:truth_of_int ~write:int_of_truth

(* Enviroment Creation *)
module Enviroment =
struct
  (** Create a new MathSat configuration*)
  let msat_create_config =
    foreign "msat_create_config" (void @-> returning msat_config)

  (** Creates a new MathSAT configuration with the default settings 
      for the given logic.*)
  let msat_create_default_config =
    foreign "msat_create_default_config" (string @-> returning msat_config)

  (** Creates a new MathSat enviroment from the given configuration*)
  let msat_create_env =
    foreign "msat_create_env" (msat_config @-> returning msat_env)

  (** Create a new MathSat configuration*)
  let msat_create_config =
    foreign "msat_create_config" (void @-> returning msat_config)

  (** Destroys an enviroment *)
  let msat_destroy_env =
    foreign "msat_destroy_env" (msat_env @-> returning void)

  (** returns the data type for Booleans in the given env*)
  let msat_get_bool_type =
    foreign "msat_get_bool_type" (msat_env @-> returning msat_type)

  (** returns the data type for rationals in the given env*)
  let msat_get_rational_type =
    foreign "msat_get_rational_type" (msat_env @-> returning msat_type)

  (** returns the data type for integers in the given env*)
  let msat_get_integer_type =
    foreign "msat_get_integer_type" (msat_env @-> returning msat_type)

  (** returns the data type for bv in the given env*)
  let msat_get_bv_type =
    foreign "msat_get_bv_type" (msat_env @-> size_t @-> returning msat_type)
end

(** Term creation *)
module Term =
struct
  (** Returns a term representing logical truth. *)
  let msat_make_true =
    foreign "msat_make_true" (msat_env @-> returning msat_term)
  
  (** Returns a term representing logical falsity. *)
  let msat_make_false =
    foreign "msat_make_false" (msat_env @-> returning msat_term)

  (** Returns a term representing the equivalence of t1 and t2. *)
  let msat_make_iff =
    foreign "msat_make_iff" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)
  (** Returns a term representing the logical OR of t1 and t2. *)
  let msat_make_or = 
    foreign "msat_make_or" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)
end

(** Term access and navigation *)
module Navigation =
struct
  (** Checks whether t is the TRUE term. *)
  let msat_term_is_true =
    foreign "msat_term_is_true" (msat_env @-> msat_term @-> returning int)

  (** Checks whether t is the FALSE term. *) 
  let msat_term_is_false =
    foreign "msat_term_is_false" (msat_env @-> msat_term @-> returning int)
end

(** Problem solving *)
module Solver =
struct

  (** Adds a logical formula to an environment. *)
  let msat_assert_formula =
    foreign "msat_assert_formula" (msat_env @-> msat_term @-> returning int)

  (** Checks the satiafiability of the given environment. *)
  let msat_solve =
    foreign "msat_solve" (msat_env @-> returning msat_result)
end
