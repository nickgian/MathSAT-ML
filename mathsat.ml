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


(** Definition of msat_model_iterator structure *)
type msat_model_iterator
let msat_model_iterator : msat_model_iterator structure typ = 
  structure "msat_model_iterator"
let repr = field msat_model_iterator "repr" (ptr void)
let () = seal msat_model_iterator

(** Definition of msat_proof_manager structure *)
type msat_proof_manager
let msat_proof_manager : msat_proof_manager structure typ = 
  structure "msat_proof_manager"
let repr = field msat_proof_manager "repr" (ptr void)
let () = seal msat_proof_manager

(** Definition of msat_proof structure *)
type msat_proof
let msat_proof : msat_proof structure typ = 
  structure "msat_proof"
let repr = field msat_proof "repr" (ptr void)
let () = seal msat_proof

(** Enviroment Creation *)
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

  (** returns the data type for an array of given index and element type
   *  in the given env*)
  let msat_get_array_type = 
    foreign "msat_get_array_type" 
      (msat_env @-> msat_type @-> msat_type @-> returning msat_type)

  (** returns the data type for float of given exp and mantissa width 
   * in the given env *)
  let msat_get_fp_type =
    foreign "msat_get_fp_type" 
      (msat_env @-> size_t @-> size_t @-> returning msat_type)

  (** returns the type for float rounding modes in the given env*)
  let msat_get_fp_roundingmode_type =
    foreign "msat_get_fp_roundingmode_type"
      (msat_env @-> returning msat_type)

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

  (** Returns a term representing the logical AND of t1 and t2. *)
  let msat_make_and = 
    foreign "msat_make_and" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns a term representing the logical negation of t1. *)
  let msat_make_not = 
    foreign "msat_make_not" 
      (msat_env @-> msat_term @-> returning msat_term)

  (** Returns a term representing the equivalence of t1 and t2. *)
  let msat_make_equal = 
    foreign "msat_make_equal" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns an atom representing (t1 <= t2). *)
  let msat_make_leq = 
    foreign "msat_make_leq" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns an atom representing (t1 + t2). *)
  let msat_make_plus = 
    foreign "msat_make_plus" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns an atom representing (t1 * t2). *)
  let msat_make_times = 
    foreign "msat_make_times" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns an expression representing (floor t). *)
  let msat_make_floor = 
    foreign "msat_make_floor" 
      (msat_env @-> msat_term @-> returning msat_term)

  (** Returns an atom representing an integer or rational number. *)
  let msat_make_number = 
    foreign "msat_make_number" 
      (msat_env @-> string @-> returning msat_term)

  (** Returns an expression representing a term if-then-else construct. *)
  let msat_make_ite = 
    foreign "msat_make_ite" 
      (msat_env @-> msat_term @-> msat_term @-> msat_term @-> returning msat_term)

  (** Creates a constant from a declaration. *)
  let msat_make_constant = 
    foreign "msat_make_constant" 
      (msat_env @-> msat_decl @-> returning msat_term)

  (** Creates an array read operation.*)
  let msat_make_array_read = 
    foreign "msat_make_array_read" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Creates an array write operation. *)
  let msat_make_array_write = 
    foreign "msat_make_array_write" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns an expression representing a bit-vector number.*)
  let msat_make_bv_number = 
    foreign "msat_make_bv_number" 
      (msat_env @-> string @-> size_t @-> size_t @-> returning msat_term)

  (** Returns a term representing the concatenation of t1 and t2.*)
  let msat_make_bv_concat = 
    foreign "msat_make_bv_concant" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns a term representing the selection of t[msb:lsb]. *)
  let msat_make_bv_extract = 
    foreign "msat_make_bv_extract" 
      (msat_env @-> size_t @-> size_t @-> msat_term @-> returning msat_term)

  (** Returns a term representing the bit-wise OR of t1 and t2. *)
  let msat_make_bv_or = 
    foreign "msat_make_bv_or" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns a term representing the bit-wise AND of t1 and t2. *)
  let msat_make_bv_and = 
    foreign "msat_make_bv_and" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns a term representing the bit-wise XOR of t1 and t2. *)
  let msat_make_bv_xor = 
    foreign "msat_make_bv_xor" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns a term representing the bit-wise negation of t *)
  let msat_make_bv_not = 
    foreign "msat_make_bv_not" 
      (msat_env @-> msat_term @-> returning msat_term)

  (** Returns a term representing the logical left shift of t1 by t2. *)
  let msat_make_bv_lshl = 
    foreign "msat_make_bv_lshl" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns a term representing the logical right shift of t1 by t2. *)
  let msat_make_bv_lshr = 
    foreign "msat_make_bv_lshr" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns a term representing the arithmetic right shift of t1 by t2 *)
  let msat_make_bv_ashr = 
    foreign "msat_make_bv_ashr" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns a term representing the zero extension of t. *)
  let msat_make_bv_zext = 
    foreign "msat_make_bv_zext" 
      (msat_env @-> size_t @-> msat_term @-> returning msat_term)

  (** Returns a term representing the sign extension of t1 by amount.  *)
  let msat_make_bv_sext = 
    foreign "msat_make_bv_sext" 
      (msat_env @-> size_t @-> msat_term @-> returning msat_term)

  (** Returns a term representing the addition of t1 and t2.  *)
  let msat_make_bv_plus = 
    foreign "msat_make_bv_plus" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns a term representing the subtraction of t1 by t2. *)
  let msat_make_bv_minus = 
    foreign "msat_make_bv_minus" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns a term representing the negation of t, te two's-complement. *)
  let msat_make_bv_neg = 
    foreign "msat_make_bv_neg" 
      (msat_env @-> msat_term @-> returning msat_term)

  (** Returns a term representing the multiplication of t1 and t2.  *)
  let msat_make_bv_times = 
    foreign "msat_make_bv_times" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns a term representing the unsigned division of t1 and t2.  *)
  let msat_make_bv_udiv = 
    foreign "msat_make_bv_udiv" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns a term representing the unsigned remainder of t1 and t2.  *)
  let msat_make_bv_urem = 
    foreign "msat_make_bv_urem" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns a term representing the signed division of t1 and t2.  *)
  let msat_make_bv_sdiv = 
    foreign "msat_make_bv_sdiv" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns a term representing the unsigned t1 < t2.  *)
  let msat_make_bv_ult = 
    foreign "msat_make_bv_ult" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns a term representing the unsigned t1 <= t2.  *)
  let msat_make_bv_uleq = 
    foreign "msat_make_bv_uleq" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns a term representing the signed t1 < t2.  *)
  let msat_make_bv_slt = 
    foreign "msat_make_bv_slt" 
      (msat_env @-> msat_term @-> msat_term @-> returning msat_term)

  (** Returns a term representing the signed t1 <= t2.  *)
  let msat_make_bv_sleq = 
    foreign "msat_make_bv_sleq" 
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

  (** Pushes a checkpoint for backtracking in an environment.*)
  let msat_push_backtrack_point =
    foreign "msat_push_backtrack_point" (msat_env @-> returning int)

  (** Backtracks to the last checkpoint set in the environment e. *)
  let msat_pop_backtrack_point =
    foreign "msat_pop_backtrack_point" (msat_env @-> returning int)

  (** returns the number of backtrack points in the given environment *)
  let msat_num_backtrack_points =
    foreign "msat_num_backtrack_points" (msat_env @-> returning int)

  (** Resets an environment. *)
  let msat_reset_env =
    foreign "msat_reset_env" (msat_env @-> returning int)

  (** Adds a logical formula to an environment. *)
  let msat_assert_formula =
    foreign "msat_assert_formula" (msat_env @-> msat_term @-> returning int)

  (** Adds a Boolean variable at the end of the list of preferred variables for
   *  branching when solving the problem. *)
  let msat_add_preferred_for_branching =
    foreign "msat_add_preferred_for_branching"
      (msat_env @-> msat_term @-> returning int)

  (** Clears the list of preferred variables for branching. *)
  let msat_clear_preferred_for_branching =
    foreign "msat_clear_preferred_for_branching" (msat_env @-> returning int)

  (** Checks the satiafiability of the given environment. *)
  let msat_solve =
    foreign "msat_solve" (msat_env @-> returning msat_result)

  (** Returns a string with search statistics for the given environment. *)
  let msat_get_search_stats =
    foreign "msat_get_search_stats" (msat_env @-> returning string)
end

