(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)
let is_prime n = true

(** Primality test based on small Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers against which to test
 *)
let is_pseudo_prime p test_seq = true
