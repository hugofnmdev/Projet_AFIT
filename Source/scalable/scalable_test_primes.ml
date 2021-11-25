(** Testing for primality *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Deterministic primality test *)
let is_prime n = true

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested bitarray
    @param testSeq sequence of bitarrays againt which to test
 *)
let is_pseudo_prime p test_seq = true
