(** Power function implementations for builtin integers *)

open Builtin
open Basic_arithmetics

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n = 0

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n = 0

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m = 0

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p = 0
