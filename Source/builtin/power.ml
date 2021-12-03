(** Power function implementations for builtin integers *)

open Builtin
open Basic_arithmetics

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n =
  let rec recpow x n =
  if n = 0 then
    1
  else x * recpow x (n-1)
in recpow x n;;

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n =
  let rec recpower x n = match n  with
  |0 -> 1
  |n when n mod 2 = 0 -> recpower (x * x) (n / 2)
  |_ -> x * recpower x (n-1)
in recpower x n;;

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m =
  let rec recmod_power xrec nrec =
    if n <= nrec then xrec
    else recmod_power (modulo (xrec*x) m) (nrec+1)
  in recmod_power 1 0;;

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p =
 if x= 0 then 0
 else
   mod_power x (modulo n (p-1)) p ;;
