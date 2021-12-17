(** Power function implementations for bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let pow x n =
  if n =[] then [0;1]else 
  if x =[] then [] else let rec powr x n = if n = [] then [0;1]
	  else mult_b x (powr x (diff_b n [0;1]))
	in powr x n;;

(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let power x n =
  let rec pwr x n=
  if compare_b n []=0 then [0;1]
  else if (compare_b n [0;1]=0) then x
  else if compare_b (mod_b n [0;0;1])([])=0 then
    pwr (mult_b x x) (quot_b n [0;0;1])
  else mult_b x (pwr(mult_b x x)(quot_b(diff_b n [0;1])[0;0;1]))
  in pwr x n;;
to_int (power (from_int 5) (from_int 2));;

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)
let mod_power x n m =
	if x = [] then
   []
 else
   let rec pow2 p x n =
	   match (p,x,n) with
      |(p,x,[]) -> p
      |(p,x,e::l) when e = 1 -> pow2 (mod_b (mult_b p x) m) (mod_b (mult_b x x) m)  l
	    |(p,x,e::l) -> pow2 p (mod_b (mult_b x x) m) l
   in match n with
   |[] -> [0;1]
   |e::l -> pow2 [0;1] x l;;

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p = if compare_b x [] = 0 then
    []
  else
    mod_power x (mod_b n(diff_b p [0;1])) p;;
