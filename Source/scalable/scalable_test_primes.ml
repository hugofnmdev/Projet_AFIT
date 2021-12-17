(** Testing for primality *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Deterministic primality test *)
let is_prime n =
  let rec primerec d =
    if (>>) (mult_b d d) n then
      true
    else if (mod_b n d) = [] then
      false
    else primerec (add_b d [0;1])
  in n=[0;0;1] || primerec [0;0;1];;

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested bitarray
    @param testSeq sequence of bitarrays againt which to test
*)
let is_pseudo_prime p test_seq =
  let rec pseudorec i =
    if (>>) (mult_b i i) p then
      false
    else
      (if (mod_b p i) = [] then
         true
       else
         pseudorec (add_b i [0;1]))
  in let rec pseudobis = function
      |[] -> true
      |e::l when mod_power p (diff_b e [0;1]) e = [0;1] && pseudorec [0;0;1] -> false
      |e::l -> pseudobis l
  in pseudobis test_seq;;
