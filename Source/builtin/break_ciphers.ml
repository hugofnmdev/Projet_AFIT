(** Factoring Builtin int primes *)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
*)

let break key = 
	let rec cipher n p =
		if (modulo n p) = 0 then (p, quot n p)
		else cipher n (p+2)
	in let (n,_) = key in
		if (modulo n 2) = 0 then (2, quot n 2)
		else cipher n 3;; 
