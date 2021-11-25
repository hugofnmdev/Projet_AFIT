(** Basic arithmetics for ordered euclidian ring, case of bitarrays. *)

open Scalable

(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let gcd_b bA bB = []

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let bezout_b bA bB = ([], [], [])
