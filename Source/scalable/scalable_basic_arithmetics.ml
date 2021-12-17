(** Basic arithmetics for ordered euclidian ring, case of bitarrays. *)

open Scalable

(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let gcd_b a b =
  if compare_b b [] = 0 && compare_b a [] = 0 then
    []
  else
  if compare_b b [] = 0 then
    []
  else
    let rec audd a b = match b with
      |[] -> a
      |[1] -> a
      |_-> audd b (mod_b a b)
    in audd (abs_b a)(abs_b b);;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let bezout_b bA bB =
  if mult_b bA bB = [] then
    ([0;1],[0;1],add_b bA bB)
  else
    let rec vivecambon bC bD (u1,v1) (u2,v2)=
      let (q,r) = div_b bC bD
      in if compare_b r [] = 0 then
        if compare_b bA bB = 1 then
          (u2,v2,bD)
        else
          (v2,u2,bD)
      else
        vivecambon bD r (u2,v2) (diff_b u1 (mult_b q u2), diff_b v1 (mult_b q v2))
    in if compare_b bA bB=1 then
      vivecambon bA bB ([0;1],[0;0])([0;0],[0;1])
    else
      vivecambon bB bA([0;1],[0;0])([0;0],[0;1]);;
