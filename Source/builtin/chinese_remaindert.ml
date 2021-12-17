(** Chinese remainder theorem *)

open Builtin
open Basic_arithmetics

(** Image of the Chinese Remainder map
    @param x positive integer of which you take image
    @param l list of pairwise relatively prime positive integers.
 *)
let crt_image x l =
  let rec crt x l = match l with
  |[] -> []
  |h::l2 -> (modulo x h)::(crt x l2) in crt x l;;

(** Inverse image of Chinese Remainder map
    @para m a positive integer
    @param l list of pairwise relatively prime factors of m
    @param y list of remainders modulo pairwise relatively prime factors of m
 *)
let crt_solver m l y = match (l, y) with
  |(x1::x2::_,y1::y2::_) -> (let (u,v, gcd) = bezout x1 x2
                             in modulo (y1*v*x2 + y2*u*x1) m)
  |_-> failwith "?";;
