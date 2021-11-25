(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
contexte zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code.

 *)

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)
let from_int x = []

(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
 *)
let to_int bA = 0

(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let print_b bA = ()

(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(* #install_printer print_b *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 otherwise.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
 *)
let rec compare_n nA nB = 10

(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>!) nA nB = true

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<!) nA nB = true

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) nA nB = true

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB = true

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 otherwise.
    @param bA A bitarray.
    @param bB A bitarray.
*)
let compare_b bA bB = 10

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB = true

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<) bA bB = true

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) bA bB = true

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB = true


(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA = 10

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA = []

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a = if a < 2 then 0 else 1

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = if a = 1 || a = 3 then 1 else 0

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (_quot_t a, _mod_t a)

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)
let add_n nA nB = []

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB = []

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b bA bB = []

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA bB = []

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec shift bA d = []

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let mult_b bA bB = []

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
let quot_b bA bB =  []

(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)
let mod_b bA bB = []

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB = ([], [])
