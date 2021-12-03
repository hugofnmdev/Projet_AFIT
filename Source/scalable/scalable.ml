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

let from_int x = if x=0 then [] else
  let rec recfrom_int x= match x with
    |0->[]
    |x-> (x mod 2)::(recfrom_int (x/2))
  in if x>0 then 0::(recfrom_int x) else 1::(recfrom_int (-x));;

(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
 *)

let rec power x n =
  if n = 0 then 1
  else if n=1 then x
  else if n mod 2 = 0 then power (x*x)  (n/ 2)
  else x * power (x*x) ((n-1)/ 2);;

let to_int bA = let i= match bA with
  |[]->1
  |e::l->if e =0 then
           1
         else -1
                in let rec to_int_rec bA i2 = match bA with
                     |[]-> 0
                     |e::l->(to_int_rec l (i2+1))+(power 2 i2)*e
                   in let i3= i*(to_int_rec bA 0)in i3/2;;

(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let print_b bA =
  let rec print bA = match bA with
    |[]->()
    |e::l->print l;
           print_int e;
  in print bA;;

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
let rec length l= match l with
  |[]->0
  |e::l->1+length l;;

let rec compare_n nA nB=
  if length nA > length nB then 1
  else if length nB>length nA then -1 else
  let rec reccompare nA nB acc = match (nA,nB) with
    |([],[])-> acc
    |(e::l,[])->1
    |([],e::l)->(-1)
    |(e::l,e1::l1)-> if e1>e then reccompare l l1 (-1)
      else if e1<e then reccompare l l1 1
      else reccompare l l1 acc
  in reccompare nA nB 0;;

(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>!) nA nB = if compare_n nA nB =1 then true else false;;
(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<!) nA nB = if compare_n nA nB=(-1) then true else false;;

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) nA nB = compare_n nA nB =1||compare_n nA nB=0 ;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB = if compare_n nA nB =1||compare_n nA nB=0 then false else true;;

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 otherwise.
    @param bA A bitarray.
    @param bB A bitarray.
*)
let compare_b bA bB = match (bA,bB) with
  |([],[]) -> 0
  |(e::l,[])-> if e = 1 then -1 else 1
  |([],e::l)-> if e = 0 then -1 else 1
  |(e::l),(e1::l1)->if e = e1 then
      match e with
      |0-> compare_n l l1
      |_->(compare_n l l1)*(-1)
    else if e<e1 then 1
    else -1;;

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB = match compare_b bA bB with
  |(1)-> true
  |_-> false;;

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<) bA bB = match compare_b bA bB with
  |(-1)-> true
  |_-> false;;

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) bA bB = match compare_b bA bB with
  |1|0-> true
  |_-> false;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB = match compare_b bA bB with
  |(1)-> false
  |_->true;;

(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA = match bA with
  |[]->1
  |e::l->if e=0 then 1 else -1;;

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA = match bA with
  |[]->[]
  |e::l-> 0::l;;

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
let add_n nA nB =
  let rec recadd nA nB acc= match (nA,nB) with
    |([],[])->if acc=1 then [1] else []
    |([],e::l)->if e+acc>1 then 0::(recadd [] l 1) else (acc+e)::l
    |(e::l,[])->if e+acc>1 then 0::(recadd l [] 1) else (acc+e)::l
    |(e::l,e1::l1) when e+e1+acc<=1 ->e+e1+acc::(recadd l l1 0)
    |(e::l,e1::l1) when e+e1+acc>1 -> if e1+e+acc=2 then 0::(recadd l l1 1)
      else 1::(recadd l l1 1)
    |(_,_)->[]
  in recadd nA nB 0;;

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
let mod_b bA bB =
  if (mult_b bB (quot_b bA bB)) =bA then
    []
  else
    (diff_b bA (mult_b (quot_b bA bB)bB));;

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB = ((quot_b bA bB),(mod_b bA bB))
