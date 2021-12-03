(** Generating primes *)

open Builtin
open Basic_arithmetics

(** List composed of 2 and then odd integers starting at 3.
    @param n limit of list of odd integers, minimum value is 2.
 *)
let init_eratosthenes n =
  let rec erarec e =
    if e = n+1 then []
    else if e=2 then 2::erarec (e+1)
    else if modulo e 2 <> 0 then e::erarec (e+1)
    else erarec (e+1)
  in erarec 2 ;;

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
 *)

let rec remove_nth n l1 l2 = match l1 with
  |[] -> l2
  | e1::l when (modulo e1 n) = 0 && e1 <> n -> remove_nth n l l2
  | e1::l -> e1::remove_nth n l l2;;

let eratosthenes n = if n < 0 then
                       invalid_arg "Error : n must be a natural"
                     else
                       let rec erastrec n i l = if i*i > n then
                                             l
                                           else
                                             erastrec n (i+1) (remove_nth i l [])
                       in erastrec n 3 (init_eratosthenes n);;

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =
  let oc = open_out file in
  let rec aux = function
    |[] -> close_out oc
    |e::l -> Printf.fprintf oc "%s\n" e; aux l
  in aux li;;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = write_list (eratosthenes n) file;;


(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in
  _create_list in_c

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = create_list (open_in file);;

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime = []

(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let primes = eratosthenes limit in
  let rec test primes =match primes with
      |[] -> []
      |e::l -> if e = 2 then
                    test l
                  else if isprime (e+2) then
                    (e,(e+2)):: test l
                  else
                    test l
  in test primes;;
