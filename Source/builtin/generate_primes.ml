(** Generating primes *)

open Builtin
open Basic_arithmetics

(** List composed of 2 and then odd integers starting at 3.
    @param n number of elements in the list of integers.
 *)

let init_eratosthenes n = if n < 2 then [] else
    let rec euh n i = if i > n then [] else i::(euh n (i+2))
    in 2::(euh n 3);;

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
let eratosthenes n = if n < 2 then [] else
    let rec vroum x l = match l with
        [] -> []
      |e::l when modulo e x = 0 -> vroum x l
      |e::l -> e::(vroum x l) in
    let rec euh n l = match l with
        [] -> []
      |e::l -> let l = vroum e l in e::(euh n l)
    in euh n (init_eratosthenes n);;

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =
  let  write  filename  list =
    let oc = open_out filename in
    let rec aux = function[] -> close_out oc
      |e::l -> Printf.fprintf  oc "%s\n" e; aux l
    in aux list in
  let rec euh l =
    match l with
        [] -> []
      |e::l -> (string_of_int(e))::(euh l)
  in write file (euh li);;

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
  with End_of_file -> None;;

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in
  _create_list in_c;;

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file =
  let ic = open_in file in
  let s = create_list ic in
  close_in ic;s;;

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t;;

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t;;

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)

let double_primes limit isprime =
  let rec euh primes = match primes with
      [] -> []
    |e::primes -> if isprime (2*e+1) then (e,2*e+1)::(euh primes) else euh primes
  in euh (eratosthenes limit);;

(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let rec euh primes = match primes with
      [] -> []
    |e::primes -> if isprime (e+2) then (e,e+2)::(euh primes) else euh primes
  in euh (eratosthenes limit);;
