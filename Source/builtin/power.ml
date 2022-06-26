(** Power function implementations for built-in integers *)

open Builtin;;
open Basic_arithmetics;;


(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let rec pow x n = if n = 0 then 1
    else x*(pow x (n-1));;

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let rec power x n = match n with
    1 -> x
  |0 -> 1
  |n when n mod 2 = 1 -> x*(power x (n-1))
  |_ -> power (x*x) (n/2);;


(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)

let mod_power x n m = if x = 0 then 0 else
  let rec euh x n m = match n with
      1 -> modulo x m
    |0 -> 1
    |n when n mod 2 = 1 -> modulo (x*(euh x (n-1) m)) m
    |_ -> euh (modulo (x*x) m) (n/2) m
  in euh x n m;;

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p =
  if modulo x p = 0 then mod_power x n p else
    let n = modulo n (p-1) in mod_power x n p;;
