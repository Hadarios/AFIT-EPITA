(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)
let is_prime n = if n = 2 then true else if modulo n 2 = 0 then false else
    let rec euh k n = match k with
        k when k*k > n -> true
      |k when modulo n k = 0 -> false
      |k -> euh (k+2) n
    in euh 3 n;;

(** Primality test based on smalle Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers againt which to test
 *)

let rec is_pseudo_prime p test_seq = match test_seq with
    [] -> true
  |e::test_seq when modulo e p = 0 -> is_pseudo_prime p test_seq
  |e::test_seq when mod_power e (p-1) p = 1 -> is_pseudo_prime p test_seq
  |_ -> false;;
