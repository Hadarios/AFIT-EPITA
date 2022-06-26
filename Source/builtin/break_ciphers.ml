(** Factoring Built-In Int Primes *)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key = let (n,_) = key in
  let max = int_of_float(sqrt(float_of_int(n))) in
  let rec euh max = if modulo n max = 0 then max else euh (max-2) in
  let q = euh max in (q, quot n q);;
