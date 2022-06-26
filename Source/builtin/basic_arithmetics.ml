(** Basic arithmetics with built-in integers *)

open Builtin

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integers
    @param b non-zero integer
*)
let rec gcd a b = if b > a then gcd b a else
    let rec euh a b = if b = 0 then a*sign a else
        let (_,r) = div a b in
        euh b r
    in euh a b;;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let bezout a b = let p = gcd a b in
  let rec euh r r2 u u2 v v2 = if r2 = p then (u2,v2,p) else
      euh r2 (modulo r r2) u2 (u - (quot r r2)*u2) v2 (v - (quot r r2)*v2)
  in euh a b 1 0 0 1;;
