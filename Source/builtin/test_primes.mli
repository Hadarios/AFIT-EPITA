(** Testing for primality *)

(** Deterministic primality test
    @param n integer bigger or equal to 2.
*)
val is_prime : int -> bool
;;

(** Primality test based on smalle Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers againt which to test
 *)
val is_pseudo_prime : int -> (int list) -> bool
;;
