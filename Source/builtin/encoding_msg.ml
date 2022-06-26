(** Encoding Strings *)

open Builtin
open Basic_arithmetics
open Power

let binary n = if n = 0|| n = 1 then n else
    let rec max2 k i n = if k > n then (k/2,i-1) else max2 (k*2) (i+1) n in
    let rec euh n (k,i) = match n with
        n when n < 2 -> n
      |n when n < k -> euh n (k/2,i-1)
      |n -> (power 10 i)+euh (n-k) (k/2,i-1)
    in euh n (max2 4 2 n);;

let bdecimal n = if n < 2 then n else
    let rec euh n k i = match n with
        0 -> 0
      |n -> let r = modulo n k in if r = 0 then euh n (k*10) (i+1)  else (power 2 i)+(euh (n-r) (k*10) (i+1))
    in euh n 10 0;;

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)

let encode str bits =
  let p = power 2 bits in
  let length = String.length(str) in
  let rec euh s b i = if i = length then 0 else
      bdecimal((binary (int_of_char s.[i])))*(power p (length-i-1))+(euh s b (i+1))
  in euh str bits 0;;

(** Decode a string containing ASCII characters. @param msg is an
    integer representing an encoded message. @param bits number of
    bits on which to store a character ; alphanumeric ASCII is 7. *)

let bchar n = Char.escaped(char_of_int((bdecimal n)));;

let decode msg bits =
  let bits = power 10 bits in
  let rec euh msg i n = match msg with
      0 when i = 0 -> ""
    |0 -> bchar n
    |msg when i = bits -> (euh msg 1 0)^(bchar n)
    |msg -> euh (msg/2) (i*10) (n+(msg mod 2)*i)
  in euh msg 1 0;;
