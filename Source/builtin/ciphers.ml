(** Ciphers
    Built-in integer based ciphers.
*)

open Builtin
open Basic_arithmetics
open Power

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 256.
 *)
let rec encrypt_cesar k m b = match m with
    [] -> []
  |e::m when e+k > b -> (e+k-b)::(encrypt_cesar k m b)
  |e::m -> (e+k)::(encrypt_cesar k m b);;

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 256.
 *)
let rec decrypt_cesar k m b = match m with
    [] -> []
  |e::m when e-k < 0 -> (b+e-k)::(decrypt_cesar k m b)
  |e::m -> (e-k)::(decrypt_cesar k m b);;
;;

(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
let generate_keys_rsa p q =
  let n = p*q in
  let r = (p-1)*(q-1) in
  let rec sel n =
    let d = Random.int(n) in
    let b = gcd d r and b1 = gcd d n in
    if b = 1 && b1 = 1 then d else sel n in
  let d = sel r in
  let (e,_,_) = bezout d r
  in ((n,e),(n,d));;

(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = prime_mod_power m e n;;

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = prime_mod_power m d n;;

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let public_data_g p = (p-2,p);;

(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
  let a = Random.int(p) in
  (prime_mod_power g a p,a);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let k = Random.int(p) in
  (prime_mod_power g k p, msg*(prime_mod_power kA k p));;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) = quot msgB (prime_mod_power msgA a p);;
