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
let rec purge l = match l with
    [] -> []
  |1::l -> 1::l
  |_::l -> purge l;; (*enleve 0 gauche*)

let neg nA = match nA with
    [] -> []
  |x::nA ->
    let x = (if x = 1 then 0 else 1) in
    let rec first nA i k = match nA with
        [] -> i
      |e::nA when e = 1 -> first nA k (k+1)
      |e::nA -> first nA i (k+1) in
    let rec euh nA i =
      if i = 0 then nA else
        (match nA with
            e::nA -> (if e=0 then 1 else 0)::(euh nA (i-1))
          |_ -> [] (*cas pour rendre exhaustivif*))
    in x::(euh nA (first nA 0 0));; (*complement a 2*)

let rec from_int x = if x = 0 then [] else
  let rec max x i k = if i > x then (i/2,k-1) else max x (i*2) (k+1) in
  let rec euh x (i,k) = match k with
      k when k<0 -> []
    |k when i > x -> 0::(euh x ((i/2),(k-1)))
    |k -> 1::(euh (x-i) ((i/2),(k-1)))
  in if x < 0 then neg(from_int(-x))
    else 0::(euh x (max x 1 0));;

(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
 *)
let rec to_int bA = match bA with
    [] -> 0
  |x::bA ->
    let rec length l = match l with
        [] -> 1
      |e::l -> 2*(length l) in
    let rec euh bA i = match bA with
        [] -> 0
      |e::bA -> (e*i)+(euh bA (i/2))
    in if x = 0 then euh bA ((length bA)/2)
      else -1*(to_int (neg (1::bA)));;

(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let rec print_b bA = match bA with
    []|[0] -> print_int(0);print_newline()
  |[1] -> print_int(1);print_newline()
  |e::bA -> print_int(e);print_b bA;;

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
let rec compare_n nA nB =
  let rec bigger nA nB = match (nA,nB) with
      ([],[]) -> true
    |([],_) -> false
    |(_,[]) -> true
    |(_::nA,_::nB) -> bigger nA nB in
  let rec euh nA nB = match (nA,nB) with
      ([],[]) -> 0
    |([],_) -> -1
    |(_,[]) -> 1
    |(e::nA,f::nB) when e>f -> if bigger nA nB then 1 else -1
    |(e::nA,f::nB) when f>e -> if bigger nB nA then -1 else 1
    |(_::nA,_::nB) -> euh nA nB
  in euh (purge nA) (purge nB);;

(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>!) nA nB = compare_n nA nB = 1;;

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<!) nA nB = compare_n nA nB = -1;;

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) nA nB = compare_n nA nB >= 0;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB = compare_n nA nB <= 0;;

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 otherwise.
    @param bA A bitarray.
    @param bB A bitarray.
*)
let compare_b bA bB = match (bA,bB) with
    [],[] -> 0
  |[], e::bB -> if e = 1 then 1 else -1
  |e::_, [] -> if e = 1 then 1 else -1
  |1::_,0::_ -> -1
  |0::_,1::_ -> 1
  |e::bA,f::bB -> if e = 1 then compare_n (neg bB) (neg bA) else compare_n bA bB;;

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB = compare_b bA bB = 1;;

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<) bA bB = compare_b bA bB = -1;;

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) bA bB = compare_b bA bB >= 0;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB = compare_b bA bB <= 0;;


(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA = match bA with
    [] -> 0
  | e::_ -> if e = 1 then -1 else 1;;

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA = if sign_b bA = 1 then bA else neg(bA);;

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a = if a < 2 then 0 else 1;;

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = if a = 1 || a = 3 then 1 else 0;;

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (_quot_t a, _mod_t a);;

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)
let rec reverse l l1 = match l with
    [] -> l1
  |e::l -> reverse l (e::l1);;

let add_n nA nB =
  let rec euh nA nB nC r = match (nA,nB) with
      [],[] -> if r = 1 then 1::nC else nC
    |e::nA,[] -> if e+r > 1 then euh nA nB ((e+r-2)::nC) 1 else euh nA nB ((e+r)::nC) 0
    |[],e::nB -> if e+r > 1 then euh nA nB ((e+r-2)::nC) 1 else euh nA nB ((e+r)::nC) 0
    |e::nA,f::nB -> if (e+r+f) > 1 then euh nA nB ((e+f+r-2)::nC) 1 else euh nA nB ((e+r+f)::nC) 0
  in (euh (reverse nA []) (reverse nB []) [] 0);;

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB =
  let rec euh nA nB nC r = match (nA,nB) with
      [],[] -> nC
    |e::nA,[] -> if e-r < 0 then euh nA nB ((e-r+2)::nC) 1 else euh nA nB ((e-r)::nC) 0
    |[],e::nB -> if e-r < 0 then euh nA nB ((e-r+2)::nC) 1 else euh nA nB ((e-r)::nC) 0
    |e::nA,f::nB -> if (e-f-r) < 0 then euh nA nB ((e-f-r+2)::nC) 1 else euh nA nB ((e-r-f)::nC) 0
  in (euh (reverse nA []) (reverse nB []) [] 0);;

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b bA bB = match (bA,bB) with
    [],[] -> []
  |[],_ -> bB
  |_,[] -> bA
  |e::bA,f::bB when e = 1 && f = 1 -> 1::(add_n bA bB)
  |e::bA,f::bB when e=0 && f = 1 -> let bB = neg(1::bB) and bA = 0::bA in
                                    if bB >>! bA then neg(diff_n bB bA)
                                    else diff_n bA bB
  |e::bA,f::bB when e=1 && f=0 -> let bA = neg(1::bA) and bB = 0::bB in
                                    if bA >>! bB then neg(diff_n bA bB)
                                    else diff_n bB bA
  |e::bA,f::bB -> 0::(add_n bA bB);;

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA bB = add_b bA (neg bB);;

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let shift bA d = if d = 0 then bA else
    let rec euh bA d = match bA with
        [] when d = 0 -> []
      |[] -> 0::(euh bA (d-1))
      |e::bA -> e::(euh bA d)
    in euh bA d;;

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let mult_b bA bB =
  let rec euh bA bB i = match bB with
      [] -> []
    |e::bB when e = 1 -> add_n (shift bA i) (euh bA bB (i+1))
    |e::bB -> euh bA bB (i+1)
  in match (bA,bB) with
      [],_ -> []
    |_,[] -> []
    |e::bA,f::bB when e = 0 && f = 0 -> 0::(euh bA (reverse bB []) 0)
    |e::bA,f::bB when e = 1 && f = 0 -> 1::(neg(euh (neg bA) (reverse bB []) 0))
    |e::bA,f::bB when e = 0 && f = 1 -> 1::(neg(euh bA (reverse (neg (1::bB)) []) 0))
    |e::_,f::_ -> 0::(euh (neg bA) (reverse (neg bB) []) 0);;

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)

let quot_b bA bB =
  let rec euh bA bB l = match bA with
      bA when bA <<! bB -> l,false
    |bA when bA >>! bB -> euh (diff_n bA bB) bB (add_n l [1])
    |bA -> (add_n l [1],true)
  in match (bA,bB) with
      [],_|_,[] -> []
    |e::bA,f::bB when e = 0 && f = 0 -> let (l,_) = euh bA bB [] in 0::l
    |e::bA,f::bB when e = 1 && f = 0 -> let (l,b) = euh (neg (1::bA)) bB []
                                        in neg(0::(if b then l else add_n l [1]))
    |e::bA,f::bB when e = 0 && f = 1 -> let (l,_) = euh bA (neg (1::bB)) [] in neg(0::l)
    |e::bA,f::bB -> let (l,b) = euh (neg bA) (neg bB) [] in 0::(if b then l else add_n l [1]);;

(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)
let mod_b bA bB = diff_b bA (mult_b bB (quot_b bA bB));;

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB =
  let q = quot_b bA bB
  in (q,diff_b bA (mult_b bB q));;
