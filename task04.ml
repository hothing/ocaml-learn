(* Find a LCM *)

let even x = x mod 2 = 0;;
let odd x = if even x then false else true;;


let rec gcd1 a b = let
    q = a / b
  and
    r = a - q * b
  in if r > 0 then gcd1 r b else b ;;
  
