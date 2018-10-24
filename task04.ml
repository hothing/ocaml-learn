(* Find a LCM *)

let rec gcd1 a b = if a >= b then let
                       q = a / b
                     in let r = a - q * b
                        in if r > 0 then gcd1 b r else b
                     else gcd1 b a;;

let even x = x mod 2 = 0;;
let odd x = if even x then false else true;;

let rec gcd2 a b = if a = 0 then b else
                 if b = 0 then a else
                   if a = 1 then a else
                     if b = 1 then b else
                       if a = b then a else
                         if (even a) && (even b) then 2 * gcd2 (a / 2) (b / 2) else
                           if (even a) && (odd b) then gcd2 (a / 2) b else
                             if (odd a) && (even b) then gcd2 a (b / 2) else
                               if a < b then gcd2 ((b - a) / 2) a else gcd2 ((a - b) / 2) b;;


let lcm0 a b fgcd = let
    x = a * b
  and
    mmax = 46340
  in if (a <= mmax) && (b <= mmax) then if x > 0 then x / fgcd a b else -x / fgcd a b else 0;;
                   
let lcm1 a b = lcm0 a b gcd1;;

let lcm2 a b = lcm0 a b gcd2;;
