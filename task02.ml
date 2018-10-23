let rec pow10 n = if n >= 1 then 10 * pow10 (n - 1) else 1;;
  
let lead_digits_at x n = if n >= 0 then x / (pow10 n) else 0;;
let tail_digits_at x n = if n > 0 then x mod (pow10 n) else 0;; 

let digit_at x n = tail_digits_at (lead_digits_at x n) 1;;

assert((digit_at 1234 0) = 4);;
assert((digit_at 1234 1) = 3);;
assert((digit_at 1234 2) = 2);;
assert((digit_at 1234 3) = 1);;
assert((digit_at 1234 4) = 0);;

let check_3digits x =
  let 
      fd = digit_at x 2
    and
      ld = digit_at x 0
  in ((fd - ld) > 1) || ((ld - fd) > 1);;

let reverse_3digits x =
  let y = tail_digits_at x 3
    and
      fd = digit_at x 2
    and
      md = digit_at x 1
    and
      ld = digit_at x 0
  in ld * (pow10 2) + md * (pow10 1) + fd * (pow10 0);;
  
    
  assert((reverse_3digits 123) = 321);;
  assert((check_3digits 123));;
    
    
    (* Direct task *)

  let emax x y = if x > y then x else y;;
  let emin x y = if x < y then x else y;;
    
  let encode x =
    let y = tail_digits_at x 2
    in if check_3digits y then
         let mx = emax (reverse_3digits y) y
           and
             nx = emin (reverse_3digits y) y
         in mx - nx
       else 0;;

    (* Inverse task *)

  let decode x =
    let 
      fd = digit_at x 0
    in if x <= 9 then
         fd * (pow10 2) + 9 * (pow10 1) + (9 - fd) * (pow10 0)
    else 0;;

      
      
         
