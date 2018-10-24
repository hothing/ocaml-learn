let even x = x mod 2 = 0;;
let odd x = if even x then false else true;;
  
let rec filter list pred =
  match list with
  | [] ->[]
  | hd::tl ->
     let new_tl = filter tl pred in
     if pred hd then hd::new_tl else new_tl;;

  
let mark_dates = [29; 4; 7; 12; 15; 17; 24; 1; 14; 16];;

let good_dates = filter mark_dates even;;

let bad_dates = filter mark_dates odd;;

if (List.length good_dates) > (List.length bad_dates) then "YES" else "NO";;  
