let check_last_digit x n = if n <= 9 then (x - n) mod 10 = 0 else false;;
let reduce_one_digit x = x / 10;;
let magic_square x = let y = reduce_one_digit x in if check_last_digit x 5 then (y * (y + 1))*100 + 25 else 0;;

assert((magic_square 5) = 25)
assert((magic_square 75) = 5625)
assert((magic_square 4255) = 18105025)

