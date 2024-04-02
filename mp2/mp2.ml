(* File: mp2.ml *)

(* Problem 1 *)
let closer_to_origin (x1, y1) (x2, y2) = 
    let squared_distance (x, y) = (x ** 2.) +. (y ** 2.) in 
    let first_distance = squared_distance (x1, y1) in
    let second_distance = squared_distance (x2, y2) in
    if first_distance < second_distance then
        -1
    else if first_distance > second_distance then
        1
    else 
        0
    ;;

(* Problem 2 *)
let swap_eq (x1, y1) (x2, y2) = 
    x1 = y2 && y1 = x2
    ;;

(* Problem 3 *)
let twist ((a, b), (c, d)) =
    ((d, a), (c, b))
    ;;

(* Problem 4 *)
let triple_pairs x (a, b, c) =
    ((x, a), (x, b), (x, c))
    ;;

(* Problem 5 *)
let triple_xprod (a, b, c) (d, e) =
    (((a, d), (b, d), (c, d)), ((a, e), (b, e), (c, e)))
    ;;

(*  Problem 6 *)
let two_funs (a, b) (x, y) =
    (a x, b y)
    ;;

(*  Problem 7 *)
let triple_app (f,g,h) x = 
    f (g (h x))
    ;;

(*  Problem 8 *)
let same_arg_twice f x = 
    f x x
    ;;


(*  Problem 9 *)
let rev_app x f = 
    f x
    ;;

(*  Problem 10 *)
let map_triple f (a,b,c) = 
    (f a, f b, f c)
    ;;

(* Problem 11 *)
let rec ackermann m n = 
    if m = 0 then
        n + 1
    else if m > 0 && n = 0 then
        ackermann (m - 1) 1
    else
        ackermann (m - 1) (ackermann m (n - 1))
    ;;

(* Problem 12 *)
let rec collatz n = 
    if n = 1 then
        0
    else if n mod 2 = 0 then
        1 + collatz (n / 2)
    else
        1 + collatz (3 * n + 1)
    ;;

(* Problem 13 *)
let rec delannoy (m, n) = 
    if m = 0 || n = 0 then
        1
    else
        delannoy (m - 1, n) + delannoy (m, n - 1) + delannoy (m - 1, n - 1)
    ;;


(* Problem 14 *)
let rec naive_fibonacci n = 
    if n <= 1 then
        1
    else
        naive_fibonacci (n - 1) + naive_fibonacci (n - 2)
    ;;

(* Problem 15 *)
let rec sum_evens_less_eq n = 
    if n <= 0 then
        0
    else if n mod 2 = 0 then
        n + sum_evens_less_eq (n - 2)
    else
        sum_evens_less_eq (n - 1)
    ;;
