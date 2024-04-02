(* CS342 - Spring 2024
 * MP3
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)


(*Problem 1*)
let rec product l =
    match l with
    | [] -> 1.0
    | h :: t -> h *. product t
    ;;

(*Problem 2*)
let rec double_all l =
    match l with
    | [] -> []
    | h :: t -> (2.0 *. h) :: double_all t
    ;;

(*Problem 3*)
let rec pair_with_all x l =
    match l with
    | [] -> []
    | h :: t -> (x, h) :: pair_with_all x t
    ;;

(*Problem 4*)
let rec interleave l1 l2 = 
    match l1, l2 with
    | [], [] -> []
    | [], _ -> l2
    | _, [] -> l1
    | h1::t1, h2::t2 -> h1 :: h2 :: interleave t1 t2
    ;;

(*Problem 5*)
let rec even_count_fr l =
    match l with
    | [] -> 0
    | h :: t -> (if h mod 2 = 0 then 1 else 0) + even_count_fr t
    ;;

(*Problem 6*)
let rec pair_sums l = 
    match l with
    | [] -> []
    | (a, b) :: t -> (a + b) :: pair_sums t
    ;;

(*Problem 7*)
let rec remove_even list = 
    match list with
    | [] -> []
    | h :: t -> if h mod 2 = 0 then remove_even t else h :: remove_even t
    ;;

(*Problem 8*)
let rec sift p l = 
    match l with
    | [] -> ([], [])
    | h :: t ->
        let (true_list, false_list) = sift p t in
        if p h then (h :: true_list, false_list)
        else (true_list, h :: false_list)
    ;;

(*Problem 9*)
let rec even_count_tr l = 
    let rec helper l acc =
        match l with
        | [] -> acc
        | h :: t -> helper t (if h mod 2 = 0 then acc + 1 else acc)
    in
    helper l 0
    ;;

(*Problem 10*)
let rec count_element l m = 
    let rec helper l m acc =
        match l with
        | [] -> acc
        | h :: t -> helper t m (if h = m then acc + 1 else acc)
    in
    helper l m 0
    ;;

(*Problem 11*)
let rec all_nonneg list = 
    let rec helper list is_all_nonneg =
        match list with
        | [] -> is_all_nonneg
        | h :: t -> if h < 0 then false else helper t is_all_nonneg
    in
    helper list true
    ;;

(*Problem 12*)
let rec split_sum l f =
    let rec helper l f sum_true sum_false =
    match l with
    | [] -> (sum_true, sum_false)
    | h :: t ->
        if f h then helper t f (sum_true + h) sum_false
        else helper t f sum_true (sum_false + h)
    in
    helper l f 0 0
    ;;

(*Problem 13*)
let even_count_fr_base = 0;;
let even_count_fr_rec x rec_val = 
    if x mod 2 = 0 then rec_val + 1 else rec_val
    ;;
  
(*Problem 14*)
let pair_sums_map_arg (a, b) = 
    a + b
    ;;


(*Problem 15*)
let remove_even_base = []
let remove_even_rec n r =
    if n mod 2 = 0 then r else n :: r
    ;;

(*Problem 16*)
let even_count_tr_start = 0
let even_count_tr_step acc_val x =
    if x mod 2 = 0 then acc_val + 1 else acc_val
    ;;



(*Problem 17*)
let split_sum_start = (0,0)
let split_sum_step f (sum_true, sum_false) x = 
    if f x then (sum_true + x, sum_false)
    else (sum_true, sum_false + x)
    ;;

