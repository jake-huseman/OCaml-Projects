(*
 * File: mp6.ml
 *)

open Common

(* Problem 1*)
let asMonoTy1 () = 
    mk_fun_ty bool_ty (mk_list_ty int_ty)
let asMonoTy2 () = 
    let a = fresh () in
    let b = fresh () in
    let c = fresh () in
    let d = fresh () in
    mk_fun_ty a (mk_fun_ty b (mk_fun_ty c d))
let asMonoTy3 () = 
    let a = fresh () in
    let b = fresh () in
    mk_fun_ty a (mk_list_ty (mk_pair_ty b int_ty))
let asMonoTy4 () = 
    let b = fresh () in
    let a = fresh () in
    mk_pair_ty string_ty (mk_fun_ty (mk_list_ty b) a)

(* Problem 2*)
let subst_fun s =
    fun n ->
        try List.assoc n s
        with Not_found -> TyVar n
;;

(* Problem 3*)
let rec monoTy_lift_subst subst monoTy = 
    match monoTy with
    | TyVar n -> 
        (try List.assoc n subst with Not_found -> TyVar n)
    | TyConst (name, ts) -> 
        TyConst (name, List.map (fun t -> monoTy_lift_subst subst t) ts)
;;

(* Problem 4*)
let rec occurs x ty =
    match ty with
    | TyVar n -> n = x
    | TyConst (_, types) -> List.exists (occurs x) types
;;

(* Problem 5*)
let rec unify eqlst =
    let rec apply_subst subst ty =
        match ty with
        | TyVar v -> (try List.assoc v subst with Not_found -> TyVar v)
        | TyConst (name, args) -> TyConst (name, List.map (apply_subst subst) args)
    in
    match eqlst with
    | [] -> Some []
    | (s, t) :: rest ->
        if s = t then
            unify rest  (* Delete rule *)
        else match (s, t) with
            | (TyVar sv, _) when not (occurs sv t) ->
                let new_subst = (sv, t) in
                let rest_substituted = List.map (fun (a, b) -> (apply_subst [new_subst] a, apply_subst [new_subst] b)) rest in
                (match unify rest_substituted with
                | None -> None
                | Some more_subst -> Some (new_subst :: more_subst))
            | (_, TyVar sv) when not (occurs sv s) ->
                let new_subst = (sv, s) in
                let rest_substituted = List.map (fun (a, b) -> (apply_subst [new_subst] a, apply_subst [new_subst] b)) rest in
                (match unify rest_substituted with
                | None -> None
                | Some more_subst -> Some (new_subst :: more_subst))
            | (TyConst (n1, args1), TyConst (n2, args2)) when n1 = n2 && List.length args1 = List.length args2 ->
                unify (List.map2 (fun a b -> (a, b)) args1 args2 @ rest)
            | _ -> None
;;