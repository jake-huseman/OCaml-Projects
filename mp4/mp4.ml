(* File: mp4.ml *)

open Common

(* Problem 1 *)

let rec import_list lst = 
    match lst with
    | [] -> ConstExp NilConst
    | (x, y) :: xs ->
      BinOpAppExp (ConsOp,
        BinOpAppExp (CommaOp, ConstExp (IntConst x), ConstExp (IntConst y)),
        import_list xs)
;;

(* Problem 2 *)
let pair_sums =
    LetRecInExp(
        "pair_sums", "lst",
        IfExp(
            BinOpAppExp(EqOp, VarExp("lst"), ConstExp(NilConst)),
            ConstExp(NilConst),
            LetInExp(
                "x", MonOpAppExp(HdOp, VarExp("lst")),
                BinOpAppExp(
                    ConsOp,
                    BinOpAppExp(
                        IntPlusOp,
                        MonOpAppExp(FstOp, VarExp("x")),
                        MonOpAppExp(SndOp, VarExp("x"))
                    ),
                    AppExp(VarExp("pair_sums"), MonOpAppExp(TlOp, VarExp("lst")))
                )
            )
        ),
        AppExp(
            VarExp("pair_sums"),
            BinOpAppExp(
                ConsOp,
                BinOpAppExp(
                    CommaOp, ConstExp(IntConst 7), ConstExp(IntConst 1)
                ),
                BinOpAppExp(
                    ConsOp,
                    BinOpAppExp(
                        CommaOp, ConstExp(IntConst 4), ConstExp(IntConst 2)
                    ),
                    BinOpAppExp(
                        ConsOp,
                        BinOpAppExp(
                            CommaOp, ConstExp(IntConst 6), ConstExp(IntConst 3)
                        ),
                        ConstExp(NilConst)
                    )
                )
            )
        )
    )
;;

(* Problem 3 *)
let rec count_const_in_exp exp =
    match exp with
    | ConstExp _ -> 1
    | VarExp _ -> 0
    | BinOpAppExp(_, exp1, exp2) ->
        count_const_in_exp exp1 + count_const_in_exp exp2
    | MonOpAppExp(_, exp) ->
        count_const_in_exp exp
    | IfExp(exp1, exp2, exp3) ->
        count_const_in_exp exp1 + count_const_in_exp exp2 + count_const_in_exp exp3
    | AppExp(exp1, exp2) ->
        count_const_in_exp exp1 + count_const_in_exp exp2
    | LetInExp(_, exp1, exp2) ->
        count_const_in_exp exp1 + count_const_in_exp exp2
    | LetRecInExp(_, _, exp1, exp2) ->
        count_const_in_exp exp1 + count_const_in_exp exp2
    | _ -> 0
;;

(* Problem 4 *)
let rec freeVarsInExp exp = 
    match exp with
    | VarExp v -> [v]
    | ConstExp _ -> []
    | MonOpAppExp(_, exp) -> freeVarsInExp exp
    | BinOpAppExp(_, exp1, exp2) ->
        (freeVarsInExp exp1) @ (freeVarsInExp exp2)
    | IfExp(exp1, exp2, exp3) ->
        (freeVarsInExp exp1) @ (freeVarsInExp exp2) @ (freeVarsInExp exp3)
    | AppExp(exp1, exp2) ->
        (freeVarsInExp exp1) @ (freeVarsInExp exp2)
    | FunExp(v, exp) ->
        List.filter (fun x -> x <> v) (freeVarsInExp exp)
    | LetInExp(v, exp1, exp2) ->
        (freeVarsInExp exp1) @ (List.filter (fun x -> x <> v) (freeVarsInExp exp2))
    | LetRecInExp(f, x, exp1, exp2) ->
        (List.filter (fun y -> (y <> f) && (y <> x)) (freeVarsInExp exp1)) @
        (List.filter (fun y -> y <> f) (freeVarsInExp exp2))
;;


(* Problem 5 *)
let rec cps_exp exp cont = 
    match exp with
    | VarExp v -> VarCPS (cont, v)
    | ConstExp c -> ConstCPS (cont, c)
    | IfExp (e1, e2, e3) ->
        let v = freshFor ["e2"; "e3";] in
        let then_branch = cps_exp e2 cont in
        let else_branch = cps_exp e3 cont in
        let new_cont = FnContCPS (v, IfCPS (v, then_branch, else_branch)) in
        cps_exp e1 new_cont
    | AppExp (e1, e2) ->
        let v2 = freshFor ["e1";] in
        let v1 = freshFor [v2;] in
        cps_exp e2 (FnContCPS (v2, cps_exp e1 (FnContCPS (v1, AppCPS (cont, v1, v2)))))
    | BinOpAppExp (op, e1, e2) ->
        let v2 = freshFor ["e1";] in
        let v1 = freshFor [v2;] in
        cps_exp e2 (FnContCPS (v2, cps_exp e1 (FnContCPS (v1, BinOpAppCPS (cont, op, v1, v2)))))
    | MonOpAppExp (op, e) ->
        let v = freshFor [] in
        cps_exp e (FnContCPS (v, MonOpAppCPS (cont, op, v)))
    | FunExp (x, e) ->
        FunCPS (cont, x, Kvar, cps_exp e (ContVarCPS Kvar))
    | LetRecInExp (f, x, e1, e2) ->
        let body_cps = cps_exp e1 (ContVarCPS Kvar) in
        let rec_def = FixCPS (cont, f, x, Kvar, body_cps) in
    cps_exp e2 cont
;;