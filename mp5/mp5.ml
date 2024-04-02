open Common

let rec gather_exp_ty_substitution gamma exp tau =
    let judgment = ExpJudgment(gamma, exp, tau) in
    match exp with
    | ConstExp c ->
        let tau' = const_signature c in
        begin match unify [(tau, freshInstance tau')] with
        | None -> None
        | Some sigma -> Some(Proof([], judgment), sigma)
        end
    | VarExp x -> 
        begin match lookup_env gamma x with
        | None -> None
        | Some gx -> 
            begin match unify [(tau, freshInstance gx)] with
            | None -> None
            | Some sigma -> Some(Proof([], judgment), sigma)
            end
        end
    | BinOpAppExp(op, e1, e2) ->
        let tau' = binop_signature op in
        let tau1 = fresh() in
        begin match gather_exp_ty_substitution gamma e1 tau1 with
        | None -> None
        | Some(e1_proof, sigma1) ->
            let e2_gamma = env_lift_subst sigma1 gamma in
            let tau2 = fresh() in
            begin match gather_exp_ty_substitution e2_gamma e2 tau2 with
            | None -> None
            | Some(e2_proof, sigma2) ->
                let composed = subst_compose sigma2 sigma1 in
                let tau_fun = mk_fun_ty tau1 (mk_fun_ty tau2 tau) in
                let left = monoTy_lift_subst composed tau_fun in
                begin match unify [(left, freshInstance tau')] with
                | None -> None
                | Some sigma -> Some(Proof([e1_proof; e2_proof], judgment), subst_compose sigma composed)
                end
            end
        end
    | MonOpAppExp(monop, e1) ->
        let tau1 = fresh() in
        begin match gather_exp_ty_substitution gamma e1 tau1 with
        | None -> None
        | Some(proof, sigma) ->
            let tau_fun = mk_fun_ty tau1 tau in
            let sigma_fun = monoTy_lift_subst sigma tau_fun in
            let tau' = monop_signature monop in
            begin match unify [(sigma_fun, freshInstance tau')] with
            | None -> None
            | Some(u_sigma) -> Some(Proof([proof], judgment), subst_compose u_sigma sigma)
            end
        end
    | IfExp(e1, e2, e3) ->
        begin match gather_exp_ty_substitution gamma e1 bool_ty with
        | None -> None
        | Some(e1_proof, sigma1) ->
            let e2_gamma = env_lift_subst sigma1 gamma in
            let tau2 = monoTy_lift_subst sigma1 tau in
            begin match gather_exp_ty_substitution e2_gamma e2 tau2 with
            | None -> None
            | Some(e2_proof, sigma2) ->
                let sigma21 = subst_compose sigma2 sigma1 in
                let e3_gamma = env_lift_subst sigma21 gamma in
                let tau3 = monoTy_lift_subst sigma21 tau in
                begin match gather_exp_ty_substitution e3_gamma e3 tau3 with
                | None -> None
                | Some(e3_proof, sigma3) -> Some(Proof([e1_proof; e2_proof; e3_proof], judgment), subst_compose sigma3 sigma21)
                end
            end
        end
    | FunExp(x, e) ->
        let tau1 = fresh () in
        let tau2 = fresh () in
        let extended_gamma = ins_env gamma x (polyTy_of_monoTy tau1) in
        begin match gather_exp_ty_substitution extended_gamma e tau2 with
        | None -> None
        | Some (body_proof, sigma1) ->
            let inferred_fun_type = mk_fun_ty (monoTy_lift_subst sigma1 tau1) (monoTy_lift_subst sigma1 tau2) in
            begin match unify [(monoTy_lift_subst sigma1 tau, inferred_fun_type)] with
            | None -> None
            | Some sigma2 ->
                let final_sigma = subst_compose sigma2 sigma1 in
                Some (Proof([body_proof], ExpJudgment(gamma, FunExp(x, e), monoTy_lift_subst final_sigma tau)), final_sigma)
            end
        end
    | AppExp(f, a) ->
        let tau_f = fresh () in
        let tau_a = fresh () in
        begin match gather_exp_ty_substitution gamma f (mk_fun_ty tau_a tau) with
        | None -> None
        | Some (proof_f, sigma_f) ->
            let gamma_prime = env_lift_subst sigma_f gamma in
            begin match gather_exp_ty_substitution gamma_prime a tau_a with
            | None -> None
            | Some (proof_a, sigma_a) ->
                let final_sigma = subst_compose sigma_a sigma_f in
                Some (Proof([proof_f; proof_a], judgment), final_sigma)
            end
        end
    | RaiseExp(e) ->
        let int_ty = int_ty in
        begin match gather_exp_ty_substitution gamma e int_ty with
        | None -> None
        | Some (proof, sigma) ->
            Some (Proof([proof], judgment), sigma)
        end
    | LetInExp(x, e1, e) -> 
        let tau1 = fresh() in
        let e1_result = gather_exp_ty_substitution gamma e1 tau1 in
        (match e1_result with 
        | None -> None
        | Some(e1_proof, sigma1) -> 
            let sigma1_gamma = env_lift_subst sigma1 gamma in
            let sigma1_tau1 = monoTy_lift_subst sigma1 tau1 in
            let sigma1_tau = monoTy_lift_subst sigma1 tau in
            let genned = gen sigma1_gamma sigma1_tau1 in
            let e_gamma = ins_env sigma1_gamma x genned in
            (match gather_exp_ty_substitution e_gamma e sigma1_tau with
            | None -> None 
            | Some(e_proof, sigma2) -> 
                let final_sigma = subst_compose sigma2 sigma1 in
                Some(Proof([e1_proof; e_proof], ExpJudgment(gamma, LetInExp(x, e1, e), monoTy_lift_subst final_sigma tau)), final_sigma)))
    | LetRecInExp(f, x, e1, e) ->
    let tau1 = fresh () in  (* Type for the function parameter *)
    let tau2 = fresh () in  (* Return type of the function *)
    let f_type = mk_fun_ty tau1 tau2 in  (* Construct the function type *)

    (* Extend the environment with the function and its parameter *)
    let extended_gamma_with_f = ins_env gamma f (polyTy_of_monoTy f_type) in
    let extended_gamma_with_x = ins_env extended_gamma_with_f x (polyTy_of_monoTy tau1) in

    begin match gather_exp_ty_substitution extended_gamma_with_x e1 tau2 with
    | None -> None
    | Some(e1_proof, sigma1) ->
        let sigma1_applied_gamma = env_lift_subst sigma1 gamma;
        let generalized_f_type = gen sigma1_applied_gamma (monoTy_lift_subst sigma1 f_type);
        let e_gamma = ins_env sigma1_applied_gamma f generalized_f_type;
        begin match gather_exp_ty_substitution e_gamma e (monoTy_lift_subst sigma1 tau) with
        | None -> None
        | Some(e_proof, sigma2) ->
            let final_sigma = subst_compose sigma2 sigma1 in
            Some(Proof([e1_proof; e_proof], ExpJudgment(gamma, LetRecInExp(f, x, e1, e), monoTy_lift_subst final_sigma tau)), final_sigma)
        end
    end
    | TryWithExp(e, opt, handler, handlers) ->
    begin match gather_exp_ty_substitution gamma e tau with
    | None -> None
    | Some(e_proof, sigma_e) ->
        (* Assuming 'opt' is not immediately used to affect the typing, focusing on e and handlers *)
        let rec process_handlers gamma handlers sigma_acc = match handlers with
        | [] -> Some([], sigma_acc) (* No more handlers; return empty proof list and accumulated sigma *)
        | (opt, handler_exp)::rest ->
            let handler_gamma = env_lift_subst sigma_acc gamma in
            begin match gather_exp_ty_substitution handler_gamma handler_exp tau with
            | None -> None
            | Some(handler_proof, sigma_handler) ->
                let final_sigma = subst_compose sigma_handler sigma_acc in
                begin match process_handlers gamma rest final_sigma with
                | None -> None
                | Some(handler_proofs, sigma_rest) ->
                    Some((handler_proof::handler_proofs), sigma_rest)
                end
            end
        in
        begin match process_handlers gamma ((opt, handler)::handlers) sigma_e with
        | None -> None
        | Some(handler_proofs, sigma_handlers) ->
            Some(Proof(e_proof::handler_proofs, judgment), sigma_handlers)
        end
    end