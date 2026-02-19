open Ppxlib

let ast_of_list_expr expr =
  let loc = expr.pexp_loc in
  let rec collect_list acc expr =
    match expr.pexp_desc with
    | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> List.rev acc
    | Pexp_construct ({ txt = Lident "::"; _ }, Some rest) ->
      (match rest.pexp_desc with
       | Pexp_tuple [ param; tail ] ->
         let name =
           match param.pexp_desc with
           | Pexp_constant (Pconst_string (s, _, _)) -> s
           | _ -> Location.raise_errorf ~loc "Parameter must be a string literal"
         in
         collect_list (name :: acc) tail
       | _ -> Location.raise_errorf ~loc "Invalid parameter list structure")
    | _ -> Location.raise_errorf ~loc "Expected a list of parameter names"
  in
  collect_list [] expr
;;

let expand ~ctxt expr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match expr.pexp_desc with
  | Pexp_tuple [ params_expr; body_expr ] ->
    let param_names = ast_of_list_expr params_expr in
    (* Build let bindings for each parameter *)
    let bindings =
      List.map
        (fun name ->
           let pat = Ast_builder.Default.pvar ~loc name in
           let expr =
             Ast_builder.Default.eapply
               ~loc
               (Ast_builder.Default.evar ~loc "Dream.param")
               [ Ast_builder.Default.evar ~loc "rq"
               ; Ast_builder.Default.estring ~loc name
               ]
           in
           Ast_builder.Default.value_binding ~loc ~pat ~expr)
        param_names
    in
    (* Build the body with let bindings *)
    let body_with_bindings =
      match bindings with
      | [] -> body_expr
      | _ -> Ast_builder.Default.pexp_let ~loc Nonrecursive bindings body_expr
    in
    (* Wrap in try-catch *)
    Ast_builder.Default.pexp_try
      ~loc
      body_with_bindings
      [ Ast_builder.Default.case
          ~lhs:(Ast_builder.Default.ppat_any ~loc)
          ~guard:None
          ~rhs:
            (Ast_builder.Default.eapply
               ~loc
               (Ast_builder.Default.evar ~loc "Dream.html")
               [ Ast_builder.Default.estring ~loc "Blog post not found" ])
      ]
  | _ -> Location.raise_errorf ~loc "require_params expects a tuple of (param_list, body)"
;;

let extension =
  Extension.V3.declare
    "require_params"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand
;;

let () =
  Ppxlib.Driver.V2.register_transformation ~extensions:[ extension ] "require_params"
;;
