open Kawa

exception Error of string

let error s = raise (Error s)

let type_error ty_actual ty_expected =
  error (Printf.sprintf "Expected %s, got %s instead"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

let type_error2 ty_actual ty_expected1 ty_expected2 =
  error (Printf.sprintf "Expected %s or %s, got %s instead"
           (typ_to_string ty_expected1) (typ_to_string ty_expected2) (typ_to_string ty_actual))

let variable_error ident =
  error (Printf.sprintf "Variable %s not declared" ident)

let class_error ident =
  error (Printf.sprintf "Class %s not declared" ident)

let attribute_error attr_name class_name =
  error (Printf.sprintf "Attribute %s not declared in class %s" attr_name class_name)

let attribute_number_error class_name attr args =
  error (Printf.sprintf "Class %s expected %d arguments, got %d" class_name (List.length attr) (List.length args))

let method_error method_name class_name =
  error (Printf.sprintf "Method %s is not a method of class %s" method_name class_name)

let non_object_error typ string =
  error (Printf.sprintf "Cannot call %s %s on a non-object type" typ string)

let eq_type_error ty_expected = 
  error (Printf.sprintf "No matching operator == or != for type %s" (typ_to_string ty_expected))

let eqs_type_error ty_expected = 
  error (Printf.sprintf "No matching operator === or =/= for type %s" (typ_to_string ty_expected))

let constr_error method_name i1 i2 = 
  error (Printf.sprintf "Constructor of class %s expects %d arguments, got %d" method_name i1 i2)

let use_of_void_error ident =
  error (Printf.sprintf "Variable %s has no value" ident)

(* Retrouve la définition d'une classe par son nom *)
let find_class classes name =
  match List.find_opt (fun c -> c.class_name = name) classes with
  | Some c -> c
  | None -> class_error name

(* Retrouve l'attribut dans une classe ou sa classe parente *)
let rec find_attribute classes class_name attr_name =
  let c = find_class classes class_name in
  match List.find_opt (fun (a, _) -> a = attr_name) c.attributes with
  | Some (_, t) -> t
  | None ->
    (match c.parent with
     | Some parent_name -> find_attribute classes parent_name attr_name
     | None -> attribute_error attr_name class_name)

(* Retrouve une méthode dans une classe ou sa classe parente *)
let rec find_method classes class_name method_name =
  let c = find_class classes class_name in
  match List.find_opt (fun m -> m.method_name = method_name) c.methods with
  | Some m -> m
  | None ->
    (match c.parent with
     | Some parent_name -> find_method classes parent_name method_name
     | None -> method_error method_name class_name)

(* Retourne la liste des (attr_name, attr_type) de la classe et de ses parents *)
let rec all_attributes classes class_name =
  let c = match List.find_opt (fun c -> c.class_name = class_name) classes with
    | Some c -> c
    | None -> class_error class_name in
      let parent_attrs = match c.parent with
        | Some parent_name -> all_attributes classes parent_name
        | None -> []
  in parent_attrs @ c.attributes

(* Détermine si t1 est un sous-type de t2 *)
let rec is_subtype classes t1 t2 =
  match t1, t2 with
    | TInt,  TInt  -> true
    | TBool, TBool -> true
    | TVoid, TVoid -> true
    | TClass c1, TClass c2 -> if c1 = c2 then true
      else let class_def = find_class classes c1 in
        (match class_def.parent with
          | Some parent_name -> is_subtype classes (TClass parent_name) (TClass c2)
          | None -> false)
    | _ -> false

module Env = Map.Make(String)
type tenv = typ Env.t

let add_env l tenv =
  List.fold_left (fun env (x, t) -> Env.add x t env) tenv l

let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in

  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error typ_e typ

  and type_expr e tenv = match e with
    | Binop(bop, e1, e2) ->
        let type1 = type_expr e1 tenv in
        let type2 = type_expr e2 tenv in
        (match bop with
            | Add | Sub | Mul | Div | Rem -> if type1 = TInt && type2 = TInt then TInt
              else if type1 <> TInt then type_error type1 TInt
              else type_error type2 TInt

            | Lt | Le | Gt | Ge -> if type1 = TInt && type2 = TInt then TBool
            else if type1 <> TInt then type_error type1 TInt
            else type_error type2 TInt
            
            | Eq | Neq | Eqs | Neqs -> if type1 = type2 then TBool
            else type_error type2 type1
            
   
            | And | Or -> if type1 = TBool && type2 = TBool then TBool
            else if type1 <> TBool then type_error type1 TBool
            else type_error type2 TBool)
    | Unop(op, e) ->
      let type1 = type_expr e tenv in
        (match op with
          | Opp -> if type1 = TInt then TInt
          else type_error type1 TInt
          
          | Not -> if type1 = TBool then TBool
          else type_error type1 TBool)
    | Get m -> type_mem_access m tenv
    | Int _  -> TInt
    | Bool _ -> TBool
    | New class_name ->
      (match List.find_opt (fun c -> c.class_name = class_name) p.classes with
        | Some _ -> TClass class_name
        | None -> class_error class_name)
    | NewObj (class_name, args) ->
      (* Vérifie l'existence de la classe *)
      (match List.find_opt (fun c -> c.class_name = class_name) p.classes with
        | Some class_def -> let attr_types = List.map snd (all_attributes p.classes class_name) in
          (* ^ Récupére les attributs *)
          (* Vérifie que le nombre d’arguments correspond au nombre total d'attributs *)
          if List.length args <> List.length attr_types then attribute_number_error class_name attr_types args;
          (* Vérifie que chaque argument correspond bien au type d'attribut *)
          List.iter2 (fun arg attr_type -> let arg_type = type_expr arg tenv in
            if arg_type <> attr_type then type_error arg_type attr_type) args attr_types;
          TClass class_name
        | None -> class_error class_name)
    | NewCstr (class_name, args) ->
      (* Vérifie l'existence de la classe *)
      (match List.find_opt (fun c -> c.class_name = class_name) p.classes with
        | Some class_def ->
          (* Vérifie qu'une méthode `constructor` existe dans la classe *)
          let constructor = match List.find_opt (fun m -> m.method_name = "constructor") class_def.methods with
            | Some m -> m
            | None -> method_error "constructor" class_name in
              (* Vérifie que le nombre d'arguments correspond au nombre attendu par le constructeur *)
              if List.length args <> List.length constructor.params then constr_error class_name (List.length constructor.params) (List.length args);
              (* Vérifie le type des arguments passés au constructeur *)
              List.iter2 (fun arg (param_name, param_type) -> let arg_type = type_expr arg tenv in
                if arg_type <> param_type then type_error arg_type param_type) args constructor.params;
             TClass class_name
       | None -> class_error class_name)
    | This -> (match Env.find_opt "this" tenv with
        | None -> error (Printf.sprintf "Cannot use 'this' outside of a class")
        | Some c -> c)
    | MethCall (e, method_name, args) -> let obj_type = type_expr e tenv in
        (* Vérifie que e est bien un objet d'une classe *)
        (match obj_type with
          | TClass class_name -> (match List.find_opt (fun c -> c.class_name = class_name) p.classes with
            (* Récupère la définition de la classe de l'objet *)
            | Some class_def -> let method_def = find_method p.classes class_name method_name in
              (* Vérifie que la méthode existe dans la classe *)
                if List.length args <> List.length method_def.params then
                  error (Printf.sprintf "Method %s expects %d arguments, got %d" 
                    method_name (List.length method_def.params) (List.length args));
                    (* ^ Vérifie que le nombre d'arguments correspond au nombre de paramètres de la méthode *)
                  List.iter2 (fun arg param -> let arg_type = type_expr arg tenv in let (_, param_type) = param in 
                    if arg_type <> param_type then type_error arg_type param_type) args method_def.params;
                    (* ^ Vérifie le type de chaque argument par rapport aux paramètres *)
                    method_def.return
            | None -> class_error class_name)
          | _ -> non_object_error "method" method_name)
    (*| _ -> failwith "Case not implemented in typechecker.type_expr"*)

  and type_mem_access m tenv = match m with
    | Var ident -> if Env.mem ident tenv then Env.find ident tenv
      else variable_error ident
    | Field (obj, attr) ->
      let obj_type = type_expr obj tenv in
      (match obj_type with
        | TClass class_name -> find_attribute p.classes class_name attr
        | _ -> non_object_error "attribute" attr)
    (*| _ -> failwith "Case not implemented in typechecker.type_mem_access"*)
  in

  let rec check_instr i ret tenv = match i with
    | Set (m, e) -> let mem_type = type_mem_access m tenv in
      let expr_type = type_expr e tenv in
      if not (is_subtype p.classes expr_type mem_type) then type_error expr_type mem_type
    | Print e -> let type1 = type_expr e tenv in 
        if type1 = TBool then check e TBool tenv
          else (if type1 = TInt then check e TInt tenv
            else (if type1 = TVoid then check e TVoid tenv
              else error (Printf.sprintf "No printing method defined for a class")))
    | If (e, i1, i2) -> let type1 = type_expr e tenv in
        if type1 = TBool then (
          check_seq i1 ret tenv;
          check_seq i2 ret tenv)
        else type_error type1 TBool
    | Iff (e, i) -> let type1 = type_expr e tenv in
        if type1 = TBool then check_seq i ret tenv
        else type_error type1 TBool
    | While (e, i) -> let type1 = type_expr e tenv in
        if type1 = TBool then
          check_seq i ret tenv
        else type_error type1 TBool
    | Return e -> check e ret tenv
    | Expr e -> check e ret tenv
    (*| _ -> failwith "Case not implemented in typechecker.check_instr"*)
  
  and check_seq s ret tenv = List.iter (fun i -> check_instr i ret tenv) s in

  let check_duplicate_globals globals =
    let global_names = List.map fst globals in
      let duplicates = List.filter (fun name -> List.length (List.filter ((=) name) global_names) > 1) global_names
        in if duplicates <> [] then error (Printf.sprintf "Duplicate global variable declarations: %s"
            (String.concat ", " duplicates))
  in check_duplicate_globals p.globals; (* check_duplicate_globals *)

  let check_duplicate_classes classes =
    let class_names = List.map (fun c -> c.class_name) classes in
      let duplicates = List.filter (fun name -> List.length (List.filter ((=) name) class_names) > 1) class_names
        in if duplicates <> [] then error (Printf.sprintf "Duplicate class declarations: %s"
            (String.concat ", " duplicates))
  in check_duplicate_classes p.classes; (* check_duplicate_class *)

  let check_method class_def method_def =
    let check_duplicate_locals method_def =
      let param_names = List.map fst method_def.params in
        let local_names = List.map fst method_def.locals in
          let all_names = param_names @ local_names in
            let duplicates = List.filter (fun name -> List.length (List.filter ((=) name) all_names) > 1) all_names
              in if duplicates <> [] then error (Printf.sprintf "Duplicate local variable declarations in method %s: %s"
                 method_def.method_name (String.concat ", " duplicates))
    in check_duplicate_locals method_def; (* check_duplicate_locals *)
    (* Crée un environnement de méthode *)
    let tenv_method = add_env method_def.locals (add_env method_def.params (Env.add "this" (TClass class_def.class_name) tenv))
      (* Vérifie qu'une méthode non void possède forcément un return quelque soit le chemin d'exécution *)
      in let rec contains_return = function
        | [] -> false
        | Return _ :: _ -> true
        | If (_, seq1, seq2) :: rest -> 
          let r1 = contains_return seq1 in
          let r2 = contains_return seq2 in
            if r1 && r2 then true
            else contains_return rest
        | Iff (_, seq) :: rest -> contains_return rest  (* On ignore le if car il ne sera pas forcément exécuté *)
        | While (_, seq) :: rest -> contains_return rest  (* On ignore le while car il ne sera pas forcément exécuté *)
        | _ :: rest -> contains_return rest
      (* Vérification de chaque méthode *)
      in let t = typ_to_string method_def.return in
        if method_def.return <> TVoid && not (contains_return method_def.code) then (* contains_return *)
          error (Printf.sprintf "Method %s in class %s has return type %s but doesn't contain a return of type %s in each branch"
            method_def.method_name class_def.class_name t t);
          check_seq method_def.code method_def.return tenv_method
  in List.iter (fun c -> List.iter (fun m -> check_method c m) c.methods) p.classes; (* check_method *)

  let check_class classes =
    let global_var_names = List.map fst p.globals in
    (* Vérifie les méthodes en double dans une classe *)
    let check_duplicate_methods class_def =
      let method_names = List.map (fun m -> m.method_name) class_def.methods in
        let duplicates = List.filter (fun name -> List.length (List.filter ((=) name) method_names) > 1) method_names
          in if duplicates <> [] then error (Printf.sprintf "Duplicate method declarations in class %s: %s"
              class_def.class_name (String.concat ", " duplicates))
    in
    (* Vérifie les attributs en double dans une classe *)
    let check_duplicate_attributes class_def =
      let attribute_names = List.map fst class_def.attributes in
        let duplicates = List.filter (fun name -> List.length (List.filter ((=) name) attribute_names) > 1) attribute_names
          in if duplicates <> [] then error (Printf.sprintf "Duplicate attribute declarations in class %s: %s"
              class_def.class_name (String.concat ", " duplicates))
    in
    (* Itère sur chaque classe pour vérifier les règles ci-dessus *)
    List.iter (fun class_def ->
      (* Vérifie qu’aucune classe n’a le même nom qu’une variable globale *)
      if List.mem class_def.class_name global_var_names then
        error (Printf.sprintf "Class %s has the same name as a global variable"
            class_def.class_name);
      (* Vérifie les méthodes et attributs en double *)
      check_duplicate_methods class_def;
      check_duplicate_attributes class_def;
      (* Vérifie qu’aucune méthode n'a le même nom qu’un attribut dans la même classe *)
      let attribute_names = List.map fst class_def.attributes in
      List.iter (fun m -> if List.mem m.method_name attribute_names then
        error (Printf.sprintf  "Method %s in class %s cannot have the same name as an attribute of the same class"
            m.method_name class_def.class_name)) class_def.methods;) classes
  in check_class p.classes; (* check_class *)

  check_seq p.main TVoid tenv

  