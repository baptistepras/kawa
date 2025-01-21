open Kawa

let env_enabled = ref false

type value =
  | VInt  of int
  | VBool of bool
  | VObj  of obj
  | Null

and obj = {
  cls:    string;
  fields: (string, value) Hashtbl.t;
}

exception Error of string
exception Return of value

let error s = raise (Error s)

let type_error ty_actual ty_expected =
  error (Printf.sprintf "Expected %s, got %s instead"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

let class_error ident =
  error (Printf.sprintf "Cannot access attribute %s of a non-object value" ident)

let undefined_mem_error exp got = 
  error (Printf.sprintf "Expected %s but got %s instead. This happened because you tried to" exp got ^
    " access an attribute of a recursive class you didn't define before.")

let value_to_type_string = function
  | VInt _ -> "int"
  | VBool _ -> "bool"
  | VObj _ -> "obj"
  | Null -> "null"
  (*| _ -> failwith "Case not implemented in interpreter.value_to_type_string"*)

let value_to_type = function
  | VInt _ -> TInt
  | VBool _ -> TBool
  | VObj id -> TClass(id.cls)
  | Null -> TVoid
  (*| _ -> failwith "Case not implemented in interpreter.value_to_type"*)
let rec_classes = ref [""]
(* On initialise toutes les variables et tous les objets à une valeur par défaut pour éviter leur utilisation indéfinie *)
let rec default_value p stack = function
  | TInt -> VInt 0
  | TBool -> VBool false
  | TClass class_name -> 
      if List.mem class_name stack then
        (* Cas des classes récursives *)
        if List.mem class_name !rec_classes then Null
        else (Printf.printf "La classe %s est récursive, arrêt de l'initialisation automatique\n" class_name;
              rec_classes := class_name::!rec_classes; Null) 
      else
        let class_def = List.find (fun c -> c.class_name = class_name) p.classes in
        let fields = Hashtbl.create 16 in
        let stack = class_name :: stack in List.iter (fun (attr_name, attr_type) ->
          Hashtbl.add fields attr_name (default_value p stack attr_type)) class_def.attributes;
      VObj { cls = class_name; fields }
  | TVoid -> error "Cannot create an object with void type"
  (*| _ -> failwith "Case not implemented in interpreter.defaut_value"*)

(* Affiche l'état de l'environnement avec les variables, leur valeur et leur type *)
let print_env (env: (string, value) Hashtbl.t) (types: (string, typ) Hashtbl.t): unit =
  let print_value = function
    | VInt n -> Printf.printf "VInt(%d)" n
    | VBool b -> Printf.printf "VBool(%b)" b
    | VObj o -> Printf.printf "VObj(%s)" o.cls
    | Null -> Printf.printf "Null"
    (*| _ -> failwith "Case not implemented in interpreter.print_value"*)
  in
  let print_type = function
    | TInt -> "TInt"
    | TBool -> "TBool"
    | TVoid -> "TVoid"
    | TClass(id) -> "TClass " ^ id
    (*| _ -> failwith "Case not implemented in interpreter.print_type"*)
  in
  Printf.printf "\nEnvironment contents:\n";
  Hashtbl.iter (fun key value ->
    Printf.printf "%s: " key;
    print_value value;
    if Hashtbl.mem types key then
      Printf.printf " [Type: %s]" (print_type (Hashtbl.find types key))
    else
      Printf.printf " [Type: Unknown]";
    Printf.printf "\n"
  ) env;
  Printf.printf "End of environment\n\n"

(* Retourne la liste des (attr_name, attr_type) de la classe et de ses parents *)
let rec all_attributes classes class_name =
  let c = List.find (fun c -> c.class_name = class_name) classes in
  let parent_attrs =
    match c.parent with
    | Some p -> all_attributes classes p
    | None -> []
  in
  parent_attrs @ c.attributes

(* Retrouve une méthode dans une classe ou sa classe parente *)
let rec find_method classes class_name method_name =
  let c = List.find (fun c -> c.class_name = class_name) classes in
    match List.find_opt (fun m -> m.method_name = method_name) c.methods with
      | Some m -> m
      | None -> (match c.parent with
        | Some parent_name -> find_method classes parent_name method_name
        | None -> raise (Error (Printf.sprintf "Method %s not found in class %s or its parents" method_name class_name)))

(* Initialise chaque attribut d'une classe avec sa valeur par défaut *)
let init_class class_name p = let fields = Hashtbl.create 16 in
  List.iter (fun (attr_name, attr_type) -> Hashtbl.add fields attr_name (default_value p [] attr_type)
    ) (all_attributes p.classes class_name);
  fields

let exec_prog (p: program): unit =
  let env = Hashtbl.create 16 in
  List.iter (fun (x, t) -> Hashtbl.add env x (default_value p [] t)) p.globals;

  let types = Hashtbl.create 16 in

  (* Initialisation des variables globales avec des valeurs Null et des types *)
  List.iter (fun (x, t) ->
    Hashtbl.add types x t
  ) p.globals;
  if !env_enabled then print_env env types; (* Affiche l'état de l'environnement au début du main *)
  
  let rec eval_call f this args =
    let method_def = find_method p.classes this.cls f in
      (* Crée un nouvel environnement local pour la méthode *)
      let method_env = Hashtbl.create 16 in
      (* Ajoute 'this' dans l'environnement local *)
      Hashtbl.add method_env "this" (VObj this);
      (* Ajoute les les paramètres évalués *)
      List.iter2 (fun (param_name, _) arg_value -> Hashtbl.add method_env param_name arg_value) method_def.params args;
      (* Ajoute les variables locales avec leur valeur par défaut *)
      List.iter (fun (local_name, local_type) -> Hashtbl.add method_env local_name (default_value p [] local_type)) method_def.locals;
      (* Exécute la méthode *)
      try exec_seq method_def.code method_env;
        Null
      with Return v -> v

  and exec_seq s lenv =
    (* L'initialisation par défaut des variables assure qu'aucune ne sera utilisée non définie, 
       sauf dans le cas d'une classe récursive (ex: une classe point qui a un attribut point) *)
    let rec evali e = match eval e with
      | VInt n -> n
      | t -> undefined_mem_error (typ_to_string TInt) (value_to_type_string t)
    and evalb e = match eval e with
      | VBool b -> b
      | t -> undefined_mem_error (typ_to_string TBool) (value_to_type_string t)
    and evalo e = match eval e with
      | VObj o -> o
      | t -> undefined_mem_error "class" (value_to_type_string t)
        
    and eval (e: expr): value = match e with
      | Int n  -> VInt n
      | Bool b -> VBool b
      | Binop(Mul, e1, e2) -> VInt(evali e1 * evali e2)
      | Binop(Add, e1, e2) -> VInt(evali e1 + evali e2)
      | Binop(Sub, e1, e2) -> VInt(evali e1 - evali e2)
      | Binop(Div, e1, e2) -> VInt(evali e1 / evali e2)
      | Binop(Rem, e1, e2) -> VInt(evali e1 mod evali e2)
      | Binop(Lt, e1, e2) -> VBool(evali e1 < evali e2)
      | Binop(Le, e1, e2) -> VBool(evali e1 <= evali e2)
      | Binop(Gt, e1, e2) -> VBool(evali e1 > evali e2)
      | Binop(Ge, e1, e2) -> VBool(evali e1 >= evali e2)
      | Binop(And, e1, e2) -> VBool(evalb e1 && evalb e2)
      | Binop(Or, e1, e2) -> VBool(evalb e1 || evalb e2)
      (* Teste l'égalité physique sur les entiers, les booléens et les classes *)
      | Binop(Eq, e1, e2) -> let v1 = eval e1 in 
        let v2 = eval e2 in
        (match v1, v2 with
          | VObj _, VObj _ -> VBool(evalo e1 == evalo e2)
          | VInt _, VInt _ -> VBool(evali e1 == evali e2)
          | VBool _, VBool _ -> VBool(evalb e1 == evalb e2)
          | Null, Null -> VBool(true) 
          | a, b -> error (Printf.sprintf "Cannot compare (==) type %s with type %s" (value_to_type_string a) (value_to_type_string b))) (* Ne devrait pas arriver *)
      (* Teste l'inégalité physique sur les entiers, les booléens et les classes *)
      | Binop(Neq, e1, e2) -> let v1 = eval e1 in
        let v2 = eval e2 in
          (match v1, v2 with
          | VObj _, VObj _ -> VBool(evalo e1 != evalo e2)
          | VInt _, VInt _ -> VBool(evali e1 != evali e2)
          | VBool _, VBool _ -> VBool(evalb e1 != evalb e2)
          | Null, Null -> VBool(false)
          | a, b -> error (Printf.sprintf "Cannot compare (!=) type %s with type %s" (value_to_type_string a) (value_to_type_string b))) (* Ne devrait pas arriver *)
      (* Teste l'égalité structurelle sur les entiers, les booléens et les classes *)
      | Binop(Eqs, e1, e2) ->     
        let v1 = eval e1 in
          let v2 = eval e2 in
            (match v1, v2 with
              | VObj _, VObj _ -> VBool(evalo e1 = evalo e2)
              | VInt _, VInt _ -> VBool(evali e1 = evali e2)
              | VBool _, VBool _ -> VBool(evalb e1 = evalb e2)
              | Null, Null -> VBool(true)
              | a, b -> error (Printf.sprintf "Cannot compare (===) type %s with type %s" (value_to_type_string a) (value_to_type_string b))) (* Ne devrait pas arriver *)
      (* Teste l'inégalité structurelle sur les entiers, les booléens et les classes *)
      | Binop(Neqs, e1, e2) ->
        let v1 = eval e1 in
          let v2 = eval e2 in
            (match v1, v2 with
              | VObj _, VObj _ -> VBool(evalo e1 <> evalo e2)
              | VInt _, VInt _ -> VBool(evali e1 <> evali e2)
              | VBool _, VBool _ -> VBool(evalb e1 <> evalb e2)
              | Null, Null -> VBool(false)
              | a, b -> error (Printf.sprintf "Cannot compare (=/=) type %s with type %s" (value_to_type_string a) (value_to_type_string b))) (* Ne devrait pas arriver *)
      | Unop(Opp, e1) -> VInt(- evali e1)
      | Unop(Not, e1) -> VBool(not (evalb e1))
      | Get(m) -> eval_mem m
      (* Initialise les attributs d'un objet par défaut *)
      | New class_name -> VObj { cls = class_name; fields=init_class class_name p }
      (* Initiliase les attributs d'un objet avec les valeurs passées en argument *)
      | NewObj (class_name, args) ->
        let fields = Hashtbl.create 16 in
          let attrs = all_attributes p.classes class_name in
            List.iter2 (fun (attr_name, _attr_type) arg -> let arg_value = eval arg in
              Hashtbl.add fields attr_name arg_value) attrs args;
        VObj { cls = class_name; fields=fields }
      (* Initialise les attributs d'un objet par défaut, puis appelle la méthode nommée 'constructor' sur cet objet avec les valeurs passées en argument *)
      | NewCstr (class_name, args) -> let obj = { cls = class_name; fields=init_class class_name p } in
        (* Appelle le constructeur *)
        let evaluated_args = List.map eval args in
          ignore (eval_call "constructor" obj evaluated_args);
        VObj obj
      | This -> Hashtbl.find lenv "this"
      | MethCall (e, method_name, args) -> let obj = evalo e in
          let evaluated_args = List.map eval args in
          eval_call method_name obj evaluated_args 
      (*| _ -> failwith "Case not implemented in interpreter.eval"*)
    
    and eval_mem (m: mem_access): value = match m with
      | Var ident -> if Hashtbl.mem lenv ident then Hashtbl.find lenv ident (* Variable locale *)
          else Hashtbl.find env ident (* Variable globale *)
      | Field (obj_expr, attr) -> Hashtbl.find (evalo obj_expr).fields attr
      (*| _ -> failwith "Case not implemented in interpreter.eval_mem"*)
    in
  
    let rec exec (i: instr): unit = match i with
      | Set (m, e) -> let value = eval e in
          (match m with
            | Var ident -> if Hashtbl.mem lenv ident then Hashtbl.replace lenv ident value (* Variable locale *)
                else Hashtbl.replace env ident value (* Variable globale *)
            | Field (obj_expr, attr) -> let obj = evalo obj_expr in
                Hashtbl.replace obj.fields attr value
            (*| _ -> failwith "Case not implemented in interpreter.exec | Set(m, e)"*))
      | Print e -> let type1 = eval e in 
        (match type1 with
          | VInt i -> Printf.printf "%d\n" i
          | VBool b -> Printf.printf "%b\n" b
          | VObj o -> error (Printf.sprintf "No printing method defined for a class") (* Ne devrait pas arriver *)
          | _ -> Printf.printf "\n")
      | If (e, i1, i2) -> let type1 = eval e in  
          (match type1 with
            | VBool b -> if b then exec_seq i1 else exec_seq i2
            | t -> error (Printf.sprintf "Condition must be a bool, got %s instead" (value_to_type_string t))) (* Ne devrait pas arriver *)
      | Iff (e, i) -> let type1 = eval e in  
          (match type1 with
            | VBool b -> if b then exec_seq i
            | t -> error (Printf.sprintf "Condition must be a bool, got %s instead" (value_to_type_string t))) (* Ne devrait pas arriver *)
      | While (e, i1) -> let type1 = eval e in 
          (match type1 with
            | VBool b -> if b then (exec_seq i1; exec i)
            | t -> error (Printf.sprintf "Condition must be a bool, got %s instead" (value_to_type_string t))) (* Ne devrait pas arriver *)
      | Return e -> raise (Return (eval e))
      | Expr e -> ignore (eval e)
      (*| _ -> failwith "Case not implemented in interpreter.exec"*)
    and exec_seq s = 
      List.iter exec s
    in

    exec_seq s
  in
  
  exec_seq p.main env;
  if !env_enabled then print_env env types; (* Affiche l'état de l'environnement à la fin du main *)
