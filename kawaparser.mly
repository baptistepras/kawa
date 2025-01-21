%{

  open Lexing
  open Kawa

%}

%token <int> INT
%token <string> IDENT
%token <bool> BOOL
%token MAIN
%token LPAR RPAR BEGIN END SEMI
%token PRINT
%token EOF
%token IF ELSE
%token WHILE
%token RETURN
%token NOT
%token PLUS MINUS STAR DIV MOD
%token EQ NEQ EQS NEQS
%token GT GE
%token LT LE
%token AND OR
%token TRUE FALSE
%token DOT COMMA
%token ASSIGN
%token NEW
%token NEWCSTR
%token THIS
%token DEFINT DEFVOID DEFBOOL
%token VAR
%token CLASS
%token ATTRIBUTE
%token METHOD
%token EXTENDS

%left EQ NEQ EQS NEQS GT GE LT LE AND OR
%left PLUS MINUS
%left STAR DIV MOD
%right NOT
%left DOT

%start program
%type <Kawa.program> program

%%


program:
| variables=var_list lclasses=list(class_def) MAIN BEGIN main=list(instruction) END EOF
    { {classes=lclasses; globals=variables; main} }
;

var_list:
| var_decls=list(var_decl) { List.flatten var_decls }
;

var_decl:
| VAR type_var=var_typ ids=separated_list(COMMA, IDENT) SEMI { List.map (fun id -> (id, type_var)) ids }
;

attr_decl:
| ATTRIBUTE type_attr=var_typ ids=separated_list(COMMA, IDENT) SEMI { List.map (fun id -> (id, type_attr)) ids }
;

attr_list:
| attr_decls=list(attr_decl) { List.flatten attr_decls }
;

method_def:
| METHOD type_meth=var_typ id=IDENT LPAR args=separated_list(COMMA, param) RPAR BEGIN variables=var_list instructions=list(instruction) END 
    { {method_name=id; code=instructions; params=args; locals=variables; return=type_meth} }
;

class_def:
| CLASS id1=IDENT BEGIN attr=attr_list meth=list(method_def) END { {class_name=id1; attributes=attr; methods=meth; parent=None} }
| CLASS id1=IDENT EXTENDS id2=IDENT BEGIN attr=attr_list meth=list(method_def) END { {class_name=id1; attributes=attr; methods=meth; parent=Some(id2)} }
;

instruction:
| m=mem ASSIGN e=expression SEMI { Set(m, e) }
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
| IF e=expression BEGIN i1=list(instruction) END ELSE BEGIN i2=list(instruction) END { If(e, i1, i2) }
| IF e=expression BEGIN i=list(instruction) END { Iff(e, i) }
| WHILE e=expression BEGIN i=list(instruction) END { While(e, i) }
| RETURN e=expression SEMI { Return(e) }
| e=expression SEMI { Expr(e) }
;

expression:
| m=mem { Get(m) }
| n=INT { Int(n) }
| TRUE { Bool(true) }
| FALSE { Bool(false) }
| e1=expression op=binop e2=expression { Binop(op, e1, e2) }
| up=unop e=expression { Unop(up, e) }
| LPAR e=expression RPAR { (e) }
(* new p; initialise les attributs de p par défaut *)
| NEW id=IDENT { New(id) }
(* new p(2, 1); initiliase les attributs de p à 2 et 1 *)
| NEW id=IDENT LPAR e=separated_list(COMMA, expression) RPAR { NewObj(id, e) }
(* newc p(2, 1); initialise les attributs de p par défaut, puis appelle la méthode nommée 'constructor' sur p *)
| NEWCSTR id=IDENT LPAR e=separated_list(COMMA, expression) RPAR  { NewCstr(id, e) }
| THIS { This }
| e=expression DOT id=IDENT LPAR args=separated_list(COMMA, expression) RPAR { MethCall(e, id, args) }
;

mem:
| id=IDENT { Var(id) }
| e=expression DOT id=IDENT { Field(e, id) }
;

param:
| type_var=var_typ id=IDENT { (id, type_var) }
;

%inline binop:
| PLUS { Add }
| STAR { Mul }
| MINUS { Sub }
| DIV { Div }
| MOD { Rem }
| EQ { Eq }
| NEQ { Neq }
| LT { Lt }
| LE { Le }
| GT { Gt }
| GE { Ge }
| AND { And }
| OR { Or }
| EQS { Eqs }
| NEQS { Neqs }
;

%inline unop:
| MINUS { Opp }
| NOT { Not }
;

%inline var_typ:
| DEFINT { TInt }
| DEFBOOL { TBool }
| DEFVOID { TVoid }
| id=IDENT { TClass(id) }
;
