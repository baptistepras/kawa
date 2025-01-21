{

  open Lexing
  open Kawaparser

  exception Error of string

  let keyword_or_ident =
  let h = Hashtbl.create 17 in
  List.iter (fun (s, k) -> Hashtbl.add h s k)
    [ "true",      TRUE;
      "false",     FALSE;
      "print",     PRINT;
      "main",      MAIN;
      "if",        IF;
      "else",      ELSE;
      "while",     WHILE;
      "return",    RETURN;
      "int",       DEFINT;
      "void",      DEFVOID;
      "bool",      DEFBOOL;
      "new",       NEW;
      "this",      THIS;
      "var",       VAR;
      "method",    METHOD;
      "class",     CLASS;
      "attribute", ATTRIBUTE;
      "extends",   EXTENDS;
      "newc",      NEWCSTR;

      (* Barba Extension *)
      "barbatrue",      TRUE;
      "barbafalse",     FALSE;
      "barbaprint",     PRINT;
      "barbamain",      MAIN;
      "barbaif",        IF;
      "barbaelse",      ELSE;
      "barbawhile",     WHILE;
      "barbareturn",    RETURN;
      "barbaint",       DEFINT;
      "barbavoid",      DEFVOID;
      "barbabool",      DEFBOOL;
      "barbanew",       NEW;
      "barbathis",      THIS;
      "barbavar",       VAR;
      "barbamethod",    METHOD;
      "barbaclass",     CLASS;
      "barbaattribute", ATTRIBUTE;
      "barbaextends",   EXTENDS;
      "barbanewc",      NEWCSTR;
    ] ;
  fun s ->
    try  Hashtbl.find h s
    with Not_found -> IDENT(s)
        
}

let digit = ['0'-'9']
let number = digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
let bool = ("true"|"false")
  
rule token = parse
  | ['\n']            { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  | "//" [^ '\n']* "\n"  { new_line lexbuf; token lexbuf }
  | "/*"                 { comment lexbuf; token lexbuf }

  | number as n  { INT(int_of_string n) }
  | ident as id  { keyword_or_ident id }
  | bool as s { BOOL(bool_of_string s) }

  | ";"  { SEMI }
  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { BEGIN }
  | "}"  { END }
  | "!" { NOT }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { DIV }
  | "%" { MOD }
  | "==" { EQ }
  | "!=" { NEQ }
  | "<" { LT }
  | "<=" { LE }
  | ">" { GT }
  | ">=" { GE }
  | "&&" { AND }
  | "||" { OR }
  | "=" { ASSIGN }
  | "." { DOT }
  | "," { COMMA }
  | "===" { EQS }
  | "=/=" { NEQS }

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { EOF }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
