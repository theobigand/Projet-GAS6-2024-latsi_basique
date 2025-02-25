{
  open Parser (* Importe les tokens définis dans le parser *)
}

(* Définition des lexèmes *)

let digit = ['0'-'9']         (* Définition des chiffres *)
let letter = ['A'-'Z']        (* Définition des lettres majuscules *)
let var = letter               (* Les variables sont des lettres majuscules *)

let nombre = digit+            (* Les nombres sont une séquence de chiffres *)

rule main = parse
  | [' ' '\t']                { main lexbuf } (* Ignorer les espaces et tabulations *)
  | '\n'                      { CR }          (* Nouvelle ligne *)
  | "NL"                      { NL }          (* Token pour une nouvelle ligne *)
  | "IMPRIME"                 { IMPRIME }     (* Token pour l'instruction IMPRIME *)
  | "SI"                      { SI }          (* Token pour l'instruction SI *)
  | "ALORS"                   { ALORS }       (* Token pour l'instruction ALORS *)
  | "VAVERS"                  { VAVERS }      (* Token pour l'instruction VAVARS *)
  | "ENTREE"                  { ENTREE }      (* Token pour l'instruction ENTREE *)
  | "FIN"                     { FIN }         (* Token pour l'instruction FIN *)
  | "REM"                     { REM }         (* Token pour l'instruction REM *)
  | '+'                       { PLUS }        (* Token pour l'opérateur + *)
  | '-'                       { MOINS }       (* Token pour l'opérateur - *)
  | '*'                       { FOIS }        (* Token pour l'opérateur * *)
  | '/'                       { DIV }         (* Token pour l'opérateur / *)
  | '='                       { EG }          (* Token pour l'opérateur = *)
  | '<'                       { PP }          (* Token pour l'opérateur < *)
  | '>'                       { PG }          (* Token pour l'opérateur > *)
  | "<>" | "><"               { DIF }         (* Token pour l'opérateur <> *)
  | "<="                      { PPQ }         (* Token pour l'opérateur <= *)
  | ">="                      { PGQ }         (* Token pour l'opérateur >= *)
  | '('                       { GPAREN }      (* Token pour la parenthèse ouvrante *)
  | ')'                       { DPAREN }      (* Token pour la parenthèse fermante *)
  | ','                       { VIRG }        (* Token pour la virgule *)
  |'"' ([^ '"' '0' - '9']* as s) '"' { STRING s }       (*Token pour une chaîne de caractères*)
  | var as v                  { VAR (v) }     (* Token pour une variable *)
  | nombre as n               { NUMBER (int_of_string n) }  (* Token pour un nombre *)
  | eof                       { EOF }         (* Fin de fichier *)
  | _                         { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }  (* Caractère inattendu *)
