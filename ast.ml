(* Définition du type pour représenter une variable *)
type var = char

(* Définition des opérateurs relationnels *)
type relop =
  | PP    (* Plus petit *)
  | PG    (* Plus grand *)
  | Egal  (* Égal *)
  | Dif   (* Différent *)
  | PPQ   (* Plus petit ou égal *)
  | PGQ   (* Plus grand ou égal *)

(* Définition du type pour représenter une expression dans une instruction *)
type expr =
  | String of string        (* Chaîne de caractères *)
  | Expression of expression  (* Expression *)

(* Définition du type pour représenter une expression *)
and expression = op_term list

(* Définition des opérateurs sur les termes *)
and op_term =
  | Plus of terme   (* Opération d'addition *)
  | Moins of terme  (* Opération de soustraction *)

(* Définition du type pour représenter un terme *)
and terme = op_facteur list  (* Liste d'opérateurs sur les facteurs *)

(* Définition des opérateurs sur les facteurs *)
and op_facteur =
  | Unsigned of facteur (* Opération non signée *)
  | Mult of facteur  (* Opération de multiplication *)
  | Div  of facteur (* Opération de division *)

(* Définition du type pour représenter un facteur *)
and facteur =
  | VarFact of var           (* Variable *)
  | IntFact of int           (* Entier *)
  | ExpressionFact of expression  (* Expression imbriquée *)


(* Définition du type pour représenter une liste de variables *)
type var_list = var list

(* Définition du type pour représenter une liste d'expressions *)
type expr_list = expr list

(* Définition du type pour représenter une instruction *)
type instr =
  | Imprime of expr_list          (* Instruction d'impression *)
  | Si of expression * relop * expression * instr  (* Instruction conditionnelle *)
  | VaVers of expression          (* Instruction de saut *)
  | LireEntree of var_list           (* Instruction de lecture d'entrée *)
  | Affectation of var * expression   (* Instruction d'affectation *)
  | Fin                          (* Fin de programme *)
  | Commentaire of string        (* Commentaire *)
  | NouvelleLigne                (* Nouvelle ligne *)

(* Définition du type pour représenter une ligne d'instruction *)
type ligne = int * instr

(* Définition du type pour représenter un programme (ensemble de lignes d'instructions) *)
type programme = ligne list


(* Fonction pour convertir une ligne en une chaîne de caractères *)
let rec line_to_string = function
  | (n, instr) -> string_of_int n ^ " " ^ (instruction_to_string instr)

(* Fonction pour convertir un facteur en une chaîne de caractères *)
and facteur_to_string = function
  | VarFact v -> String.make 1 v
  | IntFact n -> string_of_int n
  | ExpressionFact e -> expression_to_string e

and facteur_operateur_to_string = function
  | Unsigned f -> facteur_to_string f
  | Mult f -> " * " ^ facteur_to_string f
  | Div f -> " / " ^ facteur_to_string f

(* Fonction pour convertir une liste d'opérateurs sur les facteurs en une chaîne de caractères *)
and facteur_operateur_list_to_string = function
  | [] -> ""
  | op :: ops -> facteur_operateur_to_string op ^ facteur_operateur_list_to_string ops

(* Fonction pour convertir un terme en une chaîne de caractères *)
and terme_to_string = function
  | [] -> ""
  | f :: fs -> facteur_operateur_to_string f ^ facteur_operateur_list_to_string fs

(* Fonction pour convertir une liste d'opérateurs sur les termes en une chaîne de caractères *)

(* Fonction pour convertir un opérateur sur les termes en une chaîne de caractères *)
and terme_operateur_to_string = function
  | Plus t -> " + " ^ terme_to_string t
  | Moins t -> " - " ^ terme_to_string t

(* Fonction pour convertir une expression en une chaîne de caractères *)
and expression_to_string = function
  | [] -> ""
  | exp :: expl -> terme_operateur_to_string exp ^ expression_to_string expl

(* Fonction pour convertir une expression en une chaîne de caractères *)
and expr_element_to_string = function
  | String s -> "\"" ^ s ^ "\""
  | Expression e -> expression_to_string e

(* Fonction pour convertir une liste d'expressions en une chaîne de caractères *)
and expr_element_list_to_string = function
  | [] -> ""
  | exp :: expl -> expr_element_to_string exp ^ " , " ^ expr_element_list_to_string expl

(* Fonction pour convertir une liste de variables en une chaîne de caractères *)
and variable_list_to_string = function
  | x::y::xs -> (String.make 1 x) ^ ", " ^ variable_list_to_string (y::xs)
  | x::[] -> (String.make 1 x)
  | _ -> ""

(* Fonction pour convertir un opérateur relationnel en une chaîne de caractères *)
and relop_to_string = function
  | PP -> " < "
  | PG -> " > "
  | Egal -> " = "
  | Dif -> " <> "
  | PPQ -> " <= "
  | PGQ -> " >= "

(* Fonction pour convertir une instruction en une chaîne de caractères *)
and instruction_to_string = function
  | Imprime l -> "IMPRIME : " ^ (expr_element_list_to_string l)
  | Si (exp, op, exp2, instr) ->
      " SI " ^ (expression_to_string exp) ^ (relop_to_string op) ^ (expression_to_string exp2) ^ " ALORS " ^ (instruction_to_string instr)
  | VaVers n -> "VAVERS " ^ (expression_to_string n)
  | LireEntree var_l -> "ENTREE : " ^ variable_list_to_string var_l
  | Affectation(var,exp) -> (String.make 1 var) ^ " = " ^ (expression_to_string exp)
  | Fin -> "FIN"
  | Commentaire s -> "REM " ^ s
  | NouvelleLigne -> "NL"
