%{
  open Ast  (* Importe le module Ast *)
%}

(* Définition des tokens *)
%token <int> NUMBER        (* Entier *)
%token <string> STRING     (* Chaîne de caractères *)
%token <char> VAR          (* Variable *)
%token EOF CR NL           (* Fin de fichier, retour chariot, nouvelle ligne *)
%token IMPRIME SI ALORS VAVERS ENTREE FIN REM  (* Mots-clés d'instructions *)
%token PLUS MOINS FOIS DIV EG PP PG DIF PPQ PGQ  (* Opérateurs *)
%token GPAREN DPAREN VIRG  (* Parenthèses et virgule *)

// %start programme
%start <Ast.programme> input  (* Définit le point de départ de l'analyse *)

%%

(* Définition de la grammaire *)

(* Règles pour le programme *)
input: p=programme EOF { p }

programme: ll=ligne_list { ll }

ligne_list:
  | l=ligne { [l] }
  | l=ligne CR ll=ligne_list { l :: ll }
  | l=ligne CR { [l] }

ligne: num=NUMBER i=instr { (num, i) }


(* Règles pour les instructions *)
instr:
  | IMPRIME e=expr_list { Imprime(e) }       (* Instruction d'impression *)
  | SI e1=expression r=relop e2=expression ALORS i=instr { Si(e1, r, e2, i) }  (* Instruction conditionnelle *)
  | VAVERS e=expression { VaVers(e) }        (* Instruction de saut *)
  | ENTREE vl=var_list { LireEntree(vl) }        (* Instruction de lecture d'entrée *)
  | v=VAR EG e=expression { Affectation(v, e) }   (* Instruction d'affectation *)
  | FIN { Fin }                               (* Fin de programme *)
  | REM c=STRING { Commentaire(c) }           (* Commentaire *)
  | NL { NouvelleLigne }                      (* Nouvelle ligne *)

(* Règles pour les listes d'expressions *)
expr_list:
  | e=expr { [e] }
  | e=expr VIRG el=expr_list { e :: el }

(* Règles pour les expressions *)
expr:
  | s=STRING { String(s) }                   (* Chaîne de caractères *)
  | e=expression { Expression(e) }           (* Expression *)

(* Règles pour les listes de variables *)
var_list:
  | v=var { [v] }
  | v=var VIRG vl=var_list { v :: vl }

(* Règles pour les variables *)
var: v=VAR { v }

(* Règles pour les expressions *)
expression:
| t=terme { (Plus t) :: [] }                     (* Terme seul *)
| PLUS t=terme { (Plus t) :: [] }                 (* Opération d'addition *)
| MOINS t=terme { (Moins t) :: [] }               (* Opération de soustraction *)
| t=terme l=operateur_terme_list { (Plus t) :: l }  (* Terme suivi d'une liste d'opérateurs *)
| PLUS t=terme l=operateur_terme_list { (Plus t) :: l }  (* Opération d'addition suivie d'une liste d'opérateurs *)
| MOINS t=terme l=operateur_terme_list { (Moins t) :: l }  (* Opération de soustraction suivie d'une liste d'opérateurs *)

(* Règles pour les listes d'opérateurs sur les termes *)
operateur_terme_list:
  | op=operateur_terme { [op] }                 (* Opérateur seul *)
  | PLUS t=terme l=operateur_terme_list { (Plus t) :: l }  (* Opération d'addition suivie d'une liste d'opérateurs *)
  | MOINS t=terme l=operateur_terme_list { (Moins t) :: l }  (* Opération de soustraction suivie d'une liste d'opérateurs *)

(* Règles pour les opérateurs sur les termes *)
operateur_terme:
 | t=terme { Plus t }                           (* Opération d'addition *)
 | PLUS t=terme { Plus t }                      (* Opération d'addition *)
 | MOINS t=terme { Moins t }                    (* Opération de soustraction *)

(* Règles pour les termes *)
terme:
  | f=facteur { (Unsigned f) :: [] }                    (* Facteur seul *)
  | f=facteur op=op_facteur_list { (Unsigned f) :: op }  (* Facteur suivi d'une liste d'opérateurs *)

(* Règles pour les listes d'opérateurs sur les facteurs *)
op_facteur_list:
  | op=op_facteur { [op] }                     (* Opérateur seul *)
  | FOIS f=facteur fl=op_facteur_list { (Mult f) :: fl }  (* Opérateur suivi d'une liste d'opérateurs *)
  | DIV f=facteur fl=op_facteur_list { (Div f) :: fl }  (* Opérateur suivi d'une liste d'opérateurs *)

(* Règles pour les opérateurs sur les facteurs *)
op_facteur:
  | f=facteur { Unsigned f }                   (* Opération d'addition *)
  | FOIS f=facteur { Mult f }             (* Opération de multiplication *)
  | DIV f=facteur { Div f }               (* Opération de division *)

(* Règles pour les facteurs *)
facteur:
  | n=NUMBER { IntFact(n) }                  (* Entier *)
  | v=VAR { VarFact(v) }                     (* Variable *)
  | GPAREN e=expression DPAREN { ExpressionFact(e) }  (* Expression imbriquée *)

(* Règles pour les opérateurs relationnels *)
relop:
  | EG { Egal }                              (* Égal *)
  | PP { PP }                                (* Plus petit *)
  | PG { PG }                                (* Plus grand *)
  | DIF { Dif }                              (* Différent *)
  | PPQ { PPQ }                              (* Plus petit ou égal *)
  | PGQ { PGQ }                              (* Plus grand ou égal *)
