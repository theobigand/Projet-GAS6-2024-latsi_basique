open Ast  (* Ouverture du module qui contient les définitions de l'AST *)

(* Type pour représenter les raisons de sortie de programme *)
type exit_reason =
  | Finished  (* Programme terminé *)
  | EndOfInput  (* Fin de l'entrée *)

(* Exception pour signaler une sortie de programme *)
exception Exit of exit_reason

(* Type pour le contexte d'exécution *)
type info_execution = {
  current_line: int;  (* Numéro de ligne courante *)
  variables: int array  (* Tableau de 26 entiers pour les variables A à Z *)
}

(* Fonction pour créer un contexte initial *)
let create_initial_info () =
  { current_line = 0; variables = Array.make 26 0 }

(* Fonction pour obtenir l'index d'une variable (A -> 0, B -> 1, etc.) *)
let get_variable_index variable = int_of_char variable - int_of_char 'A'

(* Fonction pour obtenir la valeur d'une variable dans le contexte *)
let get_variable_value info variable = info.variables.(get_variable_index variable)

(* Fonction pour mettre à jour la valeur d'une variable dans le contexte *)
let set_variable_value info variable value = info.variables.(get_variable_index variable) <- value

(* Fonction pour mettre à jour le numéro de ligne dans le contexte *)
let set_current_line info line_number = { info with current_line = line_number }

(* Fonction récursive pour évaluer une expression *)
let rec evaluate_expression info = function
  | [] -> 0
  | Plus term :: rest -> evaluate_term info term + evaluate_expression info rest
  | Moins term :: rest -> - evaluate_term info term + evaluate_expression info rest

(* Fonction pour évaluer un terme *)
and evaluate_term info = function
  | [] -> 1
  | Mult factor :: rest -> evaluate_factor info factor * evaluate_term info rest
  | Div factor :: rest -> evaluate_factor info factor / evaluate_term info rest
  | Unsigned factor :: rest -> evaluate_factor info factor * evaluate_term info rest

(* Fonction pour évaluer un facteur *)
and evaluate_factor info = function
  | IntFact value -> value
  | VarFact variable -> get_variable_value info variable
  | ExpressionFact expr -> evaluate_expression info expr

(* Fonction récursive pour exécuter une instruction *)
let rec execute_instruction info = function
  | Imprime elements ->
    (* Imprimer une liste d'éléments *)
    List.iter
      (function
        | String s -> print_string s
        | Expression e -> print_int (evaluate_expression info e))
      elements;
      set_current_line info (info.current_line + 1)
  | Si (expr1, operator, expr2, branch) ->
    (* Exécuter une condition *)
    let value1, value2 = evaluate_expression info expr1, evaluate_expression info expr2 in
    let condition_met = match operator with
      | PP -> value1 < value2
      | PG -> value1 > value2
      | Egal -> value1 = value2
      | Dif -> value1 <> value2
      | PPQ -> value1 <= value2
      | PGQ -> value1 >= value2
    in
    if condition_met then execute_instruction info branch
    else set_current_line info (info.current_line + 1)
  | VaVers line_number ->
    (* Changer de ligne *)
    set_current_line info (evaluate_expression info line_number)
  | LireEntree variables ->
    (* Lire les valeurs d'entrée pour les variables *)
    List.iter
      (fun variable ->
          let rec read_value () =
            print_string ("Valeur pour la variable " ^ String.make 1 variable ^ " : ");
            try
              let value = read_int () in
              set_variable_value info variable value
            with
            | Failure _ ->
                print_endline "La valeur doit être un entier. Réessayez.";
                read_value ()
          in
          read_value ())
      variables;
      set_current_line info (info.current_line + 1)
  | Affectation (variable, expr) ->
    (* Affecter une valeur à une variable *)
    set_variable_value info variable (evaluate_expression info expr);
      set_current_line info (info.current_line + 1)
  | Fin -> raise (Exit Finished)  (* Terminer le programme *)
  | NouvelleLigne -> print_newline (); set_current_line info (info.current_line + 1)  (* Imprimer une nouvelle ligne *)
  | Commentaire _ -> set_current_line info (info.current_line + 1)  (* Ignorer un commentaire *)

(* Fonction pour trouver la prochaine ligne d'instruction *)
let find_next_line current_line program =
  List.find_opt (fun (line_number, _) -> line_number >= current_line) program

(* Fonction pour exécuter une étape du programme *)
let execute_step program info =
  match find_next_line info.current_line program with
  | Some (next_line_number, instruction) ->
    let info = { info with current_line = next_line_number } in
    execute_instruction info instruction
  | None -> raise (Exit EndOfInput)

(* Fonction pour évaluer un programme complet *)
let evaluate_program program =
  let initial_context = create_initial_info () in
  let rec loop info =
    try loop (execute_step program info)
    with
    | Exit Finished | Exit EndOfInput -> ()
  in
  loop initial_context

(* Analyse syntaxique du programme depuis l'entrée standard ou un fichier *)
let ast =
  if Array.length Sys.argv > 1 then
    (* Initialisation du tampon lexical pour lire depuis un fichier *)
    Parser.input Lexer.main (Lexing.from_channel (open_in Sys.argv.(1)))
  else
    (* Initialisation du tampon lexical pour lire depuis l'entrée standard *)
    Parser.input Lexer.main (Lexing.from_channel stdin)

(* Exécution du programme analysé *)
let _ = evaluate_program ast; print_newline ()
