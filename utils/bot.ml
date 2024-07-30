(* open Chess.Game *)
open Struct.Game
open Struct.State

(* Enhanced minimax function with alpha-beta pruning *)
let rec minimax_alpha_beta eval_function state current_player depth alpha beta =
  if depth <= 0 (*|| game.is_terminal*) then eval_function state
  else
    let copy = create_state_deep_copy state in
    if current_player = Struct.Macros.White then (
      (* Maximizing player *)
      let value = ref Float.neg_infinity in

      (* get all of the possible moves from a given position - may not be legal
         because king might still be in check *)
      let possible_moves = generate_moves state in

      let still_searching = ref true in
      let i = ref 0 in

      while !still_searching && !i < List.length possible_moves do
        List.iter
          (fun new_move ->
            if !still_searching then (
              let _, successor = make_move copy new_move in
              value :=
                max !value
                  (minimax_alpha_beta eval_function successor
                     Struct.Macros.Black (depth - 1) alpha beta);
              alpha := max !alpha !value;
              if beta <= alpha then still_searching := false))
          possible_moves;
        incr i
      done;
      !value)
    else
      (* Minimizing player *)
      let value = ref Float.infinity in

      let possible_moves = generate_moves state in

      (* let () = Printf.printf "This is the maximizing players moves\n\n\n\n"
         in let () = Struct.Game.print_board state in let () = Printf.printf "It
         is player %d move\n" (Struct.Macros.to_side Black) in let () =
         Struct.Game.print_moves possible_moves in let () = Printf.printf "
         possible moves = actual possible moves %b \n" (generate_moves state =
         possible_moves) in let () = Printf.printf "\nEnd of maximizing players
         moves\n\n\n\n\n" in *)
      let still_searching = ref true in
      let i = ref 0 in
      while !still_searching && !i < List.length possible_moves do
        List.iter
          (fun new_move ->
            if !still_searching then (
              let _, successor = make_move copy new_move in
              value :=
                min !value
                  (minimax_alpha_beta eval_function successor
                     Struct.Macros.White (depth - 1) alpha beta);
              beta := min !beta !value;
              if beta <= alpha then still_searching := false))
          possible_moves;
        incr i
      done;
      !value

(* end min max beta pruning algorithm *)

(**Module for a Chess Agent*)
module type ChessBot = sig
  (* val current_state : game Tree.t ref *)
  val max_depth : int
  val generate_successor : game_state -> Unsigned.UInt64.t -> bool * game_state
  val eval_function : game_state -> float
  val get_action : game_state -> Unsigned.UInt64.t option
end

(**Module to define settings for the agent*)
module type BotSettings = sig
  val max_depth : int
  (**The maximum depth for the bot in the minimax call.*)

  val eval_function : game_state -> float
  (**[eval_fctn game_state] scores the current game state.*)
end

(*Create ChessAgent from settings*)
module InitBotWithSettings (Settings : BotSettings) = struct
  (* let current_state = ref Settings.start_state *)
  let max_depth = Settings.max_depth
  let eval_function = Settings.eval_function

  (*generate a successor state*)
  let generate_successor state move =
    make_move (create_state_deep_copy state) move

  let get_action state =
    if state.side = White then
      failwith "Cannot get action when it is not bot's turn"
    else
      let possible_moves = generate_moves state in

      (* let () = List.iter (fun state -> print_endline
         (Unsigned.UInt64.to_string state)) possible_moves in *)
      if List.is_empty possible_moves then print_endline "NO LEGAL ACTIONS";

      let evaluate_move move =
        let _, successor =
          make_move (Struct.State.create_state_deep_copy state) move
        in
        minimax_alpha_beta eval_function successor Struct.Macros.White
          (max_depth - 1) (ref Float.infinity) (ref Float.neg_infinity)
      in
      let failure_case =
        (Unsigned.UInt64.of_int (int_of_float Float.infinity), Float.infinity)
      in
      let actions_with_scores =
        List.map
          (fun action ->
            try (action, evaluate_move action) with Failure _ -> failure_case)
          possible_moves
      in
      let best_move, _ =
        List.fold_left
          (fun (best_move, best_score) (move, score) ->
            if score < best_score then (Some move, score)
            else (best_move, best_score))
          (None, Float.infinity) actions_with_scores
      in
      best_move

  (* end functor *)
end
