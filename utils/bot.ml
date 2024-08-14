open Struct.Game
open Struct.State

(* Enhanced minimax function with alpha-beta pruning *)
let rec minimax_alpha_beta eval_function state depth alpha beta =
  if depth <= 0 then (eval_function state, None)
  else
    let copy = create_state_deep_copy state in

    (* generate psuedo legal moves - can capture king *)
    (* let possible_moves = generate_moves state in  *)
    let possible_moves = generate_legal_moves state in

    (* Maximizing player *)
    if state.side = Struct.Macros.White then

      (* if checkmate end the evaluation of branches of this move and return a very low evalution score for maximizing player *)
      if ((List.length possible_moves = 0) && (king_in_check copy)) then ((-500000.), None) else

      (* the best possible move, set it to negitive infinity because anything will be larger than this number *)
      let best_move_white = ref (Float.neg_infinity, None) in

      let rec maximizing_move_iter alpha beta = function 
        | [] -> !best_move_white
        | move :: moves -> 
          let _, new_state, _ = make_move copy move in
          let value, _ = minimax_alpha_beta eval new_state (depth-1) alpha beta in

          (* Check to see if this move maximizes the evaluation function more than all previous moves *)
          let () = if (value > fst !best_move_white) then best_move_white := (value, Some move) else () in

          (* if the value is smaller than alpha than found our move and must exit out of the loop *)
          if (value >= beta) then 
            !best_move_white else

          (* alpha becomes the max value of value and the previous alpha *)
          maximizing_move_iter (max value alpha) beta moves
        
        in maximizing_move_iter alpha beta possible_moves 
    
    (* Minimizing player *)
    else 

      (* if checkmate end the evaluation of branches of this move and return a very low evalution score for minimizing   player *)
      if ((List.length possible_moves = 0) && (king_in_check copy)) then ((500000.), None) else

      let best_move_black = ref (Float.infinity, None) in

      let rec minimizing_move_iter alpha beta = function 
      | [] -> !best_move_black
      | move :: moves -> 
        let _, new_state, _ = make_move copy move in
        let value, _ = minimax_alpha_beta eval new_state (depth-1) alpha beta in

        (* Check to see if this move minimizes the evaluation function more than all previous moves *)
        let () = if (value < fst !best_move_black) then best_move_black := (value, Some move) else () in

        (* if the value is smaller than alpha than found our move and must exit out of the loop *)
        if (value <= alpha) then !best_move_black else
        
        (* beta becomes the minimum value of value and the previous beta *)
        minimizing_move_iter alpha (min value beta)  moves
      
      in minimizing_move_iter alpha beta possible_moves
(* end min max beta pruning algorithm *)





(** Module for a Chess Agent*)
module type ChessBot = sig
  val max_depth : int
  (** [max_depth] is an odd number that controls how deep the minmax algorithm searches into the game tree *)

  val make_move : game_state -> Unsigned.UInt64.t -> bool * game_state * game_state
  (** [make_move game_state move] is the mechanism of making a move. The new game_state that it returns must switch player turns automatically  *)

  val eval_function : game_state -> float
  (** [eval_function state] returns a static number which determines how good of a position the two players are in *)
  
  val get_next_move : game_state -> Unsigned.UInt64.t
  (** [get_next_move state] returns the next move which the player should make. Uses the alpha-beta pruning minmax algorithm to determine which move is the best *)
end

(** Module to define settings for the agent*)
module type BotSettings = sig
  val max_depth : int
  (**The maximum depth for the bot in the minimax call.*)
end

(*Create ChessAgent from settings*)
module InitBotWithSettings (Settings : BotSettings) = struct
  let max_depth = Settings.max_depth
  let eval_function = Struct.Game.eval

  let make_move state move =
    make_move (create_state_deep_copy state) move

  let get_next_move state = 
    (* let _, best_move = minimax_alpha_beta eval_function state max_depth (ref Float.neg_infinity) (ref Float.infinity) in *)
    let _, best_move = minimax_alpha_beta eval_function state max_depth Float.neg_infinity Float.infinity in


    match best_move with 
    | None -> failwith "An error occured and the bot could not find the next move"
    | Some m -> m

  (* end functor *)
end
