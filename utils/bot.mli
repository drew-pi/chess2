open Struct.State

(** Module type representing a chess bot *)
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

end

module InitBotWithSettings : functor (_ : BotSettings) -> ChessBot
