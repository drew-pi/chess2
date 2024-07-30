(* open Chess.Game *)
open Struct.State

(**Module type representing a chess bot.*)
module type ChessBot = sig
  (* val current_state : game Tree.t ref *)
  val max_depth : int
  val generate_successor : game_state -> Unsigned.UInt64.t -> bool * game_state
  val eval_function : game_state -> float
  val get_action : game_state -> Unsigned.UInt64.t option
end

module type BotSettings = sig
  val max_depth : int
  (**Difficulty of the Bot (Easy, Medium, Hard).*)

  val eval_function : game_state -> float
  (**[eval_fctn game_state] scores the current game state.*)
end

module InitBotWithSettings : functor (_ : BotSettings) -> ChessBot
