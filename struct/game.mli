open State
open Unsigned.UInt64

val setup_board : unit -> game_state
(** Setup the default chess board, whites turn to move and all castling rights
    at default *)

val setup_tricky : unit -> game_state
(** Sets up a specific chess board setup *)

val generate_moves : game_state -> t list
(** [generate_moves state] generates all of the possible moves from a given [state] *)

val print_moves : t list -> unit
(** [print_moves move_list] prints a stylized representation of all of the possible moves given a move list *)

val make_move : game_state -> t -> bool * game_state * game_state
(** [make_move state move] actually executes the move that is specified and then
    returns [(bool * new_state * old_state)] where [bool] states whether the move is legal or not *)

val eval : game_state -> float
(** [eval state] is a simple evalution function for a given game state, by
    weighing the importance of each piece and then summing a players given
    position value
    - returns [white_pos - black_pos] so white trying to maximize the integer,
      while black is trying to minimize *)