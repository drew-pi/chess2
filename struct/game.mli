open State
open Unsigned.UInt64

val setup_board : unit -> game_state
(** Setup the default chess board, whites turn to move and all castling rights
    at default *)

val setup_tricky : unit -> game_state
(** Sets up a specific chess board setup *)

val enpassent_mate : unit -> game_state
(** A complex one move checkmate for white *)

val crazy_mate : unit -> game_state
(** A complex one move checkmate for white - not a realistic scenario *)

val king_in_check : game_state -> bool
(** [king_in_check game_state] returns true if the king is in check, false otherwise *)

val filter_psuedolegal_moves : t list -> game_state -> t list
(** [filter_psuedolegal_moves psuedo_legal_moves current_state] filters out all moves in [psuedo_legal_moves] that leave the king in check when applied to [current_state] *)

val generate_moves : game_state -> t list
(** [generate_moves state] generates all of the possible psuedo legal moves from a given [state] - i.e king can be captured and concept of check does not exist *)

val generate_legal_moves : game_state -> t list
(** [generate_legal_moves state] generates all of the possible legal moves from a given [state] - i.e king cannot be captured and the concept of check is enforced *)

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

val check_reason_gameover : game_state -> Macros.reason
(** [check_reason_gameover state] checks to see what the reason for the game ending is *)