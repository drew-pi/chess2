open Unsigned.UInt64


val bit_exists : t -> int -> bool
(** If there is a bit at [coord] where [0 <= coord <= 63]*)

val one_or_zero : t -> int -> int
(** One or zero at the coordinate position*)

val get_coord : int -> int -> int
(** Get a 1D coordinate value from the rank file coord system*)

val set_bit : t -> int -> t
(** [set_bit bitboard coord] sets the specified at [coord] bit in the [bitboard] *)

val pop_bit: t -> int -> t
(** [pop_bit bb coord] removes the specified bit at [coord] in the [bitboard] *) 

val popcount : t -> int
(** Popcount returns the number of bits in the integer
     - Taken from https://github.com/janestreet/base/blob/master/src/popcount.ml which is a slow implementation since does not use hardware optimization (i.e C stubs) 
*)

val bitscan : t -> int
(** Finds the least significant bit in the integer and returns its index in base 2
    
Uses the Martin Läuter (1997) algorithm to index the first 1 in the integer*)

val encode_move : int -> int -> int -> int -> t -> t -> t -> t -> t
(** [encode_move source target piece promoted captured double enpassent castling] encodes a move for the computer to understand how change the board 
    - [source] is where the piece originates from [0 <= source <= 63]
    - [target] is the target square [0 <= source <= 63]
    - [piece] is the piece that is being described [0 <= piece <= 11]
    - [promoted] is the piece that the promoted pawn will turn into [0 <= piece <= 11] 
    - [captured], [double], [enpassent], and [castling] are binary, represented by either [UInt64.one] or [UInt64.one]
*)

val get_source_coord : t -> int
(** [get_source_coord encoded_move] returns the source coordinate of an encoded move *)

val get_target_coord : t -> int
(** [get_target_coord encoded_move] returns the coordinate that is being targeted by the encoded move *)

val get_encoded_piece : t -> int
(** [get_encoded_piece encoded_move] returns the piece whose attack is being encoded by the move *)

val get_promoted_piece : t -> int
(** [get_promoted_piece encoded_move] returns the piece that will replace the soon to be promoted pawn. [-1] if no promotion to represent an empty space   *)

val capture_move : t -> bool
(** [capture_move encoded_move] returns true if the current move is a capture, false otherwise *)

val get_double_push : t -> bool
(** [get_double_push encoded_move] returns true if the this is a double push move *)

val get_enpassent_move : t -> bool
(** [get_enpassent_move encoded_move] returns true if the this is an enpassent move *)

val get_castle_move : t -> bool
(** [get_castle_move encoded_move] returns true if the this is a castle move *)

val wk : t
(** 0001 white king can castle to the king side *)

val wq : t
(* 0010 white king can castle to the queen side*)

val bk : t
(* 0100 black king can castle to the king side *)

val bq : t
(* 1000 black king can castle to the queen side *)



(** A type that represents the coordinate chess plane A1 - H8*)
type rankfile = 
| A1                              
| B1
| C1
| D1
| E1
| F1
| G1
| H1
| A2
| B2
| C2
| D2
| E2
| F2
| G2
| H2
| A3
| B3
| C3
| D3
| E3
| F3
| G3
| H3
| A4
| B4
| C4
| D4
| E4
| F4
| G4
| H4
| A5
| B5
| C5
| D5
| E5
| F5
| G5
| H5
| A6
| B6
| C6
| D6
| E6
| F6
| G6
| H6
| A7
| B7
| C7
| D7
| E7
| F7
| G7
| H7
| A8
| B8
| C8
| D8
| E8
| F8
| G8
| H8

(** Converts a rankfile value [A1 - H8] to a numerical 64 bit index [0-63]*)
val to_coord : rankfile -> int

val from_coord : int -> string
(** Converts a numerical coordinate value [0 <= c <= 63] to a rankfile value [A1 - H8]*)

val to_coord_rf : rankfile -> int * int
(** Converts a rankfile value [A1 - H8] to their rank and file equivalents [(0,0) - (7,7)]*)

val string_to_rankfile : string -> rankfile
(** Converts a string ["A1" - "H8"] to a rankfile [A1 - H8], fails if not a proper rankfile string *)

val from_coord_rf : int -> int * int
(** Converts a coordinate value [0-63] to its [(rank, file)] occupancy form *)

type sides = | White | Black | Both | Neither
(** The type that holds the side information*)

val to_side : sides -> int
(** Function that turns side into the proper integer representation*)

type game_pieces = P | N | B | R | Q | K | Bp | Bn | Bb | Br | Bq | Bk | E
(** The game pieces for black and white - uppercase is white, lowercase is black *)

val to_board : game_pieces -> int
(** Converts a game piece to its index in the bitboards for a game state [0-11]*)

val to_piece : int -> string
(** Converts an int to its string representation *)

type pieces =   | Pawn
| Rook
| Knight
| Bishop
| King
| Queen
(** Representation of the different pieces in chess - primarily for bishop, rook and queen attacks *)

val print_bitboard : t -> unit
(** A visual representation of a u64int as a chess board *)

val random_uint64 : unit -> t
(** Returns a random 64 bit unsigned integer using the built in Random library *)

val pow : int -> int -> int
(** [pow n exp] takes n to the exp times (exp must be positive), if the number is too big will overflow integer bounds and return 0*)



