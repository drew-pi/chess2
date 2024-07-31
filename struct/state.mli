open Unsigned.UInt64
open Macros

type game_state = {

  bitboards : t array;
    (** [bitboards] holds all of the piece information. It has length [12] and maps to the pieces with the following
    - white Pawn 0
    - white kNight 1
    - white Bishop 2
    - white Rook 3
    - white Queen 4
    - white King 5
    - Black pawn 6
    - Black knight 7
    - Black bishop 8
    - Black rook 9
    - Black queen 10
    - Black king 11
 *)

  occupancies : t array;
    (** [occupancies] holds all of general bitboards - including only black pieces, only white pieces and both black and white pieces 
    - white occupancy 0
    - black occupancy 1 
    - both occupancy 2
  *)

  mutable side : sides;
  (** [side] defines whos turn it is. [Neither] by default, but after initialization only [White] or [Black]*)

  mutable enpassent : int;
  (** [enpassent] holds the coordinate for where a possible enpassent may occur*)

  mutable castling_right : t;
    (** [castling_right] holds all of the castling rights for this board, uses the below mapping to encode this
    
    bin  dec
      
    0001    1  white king can castle to the king side

    0010    2  white king can castle to the queen side

    0100    4  black king can castle to the king side

    1000    8  black king can castle to the queen side

    examples

    1111       both sides an castle both directions

    1001       black king => queen side and white king => king side
  *)

}

val default_state : game_state
(** A default state with everything set to unverified values *)

val create_state_deep_copy : game_state -> game_state
(** [create_state_deep_copy state] Creates a deep copy of the [state] so that there is not any accidental array overwrites *)