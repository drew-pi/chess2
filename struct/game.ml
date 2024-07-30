open Unsigned.UInt64
open Macros
open King_attack
open Knight_attack
open Pawn_attacks
open Bishop_attack
open Rook_attack
open Slide_attack
open State

(* castling move in right update binary decimal

   king & rooks didn't move: 1111 & 1111 = 1111 15

   white king moved: 1111 & 1100 = 1100 12 white king's rook moved: 1111 & 1110
   = 1110 14 white queen's rook moved: 1111 & 1101 = 1101 13

   black king moved: 1111 & 0011 = 1011 3 black king's rook moved: 1111 & 1011 =
   1011 11 black queen's rook moved: 1111 & 0111 = 0111 7 *)

(** castling rights update constants *)
let castling_rights =
  [|
    7;
    15;
    15;
    15;
    3;
    15;
    15;
    11;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    15;
    13;
    15;
    15;
    15;
    12;
    15;
    15;
    14;
  |]

(** Initialize all attack tables (except queen), returns it in the form
    [(king, knight, pawn, bishop, rook)]*)
let initialize_all () =
  let king_attack_table = init_king_attacks (Array.make 64 zero) in

  let knight_attack_table = init_knight_attacks (Array.make 64 zero) in

  let pawn_attack_table = init_pawn_attacks (Array.make_matrix 2 64 zero) in

  let bishop_attack_table, rook_attack_table = initialize_slide_attacks () in

  ( king_attack_table,
    knight_attack_table,
    pawn_attack_table,
    bishop_attack_table,
    rook_attack_table )

let ( king_attack_table,
      knight_attack_table,
      pawn_attack_table,
      bishop_attack_table,
      rook_attack_table ) =
  initialize_all ()

(* [get_pawn_attack side coord] returns the pawn attack for its
    [0 <= coord <= 63] and [side] *)
(* let get_pawn_attack side coord = pawn_attack_table.(to_side side).(coord) *)

(* [get_knight_attack coord] returns the knight attack for its
    [0 <= coord <= 63]*)
(* let get_knight_attack coord = knight_attack_table.(coord) *)

(** [get_bishop_attack coord blocker_occupancy] returns the bishop attack for
    its [0 <= coord <= 63] and [occupancy] (which is just all other pieces on
    the board)*)
let get_bishop_attack coord occ =
  let blocker = logand occ bishop_mask.(coord) in
  (* let () = print_bitboard blocker in let () = print_bitboard
     bishop_mask.(coord) in *)
  let magic = bishop_magics.(coord) in
  let rel_bits = bishops_relevent_possible_moves.(coord) in
  (* let hash_index = hash_function magic blocker rel_bits in *)
  (* let () = Printf.printf " %d in %d \n\n" hash_index (Array.length
     bishop_attack_table.(coord)) in *)
  access_attack magic blocker rel_bits bishop_attack_table.(coord)

(** [get_rook_attack coord blocker_occupancy] returns the rook attack for its
    [0 <= coord <= 63] and [occupancy] (which is just all other pieces on the
    board)*)
let get_rook_attack coord occ =
  (* let () = print_endline "does this even run" in *)
  let blocker = logand occ rook_mask.(coord) in
  (* let () = print_bitboard blocker in *)
  let magic = rook_magics.(coord) in
  let rel_bits = rooks_relevent_possible_moves.(coord) in
  access_attack magic blocker rel_bits rook_attack_table.(coord)

(** [get_queen_attack coord blocker_occupancy] returns the queen attack for its
    [0 <= coord <= 63] and [occupancy] (which is just all other pieces on the
    board)
    - just a logical or operation for the rook attack and bishop attack for the
      given blocker occupation*)
let get_queen_attack coord occ =
  let b_attack = get_bishop_attack coord occ in
  let r_attack = get_rook_attack coord occ in
  logor b_attack r_attack

(* let rec check_all_pieces bitboards coord piece =
  if piece > 11 then "."
  else if bit_exists bitboards.(piece) coord then to_piece piece
  else check_all_pieces bitboards coord (piece + 1) *)


(* let print_board state = 

   let bitboards = state.bitboards in
   let side = state.side in
   let enpassent = state.enpassent in

  let () =
    for i = 0 to 63 do
      let r, f = from_coord_rf i in
      let () = if f = 0 then Printf.printf "\n\t%d   " (8 - r) else () in
      Printf.printf " %s " (check_all_pieces bitboards i 0)
    done
  in
  let () = Printf.printf "\n\n\t     a  b  c  d  e  f  g  h\n\n" in
  let () = Printf.printf "\t      Side: %d \n\n" (to_side side) in
  Printf.printf "\t      En passent: %s \n" (from_coord enpassent) *)



(** [update_occupancies piece_bitboards occupancies] updates all of the occupancies with the pieces*)
let update_occupancies piece_bb occs =  

   let rec white_looper i occ = 
      if i > 5 then occ else white_looper (i+1) (logor piece_bb.(i) occ)
   in
   let white_occs = white_looper 0 zero in

   let () = print_bitboard white_occs in

  let rec black_looper i occ =
    if i > 11 then occ else let () = print_bitboard occ in black_looper (i + 1) (logor piece_bb.(i) occ)
  in
  let black_occs = black_looper 6 zero in

  let () = print_bitboard black_occs in

   (* set white pieces *)
   let () = occs.(to_side White) <- white_occs in
   let () = occs.(to_side Black) <- black_occs in
   (* let () = occs.(to_side Both) <- logor occs.(to_side White) occs.(to_side Black) in *)
   let () = occs.(to_side Both) <- logor white_occs black_occs in

   let () = print_bitboard occs.(2) in

   ()


let setup_board () =
  let state = create_state_deep_copy default_state in

  let bitboards = state.bitboards in
  let occupancies = state.occupancies in

  (* WHITE PIECES *)

  (* setup white pawns *)
  let () = bitboards.(to_board P) <- of_string "71776119061217280" in

  (* setup white knights *)
  let () = bitboards.(to_board N) <- of_string "4755801206503243776" in

  (* setup white bishops *)
  let () = bitboards.(to_board B) <- of_string "2594073385365405696" in

  (* setup white rooks *)
  let () = bitboards.(to_board R) <- of_string "9295429630892703744" in

  (* setup white queens *)
  let () = bitboards.(to_board Q) <- of_string "576460752303423488" in

  (* setup white kings *)
  let () = bitboards.(to_board K) <- of_string "1152921504606846976" in

  (* BLACK PIECES *)

  (* setup black pawns *)
  let () = bitboards.(to_board Bp) <- of_string "65280" in

  (* setup black knights *)
  let () = bitboards.(to_board Bn) <- of_string "66" in

  (* setup black bishops *)
  let () = bitboards.(to_board Bb) <- of_string "36" in

  (* setup black rooks *)
  let () = bitboards.(to_board Br) <- of_string "129" in

  (* setup black queens *)
  let () = bitboards.(to_board Bq) <- of_string "8" in

  (* setup black kings *)
  let () = bitboards.(to_board Bk) <- of_string "16" in

  (* SETTING OCCUPANCIES *)

  (* let () = occupancies.(to_side White) <- of_string "18446462598732840960" in

     let () = occupancies.(to_side Black) <- of_string "65535" in

     let () = occupancies.(to_side Both) <- logor occupancies.(to_side White)
     occupancies.(to_side Black) in *)
  let () = update_occupancies bitboards occupancies in

  (* SETTING OTHERs *)
  let () = state.castling_right <- of_int 15 in

  let () = state.side <- White in

  let () = state.enpassent <- ~-1 in

  state

let setup_tricky () =
  let state = create_state_deep_copy default_state in

  let bitboards = state.bitboards in
  let occupancies = state.occupancies in

  (* setup white pawns *)
  let () = bitboards.(to_board P) <- of_string "65020788473856000" in

  (* setup white knights *)
  let () = bitboards.(to_board N) <- of_string "4398314946560" in

  (* setup white bishops *)
  let () = bitboards.(to_board B) <- of_string "6755399441055744" in

  (* setup white rooks *)
  let () = bitboards.(to_board R) <- of_string "9295429630892703744" in

  (* setup white queens *)
  let () = bitboards.(to_board Q) <- of_string "35184372088832" in

  (* setup white kings *)
  let () = bitboards.(to_board K) <- of_string "1152921504606846976" in

  (* BLACK PIECES *)

  (* setup black pawns *)
  let () = bitboards.(to_board Bp) <- of_string "140746083544320" in

  (* setup black knights *)
  let () = bitboards.(to_board Bn) <- of_string "2228224" in

  (* setup black bishops *)
  let () = bitboards.(to_board Bb) <- of_string "81920" in

  (* setup black rooks *)
  let () = bitboards.(to_board Br) <- of_string "129" in

  (* setup black queens *)
  let () = bitboards.(to_board Bq) <- of_string "4096" in

  (* setup black kings *)
  let () = bitboards.(to_board Bk) <- of_string "16" in

  (* SETTING OCCUPANCIES *)

  (* let () = occupancies.(to_side White) <- of_string "10520166906101497856" in

     let () = occupancies.(to_side Black) <- of_string "140746085858705" in

     let () = occupancies.(to_side Both) <- logor occupancies.(to_side White)
     occupancies.(to_side Black) in *)
  let () = update_occupancies bitboards occupancies in

  (* SETTING OTHERs *)
  let () = state.castling_right <- of_int 15 in

  let () = state.side <- White in

  let () = state.enpassent <- ~-1 in

  state

(** [square_attacked coord side game_state] checks to see if [coord] is attacked given a [game_state] *)
(* let square_attacked coord game_state =

  let piece_bitboards = game_state.bitboards in
  let occupancies = game_state.occupancies in
  let side = game_state.side in

  (* attacked by white pawn *) 
  if (side = White && not (equal (logand pawn_attack_table.(to_side Black).(coord) piece_bitboards.(to_board P)) zero)) then true else
  (* attacked by black pawn *) 
  if (side = Black && not (equal (logand pawn_attack_table.(to_side White).(coord) piece_bitboards.(to_board Bp)) zero)) then true else

  (* Attacked by knight *)
  let knight_occupancies = 
    match side with 
      | White -> piece_bitboards.(to_board N)
      | Black -> piece_bitboards.(to_board Bn)
      | _ ->  failwith "Incorrect side information inputted - game.ml/square_attacked"
  in

  if (not (equal (logand knight_attack_table.(coord) knight_occupancies) zero)) then true else
  
  (* attacked by king*)
  let king_occupancies = 
    match side with 
      | White -> piece_bitboards.(to_board K)
      | Black -> piece_bitboards.(to_board Bk)
      | _ ->  failwith "Incorrect side information inputted - game.ml/square_attacked"
  in

  if (not (equal (logand king_attack_table.(coord) king_occupancies) zero)) then true else

  (* attacked by bishop*)
  let bishop_occupancies = 
    match side with 
      | White -> piece_bitboards.(to_board B)
      | Black -> piece_bitboards.(to_board Bb)
      | _ ->  failwith "Incorrect side information inputted - game.ml/square_attacked"
  in

  if (not (equal (logand (get_bishop_attack coord occupancies.(to_side Both)) bishop_occupancies) zero)) then true else

  (* attacked by rook*)
  let rook_occupancies = 
    match side with 
      | White -> piece_bitboards.(to_board R)
      | Black -> piece_bitboards.(to_board Br)
      | _ ->  failwith "Incorrect side information inputted - game.ml/square_attacked"
  in

  if (not (equal (logand (get_rook_attack coord occupancies.(to_side Both)) rook_occupancies) zero)) then true else

  (* attacked by queen*)
  let queen_occupancies = 
    match side with 
      | White -> piece_bitboards.(to_board Q)
      | Black -> piece_bitboards.(to_board Bq)
      | _ ->  failwith "Incorrect side information inputted - game.ml/square_attacked"
  in

  if (not (equal (logand (get_queen_attack coord occupancies.(to_side Both)) queen_occupancies) zero)) then true else

  false
 *)

(** [print_board state] print all of the attacks for a given state *)
(* let print_attacks state =

  (* let side = state.side in let enpassent = state.enpassent in *)
  let () =
    for i = 0 to 63 do
      let r, f = from_coord_rf i in
      let () = if f = 0 then Printf.printf "\n\t%d   " (8 - r) else () in
      Printf.printf " %i " (if square_attacked i state then 1 else 0)
    done
  in
  let () = Printf.printf "\n\n\t     a  b  c  d  e  f  g  h\n\n" in
  () *)
    



(** [gen_white_pawn_moves occupancies enpassent bb move_list] generates the
    white pawn moves given a white pawn bitboard *)
let rec gen_white_pawn_moves occupancies enpassent bb move_list =
  if equal zero bb then move_list
  else
    let source_square = bitscan bb in
    let target_square = source_square - 8 in

    (* Generate quiet pawn moves *)
    let quiet_moves =
      if (not (bit_exists occupancies.(2) target_square)) && target_square >= 0
      then
        (* Move pawn up one square*)
        (* let () = Printf.printf "Pawn push %s%s\n" (from_coord source_square)
           (from_coord target_square) in *)
        let move =
          encode_move source_square target_square (to_board P) 0 zero zero zero
            zero
        in
        let new_moves = [ move ] in

        let new_moves =
          (* Promotion *)
          if source_square >= to_coord A7 && source_square <= to_coord H7 then
            (* let () = Printf.printf "Promotion %s%s\n" (from_coord
               source_square) (from_coord target_square) in *)
            let move_q =
              encode_move source_square target_square (to_board P) (to_board Q)
                zero zero zero zero
            in
            let move_r =
              encode_move source_square target_square (to_board P) (to_board R)
                zero zero zero zero
            in
            let move_b =
              encode_move source_square target_square (to_board P) (to_board B)
                zero zero zero zero
            in
            let move_n =
              encode_move source_square target_square (to_board P) (to_board N)
                zero zero zero zero
            in
            move_q :: move_r :: move_b :: move_n :: new_moves
          else new_moves
        in

        let new_moves =
          (* Move pawn two squares up *)
          if
            (source_square >= to_coord A2 && source_square <= to_coord H2)
            && not (bit_exists occupancies.(2) (target_square - 8))
          then
            (* let () = Printf.printf "Double pawn push %s%s\n" (from_coord
               source_square) (from_coord (target_square - 8)) in *)
            let move =
              encode_move source_square (target_square - 8) (to_board P) 0 zero
                one zero zero
            in
            move :: new_moves
          else new_moves
        in
        new_moves
      else []
    in

    (* get white pawn attacks *)
    let attacks =
      logand
        pawn_attack_table.(to_side White).(source_square)
        occupancies.(to_side Black)
    in

    (* loop over all squares that can be attacked *)
    let rec attack_looper attack_bb moves =
      if equal zero attack_bb then moves
      else
        let target_square = bitscan attack_bb in

        let move =
          (* promotion attack *)
          if source_square >= to_coord A7 && source_square <= to_coord H7 then
            (* let () = Printf.printf "Promotion %s%s\n" (from_coord
               source_square) (from_coord target_square) in *)
            let move_q =
              encode_move source_square target_square (to_board P) (to_board Q)
                one zero zero zero
            in
            let move_r =
              encode_move source_square target_square (to_board P) (to_board R)
                one zero zero zero
            in
            let move_b =
              encode_move source_square target_square (to_board P) (to_board B)
                one zero zero zero
            in
            let move_n =
              encode_move source_square target_square (to_board P) (to_board N)
                one zero zero zero
            in
            [ move_q; move_r; move_b; move_n ]
          else
            (* normal attack *)
            (* let () = Printf.printf "Pawn capture %s%s\n" (from_coord
               source_square) (from_coord target_square) in *)
            let move =
              encode_move source_square target_square (to_board P) 0 one zero
                zero zero
            in
            [ move ]
        in
        attack_looper (pop_bit attack_bb target_square) (move @ moves)
    in
    (* loop over attacks *)
    let capture_moves = attack_looper attacks [] in

   (* enpassent attack *)
   let enpass_moves = 
      if (enpassent > -1) then 
          let enpassent_attacks = logand pawn_attack_table.(to_side White).(source_square) (set_bit zero enpassent) in

          let target_enpassent = bitscan enpassent_attacks in 
          (* let () = Printf.printf "Pawn enpassent capture: %s%s\n" (from_coord source_square) (from_coord target_enpassent) in *)
          let move = encode_move source_square target_enpassent (to_board P) 0 one zero one zero in
          [move]
      else 
          []
   in

   gen_white_pawn_moves occupancies enpassent (pop_bit bb source_square) (enpass_moves @ capture_moves @ quiet_moves @ move_list)

(** [gen_black_pawn_moves occupancies enpassent bb move_list] generates the
    black pawn moves given a black pawn bitboard *)
let rec gen_black_pawn_moves occupancies enpassent bb move_list =
  if equal zero bb then move_list
  else
    let source_square = bitscan bb in
    let target_square = source_square + 8 in

    (* Generate quiet pawn moves *)
    let quiet_moves =
      if (not (bit_exists occupancies.(2) target_square)) && target_square <= 63
      then
        (* Move pawn up one square*)
        (* let () = Printf.printf "Pawn push %s%s\n" (from_coord source_square)
           (from_coord target_square) in *)
        let move =
          encode_move source_square target_square (to_board Bp) 0 zero zero zero
            zero
        in
        let new_moves = [ move ] in

        let new_moves =
          (* Promotion *)
          if source_square >= to_coord A2 && source_square <= to_coord H2 then
            (* let () = Printf.printf "Promotion %s%s\n" (from_coord
               source_square) (from_coord target_square) in *)
            let move_q =
              encode_move source_square target_square (to_board Bp) (to_board Q)
                zero zero zero zero
            in
            let move_r =
              encode_move source_square target_square (to_board Bp) (to_board R)
                zero zero zero zero
            in
            let move_b =
              encode_move source_square target_square (to_board Bp) (to_board B)
                zero zero zero zero
            in
            let move_n =
              encode_move source_square target_square (to_board Bp) (to_board N)
                zero zero zero zero
            in
            move_q :: move_r :: move_b :: move_n :: new_moves
          else new_moves
        in

        let new_moves =
          (* Move pawn two squares up *)
          if
            (source_square >= to_coord A7 && source_square <= to_coord H7)
            && not (bit_exists occupancies.(2) (target_square + 8))
          then
            (* let () = Printf.printf "Double pawn push %s%s\n" (from_coord
               source_square) (from_coord (target_square + 8)) in *)
            let move =
              encode_move source_square (target_square + 8) (to_board Bp) 0 zero
                one zero zero
            in
            move :: new_moves
          else new_moves
        in
        new_moves
      else []
    in

    (* get black pawn attacks *)
    let attacks =
      logand
        pawn_attack_table.(to_side Black).(source_square)
        occupancies.(to_side White)
    in

    (* loop over all squares that can be attacked *)
    let rec attack_looper attack_bb moves =
      if equal zero attack_bb then moves
      else
        let target_square = bitscan attack_bb in

        let move =
          (* promotion attack *)
          if source_square >= to_coord A2 && source_square <= to_coord H2 then
            (* let () = Printf.printf "Promotion %s%s\n" (from_coord
               source_square) (from_coord target_square) in *)
            let move_q =
              encode_move source_square target_square (to_board Bp) (to_board Q)
                one zero zero zero
            in
            let move_r =
              encode_move source_square target_square (to_board Bp) (to_board R)
                one zero zero zero
            in
            let move_b =
              encode_move source_square target_square (to_board Bp) (to_board B)
                one zero zero zero
            in
            let move_n =
              encode_move source_square target_square (to_board Bp) (to_board N)
                one zero zero zero
            in
            [ move_q; move_r; move_b; move_n ]
          else
            (* normal attack *)
            (* let () = Printf.printf "Pawn capture %s%s\n" (from_coord
               source_square) (from_coord target_square) in *)
            let move =
              encode_move source_square target_square (to_board Bp) 0 one zero
                zero zero
            in
            [ move ]
        in
        attack_looper (pop_bit attack_bb target_square) (move @ moves)
    in
    (* loop over attacks *)
    let capture_moves = attack_looper attacks [] in

   let enpass_moves = 
   (* enpassent attack *)
      if (enpassent > -1) then 
          let enpassent_attacks = logand pawn_attack_table.(to_side Black).(source_square) (set_bit zero enpassent) in

          let target_enpassent = bitscan enpassent_attacks in 
          (* let () = Printf.printf "Pawn enpassent capture: %s%s\n" (from_coord source_square) (from_coord target_enpassent) in *)
          let move = encode_move source_square target_enpassent (to_board Bp) 0 one zero one zero in
          [move]
      else 
          []
   in
   
   gen_black_pawn_moves occupancies enpassent (pop_bit bb source_square) (enpass_moves @ capture_moves @ quiet_moves @ move_list)

(** [get_white_castle occupancies castling_right move_list] *)
let get_white_castle occupancies castling_right move_list =
  let king_side =
    (* white king side *)
    if not (equal (logand castling_right wk) zero) then
      (* make sure no pieces in between *)
      if
        (not (bit_exists occupancies.(2) (to_coord F1)))
        && not (bit_exists occupancies.(2) (to_coord G1))
      then
        (* make sure king and squares in between are not being attacked -
           currently not implemented *)
        (* if (not (is_square_attacked (to_coord E1) Black) && not
           (is_square_attacked (to_coord F1) Black) && not (is_square_attacked
           (to_coord G1) Black)) *)
        (* let () = Printf.printf "castling move: E1G1" in *)
        let move =
          encode_move (to_coord E1) (to_coord G1) (to_board K) 0 zero zero zero
            one
        in
        [ move ]
      else []
    else []
  in

  (* white queen side *)
  let queen_side =
    if not (equal (logand castling_right wq) zero) then
      if
        (not (bit_exists occupancies.(2) (to_coord B1)))
        && (not (bit_exists occupancies.(2) (to_coord C1)))
        && not (bit_exists occupancies.(2) (to_coord D1))
      then
        (* let () = Printf.printf "castling move: E1C1" in *)
        let move =
          encode_move (to_coord E1) (to_coord C1) (to_board K) 0 zero zero zero
            one
        in
        [ move ]
      else []
    else []
  in

  king_side @ queen_side @ move_list

(** [get_black_castle occupancies castling_right move_list]*)
let get_black_castle occupancies castling_right move_list =
  (* black king side *)
  let king_side =
    if not (equal (logand castling_right bk) zero) then
      (* make sure no pieces in between *)
      if
        (not (bit_exists occupancies.(2) (to_coord F8)))
        && not (bit_exists occupancies.(2) (to_coord G8))
      then
        (* make sure king and squares in between are not being attacked -
           currently not implemented *)
        (* if (not (is_square_attacked (to_coord E8) Black) && not
           (is_square_attacked (to_coord F1) Black) && not (is_square_attacked
           (to_coord G1) Black)) *)
        (* let () = Printf.printf "castling move: E8G8" in *)
        let move =
          encode_move (to_coord E8) (to_coord G8) (to_board Bk) 0 zero zero zero
            one
        in
        [ move ]
      else []
    else (* no black castling options *)
      []
  in

  (* black queen side *)
  let queen_side =
    if not (equal (logand castling_right bq) zero) then
      if
        (not (bit_exists occupancies.(2) (to_coord D8)))
        && (not (bit_exists occupancies.(2) (to_coord C8)))
        && not (bit_exists occupancies.(2) (to_coord D8))
      then
        (* let () = Printf.printf "castling move: E8C8" in *)
        let move =
          encode_move (to_coord E8) (to_coord C8) (to_board Bk) 0 zero zero zero
            one
        in
        [ move ]
      else []
    else (* no black castling options *)
      []
  in
  queen_side @ king_side @ move_list

(** [gen_knight_moves occupancies side bitboard move_list] generates all
    possible knight moves for a [side] given that sides knight [bitboard] and
    returns a move_list *)
let rec gen_knight_moves occupancies side bb move_list =
  if equal bb zero then move_list
  else
    (* current sides occupancy *)
    let occupancy =
      if side = White then occupancies.(to_side White)
      else occupancies.(to_side Black)
    in

    let opposing_occupancy =
      if side = White then occupancies.(to_side Black)
      else occupancies.(to_side White)
    in

    let piece = if side = White then to_board N else to_board Bn in

    let source_square = bitscan bb in

    (* make sure you are not attacking your own pieces by using a logical not *)
    let attacks =
      logand knight_attack_table.(source_square) (lognot occupancy)
    in

    let rec attacker_loop attack_bb moves =
      if equal attack_bb zero then moves
      else
        let target_square = bitscan attack_bb in

        let move =
          (* quiet move *)
          if not (bit_exists opposing_occupancy target_square) then
            (* let () = Printf.printf "quiet knight move: %s%s\n" (from_coord
               source_square) (from_coord target_square) in *)
            let move =
              encode_move source_square target_square piece 0 zero zero zero
                zero
            in
            move (* capture *)
          else
            (* let () = Printf.printf "knight capture: %s%s\n" (from_coord
               source_square) (from_coord target_square) in *)
            let move =
              encode_move source_square target_square piece 0 one zero zero zero
            in
            move
        in

        attacker_loop (pop_bit attack_bb target_square) (move :: moves)
    in

    let moves = attacker_loop attacks [] in

    gen_knight_moves occupancies side (pop_bit bb source_square)
      (moves @ move_list)

(** [gen_bishop_moves occupancies side bitboard move_list] generates all
    possible bishop moves for a [side] given that sides bishop [bitboard] and
    returns a move_list *)
let rec gen_bishop_moves occupancies side bb move_list =
  if equal bb zero then move_list
  else
    (* current sides occupancy *)
    let occupancy =
      if side = White then occupancies.(to_side White)
      else occupancies.(to_side Black)
    in

    (* opposing sides*)
    let opposing_occupancy =
      if side = White then occupancies.(to_side Black)
      else occupancies.(to_side White)
    in

    let piece = if side = White then to_board B else to_board Bb in

    let source_square = bitscan bb in

    let blockers = occupancies.(2) in

    (* make sure you are not attacking your own pieces by using a logical not *)
    let attacks =
      logand (get_bishop_attack source_square blockers) (lognot occupancy)
    in

    let rec attacker_loop attack_bb moves =
      if equal attack_bb zero then moves
      else
        let target_square = bitscan attack_bb in

        let move =
          (* quiet move *)
          if not (bit_exists opposing_occupancy target_square) then
            (* let () = Printf.printf "quiet bishop move: %s%s\n" (from_coord
               source_square) (from_coord target_square) in *)
            let move =
              encode_move source_square target_square piece 0 zero zero zero
                zero
            in
            move (* capture *)
          else
            (* let () = Printf.printf "bishop capture: %s%s\n" (from_coord
               source_square) (from_coord target_square) in *)
            let move =
              encode_move source_square target_square piece 0 one zero zero zero
            in
            move
        in
        attacker_loop (pop_bit attack_bb target_square) (move :: moves)
    in

    let moves = attacker_loop attacks [] in

    gen_bishop_moves occupancies side (pop_bit bb source_square)
      (moves @ move_list)

(** [gen_rook_moves occupancies side bitboard move_list] generates all possible
    rook moves for a [side] given that sides rook [bitboard] and returns a
    move_list *)
let rec gen_rook_moves occupancies side bb move_list =
  if equal bb zero then move_list
  else
    (* current sides occupancy *)
    let occupancy =
      if side = White then occupancies.(to_side White)
      else occupancies.(to_side Black)
    in

    (* opposing sides*)
    let opposing_occupancy =
      if side = White then occupancies.(to_side Black)
      else occupancies.(to_side White)
    in

    let piece = if side = White then to_board R else to_board Br in

    let source_square = bitscan bb in

    let blockers = occupancies.(2) in

    (* make sure you are not attacking your own pieces by using a logical not *)
    let attacks =
      logand (get_rook_attack source_square blockers) (lognot occupancy)
    in

    let rec attacker_loop attack_bb moves =
      if equal attack_bb zero then moves
      else
        let target_square = bitscan attack_bb in

        let move =
          (* quiet move *)
          if not (bit_exists opposing_occupancy target_square) then
            (* let () = Printf.printf "quiet rook move: %s%s\n" (from_coord
               source_square) (from_coord target_square) in *)
            let move =
              encode_move source_square target_square piece 0 zero zero zero
                zero
            in
            move (* capture *)
          else
            (* let () = Printf.printf "rook capture: %s%s\n" (from_coord
               source_square) (from_coord target_square) in *)
            let move =
              encode_move source_square target_square piece 0 one zero zero zero
            in
            move
        in
        attacker_loop (pop_bit attack_bb target_square) (move :: moves)
    in

    let moves = attacker_loop attacks [] in

    gen_rook_moves occupancies side (pop_bit bb source_square)
      (moves @ move_list)

(** [gen_queen_moves occupancies side bitboard move_list] generates all possible
    queen moves for a [side] given that sides queen [bitboard] and returns a
    move_list *)
let rec gen_queen_moves occupancies side bb move_list =
  if equal bb zero then move_list
  else
    (* current sides occupancy *)
    let occupancy =
      if side = White then occupancies.(to_side White)
      else occupancies.(to_side Black)
    in

    (* opposing sides*)
    let opposing_occupancy =
      if side = White then occupancies.(to_side Black)
      else occupancies.(to_side White)
    in

    let piece = if side = White then to_board Q else to_board Bq in

    let source_square = bitscan bb in

    let blockers = occupancies.(2) in

    (* make sure you are not attacking your own pieces by using a logical not *)
    let attacks =
      logand (get_queen_attack source_square blockers) (lognot occupancy)
    in

    let rec attacker_loop attack_bb moves =
      if equal attack_bb zero then moves
      else
        let target_square = bitscan attack_bb in

        let move =
          (* quiet move *)
          if not (bit_exists opposing_occupancy target_square) then
            (* let () = Printf.printf "quiet queen move: %s%s\n" (from_coord
               source_square) (from_coord target_square) in *)
            let move =
              encode_move source_square target_square piece 0 zero zero zero
                zero
            in
            move (* capture *)
          else
            (* let () = Printf.printf "queen capture: %s%s\n" (from_coord
               source_square) (from_coord target_square) in *)
            let move =
              encode_move source_square target_square piece 0 one zero zero zero
            in
            move
        in
        attacker_loop (pop_bit attack_bb target_square) (move :: moves)
    in

    let moves = attacker_loop attacks [] in

    gen_queen_moves occupancies side (pop_bit bb source_square)
      (moves @ move_list)

(** [gen_king_moves occupancies side bitboard move_list] generates all possible
    normal king moves for a [side] given that sides kings [bitboard] and returns
    a move_list *)
let rec gen_king_moves occupancies side bb move_list =
  if equal bb zero then move_list
  else
    (* current sides occupancy *)
    let occupancy =
      if side = White then occupancies.(to_side White)
      else occupancies.(to_side Black)
    in

    (* opposing sides*)
    let opposing_occupancy =
      if side = White then occupancies.(to_side Black)
      else occupancies.(to_side White)
    in

    (* get the index of the piece *)
    let piece = if side = White then to_board K else to_board Bk in

    (* finds where the king currently is *)
    let source_square = bitscan bb in

    (* make sure you are not attacking your own pieces by using a logical not *)
    let attacks = logand king_attack_table.(source_square) (lognot occupancy) in

    let rec attacker_loop attack_bb moves =
      if equal attack_bb zero then moves
      else
        let target_square = bitscan attack_bb in

        let move =
          (* quiet move *)
          if not (bit_exists opposing_occupancy target_square) then
            encode_move source_square target_square piece 0 zero zero zero zero
          (* capture *)
          else
            encode_move source_square target_square piece 0 one zero zero zero
        in
        (* after each move is checked remove that from the possible move bitboard and check the next one *)
        attacker_loop (pop_bit attack_bb target_square) (move :: moves)
    in

    let moves = attacker_loop attacks [] in

    gen_king_moves occupancies side (pop_bit bb source_square)
      (moves @ move_list)

let generate_moves state =
  let bitboards = state.bitboards in
  let occupancies = state.occupancies in
  let enpass = state.enpassent in
  let castle_right = state.castling_right in
  let side = state.side in

  let move_list =
    if side = White then
      let move_list =
        gen_white_pawn_moves occupancies enpass bitboards.(0) []
      in

      let move_list = get_white_castle occupancies castle_right move_list in

      let move_list =
        gen_knight_moves occupancies White bitboards.(1) move_list
      in

      let move_list =
        gen_bishop_moves occupancies White bitboards.(2) move_list
      in

      let move_list =
        gen_rook_moves occupancies White bitboards.(3) move_list
      in

      let move_list =
        gen_queen_moves occupancies White bitboards.(4) move_list
      in

      let move_list =
        gen_king_moves occupancies White bitboards.(5) move_list
      in

      move_list
    else
      let move_list =
        gen_black_pawn_moves occupancies enpass bitboards.(6) []
      in

      let move_list = get_black_castle occupancies castle_right move_list in

      let move_list =
        gen_knight_moves occupancies Black bitboards.(7) move_list
      in

      let move_list =
        gen_bishop_moves occupancies Black bitboards.(8) move_list
      in

      let move_list =
        gen_rook_moves occupancies Black bitboards.(9) move_list
      in

      let move_list =
        gen_queen_moves occupancies Black bitboards.(10) move_list
      in

      let move_list =
        gen_king_moves occupancies Black bitboards.(11) move_list
      in

      move_list
  in

  move_list

let print_moves move_list = 
   let () = Printf.printf "  index  source  target  piece  promoted  captured  double  enpassent  castling\n" in
   let rec printer counter = function
   | [] -> ()
   | h :: t -> 
      let () = Printf.printf "   %d      %s      %s      %s      %s          %s    %s       %s    %s \n" counter (from_coord (get_source_coord h)) 
      (from_coord (get_target_coord h)) (to_piece (get_encoded_piece h)) 
      (to_piece (get_promoted_piece h)) (string_of_bool (capture_move h)) (string_of_bool (get_double_push h))
      (string_of_bool (get_enpassent_move h)) (string_of_bool (get_castle_move h)) in
      printer (counter+1) t
   in 
   printer 0 move_list


let make_move state move =
  let state = create_state_deep_copy state in

  let bitboards = state.bitboards in
  let occupancies = state.occupancies in
  let side = state.side in
  (* let enpassent = state.enpassent in *)
  let castling_right = state.castling_right in

  let move_status =
    (* preserve game board*)

    (* parse move *)
    let source_square = get_source_coord move in
    let target_square = get_target_coord move in
    let piece = get_encoded_piece move in
    let promoted = get_promoted_piece move in
    let double = get_double_push move in
    let enpass = get_enpassent_move move in
    let castle = get_castle_move move in

    let capture = capture_move move in


    let () = bitboards.(piece) <- pop_bit bitboards.(piece) source_square in
    let () = bitboards.(piece) <- set_bit bitboards.(piece) target_square in

         (* take care of capture - capture status is true if a capture occurs *)
         let capture_status =
            if capture then 

               (* setting up the indeces to loop over the bitboards: s - for start, e - for end *)
               let (st, en) = 
                  match side with
                  | White -> (* white to move *) (6,11)
                  | Black -> (* black to move *) (0,5)
                  | _ -> failwith "cannot be neither players turn"
                  in

               let rec capturer p = 
                  if p > en then false else 
                     (* if the bit exists on the capture square *)
                     if (bit_exists bitboards.(p) target_square) then 
                        (* remove that piece from the correct bitboard *)

                        (* let () = Printf.printf "monitoring capture for %s\n" (to_piece p) in
                        let () = print_moves [move] in
                        let () = Printf.printf "\nThe side is %d\n" (to_side side) in
                        let () = print_bitboard bitboards.(p) in *)


                        let () = bitboards.(p) <- pop_bit bitboards.(p) target_square in
                        
                        (* let () = print_bitboard bitboards.(p) in
                        let () = Printf.printf "\nend monitoring capture\n" in *)

                        true
                     else capturer (p+1)
               in
               capturer st
            else
               false 
         in

    (* take care of promotion - promotion status is true if a promotion occurs*)
    let promotion_status =

      (* there is no promotion because a pawn cannot be promoted to a pawn *)
      if promoted = -1 then false 
        
      (* there is promotion happening*)
      else
        (* erase the pawn from the proper bitboard *)
        let () =
          match side with
          | White -> bitboards.(0) <- pop_bit bitboards.(0) target_square
          | Black -> bitboards.(6) <- pop_bit bitboards.(6) target_square
          | _ ->
              failwith
                "should be someones turn: promotion status make move error"
        in

        (* add the piece to the proper bitboard - it is promoted so a new piece
           or a previously taken piece is added *)
        let () =
          bitboards.(promoted) <- set_bit bitboards.(promoted) target_square
        in

        true
    in

      let enpassent_status = 
        if enpass then 

            let () = 
            (* remove the pawn that is being captured of the opposite side *)
            match side with
              | White -> bitboards.(6) <- pop_bit bitboards.(6) (target_square + 8)
              | Black -> bitboards.(0) <- pop_bit bitboards.(0) (target_square - 8) 
              | _ -> failwith "should be someones turn: empessant status make move error"
            in 
            true
        else
            false
      in

      (* each turn must reset enpassent because only available move after  *)
      let () = state.enpassent <- ~-1 in


      let double_push_status = 
        if double 
            then 

              let () = 
              (* create new enpassent square that is available only the next turn *)
              match side with
                  | White -> state.enpassent <- target_square + 8  
                  | Black ->  state.enpassent <- target_square - 8 
                  | _ -> failwith "should be someones turn: double push status make move error"
              in 
              true

            else false
      in

    (* takes care of castle - castle status is true if a castle occurs *)
    let castle_status =
      if castle then
        let () =
          match target_square with
          (* white castle king side - the target square is G1 *)
          | 62 ->
              let () = bitboards.(3) <- pop_bit bitboards.(3) (to_coord H1) in
              bitboards.(3) <- set_bit bitboards.(3) (to_coord F1)
          (* white castle queen side - the target square is C1 *)
          | 58 ->
              let () = bitboards.(3) <- pop_bit bitboards.(3) (to_coord A1) in
              bitboards.(3) <- set_bit bitboards.(3) (to_coord D1)
          (* black castle king side - the target square is G8 *)
          | 6 ->
              let () = bitboards.(9) <- pop_bit bitboards.(9) (to_coord H8) in
              bitboards.(9) <- set_bit bitboards.(9) (to_coord F8)
          (* black castle king side - the target square is C8 *)
          | 2 ->
              let () = bitboards.(9) <- pop_bit bitboards.(9) (to_coord A8) in
              bitboards.(9) <- set_bit bitboards.(9) (to_coord D8)
          | _ -> failwith "not the correct castling square"
        in

        true
      else false
    in

    (* update castling rights *)
    let () =
      state.castling_right <-
        logand castling_right (of_int castling_rights.(source_square))
    in

    (* update occupancies *)
    let () = update_occupancies bitboards occupancies in

    (* Change turn *)
    let () =
      if side = White then state.side <- Black else state.side <- White
    in

         (* Make sure that the king has not been checked *)
         let king_not_checked = true in

         (* A few checks to see if the move is legally and correctly constructed *)
         let move_status = if capture_status = capture then true else false in
         let move_status = if promotion_status then (promoted > -1) && move_status else false in
         let move_status = if enpassent_status then enpass && move_status else false in
         let move_status = if double_push_status then double && move_status else false in
         let move_status = if castle_status then castle && move_status else false in


         move_status && king_not_checked
         (* capture_status && promotion_status && enpassent_status && double_push_status && castle_status && king_not_checked  *)
            
   in

   let () = print_moves [move] in

  (move_status, state)


let eval state =
  let p = 100 in
  let n = 320 in
  let b = 330 in
  let r = 500 in
  let q = 900 in
  let k = 20000 in

  (* evaluate white position *)
  let white_pos = 0 in

  let white_pos = white_pos + (popcount state.bitboards.(to_board P) * p) in
  let white_pos = white_pos + (popcount state.bitboards.(to_board N) * n) in
  let white_pos = white_pos + (popcount state.bitboards.(to_board B) * b) in
  let white_pos = white_pos + (popcount state.bitboards.(to_board R) * r) in
  let white_pos = white_pos + (popcount state.bitboards.(to_board Q) * q) in
  let white_pos = white_pos + (popcount state.bitboards.(to_board K) * k) in

  (* evaluate black position *)
  let black_pos = 0 in

  let black_pos = black_pos + (popcount state.bitboards.(to_board Bp) * p) in
  let black_pos = black_pos + (popcount state.bitboards.(to_board Bn) * n) in
  let black_pos = black_pos + (popcount state.bitboards.(to_board Bb) * b) in
  let black_pos = black_pos + (popcount state.bitboards.(to_board Br) * r) in
  let black_pos = black_pos + (popcount state.bitboards.(to_board Bq) * q) in
  let black_pos = black_pos + (popcount state.bitboards.(to_board Bk) * k) in

   float_of_int (white_pos - black_pos)



