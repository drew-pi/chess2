open OUnit2
open Unsigned.UInt64
open Struct.Macros
open Struct.Game
open Utils.Bot

let test_bit_exists _ =
  let bb = of_int 8 in
  (* Binary: 0000...1000 *)
  assert_equal true (bit_exists bb 3) ~printer:string_of_bool

let test_bit_exists2 _ =
  let bb = of_int 8 in
  (* Binary: 0000...1000 *)
  assert_equal false (bit_exists bb 2) ~printer:string_of_bool

let test_one_or_zero _ =
  let bb = of_int 9 in
  (* Binary: 0000...1001 *)
  assert_equal 1 (one_or_zero bb 0) ~printer:string_of_int

let test_one_or_zero2 _ =
  let bb = of_int 9 in
  (* Binary: 0000...1001 *)
  assert_equal 0 (one_or_zero bb 1) ~printer:string_of_int

let test_get_coord _ = assert_equal 27 (get_coord 3 3) ~printer:string_of_int

let test_set_bit _ =
  let bb = zero in
  let bb = set_bit bb 5 in
  assert_equal true (bit_exists bb 5)

let test_pop_bit _ =
  let bb = of_int 32 in
  (* Binary: 0000...0100000 *)
  let bb = pop_bit bb 5 in
  assert_equal false (bit_exists bb 5)

let test_popcount _ =
  let bb = of_string "255" in
  (* Binary: 0000...000011111111 *)
  assert_equal 8 (popcount bb) ~printer:string_of_int

let test_bitscan_1 _ =
  let bb = of_int 16 in
  (* Binary: 0000...00010000 *)
  assert_equal 4 (bitscan bb) ~printer:string_of_int

let test_encode_move_1 _ =
  let move = encode_move 0 63 5 3 one zero one zero in
  assert_equal 0 (get_source_coord move) ~printer:string_of_int

(* let test_encode_move_2 _ = let move = encode_move 0 63 5 3 one zero one zero
   in assert_equal 63 (get_target_coord move) ~printer:string_of_int *)

let test_encode_move_3 _ =
  let move = encode_move 0 63 5 3 one zero one zero in
  assert_equal 5 (get_encoded_piece move) ~printer:string_of_int

let test_encode_move_4 _ =
  let move = encode_move 0 63 5 3 one zero one zero in
  assert_equal 3 (get_promoted_piece move) ~printer:string_of_int

let test_encode_move_5 _ =
  let move = encode_move 0 63 5 3 one zero one zero in
  assert_equal true (capture_move move) ~printer:string_of_bool

let test_encode_move_6 _ =
  let move = encode_move 0 63 5 3 one zero one zero in
  assert_equal false (get_double_push move) ~printer:string_of_bool

let test_encode_move_7 _ =
  let move = encode_move 0 63 5 3 one zero one zero in
  assert_equal true (get_enpassent_move move) ~printer:string_of_bool

let test_encode_move_8 _ =
  let move = encode_move 0 63 5 3 one zero one zero in
  assert_equal false (get_castle_move move) ~printer:string_of_bool

let test_to_board _ =
  assert_equal 0 (to_board P) ~printer:string_of_int;
  assert_equal 1 (to_board N) ~printer:string_of_int;
  assert_equal 2 (to_board B) ~printer:string_of_int;
  assert_equal 3 (to_board R) ~printer:string_of_int;
  assert_equal 4 (to_board Q) ~printer:string_of_int;
  assert_equal 5 (to_board K) ~printer:string_of_int

let test_to_board2 _ =
  assert_equal 6 (to_board Bp) ~printer:string_of_int;
  assert_equal 7 (to_board Bn) ~printer:string_of_int;
  assert_equal 8 (to_board Bb) ~printer:string_of_int;
  assert_equal 9 (to_board Br) ~printer:string_of_int;
  assert_equal 10 (to_board Bq) ~printer:string_of_int;
  assert_equal 11 (to_board Bk) ~printer:string_of_int;
  assert_equal (-1) (to_board E) ~printer:string_of_int

let test_to_piece _ =
  assert_equal "P" (to_piece 0) ~printer:(fun x -> x);
  assert_equal "N" (to_piece 1) ~printer:(fun x -> x);
  assert_equal "B" (to_piece 2) ~printer:(fun x -> x);
  assert_equal "R" (to_piece 3) ~printer:(fun x -> x);
  assert_equal "Q" (to_piece 4) ~printer:(fun x -> x);
  assert_equal "K" (to_piece 5) ~printer:(fun x -> x)

let test_to_piece2 _ =
  assert_equal "p" (to_piece 6) ~printer:(fun x -> x);
  assert_equal "n" (to_piece 7) ~printer:(fun x -> x);
  assert_equal "b" (to_piece 8) ~printer:(fun x -> x);
  assert_equal "r" (to_piece 9) ~printer:(fun x -> x);
  assert_equal "q" (to_piece 10) ~printer:(fun x -> x);
  assert_equal "k" (to_piece 11) ~printer:(fun x -> x);
  assert_equal "." (to_piece 12) ~printer:(fun x -> x)

let test_castling_rules _ =
  assert_equal (of_int 1) wk ~printer:to_string;
  assert_equal (of_int 2) wq ~printer:to_string

let test_castling_rules2 _ =
  assert_equal (of_int 4) bk ~printer:to_string;
  assert_equal (of_int 8) bq ~printer:to_string

let test_side_to_int _ =
  assert_equal 0 (to_side White) ~printer:string_of_int;
  assert_equal 1 (to_side Black) ~printer:string_of_int

let test_side_to_int2 _ =
  assert_equal 2 (to_side Both) ~printer:string_of_int;
  assert_equal (-1) (to_side Neither) ~printer:string_of_int

let test_bit_exists_more _ =
  let bb = of_string "1024" in
  (* Binary: 0000...01000000000 *)
  assert_equal true (bit_exists bb 10) ~printer:string_of_bool

let test_bit_exists_more2 _ =
  let bb = of_string "0" in
  (* No bits set *)
  assert_equal false (bit_exists bb 63) ~printer:string_of_bool

let test_bit_exists_more3 _ =
  let bb = one in
  (* Binary: 0000...0001 *)
  assert_equal true (bit_exists bb 0) ~printer:string_of_bool

let test_bit_exists_more4 _ =
  let bb = of_int 2 in
  (* Binary: 0000...0010 *)
  assert_equal true (bit_exists bb 1) ~printer:string_of_bool

let test_bit_exists_more5 _ =
  let bb = of_int 16 in
  (* Binary: 0000...10000 *)
  assert_equal true (bit_exists bb 4) ~printer:string_of_bool

let test_bit_exists_more6 _ =
  let bb = of_string "9223372036854775808" in
  (* Binary: 1000...0000 *)
  assert_equal true (bit_exists bb 63) ~printer:string_of_bool

let test_one_or_zero_more _ =
  let bb = zero in
  assert_equal 0 (one_or_zero bb 5) ~printer:string_of_int

let test_one_or_zero_more2 _ =
  let bb = of_int 31 in
  (* Binary: 0000...00011111 *)
  assert_equal 1 (one_or_zero bb 0) ~printer:string_of_int

let test_one_or_zero_more3 _ =
  let bb = of_int 31 in
  (* Binary: 0000...00011111 *)
  assert_equal 1 (one_or_zero bb 4) ~printer:string_of_int

let test_one_or_zero_more4 _ =
  let bb = of_string "18446744073709551615" in
  (* All bits set *)
  assert_equal 1 (one_or_zero bb 63) ~printer:string_of_int

let test_get_coord_more _ =
  assert_equal 0 (get_coord 0 0) ~printer:string_of_int

let test_get_coord_more2 _ =
  assert_equal 7 (get_coord 0 7) ~printer:string_of_int

let test_get_coord_more3 _ =
  assert_equal 8 (get_coord 1 0) ~printer:string_of_int

let test_get_coord_more4 _ =
  assert_equal 63 (get_coord 7 7) ~printer:string_of_int

let test_set_bit_more _ =
  let bb = zero in
  let bb = set_bit bb 31 in
  assert_equal true (bit_exists bb 31) ~printer:string_of_bool

let test_set_bit_more2 _ =
  let bb = zero in
  let bb = set_bit bb 63 in
  assert_equal true (bit_exists bb 63) ~printer:string_of_bool

let test_pop_bit_more _ =
  let bb = of_int 16 in
  (* Binary: 0000...00010000 *)
  let bb = pop_bit bb 4 in
  assert_equal false (bit_exists bb 4) ~printer:string_of_bool

let test_pop_bit_more2 _ =
  let bb = of_string "18446744073709551615" in
  (* All bits set *)
  let bb = pop_bit bb 0 in
  assert_equal false (bit_exists bb 0) ~printer:string_of_bool

let test_pop_bit_more3 _ =
  let bb = of_string "18446744073709551615" in
  (* All bits set *)
  let bb = pop_bit bb 63 in
  assert_equal false (bit_exists bb 63) ~printer:string_of_bool

let test_popcount_more _ =
  let bb = zero in
  assert_equal 0 (popcount bb) ~printer:string_of_int

let test_popcount_more2 _ =
  let bb = of_string "255" in
  (* Binary: 0000...000011111111 *)
  assert_equal 8 (popcount bb) ~printer:string_of_int

let test_popcount_more3 _ =
  let bb = of_string "18446744073709551615" in
  (* All bits set *)
  assert_equal 64 (popcount bb) ~printer:string_of_int

let test_popcount_more4 _ =
  let bb = of_string "9223372036854775808" in
  (* Binary: 1000...0000 *)
  assert_equal 1 (popcount bb) ~printer:string_of_int

let test_bitscan_2 _ =
  (* Test bitscan on various positions *)
  let bb1 = shift_left one 10 in
  (* Binary: 0000...01000000000 *)
  assert_equal 10 (bitscan bb1) ~printer:string_of_int

let test_bitscan_2_2 _ =
  (* Test bitscan on various positions *)
  let bb2 = shift_left one 31 in
  (* Binary: 000...1000000000000000000000000000000 *)
  assert_equal 31 (bitscan bb2) ~printer:string_of_int

let test_bitscan_2_3 _ =
  (* Test bitscan on various positions *)
  let bb3 = shift_left one 63 in
  (* Binary: 1000...0000 *)
  assert_equal 63 (bitscan bb3) ~printer:string_of_int

let test_encode_move_2 _ =
  (* Test encoding moves with various flags set *)
  let move1 = encode_move 7 15 5 4 one zero zero zero in
  assert_bool "Encode with promotion" (not (equal move1 zero))

let test_encode_move_2_2 _ =
  (* Test encoding moves with various flags set *)
  let move2 = encode_move 8 16 1 0 zero one one zero in
  assert_bool "Encode with enpassant" (not (equal move2 zero))

let test_encode_move_2_3 _ =
  (* Test encoding moves with various flags set *)
  let move3 = encode_move 0 63 0 ~-1 zero zero zero one in
  assert_bool "Encode with castling" (not (equal move3 zero))

let test_get_source_coord _ =
  (* Test getting the source coordinate from an encoded move *)
  let move1 = encode_move 7 15 5 4 one zero zero zero in
  assert_equal 7 (get_source_coord move1) ~printer:string_of_int

let test_get_source_coord2 _ =
  (* Test getting the source coordinate from an encoded move *)
  let move2 = encode_move 8 16 1 0 zero one one zero in
  assert_equal 8 (get_source_coord move2) ~printer:string_of_int

let test_get_source_coord3 _ =
  (* Test getting the source coordinate from an encoded move *)
  let move3 = encode_move 0 63 0 ~-1 zero zero zero one in
  assert_equal 0 (get_source_coord move3) ~printer:string_of_int

let test_get_target_coord _ =
  (* Test getting the target coordinate from an encoded move *)
  let move1 = encode_move 7 15 5 4 one zero zero zero in
  assert_equal 15 (get_target_coord move1) ~printer:string_of_int

let test_get_target_coord2 _ =
  (* Test getting the target coordinate from an encoded move *)
  let move2 = encode_move 8 16 1 0 zero one one zero in
  assert_equal 16 (get_target_coord move2) ~printer:string_of_int

let test_get_target_coord3 _ =
  (* Test getting the target coordinate from an encoded move *)
  let move3 = encode_move 0 63 0 ~-1 zero zero zero one in
  assert_equal 63 (get_target_coord move3) ~printer:string_of_int

let test_get_encoded_piece _ =
  (* Test getting the encoded piece from an encoded move *)
  let move1 = encode_move 7 15 5 4 one zero zero zero in
  assert_equal 5 (get_encoded_piece move1) ~printer:string_of_int

let test_get_encoded_piece2 _ =
  (* Test getting the encoded piece from an encoded move *)
  let move2 = encode_move 8 16 1 0 zero one one zero in
  assert_equal 1 (get_encoded_piece move2) ~printer:string_of_int

let test_get_encoded_piece3 _ =
  (* Test getting the encoded piece from an encoded move *)
  let move3 = encode_move 0 63 0 ~-1 zero zero zero one in
  assert_equal 0 (get_encoded_piece move3) ~printer:string_of_int

let test_get_promoted_piece _ =
  (* Test getting the promoted piece from an encoded move *)
  let move1 = encode_move 6 7 0 3 one zero zero zero in
  (* Promotion to Queen *)
  assert_equal 3 (get_promoted_piece move1) ~printer:string_of_int

let test_get_promoted_piece2 _ =
  (* Test getting the promoted piece from an encoded move *)
  let move2 = encode_move 1 10 0 0 zero zero zero zero in
  (* No promotion *)
  assert_equal ~-1 (get_promoted_piece move2) ~printer:string_of_int

let test_get_promoted_piece3 _ =
  (* Test getting the promoted piece from an encoded move *)
  let move3 = encode_move 5 2 0 6 one zero zero zero in
  (* Invalid promotion, should return -1 *)
  assert_equal ~-1 (get_promoted_piece move3) ~printer:string_of_int

let test_capture_move _ =
  (* Test checking if the move is a capture *)
  let move1 = encode_move 2 3 0 0 one zero zero zero in
  (* Capture move *)
  assert_equal true (capture_move move1) ~printer:string_of_bool

let test_capture_move2 _ =
  (* Test checking if the move is a capture *)
  let move2 = encode_move 4 5 0 0 zero zero zero zero in
  (* Not a capture move *)
  assert_equal false (capture_move move2) ~printer:string_of_bool

let test_capture_move3 _ =
  (* Test checking if the move is a capture *)
  let move3 = encode_move 7 8 0 0 one zero zero zero in
  (* Another capture move *)
  assert_equal true (capture_move move3) ~printer:string_of_bool

let test_get_double_push _ =
  (* Test checking if the move is a double pawn push *)
  let move1 = encode_move 1 17 6 0 zero one zero zero in
  (* Double push move *)
  assert_equal true (get_double_push move1) ~printer:string_of_bool

let test_get_double_push2 _ =
  (* Test checking if the move is a double pawn push *)
  let move2 = encode_move 2 18 6 0 zero zero zero zero in
  (* Not a double push *)
  assert_equal false (get_double_push move2) ~printer:string_of_bool

let test_get_double_push3 _ =
  (* Test checking if the move is a double pawn push *)
  let move3 = encode_move 6 22 6 0 zero one zero zero in
  (* Another double push move *)
  assert_equal true (get_double_push move3) ~printer:string_of_bool

let test_get_enpassent_move _ =
  (* Test if the move is an en passant move *)
  let move1 = encode_move 4 12 0 0 zero zero one zero in
  (* En passant move *)
  assert_equal true (get_enpassent_move move1) ~printer:string_of_bool

let test_get_enpassent_move2 _ =
  (* Test if the move is an en passant move *)
  let move2 = encode_move 3 11 0 0 zero zero zero zero in
  (* Not en passant move *)
  assert_equal false (get_enpassent_move move2) ~printer:string_of_bool

let test_get_enpassent_move3 _ =
  (* Test if the move is an en passant move *)
  let move3 = encode_move 4 12 0 0 zero zero one zero in
  (* Another en passant move *)
  assert_equal true (get_enpassent_move move3) ~printer:string_of_bool

let test_get_castle_move _ =
  (* Test if the move is a castling move *)
  let move1 = encode_move 4 2 5 0 zero zero zero one in
  (* Castle move *)
  assert_equal true (get_castle_move move1) ~printer:string_of_bool

let test_get_castle_move2 _ =
  (* Test if the move is a castling move *)
  let move2 = encode_move 4 6 5 0 zero zero zero zero in
  (* Not a castle move *)
  assert_equal false (get_castle_move move2) ~printer:string_of_bool

let test_get_castle_move3 _ =
  (* Test if the move is a castling move *)
  let move3 = encode_move 4 2 5 0 zero zero zero one in
  (* Another castle move *)
  assert_equal true (get_castle_move move3) ~printer:string_of_bool

(* let test_pop_bit_2 _ = let bb = of_string "16" in (* Binary: 0000...00010000
   *) let bb_popped = pop_bit bb 4 in assert_equal false (bit_exists bb_popped
   4) ~printer:string_of_bool *)

(* let test_pop_bit_2_2 _ = let bb = of_string "16" in (* Binary:
   0000...00010000 *) let bb_popped = pop_bit bb 4 in let bb_popped_again =
   pop_bit bb_popped 4 in assert_equal false (bit_exists bb_popped_again 4)
   ~printer:string_of_bool *)

(* let test_pop_bit_2_3 _ = let bb_no_bit = zero in let bb_no_bit_popped =
   pop_bit bb_no_bit 1 in assert_equal bb_no_bit_popped zero
   ~printer:to_string *)

(* let test_popcount_3 _ = let bb_zero = zero in assert_equal 0 (popcount
   bb_zero) ~printer:string_of_int

   let test_popcount_3_2 _ = let bb_one_bit = of_string "1" in assert_equal 1
   (popcount bb_one_bit) ~printer:string_of_int

   let test_popcount_3_3 _ = let bb_all_bits = of_string "18446744073709551615"
   in (* All bits set *) assert_equal 64 (popcount bb_all_bits)
   ~printer:string_of_int

   let test_popcount_3_4 _ = let bb_alternate_bits = of_string
   "12297829382473034410" in (* 0b10101010... *) assert_equal 32 (popcount
   bb_alternate_bits) ~printer:string_of_int *)

(* Helper function to print tuples for assertions *)
let pp_tuple (x, y) = Printf.sprintf "(%d, %d)" x y

(* Tests for conversions between rankfile and numerical indices *)
let test_to_coord _ = assert_equal 0 (to_coord A8) ~printer:string_of_int
let test_to_coord2 _ = assert_equal 63 (to_coord H1) ~printer:string_of_int
let test_to_coord3 _ = assert_equal 27 (to_coord D5) ~printer:string_of_int
let test_to_coord4 _ = assert_equal 56 (to_coord A1) ~printer:string_of_int
let test_from_coord _ = assert_equal "A8" (from_coord 0) ~printer:(fun x -> x)
let test_from_coord2 _ = assert_equal "H1" (from_coord 63) ~printer:(fun x -> x)
let test_from_coord3 _ = assert_equal "D5" (from_coord 27) ~printer:(fun x -> x)
let test_from_coord4 _ = assert_equal "A1" (from_coord 56) ~printer:(fun x -> x)

let test_from_coord5 _ =
  assert_equal "NULL" (from_coord 64) ~printer:(fun x -> x)

(* Tests for conversions between rankfile and rank/file tuple *)
let test_to_coord_rf _ = assert_equal (0, 0) (to_coord_rf A8) ~printer:pp_tuple
let test_to_coord_rf2 _ = assert_equal (7, 7) (to_coord_rf H1) ~printer:pp_tuple
let test_to_coord_rf3 _ = assert_equal (3, 3) (to_coord_rf D5) ~printer:pp_tuple
let test_to_coord_rf4 _ = assert_equal (7, 0) (to_coord_rf A1) ~printer:pp_tuple

let test_from_coord_rf _ =
  assert_equal (0, 0) (from_coord_rf 0) ~printer:pp_tuple

let test_from_coord_rf2 _ =
  assert_equal (7, 7) (from_coord_rf 63) ~printer:pp_tuple

let test_from_coord_rf3 _ =
  assert_equal (3, 3) (from_coord_rf 27) ~printer:pp_tuple

let test_from_coord_rf4 _ =
  assert_equal (7, 0) (from_coord_rf 56) ~printer:pp_tuple

(* Function to generate a test case for each position *)
let test_to_coord_2 name expected rf =
  name >:: fun _ -> assert_equal expected (to_coord rf) ~printer:string_of_int

let inital_suite =
  "Bitboard Tests"
  >::: [
         "test_bit_exists" >:: test_bit_exists;
         "test_bit_exists2" >:: test_bit_exists2;
         "test_one_or_zero" >:: test_one_or_zero;
         "test_one_or_zero2" >:: test_one_or_zero2;
         "test_get_coord" >:: test_get_coord;
         "test_set_bit" >:: test_set_bit;
         "test_pop_bit" >:: test_pop_bit;
         "test_popcount" >:: test_popcount;
         "test_bitscan" >:: test_bitscan_1;
         "test_encode_move" >:: test_encode_move_1;
         "test_encode_move2" >:: test_encode_move_2;
         "test_encode_move3" >:: test_encode_move_3;
         "test_encode_move4" >:: test_encode_move_4;
         "test_encode_move5" >:: test_encode_move_5;
         "test_encode_move6" >:: test_encode_move_6;
         "test_encode_move7" >:: test_encode_move_7;
         "test_encode_move8" >:: test_encode_move_8;
         "test_to_board" >:: test_to_board;
         "test_to_board2" >:: test_to_board2;
         "test_to_piece" >:: test_to_piece;
         "test_to_piece2" >:: test_to_piece2;
         "test_castling_rules" >:: test_castling_rules;
         "test_castling_rules2" >:: test_castling_rules2;
         "test_side_to_int" >:: test_side_to_int;
         "test_side_to_int2" >:: test_side_to_int2;
       ]

let conversion_suite =
  "Conversion Tests"
  >::: [
         "test_to_coord" >:: test_to_coord;
         "test_to_coord2" >:: test_to_coord2;
         "test_to_coord3" >:: test_to_coord3;
         "test_to_coord4" >:: test_to_coord4;
         "test_from_coord" >:: test_from_coord;
         "test_from_coord2" >:: test_from_coord2;
         "test_from_coord3" >:: test_from_coord3;
         "test_from_coord4" >:: test_from_coord4;
         "test_from_coord5" >:: test_from_coord5;
         "test_to_coord_rf" >:: test_to_coord_rf;
         "test_to_coord_rf2" >:: test_to_coord_rf2;
         "test_to_coord_rf3" >:: test_to_coord_rf3;
         "test_to_coord_rf4" >:: test_to_coord_rf4;
         "test_from_coord_rf" >:: test_from_coord_rf;
         "test_from_coord_rf2" >:: test_from_coord_rf2;
         "test_from_coord_rf3" >:: test_from_coord_rf3;
         "test_from_coord_rf4" >:: test_from_coord_rf4;
       ]

let bitscan_suite = "Bitscan Tests" >::: [ "test_bitscan" >:: test_bitscan_2_2 ]

let move_features_suite =
  "Move Features Tests"
  >::: [
         "test_get_promoted_piece" >:: test_get_promoted_piece;
         "test_get_promoted_piece2" >:: test_get_promoted_piece2;
         "test_get_promoted_piece3" >:: test_get_promoted_piece3;
         "test_capture_move" >:: test_capture_move;
         "test_capture_move2" >:: test_capture_move2;
         "test_capture_move3" >:: test_capture_move3;
         "test_get_double_push" >:: test_get_double_push;
         "test_get_double_push2" >:: test_get_double_push2;
         "test_get_double_push3" >:: test_get_double_push3;
         "test_get_enpassent_move" >:: test_get_enpassent_move;
         "test_get_enpassent_move2" >:: test_get_enpassent_move2;
         "test_get_enpassent_move3" >:: test_get_enpassent_move3;
         "test_get_castle_move" >:: test_get_castle_move;
         "test_get_castle_move2" >:: test_get_castle_move2;
         "test_get_castle_move3" >:: test_get_castle_move3;
       ]

let bitscan_and_move_suite =
  "Bitscan and Move Encoding Tests"
  >::: [
         "test_bitscan" >:: test_bitscan_2;
         "test_bitscan2" >:: test_bitscan_2_2;
         "test_bitscan3" >:: test_bitscan_2_3;
         "test_encode_move" >:: test_encode_move_2;
         "test_encode_move2" >:: test_encode_move_2_2;
         "test_encode_move3" >:: test_encode_move_2_3;
         "test_get_source_coord" >:: test_get_source_coord;
         "test_get_source_coord2" >:: test_get_source_coord2;
         "test_get_source_coord3" >:: test_get_source_coord3;
         "test_get_target_coord" >:: test_get_target_coord;
         "test_get_target_coord2" >:: test_get_target_coord2;
         "test_get_target_coord3" >:: test_get_target_coord3;
         "test_get_encoded_piece" >:: test_get_encoded_piece;
         "test_get_encoded_piece2" >:: test_get_encoded_piece2;
         "test_get_encoded_piece3" >:: test_get_encoded_piece3;
       ]

let bit_manipulation_suite =
  "Bit Manipulation Tests"
  >::: [
         "test_bit_exists_more" >:: test_bit_exists_more;
         "test_bit_exists_more2" >:: test_bit_exists_more2;
         "test_bit_exists_more3" >:: test_bit_exists_more3;
         "test_bit_exists_more4" >:: test_bit_exists_more4;
         "test_bit_exists_more5" >:: test_bit_exists_more5;
         "test_bit_exists_more6" >:: test_bit_exists_more6;
         "test_one_or_zero_more" >:: test_one_or_zero_more;
         "test_one_or_zero_more2" >:: test_one_or_zero_more2;
         "test_one_or_zero_more3" >:: test_one_or_zero_more3;
         "test_one_or_zero_more4" >:: test_one_or_zero_more4;
         "test_get_coord_more" >:: test_get_coord_more;
         "test_get_coord_more2" >:: test_get_coord_more2;
         "test_get_coord_more3" >:: test_get_coord_more3;
         "test_get_coord_more4" >:: test_get_coord_more4;
         "test_set_bit_more" >:: test_set_bit_more;
         "test_set_bit_more2" >:: test_set_bit_more2;
         "test_pop_bit_more" >:: test_pop_bit_more;
         "test_pop_bit_more2" >:: test_pop_bit_more2;
         "test_pop_bit_more3" >:: test_pop_bit_more3;
         "test_popcount_more" >:: test_popcount_more;
         "test_popcount_more2" >:: test_popcount_more2;
         "test_popcount_more3" >:: test_popcount_more3;
         "test_popcount_more4" >:: test_popcount_more4;
       ]

let bit_manipulation_suite2 =
  "Bit Manipulation Tests"
  >::: [ "test_pop_bit" >:: test_pop_bit; "test_popcount" >:: test_popcount ]

let cord_sute_2 =
  "To Coord Tests"
  >::: [
         test_to_coord_2 "A8" 0 A8;
         test_to_coord_2 "B8" 1 B8;
         test_to_coord_2 "C8" 2 C8;
         (* Add all other positions following the same pattern later *)
         test_to_coord_2 "H1" 63 H1;
       ]

let bit_manipulation_suite3 =
  "Bit Manipulation Tests"
  >::: [
         "test_bit_exists" >:: test_bit_exists;
         "test_one_or_zero" >:: test_one_or_zero;
         "test_get_coord" >:: test_get_coord;
         "test_set_bit" >:: test_set_bit;
         "test_pop_bit" >:: test_pop_bit;
       ]

module TestBotSettings = struct
  let max_depth = 50
  let eval_function = eval
end

module TestBot = InitBotWithSettings (TestBotSettings)

(*Test plan: - Test that each bot chooses the best action from a given state -
  Test that each bot performs better with increasing depth*)

let get_action_or_fail = function
  | None -> failwith "Action is none"
  | Some (a : Unsigned.UInt64.t) -> a

let choose_randomly lst =
  let index = Random.int (List.length lst) in
  List.nth lst index

let bot_tests =
  "test for bot module"
  >::: [
         ( "test bot minimax" >:: fun _ ->
           let current_state_ref = ref (setup_board ()) in

           let max_state_total, min_state_total = (ref 0., ref 0.) in
           let start_min_score, start_max_score = (0., 0.) in
           for _ = 1 to 25 do
             (*Score when black side*)
             let player_move =
               choose_randomly (generate_moves !current_state_ref)
             in
             (*Black Side*)
             let _, state_after_max_moves, _ =
               make_move !current_state_ref player_move
             in
             (*Score change for max agent*)
             let state_score_for_max = eval state_after_max_moves in
             max_state_total := !max_state_total +. state_score_for_max;
             let black_action_opt = TestBot.get_action state_after_max_moves in
             let black_action = get_action_or_fail black_action_opt in
             (*White side*)
             let _, state_after_min_moves, _ =
               make_move state_after_max_moves black_action
             in

             (*Score change for min agent*)
             let state_score_for_min = eval state_after_min_moves in
             min_state_total := !min_state_total +. state_score_for_min;
             current_state_ref := state_after_min_moves
           done;
           assert_bool "No change detected"
             (!min_state_total +. !max_state_total
             <> start_max_score +. start_min_score) );
       ]

let () =
  run_test_tt_main bot_tests;
  run_test_tt_main bit_manipulation_suite;
  run_test_tt_main inital_suite;
  run_test_tt_main bitscan_and_move_suite;
  run_test_tt_main move_features_suite;
  run_test_tt_main bit_manipulation_suite2;
  run_test_tt_main conversion_suite;
  run_test_tt_main bitscan_suite;
  run_test_tt_main cord_sute_2;
  run_test_tt_main bit_manipulation_suite3
