open Unsigned.UInt64

(*
   
Bit manipulation 

*)

let bit_exists bb coord = not (equal (logand bb (shift_left one (coord))) zero)

let one_or_zero bb coord = if bit_exists bb coord then 1 else 0

let get_coord rank file = rank * 8 + file

let set_bit bb coord = logor bb (shift_left one coord)

let pop_bit bb coord = if bit_exists bb coord then logxor bb (shift_left one coord) else bb

let popcount =
  let ( + ) = add in
  let ( - ) = sub in
  let ( * ) = mul in
  let ( lsr ) = shift_right in
  let ( land ) = logand in
  let m1 = of_string "6148914691236517205" in
  (* 0b01010101... *)
  let m2 = of_string "3689348814741910323" in
  (* 0b00110011... *)
  let m4 = of_string "1085102592571150095" in
  (* 0b00001111... *)
  let h01 = of_string "72340172838076673" in
  (* 1 bit set per byte *)
  fun [@inline] x ->
    (* gather the bit count for every pair of bits *)
    let x = x - ((x lsr 1) land m1) in
    (* gather the bit count for every 4 bits *)
    let x = (x land m2) + ((x lsr 2) land m2) in
    (* gather the bit count for every byte *)
    let x = (x + (x lsr 4)) land m4 in
    (* sum the bit counts in the top byte and shift it down *)
    to_int ((x * h01) lsr 56)
;;



(** De Bruijn number for the given index sequence*)
let de_bruijn = of_string "0x03f79d71b4cb0a89"

(** Allows for quick bitscanning *)
let index = [|
  0; 1; 48; 2; 57; 49; 28; 3; 61; 58; 50; 42; 38; 29; 17; 4;
  62; 55; 59; 36; 53; 51; 43; 22; 45; 39; 33; 30; 24; 18; 12; 5;
  63; 47; 56; 27; 60; 41; 37; 16; 54; 35; 52; 21; 44; 32; 23; 11;
  46; 26; 40; 15; 34; 20; 31; 10; 25; 14; 19; 9; 13; 8; 7; 6
|]

let bitscan n =
  if equal n zero then
    0 (* No bits are set *)
  else
    let isolated_bit = logand (sub zero n) n in
    let shifted = (to_int (shift_right (mul isolated_bit de_bruijn) 58)) in
    index.(shifted) 




(* 
   binary move bits                                string integer representation
    
   0000 0000 0000 0000 0011 1111    source square       "63"
   0000 0000 0000 1111 1100 0000    target square       "4032"
   0000 0000 1111 0000 0000 0000    piece               "61440"
   0000 1111 0000 0000 0000 0000    promoted piece      "983040"
   0001 0000 0000 0000 0000 0000    capture flag        "1048576"
   0010 0000 0000 0000 0000 0000    double push flag    "2097152"
   0100 0000 0000 0000 0000 0000    enpassant flag      "4194304"
   1000 0000 0000 0000 0000 0000    castling flag       "8388608" 
   
*)




let encode_move source target piece promoted captured double enpassent castling = 
  let open Unsigned.UInt64.Infix in

  (of_int source) lor (of_int target lsl 6) 
  lor (of_int piece lsl 12) lor (of_int promoted lsl 16)
  lor (captured lsl 20) lor (double lsl 21) lor 
  (enpassent lsl 22) lor (castling lsl 23)

let get_source_coord move = to_int (logand move (of_int 63))

let get_target_coord move = to_int (shift_right (logand move (of_int 4032)) 6)

let get_encoded_piece move = to_int (shift_right (logand move (of_int 61440)) 12)

let get_promoted_piece move = let p = to_int (shift_right (logand move (of_int 983040)) 16) in if p = 0 || p = 6 then -1 else p

let capture_move move = not (equal (logand move (of_int 1048576)) zero)

let get_double_push move = not (equal (logand move (of_int 2097152)) zero)

let get_enpassent_move move = not (equal (logand move (of_int 4194304)) zero)

let get_castle_move move = not (equal (logand move (of_int 8388608)) zero)






(*

   Rep invariant

*)

(*
	1     0  0  0  1  1  0  1  1  
 	2     0  0  0  0  0  0  0  0  
 	3     0  0  0  0  0  0  0  0  
 	4     0  0  0  0  0  0  0  0  
 	5     0  0  0  0  0  0  0  0  
 	6     0  0  0  0  0  0  0  0  
 	7     0  0  0  0  0  0  0  0  
 	8     0  0  0  0  0  0  0  0  
 
	      a  b  c  d  e  f  g  h

 	Int representation (base 10):  216 

  d1 corresponds to ((0 * 8) + 3) = 3
  so 'bit_exits bb 3' -> true

  The rep invariant is that the bitboard is represented by the ranges 0-63, but the chessboard is from 1-64
*)

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

let to_coord = function
 | A8 -> 0                         
 | B8 -> 1
 | C8 -> 2
 | D8 -> 3
 | E8 -> 4
 | F8 -> 5
 | G8 -> 6
 | H8 -> 7
 | A7 -> 8
 | B7 -> 9
 | C7 -> 10
 | D7 -> 11
 | E7 -> 12
 | F7 -> 13
 | G7 -> 14
 | H7 -> 15
 | A6 -> 16
 | B6 -> 17
 | C6 -> 18
 | D6 -> 19
 | E6 -> 20
 | F6 -> 21
 | G6 -> 22
 | H6 -> 23
 | A5 -> 24
 | B5 -> 25
 | C5 -> 26
 | D5 -> 27
 | E5 -> 28
 | F5 -> 29
 | G5 -> 30
 | H5 -> 31
 | A4 -> 32
 | B4 -> 33
 | C4 -> 34
 | D4 -> 35
 | E4 -> 36
 | F4 -> 37
 | G4 -> 38
 | H4 -> 39
 | A3 -> 40
 | B3 -> 41
 | C3 -> 42
 | D3 -> 43
 | E3 -> 44
 | F3 -> 45
 | G3 -> 46
 | H3 -> 47
 | A2 -> 48
 | B2 -> 49
 | C2 -> 50
 | D2 -> 51
 | E2 -> 52
 | F2 -> 53
 | G2 -> 54
 | H2 -> 55
 | A1 -> 56
 | B1 -> 57
 | C1 -> 58
 | D1 -> 59
 | E1 -> 60
 | F1 -> 61
 | G1 -> 62
 | H1 -> 63

let from_coord = function
| 0 -> "A8"
| 1 -> "B8"
| 2 -> "C8"
| 3 -> "D8"
| 4 -> "E8"
| 5 -> "F8"
| 6 -> "G8"
| 7 -> "H8"
| 8 -> "A7"
| 9 -> "B7"
| 10 -> "C7"
| 11 -> "D7"
| 12 -> "E7"
| 13 -> "F7"
| 14 -> "G7"
| 15 -> "H7"
| 16 -> "A6"
| 17 -> "B6"
| 18 -> "C6"
| 19 -> "D6"
| 20 -> "E6"
| 21 -> "F6"
| 22 -> "G6"
| 23 -> "H6"
| 24 -> "A5"
| 25 -> "B5"
| 26 -> "C5"
| 27 -> "D5"
| 28 -> "E5"
| 29 -> "F5"
| 30 -> "G5"
| 31 -> "H5"
| 32 -> "A4"
| 33 -> "B4"
| 34 -> "C4"
| 35 -> "D4"
| 36 -> "E4"
| 37 -> "F4"
| 38 -> "G4"
| 39 -> "H4"
| 40 -> "A3"
| 41 -> "B3"
| 42 -> "C3"
| 43 -> "D3"
| 44 -> "E3"
| 45 -> "F3"
| 46 -> "G3"
| 47 -> "H3"
| 48 -> "A2"
| 49 -> "B2"
| 50 -> "C2"
| 51 -> "D2"
| 52 -> "E2"
| 53 -> "F2"
| 54 -> "G2"
| 55 -> "H2"
| 56 -> "A1"
| 57 -> "B1"
| 58 -> "C1"
| 59 -> "D1"
| 60 -> "E1"
| 61 -> "F1"
| 62 -> "G1"
| 63 -> "H1"
| _ -> "NULL"

let to_coord_rf = function
| A8 -> (0, 0)
| B8 -> (0, 1)
| C8 -> (0, 2)
| D8 -> (0, 3)
| E8 -> (0, 4)
| F8 -> (0, 5)
| G8 -> (0, 6)
| H8 -> (0, 7)
| A7 -> (1, 0)
| B7 -> (1, 1)
| C7 -> (1, 2)
| D7 -> (1, 3)
| E7 -> (1, 4)
| F7 -> (1, 5)
| G7 -> (1, 6)
| H7 -> (1, 7)
| A6 -> (2, 0)
| B6 -> (2, 1)
| C6 -> (2, 2)
| D6 -> (2, 3)
| E6 -> (2, 4)
| F6 -> (2, 5)
| G6 -> (2, 6)
| H6 -> (2, 7)
| A5 -> (3, 0)
| B5 -> (3, 1)
| C5 -> (3, 2)
| D5 -> (3, 3)
| E5 -> (3, 4)
| F5 -> (3, 5)
| G5 -> (3, 6)
| H5 -> (3, 7)
| A4 -> (4, 0)
| B4 -> (4, 1)
| C4 -> (4, 2)
| D4 -> (4, 3)
| E4 -> (4, 4)
| F4 -> (4, 5)
| G4 -> (4, 6)
| H4 -> (4, 7)
| A3 -> (5, 0)
| B3 -> (5, 1)
| C3 -> (5, 2)
| D3 -> (5, 3)
| E3 -> (5, 4)
| F3 -> (5, 5)
| G3 -> (5, 6)
| H3 -> (5, 7)
| A2 -> (6, 0)
| B2 -> (6, 1)
| C2 -> (6, 2)
| D2 -> (6, 3)
| E2 -> (6, 4)
| F2 -> (6, 5)
| G2 -> (6, 6)
| H2 -> (6, 7)
| A1 -> (7, 0)
| B1 -> (7, 1)
| C1 -> (7, 2)
| D1 -> (7, 3)
| E1 -> (7, 4)
| F1 -> (7, 5)
| G1 -> (7, 6)
| H1 -> (7, 7)


let string_to_rankfile = function
| "A8" -> A8
| "B8" -> B8 
| "C8" -> C8
| "D8" -> D8
| "E8" -> E8
| "F8" -> F8 
| "G8" -> G8
| "H8" -> H8
| "A7" -> A7
| "B7" -> B7 
| "C7" -> C7
| "D7" -> D7
| "E7" -> E7
| "F7" -> F7 
| "G7" -> G7
| "H7" -> H7
| "A6" -> A6
| "B6" -> B6 
| "C6" -> C6
| "D6" -> D6
| "E6" -> E6
| "F6" -> F6 
| "G6" -> G6
| "H6" -> H6
| "A5" -> A5
| "B5" -> B5 
| "C5" -> C5
| "D5" -> D5
| "E5" -> E5
| "F5" -> F5 
| "G5" -> G5
| "H5" -> H5
| "A4" -> A4
| "B4" -> B4 
| "C4" -> C4
| "D4" -> D4
| "E4" -> E4
| "F4" -> F4 
| "G4" -> G4
| "H4" -> H4
| "A3" -> A3
| "B3" -> B3 
| "C3" -> C3
| "D3" -> D3
| "E3" -> E3
| "F3" -> F3 
| "G3" -> G3
| "H3" -> H3
| "A2" -> A2
| "B2" -> B2 
| "C2" -> C2
| "D2" -> D2
| "E2" -> E2
| "F2" -> F2 
| "G2" -> G2
| "H2" -> H2
| "A1" -> A1
| "B1" -> B1 
| "C1" -> C1
| "D1" -> D1
| "E1" -> E1
| "F1" -> F1 
| "G1" -> G1
| "H1" -> H1
| s -> failwith (s ^ " is not a valid rank file value")



let from_coord_rf coord = (coord / 8, coord mod 8)



type sides = | White | Black | Both | Neither


let to_side = function
  | White -> 0
  | Black -> 1
  | Both -> 2
  | _ -> -1

type game_pieces = P | N | B | R | Q | K | Bp | Bn | Bb | Br | Bq | Bk | E

let to_board = function
    | P -> 0
    | N -> 1
    | B -> 2
    | R -> 3
    | Q -> 4
    | K -> 5
    | Bp -> 6
    | Bn -> 7
    | Bb -> 8
    | Br -> 9
    | Bq -> 10
    | Bk -> 11
    | E -> -1
  
let to_piece = function
    | 0 -> "P"
    | 1 -> "N"
    | 2 -> "B"
    | 3 -> "R"
    | 4 -> "Q"
    | 5 -> "K"
    | 6 -> "p"
    | 7 -> "n"
    | 8 -> "b"
    | 9 -> "r"
    | 10 -> "q"
    | 11 -> "k"
    | _ -> "."
  

  (*
     
      CASTLING RULES
  
  *)

  (* 0001 white king can castle to the king side *)
  let wk = of_int 1

  (* 0010 white king can castle to the queen side*)
  let wq = of_int 2
  
  (* 0100 black king can castle to the king side *)
  let bk = of_int 4
  
  (* 1000 black king can castle to the queen side *)
  let bq = of_int 8

(* Rep invariant translation layer - used to create [rankfile] and [to_coord], but not useful for anything other than that*)
(* let rec get_rep rank = 
  if rank > 8 then () else
  let _ = Printf.printf " | A%d\n | B%d\n | C%d\n | D%d\n | E%d\n | F%d\n | G%d\n | H%d\n" rank rank rank rank rank rank rank rank
  in get_rep (rank + 1)

let rec get_translation rank = 
  if rank = 0 then () else
  let _ = Printf.printf " | A%d -> %d\n | B%d -> %d\n | C%d -> %d\n | D%d -> %d\n | E%d -> %d\n | F%d -> %d\n | G%d -> %d\n | H%d -> %d\n" rank (((8-rank)*8) + 0) rank (((8-rank)*8) + 1) rank (((8-rank)*8) + 2) rank (((8-rank)*8) + 3) rank (((8-rank)*8) + 4) rank (((8-rank)*8) + 5) rank (((8-rank)*8) + 6) rank (((8-rank)*8) + 7)
  in get_translation (rank - 1)


let rec get_translation_to_rf rank = 
  if rank = 0 then () else
  let _ = Printf.printf " | A%d -> (%d, %d)\n | B%d -> (%d, %d)\n | C%d -> (%d, %d)\n | D%d -> (%d, %d)\n | E%d -> (%d, %d)\n | F%d -> (%d, %d)\n | G%d -> (%d, %d)\n | H%d -> (%d, %d)\n" rank (8-rank) 0 rank (8-rank) 1 rank (8-rank) 2 rank (8-rank) 3 rank (8-rank) 4 rank (8-rank) 5 rank (8-rank) 6 rank (8-rank) 7
  in get_translation_to_rf (rank - 1)

let rec get_rev_translation rank = 
  if rank = 0 then () else
  let _ = Printf.printf " | %d -> \"A%d\"\n | %d -> \"B%d\"\n | %d -> \"C%d\"\n | %d -> \"D%d\"\n | %d -> \"E%d\"\n | %d -> \"F%d\"\n | %d -> \"G%d\"\n | %d -> \"H%d\"\n" (((8-rank)*8) + 0) rank (((8-rank)*8) + 1) rank (((8-rank)*8) + 2) rank (((8-rank)*8) + 3) rank (((8-rank)*8) + 4) rank (((8-rank)*8) + 5) rank (((8-rank)*8) + 6) rank (((8-rank)*8) + 7) rank
  in get_rev_translation (rank - 1)


let rec get_string_to_rankfile rank = 
  if rank = 0 then () else
  let _ = Printf.printf " | \"A%d\" -> A%d\n | \"B%d\" -> B%d \n | \"C%d\" -> C%d\n | \"D%d\" -> D%d\n | \"E%d\" -> E%d\n | \"F%d\" -> F%d \n | \"G%d\" -> G%d\n | \"H%d\" -> H%d\n" rank rank rank rank rank rank rank rank rank rank rank rank rank rank rank rank
  in get_string_to_rankfile (rank - 1) *)



(** Representation of the different pieces in chess*)
type pieces = 
  | Pawn
  | Rook
  | Knight
  | Bishop
  | King
  | Queen




(* bitboard representation *)
let rec iterate_file bb rank file = 
  if file >= 8 then () else let _ = Printf.printf " %d " (one_or_zero bb (get_coord rank file)) in iterate_file bb rank (file + 1)

let rec iterate_rank bb rank = 
  if rank >= 8 then () else let _ = Printf.printf "\t%d    " (8 - rank) in let () = iterate_file bb rank 0 in let _ = Printf.printf " \n " in iterate_rank bb (rank + 1)

let print_bitboard bb = 
  let () = iterate_rank bb 0 in
  let _ = Printf.printf "\n\t      a  b  c  d  e  f  g  h\n"  in
  let _ = Printf.printf "\n \tInt representation (base 10):  %s \n\n" (to_string bb) in
  ()








(*
   Misc Tools
*)


let random_uint64 () = let () = Random.self_init () in let rint64 = Random.int64 Int64.max_int in of_string (Int64.to_string rint64)

let pow n exp = 
  if exp < 0 then 0 else 
  let rec pow_looper n acc = function
    | 0 -> acc
    | a -> pow_looper n (acc * n) (a-1)
in pow_looper n 1 exp
