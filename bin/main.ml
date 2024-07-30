open Utils.Bot
open Struct.Macros
open Struct.State

let black_tile = "\u{25A0}"
let white_tile = "\u{25A1}"

let white_or_black_tile coord =
  let r, f = from_coord_rf coord in
  if (r + f) mod 2 = 0 then (* white tile *) black_tile else white_tile

(** Converts bitboard index into piece string *)
let string_of_piece piece coord =
  match piece with
  | 0 -> "\u{265F}" (* White Pawn *)
  | 1 -> "\u{265E}" (* White Knight *)
  | 2 -> "\u{265D}" (* White Bishop *)
  | 3 -> "\u{265C}" (* White Rook *)
  | 4 -> "\u{265B}" (* White Queen *)
  | 5 -> "\u{265A}" (* White King *)
  | 6 -> "\u{2659}" (* Black Pawn *)
  | 7 -> "\u{2658}" (* Black Knight *)
  | 8 -> "\u{2657}" (* Black Bishop *)
  | 9 -> "\u{2656}" (* Black Rook *)
  | 10 -> "\u{2655}" (* Black Queen *)
  | 11 -> "\u{2654}" (* Black King *)
  | _ -> white_or_black_tile coord

(** Displays the rules of chess *)
let display_chess_rules () =
  print_endline "Chess Rules:";
  print_endline "1. The game is played on an 8x8 board.";
  print_endline
    "2. Each player starts with 16 pieces: 1 king, 1 queen, 2 rooks, 2 \
     knights, 2 bishops, and 8 pawns.";
  print_endline "3. The goal is to checkmate the opponent's king.";
  print_endline "4. Pieces move as follows:";
  print_endline "   - King: one square in any direction.";
  print_endline "   - Queen: any number of squares in any direction.";
  print_endline "   - Rook: any number of squares horizontally or vertically.";
  print_endline "   - Bishop: any number of squares diagonally.";
  print_endline
    "   - Knight: in an L-shape (two squares in one direction and then one \
     square perpendicular).";
  print_endline
    "   - Pawn: one square forward (or two squares forward from their starting \
     position); captures one square diagonally.";
  print_endline
    "5. Special moves include castling, en passant, and pawn promotion.";
  print_endline
    "6. The game ends in checkmate, stalemate, draw by agreement, or draw by \
     insufficient material.";
  print_endline "Press enter to continue";
  let _ = read_line () in
  ()

(** Displays an example of how to move a piece *)
let display_move_example () =
  print_endline
    "To move a piece in this version of chess you must follow a few \
     instructions";
  print_endline "\tExample Move:";
  print_endline
    "1. Enter the coordinate of the piece you want to move (e.g., e2).";
  print_endline
    "2. Enter the target coordinate where you want to move the piece (e.g., \
     e4).";
  print_endline
    "So, to move a piece from e2 to e4, you will enter e2 first, then e4.";
  print_endline "Press enter to continue";
  let _ = read_line () in
  ()

let () = display_chess_rules ()
let () = display_move_example ()

(** print_board helper function *)
let rec check_all_pieces bitboards coord piece =
  if piece > 11 then white_or_black_tile coord
  else if bit_exists bitboards.(piece) coord then string_of_piece piece coord
  else check_all_pieces bitboards coord (piece + 1)

(** [print_board state] print the current board representation *)
let print_board state =
  let bitboards = state.bitboards in

  (* let side = state.side in let enpassent = state.enpassent in *)
  let () =
    for i = 0 to 63 do
      let r, f = from_coord_rf i in
      let () = if f = 0 then Printf.printf "\n\t%d   " (8 - r) else () in
      Printf.printf " %s " (check_all_pieces bitboards i 0)
    done
  in
  let () = Printf.printf "\n\n\t     a  b  c  d  e  f  g  h\n\n" in

  let () = Printf.printf "\t Side: %d \n" (to_side state.side) in 
  let () = Printf.printf "\t En passent: %s \n" (from_coord state.enpassent) in
  let () = Printf.printf "\t Castle: %i \n\n" (Unsigned.UInt64.to_int state.castling_right) in

  ()



let state = Struct.Game.setup_board ()

(* let () = for i = 0 to 11 do let () = Printf.printf "Bitboards" in let () =
   Printf.printf "%d\n" i in print_bitboard state.bitboards.(i) done

   let () = for i = 0 to 2 do let () = Printf.printf "Occupations" in let () =
   Printf.printf "%d\n" i in print_bitboard state.bitboards.(i) done *)

let bot_activated =
  if Array.length Sys.argv < 2 || Sys.argv.(1) <> "bot" then false else true

(* clears the terminal *)
(* let () = print_endline "\x1Bc" *)

(** [get_move source target move_list] returns a move that has the specified
    [source] and [target] coordinates, [None] otherwise*)
let rec get_move source target = function
  | [] -> None
  | h :: t ->
      if get_source_coord h = source && get_target_coord h = target then Some h
      else get_move source target t

(** Handles the user input of a source square - which piece they want to move -
    and where - and returns the move they would like to make *)
let rec handle_input moves =
  let () =
    print_endline
      "Your turn. Please input the coordinate of the piece you would like to \
       move."
  in

  let inp = read_line () in
  let uppercase = String.uppercase_ascii (String.trim inp) in

  if not (Str.string_match (Str.regexp "^[A-H][1-8]$") uppercase 0) then (
    Printf.printf
      "Invalid input, please input another value, a valid value is A-H and \
       1-8, for example A8 or B5 is a valid input.\n";
    handle_input moves)
  else
    let source_coord = to_coord (string_to_rankfile uppercase) in

    print_endline
      "Please input the target square of that piece. Make sure this is a valid \
       coordinate otherwise you will have to pick a new source coordinate as \
       well.";
    let inp = read_line () in
    let uppercase = String.uppercase_ascii (String.trim inp) in

    if not (Str.string_match (Str.regexp "^[A-H][1-8]$") uppercase 0) then (
      Printf.printf
        "Invalid input, please input another value, a valid value is A-H and \
         1-8, for example A8 or B5 is a valid input.\n";
      handle_input moves)
    else
      let target_coord = to_coord (string_to_rankfile uppercase) in

      match get_move source_coord target_coord moves with
      | None ->
          Printf.printf
            "This is not a valid move, please reselect your move. If you need \
             please reread the chess rules.\n";
          handle_input moves
      | Some move -> move

(** [run_two_player state] runs the two player game and just exits the program
    when finished *)
let rec run_two_player state =
  try
    let () = print_board state in
    let () =
      Printf.printf "%s's turn. Please select a piece to move:\n"
        (if state.side = White then "White" else "Black")
    in

    let moves = Struct.Game.generate_moves state in

    (* selected move *)
    let move = handle_input moves in
    let _, new_state = Struct.Game.make_move state move in

    run_two_player new_state
  with Failure f ->
    (*handle exceptions*)
    (* let () = print_endline "\x1Bc" in *)
    let () = print_endline f in
    run_two_player state

let () =
  if not bot_activated then run_two_player state
  else print_endline "Bot Activated!\n"

(* set the difficulty *)
let bot_depth =
  if
    Array.length Sys.argv < 3
    || not (List.mem Sys.argv.(2) [ "easy"; "medium"; "hard" ])
    (*default to easy*)
  then 10
  else
    match String.trim (String.lowercase_ascii Sys.argv.(2)) with
    | "easy" -> 10
    | "medium" -> 20
    | _ -> 30

(* Configure the bot with the proper difficulty and evaluation function *)
module BotSettings = struct
  let max_depth = bot_depth
  let eval_function = Struct.Game.eval
end

(*Create Chess Bot module*)
module ChessBot = InitBotWithSettings (BotSettings)

let rec run_single_player state =
  try
    let () = print_board state in

    (* let () = for i = 0 to 11 do let () = Printf.printf "Bitboards " in let ()
       = Printf.printf "%s\n" (to_piece i) in print_bitboard state.bitboards.(i)
       done

       in

       let () = for i = 0 to 2 do let () = Printf.printf "Occupations " in let
       () = Printf.printf "%d\n" i in print_bitboard state.occupancies.(i) done

       in *)
    if state.side = White then
      let moves = Struct.Game.generate_moves state in

      (* let () = Printf.printf "This is player whites moves\n" in let () =
         Struct.Game.print_moves moves in let () = Printf.printf "\nEnd of
         player white moves\n" in *)

      (* selected move *)
      let move = handle_input moves in
      let () = Struct.Game.print_moves [ move ] in
      let _, new_state = Struct.Game.make_move state move in

      run_single_player new_state
    else
      let _, new_state =
        match ChessBot.get_action state with
        | None -> failwith "BAD ACTION!"
        | Some move ->
            let () = Struct.Game.print_moves [ move ] in
            Struct.Game.make_move state move
      in

      run_single_player new_state
  with Failure f ->
    (*handle exceptions*)
    (* let () = print_endline "\x1Bc" in *)
    let () = print_endline f in
    run_single_player state

let () = run_single_player state
