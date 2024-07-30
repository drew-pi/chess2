open Unsigned.UInt64
open Macros


(*

	Hardcoded Magics

*)
let rook_magics = [|
  of_string ("36033831794409504");
  of_string ("72093878735355905");
  of_string ("2377921496256348224");
  of_string ("900737517735663616");
  of_string ("36031030406414464");
  of_string ("72058693684470784");
  of_string ("36029897637900416");
  of_string ("2341889718947414144");
  of_string ("1189653989612896268");
  of_string ("333336742245437444");
  of_string ("3026841299498042496");
  of_string ("2305983781079613572");
  of_string ("4828421888068838912");
  of_string ("140746078552193");
  of_string ("2595199302608356872");
  of_string ("36591818007380033");
  of_string ("141287246282752");
  of_string ("23100739840672512");
  of_string ("565149514629184");
  of_string ("36311373118459904");
  of_string ("9289774079674368");
  of_string ("72340168666416128");
  of_string ("145135837384753");
  of_string ("73431983715533060");
  of_string ("1297213179331526661");
  of_string ("35185450029056");
  of_string ("40532467521691648");
  of_string ("2550869124448384");
  of_string ("2455588265907454082");
  of_string ("578749937660199040");
  of_string ("15547784269825");
  of_string ("108157043268949764");
  of_string ("306245084444098688");
  of_string ("27164555784234304");
  of_string ("422349912412160");
  of_string ("72198675132057600");
  of_string ("290273225606144");
  of_string ("1153484523715958788");
  of_string ("4614078667406971136");
  of_string ("554352771156");
  of_string ("2909360818825035777");
  of_string ("9024792015421440");
  of_string ("13510936346263568");
  of_string ("70407701004320");
  of_string ("471787460089217034");
  of_string ("36873273488900104");
  of_string ("2533283380920340");
  of_string ("288252384648495113");
  of_string ("9078117782061312");
  of_string ("576601764688560512");
  of_string ("243211972601086080");
  of_string ("2308100858626179200");
  of_string ("864770431033414144");
  of_string ("4644362902569088");
  of_string ("2261721465496576");
  of_string ("7443189072384");
  of_string ("1188951401418727553");
  of_string ("85991705970479137");
  of_string ("18157336096936042");
  of_string ("4583864244651169");
  of_string ("289919312717160482");
  of_string ("1688884488506434");
  of_string ("37452132122756");
  of_string ("2306406513285537874");
|]



let rook_mask = [|
  (of_string "282578800148862"); 
  (of_string "565157600297596"); 
  (of_string "1130315200595066"); 
  (of_string "2260630401190006"); 
  (of_string "4521260802379886"); 
  (of_string "9042521604759646"); 
  (of_string "18085043209519166"); 
  (of_string "36170086419038334"); 
  (of_string "282578800180736"); 
  (of_string "565157600328704"); 
  (of_string "1130315200625152"); 
  (of_string "2260630401218048"); 
  (of_string "4521260802403840"); 
  (of_string "9042521604775424"); 
  (of_string "18085043209518592"); 
  (of_string "36170086419037696"); 
  (of_string "282578808340736"); 
  (of_string "565157608292864"); 
  (of_string "1130315208328192"); 
  (of_string "2260630408398848"); 
  (of_string "4521260808540160"); 
  (of_string "9042521608822784"); 
  (of_string "18085043209388032"); 
  (of_string "36170086418907136"); 
  (of_string "282580897300736"); 
  (of_string "565159647117824"); 
  (of_string "1130317180306432"); 
  (of_string "2260632246683648"); 
  (of_string "4521262379438080"); 
  (of_string "9042522644946944"); 
  (of_string "18085043175964672"); 
  (of_string "36170086385483776"); 
  (of_string "283115671060736"); 
  (of_string "565681586307584"); 
  (of_string "1130822006735872"); 
  (of_string "2261102847592448"); 
  (of_string "4521664529305600"); 
  (of_string "9042787892731904"); 
  (of_string "18085034619584512"); 
  (of_string "36170077829103616"); 
  (of_string "420017753620736"); 
  (of_string "699298018886144"); 
  (of_string "1260057572672512"); 
  (of_string "2381576680245248"); 
  (of_string "4624614895390720"); 
  (of_string "9110691325681664"); 
  (of_string "18082844186263552"); 
  (of_string "36167887395782656"); 
  (of_string "35466950888980736"); 
  (of_string "34905104758997504"); 
  (of_string "34344362452452352"); 
  (of_string "33222877839362048"); 
  (of_string "30979908613181440"); 
  (of_string "26493970160820224"); 
  (of_string "17522093256097792"); 
  (of_string "35607136465616896"); 
  (of_string "9079539427579068672"); 
  (of_string "8935706818303361536"); 
  (of_string "8792156787827803136"); 
  (of_string "8505056726876686336"); 
  (of_string "7930856604974452736"); 
  (of_string "6782456361169985536"); 
  (of_string "4485655873561051136"); 
  (of_string "9115426935197958144"); 
|]




let rooks_relevent_possible_moves = [|
  12; 11; 11; 11; 11; 11; 11; 12;  
  11; 10; 10; 10; 10; 10; 10; 11;  
  11; 10; 10; 10; 10; 10; 10; 11;  
  11; 10; 10; 10; 10; 10; 10; 11;  
  11; 10; 10; 10; 10; 10; 10; 11;  
  11; 10; 10; 10; 10; 10; 10; 11;  
  11; 10; 10; 10; 10; 10; 10; 11;  
  12; 11; 11; 11; 11; 11; 11; 12;
|]

(*
Left - from E3

	8     0  0  0  0  0  0  0  0  
 	7     0  0  0  0  0  0  0  0  
 	6     0  0  0  0  0  0  0  0  
 	5     0  0  0  0  0  0  0  0  
 	4     0  0  0  0  0  0  0  0  
 	3     0  1  1  1  0  0  0  0  
 	2     0  0  0  0  0  0  0  0  
 	1     0  0  0  0  0  0  0  0  
 
	      a  b  c  d  e  f  g  h
*)

(**Left [bitboard rank file] where rank and file refer to the position of the rook*)
let rec iterate_file_l bitboard (r, f) = if f <= 1 then bitboard else let move = set_bit zero (get_coord r (f-1)) in iterate_file_l (logor bitboard move) (r, (f-1))

(*
Down - from E5

	8     0  0  0  0  0  0  0  0  
 	7     0  0  0  0  0  0  0  0  
 	6     0  0  0  0  0  0  0  0  
 	5     0  0  0  0  0  0  0  0  
 	4     0  0  0  0  1  0  0  0  
 	3     0  0  0  0  1  0  0  0  
 	2     0  0  0  0  1  0  0  0  
 	1     0  0  0  0  0  0  0  0  
 
	      a  b  c  d  e  f  g  h
*)

(** Down *)
let rec iterate_file_d bitboard (r, f) = if r > 5 then bitboard else let move = set_bit zero (get_coord (r+1) f) in iterate_file_d (logor bitboard move) ((r+1), f)

(*
Up - from E3

	8     0  0  0  0  0  0  0  0  
 	7     0  0  0  0  1  0  0  0  
 	6     0  0  0  0  1  0  0  0  
 	5     0  0  0  0  1  0  0  0  
 	4     0  0  0  0  1  0  0  0  
 	3     0  0  0  0  0  0  0  0  
 	2     0  0  0  0  0  0  0  0  
 	1     0  0  0  0  0  0  0  0  
 
	      a  b  c  d  e  f  g  h
*)

(** Up *)
let rec iterate_file_u bitboard (r, f) = if r <= 1 then bitboard else let move = set_bit zero (get_coord (r-1) f) in iterate_file_u (logor bitboard move) ((r-1), f)

(*
Right - from D4

	8     0  0  0  0  0  0  0  0  
 	7     0  0  0  0  0  0  0  0  
 	6     0  0  0  0  0  0  0  0  
 	5     0  0  0  0  0  0  0  0  
 	4     0  0  0  0  1  1  1  0  
 	3     0  0  0  0  0  0  0  0  
 	2     0  0  0  0  0  0  0  0  
 	1     0  0  0  0  0  0  0  0  
 
	      a  b  c  d  e  f  g  h
*)

(** Right *)
let rec iterate_file_r bitboard (r, f) = if f > 5 then bitboard else let move = set_bit zero (get_coord r (f+1)) in iterate_file_r (logor bitboard move) (r, (f+1))

let create_rook_mask (rank, file) = 
  let l = iterate_file_l zero (rank, file) in
  let r = iterate_file_r zero (rank, file) in
  let u = iterate_file_u zero (rank, file) in
  let d = iterate_file_d zero (rank, file) in
  logor l (logor r (logor u d))


(** Create the rook blocker mask, every possible place that a blocker may be *)
(* let init_rook_mask () = 
  let rec init n = 
    if n > 63 then () else
		let () = Printf.printf "  (of_string \"%s\"); \n" (to_string (create_rook_mask (from_coord_rf n))) in
    (* let () = rook_attacks.(n) <- create_rook_mask (n / 8) (n mod 8) in *)
    init (n+1)
  in
	let () = Printf.printf "let rook_mask = [|\n" in
  let () = init 0 in
	let () = Printf.printf "|]\n\n" in 
	() *)

 
 
(* let print_rook_masks () = 
  for i = 0 to 63 do
    let _ = Printf.printf " %d" i in 
    print_bitboard (create_rook_mask (from_coord_rf i))
  done *)




(*
Left - from E3 (with a blocker of zero)

	8     0  0  0  0  0  0  0  0  
 	7     0  0  0  0  0  0  0  0  
 	6     0  0  0  0  0  0  0  0  
 	5     0  0  0  0  0  0  0  0  
 	4     0  0  0  0  0  0  0  0  
 	3     1  1  1  1  0  0  0  0  
 	2     0  0  0  0  0  0  0  0  
 	1     0  0  0  0  0  0  0  0  
 
	      a  b  c  d  e  f  g  h
*)

(**Left [bitboard rank file] where rank and file refer to the position of the rook*)
let rec gen_attack_file_l bitboard (r, f) blocker = if f <= 0 then bitboard else 
	let move = set_bit zero (get_coord r (f-1)) in
	if (equal (logand move blocker) zero) then
		(* continue generating attack as did not hit a blocker *)
	 gen_attack_file_l (logor bitboard move) (r, (f-1)) blocker
	 (* Add this move as an attack and return*)
	else logor bitboard move

(*
Down - from E5 (with a blocker of zero)

	8     0  0  0  0  0  0  0  0  
 	7     0  0  0  0  0  0  0  0  
 	6     0  0  0  0  0  0  0  0  
 	5     0  0  0  0  0  0  0  0  
 	4     0  0  0  0  1  0  0  0  
 	3     0  0  0  0  1  0  0  0  
 	2     0  0  0  0  1  0  0  0  
 	1     0  0  0  0  1  0  0  0  
 
	      a  b  c  d  e  f  g  h
*)

(** Down *)
let rec gen_attack_file_d bitboard (r, f) blocker = if r > 6 then bitboard else 
	let move = set_bit zero (get_coord (r+1) f) in 
	if (equal (logand move blocker) zero) then
		(* continue generating attack as did not hit a blocker *)
		gen_attack_file_d (logor bitboard move) ((r+1), f) blocker
	 (* Add this move as an attack and return*)
	else logor bitboard move
	

(*
Up - from E3 (with a blocker of zero)

	8     0  0  0  0  1  0  0  0  
 	7     0  0  0  0  1  0  0  0  
 	6     0  0  0  0  1  0  0  0  
 	5     0  0  0  0  1  0  0  0  
 	4     0  0  0  0  1  0  0  0  
 	3     0  0  0  0  0  0  0  0  
 	2     0  0  0  0  0  0  0  0  
 	1     0  0  0  0  0  0  0  0  
 
	      a  b  c  d  e  f  g  h
*)

(** Up *)
let rec gen_attack_file_u bitboard (r, f) blocker = if r <= 0 then bitboard else 
	let move = set_bit zero (get_coord (r-1) f) in 
	if (equal (logand move blocker) zero) then
		(* continue generating attack as did not hit a blocker *)
		gen_attack_file_u (logor bitboard move) ((r-1), f) blocker
	 (* Add this move as an attack and return*)
	else logor bitboard move
	
	

(*
Right - from D4 (with a blocker of zero)

	8     0  0  0  0  0  0  0  0  
 	7     0  0  0  0  0  0  0  0  
 	6     0  0  0  0  0  0  0  0  
 	5     0  0  0  0  0  0  0  0  
 	4     0  0  0  0  1  1  1  1  
 	3     0  0  0  0  0  0  0  0  
 	2     0  0  0  0  0  0  0  0  
 	1     0  0  0  0  0  0  0  0  
 
	      a  b  c  d  e  f  g  h
*)

(** Right *)
let rec gen_attack_file_r bitboard (r, f) blocker = if f > 6 then bitboard else 
	let move = set_bit zero (get_coord r (f+1)) in 
	if (equal (logand move blocker) zero) then
		(* continue generating attack as did not hit a blocker *)
		gen_attack_file_r (logor bitboard move) (r, (f+1)) blocker
	 (* Add this move as an attack and return*)
	else logor bitboard move
	





(** Generate rook attacks given a blocker setup *)
let generate_rook_attack coord blocker_mask = 
	let (rank,file) = from_coord_rf coord in
	let l = gen_attack_file_l zero (rank,file) blocker_mask in
	let r = gen_attack_file_r zero (rank,file) blocker_mask in
	let u = gen_attack_file_u zero (rank,file) blocker_mask in
	let d = gen_attack_file_d zero (rank,file) blocker_mask in
	logor l (logor r (logor u d))


(* let print_rook_attacks () = 
	for i = 0 to 63 do
		let _ = Printf.printf " %d" i in 
		print_bitboard (generate_rook_attack i zero)
	done *)
