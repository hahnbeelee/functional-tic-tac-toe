(* A set of types and functions that represent and manipulate the state of a 
    game of ultimate tic-tac-toe. *)

(* A type representing a tic-tac-toe board. *)
type small_board = (char array) array

(* An abstract type representing the state of a game of ultimate tic-tac-toe. *)
type t = {
  mutable p1 : string;
  mutable p1_pts : int;
  mutable p2 : string;
  mutable p2_pts : int;
  mutable is_p1_turn : bool;
  mutable big_board : (small_board array) array;
  mutable current_board : int;
  mutable is_it_start : bool;
  mutable final_board : (char array) array;
  mutable play_anywhere : bool
}

(* Raised when any mark function is passed an invalid position argument. *)
exception Invalid_pos

(* Raised when a player attempts to mark outside of the current board. *)
exception Wrong_board

(* Raised when a player attempts to mark in a board that does not exist. *)
exception Invalid_board_num

(* Raised when any mark function is passed an argument that results in the win 
   condition or tie condition being met. *)
exception Game_over of string

(* [get_p1 ust] is a string representing the name of player one in the game 
    represented by [ust]. *)
let get_p1 ust =
  ust.p1

(* [get_p2 ust] is a string with the name of player two in the game represented 
    by [ust]. *)
let get_p2 ust =
  ust.p2

(* [get_p1_score ust] is an int with the score of player one in the game 
    represented by [ust]. *)
let get_p1_score ust =
  ust.p1_pts

(* [get_p2_score ust] is an int representing the score of player two in the game 
    represented by [ust]. *)
let get_p2_score ust =
  ust.p2_pts

(* [get_board ust] is the an array of arrays of tic-tac-toe boards that
   represents an ultimate tic-tac-toe board. *)
let get_board ust =
  ust.big_board

(* [get_final_board ust] is a tic-tac board that represents who has won in the 
    ultimate tic-tac-toe board. *)
let get_final_board ust =
  ust.final_board

(* [print_pts ust] prints the number of points each player has in the game
    represented by [ust]. *)
let print_pts ust =
  print_endline (ust.p1 ^ " has " ^ string_of_int ust.p1_pts ^ " points.");
  print_endline (ust.p2 ^ " has " ^ string_of_int ust.p2_pts ^ " points.")

(** [check_hori ust sb] is (b, p) where is b is a bool that is true if the board
    [sb] has been won by claiming three positions in a row and p is a string 
    with the name of the winning player or the empty string if b is false.  *)
let check_hori ust sb =
  if Array.for_all (fun x -> x = '1') sb.(0) then (true, ust.p1) else
  if Array.for_all (fun x -> x = '0') sb.(0) then (true, ust.p2) else
  if Array.for_all (fun x -> x = '1') sb.(1) then (true, ust.p1) else
  if Array.for_all (fun x -> x = '0') sb.(1) then (true, ust.p2) else
  if Array.for_all (fun x -> x = '1') sb.(2) then (true, ust.p1) else
  if Array.for_all (fun x -> x = '0') sb.(2) then (true, ust.p2) else 
    (false, "")

(** [check_vert ust sb] is (b, p) where is b is a bool that is true if the board 
    [sb] has been won by claiming three positions in a column and p is a string 
    with the name of the winning player or the empty string if b is false. *)
let check_vert ust sb =
  if Array.for_all (fun x -> (x.(0) = '1')) sb then (true, ust.p1) else
  if Array.for_all (fun x -> (x.(0) = '0')) sb then (true, ust.p2) else
  if Array.for_all (fun x -> (x.(1) = '1')) sb then (true, ust.p1) else
  if Array.for_all (fun x -> (x.(1) = '0')) sb then (true, ust.p2) else
  if Array.for_all (fun x -> (x.(2) = '1')) sb then (true, ust.p1) else
  if Array.for_all (fun x -> (x.(2) = '0')) sb then (true, ust.p2) else 
    (false, "")

(** [check_diag ust sb] is (b, p) where is b is a bool that is true if the board 
    [sb] has been won by claiming three positions in a row diagonally and p is a 
    string with the name of the winning player or the empty string if b is 
    false. *)
let check_diag ust sb =
  match sb with 
  | [| [|'1'; _; _|]; [|_; '1'; _|]; [|_; _; '1'|] |]
  | [| [|_; _; '1'|]; [|_; '1'; _|]; [|'1'; _; _|] |] -> (true, ust.p1)
  | [| [|'0'; _; _|]; [|_; '0'; _|]; [|_; _; '0'|] |]
  | [| [|_; _; '0'|]; [|_; '0'; _|]; [|'0'; _; _|] |] -> (true, ust.p2)
  | _ -> (false, "")

(** [get_winner sb] is (b, p) where is b is a boolean that is true if the board 
    [sb] has been won and p is a string with the name of the winning player or 
    the empty string if b is false. *)
let get_winner ust sb =
  let horizontal = check_hori ust sb in
  if fst horizontal then horizontal else
    let vertical = check_vert ust sb in
    if fst vertical then vertical else check_diag ust sb

(** [init_small_board] is char array array representing a tic-tac-toe board. *)
let init_small_board () = [|
  [|'A'; 'B'; 'C'|];
  [|'D'; 'E'; 'F'|];
  [|'G'; 'H'; 'I'|]
|]

(* [init_state p1 p2] is the initial state of a game with players [p1] and
   [p2]. *)
let init_state p1 p2 =
  {
    p1 = p1;
    p1_pts = 0;
    p2 = p2;
    p2_pts = 0;
    is_p1_turn = true;
    big_board = [| 
      [|init_small_board (); init_small_board (); init_small_board ()|];
      [|init_small_board (); init_small_board (); init_small_board ()|];
      [|init_small_board (); init_small_board (); init_small_board ()|] |];
    current_board = 5; (* The first move must be in the middle board. *)
    is_it_start = true;
    final_board = init_small_board ();
    play_anywhere = false
  }

(** [small_board_num ust board_num] is the small board associated with 
    [board_num] in [ust]. *)
let small_board_num ust = function
  | 1 -> ust.big_board.(0).(0)
  | 2 -> ust.big_board.(0).(1)
  | 3 -> ust.big_board.(0).(2)
  | 4 -> ust.big_board.(1).(0)
  | 5 -> ust.big_board.(1).(1)
  | 6 -> ust.big_board.(1).(2)
  | 7 -> ust.big_board.(2).(0)
  | 8 -> ust.big_board.(2).(1)
  | 9 -> ust.big_board.(2).(2)
  | _ -> raise Invalid_board_num

(** [final_board_num ust board_num] is the character associated with [board_num] 
    in the final board of [ust]. *)
let final_board_num ust = function
  | 1 -> ust.final_board.(0).(0)
  | 2 -> ust.final_board.(0).(1)
  | 3 -> ust.final_board.(0).(2)
  | 4 -> ust.final_board.(1).(0)
  | 5 -> ust.final_board.(1).(1)
  | 6 -> ust.final_board.(1).(2)
  | 7 -> ust.final_board.(2).(0)
  | 8 -> ust.final_board.(2).(1)
  | 9 -> ust.final_board.(2).(2)
  | _ -> raise Invalid_board_num

(** [is_tie lst] is true if all elements of the arrays in [lst] are '1' or
    '0' and is otherwise false.
    Requires: [lst] is a list of arrays representing the rows of a tic-tac-toe 
    board. *)
let rec is_tie = function
  | [] -> true
  | h :: t -> Array.for_all (fun x -> (x = '0' || x = '1')) h && is_tie t

(** [is_unmarked ust cd] is false if the coordinate [cd] is marked on the 
    board in the game represented by [ust]. 
    Requires: [cd] is a valid coordinate on a board in the game represented by 
    [ust]. *)
let is_unmarked ust (cd: int * int) =
  let pos = ust.final_board.(fst cd).(snd cd) in
  if pos = '1' || pos = '0' then false else true

(** [mark_final_board_hlpr ust pos] marks the space at position [pos] on the 
    final board with for the player whose turn it is according to [ust].
    Raises: [Invalid_pos] if [pos] is already marked or is not a valid position
    on the board. *)
let mark_final_board_hlpr ust pos =
  let board = ust.final_board
  and p_mrk = if ust.is_p1_turn then '1' else '0' 
  in
  match pos with
  | 'A' when is_unmarked ust (0, 0) -> board.(0).(0) <- p_mrk
  | 'B' when is_unmarked ust (0, 1) -> board.(0).(1) <- p_mrk
  | 'C' when is_unmarked ust (0, 2) -> board.(0).(2) <- p_mrk
  | 'D' when is_unmarked ust (1, 0) -> board.(1).(0) <- p_mrk
  | 'E' when is_unmarked ust (1, 1) -> board.(1).(1) <- p_mrk
  | 'F' when is_unmarked ust (1, 2) -> board.(1).(2) <- p_mrk
  | 'G' when is_unmarked ust (2, 0) -> board.(2).(0) <- p_mrk
  | 'H' when is_unmarked ust (2, 1) -> board.(2).(1) <- p_mrk
  | 'I' when is_unmarked ust (2, 2) -> board.(2).(2) <- p_mrk
  | _ -> raise Invalid_pos

(** [mark_final_board ust pos] marks the space at position [pos] on the final 
    board with for the player whose turn it is according to [ust].
    Raises: [Invalid_pos] if [pos] is already marked or is not a valid position
    on the board.
    Raises: [Game_over] if marking position [pos] causes the win condition or
    tie condition to be met. *)
let mark_final_board ust pos =
  let winner = get_winner (mark_final_board_hlpr ust pos; ust) ust.final_board in
  if fst winner then begin
    ust.is_p1_turn <- not ust.is_p1_turn;
    raise (Game_over (snd winner)) end
  else if is_tie (Array.to_list ust.final_board) then raise (Game_over "tie")
(* else ust.is_p1_turn <- not ust.is_p1_turn *)

let is_small_board_unmarked ust (cd: int * int) sb =
  let pos = sb.(fst cd).(snd cd) in
  if pos = '1' || pos = '0' then false else true

let mark_pos_hlpr ust sb pos =
  let p_mrk = if ust.is_p1_turn then '1' else '0' in
  match pos with
  | 'A' when is_small_board_unmarked ust (0, 0) sb -> sb.(0).(0) <- p_mrk;
    ust.current_board <- 1; ust.is_it_start <- false;
  | 'B' when is_small_board_unmarked ust (0, 1) sb -> sb.(0).(1) <- p_mrk; 
    ust.current_board <- 2; ust.is_it_start <- false;
  | 'C' when is_small_board_unmarked ust (0, 2) sb -> sb.(0).(2) <- p_mrk; 
    ust.current_board <- 3; ust.is_it_start <- false;
  | 'D' when is_small_board_unmarked ust (1, 0) sb -> sb.(1).(0) <- p_mrk; 
    ust.current_board <- 4; ust.is_it_start <- false;
  | 'E' when is_small_board_unmarked ust (1, 1) sb -> sb.(1).(1) <- p_mrk; 
    ust.current_board <- 5; ust.is_it_start <- false;
  | 'F' when is_small_board_unmarked ust (1, 2) sb -> sb.(1).(2) <- p_mrk; 
    ust.current_board <- 6; ust.is_it_start <- false;
  | 'G' when is_small_board_unmarked ust (2, 0) sb -> sb.(2).(0) <- p_mrk; 
    ust.current_board <- 7; ust.is_it_start <- false;
  | 'H' when is_small_board_unmarked ust (2, 1) sb -> sb.(2).(1) <- p_mrk; 
    ust.current_board <- 8; ust.is_it_start <- false;
  | 'I' when is_small_board_unmarked ust (2, 2) sb -> sb.(2).(2) <- p_mrk; 
    ust.current_board <- 9; ust.is_it_start <- false;
  | _ -> raise Invalid_pos

(** [mark_pos ust bn pos] marks the space at position [pos] on the board with 
    board number [bn] for the player whose turn it is according to [ust].
    Raises: [Wrong_board] if [pos] is not in the correct board.
    Raises: [Invalid_pos] if [pos] is already marked or is not a valid position
    on the board.
    Raises: [Game_over] if marking position [pos] causes the win condition or
    tie condition to be met. *)
let mark_pos ust board_num pos =
  let sb = small_board_num ust board_num in
  if ust.play_anywhere then (mark_pos_hlpr ust sb pos; ust)
  else if ust.is_it_start && board_num <> 5 
  then (ust.play_anywhere <- false; raise Wrong_board) else 
  if board_num <> ust.current_board 
  then (ust.play_anywhere <- false; raise Wrong_board) else
    let finalboard_num = final_board_num ust board_num in
    if finalboard_num = '0' || finalboard_num = '1'
    then (ust.play_anywhere <- true)
    else if is_tie (Array.to_list sb)
    then (ust.play_anywhere <- true) 
    else
      mark_pos_hlpr ust sb pos;
    ust

let mark ust board_num pos =
  let winner = get_winner (mark_pos ust board_num pos)
      (small_board_num ust board_num) in
  if fst winner then begin
    ust.play_anywhere <- true;
    let place = final_board_num ust board_num in
    if (place <> '0' && place <> '1') (* if place in final board not taken *)
    then mark_final_board ust place
    else failwith "FATAL ERROR IN ULTIMATESTATE.MARK" end;
  ust.is_p1_turn <- not ust.is_p1_turn

(** [format_row lst acc] is a string representation of a row of a small board 
    in our ultimate tic tac toe game. *)
let rec format_row lst acc =
  let format_row_hlpr = function
    | '1' -> " X "
    | '0' -> " O "
    | x -> " " ^ Char.escaped x ^ " "
  in
  match lst with
  | [] -> acc
  | ch :: [] -> acc ^ format_row_hlpr ch
  | ch :: t -> format_row t (acc ^ format_row_hlpr ch ^ "|")

(** [format_top_row sb] is a formatted string representing [sb].
    Requires: [sb] is a valid representation of a tic-tac-toe board. 
*)
let format_top_row sb =
  format_row (Array.to_list sb.(0)) ""

(** [format_mid_row sb] is a formatted string representing [sb].
    Requires: [sb] is a valid representation of a tic-tac-toe board. 
*)
let format_mid_row sb =
  format_row (Array.to_list sb.(1)) ""

(** [format_bot_row sb] is a formatted string representing [sb].
    Requires: [sb] is a valid representation of a tic-tac-toe board. 
*)
let format_bot_row sb =
  format_row (Array.to_list sb.(2)) ""

(** [border_between_rows] is a string representing the boarder between rows in a 
    tic-tac-toe board. *)
let border_between_rows = " \n----------- | ----------- | -----------\n "

(** [border_between_bigrows] is a string representing the boarder between rows 
    in an ultimate tic-tac-toe board. *)
let border_between_bigrows = " \n---------------------------------------\n "

(** [format_bigrow arr] is a formatted string representing [arr].
    Requires: [arr] is a valid representation of an ultimate tic-tac-toe board. 
*)
let format_bigrow board_array =
  format_top_row (board_array.(0)) ^ " | " ^ 
  format_top_row (board_array.(1)) ^ " | " ^
  format_top_row (board_array.(2)) ^ border_between_rows ^
  format_mid_row (board_array.(0)) ^ " | " ^
  format_mid_row (board_array.(1)) ^ " | " ^
  format_mid_row (board_array.(2)) ^ border_between_rows ^
  format_bot_row (board_array.(0)) ^ " | " ^
  format_bot_row (board_array.(1)) ^ " | " ^
  format_bot_row (board_array.(2))

(* let smallboards_to_char_arrays board_ray =
   board_ray |> Array.to_list |> List.map (get_small_board) |> Array.of_list *)

(* [format_row_list lst acc] is a formatted string representing an ultimate 
   tic-tac-toe board.
    Requires: [lst] is a valid representation of an ultimate tic-tac-toe board. 
*)
let rec format_row_list rowlst acc =
  match rowlst with
  | [] -> acc
  | row :: [] -> acc ^ (format_bigrow row)
  | row :: t -> 
    format_row_list t (acc ^ format_bigrow row ^ border_between_bigrows)

(** [print_board_hlpr lst] prints a representation of the board in the game 
    represented by [st]. *)
let rec print_board_hlpr = function
  | [] -> print_endline "\n"; ()
  | h :: t -> 
    match h with
    | "X" -> ANSITerminal.(print_string [blue] h);
      print_board_hlpr t
    | "O" -> ANSITerminal.(print_string [red] h);
      print_board_hlpr t
    | "|" -> print_string (" " ^ h ^ " ");
      print_board_hlpr t
    | "" -> print_string (" ");
      print_board_hlpr t
    | _ -> print_string h;
      print_board_hlpr t

let rec format_bigboard ust =
  let smallboard_row_lst = ust.big_board |> Array.to_list in
  (* let char_array_lst = List.map (smallboards_to_char_arrays) smallboard_row_lst in *)
  format_row_list smallboard_row_lst ""

(** [print_board_hlpr lst] prints a representation of the board in the game 
    represented by [st]. *)
let rec print_board_hlpr = function
  | [] -> print_endline "\n"; ()
  | h :: t -> 
    match h with
    | "X" -> ANSITerminal.(print_string [blue] h);
      print_board_hlpr t
    | "O" -> ANSITerminal.(print_string [red] h);
      print_board_hlpr t
    | "|" -> print_string (" " ^ h ^ " ");
      print_board_hlpr t
    | "" -> print_string " ";
      print_board_hlpr t
    | _ -> print_string (h);
      print_board_hlpr t

(* [print_board ust] prints a representation of the board in the game 
    represented by [ust]. *)
let print_board ust =
  let lst = format_bigboard ust |> String.split_on_char ' '
  in print_board_hlpr lst
(* print_endline (format_bigboard ust) *)

let rec board_to_str_hlpr lsts acc = 
  match lsts with
  | lst :: [] -> format_row lst acc
  | lst :: t -> board_to_str_hlpr t ((format_row lst acc) ^ "\n-----------\n ")
  | [] -> acc

(** [board_to_str st] is a string representation of the board in the game 
    represented by [st]. *)
let board_to_str st = 
  let lsts = Array.to_list st.final_board |> List.map Array.to_list in 
  board_to_str_hlpr lsts ""

(* [print_board st] prints a representation of the board in the game 
    represented by [st]. *)
let print_final_board st =
  print_endline "";
  let lst = st |> board_to_str |> String.split_on_char ' '
  in print_board_hlpr lst

(* [print_turn ust] prints a representation of the board and whose turn it is in 
    the game represented by [ust]. *)
let print_turn ust =
  print_board ust;
  print_endline "\nFinal board";
  print_final_board ust;
  if ust.is_p1_turn then
    begin
      ANSITerminal.(print_string [blue] ust.p1);
      print_endline ("'s turn: ")
    end
  else
    begin
      ANSITerminal.(print_string [red] ust.p2);
      print_endline("'s turn: ")
    end

(* [continue ust] resets the board and turn in [ust] to represent the start of a 
    new game. *)
let continue ust =
  ust.big_board <- [|
    [|init_small_board (); init_small_board (); init_small_board ()|];
    [|init_small_board (); init_small_board (); init_small_board ()|];
    [|init_small_board (); init_small_board (); init_small_board ()|] |];
  ust.final_board <- init_small_board ();
  ust.is_p1_turn <- true;
  ust.is_it_start <- true;
  ust.current_board <- 5