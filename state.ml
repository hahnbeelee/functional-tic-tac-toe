(* A set of types and functions that represent and manipulate the state of a 
    game of tic-tac-toe. *)

(* An abstract type representing the state of a game of tic-tac-toe. *)
type mode =
  | Easy
  | Medium
  | Hard
  | Classic
  | Ultimate

type t = {
  mutable p1 : string;
  mutable p1_pts : int;
  mutable p2 : string;
  mutable p2_pts : int;
  mutable is_p1_turn : bool;
  mutable board : (char array) array;
  diff_lvl : mode;
}


(* Raised when mark is passed an invalid position argument. *)
exception Invalid_pos

(* Raised when mark is passed an argument that results in the win condition 
    or tie condition being met. *)
exception Game_over of string

(* [init_state p1 p2] is the initial state of a game with players [p1] and
    [p2]. *)
let init_state p1 p2 md=
  {
    p1 = p1;
    p1_pts = 0;
    p2 = p2;
    p2_pts = 0;
    is_p1_turn = true;
    board = [| [|'A'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|] |];
    diff_lvl = md;
  }

(* [get_p1 st] is a string representing the name of player one in the game 
    represented by [st]. *)
let get_p1 st =
  st.p1

(* [get_p2 st] is a string with the name of player two in the game represented 
    by [st]. *)
let get_p2 st =
  st.p2

(* [get_p1_score st] is an int with the score of player one in the game 
    represented by [st]. *)
let get_p1_score st =
  st.p1_pts

(* [get_p2_score st] is an int representing the score of player two in the game 
    represented by [st]. *)
let get_p2_score st =
  st.p2_pts

let get_diff_lvl st =
  st.diff_lvl

(* [get_board st] is the board in the game represented by [st]. *)
let get_board st =
  st.board

let get_is_p1_turn st =
  st.is_p1_turn

(* [print_pts st] prints the number of points each player has in the game
    represented by [st]. *)
let print_pts st =
  print_endline (st.p1 ^ " has " ^ string_of_int st.p1_pts ^ " points.");
  if st.p2 = "" then
    print_endline ("The AI" ^ " has " ^ string_of_int st.p2_pts ^ " points.")
  else
    print_endline (st.p2 ^ " has " ^ string_of_int st.p2_pts ^ " points.")

(** [format_row lst acc] is a formatted string representing [lst].
    Requires: [lst] is a valid representation of a row in a tic-tac-toe board. 
*)
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

(** [board_to_str_hlpr lsts acc] is a string representation of the board in the 
    game represented by [st].
    Requires: [lsts] is valid representation of the rows in a tic-tac-toe board. 
*)
let rec board_to_str_hlpr lsts acc = 
  match lsts with
  | lst :: [] -> format_row lst acc
  | lst :: t -> board_to_str_hlpr t ((format_row lst acc) ^ "\n-----------\n")
  | [] -> acc

(** [board_to_str st] is a string representation of the board in the game 
    represented by [st]. *)
let board_to_str st = 
  let lsts = Array.to_list st.board |> List.map Array.to_list
  in board_to_str_hlpr lsts ""

(** [print_board_hlpr lst] prints a representation of the board in the game 
    represented by [st]. *)
let rec print_board_hlpr = function
  | [] -> print_endline "\n"; ()
  | h :: t -> 
    match h with
    | "X" -> ANSITerminal.(print_string [blue] h); print_board_hlpr t
    | "O" -> ANSITerminal.(print_string [red] h); print_board_hlpr t
    | "|" -> print_string (" " ^ h ^ " "); print_board_hlpr t
    | _ -> print_string h; print_board_hlpr t

(* [print_board st] prints a representation of the board in the game 
    represented by [st]. *)
let print_board st =
  print_endline "";
  let lst = st |> board_to_str |> String.split_on_char ' '
  in print_board_hlpr lst

(* [print_turn st] prints a representation of the board and whose turn it is in 
    the game represented by [st]. *)
let print_turn st =
  print_endline "";
  print_board st;
  if st.is_p1_turn then 
    begin
      ANSITerminal.(print_string [blue] st.p1);
      print_endline ("'s turn: ")
    end 
  else 
    begin
      ANSITerminal.(print_string [red] st.p2);
      print_endline ("'s turn: ")
    end

(* [reset_turn st] resets the turn in [st] to the first player. *)
let reset_turn st =
  st.is_p1_turn <- true

(** [check_hori st] is (b, p) where is b is a boolean that is true if the game
    has been won by claiming three positions in a row on the board in the game 
    represented by [st] and p is a string with the name of the winning player or 
    the empty string if b is false. *)
let check_hori st =
  let board = st.board in
  if Array.for_all (fun x -> x = '1') board.(0) then (true, st.p1) else
  if Array.for_all (fun x -> x = '0') board.(0) then (true, st.p2) else
  if Array.for_all (fun x -> x = '1') board.(1) then (true, st.p1) else
  if Array.for_all (fun x -> x = '0') board.(1) then (true, st.p2) else
  if Array.for_all (fun x -> x = '1') board.(2) then (true, st.p1) else
  if Array.for_all (fun x -> x = '0') board.(2) then (true, st.p2) else 
    (false, "")

(** [check_vert st] is (b, p) where is b is a bool that is true if the game has 
    been won by claiming three positions in a column on the board in the game 
    represented by [st] and p is a string with the name of the  winning player 
    or the empty string if b is false. *)
let check_vert st =
  let board = st.board in
  if Array.for_all (fun x -> (x.(0) = '1')) board then (true, st.p1) else
  if Array.for_all (fun x -> (x.(0) = '0')) board then (true, st.p2) else
  if Array.for_all (fun x -> (x.(1) = '1')) board then (true, st.p1) else
  if Array.for_all (fun x -> (x.(1) = '0')) board then (true, st.p2) else
  if Array.for_all (fun x -> (x.(2) = '1')) board then (true, st.p1) else
  if Array.for_all (fun x -> (x.(2) = '0')) board then (true, st.p2) else 
    (false, "")

(** [check_diag st] is (b, p) where is b is a bool that is true if the game has 
    been won by claiming three positions in a row diagonally on the board in the 
    game represented by [st] and p is a string with the name of the winning 
    player or the empty string if b is false. *)
let check_diag st =
  match st.board with 
  | [| [|'1'; _; _|]; [|_; '1'; _|]; [|_; _; '1'|] |]
  | [| [|_; _; '1'|]; [|_; '1'; _|]; [|'1'; _; _|] |] -> (true, st.p1)
  | [| [|'0'; _; _|]; [|_; '0'; _|]; [|_; _; '0'|] |]
  | [| [|_; _; '0'|]; [|_; '0'; _|]; [|'0'; _; _|] |] -> (true, st.p2)
  | _ -> (false, "")

(** [get_winner st] is (b, p) where is b is a boolean that is true if the game
    has a winner and p is a string with the name of the winning player or 
    the empty string if b is false. *)
let get_winner st =
  let horizontal = check_hori st in
  if fst horizontal then horizontal else
    let vertical = check_vert st in
    if fst vertical then vertical else check_diag st

(** [incr_pts st p] increments the points of player [p] in [st] by 1. *)
let incr_pts st player =
  match player with
  | p when p = st.p1 -> st.p1_pts <- st.p1_pts + 1
  | p when p = st.p2 -> st.p2_pts <- st.p2_pts + 1
  | _ -> ()

(** [is_tie lst] is true if all elements of the arrays in [lst] are '1' or
    '0' and is otherwise false.
    Requires: [lst] is a list of arrays representing the rows of a tic-tac-toe 
    board. *)
let rec is_tie = function
  | [] -> true
  | h :: t -> Array.for_all (fun x -> (x = '0' || x = '1')) h && is_tie t

(** [is_unmarked st cd] is false if the coordinate [cd] is marked on the board 
    in the game represented by [st]. 
    Requires: [cd] is a valid coordinate on the board in the game represented by 
    [st]. *)
let is_unmarked st (cd: int * int) =
  let pos = st.board.(fst cd).(snd cd) in
  if pos = '1' || pos = '0' then false else true

(** [swap_turn st] changes the turn to the opposite player's turn in the game 
    represented by [st]. *)
let swap_turn st = 
  st.is_p1_turn <- not st.is_p1_turn

(** [mark_pos_hlpr st pos] marks the position [c] on the board in the game 
    represented by [st] for the player whose turn it is in the game. 
    Raises: [Invalid_pos] if [pos] is not a valid position on the board in the 
    game represented by [st]. *)
let mark_pos_hlpr st pos = 
  let board = st.board
  and p_mrk = if st.is_p1_turn then '1' else '0' in
  match pos with
  | 'A' when is_unmarked st (0, 0) -> board.(0).(0) <- p_mrk
  | 'B' when is_unmarked st (0, 1) -> board.(0).(1) <- p_mrk
  | 'C' when is_unmarked st (0, 2) -> board.(0).(2) <- p_mrk
  | 'D' when is_unmarked st (1, 0) -> board.(1).(0) <- p_mrk
  | 'E' when is_unmarked st (1, 1) -> board.(1).(1) <- p_mrk
  | 'F' when is_unmarked st (1, 2) -> board.(1).(2) <- p_mrk
  | 'G' when is_unmarked st (2, 0) -> board.(2).(0) <- p_mrk
  | 'H' when is_unmarked st (2, 1) -> board.(2).(1) <- p_mrk
  | 'I' when is_unmarked st (2, 2) -> board.(2).(2) <- p_mrk
  | _ -> raise Invalid_pos

(** [mark_pos st pos] is [st] after marking the position [c] on the board in the 
    game represented by [st] for the player whose turn it is in the game and 
    swapping the turn to the opposite player's turn. 
    Raises: [Invalid_pos] if [pos] is not a valid position on the board in the 
    game represented by [st]. *)
let mark_pos st pos =
  mark_pos_hlpr st pos;
  swap_turn st;
  st

(* [mark st pos] marks the space at position [pos] on the board in [st] for the 
    player whose turn it is according to [st].
    Raises: [Invalid_pos] if [pos] is already marked or is not a valid position
    on the board.
    Raises: [Game_over] if marking position [pos] causes the win condition or tie 
    condition to be met. *)
let mark st pos =
  let winner = get_winner(mark_pos st pos) in
  if fst winner then
    begin
      incr_pts st (snd winner);
      swap_turn st;
      print_endline "sent";
      raise (Game_over (snd winner))
    end
  else if is_tie (Array.to_list st.board) then raise (Game_over "?")

(* [continue st] resets the board in [st] to represent the start of a new 
    game. *)
let continue st =
  st.board <- [| [|'A'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|] |]