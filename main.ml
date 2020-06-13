open State
open Command
open Ai
open Ultimatestate
open Leaderboard

let err_malf = "Sorry, that input wasn't recognized."

let err_in_prog = "A game is already in progress."

let err_u_mark = "This command only works in ultimate tic-tac-toe."

let err_u_instr = 
  "You're not playing ultimate tic-tac-toe, so the ultimate tic-tac-toe 
instructions are not useful here."

let err_empt = "Please enter a command."

let err_unavail = "That position either isn't valid or isn't available."

let err_start = "Please use the start command first."

let err_mode = "Please choose a game mode first."

(** [print_instructions] prints the game engine instructions. *)
let print_instructions () =
  print_endline 
    "\nInstructions:
      The objective of the game is to mark three spots in a row, column, or
      diagonally across the board. All commands are entered through typing 
      into the command line and pressing enter.

      Enter 'S' to start a new game.
      Enter 'M x' (where x is a letter A-I cooresponding to the position on the 
      board you'd like to mark) to mark a position on the board during a
      game.
      Enter 'L' to see the leaderboard.
      Enter 'Q' to quit at any time.
      Enter 'I' to view these instructions again.
      You may also enter 'U I' to view the instructions for ultimate mode.

      Have a great game!\n"

(** [print_ulti_instructions] prints the instructions for ultimate mode. *)
let print_ulti_instructions () =
  print_endline
    "\nInstructions for ultimate mode:\n
  In ultimate mode, you are playing on a big tic-tac-toe board made up of
  smaller tic-tac-toe boards. The objective of the game is to win three small
  boards that line up in a row, column, or diagonally across the the big
  board.

  In order to play in this mode, you must specify in each mark command the 
  board you want to mark in and what position you want to mark in that board.
  The small boards are numbered 1-9 horizontally across the big board from the 
  top down.

  The first turn must be played in the center board (board number 5). 
  Following turns must be played in the small board corresponding to the 
  relative position (in the big board) of the last mark. For example, if the
  first player marks position A in board number 5, the enemy player must now 
  mark some position in board number 1. If you need further help understanding 
  the game logic, there is a Wikipedia article about ultimate tic-tac-toe that 
  includes some helpful diagrams.

  If the board the player must play in has been won or has all of its positions 
  marked, the player can no longer play on that board. Instead, the player can 
  mark in any board that has not been won or has an open position. 

  The game ends when all possible positions have been marked, the players tie 
  on the final board, or a player achieves the objective descibred above.

  Marking positions on the board is done by entering 'M n x' where n is a 
  number 1-9 representing a small board in the big board and x is a letter A-I 
  cooresponding to the position on the small board that you'd like to mark.

  You can enter 'I' to view the game's other instructions or 'U I' to view these
  instructions again.

  Have a great game!\n"

let print_winner str = 
  print_endline ("GAME OVER: " ^ str ^ " wins!")

let print_tie st = 
  State.print_board st;
  print_endline "GAME OVER: It's a tie!"; 
  State.print_pts st

let print_u_tie st = 
  Ultimatestate.print_board st;
  print_endline "GAME OVER: It's a tie!"; 
  Ultimatestate.print_pts st

let clear () =
  Sys.command "clear" |> ignore

(** [check_p1_name n] is [n] with whitespace removed from either end if [n] 
    contains only letters, numbers, and/or spaces, otherwise [check_p1_name n]
    prompts the user for a new [n] until a valid [n] is provided. *)
let rec check_p1_name name =
  if String.trim name = "" then
    begin
      print_endline("This name is invalid. Please enter a new name.");
      print_string  "> ";
      check_p1_name (read_line ())
    end
  else name

(** [check_p2_name n p1_n] is [n] with whitespace removed from either end if 
    is not the same as [p1_n] and [n] contains only letters, numbers, and/or
    spaces, otherwise [check_p2_name n p1_n] prompts the user for a new [n] 
    until a valid [n] is provided. *)
let rec check_p2_name name p1_name =
  let name = check_p1_name name in
  if name = p1_name then
    begin
      print_endline ("Player two's name cannot be the same as player one's.\n\
                      Please enter a unique name for player two.");
      print_string  "> ";
      check_p2_name (read_line ()) p1_name 
    end 
  else name

(** [get_diff] is a REPL that prompts the user for a difficulty level and 
    is an int corresponding to a difficulty level. *)
let rec get_diff () =
  print_endline ("Enter 1 for easy difficulty, 2 for medium difficulty, or 3 \
                  for hard difficulty. You can also enter Q to quit. ");
  print_string  "> ";
  match read_line () with
  | "1" -> 0
  | "2" -> 1
  | "3" -> 2
  | "Q" | "q" -> exit 0
  | _ -> print_endline err_malf; get_diff ()

let get_dif_variant = function
  | 0 -> Easy 
  | 1 -> Medium
  | 2 -> Hard
  | _ -> failwith "FATAL ERROR IN MAIN.GET_DIF_VARIANT"

let rec play_clgame_hlpr st = 
  State.print_turn st;
  match Command.parse (read_line ()) with
  | Start -> print_endline err_in_prog; 
    play_clgame st
  | Instructions -> print_instructions ();
    play_clgame st
  | Leaderboard -> Leaderboard.print_lb (get_diff_lvl st);
    play_clgame st
  | Quit -> Leaderboard.update_lb st (get_diff_lvl st) (get_diff_lvl st); 
    exit 0
  | Mark x -> clear (); 
    State.mark st x; 
    play_clgame st
  | Continue -> print_endline err_in_prog; 
    play_clgame st
  | UMark (_, _) -> print_endline err_u_mark;
    play_clgame st
  | UInstructions -> print_endline err_u_instr; 
    play_clgame st

(** [play_clgame st] is the main REPL for classic mode. *)
and play_clgame st =
  try
    play_clgame_hlpr st
  with
  | State.Game_over x when x = "?" -> print_tie st; 
    play_clagain st
  | State.Game_over x -> State.print_board st;
    print_winner x; 
    State.print_pts st; 
    play_clagain st
  | Empty -> clear (); 
    State.print_board st;
    print_endline err_empt;
    play_clgame st
  | Malformed -> clear (); 
    print_endline err_malf; 
    play_clgame st
  | State.Invalid_pos -> clear ();
    State.print_board st;
    print_endline err_unavail; 
    play_clgame st

and play_clagain_hlpr st =
  match Command.parse (read_line ()) with
  | Continue -> State.continue st; 
    play_clgame st
  | Start -> Leaderboard.update_lb st (get_diff_lvl st) (get_diff_lvl st); 
    start ()
  | Instructions -> print_instructions ();
    play_clagain st
  | Leaderboard -> Leaderboard.print_lb (get_diff_lvl st); 
    play_clagain st
  | Quit -> Leaderboard.update_lb st (get_diff_lvl st) (get_diff_lvl st); 
    exit 0
  | Mark _ | UMark (_,_) | UInstructions ->
    print_endline "Please use the start or continue commands first.";
    play_clagain st

(** [play_clagain st] is a REPL that allows a user to play again in classical or 
    start a new game. *)
and play_clagain st =
  print_endline "Enter C to play again, S to start a new game, I for 
  instructions, L for the leaderboard, or Q to quit. \n"; print_string "> ";
  try
    play_clagain_hlpr st
  with
  | Empty -> print_endline err_empt; 
    play_clagain st
  | Malformed -> print_endline err_malf; 
    play_clagain st

(** [start_clgame] prompts the users for their names, intializes a new game
    state, then starts a classic mode game. *)
and start_clgame () =
  print_endline("Enter a name for player one.");
  print_string  "> ";
  let p1_name = check_p1_name (read_line ()) in
  print_endline("Enter a name for player two.");
  print_string  "> ";
  let p2_name = check_p2_name (read_line ()) p1_name in
  play_clgame (State.init_state p1_name p2_name Classic) ()

and play_aigame_hlpr st d =
  State.print_turn st;
  match Command.parse (read_line ()) with
  | Start -> print_endline err_in_prog; 
    play_aigame st d
  | Instructions -> print_instructions (); 
    play_aigame st d
  | Leaderboard -> Leaderboard.print_lb (get_diff_lvl st); 
    play_aigame st d
  | Quit -> Leaderboard.update_lb st (get_diff_lvl st) (get_diff_lvl st); 
    exit 0
  | Mark x -> State.mark st x; 
    Ai.mark st d;
    play_aigame st d
  | Continue -> print_endline err_in_prog; 
    play_aigame st d
  | UMark (_, _) -> print_endline err_u_mark; 
    play_aigame st d
  | UInstructions -> print_endline err_u_instr; 
    play_aigame st d

(** [play_aigame st d] is the main REPL for AI mode. *)
and play_aigame st d =
  try
    play_aigame_hlpr st d
  with
  | State.Game_over x when x = "?" -> print_tie st; 
    play_aiagain st
  | State.Game_over x when x = "" -> State.print_board st;
    print_endline "GAME OVER: The AI wins!"; 
    State.print_pts st;
    play_aiagain st
  | State.Game_over x -> State.print_board st; 
    print_winner x; 
    State.print_pts st;
    play_aiagain st
  | Empty -> print_endline err_empt;
    play_aigame st d
  | Malformed -> print_endline err_malf;
    play_aigame st d
  | State.Invalid_pos -> print_endline err_unavail; 
    play_aigame st d

and play_aiagain_hlpr st d =
  match Command.parse (read_line ()) with
  | Continue -> Ai.continue st;
    play_aigame st d d
  | Start -> Leaderboard.update_lb st (get_diff_lvl st) (get_diff_lvl st); 
    start ()
  | Instructions -> print_instructions (); 
    play_aiagain st d
  | Leaderboard -> Leaderboard.print_lb (get_diff_lvl st); 
    play_aiagain st d
  | Quit -> Leaderboard.update_lb st (get_diff_lvl st) (get_diff_lvl st); 
    exit 0
  | Mark _ | UMark (_,_) | UInstructions -> 
    print_endline "Please use the start or continue commands first.";
    play_aiagain st d

(** [play_aiagain st] is a REPL that allows a user to play again or start a new
    game. *)
and play_aiagain st d =
  print_endline
    "Enter C to play again, S to start a new game, I for instructions, L for 
    the leaderboard, or Q to quit. \n";
  print_string "> ";
  try
    play_aiagain_hlpr st d
  with
  | Empty -> print_endline err_empt; 
    play_aiagain st d
  | Malformed -> print_endline err_malf; 
    play_aiagain st d

(** [start_aigame d] prompts the user for their name, intializes a new game
    state, then starts an AI mode game. *)
and start_aigame dif =
  print_endline("Enter your name. "); 
  print_string  "> ";
  let p1_name = check_p1_name (read_line ()) in
  play_aigame (State.init_state p1_name "" (get_dif_variant dif)) dif

and play_ugame_hlpr st =
  Ultimatestate.print_turn st;
  match Command.parse (read_line ()) with
  | Start -> print_endline err_in_prog;
    play_ugame st
  | Instructions -> print_instructions ();
    play_ugame st
  | Leaderboard -> Leaderboard.print_lb Ultimate;
    play_ugame st
  | Quit -> Leaderboard.update_ulti_lb st Ultimate;
    exit 0
  | Mark x -> print_endline err_u_mark;
    play_ugame st
  | Continue -> print_endline err_in_prog;
    play_ugame st
  | UMark (board_num, ch) -> Ultimatestate.mark st board_num ch;
    play_ugame st
  | UInstructions -> print_ulti_instructions ();
    play_ugame st

(** [play_aigame st d] is the main REPL for ultimate mode. *)
and play_ugame st =
  try
    play_ugame_hlpr st
  with
  | Ultimatestate.Game_over x when x = "?" -> print_u_tie st; 
    play_uagain st
  | Ultimatestate.Game_over x -> 
    Ultimatestate.print_board st;
    print_endline "";
    Ultimatestate.print_final_board st;
    print_endline "";
    print_winner x; 
    Ultimatestate.print_pts st;
    play_uagain st
  | Empty -> print_endline err_empt; 
    play_ugame st
  | Malformed -> print_endline err_malf; 
    play_ugame st
  | Ultimatestate.Invalid_pos -> print_endline err_unavail; 
    play_ugame st
  | Wrong_board -> print_endline "You are playing in the wrong board. Please \
                                  try again."; play_ugame st
  | Invalid_board_num -> print_endline "You are trying to play in a board that \
                                        doesn't exist. Please try again."; 
    play_ugame st

and play_uagain_hlpr st =
  match Command.parse (read_line ()) with
  | Continue -> Ultimatestate.continue st; 
    play_ugame st
  | Start -> Leaderboard.update_ulti_lb st Ultimate; 
    start ()
  | Instructions -> print_instructions ();
    play_uagain st
  | Leaderboard -> Leaderboard.print_lb Ultimate; 
    play_uagain st
  | Quit -> Leaderboard.update_ulti_lb st Ultimate; 
    exit 0
  | Mark _ | UMark (_,_) | UInstructions -> 
    print_endline "Please use the start or continue commands first."; 
    play_uagain st

and play_uagain st =
  print_endline "Enter C to play again, S to start a new game, I for \
                 instructions, L for the leaderboard, or Q to quit. \n"; 
  print_string "> ";
  try
    play_uagain_hlpr st
  with
  | Empty -> print_endline err_empt; 
    play_uagain st
  | Malformed -> print_endline err_malf; 
    play_uagain st

and start_ugame () =
  print_endline "Please enter a name for player one.";
  print_string  "> ";
  let p1_name = check_p1_name (read_line ()) in
  print_endline "Please enter a name for player two.";
  print_string  "> ";
  let p2_name = check_p2_name (read_line ()) p1_name in
  print_endline "This is a complicated game mode. Would you like the \
                 instructions for this particular game mode? [Y/n]";
  print_string  "> ";
  let yes_or_no = read_line () in
  match yes_or_no with
  | "y" | "Y" -> print_ulti_instructions ();
    play_ugame (Ultimatestate.init_state p1_name p2_name)
  | "n" | "N" -> print_endline "Have a great game!";
    play_ugame (Ultimatestate.init_state p1_name p2_name)
  | _ -> print_endline (err_malf ^ " Have a good game!"); 
    play_ugame (Ultimatestate.init_state p1_name p2_name)

and start_hlpr str =
  try
    match Command.parse str with
    | Start -> print_endline("Please choose a game mode. "); 
      start ()
    | Instructions | UInstructions -> print_instructions (); 
      print_ulti_instructions ();
      start ()
    | Leaderboard -> 
      print_endline "CLASSIC MODE LEADERBOARD\n"; 
      Leaderboard.print_lb Classic;
      ANSITerminal.(print_string [white] "\n\nEASY AI MODE LEADERBOARD\n");
      Leaderboard.print_lb Easy;
      ANSITerminal.(print_string [yellow] "\n\nMEDIUM AI MODE LEADERBOARD\n");
      Leaderboard.print_lb Medium;
      ANSITerminal.(print_string [red] "\n\nHARD AI MODE LEADERBOARD\n");
      Leaderboard.print_lb Hard;
      ANSITerminal.(print_string [green] "\n\nULTIMATE MODE LEADERBOARD\n");
      Leaderboard.print_lb Ultimate;
      start ()
    | Quit -> exit 0
    | Mark _ | UMark (_, _) -> print_endline err_mode; 
      start ()
    | Continue -> print_endline err_mode; 
      start ()
  with
  | Empty -> print_endline err_mode; 
    start ()
  | Malformed -> print_endline err_malf; 
    start ()

(** [start] is a REPL that asks the user for about which game mode they'd like
    to play, then starts the game. *)
and start () =
  print_endline "\nEnter C for classic mode, U for ultimate mode, or A for \
                 AI mode. ";
  print_endline "You can also enter I for instructions, L for the leaderboard, \
                 or Q to quit.";
  print_string  "> ";
  match read_line () with
  | "C" | "c" -> start_clgame
  | "U" | "u" -> start_ugame () 
  | "A" | "a" -> let dif = (get_diff ()) in start_aigame dif dif
  | _ as str -> start_hlpr str

(** [main] is the starting REPL of the game that allows users to start a new
    game. *)
let rec main () =
  ANSITerminal.(print_string [blue] 
                  "\n\nWelcome to the Tic-Tac-Toe Game Engine!\n");
  print_endline "Enter S to start, I for instructions, L for the leaderboard, \
                 or Q to quit. \n";
  print_string  "> ";
  try
    match Command.parse (read_line ()) with
    | Start -> start ()
    | Instructions | UInstructions -> print_instructions ();
      print_ulti_instructions (); 
      main ()
    | Leaderboard ->
      ANSITerminal.(print_string [cyan] "CLASSIC MODE LEADERBOARD"); 
      Leaderboard.print_lb Classic;
      ANSITerminal.(print_string [white] "\n\nEASY AI MODE LEADERBOARD\n");
      Leaderboard.print_lb Easy;
      ANSITerminal.(print_string [yellow] "\n\nMEDIUM AI MODE LEADERBOARD\n");
      Leaderboard.print_lb Medium;
      ANSITerminal.(print_string [red] "\n\nHARD AI MODE LEADERBOARD\n");
      Leaderboard.print_lb Hard;
      ANSITerminal.(print_string [green] "\n\nULTIMATE MODE LEADERBOARD\n");
      Leaderboard.print_lb Ultimate;
      main ()
    | Quit -> Leaderboard.print_lb Ultimate; 
      exit 0
    | Mark _ | UMark (_, _) -> print_endline err_start; 
      main ()
    | Continue -> print_endline err_start; 
      main ()
  with
  | Empty -> print_endline err_empt; 
    main ()
  | Malformed -> print_endline err_malf; 
    main ()

let () = main () ()