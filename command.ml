(* A set of types and functions that handle user input. *)

type action =
  | Start
  | Instructions
  | Leaderboard
  | Quit
  | Mark of char
  | Continue
  | UMark of int * char
  | UInstructions

exception Empty

exception Malformed

(** [parse_umark brd_num pos] is a UMark action containing the board number 
    [brd_num] and position [pos] to be marked.
    Raises: [Malformed] if [brd_num] is not between 0 and 10 or if [pos] is 
    longer than one character. *)
let parse_umark brd_num pos =
  let board_num = int_of_string brd_num in
  if board_num < 1 || board_num > 9 then 
    raise Malformed
  else 
    match pos with
    | s when String.length s > 1 -> raise Malformed
    | _ -> UMark (board_num, (String.get pos 0))

(* [parse str] parses [str] (a player's input) into an [action].
    Requires: [str] contains only letters, numbers, and/or whitespace.
    Raises: [Empty] if [str] is the empty string or contains only whitespace.
    Raises: [Malformed] if the command is not recognized. *)
let parse str =
  match String.split_on_char ' ' str |> List.filter (fun x -> x <> "") with
  | [] -> raise Empty
  | h :: _ when h = "S" || h = "s" -> Start
  | h :: _ when h = "I" || h = "i" -> Instructions
  | h :: _ when h = "L" || h = "l" -> Leaderboard
  | h :: _ when h = "Q" || h = "q" -> Quit
  | h :: _ when h = "C" || h = "c" -> Continue
  | h :: pos :: [] when (h = "M" || h = "m") && String.length pos = 1-> 
    Mark (String.get pos 0)
  | h :: brd_num :: pos :: [] when h = "M" || h = "m" -> parse_umark brd_num pos
  | h :: str :: [] when (h = "U" || h = "u")  && (str = "I" || str = "i") -> 
    UInstructions
  | _ -> raise Malformed