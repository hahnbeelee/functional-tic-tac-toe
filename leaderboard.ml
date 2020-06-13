(* A set of functions that read and write from the game's json files. *)

open Yojson.Basic.Util
open State

(** The type [user] represents a user profile. *)
type user = {
  name : string;
  score : int
}

(** The type [leaderboard] represents a leaderboard with the top ten highest
    scoring users. *)
type leaderboard = user list

let mode_to_file = function
  | Classic -> "leaderboard.json"
  | Easy -> "lb_ai_easy.json"
  | Medium -> "lb_ai_med.json"
  | Hard -> "lb_ai_hard.json"
  | Ultimate -> "lb_ultimate.json"

(** [user_of_json usr] is the user obtained from parsing the json [usr].
    Requires: [usr] is a valid user json based on the formatting of 
    schema.json. *)
let user_of_json usr = {
  name = usr |> member "user" |> to_string; 
  score = usr |> member "score" |> to_int
}

(** [from_json j] is the [leaderboard] obtained from parsing the json [j].
    Requires: [j] is a valid json based on the formatting of schema.json. *)
let from_json json =
  json |> member "leaderboard" |> to_list |> List.map user_of_json

(** [print_lb_hlpr pl lst] prints profiles of the users in [lst] along with 
    their places according to [pl].
    Requires: [lst] is a sorted list of users. *)
let rec print_lb_hlpr place lst =
  match lst with
  | h :: t ->
    print_endline ("\n#" ^ string_of_int place ^ ": " ^ h.name ^ " - " ^ 
                   string_of_int h.score);
    print_string ("----------------------"); print_lb_hlpr (place + 1) t
  | [] -> ()

(* [print_lb] prints the data from leaderboard.json.
    Requires: leaderboard.json is formatted based on the formatting of
    schema.json. *)
let print_lb md =
  let board = Yojson.Basic.from_file (mode_to_file md) |> from_json in
  print_lb_hlpr 1 board;
  print_endline ""

(** [cull lst acc] is a list of the top ten users in [lst].
    Requires: [lst] is a sorted list of users. *)
let rec cull lst acc =
  match lst with
  | [] -> failwith "FATAL ERROR IN LEADERBOARD.CULL"
  | h :: final :: [] -> h :: acc
  | h :: t -> cull t (h :: acc)

(** [contains_user n lb] is true iff the user with name [u] exists in 
    leaderboard [lb] and is false otherwise. *)
let rec contains_user name lb =
  match lb with
  | [] -> false
  | usr :: t -> if usr.name = name then true else contains_user name t

(** [get_usr_by_name n lb] is the profile of the user with the name [n] in [lb]. 
    Requires: a user with the name [n] exists in [lb]. *)
let rec get_usr_by_name name lb =
  match lb with
  | [] -> failwith "FATAL ERROR IN LEADERBOARD.GET_USR_BY_NAME"
  | usr :: t -> if usr.name = name then usr else get_usr_by_name name t

(** [remove_user n lb acc] is [lb] with the user with name [n] removed. 
    Requires: a user with the name [n] exists in [lb]. *)
let rec remove_user name lb acc =
  match lb with
  | [] -> acc
  | usr :: t -> if usr.name = name then remove_user name t acc
    else remove_user name t (usr :: acc)

(** [compare_usrs usr1 usr2] is -1 if the score of [usr1] is less than the score 
    of [usr2], 0 if the score of [usr1] is equal to the score of [usr2], and 1 
    if the score of [usr1] is greater than the score of [usr2]. *)
let compare_usrs usr1 usr2 = 
  if usr1.score < usr2.score then -1 else 
  if usr1.score = usr2.score then 0 else 1 

(** [add_score_hlpr sc n lb] is the list of the top ten users in [lb] possibly 
    including the user with name [u] and score [sc]. *)
let add_score_hlpr sc name lb = 
  let new_user = {name = name; score = sc} in
  if (contains_user name lb = false) then 
    let new_board = new_user :: lb in
    List.rev (List.sort compare_usrs new_board)
  else
    let old_user = get_usr_by_name name lb in
    let new_score = old_user.score + sc in
    let new_lb = remove_user name lb [] in
    let new_user = {name = name; score = new_score} in
    let new_board = new_user :: new_lb in 
    List.rev (List.sort compare_usrs new_board)

(** [add_score sc n lb] is the list of the top ten users in [lb] possibly 
    including the user with name [u] and score [sc]. *)
let add_score sc name lb =
  let new_board = add_score_hlpr sc name lb in
  if List.length new_board > 10 then 
    cull new_board []
  else 
    new_board

(** [user_to_json usr] is the JSON representation of [usr]. *)
let user_to_json usr =
  `Assoc [("user", `String (usr.name)); ("score", `Int usr.score)]

(** [leaderboard_to_json lb] is the JSON representation of [lb]. *)
let leaderboard_to_json lb =
  let json_list = List.map (user_to_json) lb in 
  `Assoc [("leaderboard", `List (json_list))]

(** [to_file lb md] writes the data from [lb] to the json file corresponding
    with the correct mode [md]. *)
let to_file md lb = 
  let file = mode_to_file md |> Stdlib.open_out in
  lb |> leaderboard_to_json |> Yojson.Basic.pretty_to_channel file;
  Stdlib.close_out file

(* TODO - update *)
(* [update_lb st] updates the data in leaderboard.json with the data from [st] 
    then prints the data from leaderboard.json. *)
let update_lb st md =
  let lb =
    from_json (Yojson.Basic.from_file (mode_to_file md))
    |> add_score (State.get_p1_score st) (State.get_p1 st)
  in
  if State.get_p2 st = "" then
    begin
      to_file md lb;
      print_lb
    end 
  else 
    begin
      add_score (State.get_p2_score st) (State.get_p2 st) lb |> to_file md; 
      print_lb
    end

(* [update_ulti_lb ust] updates the data in lb_ultimate.json with the data from 
    [ust] then prints the data from lb_ultimate.json. *)
let update_ulti_lb ust =
  let lb = 
    from_json (Yojson.Basic.from_file "lb_ultimate.json")
    |> add_score (Ultimatestate.get_p1_score ust) (Ultimatestate.get_p1 ust) in
  if Ultimatestate.get_p2 ust = "" then
    begin
      to_file Ultimate lb; print_lb
    end
  else
    begin
      add_score (Ultimatestate.get_p2_score ust) (Ultimatestate.get_p2 ust) lb
      |> to_file Ultimate; print_lb
    end
