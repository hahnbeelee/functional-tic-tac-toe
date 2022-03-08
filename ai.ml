(* A set of functions that analyze and mark positions on the board in an 
    attempt to prevent player one from achieving the win condition. *)
(** yuh small change *)
open State

(** [rand_mark st] marks a random position on the board in the game represented 
    by [st]. *)
let rand_mark st = 
  State.mark st (char_of_int (65 + (Random.int 9)))

(** [crnr_block st otr] marks a corner of the board in [st] if player one can 
    achieve the win condition next turn using that corner. If the above is 
    untrue and [otr] is true, then the edge_block function is called. Otherwise, 
    a random position is marked on the board.
    Requires: the win condition described in state.ml has not already been met.
    Raises: [Game_over] if marking a position causes the win condition or tie 
    condition to be met. *)
let rec crnr_block st otr : unit =
  match State.get_board st with
  | [| [|_; _; 'C'|]; [|_; '1'; _|]; [|'1'; _; _|] |]
  | [| [|_; _; 'C'|]; [|_; _; '1'|]; [|_; _; '1'|] |]
  | [| [|'1'; '1'; 'C'|]; _; _;|] -> State.mark st 'C'
  | [| [|'1'; _; _|]; [|'1'; _; _|]; [|'G'; _; _|] |]
  | [| [|_; _; '1'|]; [|_; '1'; _|]; [|'G'; _; _|] |]
  | [| _; _; [|'G'; '1'; '1'|] |] -> State.mark st 'G'
  | [| [|'A'; _; _|]; [|'1'; _; _|]; [|'1'; _; _|] |]
  | [| [|'A'; _; _|]; [|_; '1'; _|]; [|_; _; '1'|] |]
  | [| [|'A'; '1'; '1'|]; _; _ |] -> State.mark st 'A'
  | [| [|'1'; _; _|]; [|_; '1'; _|]; [|_; _; 'I'|] |]
  | [| [|_; _; '1'|]; [|_; _; '1'|]; [|_; _; 'I'|] |]
  | [| _; _; [|'1'; '1'; 'I'|] |] -> State.mark st 'I'
  | _ when otr -> edge_block st false
  | _ -> rand_mark st

(** [edge_block st otr] marks an edge (not including corners) of the board in 
    [st] if player one can achieve the win condition next turn using that edge. 
    If the above is untrue and [otr] is true, then the crnr_block function is 
    called. Otherwise, a random position is marked on the board.
    Requires: the win condition described in state.ml has not already been met.
    Raises: [Game_over] if marking a position causes the win condition or tie 
    condition to be met. *)
and edge_block st otr : unit =
  match State.get_board st with
  | [| [|_; 'B'; _|]; [|_; '1'; _|]; [|_; '1'; _|] |]
  | [| [|'1'; 'B'; '1'|]; _; _ |] -> State.mark st 'B'
  | [| [|'1'; _; _|]; [|'D'; _; _|]; [|'1'; _; _|] |]
  | [| _; [|'D'; '1'; '1'|]; _ |] -> State.mark st 'D'
  | [| [|_; _; '1'|]; [|_; _; 'F'|]; [|_; _; '1'|] |]
  | [| _; [|'1'; '1'; 'F'|]; _ |] -> State.mark st 'F'
  | [| [|_; '1'; _|]; [|_;'1'; _|]; [|_; 'H'; _|] |]
  | [| _; _; [|'1'; 'H'; '1'|] |] -> State.mark st 'H'
  | _ when otr -> crnr_block st false
  | _ -> rand_mark st

(* [mark st dif] marks positions on the board in [st] based on difficulty level 
    [dif] in an attempt to prevent player one from achieving the win condition 
    described in state.ml.
    Requires: [dif] is an integer between -1 and 3.
    Requires: the win condition described in state.ml has not already been met. 
    Raises: [Game_over] if marking a position causes the win condition or tie 
    condition to be met. *)
let rec mark st d =
  Random.self_init ();
  try
    (* [rnd] is random bool meant to randomize the priority of marking edges and 
       corners. *)
    let rnd = Random.bool () in
    match d with
    | 0 -> rand_mark st
    | 1 when rnd -> crnr_block st false
    | 1 -> edge_block st false
    | 2 when rnd -> edge_block st true
    | 2 -> crnr_block st true
    | _ -> failwith "FATAL ERROR IN AI.MARK"
  with
  | Invalid_pos -> mark st d

(* [continue st] resets the board and mutates [st] to represent the start of a 
    new game. *)
let continue st =
  State.continue st;
  State.reset_turn st