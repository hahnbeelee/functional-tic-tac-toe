(** A set of functions that analyze and mark positions on the board in an 
    attempt to prevent player one from achieving the win condition. *)

(** [mark st dif] marks positions on the board in [st] based on difficulty level 
    [dif] in an attempt to prevent player one from achieving the win condition 
    described in state.ml.
    Requires: [dif] is an integer between -1 and 3.
    Requires: the win condition described in state.ml has not already been met. 
    Raises: [Game_over] if marking a position causes the win condition or tie 
    condition to be met. *)
val mark : State.t -> int -> unit

(** [continue st] resets the board and mutates [st] to represent the start of a 
    new game. *)
val continue : State.t -> unit