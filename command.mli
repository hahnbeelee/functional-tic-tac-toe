(** A set of types and functions that handle user input. *)

(** The type [action] represents a player action. *)
type action = 
  | Start
  | Instructions
  | Leaderboard
  | Quit
  | Mark of char
  | Continue
  | UMark of int * char
  | UInstructions

(** Raised when parse is passed an empty argument. *)
exception Empty

(** Raised when parse is passed a non-empty invalid argument. *)
exception Malformed

(** [parse str] parses [str] (a player's input) into an [action].
    Requires: [str] contains only letters, numbers, and/or whitespace.
    Raises: [Empty] if [str] is the empty string or contains only whitespace.
    Raises: [Malformed] if the command is malformed/ not recognized. *)
val parse : string -> action