(** A set of types and functions that represent and manipulate the state of a 
    game of tic-tac-toe. *)

(** An abstract type representing the state of a game of tic-tac-toe. *)
type t

(** A type that represents the type of tic-tac-toe game the user is playing. *)
type mode =
  | Easy
  | Medium
  | Hard
  | Classic
  | Ultimate

(** Raised when mark is passed an invalid position argument. *)
exception Invalid_pos

(** Raised when mark is passed an argument that results in the win condition 
    or tie condition being met. *)
exception Game_over of string

(** [init_state p1 p2] is the initial state of a game with players [p1] and
    [p2]. *)
val init_state : string -> string -> mode -> t

(** [get_p1 st] is a string representing the name of player one in the game 
    represented by [st]. *)
val get_p1 : t -> string

(** [get_p2 st] is a string with the name of player two in the game represented 
    by [st]. *)
val get_p2 : t -> string

(** [get_p1_score st] is an int with the score of player one in the game 
    represented by [st]. *)
val get_p1_score : t -> int

(** [get_p2_score st] is an int representing the score of player two in the game 
    represented by [st]. *)
val get_p2_score : t -> int

(** [get_diff_lvl st] is the mode of the current game state [st]. *)
val get_diff_lvl : t -> mode

(** [get_board st] is the board in the game represented by [st]. *)
val get_board : t -> (char array) array

(** [get_is_p1_turn st] is an indicator of whether it is p1's turn or not. *)
val get_is_p1_turn : t -> bool

(** [print_pts st] prints the number of points each player has in the game
    represented by [st]. *)
val print_pts : t -> unit

(** [print_board st] prints a representation of the board in the game 
    represented by [st]. *)
val print_board : t -> unit

(** [print_turn st] prints a representation of the board and whose turn it is in 
    the game represented by [st]. *)
val print_turn : t -> unit

(** [reset_turn st] resets the turn in [st] to the first player. *)
val reset_turn : t -> unit

(** [mark st c] marks the space at position [c] on the board in [st] for the 
    player whose turn it is according to [st].
    Raises: [Invalid_pos] if [c] is already marked or is not a valid position
    on the board.
    Raises: [Game_over] if marking position [c] causes the win condition or tie 
    condition to be met. *)
val mark : t -> char -> unit

(** [continue st] resets the board in [st] to represent the start of a new 
    game. *)
val continue : t -> unit