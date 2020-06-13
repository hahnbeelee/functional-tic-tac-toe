(** A set of types and functions that represent and manipulate the state of a 
    game of ultimate tic-tac-toe. *)

(** An abstract type representing the state of a game of ultimate tic-tac-toe. 
*)
type t

(** A type representing a tic-tac-toe board. *)
type small_board = (char array) array

(** Raised when any mark function is passed an invalid position argument. *)
exception Invalid_pos

(** Raised when a player attempts to mark outside of the current board. *)
exception Wrong_board

(** Raised when a player attempts to mark in a board that does not exist. *)
exception Invalid_board_num

(** Raised when any mark function is passed an argument that results in the win 
    condition or tie condition being met. *)
exception Game_over of string

(** [get_p1 ust] is a string representing the name of player one in the game 
    represented by [ust]. *)
val get_p1 : t -> string

(** [get_p2 ust] is a string with the name of player two in the game represented 
    by [ust]. *)
val get_p2 : t -> string

(** [get_p1_score ust] is an int with the score of player one in the game 
    represented by [ust]. *)
val get_p1_score : t -> int

(** [get_p2_score ust] is an int representing the score of player two in the 
    game represented by [ust]. *)
val get_p2_score : t -> int

(** [get_board ust] is the an array of arrays of tic-tac-toe boards that
    represents an ultimate tic-tac-toe board. *)
val get_board : t -> (small_board array) array

(** [get_final_board ust] is a tic-tac board that represents who has won in the 
    ultimate tic-tac-toe board. *)
val get_final_board : t -> char array array

(** [print_pts ust] prints the number of points each player has in the game
    represented by [ust]. *)
val print_pts : t -> unit

(** [init_state p1 p2] is the initial state of a game with players [p1] and
    [p2]. *)
val init_state : string -> string -> t

(** [mark ust bn pos] marks the space at position [pos] on the board with 
    board number [bn] for the player whose turn it is according to [ust].
    Raises: [Wrong_board] if [pos] is not in the correct board.
    Raises: [Invalid_board] if [bn] is not between 0 and 10.
    Raises: [Invalid_pos] if [pos] is already marked or is not a valid position
    on the board.
    Raises: [Game_over] if marking position [pos] causes the win condition or
    tie condition to be met. *)
val mark : t -> int -> char -> unit

(** [print_board ust] prints a representation of the board in the game 
    represented by [ust]. *)
val print_board : t -> unit

val print_final_board : t -> unit

(** [print_turn ust] prints a representation of the board and whose turn it is 
    in the game represented by [ust]. *)
val print_turn : t -> unit

(** [continue ust] resets the board and turn in [ust] to represent the start of 
    a new game. *)
val continue : t -> unit