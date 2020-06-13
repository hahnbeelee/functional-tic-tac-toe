(** A set of functions that read and write from the game's json files. *)
open State

(** [print_lb md] prints the data from the json file that corresponds to the 
    indicated mode [md] *)
val print_lb : State.mode -> unit

(** [update_lb st md] updates the data in the json file that corresponds to the 
    indicated mode [md] with the data from [st] then prints the data from the 
    desired json. *)
val update_lb : State.t -> State.mode -> State.mode -> unit

(** [update_ulti_lb ust md] updates the data in lb_ultimate.json with the data 
    from [ust] then prints the data from lb_ultimate.json. *)
val update_ulti_lb : Ultimatestate.t -> State.mode -> unit