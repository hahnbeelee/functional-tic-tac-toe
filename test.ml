(* Test Plan
   Ounit tests Command, Leaderboard, State, and Ultimatestate automatically.

   The test cases were created through glass box testing. We used glass box
   testing because we implemented the code and knew how the structures were 
   implemented.

   Leaderboard was tested manually because variants and file mutation prevent 
   us from testing it automatically. Some functions in leaderboard, state, and
   ultimatestate were not tested because their main functionalities were 
   printing.

   AI mode of the game was also tested manually because the AI's moves are 
   somewhat randomized so they could not be tested automatically with an 
   expected action/equality from the AI.

   We approached the testing by testing different inputs for commands that
   could be inputted by the user. The command_tests was a bit of a merge between
   black box and glass box testing because we inserted inputs that could be 
   expected from the user also. Due to this reason, this is also a correct
   approach that tests for inputs that could break the code and our tests
   were user-centered.

   As for state tests, we tried to win the game and see if the game state is
   the way we want it to be. This tests the game logic which is a correct 
   approach. We did the same for ultimate state testing.

*)


open OUnit2
open Command
open State
open Ultimatestate

let make_parse_test 
    (name : string) 
    (input: string) 
    (expected_output : action) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse input))

let command_tests = 
  [
    "parse - Empty" >:: (fun _ -> assert_raises (Empty) (fun () -> parse ""));
    "parse - incorrect input" >:: (fun _ -> assert_raises (Malformed) 
                                      (fun () -> parse "incorrect"));
    make_parse_test "parse - correct start" "S" (Start);
    make_parse_test "parse - correct instructions" "I" (Instructions);
    make_parse_test "parse - correct leaderboard" "L" (Leaderboard);
    make_parse_test "parse - correct quit" "Q" (Quit);
    make_parse_test "parse - correct continue" "C" (Continue);
    "parse - mark - missing 2nd Argument" >:: (fun _ -> assert_raises 
                                                  (Malformed) 
                                                  (fun () -> parse "M"));
    "mark 2nd Arg too long" >:: (fun _ -> assert_raises 
                                    (Malformed) 
                                    (fun () -> parse "M yo"));
    make_parse_test "parse - correct mark" "M A" (Mark 'A');
    make_parse_test "parse - mark w unnecessary spaces" "M    A" (Mark 'A');
    make_parse_test "mark w unnecessary spaces after" "M B    " (Mark 'B');
    make_parse_test "parse - Uinstructions" "U I" (UInstructions);
    make_parse_test "parse - UMark correct" "M 1 A" (UMark (1, 'A'));
    "UMark incorrect board number < 1"  >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse "M 0 A"));
    "UMark incorrect board number > 9"  >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse "M 10 A"));
    "UMark position too long"  >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse "M 10 Aaaa"));
  ]

let st = State.init_state "A" "B" Classic
let st1 = State.init_state "A" "B" Classic 
let st2 = State.init_state "A" "B" Classic
let st_tie = State.init_state "A" "B" Classic

let state_tests = 
  [
    "get_p1" >:: (fun _ -> assert_equal (State.get_p1 st) "A");
    "get_p2" >:: (fun _ -> assert_equal (State.get_p2 st) "B");
    "get_p1_score" >:: (fun _ -> assert_equal (State.get_p1_score st) 0);
    "get_p2_score" >:: (fun _ -> assert_equal (State.get_p2_score st) 0);
    "get_is_p1_turn" >:: (fun _ -> assert_equal (State.get_is_p1_turn st) true);
    "get_diff_lvl" >:: (fun _ -> assert_equal (State.get_diff_lvl st) Classic);
    "mark invalid pos" >:: (fun _ -> assert_raises (State.Invalid_pos) 
                               (fun () -> State.mark st 'j'));
    "mark correct pos" >:: 
    (fun _ -> assert_equal (State.mark st 'E'; State.get_board st) 
        ([|[|'A'; 'B'; 'C'|]; [|'D'; '1'; 'F'|]; [|'G'; 'H'; 'I'|]|]));
    "is_p1_turn switch" >:: 
    (fun _ -> assert_equal (State.mark st 'F'; State.get_is_p1_turn st) false);
    "reset_turn" >:: 
    (fun _ -> assert_equal (State.mark st 'G'; State.reset_turn st; 
                            State.get_is_p1_turn st) true);
    "continue" >:: 
    (fun _ -> assert_equal (State.mark st 'A'; State.continue st; 
                            State.get_board st) 
        ([|[|'A'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|]|]));
    "game over - p1 wins - horizontal" >::
    (fun _ -> assert_raises (State.Game_over "A") 
        (fun () -> State.mark st1 'A'; State.mark st1 'D'; State.mark st1 'B'; 
          State.mark st1 'E'; State.mark st1 'C'));
    "game over - p2 wins" >::
    (fun _ -> assert_raises (State.Game_over "B")
        (fun () -> State.mark st2 'A'; State.mark st2 'D'; State.mark st2 'B'; 
          State.mark st2 'E'; State.mark st2 'G'; State.mark st2 'F'));
    "game over - tie" >::
    (fun _ -> assert_raises (State.Game_over "?")
        (fun () -> State.mark st_tie 'A'; State.mark st_tie 'B'; 
          State.mark st_tie 'E'; State.mark st_tie 'I'; State.mark st_tie 'D'; 
          State.mark st_tie 'G';  State.mark st_tie 'C'; State.mark st_tie 'F'; 
          State.mark st_tie 'H';));
    "game over - vertical" >::
    (fun _ -> assert_raises (State.Game_over "A")
        (fun () -> 
           let vert = State.init_state "A" "B" Classic in 
           State.mark vert 'A'; State.mark vert 'B'; State.mark vert 'D'; 
           State.mark vert 'E'; State.mark vert 'G';    
        ));
    "game over - diagonal 1" >::
    (fun _ -> assert_raises (State.Game_over "A")
        (fun () -> 
           let diag = State.init_state "A" "B" Classic in 
           State.mark diag 'A'; State.mark diag 'B'; State.mark diag 'E';
           State.mark diag 'C'; State.mark diag 'I';        
        ));
    "game over - diagonal 2" >::
    (fun _ -> assert_raises (State.Game_over "A")
        (fun () -> 
           let diag = State.init_state "A" "B" Classic in 
           State.mark diag 'C'; State.mark diag 'B'; State.mark diag 'E';
           State.mark diag 'A'; State.mark diag 'G';        
        ));
  ]

let ust = Ultimatestate.init_state "A" "B"

let ultimate_tests = 
  [
    "get_p1" >:: (fun _ -> assert_equal (Ultimatestate.get_p1 ust) "A");
    "get_p2" >:: (fun _ -> assert_equal (Ultimatestate.get_p2 ust) "B");
    "get_p1_score" >:: 
    (fun _ -> assert_equal (Ultimatestate.get_p1_score ust) 0);
    "get_p2_score" >:: 
    (fun _ -> assert_equal (Ultimatestate.get_p2_score ust) 0);
    "mark invalid position (big board)" >::
    (fun _ -> assert_raises (Ultimatestate.Invalid_board_num)
        (fun () -> Ultimatestate.mark ust 0 'A'));
    "mark invalid position (small board)" >::
    (fun _ -> assert_raises (Ultimatestate.Invalid_pos)
        (fun () -> Ultimatestate.mark ust 5 'J'));
    "mark in wrong board to start" >:: 
    (fun _ -> assert_raises (Ultimatestate.Wrong_board)
        (fun () -> Ultimatestate.mark ust 4 'A'));
    "mark in wrong board after start" >:: 
    (fun _ -> assert_raises (Ultimatestate.Wrong_board)
        (fun () -> Ultimatestate.mark ust 5 'E'; Ultimatestate.mark ust 4 'E'));
    "mark same position twice" >:: 
    (fun _ -> assert_raises (Ultimatestate.Invalid_pos)
        (fun () -> Ultimatestate.mark ust 5 'E'; Ultimatestate.mark ust 5 'E'));
    "mark - correct" >::
    (fun _ -> assert_equal (let ust = Ultimatestate.init_state "A" "B" in
                            Ultimatestate.mark ust 5 'A'; 
                            Ultimatestate.get_board ust) 
        ( [|[|[|[|'A'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|]|];
              [|[|'A'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|]|];
              [|[|'A'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|]|]|];
            [|[|[|'A'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|]|];
              [|[|'1'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|]|];
              [|[|'A'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|]|]|];
            [|[|[|'A'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|]|];
              [|[|'A'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|]|];
              [|[|'A'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|]|]|]|]));
    "mark wrong board" >::
    (fun _ -> assert_raises (Ultimatestate.Wrong_board)
        (fun () -> Ultimatestate.mark ust 5 'A'; Ultimatestate.mark ust 2 'B'));
    "continue" >::
    (fun _ -> assert_equal ( Ultimatestate.continue ust; 
                             Ultimatestate.get_board ust)
        ( [|[|[|[|'A'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|]|];
              [|[|'A'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|]|];
              [|[|'A'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|]|]|];
            [|[|[|'A'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|]|];
              [|[|'A'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|]|];
              [|[|'A'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|]|]|];
            [|[|[|'A'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|]|];
              [|[|'A'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|]|];
              [|[|'A'; 'B'; 'C'|]; [|'D'; 'E'; 'F'|]; [|'G'; 'H'; 'I'|]|]|]|])
    );
    "ultimate win" >:: 
    (fun _ -> assert_raises (Ultimatestate.Game_over "B") 
        (fun _ -> 
           let ust_win = Ultimatestate.init_state "A" "B" in
           (* X                                     O *)
           Ultimatestate.mark ust_win 5 'E'; Ultimatestate.mark ust_win 5 'A';
           Ultimatestate.mark ust_win 1 'E'; Ultimatestate.mark ust_win 5 'H'; 
           Ultimatestate.mark ust_win 8 'E'; Ultimatestate.mark ust_win 5 'D'; 
           Ultimatestate.mark ust_win 4 'E'; Ultimatestate.mark ust_win 5 'G'; 
           Ultimatestate.mark ust_win 7 'B'; Ultimatestate.mark ust_win 2 'A'; 
           Ultimatestate.mark ust_win 1 'B'; Ultimatestate.mark ust_win 2 'B'; 
           Ultimatestate.mark ust_win 2 'H'; Ultimatestate.mark ust_win 8 'D'; 
           Ultimatestate.mark ust_win 4 'B'; Ultimatestate.mark ust_win 2 'C'; 
           Ultimatestate.mark ust_win 3 'H'; Ultimatestate.mark ust_win 8 'A';
           Ultimatestate.mark ust_win 8 'H'; Ultimatestate.mark ust_win 8 'G'; 
        ));
    "mid win" >::
    (fun _ -> assert_equal 
        (
          let ust_mid_win = Ultimatestate.init_state "A" "B" in
          Ultimatestate.mark ust_mid_win 5 'E'; 
          Ultimatestate.mark ust_mid_win 5 'A';
          Ultimatestate.mark ust_mid_win 1 'E'; 
          Ultimatestate.mark ust_mid_win 5 'H'; 
          Ultimatestate.mark ust_mid_win 8 'E'; 
          Ultimatestate.mark ust_mid_win 5 'D'; 
          Ultimatestate.mark ust_mid_win 4 'E'; 
          Ultimatestate.mark ust_mid_win 5 'G'; 
          Ultimatestate.mark ust_mid_win 7 'B'; 
          Ultimatestate.mark ust_mid_win 2 'A'; 
          Ultimatestate.mark ust_mid_win 1 'B'; 
          Ultimatestate.mark ust_mid_win 2 'B'; 
          Ultimatestate.mark ust_mid_win 2 'H'; 
          Ultimatestate.mark ust_mid_win 8 'D'; 
          Ultimatestate.get_final_board ust_mid_win
        ) ([|[|'A'; 'B'; 'C'|]; [|'D'; '0'; 'F'|]; [|'G'; 'H'; 'I'|]|]));
    "play anywhere" >::
    (fun _ -> assert_equal 
        (
          let ust_mid_win = Ultimatestate.init_state "A" "B" in
          Ultimatestate.mark ust_mid_win 5 'E'; 
          Ultimatestate.mark ust_mid_win 5 'A';
          Ultimatestate.mark ust_mid_win 1 'E'; 
          Ultimatestate.mark ust_mid_win 5 'H'; 
          Ultimatestate.mark ust_mid_win 8 'E'; 
          Ultimatestate.mark ust_mid_win 5 'D'; 
          Ultimatestate.mark ust_mid_win 4 'E'; 
          Ultimatestate.mark ust_mid_win 5 'G'; 
          Ultimatestate.mark ust_mid_win 7 'B'; 
          Ultimatestate.mark ust_mid_win 2 'A'; 
          Ultimatestate.mark ust_mid_win 1 'B'; 
          Ultimatestate.mark ust_mid_win 2 'B'; 
          Ultimatestate.mark ust_mid_win 2 'H'; 
          Ultimatestate.mark ust_mid_win 8 'D'; 
          Ultimatestate.mark ust_mid_win 4 'F'; 
          Ultimatestate.mark ust_mid_win 6 'E'; 
          Ultimatestate.mark ust_mid_win 3 'A'
        ) ());
    "play anywhere after tie" >::
    (fun _ -> assert_equal
        (
          let tie_ust = Ultimatestate.init_state "A" "B" in
          Ultimatestate.mark tie_ust 5 'E'; Ultimatestate.mark tie_ust 5 'I';
          Ultimatestate.mark tie_ust 9 'E'; Ultimatestate.mark tie_ust 5 'A'; 
          Ultimatestate.mark tie_ust 1 'E'; Ultimatestate.mark tie_ust 5 'B'; 
          Ultimatestate.mark tie_ust 2 'E'; Ultimatestate.mark tie_ust 5 'H'; 
          Ultimatestate.mark tie_ust 8 'F'; Ultimatestate.mark tie_ust 6 'F';
          Ultimatestate.mark tie_ust 6 'I'; Ultimatestate.mark tie_ust 9 'C'; 
          Ultimatestate.mark tie_ust 3 'E'; Ultimatestate.mark tie_ust 5 'F'; 
          Ultimatestate.mark tie_ust 6 'D'; Ultimatestate.mark tie_ust 4 'E'; 
          Ultimatestate.mark tie_ust 5 'C'; Ultimatestate.mark tie_ust 3 'G'; 
          Ultimatestate.mark tie_ust 7 'E'; Ultimatestate.mark tie_ust 5 'D'; 
          Ultimatestate.mark tie_ust 4 'H'; Ultimatestate.mark tie_ust 8 'E'; 
          Ultimatestate.mark tie_ust 5 'G'; Ultimatestate.mark tie_ust 7 'F'; 
          Ultimatestate.mark tie_ust 6 'E'; Ultimatestate.mark tie_ust 8 'B'  
        )())
  ]

let suite =
  "test suite for final project"  >::: List.flatten [
    command_tests;
    state_tests;
    ultimate_tests
  ]

let _ = run_test_tt_main suite
