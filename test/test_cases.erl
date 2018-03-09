%%%-------------------------------------------------------------------
%%% @author Brett Hazen
%%% @copyright (C) 2018
%%% @doc
%%% Simple unit tests for basic functionality
%%% @end
%%%-------------------------------------------------------------------
-module(test_cases).

-include_lib("eunit/include/eunit.hrl").
-include("../include/crossy_river.hrl").

parse_initial_state_all_left_test() ->
  ?assertEqual({"fcdg", ""}, crossy_river:state_to_bank("fcdg~")).

parse_initial_state_all_right_test() ->
  ?assertEqual({"", "fcdg"}, crossy_river:state_to_bank("~fcdg")).

parse_initial_state_split_test() ->
  ?assertEqual({"fc", "dg"}, crossy_river:state_to_bank("fc~dg")).

parse_initial_state_rubbish_test() ->
  ?assertEqual({"foobar", ""}, crossy_river:state_to_bank("foobar")).

parse_custom_record_name_test() ->
  {Names, Eats} = crossy_river:parse_custom_record(<<"c is Chicken">>, {#{}, #{}}),
  ?assertEqual(#{"c" => "Chicken"}, Names),
  ?assertEqual(#{}, Eats).

parse_custom_record_eats_test() ->
  {Names, Eats} = crossy_river:parse_custom_record(<<"c eats g">>, {#{}, #{}}),
  ?assertEqual(#{}, Names),
  ?assertEqual(#{"c" => "g"}, Eats).

parse_custom_record_rubbish_test() ->
  ?assertError(function_clause,
    crossy_river:parse_custom_record(<<"all good boys deserve fudge">>, {#{}, #{}})).

nothing_eaten_prey_only_test() ->
  State = #state{eaters = #{"a" => "b"}},
  ?assertEqual([], crossy_river_statem:check_anything_eaten("xbyz", State)).

nothing_eaten_predator_only_test() ->
  State = #state{eaters = #{"a" => "b"}},
  ?assertEqual([], crossy_river_statem:check_anything_eaten("xayz", State)).

not_each_eaten_farmer_present_test() ->
  State = #state{eaters = #{"a" => "b"}, names = #{"b" => "Banana"}},
  ?assertEqual([], crossy_river_statem:check_anything_eaten("abf", State)).

something_eaten_test() ->
  State = #state{eaters = #{"a" => "b"}, names = #{"b" => "Banana"}},
  ?assertEqual("Banana", crossy_river_statem:check_anything_eaten("ab", State)).

generate_possible_moves_left_no_previous_test() ->
  State = #state{left_bank = "abcf", right_bank = ""},
  Expected = ["f>","fc>","fb>","fa>"],
  Possible = crossy_river_statem:generate_possible_moves([], State),
  ?assertEqual(lists:sort(Expected), lists:sort(Possible)).

generate_possible_moves_left_exclude_previous_test() ->
  State = #state{left_bank = "abcf", right_bank = ""},
  Expected = ["f>","fb>","fa>"],
  Possible = crossy_river_statem:generate_possible_moves("<fc", State),
  ?assertEqual(lists:sort(Expected), lists:sort(Possible)).

generate_possible_moves_right_exclude_previous_test() ->
  State = #state{left_bank = "", right_bank = "abcf"},
  Expected = ["<f","<fb","<fa"],
  Possible = crossy_river_statem:generate_possible_moves("fc>", State),
  ?assertEqual(lists:sort(Expected), lists:sort(Possible)).

generate_possible_moves_right_no_previous_test() ->
  State = #state{left_bank = "", right_bank = "abcf"},
  Expected = ["<f","<fa","<fb","<fc"],
  Possible = crossy_river_statem:generate_possible_moves([], State),
  ?assertEqual(lists:sort(Expected), lists:sort(Possible)).

generate_possible_moves_none_test() ->
  State = #state{left_bank = "", right_bank = ""},
  Possible = crossy_river_statem:generate_possible_moves([], State),
  ?assertEqual([], lists:sort(Possible)).

attempt_move_success_test() ->
  State = #state{left_bank = ""},
  Result = crossy_river_statem:attempt_move(State),
  ?assertEqual([], Result#state.moves).

attempt_move_no_previous_move_test() ->
  State = #state{left_bank = "abcf", right_bank = ""},
  Result = crossy_river_statem:attempt_move(State),
  Possible = ["fc>","fb>","fa>"],
  ?assertEqual(["f>"], Result#state.moves),
  ?assertEqual(lists:sort(Possible), lists:sort(hd(Result#state.possible_moves))),
  ?assertEqual(["f>"], Result#state.success_moves).

attempt_move_explore_new_round_test() ->
  State = #state{
    left_bank = "cdf",
    right_bank = "ab",
    possible_moves = [["f1>","f2>"],["<fa","<fb","<fc"]],
    success_moves = ["<f","fb>"]},
  Result = crossy_river_statem:attempt_move(State),
  ?assertEqual(["fd>"], Result#state.moves),
  ?assertEqual(["fc>"], hd(Result#state.possible_moves)),
  ?assertEqual(3, length(Result#state.possible_moves)),
  ?assertEqual(["fd>","<f","fb>"], Result#state.success_moves).

attempt_move_try_other_possibilities_test() ->
  State = #state{
    left_bank = "cdf",
    right_bank = "ab",
    possible_moves = [["f1>","f2>","f3>"],["<fa","<fb","<fc"]],
    success_moves = ["fb>"]},
  Result = crossy_river_statem:attempt_move(State),
  ?assertEqual(["f1>"], Result#state.moves),
  ?assertEqual(["f2>","f3>"], hd(Result#state.possible_moves)),
  ?assertEqual(2, length(Result#state.possible_moves)),
  ?assertEqual(["f1>","fb>"], Result#state.success_moves).

% We've tried all moves for this round, so back out and try previous round
attempt_move_exhausted_round_move_test() ->
  State = #state{
    left_bank = "cdf",
    right_bank = "ab",
    possible_moves = [[],["<fa","<fb","<fc"]],
    success_moves = ["fb>"]},
  Result = crossy_river_statem:attempt_move(State),
  ?assertEqual(["<fb"], Result#state.moves),
  ?assertEqual(["<fa","<fb","<fc"], hd(Result#state.possible_moves)),
  ?assertEqual(1, length(Result#state.possible_moves)),
  ?assertEqual([], Result#state.success_moves).

reverse_move_left_test() ->
  ?assertEqual("<abc", crossy_river_statem:reverse_move("abc>")).

reverse_move_right_test() ->
  ?assertEqual("abc>", crossy_river_statem:reverse_move("<abc")).
