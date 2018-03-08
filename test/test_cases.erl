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
