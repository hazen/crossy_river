%%%-------------------------------------------------------------------
%%% @author Brett Hazen
%%% @copyright (C) 2018
%%% @doc
%%% Simple unit tests for basic functionality
%%% @end
%%%-------------------------------------------------------------------
-module(test_cases).

-include_lib("eunit/include/eunit.hrl").

parse_initial_state_all_left_test() ->
  ?assertEqual(crossy_river:state_to_bank("fcdg~"), {"fcdg", ""}).

parse_initial_state_all_right_test() ->
  ?assertEqual(crossy_river:state_to_bank("~fcdg"), {"", "fcdg"}).

parse_initial_state_split_test() ->
  ?assertEqual(crossy_river:state_to_bank("fc~dg"), {"fc", "dg"}).

parse_initial_state_rubbish_test() ->
  ?assertEqual(crossy_river:state_to_bank("foobar"), {"foobar", ""}).

parse_custom_record_name_test() ->
  {Names, Eats} = crossy_river:parse_custom_record(<<"c is Chicken">>, {#{}, #{}}),
  ?assertEqual(Names, #{"c" => "Chicken"}),
  ?assertEqual(Eats, #{}).

parse_custom_record_eats_test() ->
  {Names, Eats} = crossy_river:parse_custom_record(<<"c eats g">>, {#{}, #{}}),
  ?assertEqual(Names, #{}),
  ?assertEqual(Eats, #{"c" => "g"}).

parse_custom_record_rubbish_test() ->
  ?assertError(function_clause,
    crossy_river:parse_custom_record(<<"all good boys deserve fudge">>, {#{}, #{}})).
