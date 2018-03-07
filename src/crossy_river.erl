%%%-------------------------------------------------------------------
%%% @author Brett Hazen
%%% @copyright (C) 2018
%%% @doc
%%% Simple driver for reading the state file and parsing arguments
%%% @end
%%%-------------------------------------------------------------------
-module(crossy_river).
-define(APP, crossy_river_app).

%% API
-export([main/0]).

-spec(main() -> ok).
main() ->
  %% Set up the initial state of the application


  Args = init:get_plain_arguments(),
  configure(Args),
  ok.

%% @doc Read the optional configuration
-spec(configure([string()]) -> ok | error).
configure(["<default>", "<default>", "false"]) ->
  io:format(standard_error, "No configuration specified~n", []),
  error;
configure(["<default>", _, "false"]) ->
  io:format(standard_error, "Only custom config is not allowed~n", []),
  error;
configure([Replay, Custom, Solve]) ->
  [Initial|Moves] = setup_replay_state(Replay),
  application:set_env(?APP, initial, Initial, [{persistent, true}]),
  application:set_env(?APP, moves, Moves, [{persistent, true}]),
  {Names, Eaters} = setup_items(Custom),
  application:set_env(?APP, names, Names, [{persistent, true}]),
  application:set_env(?APP, eaters, Eaters, [{persistent, true}]),
  application:set_env(?APP, solve, list_to_atom(Solve), [{persistent, true}]),
  ok;
configure(Bad) ->
  io:format(standard_error, "Bad configuration ~p~n", Bad),
  error.

%% @doc Either set the default state and moves or read from a file
-spec(setup_replay_state(string()) -> list() | ok).
setup_replay_state("<default>") ->
  %% defaults
  Initial = "fcdg~",
  Moves = [],
  [Initial|Moves];
setup_replay_state(Replay) ->
  io:format("Loading replay file ~s~n", [Replay]),
  load_replay(file:read_file(Replay)).

%% @doc Load the replay file of the format:
%%   <initial state>
%%   <move 1>
%%    ...
%%   <move n>
-spec(load_replay({error|ok, term()}) -> ok | list()).
load_replay({error, Reason}) ->
  io:format(standard_error, "Problem loading the file: ~p~n", [Reason]);
load_replay({ok, Binary}) ->
  Lines = binary:split(Binary, <<"\n">>, [global, trim_all]),
  lists:map(fun(X) -> binary_to_list(X) end, Lines).

%% @doc Set up the names of the items and rules about who eats whom
%% or read from a customization file
-spec(setup_items(string()) -> ok | {map(), map()}).
setup_items("<default>") ->
  %% defaults
  Names = #{
    "c" => "Chicken",
    "d" => "Dog",
    "g" => "Grain"
  },
  Eaters = #{
    "d" => "c",
    "c" => "g"
  },
  {Names, Eaters};
setup_items(Custom) ->
  io:format("Loading customization file ~s~n", [Custom]),
  load_custom(file:read_file(Custom)).

%% @doc Load the customization file described in parse_custom_record
-spec(load_custom({error|ok, term()}) -> ok | {map(), map()}).
load_custom({error, Reason}) ->
  io:format(standard_error, "Problem loading the file: ~p~n", [Reason]);
load_custom({ok, Binary}) ->
  Lines = binary:split(Binary, <<"\n">>, [global, trim_all]),
  {Names, Eats} = lists:foldl(fun parse_custom_record/2, {maps:new(), maps:new()}, Lines),
  {Names, Eats}.

%% @doc Parse a single record in the customization file which defines both
%% the abbreviations of items to be taken across the river and their text description
%% and the rules about which entity is likely to eat another other entity
%% Format is either:
%%   <short_name> is <long_name>
%% or
%%   <eater> eats <dinner>
-spec(parse_custom_record(binary(), {map(), map()}) -> {map(), map()}).
parse_custom_record(<<Diner:8," eats ", Dinner/binary>>, {Names, Eats}) ->
  {Names, maps:put([Diner], binary_to_list(Dinner), Eats)};
parse_custom_record(<<Short:8, " is ", Long/binary>>, {Names, Eats}) ->
  {maps:put([Short], binary_to_list(Long), Names), Eats}.
