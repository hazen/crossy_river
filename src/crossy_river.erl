%%%-------------------------------------------------------------------
%%% @author Brett Hazen
%%% @copyright (C) 2018
%%% @doc
%%% Simple driver for reading the state file and parsing arguments
%%% @end
%%%-------------------------------------------------------------------
-module(crossy_river).

-include("../include/crossy_river.hrl").

%% API
-ifdef(EUNIT).
-compile(export_all).
-else.
-export([main/0]).
-endif.

-spec(main() -> ok).
main() ->
  %% Set up the initial state of the application
  Args = init:get_plain_arguments(),
  Result = configure(Args),
  case Result of
    %% if there is a problem, don't bother starting
    error -> ok;
    State ->
      application:set_env(?APP, initial_state, State, [{persistent, true}]),
      ok = application:start(?APP),
      launch(State),

      %% Test 10 times for the statem to complete, otherwise die
      wait_for_completion([], 10)
  end,
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Read the optional configuration files
%%--------------------------------------------------------------------

-spec(configure([string()]) -> #state{} | error).
configure(["<default>", "<default>", "false"]) ->
  io:format(standard_error, "No configuration specified~n", []),
  error;
configure(["<default>", _, "false"]) ->
  io:format(standard_error, "Only custom config is not allowed~n", []),
  error;
configure([Replay, Custom, Solve]) ->
  [Initial|Moves] = setup_replay_state(Replay),
  {Names, Eaters} = setup_items(Custom),
  {Left, Right} = state_to_bank(Initial),
  #state{
    left_bank = Left,
    right_bank = Right,
    moves = Moves,
    names = Names,
    eaters = Eaters,
    solve = list_to_atom(Solve)
  };
configure(Bad) ->
  io:format(standard_error, "Bad configuration ~p~n", Bad),
  error.

%%--------------------------------------------------------------------
%% @private
%% @doc Launch the statem with the correct configuration
%%--------------------------------------------------------------------
-spec(launch(#state{}) -> ok).
launch(#state{solve = true}) -> crossy_river_statem:solve();
launch(_) -> crossy_river_statem:replay_move().

%%--------------------------------------------------------------------
%% @private
%% @doc Wait until the FSM is in a completed state by retrying N times
%%--------------------------------------------------------------------
-spec(wait_for_completion(gen_statem:state(), integer()) -> ok).
wait_for_completion({complete, _}, _) -> ok;
wait_for_completion({eaten, _}, _) -> ok;
wait_for_completion(_, 0) ->  ok;
wait_for_completion(_, N) when is_integer(N) ->
  timer:sleep(1000),
  wait_for_completion(sys:get_state(?STATEM), N-1).

%%--------------------------------------------------------------------
%% @private
%% @doc Either set the default state and moves or read from a file
%%--------------------------------------------------------------------

-spec(setup_replay_state(string()) -> list() | ok).
setup_replay_state("<default>") ->
  %% defaults
  Initial = "fcdg~",
  Moves = [],
  [Initial|Moves];
setup_replay_state(Replay) ->
  io:format("Loading replay file ~s~n", [Replay]),
  load_replay(file:read_file(Replay)).

%%--------------------------------------------------------------------
%% @private
%% @doc Load the replay file of the format:
%%   <initial state>
%%   <move 1>
%%    ...
%%   <move n>
%%--------------------------------------------------------------------

-spec(load_replay({error|ok, term()}) -> ok | list()).
load_replay({error, Reason}) ->
  io:format(standard_error, "Problem loading the file: ~p~n", [Reason]);
load_replay({ok, Binary}) ->
  Lines = binary:split(Binary, <<"\n">>, [global, trim_all]),
  lists:map(fun(X) -> binary_to_list(X) end, Lines).

%%--------------------------------------------------------------------
%% @private
%% @doc Set up the names of the items and rules about who eats whom
%% or read from a customization file
%%--------------------------------------------------------------------

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

%%--------------------------------------------------------------------
%% @private
%% @doc Load the customization file described in parse_custom_record
%%--------------------------------------------------------------------

-spec(load_custom({error|ok, term()}) -> ok | {map(), map()}).
load_custom({error, Reason}) ->
  io:format(standard_error, "Problem loading the file: ~p~n", [Reason]);
load_custom({ok, Binary}) ->
  Lines = binary:split(Binary, <<"\n">>, [global, trim_all]),
  {Names, Eats} = lists:foldl(fun parse_custom_record/2, {maps:new(), maps:new()}, Lines),
  {Names, Eats}.

%%--------------------------------------------------------------------
%% @private
%% @doc Parse a single record in the customization file which defines both
%% the abbreviations of items to be taken across the river and their text description
%% and the rules about which entity is likely to eat another other entity
%% Format is either:
%%   <short_name> is <long_name>
%% or
%%   <eater> eats <dinner>
%%--------------------------------------------------------------------

-spec(parse_custom_record(binary(), {map(), map()}) -> {map(), map()}).
parse_custom_record(<<Diner:8," eats ", Dinner/binary>>, {Names, Eats}) ->
  {Names, maps:put([Diner], binary_to_list(Dinner), Eats)};
parse_custom_record(<<Short:8, " is ", Long/binary>>, {Names, Eats}) ->
  {maps:put([Short], binary_to_list(Long), Names), Eats}.

%%--------------------------------------------------------------------
%% @private
%% @doc Translate a string into a tuple for each river bank. Internally the river
%% representation is implied.
%%--------------------------------------------------------------------

-spec(state_to_bank(string()) -> {string(), string()} | {error, string()}).
state_to_bank(State) ->
  {Left, Right} = lists:splitwith(fun(X) -> [X] =/= "~" end, State),
  {Left, Right -- "~"}.

