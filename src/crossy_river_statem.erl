%%%-------------------------------------------------------------------
%%% @author Brett Hazen
%%% @copyright (C) 2018
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(crossy_river_statem).

-behaviour(gen_statem).

%% API
-export([
  start_link/0,
  replay_move/0
]).

%% gen_statem callbacks
-export([
  init/1,
  callback_mode/0,
  format_status/2,
  left_bank/3,
  right_bank/3,
  complete/3,
  handle_event/4,
  terminate/3,
  code_change/4
]).

-define(SERVER, ?MODULE).

-include("../include/crossy_river.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() -> {ok, pid()} | ignore | {error, term()}).
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

replay_move() ->
  gen_statem:cast(?SERVER, move).


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(gen_statem:state()) -> {ok, gen_statem:state_name(), gen_statem:state()}).
init([]) ->
  InitialData = application:get_env(?APP, initial_state, #state{}),
  InitialState = case string:str(InitialData#state.left_bank, "f") > 0 of
    true -> left_bank;
    _ -> right_bank
  end,
  {ok, InitialState, InitialData}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started it can use either state_functions
%% or handle_event_function.  We'll stick with the FSM-like state_functions.
%% @end
%%--------------------------------------------------------------------

-spec(callback_mode() -> state_functions).
callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
%%
%% @spec format_status(Opt, [PDict, StateName, State]) -> term()
%% @end
%%--------------------------------------------------------------------
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name.  If callback_mode is statefunctions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
left_bank(cast, move, Data) ->
  io:format("Welcome to the Left Bank~n"),
  {NextStateName, NewData} = do_move(Data),
  io:format("~p ~p~n", [NextStateName, NewData]),
  replay_move(),
  {next_state, NextStateName, NewData}.

right_bank(cast, move, Data) ->
  io:format("Welcome to the Right Bank~n"),
  {NextStateName, NewData} = do_move(Data),
  io:format("~p ~p~n", [NextStateName, NewData]),
  replay_move(),
  {next_state, NextStateName, NewData}.

complete(cast, move, Data) ->
  print_river(Data),
  application:stop(?APP),
  {next_state, complete, Data}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
handle_event(_EventType, _EventContent, _StateName, State) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_move(
    #state{
      left_bank = Left,
      right_bank = Right,
      moves = Moves} = Data) ->
  {NextState, NewData} =
    case Moves of
      [] ->
        {complete, Data};
      Moves when is_list(Moves) ->
        {Next, NewLeft, NewRight, Rest} = cross_river(Left, Right, Moves),
        {Next, Data#state{
          left_bank = NewLeft,
          right_bank = NewRight,
          moves = Rest
        }}
    end,
  {NextState, NewData}.

cross_river(Left, Right, [Move|Rest]) ->
  case string:str(Move, "<") > 0 of
    true ->
      Items = Move -- "<",
      {left_bank, Left ++ Items, Right -- Items, Rest};
    _ ->
      Items = Move -- ">",
      {right_bank, Left -- Items, Right ++ Items, Rest}
  end.

check_anything_eaten(Items,
    #state{eaters = Eaters,
           names = Names}) ->
    lists:foldl(fun(Key, Acc) ->
      Eaten = maps:get(Key, Eaters),
      case lists:member(Key, Items) and lists:member(Eaten, Items) of
        true -> maps:get(Eaten, Names);
        _ -> Acc
      end
      end, [], maps:keys(Eaters)).

print_river(
    #state{
      left_bank = Left,
      right_bank = Right,
      eaten = Eaten}) ->
  io:format("~s~~~s~n", [Left, Right]),
  case Eaten of
    [] -> ok;
    Eaten -> io:format("~s was eaten.", [Eaten])
  end.