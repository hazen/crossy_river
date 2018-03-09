%%%-------------------------------------------------------------------
%%% @author Brett Hazen
%%% @copyright (C) 2018
%%% @doc
%%% State machine to model moving items across a river.
%%%
%%% Four states are modelled:
%%% - Boat on the Left Bank
%%% - Boat on the Right Bank
%%% - Something ate something else
%%% - Completed journey
%%%
%%% There are two main modes of operation for the state machine:
%%% - Replay prerecorded actions (from a file).  This mode uses the
%%%   "move" action.
%%% - Solver: Discover a correct series of moves to transfer all items
%%%   from the left bank of the river to the right one. This mode uses
%%%   the "solve" action.
%%%
%%% Note: The FSM state is referred to as "state" whereas the data
%%% associated with the OTP behaviour is a record called "state", by
%%% convention, but is referred to as "Data" as much as possible.
%%% States and state machines. ¯\_(ツ)_/¯
%%% @end
%%%-------------------------------------------------------------------
-module(crossy_river_statem).

-behaviour(gen_statem).

%% API
-ifdef(EUNIT).
-compile(export_all).
-else.
-export([
  start_link/0,
  replay_move/0,
  solve/0
]).
-endif.

%% gen_statem callbacks
-export([
  init/1,
  callback_mode/0,
  format_status/2,
  left_bank/3,
  right_bank/3,
  complete/3,
  eaten/3,
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

-spec(replay_move() -> ok).
replay_move() ->
  gen_statem:cast(?SERVER, move).

solve() ->
  gen_statem:cast(?SERVER, solve).

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
  %% Start on the correct bank based on the location of the Farmer
  InitialState = case string:str(InitialData#state.left_bank, "f") > 0 of
    true -> left_bank;
    _ -> right_bank
  end,
  %% Clean out any replay moves if we are in solve mode
  InitialMoves = case InitialData#state.solve of
                   true -> [];
                   _ -> InitialData#state.moves
                 end,
  {ok, InitialState, InitialData#state{moves = InitialMoves}}.

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
  {NextStateName, NewData} = do_move(Data),
  replay_move(),
  {next_state, NextStateName, NewData};
left_bank(cast, solve, Data) ->
  NewData = attempt_move(Data),
  {NextStateName, NewData1} = do_move(NewData),
  solve(),
  {next_state, NextStateName, NewData1}.

right_bank(cast, move, Data) ->
  {NextStateName, NewData} = do_move(Data),
  replay_move(),
  {next_state, NextStateName, NewData};
right_bank(cast, solve, Data) ->
  NewData = attempt_move(Data),
  {NextStateName, NewData1} = do_move(NewData),
  solve(),
  {next_state, NextStateName, NewData1}.

eaten(cast, move, Data) ->
  print_river(Data),
  {next_state, eaten, Data};
eaten(cast, solve, Data) ->
  NewData = undo_move(Data),
  {NextStateName, NewData1} = do_move(NewData),
  solve(),
  {next_state, NextStateName, NewData1}.

complete(cast, move, Data) ->
  print_river(Data),
  {next_state, complete, Data};
complete(cast, solve, #state{success_moves = Moves} = Data) ->
  io:format("~s~n", [string:join(lists:reverse(Moves), "\n")]),
  print_river(Data),
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Process the action of moving the farmer and some items from one bank
%% to the other.  Basically a move is taken off the `moves` list and
%% the items are transferred from one bank to the other.
%%
%% When there are no other moves left, we are complete.
%% @end
%%--------------------------------------------------------------------

-spec(do_move(#state{}) -> {gen_statem:state(), #state{}}).
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
        cross_river(Left, Right, Data)
    end,
  {NextState, NewData}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Take the items from one side of the river and move it to the other.
%% The new state (typically the other bank) is also specified, but
%% can also be be the `eaten` state if something ate something else
%% after the move.
%% @end
%%--------------------------------------------------------------------

-spec(cross_river(list(), list(), #state{}) -> {gen_statem:state(), #state{}}).
cross_river(Left, Right, #state{moves = [Move|Rest]} = Data) ->
  {LeftBank, RightBank, Eaten, NewState} = case string:str(Move, "<") > 0 of
    true ->
      Items = Move -- "<",
      determine_new_state(Left ++ Items, Right -- Items, Data, left_bank);
    _ ->
      Items = Move -- ">",
      determine_new_state(Left -- Items, Right ++ Items, Data, right_bank)
  end,
  {NewState, Data#state{
    left_bank = LeftBank,
    right_bank = RightBank,
    eaten = Eaten,
    moves = Rest
  }}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the next state and specifically see if anything was eaten
%% on either side of the river.
%% @end
%%--------------------------------------------------------------------

-spec(determine_new_state(Left :: list(),
    Right :: list(),
    Data :: #state{},
    Default :: gen_statem:state()) -> {list(), list(), list(), gen_statem:state()}).
determine_new_state(Left, Right, Data, Default) ->
  Eaten = check_anything_eaten(Left, Data) ++ check_anything_eaten(Right, Data),
  {Left, Right, Eaten,
    case Eaten of
      [] -> Default;
      _ -> eaten
    end}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine if anything on this riverbank will eat anything else.
%% Return an empty list if not, or the name of the victim, if so.
%% The rules of what eats what as well as the eaten names are taken
%% from the #state{} record.
%% @end
%%--------------------------------------------------------------------

-spec(check_anything_eaten(list(), #state{}) -> list()).
check_anything_eaten(Items,
    #state{eaters = Eaters,
           names = Names}) ->
    lists:foldl(fun(Key, Acc) ->
      Eaten = maps:get(Key, Eaters),
      case lists:member(hd(Key), Items) and lists:member(hd(Eaten), Items) and not lists:member($f, Items) of
        true -> maps:get(Eaten, Names);
        _ -> Acc
      end
      end, [], maps:keys(Eaters)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Take the two bank fields and print out the compact state string which
%% is comprised of a single letter for each actor and ~ for the river.
%% Also if something was eaten along the way, write out what it was.
%% @end
%%--------------------------------------------------------------------

-spec(print_river(#state{}) -> ok).
print_river(
    #state{
      left_bank = Left,
      right_bank = Right,
      eaten = Eaten}) ->
  io:format("~s~~~s~n", [Left, Right]),
  case Eaten of
    [] -> ok;
    Eaten -> io:format("~s was eaten.~n", [Eaten])
  end.

%%%===================================================================
%%% Solver functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Try a variety of moves to determine a success path.  If everything
%% has moved from the left bank to the right, then we have succeeded.
%%
%% Three record fields are critical to tracking the progress of attempted
%% river crossings:
%% - `possible_moves` - List of lists of possible moves.  The most recent
%%   rounds are towards the front of the list.
%% - `success_moves` - Reversed list of crossings so far.  The most current
%%   one is at the front of the list.
%% - `moves` - List of moves to apply to the FSM.  In the Solver mode, this
%%   is only a single move at a time.
%%
%% If all moves for a particular round prove unsuccessful, then one sublist
%% in `possible_moves` is removed and the remaining moves of the previous
%% round are attempted.
%% @end
%%--------------------------------------------------------------------

-spec(attempt_move(#state{}) -> #state{}).

%% Everything has been moved off of the left bank, so complete
attempt_move(#state{left_bank = []} = Data) ->
  Data#state{moves = []};

%% Try new states depending on if all options have been exhausted or not
%% Update the state to reflect the move.
attempt_move(
    #state{
      possible_moves = Possible,
      success_moves = Success} = Data) ->
  LastMove = last_move(Success),
  case length(Possible) == length(Success) of
    %% New round, so generate new possible moves
    true ->
      [Move|MoreMoves] = generate_possible_moves(LastMove, Data),
      Data#state{
        moves = [Move],
        possible_moves = [MoreMoves|Possible],
        success_moves = [Move|Success]
      };
    %% Retrying a previous round, so use an existing one
    _ ->
      [ThisRound|OtherRounds] = Possible,
      case ThisRound of
        %% Out of options, so move back to a previous round
        [] -> undo_move(
          Data#state{
            possible_moves = OtherRounds
          });
        %% Still some options to try from the previous round
        [Move|MoreMoves] ->
          Data#state{
            moves = [Move],
            possible_moves = [MoreMoves|OtherRounds],
            success_moves = [Move|Success]
          }
      end
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Take the last move from the list of successful moves and issue
%% the opposite move to get back to the previous state.
%% @end
%%--------------------------------------------------------------------

-spec(undo_move(#state{}) -> #state{}).
undo_move(
    #state{
      success_moves = [Previous|Success]
    } = Data) ->
  Data#state{
    moves = [reverse_move(Previous)],
    success_moves = Success
  }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get the previous move from a list of moves. Empty list returns
%% the empty list.
%% @end
%%--------------------------------------------------------------------

-spec(last_move(list()) -> list()).
last_move([]) -> [];
last_move([Previous|_]) -> Previous.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generate all possible moves from the current state, except for
%% the opposite move of the move just made, to help prune loops.
%% @end
%%--------------------------------------------------------------------

-spec(generate_possible_moves(list(), #state{}) -> list()).
generate_possible_moves(
    Previous,
    #state{
      left_bank = Left,
      right_bank = Right} = _Data) ->
  Reverse = reverse_move(Previous),
  case lists:member($f, Left) of
    true ->
      lists:foldl(
        fun($f, Acc) ->
          ["f>"|Acc];
        (X, Acc) ->
          [lists:flatten(io_lib:format("f~s>", [[X]]))|Acc]
        end, [], Left);
    _ ->
      lists:foldl(
        fun($f, Acc) ->
          ["<f"|Acc];
        (X, Acc) ->
          [lists:flatten(io_lib:format("<f~s", [[X]]))|Acc]
        end, [], Right)
  end -- [Reverse].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generate an opposite move to the one supplied, i.e., move in the
%% opposite direction
%% @end
%%--------------------------------------------------------------------

-spec reverse_move(list()) -> list().
reverse_move([]) -> [];
reverse_move([$<|Left]) ->
  lists:flatten(io_lib:format("~s>", [Left]));
reverse_move(Right) ->
  [_|Rest] = lists:reverse(Right),
  lists:flatten(io_lib:format("<~s", [lists:reverse(Rest)])).
