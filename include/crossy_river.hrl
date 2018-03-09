%%%-------------------------------------------------------------------
%%% @author Brett Hazen
%%% @copyright (C) 2018
%%% @doc
%%% Common definitions used by application and unit tests
%%% @end
%%%-------------------------------------------------------------------

-define(APP, crossy_river).
-define(STATEM, crossy_river_statem).

-record(state, {
  %% List of items on the left side of the river
  left_bank      = [] :: list(),
  %% List of items on the right side of the river
  right_bank     = [] :: list(),
  %% Map of item single characters to their English names
  names          = #{} :: map(),
  %% Map of predators to pray (single characters)
  eaters         = #{} :: map(),
  %% If something is eaten in replay mode, its name ends up here
  eaten          = [] :: list(),
  %% Work queue of moves to make
  moves          = [] :: list(),
  %% Solver only: List of lists of possible moves, with one list per round,
  %% current round at the head of the list
  possible_moves = [] :: list(),
  %% Solver only: Current list of successful moves (in reverse)
  success_moves  = [] :: list(),
  %% True if we are in auto-solver mode
  solve          = false :: boolean()
}).
