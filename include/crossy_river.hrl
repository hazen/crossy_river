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
  left_bank      = [] :: list(),
  right_bank     = [] :: list(),
  names          = #{} :: map(),
  eaters         = #{} :: map(),
  eaten          = [] :: list(),
  moves          = [] :: list(),
  possible_moves = [] :: list(),
  success_moves  = [] :: list(),
  solve          = false :: boolean()
}).
