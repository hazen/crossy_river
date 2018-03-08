%%%-------------------------------------------------------------------
%%% @author Brett Hazen
%%% @copyright (C) 2018
%%% @doc
%%% Common definitions used by application and unit tests
%%% @end
%%%-------------------------------------------------------------------

-define(APP, crossy_river).
-record(state, {
  left_bank  = [] :: list(),
  right_bank = [] :: list(),
  names      = #{} :: map(),
  eaters     = #{} :: map(),
  moves      = [] :: list(),
  solve      = false :: boolean(),
  eaten      = [] :: list()
}).
