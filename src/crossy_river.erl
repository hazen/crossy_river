%%%-------------------------------------------------------------------
%%% @author Brett Hazen
%%% @copyright (C) 2018
%%% @doc
%%% Simple driver for reading the state file and parsing arguments
%%% @end
%%%-------------------------------------------------------------------
-module(crossy_river).

%% API
-export([main/0]).

-spec(main() -> ok).
main() ->
  Args = init:get_plain_arguments(),
  io:format("~p~n", [Args]),
  ok.

%% application:start(myapp)