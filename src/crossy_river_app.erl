%%%-------------------------------------------------------------------
%% @doc crossy_river public API
%% @end
%%%-------------------------------------------------------------------

-module(crossy_river_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    crossy_river_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
