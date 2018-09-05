%%%-------------------------------------------------------------------
%% @doc cmdb2 public API
%% @end
%%%-------------------------------------------------------------------

-module(cmdb2_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    cmdb2_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================