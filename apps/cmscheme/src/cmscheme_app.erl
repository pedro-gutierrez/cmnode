%%%-------------------------------------------------------------------
%% @doc cmscheme public API
%% @end
%%%-------------------------------------------------------------------

-module(cmscheme_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    cmscheme_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
