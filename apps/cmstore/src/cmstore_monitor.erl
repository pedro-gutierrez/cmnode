-module(cmstore_monitor).
-behaviour(gen_statem).
-export([start_link/1]). 
-export([callback_mode/0, 
         init/1, 
         terminate/3]).
-export([ready/3]).

start_link(Stores) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Stores], []).

callback_mode() ->
    state_functions.


terminate(_, _, _) ->
    ok.

init([Stores]) ->
    {ok, Data} = open_stores(Stores),
    {ok, ready, Data}.

ready(EvType, EvData, Data) ->
    cmkit:warning({?MODULE, unexpected, EvType, EvData}),
    {keep_state, Data}.


open_stores(Stores) ->
    open_stores(Stores, #{}).

open_stores([], Index) -> {ok, Index};
open_stores([#{ name := Name}=S|Rest], Index) -> 
    case cmstore_util:open(S) of 
        {ok, Pid} -> 
            Ref = erlang:monitor(process, Pid),
            ok = cmstore_util:init(Name),
            cmkit:log({cmstore, Name}),
            open_stores(Rest, Index#{ Pid => #{ store => Name,
                                                ref => Ref }});
        Other ->
            Other
    end.
