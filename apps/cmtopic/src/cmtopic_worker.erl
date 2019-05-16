-module(cmtopic_worker).
-behaviour(gen_server).
-export([
         start_link/2
        ]).
-export([
         init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3
        ]).

start_link(Id, Spec) ->
    gen_server:start_link({local, Id}, ?MODULE, [Spec], []).

init([#{ name := Name,
         spec := #{ on := Spec,
                    services := Services }}]) ->

    case cmencode:encode(Spec) of 
        {ok, Topic} ->
            cmbus:create_sub(Topic),
            cmkit:log({cmtopic, Name}),
            {ok, #{ name => Name,
                    topic => Topic,
                    services => Services }};
        Other ->
            Other
    end.

handle_call(Msg, _, Data) ->
    cmkit:warning({cmtopic, Data, call, Msg}),
    {reply, ok, Data}.

handle_cast(Msg, Data) ->
    cmkit:warning({cmtopic, Data, cast, Msg}),
    {noreply, Data}.

handle_info({update, Event}, #{ services := Services,
                                topic := T }=Data) ->
    lists:foreach(fun(S) ->
                          case cmservice:run(S, Event) of
                              ok -> 
                                  ok;
                              {error, E} ->
                                  cmkit:warning({cmtopic, T, S, E})
                          end
                  end, Services),
    {noreply, Data}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, Data) ->
    %% TODO leave group
    cmkit:warning({cmtopic, Data, terminated, Reason}),
    ok.
