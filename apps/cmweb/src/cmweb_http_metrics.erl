-module(cmweb_http_metrics).
-export([init/2]).

init(Req0, State) ->
    record_perf(),
    Headers = cmweb_http: default_headers(),
    Req = cowboy_req:reply(200, Headers#{
                                         <<"content-type">> => <<"text/plain">>
                                        }, prometheus_text_format:format() , Req0),
    {ok, Req, State}.


record_perf() ->
    #{ cpu := #{ average := Cpu }, 
       mem := #{ total := TotalMem, 
                 used := UsedMem }} = cmperf:stats(),

    cmmetrics:set(<<"cmnode_cpu">>, Cpu),
    cmmetrics:set(<<"cmnode_memory_used">>, UsedMem),
    cmmetrics:set(<<"cmnode_memory_available">>, TotalMem).

