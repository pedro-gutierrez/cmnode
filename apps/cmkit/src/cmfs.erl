-module(cmfs).
-export([stream/1]).

stream(#{ stream := Stream,
          path := Filename,
          context := #{ data := Data,
                        callback := {M, F} } = Context }=Spec) ->
    Probes = lists:map(fun(N) -> 
                               case rpc:call(N, cmkit, file_info, [Filename]) of 
                                   {ok, Info} -> Info#{ node => N,
                                                        status => found };
                                   not_found -> #{ node => N,
                                                   status => not_found };
                                   {error, E} -> 
                                       #{ node => N,
                                          status => error,
                                          reason => E}
                               end

                       end, cmcloud:current_nodes()),

    case aggregate(Probes) of 
        not_found -> 
            cmkit:stream_error(Stream, 404, Data, {M, F});
        {ok, #{ node := N}} -> 
            Spec2 = Spec#{ context => Context#{ callback => {node(), M, F}}},
            rpc:call(N, cmkit, stream_file, [Spec2])
    end.

aggregate([]) -> not_found;
aggregate([#{ status := not_found }|Rem]) -> aggregate(Rem);
aggregate([#{ status := error }|Rem]) -> aggregate(Rem);
aggregate([#{ status := found }=Loc|_]) -> {ok, Loc}.

