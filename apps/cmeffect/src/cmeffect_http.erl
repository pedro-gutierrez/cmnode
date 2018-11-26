-module(cmeffect_http).
-export([ effect_info/0,
          effect_apply/2,
          effect_stream/1
        ]).

effect_info() -> http.


effect_apply(#{ context := Context,
                method := Method, 
                url := Url } = Spec, SessionId) ->

    HttpSpec0 = #{ type => http,
                  method => Method,
                  url => url_spec(Url)},
    
    HttpSpec1 = case maps:get(headers, Spec, undef) of 
                    undef -> HttpSpec0;
                    Headers -> 
                        HttpSpec0#{ headers => #{ type => object,
                                                 spec => Headers }}
                end,
    
    HttpSpec2 = case maps:get(query, Spec, undef) of 
                    undef -> HttpSpec1;
                    Query -> 
                        HttpSpec1#{ query => #{ type => object,
                                                 spec => Query }}
                end,

    HttpSpec = case maps:get(body, Spec, undef) of 
                    undef -> HttpSpec2;
                    Body -> 
                        HttpSpec2#{ body => Body }
                end,
    
    Data = case cmencode:encode(#{ type => exec,
                                   spec => HttpSpec}) of 
               {ok, Res} -> Res;
               {error, E} -> #{ error => E }
           end,

    cmcore:update(SessionId, Data#{ context => Context});

effect_apply(#{ stream := Stream } = Q, SessionId) ->
    cmkit:log({cmeffect, http, Q}),
    cmhttp:stream(Q#{ context => #{ data => #{ id => SessionId, 
                                               stream => Stream },
                                    callback => { ?MODULE, effect_stream } }}).

effect_stream(#{ stream := Stream,
                 event := Ev,
                 id := SessionId, 
                 data := Data }=Info) -> 
    cmkit:log({cmeffect, http, Info}),
    cmcore:update(SessionId, #{ stream => Stream,
                                event => Ev,
                                data => Data }).


url_spec(Url) when is_binary(Url) -> Url;
url_spec(Url) when is_map(Url) -> Url#{ type => url}.
