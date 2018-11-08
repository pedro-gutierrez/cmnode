-module(cmeffect_http).
-export([ effect_info/0,
          effect_apply/2,
          effect_stream/1
        ]).

effect_info() -> http.


effect_apply(#{ method := Method, 
                url := Url, 
                context := Context,
                headers := Headers, 
                body := Body }, SessionId) ->
    
    Data = case cmencode:encode(#{ type => exec,
                                   spec => #{ type => http,
                                              method => Method,
                                              url => url_spec(Url),
                                              body => Body,
                                              headers => #{ type => object,
                                                            spec => Headers }}}) of 
               {ok, Res} -> Res;
               {error, E} -> #{ error => E }
           end,

    cmcore:update(SessionId, Data#{ context => Context});

effect_apply(#{ method := _, 
                url := _, 
                context := _,
                body := _ } = Spec, SessionId) ->

    effect_apply(Spec#{ headers => #{}}, SessionId);

effect_apply(#{ method := Method, 
                url := Url,
                headers := Headers,
                context := Context }, SessionId) ->
    
    Data = case cmhttp:Method(Url, Headers) of 
               {ok, In} -> 
                   In;
               {error, E} -> 
                   #{ error => E }
           end,
    
    cmcore:update(SessionId, Data#{ context => Context});

effect_apply(#{ method := Method, 
                url := Url, 
                context := Context }, SessionId) ->
    
    Data = case cmhttp:Method(Url) of 
               {ok, In} -> 
                   In;
               {error, E} -> 
                   #{ error => E }
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
