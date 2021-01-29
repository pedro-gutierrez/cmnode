-module(cmeffect_http).
-export([ effect_info/0,
          effect_apply/2,
          effect_stream/1
        ]).

effect_info() -> http.

effect_apply(#{ context := Context,
                requests := [] }, SessionId) ->

    cmcore:update(SessionId, #{ context => Context,
                                error => no_requests });

effect_apply(#{ context := Context,
                requests := Specs }, SessionId) when is_list(Specs) ->
    case do_specs(Specs, []) of 
        {ok, Resps} ->
            cmcore:update(SessionId, #{ context => Context,
                                        responses => Resps });
        Other ->
            Other
    end;

effect_apply(Spec, SessionId) when is_map(Spec) ->

    case do_spec(Spec) of 
        {ok, Resp} ->
            cmcore:update(SessionId, Resp);
        Other ->
            Other
    end;

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


with_url(Http, Url) when is_binary(Url) -> 
    {ok, Http#{ url => Url }};

with_url(Http, UrlSpec) when is_map(UrlSpec) ->
    case cmencode:encode(#{ type => url,
                            spec => UrlSpec }) of 
        {ok, #{ url := Url }} ->
            {ok, Http#{ url => Url }};
        Other ->
            Other
    end.

with_headers(Http, #{ headers := H }) ->
    {ok, Http#{ headers => H }};

with_headers(Http, _) -> {ok, Http}.

with_query(Http, #{ query := Q }) ->
    {ok, Http#{ query => Q }};

with_query(Http, _) -> {ok, Http}.

with_body(Http, #{ body := B }) ->
    {ok, Http#{ body => B }};

with_body(Http, _) -> {ok, Http}.


record_metric(Method, #{ metric := Metric }, #{ status := Status,
                                                millis := Millis }) ->
    cmmetrics:record_http_duration(Metric, Method, Status, Millis);

record_metric(_, _, _) -> ok. 


do_spec(#{ method := Method,
           url := UrlSpec } = Spec) ->

    Data = case with_url(#{ debug => maps:get(debug, Spec, false),
                            method => Method}, UrlSpec) of 
               {ok, Http0} ->
                   case with_headers(Http0, Spec) of 
                       {ok, Http1} ->
                           case with_query(Http1, Spec) of 
                               {ok, Http2} ->
                                   case with_body(Http2, Spec) of 
                                       {ok, Http3} ->
                                           cmhttp:do(Http3);
                                       Other -> 
                                           Other
                                   end;
                               Other -> 
                                   Other 
                           end;
                       Other ->
                           Other
                   end;
               Other ->
                   Other
           end,
    Context = maps:get(context, Spec, undefined),

    Data2 = case Data of 
                {ok, D} ->
                    record_metric(Method, Spec, D),
                    D#{ context => Context };
                {error, E} ->
                    #{ error => E,
                       context => Context }
            end,
    {ok, Data2}.


do_specs([], Resps) -> {ok, Resps};
do_specs([Spec|Rest], Resps) ->
    case do_spec(Spec) of 
        {ok, Resp} ->
            do_specs(Rest, [Resp|Resps]);
        Other ->
            cmkit:warning({cmeffect, http, Other}),
            do_specs(Rest, Resps)
    end.


