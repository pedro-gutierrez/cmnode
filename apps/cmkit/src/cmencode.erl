-module(cmencode).
-export([encode/1, 
         encode/2,
         encode/3,
         encode_all/1,
         encode_all/2,
         encode_all/3
        ]).

encode(Spec) -> encode(Spec, #{}).
encode(Spec, In) -> encode(Spec, In, #{}).


encode(#{ type := object, spec := Spec}, In, Config) ->
    encode_object(Spec, In, Config);
    
encode(#{ type := object }, _, _) ->
    {ok, #{}};

encode(#{ type := data, from := Key}, In, Config) when is_atom(Key) ->
    encode(Key, In, Config);

encode(Key, In, _) when ( is_atom(Key) or is_binary(Key)) ->
    
    case is_map(In) of 
        true ->
            case cmkit:value_at(Key, In) of
               undef ->
                    E = #{ status => missing_key,
                              key => Key,
                              data => In },
                    cmkit:danger({cmencode, E}),
                   
                    {error, E};
               V ->
                   {ok, V}
           end;
        false ->
                    E = #{ status => not_a_map,
                              key => Key,
                              data => In },
                    cmkit:danger({cmencode, E}),
                   
                    {error, E}
    end;

encode(#{ item := Num, in := At }, In, Config) when ( is_atom(At) or is_binary(At) or is_map(At)) ->
    case encode(At, In, Config) of 
        {ok, In2} ->
            case is_list(In2) of 
                false ->
                    {error, #{ status => not_a_list, 
                               at => At }};
                true ->
                    case Num =< length(In2) of 
                        false -> 
                            {error, #{ status => list_too_small,
                                       size => length(In2),
                                       looking_for => Num }};
                        true ->
                            {ok, lists:nth(Num, In2)}
                    end
            end;
        Other -> 
            Other
    end;


encode(#{ key := Key, in := At }=Spec, In, Config) when is_atom(Key) or is_binary(Key) -> 
    case encode(At, In, Config) of 
        {ok, In2} ->
            encode(Key, In2, Config);
        Other -> 
            cmkit:danger({cmencode, error, Spec, Other}),
            Other
    end;

encode(#{ key := KeySpec, in := _ }=Spec, In, Config) when is_map(KeySpec) -> 
    case encode(KeySpec, In, Config) of 
        {ok, Key} ->
            encode(Spec#{ key => Key }, In, Config);
        Other -> 
            cmkit:danger({cmencode, error, Spec, Other}),
            Other
    end;

encode(#{ key := Key}, In, Config) -> 
    encode(Key, In, Config);


encode(#{ type := text, value := Value }, _, _) ->
    {ok, cmkit:to_bin(Value) };


encode(#{ type := number, value:= V }, _, _) when is_number(V) ->
    {ok, V};

encode(#{ type := number, spec := Spec }, In, Config) ->
    case encode(Spec, In, Config) of 
        {ok, V} ->
            case cmkit:to_number(V) of 
                error -> {error, #{ status => encode_error,
                                    spec => Spec,
                                    data => In,
                                    reason => not_a_number }};

                Num -> 
                    {ok, Num}
            end;
        Other -> Other
    end;

encode(#{ type := member,
          spec := #{ value := ValueSpec,
                     in := CollectionSpec }}, In, Config) ->
    case encode(CollectionSpec, In, Config) of
        {ok, List} when is_list(List) ->
            case encode(ValueSpec, In, Config) of
                {ok, Member} ->
                    {ok, lists:member(Member, List)};
                Other ->
                    {error, #{ status => encode_error,
                               spec => ValueSpec,
                               data => In,
                               reason => Other
                             }
                    }
            end;
        Other ->
            {error, #{ status => encode_error,
                       spec => CollectionSpec,
                       data => In,
                       reason => Other
                     }
            }
    end;

encode(#{ type := text,
          spec := Spec }, In, Config) ->
    encode(Spec, In, Config);

encode(#{ type := keyword,
          value := Value }, _, _) when is_atom(Value) ->
    {ok, Value};

encode(#{ type := list,
          value := List }, In, Config) when is_list(List)->
    encode_list(List, In, Config);


encode(#{ type := config,
          spec := Key }, _, Config) ->
    {ok, maps:get(Key, Config)};


encode(#{ type := url,
          host := Host,
          port := Port,
          transport := Transport,
          path := Path }=Spec, In, Config) ->
    case encode(Host, In, Config) of 
        {ok, EncodedHost} ->
            case encode(Port, In, Config) of 
                {ok, EncodedPort} ->
                    case encode(Transport, In, Config) of 
                        {ok, EncodedTransport} ->
                            case encode(Path, In, Config) of 
                                {ok, EncodedPath} ->
                                    BinHost = cmkit:to_bin(EncodedHost),
                                    BinPort = cmkit:to_bin(EncodedPort),
                                    BinTransport = cmkit:to_bin(EncodedTransport),
                                    BinPath = cmkit:to_bin(EncodedPath),
                                    BinQuery = case maps:get(query, Spec, undef) of 
                                                   undef -> {ok, <<>>};
                                                   QuerySpec ->
                                                       case encode(QuerySpec, In, Config) of 
                                                           {ok, Map} ->
                                                               {ok, cmhttp:encodedQs(Map)};
                                                            Other ->
                                                               Other
                                                       end
                                               end,
                                    case BinQuery of 
                                        {ok, QueryString} -> 

                                            {ok, #{ url => <<BinTransport/binary, "://", 
                                                             BinHost/binary, ":",
                                                             BinPort/binary,
                                                             BinPath/binary,
                                                             QueryString/binary>>, 
                                                    transport => EncodedTransport, 
                                                    host => EncodedHost, 
                                                    port => EncodedPort, 
                                                    path => EncodedPath,
                                                    query => QueryString }};
                                        Error ->
                                           Error 
                                    end;
                                Other -> Other
                            end;
                        Other -> Other
                    end;
                Other -> Other
            end;
        Other -> Other
    end;

encode(#{ type := request,
          as := As,
          spec := Spec }, In, Config) ->
    case encode(As, In, Config) of 
        {ok, EncodedAlias} ->
            case encode(Spec, In, Config) of 
                {ok, EncodedSpec} -> 
                    {ok, EncodedSpec#{ as => EncodedAlias }};
                Other -> Other
            end;
        Other -> Other
    end;

encode(#{ type := http,
          method := Method,
          url := Url,
          body := Body,
          headers := Headers }, In, Config) ->
    case encode(Headers, In, Config) of 
        {ok, H} ->
            case encode(Url, In, Config) of 
                {ok, #{ url := U }} ->
                    case encode(Body, In, Config) of 
                        {ok, multipart, B, Boundary} ->
                            {ok, #{ url => U,
                                    method => Method,
                                    headers => H#{ 'content-type' => 
                                        "multipart/form-data; boundary=" 
                                            ++ binary_to_list(Boundary) },
                                    body => B }}; 
                        {ok, B} ->
                            {ok, #{ url => U,
                                    method => Method,
                                    headers => H,
                                    body => B }}; 
                        Other -> Other
                    end;
                U when is_binary(U) -> 
                    case encode(Body, In, Config) of 
                        {ok, multipart, B, Boundary} ->
                            {ok, #{ url => U,
                                    method => Method,
                                    headers => H#{ 'content-type' => 
                                        "multipart/form-data; boundary=" 
                                            ++ binary_to_list(Boundary) },
                                    body => B }}; 
                        {ok, B} ->
                            {ok, #{ url => U,
                                    method => Method,
                                    headers => H,
                                    body => B }}; 
                        Other -> Other
                    end;

                Other -> Other
            end;
        Other -> Other
    end;

encode(#{ type := http,
          method := Method,
          url := Url,
          headers := Headers }, In, Config) ->
    case encode(Headers, In, Config) of 
        {ok, H} ->
            case encode(Url, In, Config) of 
                {ok, #{ url := U }} ->
                            {ok, #{ url => U,
                                    method => Method,
                                        headers => H }}; 
                Url when is_binary(Url) ->
                    {ok, #{ url => Url,
                            method => Method }};
                Other -> 
                    Other
            end;
        Other -> Other
    end;

encode(#{ type := http,
          method := Method,
          url := Url }, In, Config) ->
    case encode(Url, In, Config) of 
        {ok, #{ url := U }} ->
            {ok, #{ url => U,
                    method => Method }}; 
        Url when is_binary(Url) ->
            {ok, #{ url => Url,
                    method => Method }};
        Other -> Other
    end;

encode( #{ type := exec,
           spec := #{ type := http } = Spec}, In, Config) ->
    case encode(Spec, In, Config) of 
        {ok, #{ method := post,
                url := Url,
                headers := Headers,
                body := Body }} ->
                
            cmkit:log({cmencode, http, out, post, Url, Headers, Body}),
            Res = cmhttp:post(Url, Headers, Body),
            cmkit:log({cmencode, http, in, Res}),
            Res;
        
        {ok, #{ method := get, 
                url := Url,
                headers := Headers }} ->
            
            cmkit:log({cmencode, http, out, get, Url, Headers}),
            Res = cmhttp:get(Url, Headers),
            cmkit:log({cmencode, http, in, Res}),
            Res;
        
        {ok, #{ method := get, 
                url := Url }} ->
            
            cmkit:log({cmencode, http, out, get, Url}),
            Res = cmhttp:get(Url),
            cmkit:log({cmencode, http, in, Res}),
            Res;
        
        {ok, U} when is_binary(U) ->
            case cmkit:prefix(U, <<"http">>) of 
                nomatch -> 
                    {error, #{ status => encode_error,
                               spec => Spec,
                               data => In,
                               reason => U}};
                _ -> 


                    cmkit:log({cmencode, http, out, get, U}),
                    Res = cmhttp:get(U),
                    cmkit:log({cmencode, http, in, Res}),
                    Res
            end;

        {ok, Other} ->
            {error, #{ status => encode_error,
                       spec => Spec,
                       data => In,
                       reason => Other }};
        Other -> 
            Other
    end;

encode(#{ type := multipart,
          files := FilesSpec }, In, Config) ->
    case cmencode:encode(FilesSpec, In, Config) of 
        {ok, Files} ->

            Boundary = cmkit:uuid(),
            StartBoundary = erlang:iolist_to_binary([<<"--">>, Boundary]),
            LineSeparator = <<"\r\n">>,
            Data = lists:foldl(fun(#{ name := Name,
                                      mime := Mime, 
                                      filename := Filename,
                                      data := Data }, Acc) ->

                                       erlang:iolist_to_binary([ Acc,
                                                                 StartBoundary, LineSeparator,
                                                                 <<"Content-Disposition: form-data; name=\"">>, Name, <<"\"; filename=\"">>, Filename, <<"\"">>, LineSeparator, 
                                                                 <<"Content-Type: ">>, Mime, LineSeparator, LineSeparator,

                                                                 Data,
                                                                 LineSeparator 
                                                               ])
                               end, <<"">>, Files),
            Data2 = erlang:iolist_to_binary([Data, StartBoundary, <<"--">>, LineSeparator]),
            {ok, multipart, Data2, Boundary};
        Other -> Other
    end;

encode(#{ type := basic_auth,
          spec := Creds }=Spec, In, Config) ->

    case encode(Creds, In, Config) of 
        {ok, #{ username := Username,
                password := Password }} ->

            UsernameBin = cmkit:to_bin(Username),
            PasswordBin = cmkit:to_bin(Password),
            Base64Encoded = base64:encode(<<UsernameBin/binary, ":", PasswordBin/binary>>),
            Value = <<"Basic ", Base64Encoded/binary>>, 
            {ok, Value };

        {ok, Other} ->
            {error, #{ status => encode_error,
                       spec => Spec,
                       data => In,
                       reason => Other
                     }
            };
        Other -> 
            {error, #{ status => encode_error,
                       spec => Spec,
                       data => In,
                       reason => Other
                     }
            }
    end;


encode(#{ height := Height,
          width := Width }, In, Config) -> 
    case cmencode:encode(Height, In, Config) of 
        {ok, H} -> 
            case cmencode:encode(Width, In, Config) of 
                {ok, W} -> 
                    {ok, #{ width => W,
                            height => H }};
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

encode(#{ type := thumbnail,
          url := Url,
          basename := Basename,
          sizes := Sizes }, In, Config) ->
    case cmencode:encode(Url, In, Config) of 
        {ok, U} ->
            case cmencode:encode(Basename, In, Config) of 
                {ok, B} ->
                    case encode_all(Sizes, In, Config) of 
                        {ok, S} ->

                            cmimg:convert(#{ url =>  U, 
                                             basename => B,
                                             dir => cmkit:assets(),
                                             sizes => S });
                        Other -> 
                            Other
                    end;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

encode(#{ type := s3,
          spec := #{ access := Access,
                     secret := Secret,
                     bucket := Bucket,
                     key := Key,
                     data := Data }}, In, Config) -> 
    case cmencode:encode(Access, In, Config) of 
        {ok, A} ->
            case cmencode:encode(Secret, In, Config) of 
                {ok, S} ->
                    case cmencode:encode(Bucket, In, Config) of 
                        {ok, B} ->
                            case cmencode:encode(Key, In, Config) of 
                                {ok, K} ->
                                    case cmencode:encode(Data, In, Config) of 
                                        {ok, D} ->
                                            case cms3:put(#{ access_key => A,
                                                             secret_key => S,
                                                             bucket => B,
                                                             key => K,
                                                             data => D }) of 
                                                ok -> {ok, ok};
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
                    end;
                Other ->
                    Other
            end;
        Other -> 
            Other
    end;


encode(#{ type := erlang,
          mod := M,
          function := F,
          args := Args }, In, Config) -> 
    case cmencode:encode(Args, In, Config) of 
        {ok, A} -> 
            case apply(M, F, A) of 
                {ok, Data} -> {ok, Data};
                {error, E} -> {error, E};
                Other -> {ok, Other}
            end;
        Other -> 
            Other
    end;

encode(#{ type := path,
          location := Path }, In, Config) -> 
    case cmencode:encode(Path, In, Config) of 
        {ok, P} ->
            {ok, cmkit:to_list(P)};
        Other -> 
            {error, #{ status => encode_error,
                       spec => Path,
                       reason => Other }}
    end;

encode(#{ type := file,
          spec := #{ path := Path, 
                     data := Data }}, In, Config) ->
    case encode(Path, In, Config) of 
        {ok, P} ->
            case encode(Data, In, Config) of 
                {ok, D} ->
                    case file:write_file(cmkit:to_list(P), D) of
                        ok -> {ok, ok};
                        Other -> 
                            Other
                    end;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;


encode(#{ type := file,
          spec := Spec }, In, Config) ->
    case encode(Spec, In, Config) of 
        {ok, Path} ->
            file:read_file(Path);
        Other -> 
            {error, #{ status => encode_error,
                       spec => Spec,
                       reason => Other }}
    end;

encode(#{ type := base64,
          spec := Spec }, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, Data} ->
            {ok, base64:encode(Data)};
        Other -> 
            {error, #{ status => encode_error,
                       spec => Spec,
                       reason => Other }}
    end;

encode(#{ type := asset,
          spec := Spec }, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, Name} ->
            cmkit:read_file(filename:join(cmkit:assets(), Name));
        Other -> Other
    end;

encode(#{ type := equal, 
          spec := Specs }, In, Config) when is_list(Specs) -> 
    
    {ok, all_equal(lists:map(fun(Spec) ->
                                encode(Spec, In, Config)
                        end, Specs))};

encode(#{ type := greater_than,
          spec := [Spec1, Spec2] }, In, Config) ->
    case { encode(Spec1, In, Config), encode(Spec2, In, Config) } of 
        { {ok, V1}, {ok, V2} } when (is_number(V1) and is_number(V2)) ->
            {ok, V1 > V2};
        Other -> Other
    end;

encode(#{ type := sum,
          spec := Specs } = Spec, In, Config) ->
    Res = lists:foldl(fun({ok, V}, Total) when is_number(V) -> 
                        Total + V;
                   (Other, _) -> 
                        {error, #{ status => encode_error,
                                   spec => Spec,
                                   data => Other,
                                   reason => not_a_number }}
                end, 0, lists:map(fun(S) ->
                                       encode(S, In, Config)
                               end, Specs)),
    case Res of 
        N when is_number(N) -> {ok, N};
        Other -> Other
    end;

encode(#{ type := join,
          terms := Specs }=Spec, In, Config) ->

    case encode_all(Specs, In, Config) of 
        {ok, EncodedTerms} ->
            {ok, cmkit:bin_join(EncodedTerms)};
        Other -> 
            fail_encoding(Spec, In, Other)
    end;

encode(#{ type := format, 
          pattern := PatternSpec,
          params := ParamsSpec }, In, Config) -> 

    case cmencode:encode(PatternSpec, In, Config) of 
        {ok, Pattern} ->
            case encode_all(ParamsSpec, In, Config) of 
                {ok, Params} ->
                    {ok, cmkit:fmt(Pattern, Params)};
                Other -> Other
            end;
        Other -> Other
    end;

encode(#{ type := format, 
          pattern := _,
          date := DateSpec }, In, Config) -> 

    case encode(DateSpec, In, Config) of 
        {ok, Date} ->
            case cmkit:format_date(Date) of 
                invalid -> {error, #{ date => Date,
                                    spec => DateSpec,
                                    error => cannot_format_date }};
                Bin -> 
                    {ok, Bin}
            end;
        Other -> Other
    end;

encode(#{ type := utc,
          amount := AmountSpec,
          factor := Factor,
          tense := Tense }, In, Config) -> 

    case encode(AmountSpec, In, Config) of 
        {ok, Amount} ->
            Now = calendar:universal_time(),
            Secs = case Tense of 
                       past -> Amount * Factor * -1;
                       future -> Amount * Factor
                   end,
            
            {ok, cmcalendar:add_seconds_to_utc(Secs, Now)};
        Other -> 
            Other
    end;

encode(#{ type := wait,
          spec := #{ retries := Retries,
                     sleep := Sleep,
                     condition := Condition }}, In, Config) ->
    
    encode_retry(Retries, Sleep, Condition, In, Config);

encode(#{ type := wait,
          spec := #{ sleep := Sleep }}, _, _) ->

    cmkit:log({cmencode, wait, Sleep }),
    timer:sleep(Sleep),
    cmkit:log({cmencode, sleep, resuming}),
    {ok, Sleep};

encode(#{ type := match,
          spec := #{ value := ValueSpec,
                     decoder := DecoderSpec } = MatchSpec }, In, Config) ->
    case cmencode:encode(ValueSpec, In, Config) of
        {ok, Value} ->
            case cmdecode:decode(DecoderSpec, Value) of 
                {ok, Decoded} -> 
                    case maps:get(map, MatchSpec, undef) of 
                        undef -> 
                            {ok, true};
                        MapSpec -> 
                            cmencode:encode(MapSpec, Decoded)
                    end;
                _ ->
                    {ok, false}
            end;
        Other ->
            Other
    end;

encode(#{ type := find,
          items := ItemsSpec, 
          target := TargetSpec}, In, Config) -> 

    case cmencode:encode(ItemsSpec, In, Config) of
        {ok, Items} ->
            case cmdecode:decode(#{ type => first,
                                    spec => TargetSpec }, Items, Config) of 
                {ok, _} -> {ok, true};
                _ -> {ok, false}
            end;
        Other ->
            Other
    end;

encode(#{ type := iterate, 
          source := SourceSpec,
          filter := FilterSpec,
          dest := DestSpec }, In, Config) -> 

    case cmencode:encode(SourceSpec, In, Config) of 
        {ok, Source} when is_list(Source) -> 
            Source2 = case FilterSpec of 
                          none -> Source;
                          _ -> 
                              lists:filter(fun(Item) -> 
                                                   case cmdecode:decode(FilterSpec, Item) of
                                                       {ok, _} -> true;
                                                       _ -> false
                                                    end
                                           end, Source)
                        end,
            map(DestSpec, In, Config, Source2);
        Other -> 
            Other
    end;


encode(#{ type := merge,
          spec := Specs } = Spec, In, Config) -> 
    case encode_all(Specs, In, Config) of 
        {ok, EncodedTerms} ->
            {ok, lists:foldl(fun(M, Acc) -> 
                                     maps:merge(Acc, M)
                             end, #{}, EncodedTerms) };
        Other -> 
            fail_encoding(Spec, In, Other)
    end;

encode(#{ type := connect,
          as := As,
          spec := Spec }=Spec0, In, Config) -> 
    case encode(As, In, Config) of 
        {ok, Name} ->
            case encode(Spec, In, Config) of 
                {ok, #{ host := _,
                        port := _,
                        transport := Transport,
                        path := _ } = Config0 } ->
                    
                    Config1 = Config0#{ debug => true,
                                        persistent => false },
                    
                    Config2 = case maps:get(protocol, Spec0, undef) of
                                  undef -> Config1;
                                  Protocol ->
                                      Config1#{ protocol => Protocol }
                              end,

                    case cmkit:to_bin(Transport) of 
                        T when T =:= <<"ws">> orelse T =:= <<"wss">> ->
                            Url = cmkit:url(Config2),
                            {ok, Pid } = cmtest_ws_sup:new(Name, Config2, self()),
                            {ok, #{ connection => Config2#{ name => Name,
                                                            transport => Transport,
                                                            class => websocket,
                                                            pid => Pid,
                                                            status => undef,
                                                            inbox => [],
                                                            url => Url }}};

                        T when T =:= <<"http">> orelse T =:= <<"https">> ->
                            Url = cmkit:url(Config2),
                            Res = cmhttp:get(Url),
                            Status = case Res of
                                         {ok, _} -> up;
                                         {error, S} -> S
                                     end,
                            {ok, #{ connection => Config2#{ name => Name,
                                                            transport => Transport,
                                                            class => http,
                                                            status => Status,
                                                            inbox => [],
                                                            url => Url }}}
                    end;
                Other ->
                    Other
            end;
        Other ->
            Other
    end;

encode(#{ type := send,
          spec := #{ to := ConnSpec, 
                     spec := Spec }}, #{ conns := Conns }=In, _) -> 
    case cmencode:encode(ConnSpec, In) of 
        {ok, Name} ->
            case maps:get(Name, Conns, undef) of
                undef ->
                    {error, #{ error => no_such_connection,
                               info => Name}};

                Conn ->
                    case cmencode:encode(Spec, In) of
                        {ok, Encoded } ->
                            case Conn of
                                #{  class := websocket, pid := Pid } ->
                                    cmwsc:send(Pid, Encoded),
                                    {ok, #{ connection => Conn#{ inbox => []}}};

                                #{ class := http, transport := Transport, url := Url } ->
                                    case Encoded of
                                        #{ body := Body,
                                           headers := Headers } ->

                                            cmkit:log({cmencode, Transport, out, Url, Headers, Body}),
                                            {_, Res} = cmhttp:post(Url, Headers, Body),
                                            cmkit:log({cmencode, Transport, in, Res}),
                                            {ok, #{ connection => Conn#{ inbox => [Res]}}};

                                        Other -> 
                                            {error, #{ error => wrong_http_request,
                                                       info => Other } }
                                    end;
                                Other ->
                                    { error, #{ error => connection_class_not_supported,
                                                info => Other }}
                            end;
                        Other -> 
                            Other
                    end
            end;
        Other -> 
            Other
    end;

encode(#{ type := kube,
          spec := #{ name := NameSpec,
                     namespace := NsSpec,
                     resource := Resource,
                     server := ServerSpec,
                     state := StateSpec } = Spec }, In, Config) ->

    case encode(NameSpec, In, Config) of 
        {ok, Name} ->
            case encode(NsSpec, In, Config) of 
                {ok, Ns} ->
                    case encode(ServerSpec, In, Config) of 
                        {ok, Server} ->
                            case encode(StateSpec, In, Config) of 
                                {ok, State} ->
                                    Params = #{ name => Name,
                                                namespace => Ns,
                                                resource => Resource,
                                                state => State,
                                                server => Server },
                                    case maps:get(props, Spec, undef) of 
                                        undef ->
                                            cmkube:do(Params);
                                        PropsSpec ->
                                            case encode(PropsSpec, In, Config) of 
                                                {ok, Props} ->
                                                    cmkube:do(Params#{ props => Props });
                                                Other ->
                                                    Other
                                            end
                                    end;
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
    end;
                     
encode(#{ spec := Spec }, _, _) -> {ok, Spec}.

encode_object(Spec, In, Config) ->
    encode_object(maps:keys(Spec), Spec, In, Config, #{}).

encode_object([], _, _,_, Out) -> {ok, Out};
encode_object([K|Rem], Spec, In, Config, Out) ->
    case encode(maps:get(K, Spec), In, Config) of 
        {ok, V} ->
            encode_object(Rem, Spec, In, Config, Out#{ K => V });
        Other -> Other
    end.

encode_list(Specs, In, Config) ->
    encode_list(Specs, In, Config, []).

encode_list([], _, _, Out) -> {ok, lists:reverse(Out)};
encode_list([Spec|Rem], In, Config, Out) ->
    case encode(Spec, In, Config) of 
        {ok, Encoded} ->
            encode_list(Rem, In, Config, [Encoded|Out]);
        Other -> 
            fail_encoding(Spec, In, Other)
    end.

map(Dest, In, Config, Source) -> 
    map(Dest, In, Config, Source, []).

map(_, _, _, [], Out) ->  {ok, lists:reverse(Out)};
map(DestSpec, In, Config, [Item|Rem], Out) -> 
    case cmencode:encode(DestSpec, In#{ item =>  Item}, Config) of 
        {ok, Encoded} ->
            map(DestSpec, In, Config, Rem, [Encoded|Out]);
        Other -> 
            Other
    end.


fail_encoding(Spec, In, Out) ->
    {error, #{ status => encode_error,
               spec => Spec,
               data => In,
               reason => Out 
             }
    }.

encode_all(Specs) -> encode_all(Specs, #{}, #{}).
encode_all(Specs, In) -> encode_all(Specs, In, #{}).

encode_all(Specs, In, Config) ->
    encode_all(Specs, In, Config, []).

encode_all([], _, _, Out) -> {ok, lists:reverse(Out)};
encode_all([Spec|Rem], In, Config, Out) ->
    case encode(Spec, In, Config) of 
        {ok, Encoded} ->
            encode_all(Rem, In, Config, [Encoded|Out]);
        Other ->
            Other
    end.


encode_retry(0, _, Condition, In, _) -> 
    {error, #{ status => encode_error,
               spec => Condition,
               data => In,
               reason => max_retries_reached 
             }
    };

encode_retry(Retries, Millis, Condition, In, Config) -> 
    cmkit:log({cmencode, sleeping, Millis}),
    timer:sleep(Millis),
    cmkit:log({cmencode, trying, Condition, In}),
    case encode(Condition, In, Config) of 
        {ok, true} -> {ok, true};
        _ -> 
            encode_retry(Retries-1, Millis, Condition, In, Config)
    end.

all_equal([V|Rem]) -> all_equal(Rem, V).
all_equal([], _) -> true;
all_equal([V|Rem], V) -> all_equal(Rem, V);
all_equal(_, _) -> false.
