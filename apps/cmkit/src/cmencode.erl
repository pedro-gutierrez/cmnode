-module(cmencode).
-export([encode/1, 
         encode/2,
         encode/3]).

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

encode(#{ key := Key, in := At }, In, Config) when is_atom(Key) and ( is_atom(At) or is_binary(At))-> 
    case encode(At, In, Config) of 
        {ok, In2} ->
            encode(Key, In2, Config);
        Other -> 
            Other
    end;

encode(#{ key := Key, in := At }, In, Config) when is_atom(Key) and is_map(At) -> 
    case encode(At, In, Config) of 
        {ok, In2} ->
            encode(Key, In2, Config);
        Other -> 
            Other
    end;

encode(#{ key := Key}, In, Config) when is_atom(Key) -> 
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
          path := Path }, In, Config) ->
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

                                    {ok, #{ url => <<BinTransport/binary, "://", 
                                                     BinHost/binary, ":",
                                                     BinPort/binary,
                                                     BinPath/binary >>, 
                                            transport => EncodedTransport, 
                                            host => EncodedHost, 
                                            port => EncodedPort, 
                                            path => EncodedPath }};
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
        Other -> Other
    end;

encode( #{ type := exec,
           spec := #{ type := http } = Spec}, In, Config) ->
    case encode(Spec, In, Config) of 
        {ok, #{ method := post,
                url := Url,
                headers := Headers,
                body := Body }} ->
                cmhttp:post(Url, Headers, Body);
        {ok, #{ method := get, 
                url := Url,
                headers := Headers }} ->
            cmhttp:get(Url, Headers);
        {ok, #{ method := get, 
                url := Url }} ->
            cmhttp:get(Url);
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

encode(#{ type := path,
          location := Path }, _, _) -> {ok, cmkit:to_list(Path)};

encode(#{ type := file,
          spec := Spec }, In, Config) ->
    case encode(Spec, In, Config) of 
        {ok, Path} ->
            file:read_file(Path);
        Other -> 
            Other
    end;

encode(#{ type := base64,
          spec := Spec }, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, Data} ->
            {ok, base64:encode(Data)};
        Other -> 
            Other
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

encode(#{ type := wait,
          spec := #{ retries := Retries,
                     sleep := Sleep,
                     condition := Condition }}, In, Config) ->
    
    encode_retry(Retries, Sleep, Condition, In, Config);

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

fail_encoding(Spec, In, Out) ->
    {error, #{ status => encode_error,
               spec => Spec,
               data => In,
               reason => Out 
             }
    }.

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
