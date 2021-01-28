-module(cmencode).

-export([encode/1, encode/2, encode/3, encode_all/1, encode_all/2, encode_all/3]).

encode(Spec) ->
    encode(Spec, #{}).

encode(Spec, In) ->
    encode(Spec, In, #{}).

encode(Data, _, _) when is_binary(Data) or is_number(Data) ->
    {ok, Data};
encode(#{literal := Value}, _, _) ->
    {ok, Value};
encode(#{maybe := Spec}, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, V} ->
            {ok, V};
        _ ->
            {ok, null}
    end;
encode(Map, _, _) when is_map(Map) andalso map_size(Map) =:= 0 ->
    {ok, #{}};
encode(#{type := object,
         mode := Mode,
         spec := Spec} =
           Spec0,
       In,
       Config) ->
    case with_where(Spec0, In, Config) of
        {ok, In2} ->
            encode_object(Spec, In2, Config, Mode);
        Other ->
            Other
    end;
encode(#{type := object, spec := Spec} = Spec0, In, Config) ->
    case with_where(Spec0, In, Config) of
        {ok, In2} ->
            encode_object(Spec, In2, Config, strict);
        Other ->
            Other
    end;
encode(#{type := object}, _, _) ->
    {ok, #{}};
encode(#{type := data, from := Key}, In, Config) when is_atom(Key) ->
    encode(Key, In, Config);
encode(#{type := size_of, spec := Spec}, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, M} when is_map(M) ->
            {ok, map_size(M)};
        {ok, L} when is_list(L) ->
            {ok, length(L)};
        {ok, Other} ->
            {error,
             #{reason => not_supported,
               spec => Spec,
               type => size_of,
               value => Other}};
        Other ->
            Other
    end;
encode(#{item := Num, in := At}, In, Config)
  when is_atom(At) or is_binary(At) or is_map(At) ->
    case encode(#{key => At}, In, Config) of
        {ok, In2} ->
            case is_list(In2) of
                false ->
                    {error, #{status => not_a_list, at => At}};
                true ->
                    case Num =< length(In2) of
                        false ->
                            {error,
                             #{status => list_too_small,
                               size => length(In2),
                               looking_for => Num}};
                        true ->
                            {ok, lists:nth(Num, In2)}
                    end
            end;
        Other ->
            Other
    end;
encode(#{type := keys,
         spec := KeysSpec,
         in := InSpec} =
           Spec,
       In,
       Config) ->
    case encode(KeysSpec, In, Config) of
        {ok, Keys} ->
            case encode(InSpec, In, Config) of
                {ok, Map} when is_map(Map) ->
                    encode_keys(Keys, Map);
                {ok, Other} ->
                    encode_error(not_a_map, #{spec => Spec, encoded => Other});
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
encode(#{key := Key, in := At}, In, Config) when is_atom(Key) or is_binary(Key) ->
    case encode(#{key => At}, In, Config) of
        {ok, In2} ->
            encode(#{key => Key}, In2, Config);
        Other ->
            Other
    end;
encode(#{key := Key} = Spec, In, Config)
  when is_binary(Key) or is_atom(Key) andalso is_map(In) ->
    case cmkit:value_at(Key, In) of
        undef ->
            case maps:get(default, Spec, undef) of
                undef ->
                    E = #{error => missing_key,
                          key => Key,
                          keys => maps:keys(In)},
                    {error, E};
                Default ->
                    encode(Default, In, Config)
            end;
        V ->
            {ok, V}
    end;
encode(#{key := KeySpec} = Spec, In, Config) when is_map(KeySpec) ->
    case encode(KeySpec, In, Config) of
        {ok, Key} when is_binary(Key) or is_atom(Key) ->
            encode(Spec#{key => Key}, In, Config);
        {ok, Map} when is_map(Map) ->
            {ok, Map};
        Other ->
            Other
    end;
encode(#{key := _} = Spec, In, _) ->
    E = #{status => invalid_key_spec,
          spec => Spec,
          in => In},
    cmkit:danger({cmencode, E}),
    {error, E};
encode(#{type := is_set, spec := Spec}, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, _} ->
            {ok, true};
        _ ->
            {ok, false}
    end;
encode(#{type := one_of, specs := Specs}, In, Config) when is_list(Specs) ->
    encode_first(Specs, Specs, In, Config);
encode(#{type := one_of, specs := Specs} = Spec, _, _) ->
    fail(Spec, Specs, not_a_list);
encode(#{one_of := Specs}, In, Config) ->
    encode(#{type => one_of, specs => Specs}, In, Config);
encode(#{type := either, options := Opts}, In, Config) ->
    encode_options(Opts, Opts, In, Config);
encode(#{type := condition,
         condition := #{type := true},
         spec := Spec},
       In,
       Config) ->
    encode(Spec, In, Config);
encode(#{type := condition,
         condition := #{type := false},
         spec := _},
       _,
       _) ->
    {error, condition_not_satisfied};
encode(#{type := condition,
         condition := C,
         spec := Spec} =
           Expr,
       In,
       Config) ->
    case encode(C, In, Config) of
        {ok, true} ->
            encode(Spec, In, Config);
        {ok, false} ->
            {error, condition_not_satisfied};
        {ok, Other} ->
            {error,
             #{error => unexpected_result_from_condition,
               want => [true, false],
               got => Other,
               expression => Expr}};
        Other ->
            Other
    end;
encode(#{type := 'case',
         spec := Case,
         'of' := Of} =
           Spec,
       In,
       Config) ->
    case with_where(Spec, In, Config) of
        {ok, In2} ->
            case encode(Case, In2, Config) of
                {ok, Expr} ->
                    encode_case_clause(Of, Expr, In2, Config);
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
encode(#{type := text, spec := Spec} = Spec0, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, V} when is_list(V) or is_number(V) ->
            {ok, cmkit:to_bin(V)};
        {ok, V} when is_binary(V) ->
            {ok, V};
        {ok, Spec2} when is_map(Spec2) ->
            encode(Spec2, In, Config);
        {ok, Other} ->
            {error,
             #{error => encode_error,
               reason => not_supported,
               spec => Spec0,
               info => Other}};
        Other ->
            Other
    end;
encode(#{type := text, value := Value}, _, _) when is_binary(Value) ->
    {ok, Value};
encode(#{type := text, value := Value}, _, _) when is_number(Value) ->
    {ok, cmkit:to_bin(Value)};
encode(#{type := text, value := Spec}, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, V} ->
            {ok, cmkit:to_bin(V)};
        Other ->
            Other
    end;
encode(#{type := capitalized, spec := Spec}, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, V} when is_binary(V) ->
            {ok, cmkit:capitalize(V)};
        Other ->
            Other
    end;
encode(#{type := number, value := V}, _, _) when is_number(V) ->
    {ok, V};
encode(#{type := number, spec := Spec}, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, V} ->
            case cmkit:to_number(V) of
                error ->
                    {error,
                     #{status => encode_error,
                       spec => Spec,
                       data => In,
                       reason => not_a_number}};
                Num ->
                    {ok, Num}
            end;
        Other ->
            Other
    end;
encode(#{type := os,
         name := VarSpec,
         default := DefaultSpec},
       In,
       Config) ->
    case encode(VarSpec, In, Config) of
        {ok, V} ->
            case encode(DefaultSpec, In, Config) of
                {ok, D} ->
                    {ok,
                     cmkit:to_bin(
                       os:getenv(
                         cmkit:to_list(V), cmkit:to_list(D)))};
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
encode(#{type := os, name := VarSpec} = Spec, In, Config) ->
    case encode(VarSpec, In, Config) of
        {ok, V} ->
            case os:getenv(
                   cmkit:to_list(V))
            of
                false ->
                    {error,
                     #{status => encode_error,
                       spec => Spec,
                       reason => no_such_env_var}};
                Value ->
                    {ok, cmkit:to_bin(Value)}
            end;
        Other ->
            Other
    end;
encode(#{type := member, spec := #{value := ValueSpec, in := CollectionSpec}},
       In,
       Config) ->
    case encode(CollectionSpec, In, Config) of
        {ok, List} when is_list(List) ->
            case encode(ValueSpec, In, Config) of
                {ok, Member} ->
                    {ok, lists:member(Member, List)};
                Other ->
                    {error,
                     #{status => encode_error,
                       spec => ValueSpec,
                       data => In,
                       reason => Other}}
            end;
        Other ->
            {error,
             #{status => encode_error,
               spec => CollectionSpec,
               data => In,
               reason => Other}}
    end;
encode(#{type := keyword, value := no}, _, _) ->
    {ok, false};
encode(#{type := keyword, value := yes}, _, _) ->
    {ok, true};
encode(#{type := keyword, value := Value}, _, _) when is_atom(Value) ->
    {ok, Value};
encode(#{type := keyword, spec := Spec}, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, V} ->
            {ok, cmkit:to_atom(V)};
        Other ->
            Other
    end;
encode(#{type := list, value := List}, In, Config) when is_list(List) ->
    encode_list(List, In, Config);
encode(#{type := flatten, spec := Items} = S, In, Config) ->
    case encode(Items, In, Config) of
        {ok, EncodedItems} when is_list(EncodedItems) ->
            {ok, lists:flatten(EncodedItems)};
        {ok, Other} ->
            {error,
             #{error => not_a_list,
               spec => S,
               value => Other}};
        Other2 ->
            Other2
    end;
encode(#{type := config, spec := Spec}, In, Config) when is_map(Spec) ->
    In2 = case is_map(In) of
              true ->
                  maps:merge(In, Config);
              false ->
                  Config
          end,
    case encode(Spec, In2) of
        {ok, V} ->
            {ok, V};
        Other ->
            {error,
             #{status => config_error,
               spec => Spec,
               reason => Other}}
    end;
encode(#{type := url,
         spec :=
             #{host := Host,
               port := Port,
               transport := Transport,
               path := Path} =
             Spec},
       In,
       Config) ->
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
                                    BinQuery =
                                        case maps:get(query, Spec, undef) of
                                            undef ->
                                                {ok, <<>>};
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
                                            {ok,
                                             #{url =>
                                                   <<BinTransport/binary,
                                                     "://",
                                                     BinHost/binary,
                                                     ":",
                                                     BinPort/binary,
                                                     BinPath/binary,
                                                     QueryString/binary>>,
                                               transport => EncodedTransport,
                                               host => EncodedHost,
                                               port => EncodedPort,
                                               path => EncodedPath,
                                               query => QueryString}};
                                        Error ->
                                            Error
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
encode(#{type := url, spec := URI} = Spec, _, _) when is_binary(URI) ->
    case http_uri:parse(URI) of
        {ok, {Transport, UserInfo, Host, Port, Path, Query}} ->
            {ok,
             #{url => URI,
               transport => Transport,
               user_info => UserInfo,
               host => Host,
               port => Port,
               path => Path,
               query => Query}};
        {ok, Other} ->
            {error,
             #{status => encode_error,
               spec => Spec,
               reason => not_supported,
               value => Other}};
        {error, E} ->
            {error,
             #{status => encode_error,
               spec => Spec,
               reason => E}}
    end;
encode(#{type := url, spec := Spec}, In, Config) ->
    case encode(Spec, In, Config) of
        {ok,
         #{host := _,
           port := _,
           transport := _,
           path := _} =
             Url} ->
            encode(#{type => url, spec => Url}, In, Config);
        Other ->
            Other
    end;
encode(#{type := request,
         as := As,
         spec := Spec},
       In,
       Config) ->
    case encode(As, In, Config) of
        {ok, EncodedAlias} ->
            case encode(Spec, In, Config) of
                {ok, EncodedSpec} ->
                    {ok, EncodedSpec#{as => EncodedAlias}};
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
encode(#{type := http,
         method := Method,
         url := Url,
         body := Body} =
           Spec,
       In,
       Config) ->
    H = case maps:get(headers, Spec, undef) of
            undef ->
                #{};
            H0 ->
                H0
        end,

    Q = case maps:get(query, Spec, undef) of
            undef ->
                #{};
            Q0 ->
                Q0
        end,

    D = maps:get(debug, Spec, false),

    case encode(Method, In, Config) of
        {ok, M} ->
            encode_http(M, Url, Q, H, D, Body, In, Config);
        Other ->
            Other
    end;
encode(#{type := http,
         method := Method,
         url := Url} =
           Spec,
       In,
       Config) ->
    H = case maps:get(headers, Spec, undef) of
            undef ->
                #{};
            H0 ->
                H0
        end,

    Q = case maps:get(query, Spec, undef) of
            undef ->
                #{};
            Q0 ->
                Q0
        end,

    D = maps:get(debug, Spec, false),

    case encode(Method, In, Config) of
        {ok, M} ->
            encode_http(M, Url, Q, H, D, In, Config);
        Other ->
            Other
    end;
encode(#{type := http} = Spec, _, _) ->
    encode_error(unsupported_http, Spec);
encode(#{type := exec, spec := #{type := http} = Spec}, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, #{url := Url} = Req} when is_binary(Url) ->
            cmhttp:do(Req);
        {ok, U} when is_binary(U) ->
            case cmkit:prefix(U, <<"http">>) of
                nomatch ->
                    {error,
                     #{status => encode_error,
                       spec => Spec,
                       data => In,
                       reason => U}};
                _ ->
                    cmhttp:do(#{url => U})
            end;
        {ok, Other} ->
            {error,
             #{status => encode_error,
               spec => Spec,
               data => In,
               reason => Other}};
        Other ->
            Other
    end;
encode(#{type := multipart, files := FilesSpec}, In, Config) ->
    case cmencode:encode(FilesSpec, In, Config) of
        {ok, Files} ->
            Boundary = cmkit:uuid(),
            StartBoundary = erlang:iolist_to_binary([<<"--">>, Boundary]),
            LineSeparator = <<"\r\n">>,
            Data =
                lists:foldl(fun(#{name := Name,
                                  mime := Mime,
                                  filename := Filename,
                                  data := Data},
                                Acc) ->
                                    erlang:iolist_to_binary([Acc,
                                                             StartBoundary,
                                                             LineSeparator,
                                                             <<"Content-Disposition: form-data; name=\"">>,
                                                             Name,
                                                             <<"\"; filename=\"">>,
                                                             Filename,
                                                             <<"\"">>,
                                                             LineSeparator,
                                                             <<"Content-Type: ">>,
                                                             Mime,
                                                             LineSeparator,
                                                             LineSeparator,
                                                             Data,
                                                             LineSeparator])
                            end,
                            <<"">>,
                            Files),
            Data2 = erlang:iolist_to_binary([Data, StartBoundary, <<"--">>, LineSeparator]),
            {ok, multipart, Data2, Boundary};
        Other ->
            Other
    end;
encode(#{type := basic_auth, spec := Creds} = Spec, In, Config) ->
    case encode(Creds, In, Config) of
        {ok, #{username := Username, password := Password}} ->
            UsernameBin = cmkit:to_bin(Username),
            PasswordBin = cmkit:to_bin(Password),
            Base64Encoded = base64:encode(<<UsernameBin/binary, ":", PasswordBin/binary>>),
            Value = <<"Basic ", Base64Encoded/binary>>,
            {ok, Value};
        {ok, Other} ->
            {error,
             #{status => encode_error,
               spec => Spec,
               data => In,
               reason => Other}};
        Other ->
            {error,
             #{status => encode_error,
               spec => Spec,
               data => In,
               reason => Other}}
    end;
encode(#{type := uuid}, _, _) ->
    {ok, cmkit:uuid()};
encode(#{type := slack,
         spec :=
             #{enabled := Enabled,
               token := Token,
               channel := Channel,
               subject := Subject,
               severity := Severity,
               body := Body}},
       In,
       Config) ->
    case encode(Enabled, In, Config) of
        {ok, E} ->
            case encode(Token, In, Config) of
                {ok, T} ->
                    case encode(Channel, In, Config) of
                        {ok, Ch0} ->
                            Ch = <<"#", Ch0/binary>>,
                            case encode(Subject, In, Config) of
                                {ok, S} ->
                                    case encode(Body, In, Config) of
                                        {ok, B} ->
                                            case encode(Severity, In, Config) of
                                                {ok, Sev} ->
                                                    case E of
                                                        false ->
                                                            cmkit:log({slack,
                                                                       disabled,
                                                                       Ch,
                                                                       Sev,
                                                                       S,
                                                                       B});
                                                        true ->
                                                            cmkit:log({slack,
                                                                       enabled,
                                                                       Ch,
                                                                       Sev,
                                                                       S,
                                                                       B}),
                                                            cmslack:Sev(#{token => T,
                                                                          channel => Ch,
                                                                          subject => S,
                                                                          text => B})
                                                    end,
                                                    {ok, E};
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
encode(#{height := Height, width := Width}, In, Config) ->
    case cmencode:encode(Height, In, Config) of
        {ok, H} ->
            case cmencode:encode(Width, In, Config) of
                {ok, W} ->
                    {ok, #{width => W, height => H}};
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
encode(#{type := thumbnail,
         url := Url,
         basename := Basename,
         sizes := Sizes},
       In,
       Config) ->
    case cmencode:encode(Url, In, Config) of
        {ok, U} ->
            case cmencode:encode(Basename, In, Config) of
                {ok, B} ->
                    case encode_all(Sizes, In, Config) of
                        {ok, S} ->
                            cmimg:convert(#{url => U,
                                            basename => B,
                                            dir => cmkit:assets(),
                                            sizes => S});
                        Other ->
                            Other
                    end;
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
encode(#{type := s3,
         spec :=
             #{access := Access,
               secret := Secret,
               bucket := Bucket,
               key := Key,
               data := Data}},
       In,
       Config) ->
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
                                            case cms3:put(#{access_key => A,
                                                            secret_key => S,
                                                            bucket => B,
                                                            key => K,
                                                            data => D})
                                            of
                                                ok ->
                                                    {ok, ok};
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
encode(#{type := git, spec := #{action := pull, dir := Dir}}, In, Config) ->
    case cmencode:encode(Dir, In, Config) of
        {ok, D} ->
            cmgit:pull(D);
        Other ->
            Other
    end;
encode(#{type := erlang,
         mod := M,
         function := F,
         args := Args},
       In,
       Config) ->
    case cmencode:encode(Args, In, Config) of
        {ok, A} ->
            case apply(M, F, A) of
                {ok, Data} ->
                    {ok, Data};
                {error, E} ->
                    {error, E};
                Other ->
                    {ok, Other}
            end;
        Other ->
            Other
    end;
encode(#{type := path, location := Path}, In, Config) ->
    case cmencode:encode(Path, In, Config) of
        {ok, P} ->
            {ok, cmkit:to_list(P)};
        Other ->
            {error,
             #{status => encode_error,
               spec => Path,
               reason => Other}}
    end;
encode(#{type := file, spec := #{path := Path, data := Data}}, In, Config) ->
    case encode(Path, In, Config) of
        {ok, P} ->
            case encode(Data, In, Config) of
                {ok, D} ->
                    case file:write_file(
                           cmkit:to_list(P), D)
                    of
                        ok ->
                            {ok, ok};
                        Other ->
                            Other
                    end;
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
encode(#{type := file, spec := Spec}, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, Path} ->
            file:read_file(Path);
        Other ->
            {error,
             #{status => encode_error,
               spec => Spec,
               reason => Other}}
    end;
encode(#{type := base64, spec := Spec}, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, Data} ->
            {ok, base64:encode(Data)};
        Other ->
            {error,
             #{status => encode_error,
               spec => Spec,
               reason => Other}}
    end;
encode(#{type := hex, spec := Spec}, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, Value} when is_binary(Value); is_list(Value) ->
            {ok, cmkit:hex(Value)};
        {ok, Other} ->
            fail(Spec, Other, not_a_list_or_binary);
        Other ->
            Other
    end;
encode(#{type := json, spec := JsonSpec}, In, Config) ->
    case encode(JsonSpec, In, Config) of
        {ok, Term} ->
            {ok, cmkit:jsone(Term)};
        Other ->
            Other
    end;
encode(#{type := pbkdf2,
         value := Value,
         salt := Salt,
         iterations := Iterations,
         length := Length},
       In,
       Config) ->
    case encode(Value, In, Config) of
        {ok, V} ->
            case encode(Salt, In, Config) of
                {ok, S} ->
                    case encode(Iterations, In, Config) of
                        {ok, I} ->
                            case encode(Length, In, Config) of
                                {ok, L} ->
                                    pbkdf2:pbkdf2({hmac, sha512}, V, S, I, L);
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
encode(#{type := pbkdf2,
         value := Value,
         using := Using},
       In,
       Config) ->
    case encode(Value, In, Config) of
        {ok, V} ->
            case encode(Using, In, Config) of
                {ok,
                 #{salt := S,
                   iterations := I,
                   length := L}} ->
                    pbkdf2:pbkdf2({hmac, sha512}, V, S, I, L);
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
encode(#{type := encrypt,
         spec :=
             #{method := aes_cbc,
               key := KeySpec,
               value := ValueSpec}},
       In,
       Config) ->
    case encode(KeySpec, In, Config) of
        {ok, Key} ->
            case encode(ValueSpec, In, Config) of
                {ok, Value} ->
                    case cmkit:encrypt(Key, Value) of
                        error ->
                            {error, encrypt_error};
                        {ok, Cypher} ->
                            {ok, Cypher}
                    end;
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
encode(#{type := pub,
         topic := Topic,
         spec := Msg},
       In,
       Config) ->
    case encode(Topic, In, Config) of
        {ok, T} ->
            case encode(Msg, In, Config) of
                {ok, M} ->
                    case cmbus:pub(T, M) of
                        ok ->
                            {ok, ok};
                        Other ->
                            Other
                    end;
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
encode(#{type := perf}, _, _) ->
    {ok, cmperf:stats()};
encode(#{type := asset, spec := Spec}, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, Name} ->
            cmkit:read_file(
              filename:join(
                cmkit:assets(), Name));
        Other ->
            Other
    end;
encode(#{type := cmdata}, _, _) ->
    {ok, cmkit:data()};
encode(#{type := equal, spec := Specs} = Spec, In, Config) when is_list(Specs) ->
    case encode_all(Specs, In, Config) of
        {ok, Terms} ->
            {ok, all_equal(Terms)};
        {error, E} ->
            fail(Spec, In, E)
    end;
encode(#{type := greater_than, spec := [Spec1, Spec2]}, In, Config) ->
    case {encode(Spec1, In, Config), encode(Spec2, In, Config)} of
        {{ok, V1}, {ok, V2}} when is_number(V1) and is_number(V2) ->
            {ok, V1 > V2};
        Other ->
            Other
    end;
encode(#{type := 'or', spec := Exprs}, In, Config) ->
    first_true(Exprs, In, Config);
encode(#{type := multiply, spec := Specs} = Spec, In, Config) ->
    Res = foldl(Specs,
                In,
                Config,
                1,
                fun (V, Acc) when is_number(V) ->
                        {ok, Acc * V};
                    (Other, _) ->
                        nan(Other, Spec)
                end),

    case Res of
        N when is_number(N) ->
            {ok, N};
        Other ->
            Other
    end;
encode(#{type := sum, spec := Spec} = Spec0, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, Terms} when is_list(Terms) ->
            fold_numbers(Terms, fun(A, B) -> A + B end);
        {ok, NotSupported} ->
            fail(Spec0, NotSupported, not_a_list);
        Other ->
            Other
    end;
encode(#{type := max, spec := Spec} = Spec0, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, Terms} when is_list(Terms) ->
            {ok, lists:max(Terms)};
        {ok, NotSupported} ->
            fail(Spec0, NotSupported, not_a_list);
        Other ->
            Other
    end;
encode(#{type := difference, spec := [First | Rem]} = Spec, In, Config) ->
    Res = case encode(First, In, Config) of
              {ok, Encoded} when is_number(Encoded) ->
                  foldl(Rem,
                        In,
                        Config,
                        Encoded,
                        fun (V, Acc) when is_number(V) ->
                                {ok, Acc - V};
                            (Other, _) ->
                                nan(Other, Spec)
                        end);
              Other ->
                  nan(Other, Spec)
          end,

    case Res of
        N when is_number(N) ->
            {ok, N};
        Other2 ->
            Other2
    end;
encode(#{type := join,
         separator := SepSpec,
         spec := Specs},
       In,
       Config)
  when is_list(Specs) ->
    case encode_all(Specs, In, Config) of
        {ok, EncodedTerms} ->
            case encode(SepSpec, In, Config) of
                {ok, Sep} ->
                    {ok, cmkit:bin_join(EncodedTerms, Sep)};
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
encode(#{type := join,
         separator := SepSpec,
         spec := Spec},
       In,
       Config) ->
    case encode(Spec, In, Config) of
        {ok, EncodedTerms} when is_list(EncodedTerms) ->
            case encode(SepSpec, In, Config) of
                {ok, Sep} ->
                    {ok, cmkit:bin_join(EncodedTerms, Sep)};
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
encode(#{type := split,
         separator := SepSpec,
         spec := Spec},
       In,
       Config) ->
    case encode(Spec, In, Config) of
        {ok, Encoded} ->
            case encode(SepSpec, In, Config) of
                {ok, Sep} ->
                    {ok, cmkit:bin_split(Encoded, Sep)};
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
encode(#{type := length, spec := Spec}, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, Encoded} when is_list(Encoded) ->
            {ok, length(Encoded)};
        Other ->
            Other
    end;
encode(#{type := head, spec := Spec}, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, [H | _]} ->
            {ok, H};
        Other ->
            Other
    end;
encode(#{type := tail, spec := Spec}, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, [_ | T]} ->
            {ok, T};
        Other ->
            Other
    end;
encode(#{type := lowercase, spec := Spec}, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, Bin} when is_binary(Bin) ->
            {ok, cmkit:lower_bin(Bin)};
        {ok, Other} ->
            fail(Other, In, Other);
        Other ->
            Other
    end;
encode(#{type := format,
         pattern := PatternSpec,
         params := ParamsSpec},
       In,
       Config) ->
    case cmencode:encode(PatternSpec, In, Config) of
        {ok, Pattern} ->
            case encode(ParamsSpec, In, Config) of
                {ok, []} ->
                    {ok, Pattern};
                {ok, List} when is_list(List) ->
                    {ok, cmkit:fmt(Pattern, List)};
                {ok, null} ->
                    {ok, Pattern};
                {ok, Map} when is_map(Map) ->
                    cmkit:fmt_named(Pattern, Map);
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
encode(#{type := format,
         pattern := PatternSpec,
         date := DateSpec},
       In,
       Config) ->
    case encode(DateSpec, In, Config) of
        {ok, Date} ->
            case encode(PatternSpec, In, Config) of
                {ok, Pattern} ->
                    case cmkit:format_date(Date, Pattern) of
                        invalid ->
                            {error,
                             #{date => Date,
                               pattern => Pattern,
                               error => cannot_format_date}};
                        Bin ->
                            {ok, Bin}
                    end;
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
encode(#{type := replace,
         source := SourceSpec,
         text := SearchSpec,
         with := ReplaceSpec},
       In,
       Config) ->
    case encode(SourceSpec, In, Config) of
        {ok, Source} ->
            case encode(SearchSpec, In, Config) of
                {ok, Search} ->
                    case encode(ReplaceSpec, In, Config) of
                        {ok, Replace} ->
                            {ok, cmkit:replace(Source, Search, Replace)};
                        Other ->
                            Other
                    end;
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
encode(#{type := utc,
         amount := AmountSpec,
         factor := Factor,
         tense := Tense},
       In,
       Config) ->
    case encode(AmountSpec, In, Config) of
        {ok, Amount} ->
            Now = calendar:universal_time(),
            Secs =
                case Tense of
                    past ->
                        Amount * Factor * -1;
                    future ->
                        Amount * Factor
                end,

            {ok, cmcalendar:add_seconds_to_utc(Secs, Now)};
        Other ->
            Other
    end;
encode(#{type := now, resolution := seconds}, _, _) ->
    {ok, cmkit:seconds()};
encode(#{type := now, resolution := millis}, _, _) ->
    {ok, cmkit:now()};
encode(#{type := now, resolution := micros}, _, _) ->
    {ok, cmkit:micros()};
encode(#{type := wait,
         spec :=
             #{retries := Retries,
               sleep := Sleep,
               condition := Condition}},
       In,
       Config) ->
    encode_retry(Retries, Sleep, Condition, In, Config);
encode(#{type := wait, spec := #{sleep := Sleep}}, _, _) ->
    cmkit:log({cmencode, wait, Sleep}),
    timer:sleep(Sleep),
    cmkit:log({cmencode, sleep, resuming}),
    {ok, Sleep};
encode(#{type := match,
         spec := #{value := ValueSpec, decoder := DecoderSpec} = MatchSpec},
       In,
       Config) ->
    case cmencode:encode(ValueSpec, In, Config) of
        {ok, Value} ->
            case cmdecode:decode(DecoderSpec, Value, In) of
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
encode(#{type := find,
         items := ItemsSpec,
         target := TargetSpec},
       In,
       Config) ->
    case cmencode:encode(ItemsSpec, In, Config) of
        {ok, Items} ->
            case cmdecode:decode(#{type => first, spec => TargetSpec}, Items, Config) of
                {ok, _} ->
                    {ok, true};
                _ ->
                    {ok, false}
            end;
        Other ->
            Other
    end;
encode(#{type := error, spec := Spec}, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, Encoded} ->
            {error, Encoded};
        {error, Other} ->
            {error, Other}
    end;
encode(#{type := attempt,
         spec := Spec,
         onerror := OnError},
       In,
       Config) ->
    case encode(Spec, In, Config) of
        {ok, Encoded} ->
            {ok, Encoded};
        Other ->
            cmkit:warning({encode, attempted, Spec, Other}),
            cmencode:encode(OnError, In, Config)
    end;
encode(#{type := sort,
         items := ItemsSpec,
         by := PropSpec,
         mode := ModeSpec},
       In,
       Config) ->
    case cmencode:encode(ItemsSpec, In, Config) of
        {ok, Items} when is_list(Items) ->
            case cmencode:encode(PropSpec, In, Config) of
                {ok, SimpleProp} when is_binary(SimpleProp) or is_atom(SimpleProp) ->
                    case cmencode:encode(ModeSpec, In, Config) of
                        {ok, Mode} ->
                            SortFun =
                                case Mode of
                                    asc ->
                                        fun(A, B) ->
                                                cmkit:value_at(SimpleProp, A) <
                                                    cmkit:value_at(SimpleProp, B)
                                        end;
                                    desc ->
                                        fun(A, B) ->
                                                cmkit:value_at(SimpleProp, A) >
                                                    cmkit:value_at(SimpleProp, B)
                                        end
                                end,
                            {ok, lists:sort(SortFun, Items)};
                        Other ->
                            Other
                    end;
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
encode(#{type := encode,
         source := SourceSpec,
         as := AsSpec,
         dest := DestSpec},
       In,
       Config) ->
    case cmencode:encode(SourceSpec, In, Config) of
        {ok, Source} ->
            case map(DestSpec, In, Config, Source, AsSpec) of
                {ok, [Encoded]} ->
                    {ok, Encoded};
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
encode(#{type := group,
         source := ItemsSpec,
         into := IntoSpec,
         by := GroupingSpec,
         as := GroupNameSpec},
       In,
       Config) ->
    case encode(GroupNameSpec, In, Config) of
        {ok, GroupName} ->
            case encode(IntoSpec, In, Config) of
                {ok, Into} ->
                    case encode(ItemsSpec, In, Config) of
                        {ok, Items} when is_list(Items) ->
                            group_items(GroupName, GroupingSpec, Into, Items);
                        {ok, Unsupported} ->
                            fail(ItemsSpec, Unsupported, not_a_list);
                        Other ->
                            Other
                    end;
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
encode(#{type := iterate,
         spec :=
             #{source := SourceSpec,
               filter := FilterSpec,
               as := AsSpec,
               dest := DestSpec}},
       In,
       Config) ->
    case encode_as(AsSpec, In, Config) of
        {ok, As} ->
            case cmencode:encode(SourceSpec, In, Config) of
                {ok, Source} when is_list(Source) ->
                    EncodedSource =
                        case FilterSpec of
                            none ->
                                {ok, Source};
                            _ ->
                                EncodedFilterSpec =
                                    case FilterSpec of
                                        #{type := object} ->
                                            {ok, FilterSpec};
                                        _ ->
                                            case encode(FilterSpec, In, Config) of
                                                {ok, Encoded} ->
                                                    {ok, Encoded};
                                                Other ->
                                                    Other
                                            end
                                    end,

                                case EncodedFilterSpec of
                                    {ok, FilterSpec2} ->
                                        {ok,
                                         lists:filter(fun(Item) ->
                                                              FilterContext =
                                                                  case As of
                                                                      none -> Item;
                                                                      undef -> Item;
                                                                      K -> #{K => Item}
                                                                  end,

                                                              case cmdecode:decode(FilterSpec2,
                                                                                   FilterContext,
                                                                                   In)
                                                              of
                                                                  {ok, _} -> true;
                                                                  _ -> false
                                                              end
                                                      end,
                                                      Source)};
                                    Other2 ->
                                        Other2
                                end
                        end,
                    case EncodedSource of
                        {ok, Source2} ->
                            case DestSpec of
                                none ->
                                    {ok, Source2};
                                _ ->
                                    map(DestSpec, In, Config, Source2, AsSpec)
                            end;
                        Other3 ->
                            Other3
                    end;
                Other4 ->
                    Other4
            end;
        Other ->
            Other
    end;
encode(#{type := iterate, spec := #{source := _, dest := _} = S0} = Spec, In, Config) ->
    encode(Spec#{spec => S0#{filter => none}}, In, Config);
encode(#{type := merge, spec := Specs} = Spec, In, Config) ->
    EncodedSpecs =
        case is_list(Specs) of
            true ->
                encode_all(Specs, In, Config);
            false ->
                encode(Specs, In, Config)
        end,

    case EncodedSpecs of
        {ok, []} ->
            {ok, #{}};
        {ok, [First | _] = EncodedTerms} when is_map(First) ->
            merge_maps(EncodedTerms);
        {ok, [First | _] = EncodedTerms} when is_list(First) ->
            merge_lists(EncodedTerms);
        {ok, Other} ->
            fail(Spec, In, #{error => unsupported_argument, data => Other});
        {error, E} ->
            fail(Spec, In, E)
    end;
encode(#{type := send, spec := #{to := ConnSpec, spec := Spec}},
       #{conns := Conns} = In,
       _) ->
    case cmencode:encode(ConnSpec, In) of
        {ok, Name} ->
            case maps:get(Name, Conns, undef) of
                undef ->
                    {error, #{error => no_such_connection, info => Name}};
                Conn ->
                    case cmencode:encode(Spec, In) of
                        {ok, Encoded} ->
                            case Conn of
                                #{class := websocket, pid := Pid} ->
                                    cmwsc:send(Pid, Encoded),
                                    {ok, #{connection => Conn#{inbox => []}}};
                                #{class := http,
                                  transport := Transport,
                                  url := Url} ->
                                    case Encoded of
                                        #{body := Body, headers := Headers} ->
                                            cmkit:log({cmencode,
                                                       Transport,
                                                       out,
                                                       Url,
                                                       Headers,
                                                       Body}),
                                            {_, Res} = cmhttp:post(Url, Headers, Body),
                                            cmkit:log({cmencode, Transport, in, Res}),
                                            {ok, #{connection => Conn#{inbox => [Res]}}};
                                        Other ->
                                            {error, #{error => wrong_http_request, info => Other}}
                                    end;
                                Other ->
                                    {error,
                                     #{error => connection_class_not_supported, info => Other}}
                            end;
                        Other ->
                            Other
                    end
            end;
        Other ->
            Other
    end;
encode(#{type := kube,
         spec :=
             #{name := NameSpec,
               namespace := NsSpec,
               resource := Resource,
               server := ServerSpec,
               state := StateSpec} =
             Spec},
       In,
       Config) ->
    case encode(NameSpec, In, Config) of
        {ok, Name} ->
            case encode(NsSpec, In, Config) of
                {ok, Ns} ->
                    case encode(ServerSpec, In, Config) of
                        {ok, Server} ->
                            case encode(StateSpec, In, Config) of
                                {ok, State} ->
                                    Params =
                                        #{name => Name,
                                          namespace => Ns,
                                          resource => Resource,
                                          state => State,
                                          server => Server},
                                    case maps:get(props, Spec, undef) of
                                        undef ->
                                            cmkube:do(Params);
                                        PropsSpec ->
                                            case encode(PropsSpec, In, Config) of
                                                {ok, Props} ->
                                                    cmkube:do(Params#{props => Props});
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
encode(#{type := hash, spec := Hash}, In, Config) ->
    case encode(Hash, In, Config) of
        {ok, Encoded} ->
            Hashed = cmkit:hash(Encoded),
            {ok, Hashed};
        Other ->
            Other
    end;
encode(#{encoder := NameSpec} = Spec, In, #{encoders := Encs} = Config) ->
    case encode(NameSpec, In, Config) of
        {ok, Name} ->
            case maps:get(Name, Encs, undef) of
                undef ->
                    {error,
                     #{reason => encode,
                       spec => Spec,
                       encoder => Name,
                       status => not_such_encoder,
                       encoders => Encs}};
                Enc ->
                    encode(Enc, In, Config)
            end;
        Other ->
            Other
    end;
encode(#{value := V}, _, _) ->
    {ok, V};
encode(#{type := prefix,
         spec := Spec,
         with := Prefix},
       In,
       Config) ->
    case encode(Prefix, In, Config) of
        {ok, P} ->
            PBin = cmkit:to_bin(P),
            case encode(Spec, In, Config) of
                {ok, Items} when is_list(Items) ->
                    {ok, lists:map(fun(I) -> <<PBin/binary, (cmkit:to_bin(I))/binary>> end, Items)};
                {ok, S} ->
                    {ok, <<PBin/binary, (cmkit:to_bin(S))/binary>>};
                Other ->
                    {error,
                     #{error => encode,
                       spec => Spec,
                       reason => Other}}
            end;
        Other ->
            Other
    end;
encode(#{type := sequence,
         from := From,
         to := To},
       In,
       Config) ->
    case encode(From, In, Config) of
        {ok, F} ->
            case encode(To, In, Config) of
                {ok, T} ->
                    {ok, lists:seq(F, T)};
                Other ->
                    Other
            end;
        Other ->
            Other
    end;
encode(#{type := pipe,
         specs := Specs,
         as := As},
       In,
       Config) ->
    pipe(Specs, As, In, Config);
encode(#{spec := Spec}, _, _) ->
    {ok, Spec};
encode(List, In, Config) when is_list(List) ->
    encode_all(List, In, Config);
encode(Value, _, _) ->
    {ok, Value}.

encode_object(Spec, In, Config, Mode) ->
    encode_object(maps:keys(Spec), Spec, In, Config, Mode, #{}).

encode_object([], _, _, _, _, Out) ->
    {ok, Out};
encode_object([K | Rem], Spec, In, Config, strict, Out) ->
    case encode(maps:get(K, Spec), In, Config) of
        {ok, V} ->
            encode_object(Rem, Spec, In, Config, strict, Out#{K => V});
        Other ->
            Other
    end;
encode_object([K | Rem], Spec, In, Config, loose, Out) ->
    case encode(maps:get(K, Spec), In, Config) of
        {ok, V} ->
            encode_object(Rem, Spec, In, Config, loose, Out#{K => V});
        _ ->
            encode_object(Rem, Spec, In, Config, loose, Out)
    end.

encode_list(Specs, In, Config) ->
    encode_list(Specs, In, Config, []).

encode_list([], _, _, Out) ->
    {ok, lists:reverse(Out)};
encode_list([Spec | Rem], In, Config, Out) ->
    case encode(Spec, In, Config) of
        {ok, Encoded} ->
            encode_list(Rem, In, Config, [Encoded | Out]);
        {error, E} ->
            fail(Spec, In, E)
    end.

map(Dest, In, Config, Source, AsSpec) ->
    case AsSpec of
        none ->
            map(Dest, In, Config, Source, none, []);
        S0 ->
            case encode(S0, In, Config) of
                {ok, As} ->
                    map(Dest, In, Config, Source, As, []);
                Other ->
                    Other
            end
    end.

map(_, _, _, [], _, Out) ->
    {ok, lists:reverse(Out)};
map(DestSpec, In, Config, [Item | Rem], As, Out) ->
    case map(DestSpec, In, Config, Item, As, Out) of
        {ok, Encoded} ->
            map(DestSpec, In, Config, Rem, As, [Encoded | Out]);
        Other ->
            Other
    end;
map(DestSpec, In, Config, Item, As, _) ->
    In2 = case As of
              none ->
                  maps:merge(In, Item);
              _ ->
                  In#{As => Item}
          end,
    cmencode:encode(DestSpec, In2, Config).

fail(Spec, Data, Reason) ->
    {error,
     #{status => encode_error,
       spec => Spec,
       data => Data,
       reason => Reason}}.

first_true([], _, _) ->
    {ok, false};
first_true([Spec | Rem], In, Config) ->
    case encode(Spec, In, Config) of
        {ok, true} ->
            {ok, true};
        _ ->
            first_true(Rem, In, Config)
    end.

encode_all(Specs) ->
    encode_all(Specs, #{}, #{}).

encode_all(Specs, In) ->
    encode_all(Specs, In, #{}).

encode_all(Specs, In, Config) ->
    encode_all(Specs, In, Config, []).

encode_all([], _, _, Out) ->
    {ok, lists:reverse(Out)};
encode_all([Spec | Rem], In, Config, Out) ->
    case encode(Spec, In, Config) of
        {ok, Encoded} ->
            encode_all(Rem, In, Config, [Encoded | Out]);
        Other ->
            Other
    end.

pipe([], As, In, _) ->
    case maps:get(As, In, undef) of
        undef ->
            {error,
             #{status => encode_error,
               spec => pipe,
               data => In,
               as => As,
               reason => no_data}};
        Encoded ->
            {ok, Encoded}
    end;
pipe([Spec | Rem], As, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, Encoded} ->
            pipe(Rem, As, In#{As => Encoded}, Config);
        Other ->
            Other
    end.

with_where(#{where := WhereSpec}, In, Config) ->
    case encode(WhereSpec, In, Config) of
        {ok, Where} ->
            {ok, maps:merge(In, Where)};
        Other ->
            Other
    end;
with_where(_, In, _) ->
    {ok, In}.

encode_case_clause(Map, Expr, In, Config) when is_map(Map) ->
    case maps:get(Expr, Map, undef) of
        undef ->
            {error, #{error => encode_error, reason => no_clause_matches}};
        Found ->
            encode(Found, In, Config)
    end;
encode_case_clause([], _, _, _) ->
    {error, #{error => encode_error, reason => no_clause_matches}};
encode_case_clause([#{condition := Condition, spec := Spec} | Rem], Expr, In, Config) ->
    case cmdecode:decode(Condition, Expr, Config) of
        {ok, _} ->
            encode(Spec, In, Config);
        _ ->
            encode_case_clause(Rem, Expr, In, Config)
    end.

encode_options([], All, In, _) ->
    {error,
     #{status => encode_error,
       reason => no_condition_did_verify,
       in => In,
       options => All}};
encode_options([C | Rem], All, In, Config) ->
    case encode(C, In, Config) of
        {ok, V} ->
            {ok, V};
        {error, condition_not_satisfied} ->
            encode_options(Rem, All, In, Config);
        Other ->
            Other
    end.

encode_first([], All, In, Config) ->
    {error,
     #{status => encode_error,
       reason => all_options_failed,
       in => In,
       config => Config,
       options => All}};
encode_first([Spec | Rem], All, In, Config) ->
    case encode(Spec, In, Config) of
        {ok, Encoded} ->
            {ok, Encoded};
        _Other ->
            encode_first(Rem, All, In, Config)
    end.

encode_retry(0, _, Condition, In, _) ->
    {error,
     #{status => encode_error,
       spec => Condition,
       data => In,
       reason => max_retries_reached}};
encode_retry(Retries, Millis, Condition, In, Config) ->
    cmkit:log({cmencode, sleeping, Millis}),
    timer:sleep(Millis),
    cmkit:log({cmencode, retrying}),
    case encode(Condition, In, Config) of
        {ok, true} ->
            {ok, true};
        _ ->
            encode_retry(Retries - 1, Millis, Condition, In, Config)
    end.

encode_http(Method, Url, Query, Headers, Debug, Body, In, Config) ->
    case encode_http(Method, Url, Query, Headers, Debug, In, Config) of
        {ok, Spec} ->
            case encode(Body, In, Config) of
                {ok, multipart, B, Boundary} ->
                    #{headers := H} = Spec,
                    {ok,
                     Spec#{body => B,
                           headers =>
                               H#{'content-type' =>
                                      "multipart/form-data; boundary=" ++
                                      binary_to_list(Boundary)}}};
                {ok, B} ->
                    {ok, Spec#{body => B}};
                Other ->
                    Other
            end;
        Other ->
            Other
    end.

encode_http(Method, Url, Query, Headers, Debug, In, Config) ->
    case encode(Headers, In, Config) of
        {ok, H} ->
            case encode(Query, In, Config) of
                {ok, Q} ->
                    case encode(Debug, In, Config) of
                        {ok, D} ->
                            case encode_url(Url, In, Config) of
                                {ok, #{url := U}} ->
                                    {ok,
                                     #{url => U,
                                       debug => D,
                                       method => Method,
                                       headers => H,
                                       query => Q}};
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
    end.

encode_url(Url, In, Config) ->
    case encode(Url, In, Config) of
        {ok, #{url := _} = Spec} ->
            {ok, Spec};
        {ok,
         #{host := _,
           port := _,
           transport := _,
           path := _} =
             Spec} ->
            encode_url(#{type => url, spec => Spec}, In, Config);
        {ok, U} when is_binary(U) ->
            {ok, #{url => U}};
        U when is_binary(U) ->
            {ok, #{url => U}};
        Other ->
            Other
    end.

merge_lists(Lists) ->
    {ok, lists:merge(Lists)}.

merge_maps(Maps) ->
    merge_maps(Maps, #{}).

merge_maps([], Out) ->
    {ok, Out};
merge_maps([M | Rem], Out) when is_map(M) ->
    merge_maps(Rem, maps:merge(Out, M));
merge_maps([Other | _], _) ->
    encode_error(not_a_map, Other).

encode_error(R, Data) ->
    {error, #{error => R, data => Data}}.

all_equal([V | Rem]) ->
    all_equal(Rem, V).

all_equal([], _) ->
    true;
all_equal([V | Rem], V) ->
    all_equal(Rem, V);
all_equal(_, _) ->
    false.

fold_numbers(Numbers, Fun) ->
    fold_numbers(Numbers, Fun, 0).

fold_numbers([], _, Result) ->
    {ok, Result};
fold_numbers([N | Rest], Fun, Result) when is_number(N) ->
    fold_numbers(Rest, Fun, Fun(N, Result));
fold_numbers([N | _], _, _) ->
    {error, #{error => not_a_number, value => N}}.

foldl([], _, _, Acc, _) ->
    {ok, Acc};
foldl([S | Rem], In, Config, Acc, Fun) ->
    case encode(S, In, Config) of
        {ok, Encoded} ->
            case Fun(Encoded, Acc) of
                {ok, Acc2} ->
                    foldl(Rem, In, Config, Acc2, Fun);
                Other ->
                    Other
            end;
        Other ->
            Other
    end.

encode_keys(Keys, Map) ->
    encode_keys(Keys, Map, #{}).

encode_keys([], _, Out) ->
    {ok, Out};
encode_keys([K | Rem], Map, Out) ->
    case cmkit:value_at(K, Map) of
        undef ->
            encode_error(missing_key, #{key => K, keys => maps:keys(Map)});
        V ->
            encode_keys(Rem, Map, Out#{K => V})
    end.

nan(Value, Spec) ->
    {error,
     #{status => encode_error,
       spec => Spec,
       data => Value,
       reason => not_a_number}}.

group_items(GroupAlias, GroupingSpec, Into, Items) ->
    case groups(Items, GroupingSpec, #{names => [], groups => #{}}) of
        {ok, Index} ->
            sorted_groups(GroupAlias, Into, Index);
        Other ->
            Other
    end.

groups([], _, Index) ->
    {ok, Index};
groups([Item | Rest], GroupingSpec, #{names := GroupNames, groups := Groups} = Index) ->
    case encode(GroupingSpec, Item) of
        {ok, GroupName} ->
            case maps:get(GroupName, Groups, undef) of
                undef ->
                    groups(Rest,
                           GroupingSpec,
                           Index#{names => [GroupName | GroupNames],
                                  groups => Groups#{GroupName => [Item]}});
                Group ->
                    groups(Rest,
                           GroupingSpec,
                           Index#{groups => Groups#{GroupName => [Item | Group]}})
            end;
        Other ->
            Other
    end.

sorted_groups(GroupAlias, Into, #{names := GroupNames, groups := Groups}) ->
    {ok,
     lists:map(fun(N) ->
                       Items = maps:get(N, Groups),
                       #{GroupAlias => N, Into => lists:reverse(Items)}
               end,
               lists:reverse(GroupNames))}.

encode_as(none, _, _) ->
    {ok, undef};
encode_as(Spec, In, Config) ->
    encode(Spec, In, Config).
