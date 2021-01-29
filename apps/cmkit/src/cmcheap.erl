-module(cmcheap).
-export([add/2, replace/2, hosts/1]).


replace(#{ 'Name' := Name,
           'Type' := _,
           'Address' := _,
           'TTL' := _ }=H, Config) ->

    case hosts(Config) of 
        {ok, Hosts} ->
            case set_hosts([H|hosts_without(Name, Hosts)], Config) of 
                {ok, "true"} ->
                    ok;
                {ok, Other} ->
                    {error, Other};
                Other ->
                    Other
            end;
        Other ->
            Other
    end.


add(#{ 'Name' := _,
       'Type' := _,
       'Address' := _,
       'TTL' := _ }=H, Config) ->

    case hosts(Config) of 
        {ok, Hosts} ->
            case set_hosts([H|Hosts], Config) of 
                {ok, "true"} ->
                    ok;
                {ok, Other} ->
                    {error, Other};
                Other ->
                    Other
            end;
        Other ->
            Other
    end;

add(_, _) -> err(invalid_host).

set_hosts(Hosts, Config) ->
    {Q, _} = lists:foldl(fun(H, {Acc, Idx}) ->
                                 {replace_host(H, Acc, Idx), Idx+1}
                         end, {#{}, 1}, Hosts),
    case cmd(post, 'namecheap.domains.dns.setHosts', 'DomainDNSSetHostsResult', Q, Config) of 
        {ok, Attrs, _} ->
            case lists:keyfind('IsSuccess', 1, Attrs) of 
                false ->
                    unknown_err();
                {_, Value} ->
                    {ok, Value}
            end;
        Other ->
            Other
    end.

replace_host(#{ 'Name' := Name,
                'Type' := Type,
                'Address' := Address,
                'TTL' := TTL }= Host, Hosts, Idx) ->

    IdxBin = (cmkit:to_bin(Idx)),
    NameKey = <<"HostName", IdxBin/binary>>,
    RecordTypeKey = <<"RecordType", IdxBin/binary>>,
    AddressKey = <<"Address", IdxBin/binary>>,
    TTLKey = <<"TTL", IdxBin/binary>>,

    Hosts2 = Hosts#{ NameKey => Name,
                     RecordTypeKey => Type,
                     AddressKey => Address,
                     TTLKey => TTL },

    case maps:get('MXPref', Host, undef) of 
        undef ->
            Hosts2;
        MXPref ->
            MXPrefKey = <<"MXPref", IdxBin/binary>>,
            Hosts2#{ MXPrefKey => MXPref }
    end.


hosts(Config) ->
    case cmd(get, 'namecheap.domains.dns.getHosts', 'DomainDNSGetHostsResult', #{}, Config) of 
        {ok, _, Hosts} ->
            {ok, lists:map(fun({host, Attrs, _}) ->
                                   maps:from_list(Attrs)
                           end, Hosts)};
        Other ->
            Other
    end.

cmd(Method, Command, Resp, Extra, #{ sld := Sld,
                                     tld := Tld,
                                     user := User,
                                     key := ApiKey }) -> 

    extract(cmhttp:do(#{ method => Method,
                         url => <<"https://api.namecheap.com/xml.response">>,
                         query => maps:merge(Extra, #{ 'ApiUser' => User,
                                                       'UserName' => User,
                                                       'ApiKey' => ApiKey,
                                                       'Command' => Command,
                                                       'ClientIp' => client_ip(),
                                                       'SLD' => Sld,
                                                       'TLD' => Tld })
                       }), Resp).

client_ip() ->
    cmkit:to_bin(cmkit:ip_str(cmkit:ipv4())).


extract({ok, #{ status := 200,
                body := {'ApiResponse', Attrs, Children}}}, RespName) ->

    case lists:keyfind('Status', 1, Attrs) of 
        false ->
            unknown_err();
        {_, "ERROR"} ->
            extract_error(Children);
        {_, "OK"} ->
            extract_response(Children, RespName)
    end;


extract({ok, #{ status := Other }}, _) when Other =/= 200 ->
    {error, Other}.

extract_error(Resp) ->
    case lists:keyfind('Errors', 1, Resp) of 
        false ->
            unknown_err();
        {_, _, Errors} ->
            case lists:keyfind('Error', 1, Errors) of 
                false ->
                    unknown_err();
                {_, _, [E]} ->
                    err(E)
            end
    end.

extract_response(Resp, RespName) ->
    case lists:keyfind('CommandResponse', 1, Resp) of 
        false ->
            unknown_err();
        {_, _, Data} ->
            case lists:keyfind(RespName, 1, Data) of 
                false ->
                    unknown_err();
                {_, Attrs, Payload} ->
                    {ok, Attrs, Payload}
            end              
    end.

unknown_err() ->
    err(unknown).

err(E) ->
    {error, E}.


hosts_without(N, Hosts) ->
    [ H || #{'Name' := Name} = H <- Hosts, cmkit:to_bin(Name) =/= cmkit:to_bin(N) ].
