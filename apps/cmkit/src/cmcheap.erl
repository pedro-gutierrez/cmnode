-module(cmcheap).
-export([add_host/2, hosts/1]).

add_host(H, Config) ->

    case hosts(Config) of 
        {ok, Hosts} ->
            set_hosts([H|Hosts], Config);
        Other ->
            Other
    end.
 
set_hosts(Hosts, #{ sld := Sld,
                    tld := Tld,
                    user := User,
                    key := ApiKey }) ->

    ok.


hosts(Config) ->
    cmd(get, <<"namecheap.domains.dns.getHosts">>, 'DomainDNSGetHostsResult', #{}, Config).

cmd(Method, Command, Resp, _Extra, #{ sld := Sld,
                                tld := Tld,
                                user := User,
                                key := ApiKey }) -> 
    
    extract(cmhttp:do(#{ method => Method,
                 url => <<"https://api.namecheap.com/xml.response">>,
                 query => #{ 'ApiUser' => User,
                             'UserName' => User,
                             'ApiKey' => ApiKey,
                             'Command' => Command,
                             'ClientIp' => client_ip(),
                             'SLD' => Sld,
                             'TLD' => Tld }}), Resp).

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
                {_, _, Payload} ->
                    {ok, Payload}
            end              
    end.

unknown_err() ->
    err(unknown).

err(E) ->
    {error, E}.
    
