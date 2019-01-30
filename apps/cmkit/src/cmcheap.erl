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
    cmd(get, <<"namecheap.domains.dns.getHosts">>, #{}, Config).

cmd(Method, Command, _Extra, #{ sld := Sld,
                                tld := Tld,
                                user := User,
                                key := ApiKey }) -> 
    
    parse(cmhttp:do(#{ method => Method,
                 url => <<"https://api.namecheap.com/xml.response">>,
                 query => #{ 'ApiUser' => User,
                             'UserName' => User,
                             'ApiKey' => ApiKey,
                             'Command' => Command,
                             'ClientIp' => client_ip(),
                             'SLD' => Sld,
                             'TLD' => Tld }})).

client_ip() ->
    cmkit:to_bin(cmkit:ip_str(cmkit:ipv4())).


parse({ok, #{ status := 200,
              body := Body }}) ->
    {ok, Body};

parse({ok, #{ status := Other }}) ->
    {error, Other}.
