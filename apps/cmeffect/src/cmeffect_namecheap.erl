-module(cmeffect_namecheap).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> namecheap.
effect_apply(#{ context := Context,
                config := #{ tld := Tld,
                             sld := Sld,
                             user := _,
                             key := _ } = Config,
                replace := #{ name := Name,
                          type := Type,
                          value := Value,
                          ttl := TTL } }, Pid) ->

    Res = case cmcheap:replace(#{ 'Name' => Name,
                                   'Address' => Value,
                                   'Type' => Type,
                                   'TTL' => TTL }, Config) of 
              ok ->
                    case await(Name, Sld, Tld, Type, Value) of 
                        true ->
                            #{ context => Context,
                                status => ok };

                        false ->
                            #{ context => Context,
                               status => timeout }
                    end;

              {error, E} ->
                  #{ context => Context,
                     status => error,
                     error => E }
          end,
    
    cmcore:update(Pid, Res).


dns_name(Name, Sld, Tld) ->
    cmkit:to_list(<<Name/binary, ".",
                    Sld/binary, ".",
                    Tld/binary>>).

inet_type(<<"TXT">>) -> txt;
inet_type("TXT") -> txt.

await(Name, Sld, Tld, Type, Expected) ->
    cmkit:await(fun() ->
                        Current = inet_res:lookup(dns_name(Name, Sld, Tld),
                                             in, inet_type(Type)),
                        case Current of
                            [[V]] ->
                                cmkit:to_bin(V) =:= Expected;
                            _->
                                false
                        end
                end, #{ retries => 40,
                        sleep => 1000 }).
