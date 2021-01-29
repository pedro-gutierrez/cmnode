-module(cmeffect_namecheap).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> namecheap.
effect_apply(#{ context := Context,
                config := #{ tld := _,
                             sld := _,
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
                  #{ context => Context,
                     status => ok };

              {error, E} ->
                  #{ context => Context,
                     status => error,
                     error => E }
          end,

    cmcore:update(Pid, Res).
