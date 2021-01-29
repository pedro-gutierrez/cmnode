-module(cmeffect_db).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> db.

effect_apply(#{ context := C,
                bucket := B,
                reset := _ }=Spec, Id) ->

    R = reply_from(cmdb:reset(B), Spec),
    cmcore:update(Id, R#{ bucket => B,
                          context => C });

effect_apply(#{ context := C,
                buckets := Buckets,
                restart := _ }, Id) ->

    R = case restart(Buckets) of 
            ok ->
                #{status => ok};
            {error, E} ->
                #{ status => error,
                   reason => E}
        end,

    cmcore:update(Id, R#{ buckets => Buckets,
                          context => C });

effect_apply(#{ context := C,
                bucket := B,
                put := Values }=Spec, Id) ->

    R = reply_from(cmdb:put(B, values(Values)), Spec),
    cmcore:update(Id, R#{ bucket => B,
                          context => C });

effect_apply(#{ context := C,
                bucket := B,
                delete := Keys }=Spec, Id) ->

    R = reply_from(cmdb:delete(B, keys(Keys)), Spec),
    cmcore:update(Id, R#{ bucket => B,
                          context => C });

effect_apply(#{ context := C,
                bucket := B,
                insert := Values }=Spec, Id) ->

    R = reply_from(cmdb:insert(B, values(Values)), Spec),
    cmcore:update(Id, R#{ bucket => B,
                          context => C });

effect_apply(#{ context := C,
                bucket := B,
                match := #{ subject := S,
                            predicate := P,
                            object := O,
                            value := Match },
                merge := Merge } = Spec, Id) ->

    R = reply_from(cmdb:merge(B, S, P, O, decoder(Match), Merge), Spec),
    cmcore:update(Id, R#{ bucket => B,
                          context => C });

effect_apply(#{ context := C,
                bucket := B,
                match := #{ subject := S,
                            predicate := P,
                            value := Match },
                merge := Merge } = Spec, Id) ->

    R = reply_from(cmdb:merge(B, S, P, decoder(Match), Merge), Spec),
    cmcore:update(Id, R#{ bucket => B,
                          context => C });

effect_apply(#{ context := C,
                bucket := B,
                match := #{ subject := S,
                            value := Match }, 
                merge := Merge }=Spec, Id) ->

    R = reply_from(cmdb:merge(B, S, decoder(Match), Merge), Spec),
    cmcore:update(Id, R#{ bucket => B,
                          context => C });

effect_apply(#{ context := C,
                bucket := B,
                match := #{ subject := S,
                            value := Match }}=Spec, Id) ->

    R = reply_from(cmdb:match(B, S, decoder(Match)), Spec),
    cmcore:update(Id, R#{ bucket => B,
                          context => C });

effect_apply(#{ context := C,
                bucket := B,
                get := #{ subject := S,
                          predicate := P,
                          object := O}}=Spec, Id)  ->

    R = reply_from(cmdb:get(B, S, P, O), Spec#{ single => true }),
    cmcore:update(Id, R#{ bucket => B,
                          context => C });

effect_apply(#{ context := C,
                bucket := B,
                get := #{ subject := S,
                          predicate := P}}=Spec, Id)  ->

    R = reply_from(cmdb:get(B, S, P), Spec),
    cmcore:update(Id, R#{ bucket => B,
                          context => C });

effect_apply(#{ context := C,
                bucket := B,
                get := #{ subject := S }}=Spec, Id)  ->

    R = reply_from(cmdb:get(B, S), Spec),
    cmcore:update(Id, R#{ bucket => B,
                          context => C });

effect_apply(#{ context := C,
                bucket := B,
                get := #{ subjects := S }}=Spec, Id)  ->

    R = lists:flatten(lists:map(fun(Subject) ->
                                        cmdb:get(B, Subject)
                                end, S)),

    R2 = reply_from(R, Spec),
    cmcore:update(Id, R2#{ bucket => B,
                           context => C });

effect_apply(#{ context := C,
                bucket := B,
                get := #{ subject := S,
                          predicate := P,
                          between := [O1, O2]}} = Spec, Id)  ->

    R = reply_from(cmdb:get(B, S, P, O1, O2), Spec),
    cmcore:update(Id, R#{ bucket => B,
                          context => C });

effect_apply(#{ context := C,
                bucket := B,
                get := Keys} = Spec, Id) when is_list(Keys) ->

    R = reply_from(cmdb:get(B, keys(Keys)), Spec),
    cmcore:update(Id, R#{ bucket => B,
                          context => C });

effect_apply(#{ context := C,
                bucket := B, 
                merge:= #{ subject := S1,
                           predicate := P1 },
                with := #{ subject := S2,
                           predicate := P2 }} = Spec, Id) ->

    R = reply_from(lists:flatten(lists:map(fun({_, _, O, V}) ->
                                                   lists:map(fun({S3, P3, O3, V2}) ->
                                                                     {S3, P3, O3, maps:merge(V2, V)}
                                                             end, cmdb:get(B, S2, P2, O))
                                           end, cmdb:get(B, S1, P1))), Spec),
    cmcore:update(Id, R#{ bucket => B,
                          context => C });

effect_apply(#{ context := C,
                bucket := B,
                map := #{ subject := S1,
                          predicate := P1 },
                with := #{ subject := S2,
                           predicate := P2 }}=Spec, Id ) ->

    R = reply_from(lists:flatten(lists:map(fun({_, _, _, V}) ->
                                                   cmdb:get(B, S2, P2, V)
                                           end, cmdb:get(B, S1, P1))), Spec),
    cmcore:update(Id, R#{ bucket => B,
                          context => C });


effect_apply(#{ context := C} = Spec, Id) ->
    cmkit:danger({cmeffect, db, unsupported, Spec}),
    R = reply_from({error, Spec }, Spec),
    cmcore:update(Id, R#{ context => C}).

decoder(Spec) ->
    #{ type => object,
       spec => Spec }.

key(#{ subject := S,
       predicate := P,
       object := O }) -> {S, P, O};

key(#{ subject := S,
       predicate := P}) -> {S, P};

key(#{ subject := S }) -> {S}.


value(#{ subject:= S,
         predicate := P,
         object := O,
         value := V}) -> {S, P, O, V}.


map({S, P, O, V}, #{ labels := #{ subject := SLabel,
                                  predicate := PLabel,
                                  object := OLabel,
                                  value := VLabel }}) ->
    #{ SLabel => S,
       PLabel => P,
       OLabel => O,
       VLabel => V };

map({S, P, O, V}, _) -> 
    #{ subject => S,
       predicate => P,
       object => O,
       value => V }.

keys(K) when is_list(K) -> lists:map(fun key/1, K);
keys(K) when is_map(K) -> [key(K)].


values(V) when is_list(V) -> lists:map(fun value/1, V);
values(V) when is_map(V) -> [value(V)].

reply_from(ok, _) -> #{ status => ok };

reply_from([Item], #{ single := true }=Spec) -> 
    #{ status => ok,
       data => map(Item, Spec) };

reply_from(Items, Spec) when is_list(Items) -> 
    #{ status => ok,
       data => lists:map(fun(I) ->
                                 map(I, Spec)
                         end, Items) };

reply_from({error, E}, _) -> 
    #{ status => error,
       error => E };

reply_from(Other, _) -> 
    #{ status => error,
       error => Other }.


restart([]) -> ok;
restart([B|Rest]) ->
    case cmdb:restart(cmkit:to_atom(B)) of 
        ok -> 
            restart(Rest);
        Other ->
            Other
    end.
