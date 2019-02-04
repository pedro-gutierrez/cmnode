-module(cmeffect_db).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> db.

effect_apply(#{ context := C,
                bucket := B,
                reset := _ }=Spec, Id) ->
    
    R = reply_from(cmdb:reset(B), Spec),
    cmcore:update(Id, R#{ context => C });

effect_apply(#{ context := C,
                bucket := B,
                put := Values }=Spec, Id) ->

    R = reply_from(cmdb:put(B, values(Values)), Spec),
    cmcore:update(Id, R#{ context => C });

effect_apply(#{ context := C,
                bucket := B,
                insert := Values }=Spec, Id) ->

    R = reply_from(cmdb:insert(B, values(Values)), Spec),
    cmcore:update(Id, R#{ context => C });

effect_apply(#{ context := C,
                bucket := B,
                subject := S,
                predicate := P,
                match := Match,
                merge := Merge } = Spec, Id) ->

    R = reply_from(cmdb:merge(B, S, P, decoder(Match), Merge), Spec),
    cmcore:update(Id, R#{ context => C });

effect_apply(#{ context := C,
                bucket := B,
                subject := S,
                match := Match,
                merge := Merge }=Spec, Id) ->

    R = reply_from(cmdb:merge(B, S, decoder(Match), Merge), Spec),
    cmcore:update(Id, R#{ context => C });


effect_apply(#{ context := C,
                bucket := B,
                get := #{ subject := S,
                          predicate := P,
                          object := O}}=Spec, Id)  ->

    R = reply_from(cmdb:get(B, S, P, O), Spec),
    cmcore:update(Id, R#{ context => C });

effect_apply(#{ context := C,
                bucket := B,
                get := #{ subject := S,
                          predicate := P}}=Spec, Id)  ->

    R = reply_from(cmdb:get(B, S, P), Spec),
    cmcore:update(Id, R#{ context => C });

effect_apply(#{ context := C,
                bucket := B,
                get := #{ subject := S }}=Spec, Id)  ->

    R = reply_from(cmdb:get(B, S), Spec),
    cmcore:update(Id, R#{ context => C });

effect_apply(#{ context := C,
                bucket := B,
                get := #{ subjects := S }}=Spec, Id)  ->
    
    R = lists:flatten(lists:map(fun(Subject) ->
                                    cmdb:get(B, Subject)
                                end, S)),
    
    R2 = reply_from(R, Spec),
    cmcore:update(Id, R2#{ context => C });

effect_apply(#{ context := C,
                bucket := B,
                get := #{ subject := S,
                          predicate := P,
                          between := [O1, O2]}} = Spec, Id)  ->

    R = reply_from(cmdb:get(B, S, P, O1, O2), Spec),
    cmcore:update(Id, R#{ context => C });

effect_apply(#{ context := C,
                bucket := B,
                get := Keys} = Spec, Id) when is_list(Keys) ->

    R = reply_from(cmdb:get(B, keys(Keys)), Spec),
    cmcore:update(Id, R#{ context => C });

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

reply_from([Item], Spec) -> 
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
