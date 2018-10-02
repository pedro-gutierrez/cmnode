-module(cmnode_effect_db_get).
-export([ effect_info/0,
          effect_apply/2
        ]).
-define(REL, is).

effect_info() -> db_get.


effect_apply(#{ context := C,
                bucket := B,
                subject := S, 
                predicate := P,
                labels := #{ object := OLabel,
                             value := VLabel }}, SessionId) ->

    R = case cmdb:get(B, S, P) of 
            {ok, Entries} -> 
                #{ context => C,
                   value => [ #{ OLabel => O, 
                                 VLabel => V }  || {_, _, O, V} <- Entries] };
            {error, E }-> 
                #{ context => C,
                   error => E }
        end,
    cmcore:update(SessionId, R);

effect_apply(#{ context := C,
                bucket := B,
                subject := S, 
                labels := #{ predicate := PLabel,
                             object := OLabel,
                             value := VLabel }}, SessionId) ->

    R = case cmdb:get(B, S) of 
            {ok, Entries} -> 
                #{ context => C,
                   value => [ #{ PLabel => P,
                                 OLabel => O, 
                                 VLabel => V }  || {_, P, O, V} <- Entries] };
            {error, E }-> 
                #{ context => C,
                   error => E }
        end,
    cmcore:update(SessionId, R);

effect_apply(#{ bucket := Db, 
                type := Type, 
                id := Id } = Q, SessionId) ->

    R = case cmdb:get(Db, Type, ?REL, Id) of 
            {ok, []} -> Q#{ error => not_found };
            {ok, [{_, _, _, Value}]} -> Q#{ value => Value};
            {ok, Other}-> Q#{ error => Other };
            {error, E }-> Q#{ error => E }
        end,
    cmcore:update(SessionId, R);

effect_apply(#{  bucket := Db, 
                 type := Type } = Q, SessionId) ->
    cmcore:update(SessionId, case cmdb:get(Db, Type, ?REL) of
                                 {ok, Entries} -> 
                                     Q#{ values => [ V || {_, _, _, V} <- Entries] };
                                 {error, E}-> Q#{ error => E }
                             end);


effect_apply(#{ query := #{ bucket := Db, 
                            type := Type, 
                            id := Id } = Q,
                join := J }, SessionId) ->

    R = case cmdb:get(Db, Type, ?REL, Id) of 
            {ok, []} -> 
                Q#{ error => not_found };
            {error, E }-> 
                Q#{ error => E };
            {ok, Items} -> 
                Items2 = lists:map(fun({_, _, _, I}) ->
                                           join(J, I)
                                   end, Items),
                Q#{ value => value(Items2, maps:get(all, Q, false))}
        end,

    cmcore:update(SessionId, R).

value([V], false) -> V;
value([V|_], false) -> V;
value(Items, _) -> Items.


join(Join, I) -> 
    join(maps:keys(Join), Join, I).

join([], _, Out) -> Out;
join([K|Rem], Join, Out) ->
    join(Rem, Join, item_with_joined_prop(K, Join, Out)).

item_with_joined_prop(K, Join, Item) ->
    case maps:get(K, Join) of 
        #{ bucket := Bucket,
           type := Type } -> 
            
            Item2 = case maps:get(K, Item, undef) of 
                undef -> 
                    cmkit:warning({cmnode_effect_db_get, 
                                   no_such_prop_to_join, K, Item}),
                    Item;
                Id when is_binary(Id) ->
                            case resolve(Bucket, Type, Id) of 
                                {ok, Value} -> 
                                    Item#{ K => Value};
                                {error, E} -> 
                                    cmkit:warning({cmnode_effect_db_get,
                                                   prop_not_resolved, K, Item,
                                                   E}),
                                    Item
                            end;
                [_|_] = Ids ->
                    Values = [ V || {ok, V} <- lists:map(fun(Id) ->
                                                                   resolve(Bucket, Type, Id)
                                                           end, Ids)] ,
                    Item#{ K => Values};
                Other ->
                    cmkit:warning({cmnode_effect_db_get, 
                                   unsupported_join_reference, K, Item, Other}),
                    Item
            end,

            Item2;
        Other -> 
            cmkit:warning({cmnode_effect_db_get, unsupported_join_spec, K, Item, Other}),
            Item
    end.

resolve(Bucket, Type, Id) -> 
    case cmdb:get(Bucket, Type, ?REL, Id) of 
        {ok, []} -> 
            {error, #{ reason => not_found,
                       bucket => Bucket,
                       type => Type,
                       id => Id }};

        {ok, [{_, _, _, Value}]} ->
            {ok, Value};
        {ok, _} ->
            {error, #{ reason => too_many_values,
                       bucket => Bucket,
                       type => Type,
                       id => Id}};
        {error, E} ->
            {error, #{ reason => error,
                       bucket => Bucket,
                       type => Type,
                       id => Id,
                       info => E }}
    end.
