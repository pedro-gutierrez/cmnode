-module(cmeffect_store).
-export([ effect_info/0,
          effect_apply/2
        ]).

effect_info() -> store.

effect_apply(#{ context := C,
                store := Store,
                reset := _ }, Pid) ->

    R = result(C, Store, cmstore:reset(Store)),
    cmcore:update(Pid, R);

effect_apply(#{ context := C, 
                store := Store,
                read := Spec }, Pid) ->

    Res = case cmstore:read(Store, Spec) of 
              {ok, [Single]} -> 
                  {ok, Single};
              Other -> 
                  Other
          end,

    cmcore:update(Pid, result(C, Store, Res));

effect_apply(#{ context := C,
                store := Store,
                write := #{ app := _,
                            kind := K,
                            id := Id,
                            event := Ev,
                            data := _ } = Event}, Pid) ->

    case cmstore:write(Store, Event) of 
        ok ->
            pub([[K, Id], [K, Ev]], Event),
            cmcore:update(Pid, result(C, Store, ok));
        Other  ->
            cmcore:update(Pid, result(C, Store, Other))
    end.

pub([], _) -> ok;
pub([T|Rem], Event) ->
    case cmbus:pub(T, Event, closest) of
        ok ->
            pub(Rem, Event);
        {error, E} ->
            cmkit:warning({effect, store, T, E}),
            pub(Rem, Event)
    end.


result(C, S, ok) -> 
    #{ store => S,
       context => C,
       status => ok };

result(C, S, {ok, Data}) -> 
    #{ store => S,
       context => C,
       status => ok,
       data => Data };

result(C, S, {error, E}) -> 
    #{ store => S,
       context => C,
       status => error,
       error => E }.
