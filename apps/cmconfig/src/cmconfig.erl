-module(cmconfig).
-export([
         tests/0,
         test/1,
         scenario/2,
         buckets/0, 
         templates/0, 
         modules/0,
         module/1,
         ports/0,
         port/1,
         apps/0,
         app/1,
         mount/3,
         effects/0
        ]).

all(Type) -> cmconfig_cache:all(Type).
find(Type, Id) -> cmconfig_cache:find(Type, Id).
ports() -> all(port).
port(Name) -> find(port, Name).
templates() -> all(template).
modules() -> all(module).
apps() -> all(app).
buckets() -> all(bucket).
module(Name) -> find(module, Name).
app(Name) -> find(app, Name).

mount(App, Port, Transport) ->
    case port(Port) of 
        {ok, #{ port := PortNumber,
                apps := Apps }} ->
            case mount(Transport, lists:filter(fun(#{ name := AppName }) ->
                                       AppName =:= App
                               end, Apps)) of 
                {ok, Mount} -> {ok, Mount#{ port => PortNumber }};
                Other -> Other
            end;
        Other -> Other
    end.

mount(_, []) -> {error, not_found};
mount(Transport, [#{ mounts := Mounts }]) ->
    case lists:filter(fun(#{ transport := T }) ->
                              T =:= Transport
                      end, Mounts) of 
        [] -> {error, not_found};
        [M] -> {ok, M};
        Other -> {error, Other}
    end.



effect_contract() ->
  [{effect_info, 0}, 
   {effect_apply, 2}
  ].

effects() ->
    [ #{ name => M:effect_info(),
         mod => M } || M <-erlang:loaded(), cmkit:implements(M, effect_contract())].    

tests() -> all(test).
test(Name) -> find(test, Name).

scenario(Test, Scenario) ->
    case test(Test) of 
        {ok, #{ scenarios := Scenarios}} ->
            case lists:filter(fun(#{ title := Title}) -> 
                                      cmkit:to_lower(Scenario) =:= cmkit:to_lower(Title)
                              end, Scenarios) of 
                [Spec] -> {ok, Spec#{ test => Test} };
                [] -> {error, not_found};
                Other -> {error, Other}
            end;
        Other -> Other
    end.
