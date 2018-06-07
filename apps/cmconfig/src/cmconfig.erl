-module(cmconfig).
-export([
         tests/0,
         test/1,
         buckets/0, 
         templates/0, 
         modules/0,
         module/1,
         ports/0,
         port/1,
         apps/0,
         app/1,
         mount/3,
         effects/0,
         queues/0,
         queue/1,
         settings/0,
         settings/1,
         settings/2,
         crons/0,
         cron/1
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
queues() -> all(queue).
queue(Name) -> find(queue, Name).
settings() -> all(settings).
settings(Name) -> find(settings, Name).
settings(Name, true) -> 
    case settings(Name) of 
        {ok, #{ spec := Spec}} ->
            cmencode:encode(Spec);
         Other -> Other
    end;

settings(Name, false) -> settings(Name).

crons() -> all(cron).
cron(Name) -> find(cron, Name).


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
                              cmkit:to_bin(T) =:= cmkit:to_bin(Transport)
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
