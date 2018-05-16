-module(cmconfig).
-export([
         tests/0,
         test/1,
         buckets/0, 
         templates/0, 
         modules/0,
         module/1,
         ports/0,
         apps/0,
         app/1,
         effects/0
        ]).

all(Type) -> cmconfig_cache:all(Type).
find(Type, Id) -> cmconfig_cache:find(Type, Id).
ports() -> all(port).
templates() -> all(template).
modules() -> all(module).
apps() -> all(app).
buckets() -> all(bucket).
tests() -> all(test).
test(Name) -> find(test, Name).
module(Name) -> find(module, Name).
app(Name) -> find(app, Name).

effect_contract() ->
  [{effect_info, 0}, 
   {effect_apply, 2}
  ].

effects() ->
    [ #{ name => M:effect_info(),
         mod => M } || M <-erlang:loaded(), cmkit:implements(M, effect_contract())].    
