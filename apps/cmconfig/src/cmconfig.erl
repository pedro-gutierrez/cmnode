-module(cmconfig).
-export([
         buckets/0, 
         templates/0, 
         modules/0, 
         ports/0,
         apps/0,
         app/1,
         effects/0
        ]).

ports() ->
    [ cmconfig_util:compile_port(Port) 
      || {ok, Port } <- cmyamls:of_type(port) ].


templates() ->
    [ cmconfig_util:compile_template(Tpl) 
      || {ok, Tpl } <- cmyamls:of_type(template) ].

modules() ->
    [ cmconfig_util:compile_module(Spec) 
      || {ok, Spec } <- cmyamls:of_type(module) ].

apps() ->
    Mods = modules(),
    [ cmconfig_util:compile_app(Spec, Mods) 
      || {ok, Spec} <- cmyamls:of_type(app) ].


app(Name) ->
    app(Name, apps()).

app(_, []) -> {error, not_found};
app(Name, [#{ name := Name}=App|_]) -> {ok, App};
app(Name, [_|Rem]) -> app(Name, Rem).

buckets() ->
    [ cmconfig_util:compile_bucket(Spec) 
      || {ok, Spec} <- cmyamls:of_type(bucket) ].

effect_contract() ->
  [{effect_info, 0}, 
   {effect_apply, 2}
  ].

effects() ->
    [ #{ name => M:effect_info(),
         mod => M } || M <-erlang:loaded(), cmkit:implements(M, effect_contract())].    
