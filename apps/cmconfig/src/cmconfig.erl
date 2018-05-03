-module(cmconfig).
-export([
         buckets/0, 
         templates/0, 
         modules/0,
         module/1,
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

module(Name) ->
    case cmyamls:of_type_name(module, Name) of 
        [{ok, Spec}] -> {ok, cmconfig_util:compile_module(Spec)};
        [] -> {error, not_found};
        Other -> {error, Other}
    end.

apps() ->
    Mods = modules(),
    [ cmconfig_util:compile_app(Spec, Mods) 
      || {ok, Spec} <- cmyamls:of_type(app) ].


app(Name) ->
    app(Name, apps()).

app(_, []) -> {error, not_found};
app(Name, [#{ name := Name}=App|_]) when is_atom(Name) -> {ok, App};
app(Name, [#{ name := AppName}=App|Rem]) when is_binary(Name) -> 
    case cmkit:to_bin(AppName) of 
        Name -> {ok, App};
        _ -> app(Name, Rem)
    end;
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
