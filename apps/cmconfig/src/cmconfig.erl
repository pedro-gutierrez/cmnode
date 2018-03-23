-module(cmconfig).
-export([
         buckets/0, 
         templates/0, 
         modules/0, 
         ports/0,
         apps/0
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

buckets() ->
    [ cmconfig_util:compile_bucket(Spec) 
      || {ok, Spec} <- cmyamls:of_type(bucket) ].
