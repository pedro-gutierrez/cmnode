-module(cmconfig).
-export([
         buckets/0, 
         templates/0, 
         modules/0, 
         apps/0
        ]).

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
