-module(cmetc).
-export([buckets/0, tpls/0, mods/0, apps/0]).

tpls() ->
    [ cmetc_util:compile_template(Tpl) 
      || {ok, Tpl } <- cmyamls:of_type(template) ].

mods() ->
    [ cmetc_util:compile_module(Spec) 
      || {ok, Spec } <- cmyamls:of_type(module) ].

apps() ->
    Mods = mods(),
    [ cmetc_util:compile_app(Spec, Mods) 
      || {ok, Spec} <- cmyamls:of_type(app) ].

buckets() ->
    [ cmetc_util:compile_bucket(Spec) 
      || {ok, Spec} <- cmyamls:of_type(bucket) ].
