-module(cmconfig_util).
-export([
         compile_port/1,
         compile_template/1,
         compile_module/1,
         compile_app/2,
         compile_bucket/1,
         compile_keyword/1,
         compile_decoders/1,
         compile_encoders/1,
         compile_updates/1,
         compile_term/1,
         resolve_modules/2,
         compile_assets/1,
         compile_effects/1
        ]).

compile_port(#{ <<"name">> := Name,
                <<"spec">> := #{
                    <<"port">> := Port,
                    <<"acceptors">> := Acceptors,
                    <<"apps">> := Apps }}) ->
    
    #{ name => compile_keyword(Name),
       port  => Port,
       acceptors => Acceptors,
       apps => compile_port_apps(Apps) }.

compile_port_apps(Apps) ->
    maps:fold(fun(Name, Mounts, List) ->
        [compile_port_app(Name, Mounts)|List]
               end, [], Apps).

compile_port_app(Name, Mounts) ->
    #{ name => compile_keyword(Name),
       mounts => compile_port_app_mounts(Mounts) }.

compile_port_app_mounts(Mounts) when is_map(Mounts) ->
    maps:fold(fun(Transport, Path, List) ->
                       [#{ transport => compile_keyword(Transport),
                         path => cmkit:to_list(Path) }|List]
               end, [], Mounts).


compile_template(#{ <<"name">> := Name,
                    <<"spec">> := #{
                        <<"contents">> := Contents,
                        <<"params">> := ParamsSpec }}) ->

    #{ name => cmkit:to_atom(Name),
         contents => Contents,
         params => cmconfig_util:compile_term(ParamsSpec) }.


compile_module(#{ <<"name">> := Name, <<"category">> := Cat,
            <<"spec">> := Spec }) ->

    
    #{ spec := CompiledSpec } = compile_spec(Spec),
    maps:merge(CompiledSpec,  #{
                 name => cmkit:to_atom(Name),
                 type => module,
                 category => cmconfig_util:compile_keyword(Cat)
    }).

compile_app(#{ <<"name">> := Name, <<"category">> := Cat,
            <<"spec">> := #{
                <<"modules">> := Modules 
               }}, Mods) ->
        
    #{ name => cmkit:to_atom(Name),
       type => app,
       category => cmconfig_util:compile_keyword(Cat),
       modules  => cmconfig_util:resolve_modules(Modules, Mods)
     }.

compile_bucket(#{ <<"name">> := Name,
                  <<"spec">> := #{ <<"hosts">> := Hosts,
                                   <<"storage">> := Storage
                                 }}) ->

    #{ name => cmkit:to_atom(Name),
       storage =>  cmkit:to_atom(Storage),
       hosts => Hosts }.


compile_spec(Spec) ->
    
    Keys = [<<"decoders">>, <<"encoders">>, <<"init">>,
            <<"update">>, <<"views">>, <<"effects">> ],
    
    Contents = lists:foldl(fun(Key, C0) ->
                                   Term = #{ Key => maps:get(Key, Spec, #{})},
                                   case compile_term(Term) of 
                                        [] -> C0;
                                        [_|_]=Other -> C0#{ compile_keyword(Key) => Other };
                                        Map when map_size(Map) =:= 0 -> C0;
                                        Other -> C0#{ compile_keyword(Key) => Other }
                                   end
                           end, #{}, Keys),
    #{ spec => Contents }.

compile_term(#{ <<"decoders">> := Decs }) ->
    compile_decoders(Decs);

compile_term(#{ <<"encoders">> := Encs }) ->
    compile_encoders(Encs);

compile_term(#{ <<"init">> := Init }) ->
    compile_init(Init);

compile_term(#{ <<"update">> := Update }) ->
    compile_updates(Update);

compile_term(#{ <<"views">> := Views }) ->
    compile_views(Views);

compile_term(#{ <<"effects">> := Effects }) ->
    compile_effects(Effects);

compile_term(#{ <<"data">> := <<"any">> }) ->
    #{ type => data };


compile_term(#{ <<"data">> := Spec }) ->
    maps:merge(#{ type => data},
               compile_term(Spec));

compile_term(#{ <<"object">> := Object}) ->
    compile_object(Object);

compile_term(#{ <<"view">> := View }) ->
    #{ type => view ,
       spec => compile_view(View)
     };

compile_term(#{ <<"list">> := <<"any">>}) ->
    #{ type => list };

compile_term(#{ <<"list">> := Items }) when is_list(Items) ->
    #{ type => list,
       value => lists:map( fun compile_term/1, Items) 
     };

compile_term(#{ <<"spec">> := Spec }) ->
   compile_spec(Spec); 

compile_term(#{ <<"type">> := Type}) ->
    compile_keyword(Type);

compile_term(#{ <<"keyword">> := Keyword }) ->
    #{ type => keyword,
       value => cmkit:to_atom(Keyword) };

compile_term(#{ <<"number">> := _ }) ->
    #{ type => number };

compile_term(#{ <<"literal">> := Text }) ->
    #{ value => Text };

compile_term(#{ <<"text">> := <<"any">> }) ->
    #{ type => text };

compile_term(#{ <<"text">> := Spec }) ->
    maps:merge(#{ type => text},
               compile_term(Spec));
    
compile_term(#{ <<"from">> := From,
                <<"at">> := At }) ->
    #{ from => compile_from(From),
       at => compile_keyword(At) };

compile_term(<<"from_data">>) -> from_data;

compile_term(#{ <<"from">> := From  }) ->
    #{ from => compile_from(From) }.



compile_from(From) when is_binary(From)-> compile_keyword(From);
compile_from(From) when is_map(From) ->
    compile_term(From).

compile_object(<<"any">>) -> 
    #{ type => object };

compile_object(Map) when is_map(Map) ->
    #{ type => object,
       spec => compile_object(maps:keys(Map), Map, #{}) }.

compile_object([], _, Out) -> Out;
compile_object([K|Rem], Map, Out) ->
    compile_object(Rem, Map, Out#{ 
                               compile_keyword(K) => compile_term(maps:get(K, Map))
                              }).

compile_keyword(K) -> cmkit:to_atom(K).

compile_decoders(Decs) ->
    compile_decoders(maps:keys(Decs), Decs, []).

compile_decoders([], _, Out) -> Out;
compile_decoders([K|Rem], Decs, Out) ->
    Msg = compile_keyword(K),
    Dec = compile_term(maps:get(K, Decs)),
    compile_decoders(Rem, Decs, [#{ msg => Msg,
                                    spec => Dec}|Out]). 

compile_encoders(Encs) ->
    compile_encoders(maps:keys(Encs), Encs, #{}).

compile_encoders([], _, Out) -> Out;
compile_encoders([K|Rem], Encs, Out) ->
    Name = compile_keyword(K),
    Enc = compile_term(maps:get(K, Encs)),
    compile_encoders(Rem, Encs, Out#{ Name => Enc}). 


compile_updates(Updates) -> 
    compile_updates(maps:keys(Updates), Updates, #{}).

compile_updates([], _, Out) -> Out;
compile_updates([K|Rem], Updates, Out) ->
    Name = compile_keyword(K),
    Init = compile_init(maps:get(K, Updates)),
    compile_updates(Rem, Updates, Out#{ Name => Init }).

compile_init(#{ <<"model">> := Model,
                   <<"cmds">> := Cmds }) ->
    #{ model => compile_model(Model),
       cmds => compile_cmds(Cmds) };

compile_init(#{ <<"model">> := Model }) ->
    #{ model =>  compile_model(Model) };

compile_init(#{ <<"cmds">> := Cmds }) ->
    #{ cmds =>  compile_cmds(Cmds) };

compile_init(_) -> #{}.

compile_views(Views) ->
    compile_views(maps:keys(Views), Views, #{}).

compile_views([], _, Out) -> Out;
compile_views([K|Rem], Views, Out) ->
    Name = compile_keyword(K),
    View = compile_view(maps:get(K, Views)),
    compile_views(Rem, Views, Out#{ Name => View }).


compile_view(#{ <<"view">> := View,
                <<"when">> := When }) ->
    
    #{ view => compile_keyword(View),
       condition => compile_condition(When) };


compile_view(#{ <<"tag">> := Tag,
                <<"attrs">> := Attrs,
                <<"children">> := Children }) ->
    #{ tag => Tag,
       attrs => Attrs,
       children => lists:map(fun compile_view/1, Children) };

compile_view(#{ <<"tag">> := _ ,
                <<"children">> := _ }=View) ->
    compile_view(View#{ <<"attrs">> => #{}});

compile_view(#{ <<"tag">> := _ ,
                <<"attrs">> := _ }=View) ->
    compile_view(View#{ <<"children">> => []});

compile_view(#{ <<"tag">> := _ }=View) ->
    compile_view(View#{ 
                   <<"attrs">> => #{},
                   <<"children">> => []});

compile_view(#{ <<"text">> := Text}) when is_binary(Text) ->
    #{ text => #{ literal => Text }};

compile_view(#{ <<"text">> := Spec}) ->
    #{ text => compile_term(Spec) }.


compile_condition(Prop) when is_binary(Prop) ->
    #{ is_set => Prop }.

compile_model(Map) -> compile_object(Map).

compile_cmds(Cmds) ->
    lists:map(fun compile_cmd/1, Cmds).

compile_cmd(#{ <<"effect">> := Effect, 
               <<"encoder">> := Encoder }) ->
    #{ effect => compile_keyword(Effect),
       encoder => compile_keyword(Encoder) };

compile_cmd(#{ <<"effect">> := Effect  }) ->
    #{ effect => compile_keyword(Effect) }.

resolve_modules(Names, All) ->
    lists:map(fun(Name) ->
                    resolve_module(Name, All)
              end, Names).

resolve_module(Name, []) -> #{ status => unknown,
                               name => compile_keyword(Name) };

resolve_module(Name, [#{ name := Name2}=Mod|Rest]) ->
    case cmkit:to_bin(Name2) of 
        Name -> Mod;
        _ -> resolve_module(Name, Rest)
    end.
    

compile_assets(Assets) when is_map(Assets) ->
compile_assets(maps:keys(Assets), Assets, []).
compile_assets([], _, Out) -> Out;
compile_assets([K|Rem], Assets, Out) ->
    Asset = compile_asset(K, maps:get(K, Assets)),
    compile_assets(Rem, Assets, [Asset|Out]). 

compile_asset(Filename, #{ 
                <<"path">> := Path,
                <<"spec">> := #{
                    <<"type">> := Type, 
                    <<"name">> := Name }}) ->
    #{ type => compile_keyword(Type),
       name => compile_keyword(Name),
       path => filename:join([cmkit:to_list(Path),
                              cmkit:to_list(Filename)])
     }.

compile_effects(Effects) ->
    compile_effects(maps:keys(Effects), Effects, #{}).

compile_effects([], _, Out) -> Out;
compile_effects([K|Rem], Effects, Out) ->
    Name = compile_keyword(K),
    compile_effects(Rem, Effects, Out#{ Name => compile_effect(Name, maps:get(K, Effects)) }).

compile_effect(Name, #{ <<"type">> := Type, <<"settings">> := Settings}) ->
    #{ type => effect,
       name => Name,
       class => Type,
       settings => Settings };

compile_effect(Name, #{ <<"type">> := _ }=Effect) -> compile_effect(Name, Effect#{ <<"settings">> => #{}}).
