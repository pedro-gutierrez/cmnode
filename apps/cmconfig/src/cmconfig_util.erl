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

compile_app(#{ <<"name">> := Name, 
               <<"category">> := Cat,
               <<"spec">> := #{
                   <<"modules">> := Modules 
                  } = Spec
             }, Mods) ->


        
    #{ name => cmkit:to_atom(Name),
       type => app,
       config => compile_config(maps:get(<<"config">>, Spec, #{})),
       debug => compile_keyword(maps:get(<<"debug">>, Spec, <<"false">>)),
       category => cmconfig_util:compile_keyword(Cat),
       spec => compile_modules(cmconfig_util:resolve_modules(Modules, Mods), #{})
     }.

compile_bucket(#{ <<"name">> := Name,
                  <<"spec">> := #{ <<"hosts">> := Hosts,
                                   <<"storage">> := Storage
                                 }}) ->

    #{ name => cmkit:to_atom(Name),
       storage =>  cmkit:to_atom(Storage),
       hosts => Hosts }.

compile_spec(#{ <<"modules">> := Modules}) when is_list(Modules) ->
    Out = #{ spec => compile_modules(lists:map(fun cmconfig:module/1, Modules), #{})},
    Out;


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

compile_config(Spec) -> 
    case cmencode:encode(compile_object(Spec)) of 
        {ok, Config} -> Config;
        {error, E} -> 
            cmkit:log({cmconfig, invalid_config, Spec, E}),
            #{}
    end.

compile_modules([], #{ decoders := Decs }=Spec) -> 
    Spec#{ decoders => sort_decoders(Decs) };

compile_modules([], Spec) -> Spec;
    
compile_modules([{ok, ModSpec}|Rest], Spec) ->
    compile_modules([ModSpec|Rest], Spec);

compile_modules([ModSpec|Rest], Spec) when is_map(ModSpec) ->
    compile_modules(Rest, merge_spec([decoders, 
                                      encoders, 
                                      init, 
                                      update, 
                                      views, 
                                      effects
                                     ], ModSpec, Spec));
    

compile_modules([_|Rest], Spec) -> 
    compile_modules(Rest, Spec).


merge_spec([], _, Spec) -> Spec;
merge_spec([decoders|Rem], Spec, Spec0) ->
    Decs0 = maps:get(decoders, Spec0, []),
    Decs = maps:get(decoders, Spec, []),
    merge_spec(Rem, Spec, maps:put(decoders, Decs ++ Decs0, Spec0));

merge_spec([init|Rem], 
           #{ init := [ #{ model := Model, cmds := Cmds }]}=Spec, 
           #{ init := #{ model := Model0, cmds := Cmds0 }}=Spec0) ->
           merge_spec(Rem, Spec, 
                      maps:put(init, #{ model => merge_model_spec(Model, Model0),
                                        cmds => merge_cmds_spec(Cmds, Cmds0) 
                                      }, Spec0));

merge_spec([init|Rem], 
           #{ init := [ #{ model := Model, cmds := Cmds }]}=Spec, Spec0) ->
           merge_spec(Rem, Spec, 
                      maps:put(init, #{ model => Model, 
                                        cmds => Cmds 
                                      }, Spec0));

merge_spec([update|Rem], Spec, Spec0) ->
           merge_spec(Rem, Spec, 
                      maps:put(update, merge_updates_spec( 
                                         maps:get(update, Spec, #{}), 
                                         maps:get(update, Spec0, #{})), Spec0));

merge_spec([Key|Rem], Spec, Spec0) ->
    merge_spec(Rem, Spec, 
               maps:put(Key, maps:merge(maps:get(Key, Spec0, #{}),  maps:get(Key, Spec, #{})), Spec0)).

merge_model_spec(#{ spec := S1 }, #{ spec := S2}) -> 
    #{ type => object,
       spec => maps:merge(S2, S1) 
     }.

merge_cmds_spec(Cmds1, Cmds2) -> Cmds1 ++ Cmds2. 

merge_updates_spec(Updates1, Updates2) when map_size(Updates2) =:= 0 -> Updates1;
merge_updates_spec(Updates1, Updates2) ->
    merge_updates_spec(maps:keys(Updates1), Updates1, Updates2).

merge_updates_spec([], _, Out) -> Out;
merge_updates_spec([K|Rem], U1, U2) ->
    merge_updates_spec(Rem, U1, 
                       maps:put(K, maps:get(K, U2, []) ++ maps:get(K, U1), U2)).

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

compile_term(#{ <<"config">> := Key }) ->
    #{ type => config,
       spec => compile_keyword(Key) 
     };

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

compile_term(#{ <<"list">> := Spec }) when is_map(Spec) ->
    #{ type => list,
       spec => compile_term(Spec)
     };

compile_term(#{ <<"spec">> := Spec }) ->
   compile_spec(Spec); 

compile_term(#{ <<"type">> := Type}) ->
    compile_keyword(Type);

compile_term(#{ <<"keyword">> := Keyword }) when is_binary(Keyword) ->
    #{ type => keyword,
       value => cmkit:to_atom(Keyword) };

compile_term(#{ <<"keyword">> := Keyword }) when is_atom(Keyword) ->
    #{ type => keyword,
       value => Keyword };

compile_term(#{ <<"keyword">> := Spec }) when is_map(Spec) ->
    #{ type => keyword,
       spec => compile_term(Spec) };

compile_term(#{ <<"number">> := _ }) ->
    #{ type => number };

compile_term(#{ <<"literal">> := Text }) ->
    #{ value => Text };

compile_term(#{ <<"same_as">> := Prop }) ->
    #{ constraint => equal, 
       relates_to => cmkit:to_atom(Prop) 
     };

compile_term(#{ <<"text">> := <<"any">> }) ->
    #{ type => text };

compile_term(#{ <<"any">> := <<"tetx">> }) ->
    #{ type => text };

compile_term(#{ <<"email">> := <<"any">> }) ->
    #{ type => email };

compile_term(#{ <<"any">> := <<"email">> }) ->
    #{ type => email };

compile_term(#{ <<"any">> := <<"keyword">> }) ->
    #{ type => keyword };

compile_term(#{ <<"text">> := Spec }) ->
    maps:merge(#{ type => text},
               compile_term(Spec));
    
compile_term(#{ <<"from">> := From,
                <<"at">> := At }) when is_binary(At) ->
    #{ from => compile_from(From),
       at => compile_keyword(At) };

compile_term(#{ <<"from">> := From,
                <<"at">> := At }) when is_map(At) ->
    #{ from => compile_from(From),
       at => compile_term(At) };



compile_term(<<"from_data">>) -> from_data;

compile_term(#{ <<"from">> := From  }) ->
    #{ from => compile_from(From) };

compile_term(#{ <<"one_of">> := Specs }) when is_list(Specs) ->
    #{ one_of => lists:map(fun compile_term/1, Specs) }; 


compile_term(#{ <<"loop">> := From,
                <<"with">> := View }) when is_map(From) ->

    #{ loop => compile_term(From),
       with => compile_keyword(View) };

compile_term(#{ <<"loop">> := From,
                <<"with">> := _ } = Spec) when is_binary(From) ->

    compile_term(Spec#{ <<"loop">> => #{ <<"from">> => From }});

compile_term(Text) when is_binary(Text) -> Text.

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

compile_decoders([], _, Out) -> sort_decoders(Out);
compile_decoders([K|Rem], Decs, Out) ->
    Msg = compile_keyword(K),
    Dec = maps:get(K, Decs),
    Spec = compile_term(Dec),
    compile_decoders(Rem, Decs, [#{ msg => Msg,
                                    priority => compile_priority(Dec),
                                    spec => Spec}|Out]). 

compile_priority(#{ <<"priority">> := P }) ->
    compile_keyword(P);

compile_priority(_) ->
    compile_keyword(normal).


sort_decoders(Decs) -> lists:sort(fun compare_priorities/2, Decs).

compare_priorities(#{ priority := _ }, #{ priority := lowest }) -> true;
compare_priorities(#{ priority := lowest }, #{ priority := _ }) -> false;
compare_priorities(#{ priority := _ }, #{ priority := _ }) -> true.




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

compile_update_spec(Spec) when is_map(Spec) -> 
    #{ condition => compile_condition(maps:get(<<"when">>, Spec, [])), 
       model => compile_model(maps:get(<<"model">>, Spec, #{})),
       cmds => compile_cmds(maps:get(<<"cmds">>, Spec, [])) 
     }.

compile_init(Spec) when is_map(Spec) ->
    compile_init([Spec]);

compile_init(Specs) when is_list(Specs) -> 
    lists:map(fun compile_update_spec/1, Specs).

compile_views(Views) ->
    compile_views(maps:keys(Views), Views, #{}).

compile_views([], _, Out) -> Out;
compile_views([K|Rem], Views, Out) ->
    Name = compile_keyword(K),
    View = compile_view(maps:get(K, Views)),
    compile_views(Rem, Views, Out#{ Name => View }).


compile_view(#{ <<"view">> := View,
                <<"params">> := Params,
                <<"when">> := When }) ->
    
    #{ view => compile_keyword(View),
       params => compile_term(Params),
       condition => compile_condition(When) };

compile_view(#{ <<"view">> := View,
                <<"when">> := When }) ->
    
    #{ view => compile_keyword(View),
       condition => compile_condition(When) };
    

compile_view(#{ <<"view">> := View,
                <<"params">> := Params }) ->
    
    #{ view => compile_keyword(View),
       params  => compile_term(Params) };

compile_view(#{ <<"tag">> := Tag,
                <<"attrs">> := Attrs,
                <<"children">> := Children }) when is_map(Children)->
    #{ tag => Tag,
       attrs => compile_view_attrs(Attrs),
       children => compile_term(Children) 
     };

compile_view(#{ <<"tag">> := Tag,
                <<"attrs">> := Attrs,
                <<"children">> := Children }) when is_list(Children)->
    #{ tag => Tag,
       attrs => compile_view_attrs(Attrs),
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
    #{ text => compile_term(Spec) };


compile_view(#{ <<"iterate">> := From,
                <<"using">> := ItemView }) ->
    #{ iterate => compile_term(From), 
       using => compile_keyword(ItemView) }.


compile_view_attrs(Attrs) when is_map(Attrs) ->
    maps:fold(fun(K, V, Attrs2) ->
                      Attrs2#{ compile_keyword(K) => compile_term(V) }
              end, #{}, Attrs). 

compile_condition(Prop) when is_binary(Prop) ->
    #{ type => present,
       spec => [ compile_keyword(Prop) ] };

compile_condition(#{ <<"eq">> := Spec }) ->
    #{ type => equal,
       spec => compile_object(maps:keys(Spec), Spec, #{}) 
     };

compile_condition(#{ <<"member">> := Spec }) ->
    #{ type => member,
       spec => compile_object(maps:keys(Spec), Spec, #{}) 
     };

compile_condition(#{ <<"all">> := Conds }) ->
    #{ type => all,
       spec => lists:map(fun compile_condition/1, Conds) 
     };

compile_condition([]) -> 
    #{ type => true }.

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
       settings => compile_object(maps:keys(Settings), Settings, #{}) 
     };

compile_effect(Name, #{ <<"type">> := _ }=Effect) -> compile_effect(Name, Effect#{ <<"settings">> => #{}}).
