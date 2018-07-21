-module(cmconfig_util).
-export([
         parse/0,
         rank/1,
         ranked/1,
         compile/1,
         deps/1,
         compile_port/1,
         compile_template/1,
         compile_module/1,
         compile_app/1,
         compile_bucket/1,
         compile_keyword/1,
         compile_decoders/1,
         compile_encoders/1,
         compile_updates/1,
         compile_term/1,
         resolve_modules/2,
         compile_assets/1,
         compile_effects/1,
         compile_test/1,
         compile_settings/1
        ]).

parse() ->
    case lists:foldr(fun({yaml, _, {ok, Spec}}, {Specs, Errors}) ->
                        {[Spec|Specs], Errors};
                   ({yaml, Filename, {error, E}}, {Specs, Errors}) ->
                        {Specs, [{Filename, E}|Errors]}
                end, {[], []}, cmyamls:all()) of 
        {Specs, []} -> {ok, Specs};
        {_, Errors} -> {error, Errors}
    end.
   
rank(port) -> 3;
rank(app) -> 2;
rank(module) -> 1;
rank(_) -> 0.

ranked(#{ <<"type">> := Type,
          <<"name">> := Name }=Spec) -> 
    T = cmkit:to_atom(Type),
    N = cmkit:to_atom(Name),
    Children = case cmconfig_cache:children(T, N) of 
                   {ok, C} -> length(C);
                   _ -> 0
               end,
    Spec#{ <<"rank">> => cmkit:to_float(cmconfig_util:rank(T), Children)}. 

compile(#{ <<"type">> := <<"port">> }=Spec) -> {ok, compile_port(Spec)};
compile(#{ <<"type">> := <<"app">> }=Spec) -> {ok, compile_app(Spec)};
compile(#{ <<"type">> := <<"bucket">> }=Spec) -> {ok, compile_bucket(Spec)};
compile(#{ <<"type">> := <<"template">> }=Spec) -> {ok, compile_template(Spec)};
compile(#{ <<"type">> := <<"module">> }=Spec) -> {ok, compile_module(Spec)};
compile(#{ <<"type">> := <<"test">> }=Spec) -> {ok, compile_test(Spec)};
compile(#{ <<"type">> := <<"queue">> }=Spec) -> {ok, compile_queue(Spec)};
compile(#{ <<"type">> := <<"settings">> }=Spec) -> {ok, compile_settings(Spec)};
compile(#{ <<"type">> := <<"cron">> }=Spec) -> {ok, compile_cron(Spec)};
compile(#{ <<"type">> := <<"task">> }=Spec) -> {ok, compile_task(Spec)};

compile(Spec) ->
    cmkit:danger({cmconfig, unknown_spec, Spec}),
    {error, Spec}.

deps(#{ <<"type">> := T, 
        <<"name">> := N }=Spec) -> 
    Type = cmkit:to_atom(T),
    Name = cmkit:to_atom(N),
    {ok, Type, Name, cmkit:distinct(deps(Type, Spec))}.

deps(app, #{ <<"spec">> := #{
                   <<"modules">> := Modules }}) -> 
    lists:map(fun(Name) ->
                      {module, cmkit:to_atom(Name)}
              end, Modules);

deps(port, #{ <<"spec">> := #{
                    <<"apps">> := Apps }}) when is_map(Apps) -> 
    lists:map(fun(Name) ->
                      {app, cmkit:to_atom(Name)}
              end, maps:keys(Apps));

deps(module, #{ <<"spec">> := 
                #{ <<"encoders">> := Encoders }}) when is_map(Encoders) ->
    module_deps(maps:values(Encoders), []);

deps(_, _) -> [].

module_deps([], Deps) -> Deps;
module_deps([#{<<"object">> := Spec}|Rem], Deps) when is_map(Spec) ->
   module_deps(Rem, modules_from_object_spec(maps:keys(Spec), Spec, []) ++ Deps); 

module_deps([_|Rem], Deps) -> module_deps(Rem, Deps). 

modules_from_object_spec([], _, Mods) -> Mods;
modules_from_object_spec([K|Rem], Spec, Mods) ->
    case maps:get(K, Spec) of
        #{ <<"spec">> := 
            #{ <<"modules">> := MoreMods }} ->
            modules_from_object_spec(Rem, Spec, lists:map(fun(Name) ->
                                                            {module, cmkit:to_atom(Name)}     
                                                          end, MoreMods)
                                     ++ Mods);
        _ ->
            modules_from_object_spec(Rem, Spec, Mods)
    end.

compile_port(#{ <<"name">> := Name,
                <<"rank">> := Rank,
                <<"spec">> := #{
                    <<"port">> := Port,
                    <<"acceptors">> := Acceptors,
                    <<"apps">> := Apps }}) ->
    
    #{ name => compile_keyword(Name),
       type => port,
       rank => Rank,
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

compile_settings(#{ <<"name">> := Name,
                    <<"rank">> := Rank,
                    <<"spec">> := Spec }) ->
    #{ type => settings,
       rank => Rank,
       name => cmkit:to_atom(Name),
       spec => compile_term(Spec) }.

compile_queue(#{ <<"name">> := Name,
                 <<"rank">> := Rank,
                 <<"spec">> := #{
                    <<"concurrency">> := C,
                    <<"max">> := M
                    }}) ->
    
    Worker = cmkit:to_atom(cmkit:bin_join([Name, <<"queue">>], <<"_">>)),

    #{ type => queue,
       rank => Rank,
       name => cmkit:to_atom(Name),
       worker => Worker, 
       capacity => #{ max => M, concurrency => C }}.

compile_cron(#{ <<"name">> := Name,
                <<"rank">> := Rank,
                <<"spec">> := #{ <<"schedule">> := Schedule,
                                 <<"jobs">> := Jobs }}) ->

    CronName = cmkit:to_atom(Name),
    #{ type => cron,
       rank => Rank,
       name => CronName,
       schedule  => compile_cron_schedule(Schedule),
       jobs => compile_cron_jobs(Jobs) 
     }.

compile_cron_schedule(#{ <<"pm">> := #{ <<"hour">> := Hour,
                                           <<"min">> := Min }}) ->
    #{ type => daily,
       hour => Hour,
       min => Min,
        period => pm };

compile_cron_schedule(#{ <<"am">> := #{ <<"hour">> := Hour,
                                           <<"min">> := Min }}) ->
    #{ type => daily,
       hour => Hour,
       min => Min,
        period => am }.

compile_cron_jobs(Specs) ->
    lists:map(fun(#{ <<"module">> := Mod,
                        <<"fun">> := Fun,
                        <<"args">> := Args }) ->
                            
                       {ok, EncodedArgs } = cmencode:encode(#{ type => list,
                                                               value => compile_terms(Args) }),
                       
                       #{ module => cmkit:to_atom(Mod),
                          function => cmkit:to_atom(Fun),
                          args => EncodedArgs
                        }
               end, Specs).

compile_task(#{ <<"name">> := Name,
                <<"rank">> := Rank,
                <<"spec">> := Items }) ->

    #{ type => task,
       rank => Rank,
       name => cmkit:to_atom(Name),
       items => compile_terms(Items)
     }.

compile_template(#{ <<"name">> := Name,
                    <<"rank">> := Rank,
                    <<"spec">> := #{
                        <<"contents">> := Contents }}) ->

    #{   type => template,
         rank => Rank,
         name => cmkit:to_atom(Name),
         contents => Contents }.


compile_module(#{ <<"name">> := Name,
                  <<"rank">> := Rank,
                  <<"spec">> := Spec }) ->

    
    #{ spec := CompiledSpec } = compile_spec(Spec),
    maps:merge(CompiledSpec,  #{
                 name => cmkit:to_atom(Name),
                 type => module,
                 rank => Rank
    }).

compile_app(#{ <<"name">> := Name,
               <<"rank">> := Rank,
               <<"category">> := Cat,
               <<"spec">> := #{
                   <<"modules">> := Modules 
                  } = Spec
             }) ->

    Mods = cmconfig:modules(),
    ResolvedModules = cmconfig_util:resolve_modules(Modules, Mods),
        
    #{ name => cmkit:to_atom(Name),
       type => app,
       rank => Rank,
       modules => lists:map(fun(#{ status := unknown }=M) ->
                                    M;
                               (#{ name := ModName }) ->
                                    #{ name => ModName,
                                       status => resolved
                                     }
                            end, ResolvedModules),
       config => compile_config(maps:get(<<"config">>, Spec, #{})),
       debug => compile_keyword(maps:get(<<"debug">>, Spec, <<"false">>)),
       category => cmconfig_util:compile_keyword(Cat),
       spec => compile_modules(ResolvedModules, #{})
     }.


compile_bucket(#{ <<"name">> := Name,
                  <<"rank">> := Rank,
                  <<"spec">> := #{ <<"hosts">> := Hosts,
                                   <<"storage">> := Storage
                                 }}) ->

    #{ type => bucket,
       rank => Rank,
       name => cmkit:to_atom(Name),
       storage =>  cmkit:to_atom(Storage),
       hosts => Hosts };

compile_bucket(#{ <<"name">> := Name,
                  <<"rank">> := Rank,
                  <<"spec">> := #{ <<"storage">> := Storage
                                 }}) ->

    #{ type => bucket,
       rank => Rank,
       name => cmkit:to_atom(Name),
       storage =>  cmkit:to_atom(Storage)
     }.

compile_test(#{ <<"name">> := Name,
                <<"rank">> := Rank,
                <<"spec">> := Spec
              }) ->
    
        
    Config = maps:get(<<"config">>, Spec, #{}),
    Scenarios = compile_scenarios(maps:get(<<"scenarios">>, Spec, [])),
    Backgrounds = maps:get(<<"backgrounds">>, Spec, []),
    Procedures = maps:get(<<"procedures">>, Spec, []),
    Steps = maps:get(<<"steps">>, Spec, []),

    #{ type => test,
       rank => Rank,
       name => cmkit:to_atom(Name),
       config => compile_config(Config),
       scenarios => Scenarios,
       steps => compile_steps(Steps),
       procedures => compile_procedures(Procedures),
       backgrounds => compile_backgrounds(Backgrounds, Scenarios)
     }.

compile_procedures(Specs) ->
    lists:map(fun compile_procedure/1, Specs).

compile_procedure(#{ <<"name">> := Name,
                     <<"spec" >> := Spec,
                     <<"as">> := As }) ->
    #{ name => cmkit:to_atom(Name),
       spec => compile_term(Spec),
       as => compile_term(As) };

compile_procedure(#{ <<"name">> := Name,
                     <<"spec" >> := Spec}) ->
    #{ name => cmkit:to_atom(Name),
       spec => compile_term(Spec) }.

compile_scenarios(Specs) -> 
    lists:map(fun compile_scenario/1, Specs).

compile_scenario(#{ <<"title">> := Title }=Spec) ->

    Id = test_item_id(Title),
    Tags = maps:get(<<"tags">>, Spec, []),
    Backgrounds = maps:get(<<"backgrounds">>, Spec, []),
    Steps =  maps:get(<<"steps">>, Spec, []),

    #{ title => Title,
       id => Id,
       tags => compile_tags(Tags),
       backgrounds => lists:map(fun(T) -> 
                                        #{ id => test_item_id(T),
                                           title => T }
                                end, Backgrounds),
       steps => compile_steps(Steps)
     }.

compile_background(#{ <<"title">> := Title,
                      <<"steps">> := Steps }=Spec, Scenarios) ->
    
    Id = test_item_id(Title),
    Tags = maps:get(<<"tags">>, Spec, []),

    RelatedScenarios = lists:filter(fun(#{ backgrounds := BackgroundRefs }) ->
                                            lists:any(fun(#{ id := BackgroundId }) ->
                                                                 BackgroundId =:= Id
                                                         end, BackgroundRefs)
                                    end, Scenarios),


    #{ title => Title,
       id => Id,
       tags => compile_tags(Tags),
       scenarios => lists:map(fun(S) -> 
                                        maps:with([id, title], S)
                                end, RelatedScenarios),
       steps => compile_steps(Steps)
     }.


test_item_id(Title) ->
    cmkit:hash(cmkit:to_lower(Title)).


compile_backgrounds(Specs, Scenarios) ->
    lists:foldl(fun(Spec, Out) -> 
                        #{ id := Key } = Compiled = compile_background(Spec, Scenarios),
                    Out#{ Key => Compiled } 
                end, #{}, Specs).

compile_tags(Tags) when is_list(Tags) ->
    lists:map(fun cmkit:to_atom/1, Tags);

compile_tags(Spec) ->
    cmkit:danger({cmconfig, compile, invalid_tags, Spec}),
    [].

compile_steps(Steps) -> 
    compile_steps(Steps, []).

compile_steps([], Out) -> lists:reverse(Out);
compile_steps([Step|Rem], Out) ->
    compile_steps(Rem, [compile_step(Step)|Out]).

compile_step(#{ <<"title">> := Title }=Spec) ->
    maps:merge(
      compile_term(Spec),
      #{ title => Title } );

compile_step(#{ <<"ref">> := Title })  ->
      compile_step(Title);

compile_step(Title) when is_binary(Title) ->
      #{ ref => Title }.

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

compile_option(#{ <<"when">> := When } = Spec) ->

    #{ type => condition,
       condition => compile_term(When),
       spec => compile_term(maps:without([<<"when">>], Spec)) 
     };

compile_option(Spec) ->

    #{ type => condition,
       condition => #{ type => true },
       spec => compile_term(Spec) 
     }.

compile_terms(Specs) -> 
    lists:map(fun compile_term/1, Specs).

compile_term(#{ <<"decoders">> := Decs }) ->
    compile_decoders(Decs);

compile_term(#{ <<"encoders">> := Encs }) ->
    compile_encoders(Encs);

compile_term(#{ <<"encoder">> := Enc }) when is_binary(Enc) ->
    #{ encoder => compile_keyword(Enc) };

compile_term(#{ <<"init">> := Init }) ->
    compile_init(Init);

compile_term(#{ <<"update">> := Update }) ->
    compile_updates(Update);

compile_term(#{ <<"views">> := Views }) ->
    compile_views(Views);

compile_term(#{ <<"effects">> := Effects }) ->
    compile_effects(Effects);

compile_term(#{ <<"either">> := Specs }) when is_list(Specs) ->
    #{ type => either,
       options => lists:map(fun compile_option/1, Specs) };


compile_term(#{ <<"spec">> := Spec,
                <<"to">> := To }) when is_binary(To) ->
    #{ to => cmkit:to_atom(To),
       spec => compile_term(Spec) 
     };

compile_term(#{ <<"connection">> := ConnSpec,
                <<"status">> := Status }) ->
    #{ connection => compile_term(ConnSpec),
       status => cmkit:to_atom(Status)
     };

compile_term(#{ <<"data">> := <<"any">> }) ->
    #{ type => data };

compile_term(#{ <<"any">> := <<"data">> }) ->
    #{ type => data };

compile_term(#{ <<"file">> := <<"any">> }) ->
    #{ type => file };

compile_term(#{ <<"base64">> := Spec,
                <<"as">> := As }) ->

    #{ type => base64,
       as => cmkit:to_atom(As),
      spec => compile_term(Spec) };

compile_term(#{ <<"base64">> := Spec }) ->
    #{ type => base64,
      spec => compile_term(Spec) };

compile_term(#{ <<"as">> := As,
                <<"file">> := Spec }) when is_map(Spec) ->
    #{ type => file,
       as => cmkit:to_atom(As),
       spec => compile_term(Spec) 
     };

compile_term(#{ <<"file">> := #{ <<"path">> := P,
                                 <<"data">> := D }}) -> 
    #{ type => file,
       spec => #{ path => compile_term(P),
                  data => compile_term(D)}};

compile_term(#{ <<"file">> := Spec }) when is_map(Spec) ->
    #{ type => file,
       spec => compile_term(Spec) 
     };

compile_term(#{ <<"asset">> := Spec, 
                <<"as">> := As }) ->
    
    #{ type => asset,
       as => cmkit:to_atom(As),
       spec => compile_term(Spec) 
     };

compile_term(#{ <<"asset">> := Spec}) ->
    
    #{ type => asset,
       spec => compile_term(Spec) 
     };

compile_term(#{ <<"any">> := <<"file">> }) ->
    #{ type => file };

compile_term(#{ <<"at">> := Spec,
                <<"contents">> := DataSpec }) ->
    #{ type => path,
       location => compile_term(Spec),
       state => present,
       contents => compile_term(DataSpec)
     };

compile_term(#{ <<"at">> := Spec }) ->
    #{ type => path,
       location => compile_term(Spec) 
     };

compile_term(#{ <<"data">> := Spec,
                <<"as">> := As }) ->
    #{ type => data,
        spec => compile_term(Spec),
        as => cmkit:to_atom(As) };

compile_term(#{ <<"data">> := Spec }) ->
    maps:merge(#{ type => data},
               compile_term(Spec));

compile_term(#{ <<"rm">> := Spec }) -> 
    #{ type => rm,
      spec => compile_term(Spec) };

compile_term(#{ <<"template">> := #{ <<"name">> := Name,
                                     <<"params">> := ParamsSpec,
                                     <<"dest">> := Dest }}) ->
    #{ type => template,
       name => cmkit:to_atom(Name),
       params => compile_term(ParamsSpec),
       dest => compile_term(Dest) };

compile_term(#{<<"empty">> := <<"object">> }) ->
    #{ type => object, size => 0 };

compile_term(#{<<"empty">> := <<"list">> }) ->
    #{ type => list, size => 0 };

compile_term(#{<<"empty">> := _}) ->
    #{ type => empty };

compile_term(#{ <<"config">> := Key }) ->
    #{ type => config,
       spec => compile_keyword(Key) 
     };

compile_term(#{ <<"maybe">> := Spec }) ->
    #{ maybe => compile_term(Spec) };

compile_term(#{ <<"object">> := Object}) ->
    compile_object(Object);

compile_term(#{ <<"entries">> := Spec }) ->
    #{ type => entries,
       spec => compile_term(Spec)
     };

compile_term(#{ <<"view">> := View }) ->
    #{ type => view ,
       spec => compile_view(View)
     };

compile_term(#{ <<"list">> := <<"empty">>}) ->
    #{ type => list, size => 0 };

compile_term(#{ <<"list">> := <<"any">>}) ->
    #{ type => list };

compile_term(#{ <<"any">> := <<"list">>}) ->
    #{ type => list };

compile_term(#{ <<"list">> := #{ <<"with">> := Spec}}) ->
    #{ type => list, with => compile_term(Spec) };

compile_term(#{ <<"list">> := #{ <<"without">> := Spec}}) ->
    #{ type => list, without => compile_term(Spec) };

compile_term(#{ <<"list">> := Items }) when is_list(Items) ->
    #{ type => list,
       value => lists:map( fun compile_term/1, Items) 
     };

compile_term(#{ <<"list">> := #{ <<"size">> := Size } = Spec }) when is_map(Spec) ->
    #{ type => list,
       size => Size,
       spec => compile_term(Spec)
     };

compile_term(#{ <<"list">> := Spec }) when is_map(Spec) ->
    #{ type => list,
       spec => compile_term(Spec)
     };

compile_term(#{ <<"first">> := Spec }) when is_map(Spec) ->
    #{ type => first,
       spec => compile_term(Spec)
     };


compile_term(#{ <<"merge">> := Specs }) when is_list(Specs) -> 
    #{ type => merge,
       spec => compile_terms(Specs)
     };

compile_term(#{ <<"map">> := #{ <<"value">> := From,
                                <<"options">> := Options }}) ->
    #{ type => map,
       spec => #{ 
         value => compile_term(From),
         options => compile_options(Options)
        }
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


compile_term(#{ <<"number">> := Num }) when is_number(Num) ->
    #{ type => number, value => Num };

compile_term(#{ <<"number">> := <<"any">> }) ->
    #{ type => number };

compile_term(#{ <<"number">> := Spec }) when is_map(Spec) ->
    #{ type => number, spec => compile_term(Spec) };

compile_term(#{ <<"at_least">> := Spec }) ->
    #{ type => greater_than,
       spec => compile_term(Spec) };

compile_term(#{ <<"literal">> := Text }) ->
    #{ value => Text };

compile_term(#{ <<"object">> := <<"any">> }) ->
    #{ type => object };

compile_term(#{ <<"object">> := <<"empty">> }) ->
    #{ type => object, size => 0 };

compile_term(#{ <<"empty">> := <<"object">> }) ->
    #{ type => object, size => 0 };

compile_term(#{ <<"any">> := <<"object">> }) ->
    #{ type => object };

compile_term(#{ <<"text">> := <<"any">> }) ->
    #{ type => text };

compile_term(#{ <<"any">> := <<"text">> }) ->
    #{ type => text };

compile_term(#{ <<"email">> := <<"any">> }) ->
    #{ type => email };

compile_term(#{ <<"any">> := <<"email">> }) ->
    #{ type => email };

compile_term(#{ <<"any">> := <<"keyword">> }) ->
    #{ type => keyword };

compile_term(#{ <<"keyword">> := <<"any">> }) ->
    #{ type => keyword };

compile_term(#{ <<"any">> := <<"number">> }) ->
    #{ type => number };

compile_term(#{ <<"number">> := <<"any">> }) ->
    #{ type => number };

compile_term(#{ <<"any">> := <<"boolean">> }) ->
    #{ type => boolean };

compile_term(#{ <<"boolean">> := <<"any">> }) ->
    #{ type => boolean };

compile_term(#{ <<"regexp">> := Regex }) ->
    #{ type => regexp,
        value => compile_term(Regex) };

compile_term(#{ <<"text">> := Spec }) ->
    
    maps:merge(#{ type => text},
               compile_term(Spec));
  

compile_term(#{ <<"format">> := #{ <<"pattern">> := Pattern,
                                   <<"params">> := Params }}) when is_list(Params) -> 
    #{ type => format,
       pattern => compile_term(Pattern),
       params => compile_terms(Params) };

compile_term(#{ <<"format">> := #{ <<"pattern">> := Pattern,
                                   <<"params">> := Params }}) when is_map(Params) -> 
    #{ type => format,
       pattern => compile_term(Pattern),
       params => compile_term(Params) };


compile_term(#{ <<"files">> := Spec }) -> 
    #{ type => files,
       spec => compile_term(Spec) 
     };

compile_term(#{ <<"size">> := Size}) -> 
    #{ size => Size };

compile_term(<<"from_data">>) -> from_data;
compile_term(#{ <<"from_data">> := _ }) -> from_data;

compile_term(#{ <<"item">> := Num,
                <<"in">> := In }) when is_map(In) ->
    
    #{ item => Num,
       in => compile_term(In)
     };

compile_term(#{ <<"item">> := Num,
                <<"in">> := In }) when is_binary(In) ->
    
    #{ item => Num,
       in => compile_keyword(In)
     };

compile_term(#{ <<"item">> := Num }) when is_number(Num) ->
    #{ item => Num };

compile_term(#{ <<"key">> := Key, 
                <<"in">> := In }) when is_binary(Key) and is_binary(In) -> 
    #{ key => compile_keyword(Key),
       in => compile_keyword(In)
     };


compile_term(#{ <<"key">> := Key, 
                <<"in">> := In }) when is_binary(Key) -> 
    #{ key => compile_keyword(Key),
       in => compile_term(In)
     };

compile_term(#{ <<"key">> := Key, 
                <<"in">> := In }) when is_binary(In) -> 
    #{ key => compile_term(Key),
       in => compile_keyword(In)
     };

compile_term(#{ <<"key">> := Key, 
                <<"in">> := In }) -> 
    #{ key => compile_term(Key),
       in => compile_term(In)
     };

compile_term(#{ <<"key">> := Key}) ->
    #{ key => compile_keyword(Key) };


compile_term(#{ <<"one_of">> := Specs }) when is_list(Specs) ->
    #{ one_of => lists:map(fun compile_term/1, Specs) }; 


compile_term(#{ <<"loop">> := From,
                <<"context">> := Context,
                <<"with">> := View }) when is_map(From) ->
    
    ItemViewSpec = case is_binary(View) of 
                       true -> cmkit:to_atom(View);
                       false -> compile_term(View)
                   end,

    #{ loop => compile_term(From),
       with => ItemViewSpec,
       context => compile_term(Context) };

compile_term(#{ <<"loop">> := From,
                <<"context">> := _,
                <<"with">> := _ } = Spec) when is_binary(From) ->

    compile_term(Spec#{ <<"loop">> => #{ <<"key">> => From }});

compile_term(#{ <<"loop">> := _} = Spec)  ->

    compile_term(Spec#{ <<"context">> => #{} });

compile_term(#{ <<"are_set">> := Specs }) when is_list(Specs) ->
    #{ type => are_set,
       spec => lists:map(fun compile_term/1, Specs)
     };

compile_term(#{ <<"is_set">> := Spec }) when is_map(Spec) ->
    #{ type => is_set,
       spec => compile_term(Spec)
     };

compile_term(#{ <<"not">> := Spec }) ->
    #{ type => 'not',
       spec => compile_term(Spec)
     };

compile_term(#{ <<"eq">> := Specs }) when is_list(Specs) ->
    #{ type => equal,
       spec => lists:map(fun compile_term/1, Specs)
     };

compile_term(#{ <<"other_than">> := Spec }) ->
    #{ type => other_than,
       spec => compile_term(Spec) };

compile_term(#{ <<"gt">> := Specs }) when is_list(Specs) ->
    #{ type => greater_than,
       spec => lists:map(fun compile_term/1, Specs)
     };

compile_term(#{ <<"sum">> := Specs }) when is_list(Specs) ->
    #{ type => sum,
       spec => lists:map(fun compile_term/1, Specs)
     };

compile_term(#{ <<"and">> := Specs }) when is_list(Specs) ->
    #{ type => 'and',
       spec => lists:map(fun compile_term/1, Specs)
     };

compile_term(#{ <<"ratio">> := #{ <<"num">> := Num,
                                    <<"den">> := Den }}) ->
    #{ type => ratio,
       num => compile_term(Num),
       den => compile_term(Den)
     };

compile_term(#{ <<"percentage">> := #{ <<"num">> := Num,
                                    <<"den">> := Den }}) ->
    #{ type => percentage,
       num => compile_term(Num),
       den => compile_term(Den)
     };

compile_term(#{ <<"member">> := Spec }) -> 
   
    #{ type => member,
       spec => compile_term(Spec)
     };

compile_term(#{ <<"present">> := Spec }) ->
    #{ type => present,
       spec => compile_term(Spec) };

compile_term(#{ <<"value">> := ValueSpec,
                <<"of">> := CollectionSpec
              }) ->
    
    #{ value => compile_term(ValueSpec),
                  in => compile_term(CollectionSpec)
     };

compile_term(#{ <<"all">> := Conds }) ->
    #{ type => all,
       spec => lists:map(fun compile_term/1, Conds) 
     };

compile_term(#{ <<"connect">> := Spec,
               <<"as">> := As } = Spec0) ->
    Expr = #{ type => connect,
              as => compile_term(As),
              spec => compile_term(Spec) },

    Expr2 = case maps:get(<<"protocol">>, Spec0, undef) of 
                undef -> 
                    Expr;
                ProtocolSpec -> 
                    Expr#{ protocol => compile_term(ProtocolSpec) }
            end,

    Expr2;
    

compile_term(#{ <<"connect">> := Spec }) ->
    #{ type => connect,
       spec => compile_term(Spec) };

compile_term(#{ <<"probe">> := Spec }) ->
    
    #{ type => probe,
       spec => compile_term(Spec)
     };

compile_term(#{ <<"disconnect">> := Spec }) ->
    
    #{ type => disconnect,
       spec => compile_term(Spec)
     };

compile_term(#{ <<"send">> := 
                #{ <<"to">> := ConnSpec,
                   <<"spec">> := Spec }}) ->
    #{ type => send,
       spec => #{ to => compile_term(ConnSpec),
                  spec => compile_term(Spec) }};

compile_term(#{ <<"send">> := Spec }) ->
    
    #{ type => send,
       spec => compile_term(Spec)
     };


compile_term(#{ <<"receive">> := 
                #{ <<"from">> := From,
                   <<"spec">> := Spec,
                   <<"as">> := As }}) ->
    #{ type => recv,
       from => compile_term(From),
       spec => compile_term(Spec),
       as => compile_keyword(As)
     };

compile_term(#{ <<"receive">> := 
                #{ <<"from">> := From,
                   <<"spec">> := Spec,
                   <<"remember">> := Remember }}) ->
    #{ type => recv,
       from => compile_term(From),
       spec => compile_term(Spec),
       as => compile_object(Remember)
     };

compile_term(#{ <<"receive">> := 
                #{ <<"from">> := From,
                   <<"spec">> := Spec }}) ->

    #{ type => recv,
       from => compile_term(From),
       spec => compile_term(Spec),
       as => latest 
     };

compile_term(#{ <<"expect">> := Spec }) ->
    #{ type => expect,
       spec => compile_term(Spec)
     };

compile_term(#{ <<"request">> := Spec,
                <<"as">> := As }) ->
    #{ type => request,
       as => compile_term(As),
       spec => compile_term(Spec) 
     };

compile_term(#{ <<"request">> := Spec }) ->
    #{ type => request,
       spec => compile_term(Spec) 
     };

compile_term(#{ <<"response">> := Spec }) ->
    #{ type => response,
         spec => compile_term(Spec) 
    };

compile_term(#{ <<"status">> := Status }) ->
    #{  type => http, 
        status => Status
     };

compile_term(#{ <<"join">> := #{ 
                    <<"terms">> := Terms 
                   }
              }) ->
    #{ type => join,
       terms => compile_terms(Terms) };

compile_term(#{ <<"join">> := Terms}) when is_list(Terms) ->  
    #{ type => join,
       terms => compile_terms(Terms) };

compile_term(#{ <<"url">> := Url,
                <<"method">> := Method } = Spec) ->
    
    Expr = #{ type => http,
              url => compile_term(Url),
              method => cmkit:to_atom(Method)
            },

    Expr2 = case maps:get(<<"headers">>, Spec, undef) of 
                undef -> Expr;
                HeadersSpec -> 
                    Expr#{ headers => compile_term(HeadersSpec) }
            end,

    Expr3 = case maps:get(<<"body">>, Spec, undef) of 
                undef -> Expr2;
                BodySpec -> 
                    Expr2#{ body => compile_term(BodySpec) }
            end,
    Expr3;

compile_term(#{ <<"host">> := Host,
                <<"port">> := Port,
                <<"transport">> := Transport,
                <<"path">> := Path } = Spec) ->
    
    Expr = #{ type => url,
              host => compile_term(Host),
              port => compile_term(Port),
              transport => compile_term(Transport),
              path => compile_term(Path) },

    Expr2 = case maps:get(<<"query">>, Spec, undef) of 
                undef ->
                    Expr;
                QuerySpec -> 
                    Expr#{ query => compile_term(QuerySpec) }
            end,
    Expr2;


compile_term(#{ <<"multipart">> := #{ <<"files">> := FilesSpec } }) ->
    #{ type => multipart,
       files => compile_term(FilesSpec) };


compile_term(#{ <<"http">> := Spec }) ->
    #{ type => exec,
       spec => compile_term(Spec) };

compile_term(#{ <<"procedure">> := Name,
                <<"params">> := Params }) ->

    #{ type => procedure,
       name => cmkit:to_atom(Name),
       params => compile_term(Params) };

compile_term(#{ <<"basic-auth">> := Spec }) ->
    #{ type => basic_auth,
       spec => compile_term(Spec) };

compile_term(#{ <<"task">> := Task }) -> 
    #{ type => task,
       name => compile_keyword(Task) };

compile_term(#{ <<"shell">> := 
                    #{ <<"chwd">> := Chwd,
                       <<"cmd">> := Cmd }}) ->
    #{ type => shell,
       chwd => compile_term(Chwd),
       cmd => compile_term(Cmd) };

compile_term(#{}=Map) when map_size(Map) == 0 ->
    #{ type => object };

compile_term(null) -> 
    #{ type => object };

compile_term([]) -> 
    #{ type => list, value => [] };

compile_term(#{ <<"kube">> := Spec}) ->
    #{ type => kube,
       spec => compile_kube_spec(Spec) };

compile_term(#{ <<"slack">> := #{ <<"settings">> := SettingsSpec,
                                  <<"severity">> := SeveritySpec,
                                  <<"subject">> := SubjectSpec,
                                  <<"body">> := BodySpec }}) ->
    #{ type => slack,
       spec => #{ settings => compile_term(SettingsSpec),
                  severity => compile_term(SeveritySpec),
                  subject => compile_term(SubjectSpec),
                  body => compile_term(BodySpec) }};

compile_term(#{ <<"git">> := #{ 
                    <<"credentials">> := CredsSpec,
                    <<"clone">> := #{ <<"repo">> := Repo,
                                      <<"branch">> := Branch,
                                      <<"dir">> := Dir }}}) -> 
    #{ type => git,
       spec => #{ action => clone,
                  credentials => compile_term(CredsSpec),
                  repo => compile_term(Repo),
                  branch => cmkit:to_atom(Branch),
                  dir => compile_term(Dir) }};

compile_term(#{ <<"git">> := #{ 
                    <<"credentials">> := CredsSpec,
                    <<"clone">> := #{ <<"repo">> := Repo,
                                      <<"dir">> := Dir }}}) -> 
    #{ type => git,
       spec => #{ action => clone,
                  credentials => compile_term(CredsSpec),
                  branch => master,
                  repo => compile_term(Repo),
                  dir => compile_term(Dir) }};

compile_term(#{ <<"git">> := #{ 
                    <<"as">> := As,
                    <<"credentials">> := CredsSpec,
                    <<"tag">> := #{ <<"repo">> := Repo,
                                    <<"dir">> := Dir,
                                    <<"prefix">> := Prefix,
                                    <<"increment">> := Increment } = Spec}}) -> 
    GitSpec = #{ action => tag,
                  as => cmkit:to_atom(As),
                  credentials => compile_term(CredsSpec),
                  repo => compile_term(Repo),
                  dir => compile_term(Dir),
                  prefix => compile_term(Prefix),
                  increment => cmkit:to_atom(Increment) },

    GitSpec2  = case maps:get(<<"branch">>, Spec, undef) of 
                    undef -> GitSpec;
                    Br -> GitSpec#{ branch => cmkit:to_atom(Br) }
                end,

    GitSpec3 = case maps:get(<<"clone">>, Spec, undef) of 
                   undef -> GitSpec2#{ clone => true };
                   V -> 
                       GitSpec2#{ clone => cmkit:to_atom(V) }
                    end,
    
    #{ type => git,
       spec => GitSpec3 };

compile_term(#{ <<"docker">> := #{ 
                    <<"credentials">> := CredsSpec,
                    <<"build">> := #{ <<"repo">> := Repo,
                                                     <<"tag">> := Tag,
                                                     <<"dir">> := Dir }}}) -> 
    #{ type => docker,
       spec => #{ action => build,
                  credentials => compile_term(CredsSpec),
                  repo => compile_term(Repo),
                  tag => compile_term(Tag),
                  dir => compile_term(Dir) }
     };

compile_term(#{ <<"test">> := #{ 
                    <<"name">> := Test,
                    <<"settings">> := Settings,
                    <<"opts">> := Opts }}) -> 
    #{ type => test,
       spec => #{ name => cmkit:to_atom(Test),
                  settings => cmkit:to_atom(Settings),
                  opts => compile_term(Opts) }};

compile_term(#{ <<"wait">> := #{
                    <<"sleep">> := Sleep,
                    <<"retries">> := Retries,
                    <<"condition">> := Condition }}) ->
    #{ type => wait,
       spec => #{ sleep => Sleep,
                  retries => Retries,
                  condition => compile_term(Condition) }};

compile_term(#{ <<"match">> := #{
                    <<"value">> := ValueSpec,
                    <<"with">> := DecoderSpec }}) ->
    #{ type => match,
       spec => #{ value => compile_term(ValueSpec),
                  decoder => compile_term(DecoderSpec) }};

compile_term(#{ <<"find">> := TargetSpec,
                <<"in">> := SourceSpec }) -> 
    
    #{ type => find,
       items => compile_term(SourceSpec),
       target => compile_term(TargetSpec) };

compile_term(#{ <<"iterate">> := SourceSpec,
                <<"filter">> := FilterSpec, 
                <<"with">> := DestSpec }) ->
    
    #{ type => iterate,
       source => compile_term(SourceSpec),
       filter => compile_term(FilterSpec),
       dest => compile_term(DestSpec) };

compile_term(#{ <<"iterate">> := SourceSpec,
                <<"with">> := DestSpec }) ->
    
    #{ type => iterate,
       source => compile_term(SourceSpec),
       filter => none,
       dest => compile_term(DestSpec) };

compile_term(#{ <<"attempt">> := #{ <<"spec">> := Spec,
                                    <<"onerror">> := OnError } }) -> 
    
    #{ type => attempt,
       spec  => compile_term(Spec),
       onerror => compile_term(OnError)
     };

compile_term(#{ <<"erlang">> := #{ <<"mod">> := Mod,
                                   <<"fun">> := Fun } = Spec}) -> 
    
    Args = maps:get(<<"args">>, Spec, []),

    #{ type => erlang,
       mod => compile_keyword(Mod),
       function => compile_keyword(Fun),
       args => compile_term(Args) };

compile_term(#{ <<"thumbnail">> := #{ <<"url">> := U,
                                      <<"basename">> := B,
                                      <<"sizes">> := S }}) -> 
    
    #{ type => thumbnail,
       url => compile_term(U),
       basename => compile_term(B),
       sizes => compile_terms(S) };

compile_term(#{ <<"s3">> := #{ <<"access">> := Access,
                               <<"secret">> := Secret,
                               <<"bucket">> := Bucket,
                               <<"key">> := Key,
                               <<"data">> := Data }}) -> 
    
    #{ type => s3,
       spec => #{ access => compile_term(Access),
                  secret => compile_term(Secret),
                  bucket => compile_term(Bucket),
                  key => compile_term(Key),
                  data => compile_term(Data) }};

compile_term(#{ <<"db">> := #{ <<"bucket">> := B,
                               <<"type">> := T,
                               <<"id">> := Id,
                               <<"value">> := Value }}) -> 
    #{ type => db,
       spec => #{ bucket => compile_term(B),
                  type => compile_term(T),
                  id => compile_term(Id),
                  value => compile_term(Value) }};


compile_term(#{ <<"queue">> := #{ <<"name">> := Name,
                                  <<"finish">> := Id}}) ->
    
    #{ type => queue,
       spec => #{ action => finish,
                  name => compile_keyword(Name),
                  id => compile_term(Id) }};

compile_term(Num) when is_number(Num) ->
    #{ type => number, value => Num };

compile_term(Text) when is_binary(Text) -> 
    
    #{ type => text, value => Text };

compile_term(Term) when is_map(Term) ->
    #{ type => object,
       spec => maps:fold(fun(K, V, Out) ->
                            K2 = cmkit:to_atom(K),
                            V2 = compile_term(V),
                            Out#{ K2 => V2 }
                         end, #{}, Term) };

compile_term(Terms) when is_list(Terms) ->
    #{ type => list,
       spec => lists:foldr(fun(T, Out) ->
                                   [compile_term(T)|Out]
                         end, [], Terms) };


compile_term(true) -> 
    #{ type => keyword,
       value => true };

compile_term(<<"yes">>) -> 
    #{ type => keyword,
       value => true };

compile_term(false) -> 
    #{ type => keyword,
       value => false };

compile_term(<<"no">>) -> 
    #{ type => keyword,
       value => false };

compile_term(#{ <<"lat">> := Lat, <<"lon">> := Lon }) ->
    #{ lat => Lat, 
       lon => Lon };

compile_term(Spec) ->
    cmkit:danger({cmconfig, compile, term_not_supported, Spec}),
    #{ type => unknown, spec => Spec }.


compile_kube_spec(#{ <<"secret">> := Spec}) ->
    #{ query => create,
       resource => secret,
       params => compile_term(Spec)
     };

compile_kube_spec(#{ <<"kind">> := Kind,
                     <<"name">> := NameSpec, 
                     <<"namespace">> := NsSpec,
                     <<"state">> := StateSpec,
                     <<"server">> := ApiServerSpec }=Spec) ->
    
    Expr = #{ state => compile_term(StateSpec),
              namespace => compile_term(NsSpec),
              resource => cmkit:to_atom(Kind),
              name => compile_term(NameSpec),
              server => compile_term(ApiServerSpec)
            },
    
    Expr2 = case maps:get(<<"props">>, Spec, undef) of 
                undef ->
                    Expr;
                PropsSpec ->
                    Expr#{ props => compile_term(PropsSpec) }
            end,

    Expr2;

compile_kube_spec(#{ <<"delete">> := Spec}) ->
    #{ query => delete,
       params => compile_term(Spec)
     };

compile_kube_spec(#{ <<"query">> := Verb,
                     <<"resource">> := Resource}) ->
    #{ query => cmkit:to_atom(Verb),
       resource => cmkit:to_atom(Resource) }.

%compile_from(From) when is_binary(From)-> compile_keyword(From);
%compile_from(From) when is_map(From) ->
%    compile_term(From).

compile_object(<<"any">>) -> 
    #{ type => object };

compile_object(<<"empty">>) -> 
    #{ type => object, size => 0 };

compile_object(null) ->
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

compile_decoders(Decs) when is_map(Decs) ->
    compile_decoders(maps:keys(Decs), Decs, []);

compile_decoders(_) -> [].


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


compile_options(Spec) when is_map(Spec) ->
    compile_options(maps:keys(Spec), Spec, []).

compile_options([], _, Out) -> Out;
compile_options([K|Rem], Spec, Out) ->
    compile_options(Rem, Spec, [#{ source => compile_term(K),
                                   target => compile_term(maps:get(K, Spec))
                                 }|Out]).


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

compile_update_spec(#{ <<"when">> := When}=Spec) when is_map(Spec) -> 
    #{ condition => compile_term(When), 
       model => compile_model(maps:get(<<"model">>, Spec, #{})),
       cmds => compile_cmds(maps:get(<<"cmds">>, Spec, [])) 
     };

compile_update_spec(Spec) when is_map(Spec) -> 
    #{ condition => #{ type => true }, 
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
       condition => compile_term(When) };

compile_view(#{ <<"view">> := View,
                <<"when">> := When }) ->
    
    #{ view => compile_keyword(View),
       condition => compile_term(When) };
    

compile_view(#{ <<"view">> := View,
                <<"params">> := Params }) ->
    
    #{ view => compile_view_name(View),
       params  => compile_term(Params) };

compile_view(#{ <<"view">> := View }) ->
    
    #{ view => compile_view_name(View),
       params  => #{} };
    
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
       using => compile_keyword(ItemView) };

compile_view(#{ <<"json">> := Spec,
                <<"indent">> := Indent }) ->
    #{ json => compile_term(Spec),
       indent => compile_term(Indent) };

compile_view(#{ <<"json">> := _} = Spec) ->
    compile_view(Spec#{ <<"indent">> => 2 });

compile_view(#{ <<"timestamp">> :=  #{ <<"format">> := Format,
                                       <<"value">> := Value }}) ->
    
    #{ timestamp => #{ format => cmkit:to_atom(Format),
                       value => compile_term(Value) }};

compile_view(#{ <<"date">> :=  #{ <<"format">> := Format,
                                  <<"value">> := Value }}) ->
    
    #{ date  => #{ 
         format => compile_term(Format),
         value => compile_term(Value) }};


compile_view(#{ <<"map">> := #{ <<"id">> := Id,
                                <<"style">> := Style,
                                <<"zoom">> := Zoom,
                                <<"center">> := Center,
                                <<"markers">> := Markers }}) ->

    #{ map => #{ id => compile_term(Id),
                 style => cmkit:to_atom(Style),
                 zoom => Zoom,
                 center => compile_term(Center),
                 markers => compile_terms(Markers) }};

compile_view(Spec) ->
    cmkit:danger({cmconfig, compile, view_spec_not_supported, Spec}),
    #{ view => not_supported,
       spec => Spec }.

compile_view_name(Text) when is_binary(Text) ->
    compile_keyword(Text);

compile_view_name(Spec) ->
    compile_term(Spec).


compile_view_attrs(Attrs) when is_map(Attrs) ->
    maps:fold(fun(K, V, Attrs2) ->
                      Attrs2#{ compile_keyword(K) => compile_term(V) }
              end, #{}, Attrs). 


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
