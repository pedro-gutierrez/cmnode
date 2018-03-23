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
         compile_assets/1
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
            <<"spec">> := #{
                <<"decoders">> := Decoders,
                <<"encoders">> := Encoders,
                <<"init">> := Init,
                <<"update">> := Update }}) ->

    #{ name => cmkit:to_atom(Name),
         type => module,
         category => cmconfig_util:compile_keyword(Cat),
         decoders => cmconfig_util:compile_decoders(Decoders),
         encoders => cmconfig_util:compile_encoders(Encoders),
         init => cmconfig_util:compile_updates(Init),
         updates => cmconfig_util:compile_updates(Update) }.


compile_app(#{ <<"name">> := Name, <<"category">> := Cat,
            <<"spec">> := #{
                <<"port">> := Port,
                <<"acceptors">> := Acceptors,
                <<"modules">> := Modules 
               }=Spec}, Mods) ->
        
    Assets = cmconfig_util:compile_assets(maps:get(<<"assets">>, Spec, #{})),

    #{ name => cmkit:to_atom(Name),
       type => app,
       category => cmconfig_util:compile_keyword(Cat),
       port  => Port,
       acceptors => Acceptors,
       modules  => cmconfig_util:resolve_modules(Modules, Mods),
       assets => Assets
     };

compile_app(#{ <<"name">> := Name, <<"category">> := Cat,
            <<"spec">> := #{}=_Spec}, _Mods) ->
        
    #{ name => cmkit:to_atom(Name),
       type => app,
       category => cmconfig_util:compile_keyword(Cat)
     }.


compile_bucket(#{ <<"name">> := Name,
                  <<"spec">> := #{ <<"hosts">> := Hosts,
                                   <<"storage">> := Storage
                                 }}) ->

    #{ name => cmkit:to_atom(Name),
       storage =>  cmkit:to_atom(Storage),
       hosts => Hosts }.


compile_term(#{ <<"object">> := Object}) ->
    compile_object(Object);

compile_term(#{ <<"list">> := Items }) when is_list(Items) ->
    #{ type => list,
       value => lists:map( fun compile_term/1, Items) 
     };

compile_term(#{ <<"type">> := Type}) ->
    compile_keyword(Type);

compile_term(#{ <<"keyword">> := Keyword }) ->
    #{ type => keyword,
       value => cmkit:to_atom(Keyword) };

compile_term(#{ <<"number">> := _ }) ->
    #{ type => number };

compile_term(#{ <<"text">> := _ }) ->
    #{ type => text };

compile_term(#{ <<"from">> := From,
                <<"at">> := At }) ->
    #{ from => compile_from(From),
       at => compile_keyword(At) };

compile_term(#{ <<"from">> := From  }) ->
    #{ from => compile_from(From) }.

compile_from(From) when is_binary(From)-> compile_keyword(From);
compile_from(From) when is_map(From) ->
    compile_term(From).

compile_object(Map) when is_map(Map) ->
    compile_object(maps:keys(Map), Map, #{}).

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
                                    decoder => Dec}|Out]). 

compile_encoders(Encs) ->
    compile_encoders(maps:keys(Encs), Encs, #{}).

compile_encoders([], _, Out) -> Out;
compile_encoders([K|Rem], Encs, Out) ->
    Name = compile_keyword(K),
    Enc = compile_term(maps:get(K, Encs)),
    compile_encoders(Rem, Encs, Out#{ Name => Enc}). 

compile_updates(#{ <<"model">> := Model,
                <<"cmds">> := Cmds }) ->
    #{ model => compile_model(Model),
       cmds => compile_cmds(Cmds) };

compile_updates(#{ <<"model">> := Model }) ->
    #{ model =>  compile_model(Model) };

compile_updates(#{ <<"cmds">> := Cmds }) ->
    #{ cmds =>  compile_cmds(Cmds) };

compile_updates(#{}) -> #{}.

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


