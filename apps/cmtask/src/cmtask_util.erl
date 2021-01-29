-module(cmtask_util).
-export([settings/1, run/3]).

settings(Names) when is_list(Names) ->
    settings(Names, [], #{});

settings(Name) ->
    case cmconfig:settings(Name) of 
        {ok, #{ name := N,
                spec := Spec }} ->
            case cmencode:encode(Spec) of 
                {ok, Encoded} ->
                    {ok, N, Encoded};
                Other ->
                    Other
            end;
        {error, E} ->
            {error, #{ settings => Name,
                       error => E }}
    end.

settings([], Names, Spec) ->
    {ok, lists:reverse(Names), Spec};

settings([Name|Rem], Names, Spec) ->
    case settings(Name) of 
        {ok, N, Spec0} ->
            settings(Rem, [Name|Names], Spec#{ N => Spec0 });
        Other ->
            Other
    end.

run(#{ name := Name,
       items := Items }, Settings, Params) ->

    Input = #{ params => Params,
               context => #{},
               settings => maps:get(spec, Settings, #{})},

    case resolve_items(Items) of 
        {ok, Items2} -> 
            case run_items(Name, Items2, Input) of 
                {error, E} ->
                    cmkit:danger({task, Name, error, E});
                _ ->
                    ok
            end;
        Other -> 
            Other
    end.

resolve_items(Items) -> 
    resolve_items(Items,[]).

resolve_items([], Out) -> {ok, lists:reverse(Out) };
resolve_items([#{ type := task, name := Name }|Rem], Out) ->
    case cmconfig:task(Name) of 
        {ok, #{ items := Items }} ->
            resolve_items(Rem, lists:reverse(Items) ++ Out); 
        Other ->
            Other
    end;

resolve_items([Item|Rem], Out) ->
    resolve_items(Rem, [Item|Out]).

run_items(_, [], In) ->
    {ok, In};

run_items(Name, [Item|Rem], In) -> 
    case run_item(Name, Item, #{ context := Ctx } = In) of 
        ok -> 
            run_items(Name, Rem, In);
        {ok, Extra } when is_map(Extra) ->
            Ctx2 = maps:merge(Ctx, Extra),
            Ctx3 = Ctx2#{ last => Extra },
            run_items(Name, Rem, In#{ context => Ctx3 }); 
        {ok, Extra } -> 
            run_items(Name, Rem, In#{ context => Ctx#{ last => Extra }}); 
        {alias, Extra} ->
            In2 = In#{ context => Ctx },
            In3 = maps:merge(In2, Extra),
            run_items(Name, Rem, In3); 
        Other -> 
            Other
    end.

run_item(Name, #{ type := dump, spec := Spec}, In) ->
    cmkit:log({Name, dump, Spec, In, maps:get(Spec, In, undefined)}),
    ok;

run_item(Name, #{ type := slack,
                  spec := #{ settings := SettingsSpec, 
                             severity := SeveritySpec,
                             subject := SubjectSpec,
                             body := BodySpec }}, #{ settings := Settings} = In) ->
    case maps:get(slack, Settings, undef) of 
        undef -> 
            {error, #{ task => Name,
                       error => missing_slack_settings,
                       reason => missing_key,
                       key => slack,
                       settings => Settings }};
        Slack ->
            case cmencode:encode(SettingsSpec, In) of 
                {ok, SlackSettingsKey} ->
                    case maps:get(SlackSettingsKey, Slack, undef) of 
                        undef ->
                            {error, #{ task => Name,
                                       error => unknown_slack_settings,
                                       reason => missing_key,
                                       key => SlackSettingsKey, 
                                       settings => Slack }};
                        #{ enabled := Enabled,
                           channel := Ch,
                           token := T } -> 

                            case cmencode:encode(#{ type => slack,
                                                    spec => #{ enabled => Enabled,
                                                               token => T,
                                                               channel => Ch,
                                                               subject => SubjectSpec,
                                                               severity => SeveritySpec,
                                                               body => BodySpec }}, In) of 
                                {ok, _} ->
                                    ok;
                                Other ->
                                    Other
                            end;
                        Other -> Other
                    end;
                Other -> Other
            end
    end;

run_item(_, #{ type := git, spec := #{ action := clone,
                                       credentials := CredsSpec,
                                       repo := RepoSpec, 
                                       branch := Branch,
                                       dir := DirSpec }}, In) ->
    case cmencode:encode(RepoSpec, In) of 
        {ok, Repo} -> 
            case cmencode:encode(DirSpec, In) of 
                {ok, Dir} ->
                    case cmencode:encode(CredsSpec, In) of 
                        {ok, Creds} -> 
                            cmgit:clone(Repo, #{ dir => Dir,
                                                 branch => Branch,
                                                 credentials => Creds });
                        Other -> 
                            Other
                    end;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

run_item(_, #{ type := git, spec := #{ action := tag,
                                       as := As,
                                       credentials := CredsSpec,
                                       repo := RepoSpec, 
                                       clone := Clone,
                                       dir := DirSpec,
                                       prefix := PrefixSpec,
                                       increment := IncrementSpec
                                     } = Spec }, In) ->
    case cmencode:encode(RepoSpec, In) of 
        {ok, Repo} -> 
            case cmencode:encode(DirSpec, In) of 
                {ok, Dir} ->
                    case cmencode:encode(CredsSpec, In) of 
                        {ok, Creds} -> 
                            case cmencode:encode(PrefixSpec, In) of 
                                {ok, Prefix} -> 
                                    case cmencode:encode(IncrementSpec, In) of 
                                        {ok, Increment} -> 
                                            GitParams = #{ dir => Dir,
                                                           clone => Clone,
                                                           credentials => Creds,
                                                           increment => Increment,
                                                           prefix => Prefix },        

                                            GitParams2 = case maps:get(branch, Spec, undef) of 
                                                             undef -> GitParams;
                                                             Br -> GitParams#{ branch => Br }
                                                         end,
                                            case cmgit:tag(Repo, GitParams2) of 
                                                {ok, Tag} -> 
                                                    {ok, #{ As => Tag }};
                                                Other -> 
                                                    Other
                                            end;
                                        Other -> Other
                                    end;
                                Other -> 
                                    Other
                            end;
                        Other -> 
                            Other
                    end;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

run_item(_, #{ type := docker, spec := #{ action := build,
                                          credentials := CredsSpec,
                                          repo := RepoSpec,
                                          tag := TagSpec,
                                          dir := DirSpec,
                                          errors := ErrorsSpec }}, In) ->
    case cmencode:encode(RepoSpec, In) of 
        {ok, Repo} -> 
            case cmencode:encode(DirSpec, In) of 
                {ok, Dir} ->
                    case cmencode:encode(TagSpec, In) of 
                        {ok, Tag} -> 
                            case cmencode:encode(CredsSpec, In) of 
                                {ok, Creds} -> 
                                    case cmencode:encode(ErrorsSpec, In) of 
                                        {ok, Errors} ->
                                            cmdocker:build(#{ credentials => Creds,
                                                              dir => Dir,
                                                              repo => Repo,
                                                              tag => Tag,
                                                              errors => Errors });
                                        Other ->
                                            Other
                                    end;
                                Other -> 
                                    Other
                            end;
                        Other -> 
                            Other
                    end;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

run_item(_, #{ type := docker, spec := #{ action := pull,
                                          credentials := CredsSpec,
                                          repo := RepoSpec,
                                          tag := TagSpec }}, In) ->
    case cmencode:encode(RepoSpec, In) of 
        {ok, Repo} -> 
            case cmencode:encode(TagSpec, In) of 
                {ok, Tag} -> 
                    case cmencode:encode(CredsSpec, In) of 
                        {ok, Creds} -> 
                            cmdocker:pull(#{ credentials => Creds,
                                             repo => Repo,
                                             tag => Tag });
                        Other -> 
                            Other
                    end;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

run_item(_, #{ type := wait }=Spec, In) ->
    case cmencode:encode(Spec, In) of 
        {ok, true} -> ok;
        Other -> 
            Other
    end;

run_item(_, #{ type := exec }=Spec, In) ->
    cmencode:encode(Spec, In);

run_item(_, #{ type := test, spec := #{ name := Test,
                                        settings := Settings,
                                        opts := OptsSpec }}, In) ->
    case cmencode:encode(OptsSpec, In) of 
        {ok, Opts} ->
            cmtest:schedule(Test, Settings, Opts),
            ok;
        Other -> 
            Other
    end;

run_item(_, #{ type := thumbnail, as := As }=Spec, In) -> 
    case cmencode:encode(Spec, In) of 
        {ok, Data} -> 
            {ok, #{ As => Data }};
        Other -> 
            Other 
    end;

run_item(_, #{ type := rm, spec := Location}, In)  ->
    case cmencode:encode(Location, In) of 
        {ok, Path} ->
            Filename = cmkit:to_list(Path),
            case file:delete(Filename) of 
                ok -> ok;
                {error, enoent} -> ok;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

run_item(_, #{ type := template, 
               name := Name,
               params := ParamsSpec,
               dest := Dest }, In) -> 

    case cmencode:encode(ParamsSpec, In) of 
        {ok, Params} -> 
            case cmencode:encode(Dest, In) of 
                {ok, Path} -> 
                    Filename = cmkit:to_list(Path),
                    case cmtemplate:render(Name, Params) of 
                        {ok, Data} -> 
                            case file:write_file(Filename, Data) of 
                                ok -> 
                                    cmkit:log({cmtask, Name, written, Filename, 
                                               size(Data), bytes}),
                                    ok;
                                Other -> 
                                    Other
                            end;
                        Other -> 
                            Other
                    end;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;

run_item(Name, #{ type := shell,
                  chwd := ChwdSpec,
                  cmd := CmdSpec }, In) -> 
    case cmencode:encode(ChwdSpec, In) of 
        {ok, Chwd} -> 
            case cmencode:encode(CmdSpec, In) of 
                {ok, Cmd} ->
                    case cmsh:sh(Cmd, [{cd, Chwd}]) of 
                        {ok, Out} -> 
                            cmkit:log({cmtask, Name, shell, Cmd, Chwd, Out}),
                            ok;
                        Other -> 
                            Other
                    end;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;


run_item(Name, #{ type := list,
                  value := Specs }, In) ->
    run_items(Name, Specs, In);

run_item(Name, #{ type := attempt, 
                  spec := Spec, 
                  onerror := OnError }, In) -> 
    case run_item(Name, Spec, In) of 
        ok -> 
            ok;
        {ok, Data} -> 
            {ok, Data};
        Other  ->
            cmkit:warning({task, Name, attempted, Spec, Other}),
            run_item(Name, OnError, In)
    end;

run_item(_, #{ type := db,
               spec := #{ bucket := Bucket,
                          type := Type,
                          id := Id,
                          value := Value }}, In) -> 
    case cmencode:encode(Bucket, In) of 
        {ok, B} -> 
            case cmencode:encode(Type, In) of 
                {ok, T} ->
                    case cmencode:encode(Id, In) of 
                        {ok, I} ->
                            case cmencode:encode(Value, In) of 
                                {ok, V} ->
                                    cmdb:put(B, [{T, has_id, I, V}]);
                                Other -> 
                                    Other
                            end;
                        Other -> 
                            Other
                    end;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end;


run_item(_, #{ type := queue,
               spec := #{ action := finish,
                          id := IdSpec,
                          name := Name }}, In) ->

    case cmencode:encode(IdSpec, In) of 
        {ok, Id} ->
            cmqueue:finish(Name, Id);
        Other -> 
            Other
    end;

run_item(_, #{ type := queue,
               name := NameSpec,
               notify := JobSpec,
               info := InfoSpec }, In) ->

    case cmencode:encode(NameSpec, In) of 
        {ok, Name} ->
            case cmencode:encode(JobSpec, In) of 
                {ok, Job} ->
                    case cmencode:encode(InfoSpec, In) of 
                        {ok, Info} ->
                            cmqueue:notify(Name, Job, Info); 
                        Other ->
                            Other
                    end;
                Other ->
                    Other
            end;
        Other -> 
            Other
    end;

run_item(_, #{ type := queue,
               name := NameSpec,
               finish := JobSpec }, In) ->

    case cmencode:encode(NameSpec, In) of 
        {ok, Name} ->
            case cmencode:encode(JobSpec, In) of 
                {ok, Job} ->
                    cmqueue:finish(Name, Job); 
                Other ->
                    Other
            end;
        Other -> 
            Other
    end;

run_item(_, #{ type := alias,
               target := TargetSpec,
               as := AliasSpec } = Spec, In) ->
    case cmencode:encode(TargetSpec, In) of 
        {ok, Value} ->
            case cmencode:encode(AliasSpec, In) of 
                {ok, As} ->
                    {alias, #{ As => Value }};
                Other ->
                    Other
            end;
        Other ->
            {error, #{ reason => Other,
                       spec => Spec,
                       context => In }}
    end;

run_item(_, Spec, In) -> 
    cmencode:encode(Spec, In).
