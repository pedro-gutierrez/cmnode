-module(cmtask_util).
-export([run/3]).

run(#{ name := Name,
       items := Items }, #{ name := SettingsName,
                            spec := SettingsSpec }, Params) ->
    case cmencode:encode(SettingsSpec) of 
        {ok, Settings} ->
            Input = #{ settings => Settings,
                       params => Params,
                       context => #{} },
            run_items(Name, SettingsName, Items, Input);
        Other -> Other
    end.

run_items(Name, SettingsName, [], _) ->
    cmkit:success({task, Name, SettingsName, finished}),
    ok;

run_items(Name, Settings, [Item|Rem], In) -> 
    case run_item(Name, Item, #{ context := Ctx } = In) of 
        ok -> 
            run_items(Name, Settings, Rem, In);
        {ok, Extra } when is_map(Extra) ->
            run_items(Name, Settings, Rem, In#{ context => maps:merge(Ctx, Extra)}); 
        Other -> 
            cmkit:danger({task, Name, Settings, Item, Other}),
            Other
    end.

run_item(_, #{ type := kube,
       spec := #{ query := create,
                  resource := deployment,
                  params := ParamsSpec }}, #{ settings := Settings } = In) ->
    
    case Settings of 
        #{ kubernetes := #{ api := Host,
                            token := Token }} -> 
    
            case cmencode:encode(ParamsSpec, In) of 
                {ok, Params} ->
                    case cmkube:do(Params#{ verb => <<"create">>, 
                                            resource => <<"deployment">>,
                                            host => Host,
                                            token => Token}) of 
                        {error, E} -> {error, E};
                        {ok, _} -> ok
                    end;
                Other ->
                    Other
            end;
        _ -> 
            {error, missing_kube_settings}
    end;

run_item(Name, #{ type := slack,
            spec := #{ settings := SettingsSpec, 
                       severity := SeveritySpec,
                       subject := SubjectSpec,
                       body := BodySpec }}, #{ settings := Settings} = In) ->
    case maps:get(slack, Settings, undef) of 
        undef -> 
            {error, missing_slack_settings};
        Slack ->
            case cmencode:encode(SettingsSpec, In) of 
                {ok, SlackSettingsKey} ->
                    case maps:get(SlackSettingsKey, Slack, undef) of 
                        undef ->
                            {error, unknown_slack_settings};
                        #{ enabled := Enabled,
                           channel := Ch,
                           token := T } -> 
                            
                            case cmencode:encode(SeveritySpec, In) of 
                                {ok, S} ->
                                    case cmencode:encode(SubjectSpec, In) of 
                                        {ok, Sub} ->
                                            case cmencode:encode(BodySpec, In) of 
                                                {ok, Body} ->
                                                    case Enabled of 
                                                        false ->
                                                            cmkit:log({cmtask, Name, slack, disabled,
                                                                S, Sub, Body}),
                                                            ok;

                                                        true -> 
                                                            cmslack:S(#{ token => T,
                                                                         channel => Ch,
                                                                         subject => Sub,
                                                                         text => Body }),
                                                            ok
                                                    end;
                                                Other -> Other
                                            end;
                                        Other -> Other
                                    end;
                                Other -> Other
                            end;

                        Other -> Other
                    end;
                Other -> Other
            end
    end;

run_item(_, #{ type := git, spec := #{ action := clone,
                                          credentials := CredsSpec,
                                       repo := RepoSpec, 
                                       dir := DirSpec }}, In) ->
    case cmencode:encode(RepoSpec, In) of 
        {ok, Repo} -> 
            case cmencode:encode(DirSpec, In) of 
                {ok, Dir} ->
                    case cmencode:encode(CredsSpec, In) of 
                        {ok, Creds} -> 
                            cmgit:clone(Repo, #{ dir => Dir,
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
                                       dir := DirSpec,
                                       prefix := PrefixSpec,
                                       increment := Increment
                                     }}, In) ->
    case cmencode:encode(RepoSpec, In) of 
        {ok, Repo} -> 
            case cmencode:encode(DirSpec, In) of 
                {ok, Dir} ->
                    case cmencode:encode(CredsSpec, In) of 
                        {ok, Creds} -> 
                            case cmencode:encode(PrefixSpec, In) of 
                                {ok, Prefix} -> 
                                    case cmgit:tag(Repo, #{ dir => Dir,
                                                       credentials => Creds,
                                                       increment => Increment,
                                                       prefix => Prefix }) of 
                                        {ok, Tag} -> 
                                            {ok, #{ As => Tag }};
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

run_item(_, #{ type := docker, spec := #{ action := build,
                                          credentials := CredsSpec,
                                          repo := RepoSpec,
                                          tag := TagSpec,
                                          dir := DirSpec }}, In) ->
    case cmencode:encode(RepoSpec, In) of 
        {ok, Repo} -> 
            case cmencode:encode(DirSpec, In) of 
                {ok, Dir} ->
                    case cmencode:encode(TagSpec, In) of 
                        {ok, Tag} -> 
                            case cmencode:encode(CredsSpec, In) of 
                                {ok, Creds} -> 
                                    cmdocker:build(#{ credentials => Creds,
                                                      dir => Dir,
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
        Other -> 
            Other
    end;

run_item(_, #{ type := wait }=Spec, In) ->
    case cmencode:encode(Spec, In) of 
        {ok, true} -> ok;
        Other -> 
            Other
    end;

run_item(Name, #{ type := exec }=Spec, In) ->
    case cmencode:encode(Spec, In) of 
        {ok, Res} -> 
            cmkit:log({cmtask, Name, exec, Res}),
            ok;
        Other -> 
            Other
    end;

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

run_item(_, _, _) ->
    {error, task_item_not_supported}.
