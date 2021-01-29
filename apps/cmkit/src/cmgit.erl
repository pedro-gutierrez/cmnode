-module(cmgit).
-export([pull/1, clone/2, tag/2, tags/1]).


pull(Dir) ->
    cmkit:log({cmgit, pull, Dir}),
    cmsh:sh(pull_cmd(), [{cd, Dir}]).

clone(Repo, #{ clone := false }) -> 
    cmkit:log({cmgit, clone, Repo, skipped}),
    ok;

clone(Repo, Params) ->
    case clone(Params#{ repo => Repo }) of 
        {ok, _} ->
            cmkit:log({cmgit, cloned, Repo}),
            ok;
        Other -> 
            Other
    end.

tag(Repo, #{ dir := Dir,
             prefix := Prefix,
             increment := Increment} = Params) -> 
    case clone(Repo, Params) of 
        ok -> 
            case tags(Dir) of
                {ok, AllTags} -> 
                    Tags = tags_filtered_by(Prefix, AllTags),
                    case next_tag(Tags, cmkit:to_bin(Increment)) of 
                        {ok, NextTag} ->
                            TagWithPrefix =  tag_with_prefix(Prefix, NextTag),
                            case branch(Params#{ repo => Repo, 
                                                 tag => TagWithPrefix }) of 
                                ok -> 
                                    cmkit:log({cmgit, Repo, tags, Prefix, Tags, TagWithPrefix}),
                                    case tag(Params#{ repo => Repo, tag => TagWithPrefix }) of 
                                        {ok, NewTag } -> 
                                            cmkit:log({cmgit, Repo, tagged, NewTag}),
                                            {ok, NewTag};
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
    end.

branch(#{ repo := Repo,
          credentials := Creds,
          dir := Dir,
          tag := Tag }) ->

    Url = git_http_url(Repo, Creds),
    Branch = <<Tag/binary, "-release">>,
    cmkit:log({cmgit, branching, Repo, Branch}),
    case cmsh:script([ branch_create_cmd(Branch),
                       touch_branch_cmd(Branch),
                       add_cmd(),
                       commit_cmd(Branch),
                       push_branch_cmd(Url, Branch) 
                     ], [{cd, Dir}]) of 
        {ok, Out} ->
            cmkit:log({cmgit, branched, Repo, Branch, Out}),
            ok;
        Other -> 
            Other
    end.

clone(#{ repo := Repo,
         credentials := Creds,
         dir := Dir,
         branch := Branch }) -> 
    case is_tmp(Dir) of
        true ->
            {ok, _} = cmsh:sh(rmdir_cmd(Dir), []),
            ok = filelib:ensure_dir(Dir),
            Url = git_http_url(Repo, Creds),
            cmkit:log({cmgit, cloning, Repo, Branch}),
            {ok, _} = cmsh:sh(clone_cmd(Url, Dir, Branch), []),
            {ok, LastCommitInfo} = cmsh:sh(last_commit_cmd(), [{cd, Dir}]),
            cmkit:log({cmgit, last_commit, LastCommitInfo}),
            {ok, LastCommitInfo};
        false ->
            {error, not_a_tmp_folder}
    end;

clone(Params) -> 
    clone(Params#{ branch => master }).

tag(#{ repo := Repo, 
       credentials := Creds,
       dir := Dir,
       tag := Tag }) ->     
    Url = git_http_url(Repo, Creds),
    cmkit:log({cmgit, tagging, Repo, Tag}),
    case cmsh:sh(tag_create_cmd(Tag), [{cd, Dir}]) of 
        {ok, _} -> 
            {ok, Out} = cmsh:sh(tag_push_cmd(Url, Tag), [{cd, Dir}]),
            case re:run(cmkit:to_bin(Out), <<"[new tag]">>) of 
                {match, _} -> {ok, Tag};
                nomatch -> {error, Out}
            end;
        Other -> 
            Other
    end.


tags(Dir) -> 
    case cmsh:sh(tags_cmd(), [{cd, Dir}]) of 
        {ok, Out} -> 
            Lines = cmkit:bin_split(cmkit:to_bin(Out), <<"\n">>),
            Tags = lists:foldr( fun(V, Acc) -> 
                                        case ref(cmkit:bin_split(V, <<"/">>), <<"tags">>) of 
                                            {ok, T} -> 
                                                [T|Acc];
                                            _ -> 
                                                Acc
                                        end
                                end, [], Lines),
            {ok, Tags};
        Other ->
            cmkit:danger({cmgit, tags, Dir, Other}),
            {error, #{ reason => cannot_fetch_tags,
                       dir => Dir }}
    end.

ref([_, Type, Value], Type) -> {ok, Value};
ref(_, _) -> nomatch.

is_tmp(Path) ->
    case re:run(Path, "^/tmp") of
        {match, _} -> true;
        nomath -> false
    end.

rmdir_cmd(Path) ->
    cmkit:fmt("rm -rf ~s", [Path]).

pull_cmd() -> "git pull".

tags_cmd() -> "git ls-remote".

tag_create_cmd(Tag) ->
    cmkit:fmt("git tag ~s", [Tag]).

tag_push_cmd(RepoURL, Tag) ->
    cmkit:fmt("git push \"~s\" ~s", [RepoURL, Tag]).

last_commit_cmd() ->
    "git show --summary".

clone_cmd(RepoURL, RepoPath, Branch) ->
    cmkit:fmt("git clone -b ~s \"~s\" \"~s\"", [Branch, RepoURL, RepoPath]).

branch_create_cmd(Name) ->
    cmkit:fmt("git checkout -b ~s", [Name]).

touch_branch_cmd(Name) ->
    cmkit:fmt("echo '{ \"rel\": \"~s\", \"date\": \"~s\"}' > RELEASE.json", [Name, cmkit:fmt_date()]).

add_cmd() -> "git add .".

commit_cmd(Branch) ->
    cmkit:fmt("git commit -am '~s'", [Branch]).

push_branch_cmd(_Url, Branch) -> 
    cmkit:fmt("git push origin ~s", [Branch]).

next_tag([], <<"patch">>) -> 
    {ok, {0, 0, 1}};

next_tag([], <<"minor">>) -> 
    {ok, {0, 1, 0}};

next_tag([Latest|_], Increment) ->
    case Latest of  
        {Major, Minor, Patch}-> 
            next_tag(Major, Minor, Patch, Increment);
        Other -> 
            {error, Other}
    end.

next_tag(Major, Minor, _, <<"minor">>) ->
    {ok, {Major, Minor+1, 0}};

next_tag(Major, _, _, <<"major">>) ->
    {ok, {Major+1, 0, 0}};

next_tag(Major, Minor, Patch, <<"patch">>) ->
    {ok, {Major, Minor, Patch +1}}.

tags_filtered_by(Prefix, Items) -> 
    lists:sublist(lists:reverse(lists:sort(fun compare_tag/2, lists:foldr(fun(I, Acc) -> 
                                                                                  case cmkit:prefix(I, Prefix) of 
                                                                                      nomatch -> Acc;
                                                                                      Suffix -> 
                                                                                          [Maj, Min, Patch] = cmkit:bin_split(Suffix, <<".">>),
                                                                                          [{ cmkit:to_number(Maj), 
                                                                                             cmkit:to_number(Min),
                                                                                             cmkit:to_number(Patch) }|Acc]
                                                                                  end
                                                                          end, [], Items))), 5).

tag_with_prefix(Prefix, {Maj, Min, Patch}) ->
    MajBin = cmkit:to_bin(Maj),
    MinBin = cmkit:to_bin(Min),
    PatchBin = cmkit:to_bin(Patch),
    <<Prefix/binary, MajBin/binary, ".", MinBin/binary, ".", PatchBin/binary>>. 

compare_tag({Maj1, _, _}, {Maj2, _, _}) when Maj1 < Maj2 -> true;
compare_tag({Maj1, _, _}, {Maj2, _, _}) when Maj1 > Maj2 -> false;
compare_tag({_, Min1, _}, {_, Min2, _}) when Min1 < Min2 -> true;
compare_tag({_, Min1, _}, {_, Min2, _}) when Min1 > Min2 -> false;
compare_tag({_, _, Patch1}, {_, _, Patch2}) when Patch1 < Patch2 -> true;
compare_tag({_, _, _}, {_, _, _}) -> false.

git_http_url(Repo, #{ user := User,
                      password := Password }) -> 
    cmkit:fmt("https://~s:~s@~s", [ cmkit:to_list(User),
                                    cmkit:to_list(Password),
                                    cmkit:to_list(Repo) ]).

