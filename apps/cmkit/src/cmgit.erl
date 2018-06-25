-module(cmgit).
-export([clone/2]).

clone(Repo, #{ dir := Dir,
               credentials := Creds }) -> 
    Url = git_http_url(Repo, Creds),
    case git:clone(Url, cmkit:to_list(Dir)) of 
        {ok, _} ->
            cmkit:log({cmgit, cloned, Repo, Dir}),
            ok;
        Other -> 
            Other
    end.


git_http_url(Repo, #{ user := User,
                      password := Password }) -> 
    cmkit:fmt("https://~s:~s@~s", [ cmkit:to_list(User),
                              cmkit:to_list(Password),
                              cmkit:to_list(Repo) ]).
    
