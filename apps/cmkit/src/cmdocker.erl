-module(cmdocker).
-export([build/1, pull/1]).
-define(BASE_URL, <<"http://localhost:1234">>).

pull(#{ repo := _Repo,
        tag := _Version,
        credentials := _Creds }) -> 
    {error, not_implemented_yet}.



delete(#{ repo := Repo }) ->

    Url = <<?BASE_URL/binary, "/images/", Repo/binary>>,
    case cmhttp:delete(Url) of 
        {ok, #{ status := 200 }}  ->
            cmkit:log({cmdocker, deleted, Repo}),
            ok;
        {ok, #{ status := 404 }}  ->
            cmkit:log({cmdocker, absent, Repo}),
            ok;
        Other ->
            Other
    end.


build(#{ dir := Dir,
         repo := Repo,
         tag := Version,
         credentials := Creds } = Req) -> 

    [User, Image] = cmkit:bin_split(Repo, <<"/">>),
    TarFilename = cmkit:to_list(cmkit:bin_join([User, Image, Version], <<"-">>)),
    TarFile = filename:join(["/tmp", TarFilename ++ ".tar.gz" ]),
    file_absent(TarFile),
    case cmkit:tar(TarFile, Dir) of 
        ok ->
            case file:read_file(TarFile) of 
                {ok, Data} ->
                    case delete(Req) of 
                        ok -> 
                            Tag = cmkit:to_bin(cmkit:fmt("~s:~s", [Repo, Version])),
                            Url = <<?BASE_URL/binary, "/build?nocache=true&t=", Tag/binary>>,
                            Headers = #{ 'content-type' => <<"application/x-tar">> },
                            cmkit:log({cmdocker, building, Tag}),
                            case cmhttp:post(Url, Headers, Data) of 
                                {ok, #{ status := 200, body := R } } ->
                                    cmkit:log({cmdocker, build, R}),
                                    PushUrl = <<?BASE_URL/binary, "/images/", Repo/binary, 
                                                "/push?tag=", Version/binary>>,

                                    cmkit:log({cmdocker,pushing, PushUrl}),
                                    case cmhttp:post(PushUrl, #{ 
                                                       'content-type' => "application/json",
                                                       'X-Registry-Auth' => auth_config(Creds) }, #{}) of 
                                        {ok, #{ status := 200, body := R2 }} -> 
                                            cmkit:log({cmdocker, pushed, R2}),
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
                    Other -> Other
            end;
        Other -> 
            Other
    end.

file_absent(Path) ->
    {ok, _} = cmsh:sh(cmkit:fmt("rm -rf ~s", [Path]), []).

auth_config(#{ user := User,
               password := Password,
               email := Email,
               registry := Registry }) -> 
    
    base64:encode(cmkit:jsone(#{ username => User,
                                 password => Password,
                                 email => Email,
                                 serveraddress => Registry })).

