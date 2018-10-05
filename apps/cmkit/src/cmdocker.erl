-module(cmdocker).
-export([build/1, pull/1]).


pull(#{ repo := _Repo,
        tag := _Version,
        credentials := _Creds }) -> 
    {error, not_implemented_yet}.



build(#{ dir := Dir,
         repo := Repo,
         tag := Version,
         credentials := Creds }) -> 

    [User, Image] = cmkit:bin_split(Repo, <<"/">>),
    TarFilename = cmkit:to_list(cmkit:bin_join([User, Image, Version], <<"-">>)),
    TarFile = filename:join(["/tmp", TarFilename ++ ".tar.gz" ]),
    case cmkit:tar(TarFile, Dir) of 
        ok ->
            case file:read_file(TarFile) of 
                {ok, Data} -> 
                    BaseUrl = <<"http://localhost:1234">>,
                    Tag = cmkit:to_bin(cmkit:fmt("~s:~s", [Repo, Version])),
                    Url = <<BaseUrl/binary, "/build?nocache=true&t=", Tag/binary>>,
                    Headers = #{ 'content-type' => <<"application/x-tar">> },
                    cmkit:log({cmdocker, building, Tag}),
                    case cmhttp:post(Url, Headers, Data) of 
                        {ok, #{ status := 200, body := R } } ->
                            cmkit:log({cmdocker, build, R}),
                            PushUrl = <<BaseUrl/binary, "/images/", Repo/binary, 
                                        "/push?tag=", Version/binary>>,

                            cmkit:log({cmdocker,pushing, PushUrl}),
                            case cmhttp:post(PushUrl, #{ 
                                               'content-type' => "application/json",
                                               'X-Registry-Auth' => auth_config(Creds) }, #{}) of 
                                {ok, #{ status := 200, body := R2 }} -> 
                                    cmkit:log({cmdocker, push, R2}),
                                    ok;
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

auth_config(#{ user := User,
               password := Password,
               email := Email,
               registry := Registry }) -> 
    
    base64:encode(cmkit:jsone(#{ username => User,
                                 password => Password,
                                 email => Email,
                                 serveraddress => Registry })).

