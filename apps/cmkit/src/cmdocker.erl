-module(cmdocker).
-export([build/1, pull/1]).
-define(BASE_URL, <<"http://localhost:1234">>).

pull(#{ repo := Repo,
        tag := Tag,
        credentials := Creds }) -> 

    Image = <<Repo/binary, ":", Tag/binary>>,
    cmkit:log({cmdocker, pull, Image}),
    PushUrl = <<?BASE_URL/binary, "/images/create?fromImage=", Image/binary>>,
    case cmhttp:post(PushUrl, #{ 
                                 'content-type' => "application/json",
                                 'X-Registry-Auth' => auth_config(Creds) }, #{}) of 
        {ok, #{ status := 200, body := R2 }} -> 
            cmkit:log({cmdocker, pulled, R2}),
            ok;
        Other -> 
            Other
    end.


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
         credentials := Creds,
         errors := Errors0 } = Req) -> 

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
                            cmkit:log({post, Url, Headers, size(Data)}),
                            HttpOpts = #{ debug => true,
                                          raw => true },
                            case cmhttp:post(Url, Headers, Data, HttpOpts) of 
                                {ok, #{ status := 200, raw := R, mime := Mime } } ->
                                    Errors = [<<"non-zero code">>|Errors0],
                                    case has_errors(cmkit:to_bin(R), Errors) of 
                                        {true, E} ->
                                            cmkit:danger({cmdocker, build, E, R}),
                                            {error, E};
                                        false ->
                                            cmkit:log({cmdocker, built, Mime, R}),
                                            PushUrl = <<?BASE_URL/binary, "/images/", Repo/binary, 
                                                        "/push?tag=", Version/binary>>,

                                            cmkit:log({cmdocker,pushing, PushUrl}),
                                            case cmhttp:post(PushUrl, #{ 
                                                                         'content-type' => "application/json",
                                                                         'X-Registry-Auth' => auth_config(Creds) 
                                                                       }, #{}, HttpOpts) of 
                                                {ok, #{ status := 200, raw := R2, mime := Mime }} -> 
                                                    cmkit:log({cmdocker, pushed, Mime, R2}),
                                                    ok;
                                                Other -> 
                                                    cmkit:warning({cmdocker,push, unexpected, Other}),
                                                    Other
                                            end
                                    end;
                                Other -> 
                                    cmkit:danger({cmdocker, Tag, error, Other}),
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

has_errors(_, []) -> false;
has_errors(Raw, [E|Rem]) ->
    case binary:matches(Raw, E) of 
        [] ->
            has_errors(Raw, Rem);
        _ ->
            {true, E}
    end.
