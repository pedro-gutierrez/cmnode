-module(cmtest_util).
-export([start/4,
         scenarios_by_tag/2,
         steps/2,
         run/3,
         close/2, 
         report_sort_fun/2]).

start(Test, #{ title := Title}=Scenario, Settings, Runner) ->
    case cmtest_scenario:start(Test, Scenario, Settings, Runner) of 
        {ok, Pid} ->
            {ok, Pid};
        Other -> 
            {error, {Title, Other}}
    end.

scenarios_by_tag(Tag, Scenarios) ->
    {ok, lists:filter(fun(#{ tags := Tags}) ->
                              lists:member(Tag, Tags)
                      end, Scenarios)}.

steps(#{ steps := Steps,
         backgrounds := Backgrounds }, Test) ->
    case resolve_backgrounds(Backgrounds, Test) of 
        {ok, Resolved } ->
            {ok, lists:flatten(lists:map(fun(#{ steps := Steps2 }) -> 
                                            Steps2 
                                    end, Resolved)) ++ Steps};
        Other -> Other
    end.

resolve_backgrounds(Backgrounds, Test) ->
    resolve_backgrounds(Backgrounds, Test, []).

resolve_backgrounds([], _, Out) -> {ok, lists:reverse(Out)};
resolve_backgrounds([#{ id := BId }=BRef|Rem], #{ backgrounds := Backgrounds }=Test, Out) ->
    case maps:get(BId, Backgrounds, undef) of 
        undef -> {error, {undefined, background, BRef}};
        Resolved -> resolve_backgrounds(Rem, Test, [Resolved|Out])
    end.

world_with_new_conn(App, Props, #{ conns := Conns }=World) ->
    Conns2 = Conns#{ App => maps:merge(Props, #{ inbox => [],
                                                 name => App,
                                                 app => App })},
    World#{ conns => Conns2 }.

connect(Name, #{ transport := Transport }=Config, World) ->
    case Transport of 
        <<"ws">> -> 
            connect_ws(Name, Config, World);
        <<"wss">> -> 
            connect_ws(Name, Config, World);

        <<"http">> ->
            connect_http(Name, Config, World);
        <<"https">> ->
            connect_http(Name, Config, World);
        <<"kubernetes">> -> 
            connect_kubernetes(Name, Config, World)
    end.


connect_ws(Name, Config, World) ->
    Url = cmkit:url(Config),
    {ok, Pid } = cmtest_ws_sup:new(Name, Config, self()),
    {ok, world_with_new_conn(Name, #{ transport => ws,
                                      class => websocket,
                                      pid => Pid,
                                      status => undef,
                                      url => Url }, World)}.


connect_http(Name, #{ transport := Transport }=Config, World) ->
    Url = cmkit:url(Config),
    Res = cmhttp:get(Url),
    Status = case Res of
                 {ok, _} -> up;
                 {error, S} -> S
             end,
    {ok, world_with_new_conn(Name, #{ transport => Transport,
                                      class => http,
                                      status => Status,
                                      url => Url}, World) }.

connect_kubernetes(Name, Config, World) ->

    Status = case cmkube:query(Config#{ 
                                 verb => get,
                                 resource => api_versions 
                                }) of 

        {ok, _, _} ->  up;
        _ -> down
    end,

    {ok, world_with_new_conn(Name, Config#{ class => kubernetes,
                                            status => Status }, World) }.


run(#{ type := _ }, _, #{ retries := #{ left := 0}}) ->
    {error, #{ error => max_retries_reached, 
               info => none }};

run(#{ type := connect,
       as := Name,
       spec := Spec }, Settings, World) ->

    case cmencode:encode(Spec, #{ world => World, 
                                  settings => Settings }) of
        {ok, #{ app := App,
                port := Port,
                transport := Transport } } ->
            
            case cmconfig:mount(App, Port, Transport) of
                {error, not_found} ->
                    {error, {not_found, App, Port, Transport}};
                {ok, Mount } ->
                    Config = Mount#{ debug => false ,
                                     host => "localhost",
                                     persistent => false
                                   },

                    connect(Name, Config, World)
            end;

        {ok, #{ host := _,
                  port := _,
                  path := _,
                  transport := _ } = Mount } ->

            Config = Mount#{ debug => false ,
                             persistent => false
                           },

            connect(Name, Config, World);

        {ok, #{ kubernetes := #{ host := _,
                                 token := _ } = Config }} -> 
            
            connect(Name, Config#{ transport => <<"kubernetes">> }, World);

        {error, E} ->
            {error, #{ error => encode_error,
                       info => E }}
    end;

run(#{ type := probe, 
       spec := #{
         status := Status 
        } = Spec }, _, #{ conns := Conns }=World) ->

    case maps:get(app, Spec, maps:get(connection, Spec, undef)) of 
        undef ->
            {error, #{ error => no_such_connection,
                       info => missing_app_or_connection_in_spec }};
        Name ->

            case maps:get(Name, Conns, undef) of
                undef ->
                    {error, #{ error => not_such_connection,
                               info => Name}};
                #{ status := Status } ->
                    {ok, World};
                _ ->
                    retry
            end
    end;

run(#{ type := expect, 
       spec := Spec }, Settings, World) ->
    case cmeval:eval(Spec, World, Settings) of 
        true ->
            {ok, World};
        false ->
            retry
    end;

run(#{ type := send, 
       spec := #{ 
         to := App,
         spec := Spec 
        }
     }, Settings, #{ conns := Conns }=World) ->
    case maps:get(App, Conns, undef) of 
        undef -> 
            {error, #{ error => no_such_connection,
                       info => App }};
        
        Conn ->
            case cmencode:encode(Spec, World, Settings) of 
                {ok, Encoded } ->
                    case Conn of 
                        #{  class := websocket, pid := Pid } -> 
                            cmwsc:send(Pid, Encoded),
                            {ok, World#{ conns => Conns#{ 
                                                    App => Conn#{inbox => []}
                                                   }}};
                        #{ class := http, transport := Transport, url := Url }  ->
                            case Encoded of 
                                #{ body := Body,
                                    headers := Headers } -> 
                                    
                                    cmkit:log({cmtest, Transport, out, Url, Headers, Body}),
                                    {_, Res} = cmhttp:post(Url, Headers, Body),
                                    cmkit:log({cmtest, Transport, in, Res}),
                                    {ok, World#{ conns => Conns#{
                                                            App => Conn#{inbox => [Res]}
                                                           }}};
                                Other ->
                                    {error, #{ error => wrong_http_request,
                                               info => Other } }
                            end;

                        #{ class := kubernetes } = Config ->
                            case Encoded of 
                                #{ list := R } -> 
                                    {ok, Res, _} = cmkube:query(Config#{ verb => list,
                                                                resource => R }),
                                    {ok, World#{ conns => Conns#{ 
                                                            App => Conn#{inbox => [Res]}
                                                             }}};
                                Other ->
                                    {error, #{ error => unsupported_kubernetes_query,
                                               info => Other }}
                            end
                    end;
                {error, E} -> {error, #{ error => encode_error,
                                         info => E }}
            end
    end;

run(#{ type := recv, 
       spec := #{
         as := As,
         from := App,
         spec := Spec
        }} , Settings,  #{ data := Data,
                conns := Conns
              }=World) ->
    case maps:get(App, Conns, undef) of 
        undef -> 
            {error, #{ error => no_such_connection,
                       info => App }};
        #{ inbox := Inbox } ->
            case cmdecode:decode(#{ type => first,
                                    spec => Spec }, Inbox, Settings) of 
                {ok, Decoded} ->
                    {ok, World#{ data => Data#{ As => Decoded }}};
                no_match -> 
                    retry
            end
    end;

run(#{  type := file,
        as := As, 
        spec := #{ type := path,
                   location := Path }}, _, #{ data := Data}=World) -> 
    case file:read_file(Path) of 
        {ok, Bin} ->
           {ok, World#{ data => Data#{ As => Bin }}};
        _ -> {error, #{ error => file_error,
                        info => Path}}
    end;

run(#{ type := kube,
       spec := #{
            resource := Resource,
            query := Verb
        }} = Spec, Settings, #{ data := Data }=World ) ->

    case kube_settings(Settings) of 
        {ok, Host, Token} -> 
            case cmkube:query(#{ host => Host,
                                token => Token,
                                verb => Verb,
                                resource => Resource }) of 
                {ok, Result, _} ->
                    Key = maps:get(as, Spec, latest),
                    { ok, World#{ data => Data#{ Key => Result }}};
                {error, E} ->
                    {error, #{ error => kubernetes_error,
                               info => E }}
            end;
        {error, E } -> 
            {error, #{ error => settings_error,
                       info => E }}
    end;

run(Step, _, _) ->
    {error, #{ error => unsupported,
               info => Step }}.

close(#{ conns := _Conns }, _Pid) ->
    %lists:foreach(fun(#{ name := Name,
    %                     pid := Pid }) ->
    %                      cmkit:log({cmtest, closing, Name, Pid})
    %                      cmtest_ws_sup:stop(Pid)
    %              end, maps:values(Conns)),
    ok.


kube_settings(#{ kubernetes := #{ api := Host, 
                            token := Token }}) ->
    {ok, Host, Token};

kube_settings(_) ->
    {error, wrong_kube_settings}.

report_sort_fun(#{ timestamp := T1}, #{ timestamp := T2}) ->
    T1 > T2.

