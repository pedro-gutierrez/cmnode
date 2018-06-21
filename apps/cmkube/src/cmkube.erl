-module(cmkube).
-export([
         do/1
        ]).

do(#{ host := Host,
        token := Token,
        verb := get,
        resource := api_versions }) ->
    do(kuberl_core_api, get_api_versions, Host, Token);


do(#{ host := Host,
        token := Token,
        verb := list,
        resource := namespaces}) ->
    do(kuberl_core_v1_api, list_namespace, Host, Token);

do(#{ host := Host,
      token := Token,
      namespace := Ns,
      service := App,
      verb := <<"list">>,
      resource := <<"endpoints">> }) ->
    do(kuberl_core_v1_api, list_namespaced_endpoints, Ns, 
       #{ labelSelector => <<"app=", App/binary>> }, Host, Token);

do(#{ host := Host,
      token := Token,
      namespace := Ns,
      verb := <<"create">>,
      resource := <<"configmap">>,
      name := Name,
      data := Data
    }) ->
    
    Ctx = ctx:background(),
    Cfg = kuberl:cfg_with_bearer_token(kuberl:cfg_with_host(cmkit:to_list(Host)), Token),
    kuberl_core_v1_api:delete_namespaced_config_map(Ctx, Name, Ns, #{}, #{ cfg => Cfg }),
    kuberl_core_v1_api:create_namespaced_config_map(Ctx, Ns, #{ apiVersion => <<"v1">>,
                                                                kind => <<"ConfigMap">>,
                                                                metadata => #{ name => Name,
                                                                               namespace => Ns },
                                                                data => Data }, #{ cfg => Cfg });


do(#{ host := Host,
      token := Token,
      namespace := Ns,
      verb := <<"create">>,
      resource := <<"service">>,
      name := Name,
      labels := Labels,
      ports := Ports
    }) ->
    
    Ctx = ctx:background(),
    Cfg = kuberl:cfg_with_bearer_token(kuberl:cfg_with_host(cmkit:to_list(Host)), Token),
    kuberl_core_v1_api:delete_namespaced_service(Ctx, Name, Ns, #{ cfg => Cfg }),
    kuberl_core_v1_api:create_namespaced_service(Ctx, Ns, #{ apiVersion => <<"v1">>,
                                                             kind => <<"Service">>,
                                                             metadata => #{ name => Name,
                                                                            namespace => Ns,
                                                                            labels => Labels },
                                                             spec => #{ selector => Labels, 
                                                                        ports => Ports } }, #{ cfg => Cfg });

do(#{ host := Host,
      token := Token,
      namespace := Ns,
      verb := <<"create">>,
      resource := <<"deployment">>,
      name := Name,
      labels := Labels,
      replicas := Replicas,
      spec := Spec
    }) ->

    Ctx = ctx:background(),
    Opts = opts(Host, Token),
    case kuberl_apps_v1_api:delete_namespaced_deployment(Ctx, Name, Ns, #{}, Opts) of 
        {error, E} -> {error, E};
        _ ->
            Dep = #{ apiVersion => <<"apps/v1">>,
                     kind => <<"Deployment">>,
                     metadata => #{ name => Name,
                                    namespace => Ns,
                                    labels => Labels },
                     spec => #{ replicas => Replicas,
                                selector => #{ matchLabels => Labels },
                                template =>
                                #{ metadata => #{ labels => Labels },
                                   spec => Spec } }},
            
            case kuberl_apps_v1_api:create_namespaced_deployment(Ctx, Ns, Dep, Opts) of 
                {error, #{ message := E }, _} -> {error, E};
                {ok, FullSpec, _} -> {ok, FullSpec}
            end
    end;


do(Q) ->
    {error, #{ status => not_supported_yet,
               query => Q#{ token => <<"NOT SHOWN">> }}}.

opts(Host, Token) ->    
    Cfg = kuberl:cfg_with_bearer_token(kuberl:cfg_with_host(cmkit:to_list(Host)), Token),
    #{ cfg => Cfg }.


do(Api, Op, Host, Token) ->
    Cfg = kuberl:cfg_with_bearer_token(kuberl:cfg_with_host(cmkit:to_list(Host)), Token),
    Api:Op(ctx:background(), #{cfg => Cfg}).

do(Api, Op, Ns, Opts, Host, Token) ->
    Cfg = kuberl:cfg_with_bearer_token(kuberl:cfg_with_host(cmkit:to_list(Host)), Token),
    Api:Op(ctx:background(), Ns, #{ params => Opts,
                                    cfg => Cfg }).
