-module(cmkube).
-export([
         query/1
        ]).

query(#{ host := Host,
        token := Token,
        verb := get,
        resource := api_versions }) ->
    do(kuberl_core_api, get_api_versions, Host, Token);


query(#{ host := Host,
        token := Token,
        verb := list,
        resource := namespaces}) ->
    do(kuberl_core_v1_api, list_namespace, Host, Token);

query(#{ host := Host,
         token := Token,
         namespace := Ns,
         service := App,
         verb := <<"list">>,
         resource := <<"endpoints">> }) ->
    do(kuberl_core_v1_api, list_namespaced_endpoints, Ns, 
       #{ labelSelector => <<"app=", App/binary>> }, Host, Token);

query(Q) ->
    {error, #{ status => not_supported_yet,
               query => Q#{ token => <<"NOT SHOWN">> }}}.

do(Api, Op, Host, Token) ->
    Cfg = kuberl:cfg_with_bearer_token(kuberl:cfg_with_host(cmkit:to_list(Host)), Token),
    Api:Op(ctx:background(), #{cfg => Cfg}).

do(Api, Op, Ns, Opts, Host, Token) ->
    Cfg = kuberl:cfg_with_bearer_token(kuberl:cfg_with_host(cmkit:to_list(Host)), Token),
    Api:Op(ctx:background(), Ns, #{ params => Opts,
                                    cfg => Cfg }).

