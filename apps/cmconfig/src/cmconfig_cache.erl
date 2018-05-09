-module(cmconfig_cache).
-behaviour(gen_statem).
-export([
         start_link/0,
         init/1, 
         callback_mode/0, 
         ready/3,
         terminate/3,
         update/1,
         build_graph/1,
         all/1,
         find/2,
         children/2,
         ancestors/2
        ]).
-record(data, {specs, graph }).

callback_mode() ->
    state_functions.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, ready, #data{ specs = #{},
                       graph = #{} }}.


ready({call, From}, {graph, Type, Id, Deps}, #data{ graph = Graph }=Data) ->

    Key = {Type, Id},
    
    NewAncestors = lists:foldl(fun({T, N}, Acc) ->
                                       AncestorsKey = {ancestors, T, N},
                                       Ancestors = maps:get(AncestorsKey, Graph, []),
                                       Acc#{ AncestorsKey => cmkit:distinct([Key|Ancestors])}
                end, #{}, Deps),
    Graph2 = maps:merge(Graph, NewAncestors),
    
    ChildrenKey = {children, Type, Id},
    Children = maps:get(ChildrenKey, Graph, []),
    Graph3 = maps:merge(Graph2, #{ ChildrenKey => cmkit:distinct(Deps ++ Children) }),
    {next_state, ready, Data#data{ graph = Graph3 }, [{reply, From, ok}]};

ready({call, From}, {put, #{ name := Name,
                             type := Type }=Spec }, 
      #data{ specs = Specs }=Data) ->
    
    Key = {Type, Name},
    Alias = {Type, cmkit:to_bin(Name)},
    Specs2 = maps:merge(Specs, #{
                         Key => Spec,
                         Alias => Key
                        }),
    {next_state, ready, Data#data{ specs = Specs2 }, [{reply, From, ok}]};

ready({call, From}, {all, Type}, #data{ specs = Specs}=Data) ->
    All = lists:filter(fun(#{ type := Type2 }) ->  
                               Type2 =:= Type;
                          (_) -> false
                       end, maps:values(Specs)),
    {next_state, ready, Data, [{reply, From, All}]};

ready({call, From}, {find, Type, Id}, #data{ specs = Specs }=Data) ->
    Res = case maps:get({Type, Id}, Specs, undef) of 
        undef -> 
            {error, not_found};
        {_, _} = Alias ->
            {ok, maps:get(Alias, Specs)};
        Spec when is_map(Spec) ->
            {ok, Spec}
    end,
    {next_state, ready, Data, [{reply, From, Res}]};

ready({call, From}, {ancestors, Type, Id}, #data{ graph = Graph }=Data) ->
    Res = cache_find({ancestors, Type, Id}, Graph),
    {next_state, ready, Data, [{reply, From, Res}]};

ready({call, From}, {children, Type, Id}, #data{ graph = Graph }=Data) ->
    Res = cache_find({children, Type, Id}, Graph),
    {next_state, ready, Data, [{reply, From, Res}]}.

terminate(Reason, _, _) ->
    cmkit:log({cmconfig_cache, terminate, Reason}),
    ok.

call(Msg) ->
    gen_statem:call({?MODULE, node()}, Msg).

cache_find(K, Cache) ->
    case maps:get(K, Cache, undef) of 
        undef -> {error, not_found};
        R -> {ok, R}
    end.

update(Spec) ->
    call({put, Spec}).

all(Type) ->
    call({all, Type}).

find(Type, Id) ->
    call({find, Type, Id}).

children(Type, Id) ->
    call({children, Type, Id}).

ancestors(Type, Id) ->
    call({ancestors, Type, Id}).

build_graph(Spec) ->
    {ok, Type, Name, Deps} = cmconfig_util:deps(Spec),
    call({graph, Type, Name, Deps}).
